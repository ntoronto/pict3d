#lang typed/racket/base

(require racket/match
         math/flonum
         math/base
         typed/opengl
         (except-in typed/opengl/ffi cast ->)
         "../math/flt3.rkt"
         "gl.rkt"
         "utils.rkt"
         "shader-lib.rkt"
         "draw-pass.rkt")

(provide z-near-distance
         z-far-distance
         fov-degrees
         bloom-buffer-size
         draw-draw-passes)

;; ===================================================================================================
;; Parameters

(: ambient-intensity (Parameterof FlVector))
(define ambient-intensity (make-parameter (flvector 1.0 1.0 1.0)))

(: z-near-distance (Parameterof Flonum))
(define z-near-distance (make-parameter (flexpt 2.0 -10.0)))

(: z-far-distance (Parameterof Flonum))
(define z-far-distance (make-parameter (flexpt 2.0 30.0)))

(: fov-degrees (Parameterof Real))
(define fov-degrees (make-parameter 90))

(: bloom-buffer-size (Parameterof Positive-Integer))
(define bloom-buffer-size (make-parameter 256))

;; ===================================================================================================
;; Fragment shaders for transparency blending and bloom

(define blend-fragment-code
  #<<code
#version 130

uniform sampler2D rgbv;
uniform sampler2D alpha;

void main() {
  vec2 st = gl_TexCoord[0].st;
  vec4 accum = texture(rgbv, st);
  float reveal = accum.a;
  accum.a = texture(alpha, st).r;
  gl_FragColor = vec4(accum.rgb / max(accum.a, 1e-5), reveal);
}
code
  )

(define bloom-extract-fragment-code
  (string-append
   "#version 130\n\n"
   rgb-hsv-code
   #<<code
uniform sampler2D rgba;

void main() {
  vec2 st = gl_TexCoord[0].st;
  //vec4 p = texture(rgba, st) - vec4(1.0, 1.0, 1.0, 0.0);
  //gl_FragColor = vec4(max(vec3(0.0), p.rgb), p.a);
  vec4 color = texture(rgba, st);
  vec3 hsv = rgb_to_hsv(color.rgb);
  vec3 bloom = vec3(hsv.xy, max(0.0, hsv.z - 1.0));  // keep intensity >= 1.0
  gl_FragColor = vec4(hsv_to_rgb(bloom), 1.0);
}
code
  ))

(define bloom-combine-fragment-code
  (string-append
   "#version 130\n\n"
   rgb-hsv-code
   #<<code
uniform sampler2D color_tex;
uniform sampler2D bloom_tex;

vec3 tone_map(vec3 rgb) {
  return pow(clamp(rgb,vec3(0.0),vec3(1.0)), vec3(1.0/2.2));
}

//vec3 tone_map(vec3 rgb) {
//  vec3 color = rgb * 1.0; // TODO: replace constant by exposure constant
//  vec3 x = max(vec3(0.0), color - vec3(0.004));
//  return (x * (6.2*x + vec3(0.5))) / (x * (6.2*x + vec3(1.7)) + vec3(0.06));
//}

void main() {
  vec2 st = gl_TexCoord[0].st;
  vec4 color = texture(color_tex, st);
  vec4 bloom = texture(bloom_tex, st);
  vec3 hsv = rgb_to_hsv(color.rgb);
  vec3 rgb = hsv_to_rgb(vec3(hsv.xy, min(1.0, hsv.z)));
  gl_FragColor = vec4(tone_map(rgb + bloom.rgb), 1.0);
}
code
   ))

(define blur-vert-fragment-code
  #<<code
#version 130

uniform sampler2D rgba;
uniform int height;

void main() {
    vec2 xy = gl_TexCoord[0].st;
    float s = 1.0/height;
    gl_FragColor = texture(rgba, xy)*1.5
                 + texture(rgba, xy + vec2(0,-1.5*s))*2.0
                 + texture(rgba, xy + vec2(0,+1.5*s))*2.0
                 + texture(rgba, xy + vec2(0,-3.5*s))*1.0
                 + texture(rgba, xy + vec2(0,+3.5*s))*1.0
                 ;
    gl_FragColor /= 7.5;
}
code
  )

(define blur-horz-fragment-code
  #<<code
#version 130

uniform sampler2D rgba;
uniform int width;

void main() {
    vec2 xy = gl_TexCoord[0].st;
    float s = 1.0/width;
    gl_FragColor = texture(rgba, xy)*1.5
                 + texture(rgba, xy + vec2(-1.5*s,0))*2.0
                 + texture(rgba, xy + vec2(+1.5*s,0))*2.0
                 + texture(rgba, xy + vec2(-3.5*s,0))*1.0
                 + texture(rgba, xy + vec2(+3.5*s,0))*1.0
                 ;
    gl_FragColor /= 7.5;
}
code
  )

(define-singleton (blend-program)
  (make-gl-program (make-vao-struct)
                   (list (make-gl-shader GL_FRAGMENT_SHADER blend-fragment-code))))

(define-singleton (bloom-extract-program)
  (make-gl-program (make-vao-struct)
                   (list (make-gl-shader GL_FRAGMENT_SHADER bloom-extract-fragment-code))))

(define-singleton (bloom-combine-program)
  (make-gl-program (make-vao-struct)
                   (list (make-gl-shader GL_FRAGMENT_SHADER bloom-combine-fragment-code))))

(define-singleton (blur-vert-program)
  (make-gl-program (make-vao-struct)
                   (list (make-gl-shader GL_FRAGMENT_SHADER blur-vert-fragment-code))))

(define-singleton (blur-horz-program)
  (make-gl-program (make-vao-struct)
                   (list (make-gl-shader GL_FRAGMENT_SHADER blur-horz-fragment-code))))

;; ===================================================================================================
;; Framebuffers

(define texture-params
  (list (cons GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
        (cons GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
        (cons GL_TEXTURE_MIN_FILTER GL_NEAREST)
        (cons GL_TEXTURE_MAG_FILTER GL_NEAREST)))

(: size-lte (-> (List Natural Natural)
                (List Natural Natural)
                (U (List Natural Natural) Boolean)))
(define (size-lte s1 s2)
  (match-define (list w1 h1) s1)
  (match-define (list w2 h2) s2)
  (define new-w (* (quotient (+ w1 63) 64) 64))
  (define new-h (* (quotient (+ h1 63) 64) 64))
  (cond [(and (= new-w w2) (= new-h h2))  #t]
        [else  (list new-w new-h)]))

(define-singleton/context (get-depth-buffer [width : Natural] [height : Natural]) #:lte size-lte
  (printf "new ~a × ~a depth-buffer~n" width height)
  (make-gl-texture-2d width height GL_DEPTH_COMPONENT24 GL_DEPTH_COMPONENT GL_FLOAT
                      texture-params))

(define-singleton/context (get-tran-depth-buffer [width : Natural] [height : Natural]) #:lte size-lte
  (printf "new ~a × ~a tran-depth-buffer~n" width height)
  (make-gl-texture-2d width height GL_DEPTH_COMPONENT24 GL_DEPTH_COMPONENT GL_FLOAT
                      texture-params))

(define-singleton/context (get-mat-fbo [width : Natural] [height : Natural]) #:lte size-lte
  (printf "new ~a × ~a mat-fbo~n" width height)
  (define nnsa (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT texture-params))
  (make-gl-framebuffer width height
                       (list (cons GL_COLOR_ATTACHMENT0 nnsa)
                             (cons GL_DEPTH_ATTACHMENT (get-depth-buffer width height)))))

(define-singleton/context (get-tran-mat-fbo [width : Natural] [height : Natural]) #:lte size-lte
  (printf "new ~a × ~a tran-mat-fbo~n" width height)
  (define nnsa (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT texture-params))
  (make-gl-framebuffer width height
                       (list (cons GL_COLOR_ATTACHMENT0 nnsa)
                             (cons GL_DEPTH_ATTACHMENT (get-tran-depth-buffer width height)))))

(define-singleton/context (get-light-fbo [width : Natural] [height : Natural]) #:lte size-lte
  (printf "new ~a × ~a light-fbo~n" width height)
  (define diff (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT texture-params))
  (define spec (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT texture-params))
  (define fbo
    (make-gl-framebuffer width height
                         (list (cons GL_COLOR_ATTACHMENT0 diff)
                               (cons GL_COLOR_ATTACHMENT1 spec))))
  (with-gl-framebuffer fbo
    (glDrawBuffers 2 (s32vector GL_COLOR_ATTACHMENT0 GL_COLOR_ATTACHMENT1)))
  fbo)

(define-singleton/context (get-tran-fbo [width : Natural] [height : Natural]) #:lte size-lte
  (printf "new ~a × ~a tran-fbo~n" width height)
  (define rgbv (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT texture-params))
  (define alpha (make-gl-texture-2d width height GL_R16F GL_RED GL_FLOAT texture-params))
  (define fbo
    (make-gl-framebuffer width height
                         (list (cons GL_COLOR_ATTACHMENT0 rgbv)
                               (cons GL_COLOR_ATTACHMENT1 alpha)
                               (cons GL_DEPTH_ATTACHMENT (get-depth-buffer width height)))))
  (with-gl-framebuffer fbo
    (glDrawBuffers 2 (s32vector GL_COLOR_ATTACHMENT0 GL_COLOR_ATTACHMENT1)))
  fbo)

(define-singleton/context (get-draw-fbo [width : Natural] [height : Natural]) #:lte size-lte
  (printf "new ~a × ~a draw-fbo~n" width height)
  (define rgba (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT texture-params))
  (define fbo
    (make-gl-framebuffer width height
                         (list (cons GL_COLOR_ATTACHMENT0 rgba)
                               (cons GL_DEPTH_ATTACHMENT (get-depth-buffer width height)))))
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  fbo)

(define-singleton/context (get-reduce-fbo [width : Natural] [height : Natural]) #:lte size-lte
  (printf "new ~a × ~a reduce-fbo~n" width height)
  (define color (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT
                                    (list (cons GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
                                          (cons GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
                                          (cons GL_TEXTURE_MIN_FILTER GL_NEAREST_MIPMAP_NEAREST)
                                          (cons GL_TEXTURE_MAG_FILTER GL_NEAREST))))
  (make-gl-framebuffer width height (list (cons GL_COLOR_ATTACHMENT0 color))))

(define blur-texture-params
  (list (cons GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
        (cons GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
        (cons GL_TEXTURE_MIN_FILTER GL_LINEAR)
        (cons GL_TEXTURE_MAG_FILTER GL_LINEAR)))

(define-singleton/context (get-bloom-fbo [width : Natural] [height : Natural]) #:lte size-lte
  (printf "new ~a × ~a bloom-fbo~n" width height)
  (define rgba (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT blur-texture-params))
  (make-gl-framebuffer width height (list (cons GL_COLOR_ATTACHMENT0 rgba))))

(define-singleton/context (get-blur-fbo [width : Natural] [height : Natural]) #:lte size-lte
  (printf "new ~a × ~a blur-fbo~n" width height)
  (define rgba (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT blur-texture-params))
  (make-gl-framebuffer width height (list (cons GL_COLOR_ATTACHMENT0 rgba))))

;; ===================================================================================================
;; Draw a frame with OpenGL

(define frame-count 0)

(: gl-log-depth (-> Boolean Boolean Any))
;; Prep depth for logarithmic read/write
(define (gl-log-depth write? strict?)
  (glEnable GL_DEPTH_TEST)
  (glDepthFunc (if strict? GL_GREATER GL_GEQUAL))
  (glDepthMask write?)
  (glClearDepth 0.0))

(: draw-draw-passes* (-> (Vectorof draw-passes) Natural Natural FlProjective3 Void))
(define (draw-draw-passes* passes width height model->view)
  (define znear (z-near-distance))
  (define zfar (z-far-distance))
  
  (set! frame-count (+ 1 frame-count))
  
  (define fov-radians (degrees->radians (fl (fov-degrees))))
  (define view->clip (perspective-flt3/viewport (fl width) (fl height) fov-radians znear zfar))
  (define view->model (flt3inverse model->view))
  (define clip->view (flt3inverse view->clip))
  
  (define unproj0 (flt3->unproj0 view->clip))
  (define unproj1 (flt3->unproj1 view->clip))
  
  (glEnable GL_TEXTURE_2D)
  
  (define-values (bloom-width bloom-height)
    (let ([s  (bloom-buffer-size)])
      (if (> height width)
          (values s (min (* 2 s) (max 1 (round (* height (/ s width))))))
          (values (min (* 2 s) (max 1 (round (* width (/ s height))))) s))))
  
  ;; Set up framebuffer objects for the different passes
  (define depth-buffer (get-depth-buffer width height))
  (define tran-depth-buffer (get-tran-depth-buffer width height))
  (define mat-fbo (get-mat-fbo width height))
  (define tran-mat-fbo (get-tran-mat-fbo width height))
  (define light-fbo (get-light-fbo width height))
  (define tran-fbo (get-tran-fbo width height))
  (define draw-fbo (get-draw-fbo width height))
  (define reduce-fbo (get-reduce-fbo width height))
  (define bloom-fbo (get-bloom-fbo bloom-width bloom-height))
  (define blur-fbo (get-blur-fbo bloom-width bloom-height))
  
  (: standard-uniforms (HashTable Symbol Uniform))
  (define standard-uniforms
    (make-immutable-hasheq
     (list (cons 'view (uniform-mat (flprojective3-entries model->view) 4))
           (cons 'unview (uniform-mat (flprojective3-entries view->model) 4))
           (cons 'proj (uniform-mat (flprojective3-entries view->clip) 4))
           (cons 'unproj (uniform-mat (flprojective3-entries clip->view) 4))
           (cons 'unproj0 (uniform-mat (fllinear3-entries unproj0) 3))
           (cons 'unproj1 (uniform-mat (fllinear3-entries unproj1) 3))
           (cons 'znear (uniform-float znear))
           (cons 'zfar (uniform-float zfar))
           (cons 'width (uniform-int width))
           (cons 'height (uniform-int height))
           (cons 'ambient (uniform-float (ambient-intensity))))))
  
  (define fullscreen-width (fl (/ width (gl-framebuffer-width draw-fbo))))
  (define fullscreen-height (fl (/ height (gl-framebuffer-height draw-fbo))))
  
  ;; ----------------------------------------------------------------------------------------------
  ;; Pass 1 (pre-light): Compute nearest opaque geometry depth, normals and specular powers
  
  (with-gl-framebuffer mat-fbo
    (glViewport 0 0 width height)
    ;; Write to the depth buffer
    (gl-log-depth #t #t)
    ;; Blending normals and specular powers doesn't make sense
    (glDisable GL_BLEND)
    (glClearColor 0.0 0.0 0.0 0.0)
    (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
    ;; Draw pass 1
    (draw-pass 1 passes standard-uniforms))
  
  ;; ----------------------------------------------------------------------------------------------
  ;; Pass 0 (light): Accumulate opaque geometry diffuse and specular reflectance
  
  (with-gl-framebuffer light-fbo
    (glViewport 0 0 width height)
    ;; Doesn't matter how deep each light's impostor geometry is; we're only accumulating their
    ;; contributions to the colors at known positions
    (glDisable GL_DEPTH_TEST)
    ;; Prepare to accumulate light contributions additively
    (glEnable GL_BLEND)
    (glBlendFuncSeparate GL_ONE GL_ONE GL_ONE GL_ONE)
    (glClearColor 0.0 0.0 0.0 0.0)
    (glClear GL_COLOR_BUFFER_BIT)
    ;; Load depth and material buffers into texture units 0 and 1
    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D (gl-object-handle (gl-texture-2d-object depth-buffer)))
    (glActiveTexture GL_TEXTURE1)
    (glBindTexture GL_TEXTURE_2D (gl-object-handle
                                  (gl-texture-2d-object
                                   (gl-framebuffer-texture-2d mat-fbo GL_COLOR_ATTACHMENT0))))
    (glActiveTexture GL_TEXTURE0)
    ;; Draw pass 0
    (let* ([standard-uniforms  (hash-set standard-uniforms 'depth (uniform-int 0))]
           [standard-uniforms  (hash-set standard-uniforms 'material (uniform-int 1))])
      (draw-pass 0 passes standard-uniforms)))
  
  ;; ----------------------------------------------------------------------------------------------
  ;; Pass 2 (color): Draw opaque geometry with lighting
  
  (with-gl-framebuffer draw-fbo
    (glViewport 0 0 width height)
    ;; Don't write to depth buffer, and only draw fragment on nearest z
    (gl-log-depth #f #f)
    ;; Opaque geometry occludes
    (glDisable GL_BLEND)
    (glClearColor 0.0 0.0 0.0 1.0)
    (glClear GL_COLOR_BUFFER_BIT)
    ;; Load diffuse and specular buffers into texture units 0 and 1
    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D (gl-object-handle
                                  (gl-texture-2d-object
                                   (gl-framebuffer-texture-2d light-fbo GL_COLOR_ATTACHMENT0))))
    (glActiveTexture GL_TEXTURE1)
    (glBindTexture GL_TEXTURE_2D (gl-object-handle
                                  (gl-texture-2d-object
                                   (gl-framebuffer-texture-2d light-fbo GL_COLOR_ATTACHMENT1))))
    (glActiveTexture GL_TEXTURE0)
    ;; Draw pass 2
    (let* ([standard-uniforms  (hash-set standard-uniforms 'diffuse (uniform-int 0))]
           [standard-uniforms  (hash-set standard-uniforms 'specular (uniform-int 1))])
      (draw-pass 2 passes standard-uniforms)))
  
  ;; ----------------------------------------------------------------------------------------------
  ;; Pass 3 (pre-light): Compute nearest transparent geometry depth, normals and specular powers
  
  (with-gl-framebuffer tran-mat-fbo
    (glViewport 0 0 width height)
    ;; Write to the depth buffer
    (gl-log-depth #t #t)
    ;; Blending normals and specular powers doesn't make sense
    (glDisable GL_BLEND)
    (glClearColor 0.0 0.0 0.0 0.0)
    (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
    ;; Draw pass 3
    (draw-pass 3 passes standard-uniforms))
  
  ;; ----------------------------------------------------------------------------------------------
  ;; Pass 0 (light): Accumulate transparent geometry diffuse and specular reflectance
  
  (with-gl-framebuffer light-fbo
    (glViewport 0 0 width height)
    ;; Doesn't matter how deep each light's impostor geometry is; we're only accumulating their
    ;; contributions to the colors at known positions
    (glDisable GL_DEPTH_TEST)
    ;; Prepare to accumulate light contributions additively
    (glEnable GL_BLEND)
    (glBlendFuncSeparate GL_ONE GL_ONE GL_ONE GL_ONE)
    (glClearColor 0.0 0.0 0.0 0.0)
    (glClear GL_COLOR_BUFFER_BIT)
    ;; Load depth and material buffers into texture units 0 and 1
    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D (gl-object-handle (gl-texture-2d-object tran-depth-buffer)))
    (glActiveTexture GL_TEXTURE1)
    (glBindTexture GL_TEXTURE_2D (gl-object-handle
                                  (gl-texture-2d-object
                                   (gl-framebuffer-texture-2d tran-mat-fbo GL_COLOR_ATTACHMENT0))))
    (glActiveTexture GL_TEXTURE0)
    ;; Draw pass 0
    (let* ([standard-uniforms  (hash-set standard-uniforms 'depth (uniform-int 0))]
           [standard-uniforms  (hash-set standard-uniforms 'material (uniform-int 1))])
      (draw-pass 0 passes standard-uniforms)))
  
  ;; ----------------------------------------------------------------------------------------------
  ;; Pass 4 (transparency): Accumulate transparent geometry weighted outputs
  
  (with-gl-framebuffer tran-fbo
    (glViewport 0 0 width height)
    ;; Don't write to depth buffer, and only draw in front of the nearest z
    (gl-log-depth #f #t)
    ;; Set up for order-independent, weighted transparency w/out per-render-target blending
    (glClearColor 0.0 0.0 0.0 1.0)
    (glClear GL_COLOR_BUFFER_BIT)
    (glEnable GL_BLEND)
    (glBlendFuncSeparate GL_ONE GL_ONE GL_ZERO GL_ONE_MINUS_SRC_ALPHA)
    ;; Load diffuse and specular buffers into texture units 0 and 1
    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D (gl-object-handle
                                  (gl-texture-2d-object
                                   (gl-framebuffer-texture-2d light-fbo GL_COLOR_ATTACHMENT0))))
    (glActiveTexture GL_TEXTURE1)
    (glBindTexture GL_TEXTURE_2D (gl-object-handle
                                  (gl-texture-2d-object
                                   (gl-framebuffer-texture-2d light-fbo GL_COLOR_ATTACHMENT1))))
    (glActiveTexture GL_TEXTURE0)
    ;; Draw pass 4
    (let* ([standard-uniforms  (hash-set standard-uniforms 'diffuse (uniform-int 0))]
           [standard-uniforms  (hash-set standard-uniforms 'specular (uniform-int 1))])
      (draw-pass 4 passes standard-uniforms)))
  
  ;; ----------------------------------------------------------------------------------------------
  ;; Compositing: Draw weighted transparency output
  
  ;; With framebuffer that already contains opaque pixels and their depths
  (with-gl-framebuffer draw-fbo
    (glViewport 0 0 width height)
    (glEnable GL_TEXTURE_2D)
    ;; Write transparent pixels' weighted averages
    (define program (blend-program))
    (with-gl-program program
      (glDisable GL_DEPTH_TEST)
      (glEnable GL_BLEND)
      (glBlendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA)
      
      (gl-program-uniform program "rgbv" (uniform-int 0))
      (glActiveTexture GL_TEXTURE0)
      (glBindTexture GL_TEXTURE_2D (gl-object-handle
                                    (gl-texture-2d-object
                                     (gl-framebuffer-texture-2d tran-fbo GL_COLOR_ATTACHMENT0))))
      
      (gl-program-uniform program "alpha" (uniform-int 1))
      (glActiveTexture GL_TEXTURE1)
      (glBindTexture GL_TEXTURE_2D (gl-object-handle
                                    (gl-texture-2d-object
                                     (gl-framebuffer-texture-2d tran-fbo GL_COLOR_ATTACHMENT1))))
      (glActiveTexture GL_TEXTURE0)
      
      (gl-fullscreen-quad fullscreen-width fullscreen-height)))
  
  ;; ----------------------------------------------------------------------------------------------
  ;; Compositing: Extract overbright values and blur
  
  (with-gl-framebuffer reduce-fbo
    (glViewport 0 0 width height)
    (glDisable GL_BLEND)
    (glDisable GL_DEPTH_TEST)
    (with-gl-program (bloom-extract-program)
      (with-gl-texture-2d (gl-framebuffer-texture-2d draw-fbo GL_COLOR_ATTACHMENT0)
        (gl-fullscreen-quad fullscreen-width fullscreen-height))))
  
  (with-gl-texture-2d (gl-framebuffer-texture-2d reduce-fbo GL_COLOR_ATTACHMENT0)
    (glHint GL_GENERATE_MIPMAP_HINT GL_NICEST)
    ;; Some stupid ATI drivers don't generate mipmaps unless this enable is done immediately before:
    (glEnable GL_TEXTURE_2D)
    (glGenerateMipmap GL_TEXTURE_2D))
  
  (with-gl-framebuffer mat-fbo
    (glClearColor 0.0 0.0 0.0 0.0)
    (glClear GL_COLOR_BUFFER_BIT))
  
  (define horz-program (blur-horz-program))
  (define vert-program (blur-vert-program))
  
  (glClearColor 0.0 0.0 0.0 0.25)
  
  (for ([denom  (in-list '(1 2 4 8))])
    (glDisable GL_BLEND)
    
    (glColorMask #t #t #t #t)
    
    (with-gl-framebuffer bloom-fbo
      (glViewport 0 0 bloom-width bloom-height)
      (glClear GL_COLOR_BUFFER_BIT))
    
    (with-gl-framebuffer blur-fbo
      (glViewport 0 0 bloom-width bloom-height)
      (glClear GL_COLOR_BUFFER_BIT))
    
    (glColorMask #t #t #t #f)
    
    (with-gl-framebuffer bloom-fbo
      (glViewport 0 0 (quotient bloom-width denom) (quotient bloom-height denom))
      (with-gl-texture-2d (gl-framebuffer-texture-2d reduce-fbo GL_COLOR_ATTACHMENT0)
        (gl-fullscreen-quad fullscreen-width fullscreen-height)))
    
    (define fullscreen-bloom-width (fl (/ (quotient bloom-width denom)
                                          (gl-framebuffer-width bloom-fbo))))
    (define fullscreen-bloom-height (fl (/ (quotient bloom-height denom)
                                           (gl-framebuffer-height bloom-fbo))))
    
    ;; Ping-pong horizontal and vertical blur, alternating between "bloom-fbo => blur-fbo"
    ;; and "blur-fbo => bloom-fbo"
    (for ([_  (in-range 2)])
      (with-gl-program horz-program
        ;; Write to blur-fbo
        (with-gl-framebuffer blur-fbo
          (glViewport 0 0 (quotient bloom-width denom) (quotient bloom-height denom))
          (gl-program-uniform horz-program "rgba" (uniform-int 0))
          (gl-program-uniform horz-program "width" (uniform-int (gl-framebuffer-width blur-fbo)))
          ;; Read from bloom-fbo
          (with-gl-texture-2d (gl-framebuffer-texture-2d bloom-fbo GL_COLOR_ATTACHMENT0)
            (gl-fullscreen-quad fullscreen-bloom-width fullscreen-bloom-height))))
      
      (with-gl-program vert-program
        ;; Write to bloom-fbo
        (with-gl-framebuffer bloom-fbo
          (glViewport 0 0 (quotient bloom-width denom) (quotient bloom-height denom))
          (gl-program-uniform vert-program "rgba" (uniform-int 0))
          (gl-program-uniform vert-program "height" (uniform-int (gl-framebuffer-height bloom-fbo)))
          ;; Read from blur-fbo
          (with-gl-texture-2d (gl-framebuffer-texture-2d blur-fbo GL_COLOR_ATTACHMENT0)
            (gl-fullscreen-quad fullscreen-bloom-width fullscreen-bloom-height)))))
    
    (glEnable GL_BLEND)
    (glBlendFunc GL_SRC_ALPHA GL_ONE)
    (with-gl-framebuffer mat-fbo
      (glViewport 0 0 width height)
      (with-gl-texture-2d (gl-framebuffer-texture-2d bloom-fbo GL_COLOR_ATTACHMENT0)
        (gl-fullscreen-quad fullscreen-bloom-width fullscreen-bloom-height)))
    )
  
  (glColorMask #t #t #t #t)
  
  ;; ----------------------------------------------------------------------------------------------
  ;; Compositing: Draw image and bloom onto system-provided framebuffer
  
  (glViewport 0 0 width height)
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  
  (glEnable GL_TEXTURE_2D)
  (define program (bloom-combine-program))
  (with-gl-program program
    (glDisable GL_DEPTH_TEST)
    (glDisable GL_BLEND)
    
    (gl-program-uniform program "color_tex" (uniform-int 0))
    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D (gl-object-handle
                                  (gl-texture-2d-object
                                   (gl-framebuffer-texture-2d draw-fbo GL_COLOR_ATTACHMENT0))))
    
    (gl-program-uniform program "bloom_tex" (uniform-int 1))
    (glActiveTexture GL_TEXTURE1)
    (glBindTexture GL_TEXTURE_2D (gl-object-handle
                                  (gl-texture-2d-object
                                   (gl-framebuffer-texture-2d mat-fbo GL_COLOR_ATTACHMENT0))))
    (glActiveTexture GL_TEXTURE0)
    
    (gl-fullscreen-quad fullscreen-width fullscreen-height))
  #|
  (glViewport 0 0 width height)
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glEnable GL_TEXTURE_2D)
  (glDisable GL_BLEND)
  (glDisable GL_DEPTH_TEST)
  (with-gl-texture-2d (gl-framebuffer-texture-2d mat-fbo GL_COLOR_ATTACHMENT0)
    (gl-fullscreen-quad))
  |#
  )

(: draw-draw-passes (-> (Vectorof draw-passes) Natural Natural FlTransform3 Void))
(define (draw-draw-passes passes width height model->view)
  (values ;time
   (draw-draw-passes* passes width height (->flprojective3 model->view))))
