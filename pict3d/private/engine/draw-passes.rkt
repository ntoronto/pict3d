#lang typed/racket/base

(require racket/match
         math/flonum
         math/base
         typed/opengl
         (except-in typed/opengl/ffi cast ->)
         "../math/flt3.rkt"
         "../gl.rkt"
         "utils.rkt"
         "shader-lib.rkt"
         "draw-pass.rkt")

(provide bloom-buffer-size
         draw-draw-passes)

(: bloom-buffer-size (Parameterof Positive-Integer))
(define bloom-buffer-size (make-parameter 256))

(define bloom-levels '(1 2 4 8))

;; ===================================================================================================
;; Fragment shaders for transparency blending and bloom

(define blend-fragment-code
  #<<code
#version 130

uniform sampler2D rgba;
uniform sampler2D weight;

void main() {
  vec2 st = gl_TexCoord[0].st;
  float w = texture(weight, st).r;
  if (w == 0.0) discard;
  vec4 accum = texture(rgba, st);
  gl_FragColor = accum / w;
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
  vec4 color = texture(rgba, st);
  vec3 hsv = rgb_to_hsv(color.rgb);
  vec3 bloom = vec3(hsv.xy, max(hsv.z - color.a, 0));  // keep intensity >= alpha
  gl_FragColor = vec4(hsv_to_rgb(bloom), 0.0);
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
uniform float bloom_frac;

vec3 tone_map(vec3 color) {
  return pow(color, vec3(1/2.2));
}

void main() {
  vec2 st = gl_TexCoord[0].st;
  vec4 color = texture(color_tex, st);
  vec3 hsv = rgb_to_hsv(color.rgb);
  vec3 rgb = hsv_to_rgb(vec3(hsv.xy, min(color.a, hsv.z)));
  vec4 bloom = texture(bloom_tex, st) * bloom_frac;
  color = vec4(rgb + bloom.rgb, color.a);
  color = clamp(color, vec4(0), vec4(1));
  color = vec4(tone_map(color.rgb), color.a);
  color = vec4(color.rgb, max(max(color.a, color.r), max(color.g, color.b)));
  gl_FragColor = color;
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
    vec4 color = texture(rgba, xy)*1.5
               + texture(rgba, xy + vec2(0,-1.5*s))*2.0
               + texture(rgba, xy + vec2(0,+1.5*s))*2.0
               + texture(rgba, xy + vec2(0,-3.5*s))*1.0
               + texture(rgba, xy + vec2(0,+3.5*s))*1.0
               ;
    gl_FragColor = color / 7.5;
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
    vec4 color = texture(rgba, xy)*1.5
               + texture(rgba, xy + vec2(-1.5*s,0))*2.0
               + texture(rgba, xy + vec2(+1.5*s,0))*2.0
               + texture(rgba, xy + vec2(-3.5*s,0))*1.0
               + texture(rgba, xy + vec2(+3.5*s,0))*1.0
               ;
    gl_FragColor = color / 7.5;
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

(: dimension-ceiling (-> Natural Natural))
(define (dimension-ceiling w)
  (* (quotient (+ w 63) 64) 64))

(define-singleton/context (get-depth-buffer [width : Natural] [height : Natural])
  (printf "new ~a × ~a depth-buffer~n" width height)
  (make-gl-texture-2d width height GL_DEPTH_COMPONENT24 GL_DEPTH_COMPONENT GL_FLOAT
                      texture-params))

(define-singleton/context (get-tran-depth-buffer [width : Natural] [height : Natural])
  (printf "new ~a × ~a tran-depth-buffer~n" width height)
  (make-gl-texture-2d width height GL_DEPTH_COMPONENT24 GL_DEPTH_COMPONENT GL_FLOAT
                      texture-params))

(define-singleton/context (get-mat-fbo [width : Natural] [height : Natural])
  (printf "new ~a × ~a mat-fbo~n" width height)
  (define nnsa (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT texture-params))
  (make-gl-framebuffer width height
                       (list (cons GL_COLOR_ATTACHMENT0 nnsa)
                             (cons GL_DEPTH_ATTACHMENT (get-depth-buffer width height)))))

(define-singleton/context (get-tran-mat-fbo [width : Natural] [height : Natural])
  (printf "new ~a × ~a tran-mat-fbo~n" width height)
  (define nnsa (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT texture-params))
  (make-gl-framebuffer width height
                       (list (cons GL_COLOR_ATTACHMENT0 nnsa)
                             (cons GL_DEPTH_ATTACHMENT (get-tran-depth-buffer width height)))))

(define-singleton/context (get-light-fbo [width : Natural] [height : Natural])
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

(define-singleton/context (get-tran-fbo [width : Natural] [height : Natural])
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

(define-singleton/context (get-draw-fbo [width : Natural] [height : Natural])
  (printf "new ~a × ~a draw-fbo~n" width height)
  (define rgba (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT texture-params))
  (define fbo
    (make-gl-framebuffer width height
                         (list (cons GL_COLOR_ATTACHMENT0 rgba)
                               (cons GL_DEPTH_ATTACHMENT (get-depth-buffer width height)))))
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  fbo)

(define-singleton/context (get-reduce-fbo [width : Natural] [height : Natural])
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

(define-singleton/context (get-bloom-fbo [width : Natural] [height : Natural])
  (printf "new ~a × ~a bloom-fbo~n" width height)
  (define rgba (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT blur-texture-params))
  (make-gl-framebuffer width height (list (cons GL_COLOR_ATTACHMENT0 rgba))))

(define-singleton/context (get-blur-fbo [width : Natural] [height : Natural])
  (printf "new ~a × ~a blur-fbo~n" width height)
  (define rgba (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT blur-texture-params))
  (make-gl-framebuffer width height (list (cons GL_COLOR_ATTACHMENT0 rgba))))

;; ===================================================================================================
;; Draw a frame with OpenGL

(: gl-log-depth (-> Boolean Boolean Any))
;; Prep depth for logarithmic read/write
(define (gl-log-depth write? strict?)
  (glEnable GL_DEPTH_TEST)
  (glDepthFunc (if strict? GL_GREATER GL_GEQUAL))
  (glDepthMask write?)
  (glClearDepth 0.0))

(: draw-draw-passes (-> (Vectorof draw-passes) Natural Natural
                        FlAffine3- FlTransform3
                        FlVector FlVector Flonum
                        Void))
(define (draw-draw-passes passes width height view* proj* background ambient-color ambient-intensity)
  (define face (if (flt3consistent? view*) 'front 'back))
  
  (define view (->flprojective3 view*))
  (define proj (->flprojective3 proj*))
  ;; Gamma-correct the ambient color and multiply by its intensity
  (define ambient (flvector (* (flexpt (flvector-ref ambient-color 0) 2.2) ambient-intensity)
                            (* (flexpt (flvector-ref ambient-color 1) 2.2) ambient-intensity)
                            (* (flexpt (flvector-ref ambient-color 2) 2.2) ambient-intensity)))
  
  (define znear (flprojective3-z-near proj))
  (define zfar  (flprojective3-z-far  proj))
  
  (glEnable GL_TEXTURE_2D)
  
  (define-values (bloom-width bloom-height)
    (let ([s  (bloom-buffer-size)])
      (if (> height width)
          (values s (min (* 2 s) (max 1 (round (* height (/ s width))))))
          (values (min (* 2 s) (max 1 (round (* width (/ s height))))) s))))
  
  (define dwidth (dimension-ceiling width))
  (define dheight (dimension-ceiling height))
  (define bloom-dwidth (dimension-ceiling bloom-width))
  (define bloom-dheight (dimension-ceiling bloom-height))
  
  ;; Set up framebuffer objects for the different passes
  (define depth-buffer (get-depth-buffer dwidth dheight))
  (define tran-depth-buffer (get-tran-depth-buffer dwidth dheight))
  (define mat-fbo (get-mat-fbo dwidth dheight))
  (define tran-mat-fbo (get-tran-mat-fbo dwidth dheight))
  (define light-fbo (get-light-fbo dwidth dheight))
  (define tran-fbo (get-tran-fbo dwidth dheight))
  (define draw-fbo (get-draw-fbo dwidth dheight))
  (define reduce-fbo (get-reduce-fbo dwidth dheight))
  (define bloom-fbo (get-bloom-fbo bloom-dwidth bloom-dheight))
  (define blur-fbo (get-blur-fbo bloom-dwidth bloom-dheight))
  
  (define tex-width (fl (/ width dwidth)))
  (define tex-height (fl (/ height dheight)))
  (define bloom-tex-width (fl (/ bloom-width bloom-dwidth)))
  (define bloom-tex-height (fl (/ bloom-height bloom-dheight)))
  
  (: draw-fullscreen-quad (-> Flonum Flonum Void))
  (define (draw-fullscreen-quad tex-w tex-h)
    (glBegin GL_TRIANGLE_STRIP)
    (glTexCoord2f  0.0   0.0)  (glVertex2f -1.0 -1.0)
    (glTexCoord2f tex-w  0.0)  (glVertex2f +1.0 -1.0)
    (glTexCoord2f  0.0  tex-h) (glVertex2f -1.0 +1.0)
    (glTexCoord2f tex-w tex-h) (glVertex2f +1.0 +1.0)
    (glEnd))
  
  (: standard-uniforms (HashTable Symbol Uniform))
  (define standard-uniforms
    (make-immutable-hasheq
     (list (cons 'view (uniform-mat (flprojective3-entries view) 4))
           (cons 'unview (uniform-mat (flprojective3-entries (flt3inverse view)) 4))
           (cons 'proj (uniform-mat (flprojective3-entries proj) 4))
           (cons 'unproj (uniform-mat (flprojective3-entries (flt3inverse proj)) 4))
           (cons 'znear (uniform-float znear))
           (cons 'zfar (uniform-float zfar))
           (cons 'log2_znear_zfar (uniform-float (fllog2 (/ znear zfar))))
           (cons 'width (uniform-int width))
           (cons 'height (uniform-int height))
           (cons 'ambient (uniform-float ambient)))))
  
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
    (draw-pass 1 passes standard-uniforms face))
  
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
    (with-gl-active-texture GL_TEXTURE0
      (with-gl-texture depth-buffer
        (with-gl-active-texture GL_TEXTURE1
          (with-gl-texture (gl-framebuffer-texture-2d mat-fbo GL_COLOR_ATTACHMENT0)
            ;; Draw pass 0
            (let* ([standard-uniforms  (hash-set standard-uniforms 'depth (uniform-int 0))]
                   [standard-uniforms  (hash-set standard-uniforms 'material (uniform-int 1))])
              (draw-pass 0 passes standard-uniforms face)))))))
  
  ;; ----------------------------------------------------------------------------------------------
  ;; Pass 2 (color): Draw opaque geometry with lighting
  
  (with-gl-framebuffer draw-fbo
    (glViewport 0 0 width height)
    ;; Don't write to depth buffer, and only draw fragment on nearest z
    (gl-log-depth #f #f)
    ;; Opaque geometry occludes
    (glDisable GL_BLEND)
    (glClearColor 0.0 0.0 0.0 0.0)
    (glClear GL_COLOR_BUFFER_BIT)
    ;; Load diffuse and specular buffers into texture units 0 and 1
    (with-gl-active-texture GL_TEXTURE0
      (with-gl-texture (gl-framebuffer-texture-2d light-fbo GL_COLOR_ATTACHMENT0)
        (with-gl-active-texture GL_TEXTURE1
          (with-gl-texture (gl-framebuffer-texture-2d light-fbo GL_COLOR_ATTACHMENT1)
            ;; Draw pass 2
            (let* ([standard-uniforms  (hash-set standard-uniforms 'diffuse (uniform-int 0))]
                   [standard-uniforms  (hash-set standard-uniforms 'specular (uniform-int 1))])
              (draw-pass 2 passes standard-uniforms face)))))))
  
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
    (draw-pass 3 passes standard-uniforms face))
  
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
    (with-gl-active-texture GL_TEXTURE0
      (with-gl-texture tran-depth-buffer
        (with-gl-active-texture GL_TEXTURE1
          (with-gl-texture (gl-framebuffer-texture-2d tran-mat-fbo GL_COLOR_ATTACHMENT0)
            ;; Draw pass 0
            (let* ([standard-uniforms  (hash-set standard-uniforms 'depth (uniform-int 0))]
                   [standard-uniforms  (hash-set standard-uniforms 'material (uniform-int 1))])
              (draw-pass 0 passes standard-uniforms face)))))))
  
  ;; ----------------------------------------------------------------------------------------------
  ;; Pass 4 (transparency): Accumulate transparent geometry weighted outputs
  
  (with-gl-framebuffer tran-fbo
    (glViewport 0 0 width height)
    ;; Don't write to depth buffer, and only draw in front of the nearest z
    (gl-log-depth #f #t)
    ;; Set up for order-independent, weighted transparency w/out per-render-target blending
    (glClearColor 0.0 0.0 0.0 0.0)
    (glClear GL_COLOR_BUFFER_BIT)
    (glEnable GL_BLEND)
    (glBlendFuncSeparate GL_ONE GL_ONE GL_ONE GL_ONE)
    ;; Load diffuse and specular buffers into texture units 0 and 1
    (with-gl-active-texture GL_TEXTURE0
      (with-gl-texture (gl-framebuffer-texture-2d light-fbo GL_COLOR_ATTACHMENT0)
        (with-gl-active-texture GL_TEXTURE1
          (with-gl-texture (gl-framebuffer-texture-2d light-fbo GL_COLOR_ATTACHMENT1)
            ;; Draw pass 4
            (let* ([standard-uniforms  (hash-set standard-uniforms 'diffuse (uniform-int 0))]
                   [standard-uniforms  (hash-set standard-uniforms 'specular (uniform-int 1))])
              (draw-pass 4 passes standard-uniforms face)))))))
  
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
      (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)
      (with-gl-active-texture GL_TEXTURE0
        (with-gl-texture (gl-framebuffer-texture-2d tran-fbo GL_COLOR_ATTACHMENT0)
          (with-gl-active-texture GL_TEXTURE1
            (with-gl-texture (gl-framebuffer-texture-2d tran-fbo GL_COLOR_ATTACHMENT1)
              (gl-program-uniform program "rgba" (uniform-int 0))
              (gl-program-uniform program "weight" (uniform-int 1))
              (draw-fullscreen-quad tex-width tex-height)))))))
  
  ;; ----------------------------------------------------------------------------------------------
  ;; Compositing: Extract overbright values and blur
  
  (with-gl-framebuffer reduce-fbo
    (glViewport 0 0 width height)
    (glDisable GL_BLEND)
    (glDisable GL_DEPTH_TEST)
    (define program (bloom-extract-program))
    (with-gl-program program
      (gl-program-uniform program "rgba" (uniform-int 0))
      (with-gl-texture (gl-framebuffer-texture-2d draw-fbo GL_COLOR_ATTACHMENT0)
        (draw-fullscreen-quad tex-width tex-height))))
  
  (with-gl-texture (gl-framebuffer-texture-2d reduce-fbo GL_COLOR_ATTACHMENT0)
    (glHint GL_GENERATE_MIPMAP_HINT GL_NICEST)
    ;; Some stupid ATI drivers don't generate mipmaps unless this enable is done immediately before:
    (glEnable GL_TEXTURE_2D)
    (glGenerateMipmap GL_TEXTURE_2D))
  
  (with-gl-framebuffer mat-fbo
    (glViewport 0 0 width height)
    (glClearColor 0.0 0.0 0.0 0.0)
    (glClear GL_COLOR_BUFFER_BIT))
  
  (define horz-program (blur-horz-program))
  (define vert-program (blur-vert-program))
  
  (glClearColor 0.0 0.0 0.0 0.0)
  
  (for ([denom  (in-list bloom-levels)])
    (glDisable GL_BLEND)
    
    (with-gl-framebuffer bloom-fbo
      (glViewport 0 0 bloom-dwidth bloom-dheight)
      (glClear GL_COLOR_BUFFER_BIT))
    
    (with-gl-framebuffer blur-fbo
      (glViewport 0 0 bloom-dwidth bloom-dheight)
      (glClear GL_COLOR_BUFFER_BIT))
    
    (with-gl-framebuffer bloom-fbo
      (glViewport 0 0 (quotient bloom-width denom) (quotient bloom-height denom))
      (with-gl-texture (gl-framebuffer-texture-2d reduce-fbo GL_COLOR_ATTACHMENT0)
        (draw-fullscreen-quad tex-width tex-height)))
    
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
          (with-gl-texture (gl-framebuffer-texture-2d bloom-fbo GL_COLOR_ATTACHMENT0)
            (draw-fullscreen-quad (/ bloom-tex-width denom) (/ bloom-tex-height denom)))))
      
      (with-gl-program vert-program
        ;; Write to bloom-fbo
        (with-gl-framebuffer bloom-fbo
          (glViewport 0 0 (quotient bloom-width denom) (quotient bloom-height denom))
          (gl-program-uniform vert-program "rgba" (uniform-int 0))
          (gl-program-uniform vert-program "height" (uniform-int (gl-framebuffer-height bloom-fbo)))
          ;; Read from blur-fbo
          (with-gl-texture (gl-framebuffer-texture-2d blur-fbo GL_COLOR_ATTACHMENT0)
            (draw-fullscreen-quad (/ bloom-tex-width denom) (/ bloom-tex-height denom))))))
    
    (glEnable GL_BLEND)
    (glBlendFuncSeparate GL_ONE GL_ONE GL_ONE GL_ONE)
    (with-gl-framebuffer mat-fbo
      (glViewport 0 0 width height)
      (with-gl-texture (gl-framebuffer-texture-2d bloom-fbo GL_COLOR_ATTACHMENT0)
        (draw-fullscreen-quad (/ bloom-tex-width denom) (/ bloom-tex-height denom))))
    )
  
  ;; ----------------------------------------------------------------------------------------------
  ;; Compositing: Draw image and bloom onto system-provided framebuffer
  
  (glViewport 0 0 width height)
  (define alpha (flvector-ref background 3))
  (glClearColor (* (flvector-ref background 0) alpha)
                (* (flvector-ref background 1) alpha)
                (* (flvector-ref background 2) alpha)
                alpha)
  (glClear GL_COLOR_BUFFER_BIT)
  
  (glEnable GL_TEXTURE_2D)
  (define program (bloom-combine-program))
  (with-gl-program program
    (glDisable GL_DEPTH_TEST)
    (glEnable GL_BLEND)
    (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)
    
    (with-gl-active-texture GL_TEXTURE0
      (with-gl-texture (gl-framebuffer-texture-2d draw-fbo GL_COLOR_ATTACHMENT0)
        (with-gl-active-texture GL_TEXTURE1
          (with-gl-texture (gl-framebuffer-texture-2d mat-fbo GL_COLOR_ATTACHMENT0)
            (define bloom-frac (/ 1.0 (fl (length bloom-levels))))
            (gl-program-uniform program "bloom_frac" (uniform-float bloom-frac))
            (gl-program-uniform program "color_tex" (uniform-int 0))
            (gl-program-uniform program "bloom_tex" (uniform-int 1))
            (draw-fullscreen-quad tex-width tex-height))))))

#|  
  (glViewport 0 0 width height)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glEnable GL_TEXTURE_2D)
  (glDisable GL_BLEND)
  (glDisable GL_DEPTH_TEST)
  (with-gl-texture (gl-framebuffer-texture-2d mat-fbo GL_COLOR_ATTACHMENT0)
    (draw-fullscreen-quad tex-width tex-height))
  |#
  )
