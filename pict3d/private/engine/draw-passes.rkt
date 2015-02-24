#lang typed/racket/base

(require racket/match
         racket/list
         racket/bool
         math/flonum
         math/base
         typed/opengl
         (except-in typed/opengl/ffi cast ->)
         "../math/flt3.rkt"
         "../gl.rkt"
         "../utils.rkt"
         "utils.rkt"
         "shader-lib.rkt"
         "draw-pass.rkt")

(provide bloom-buffer-size
         draw-draw-passes)

(: bloom-buffer-size (Parameterof Positive-Integer))
(define bloom-buffer-size (make-parameter 256))

(define bloom-levels 4)

;; ===================================================================================================

(define fullscreen-vertex-code
  #<<code
in vec2 vert_position;
in vec2 vert_texcoord;

smooth out vec2 frag_texcoord;

void main() {
  gl_Position = vec4(vert_position, 0, 1);
  frag_texcoord = vert_texcoord;
}
code
  )

(define fullscreen-fragment-code
  #<<code
uniform sampler2D rgba;

smooth in vec2 frag_texcoord;

out vec4 out_color;

void main() {
  out_color = texture(rgba, frag_texcoord);
}
code
  )

(define fullscreen-vertex-struct
  (make-vao-struct
   (make-vao-field "vert_position" 2 GL_FLOAT)
   (make-vao-field "vert_texcoord" 2 GL_FLOAT)))

(define fullscreen-vertex-data
  (f32vector -1.0 -1.0 0.0 0.0
             +1.0 -1.0 1.0 0.0
             -1.0 +1.0 0.0 1.0
             +1.0 +1.0 1.0 1.0))

(define fullscreen-data-length (* 4 (f32vector-length fullscreen-vertex-data)))

(define-singleton/context (fullscreen-program)
  (log-pict3d-info "<engine> creating fullscreen program")
  (make-gl-program
   "fullscreen-program"
   empty
   (make-vao-struct)
   (list "out_color")
   (list (make-gl-shader GL_VERTEX_SHADER fullscreen-vertex-code)
         (make-gl-shader GL_FRAGMENT_SHADER fullscreen-fragment-code))))

(define-singleton/context (fullscreen-vao)
  (log-pict3d-info "<engine> creating vao for fullscreen compositing passes")
  (define vao (make-gl-vertex-array))
  (with-gl-vertex-array vao
    (define buf (make-gl-array-buffer))
    (with-gl-array-buffer buf
      (vao-struct-bind-attributes fullscreen-vertex-struct)
      (glBufferData GL_ARRAY_BUFFER fullscreen-data-length 0 GL_DYNAMIC_DRAW))
    (list vao buf)))

(: draw-fullscreen-quad (-> Flonum Flonum Void))
(define (draw-fullscreen-quad tex-w tex-h)
  (f32vector-set! fullscreen-vertex-data  6 tex-w)
  (f32vector-set! fullscreen-vertex-data 11 tex-h)
  (f32vector-set! fullscreen-vertex-data 14 tex-w)
  (f32vector-set! fullscreen-vertex-data 15 tex-h)
  
  (match-define (list vao buf) (fullscreen-vao))
  (with-gl-vertex-array vao
    (with-gl-array-buffer buf
      (glBufferSubData GL_ARRAY_BUFFER 0 fullscreen-data-length fullscreen-vertex-data)
      (glDrawArrays GL_TRIANGLE_STRIP 0 4))))

;; ===================================================================================================
;; Fragment shaders for transparency blending and bloom

(define blend-fragment-code
  #<<code
uniform sampler2D rgba;
uniform sampler2D weight;

smooth in vec2 frag_texcoord;

out vec4 out_color;

void main() {
  float w = texture(weight, frag_texcoord).r;
  if (w == 0.0) discard;
  vec4 accum = texture(rgba, frag_texcoord);
  out_color = accum / w;
}
code
  )

(define-singleton/context (blend-program)
  (log-pict3d-info "<engine> creating weighted transparency blend program")
  (make-gl-program
   "blend-program"
   empty
   (make-vao-struct)
   (list "out_color")
   (list (make-gl-shader GL_VERTEX_SHADER fullscreen-vertex-code)
         (make-gl-shader GL_FRAGMENT_SHADER blend-fragment-code))))

(define bloom-extract-fragment-code
  (string-append
   rgb-hsv-code
   #<<code
uniform sampler2D rgba;

smooth in vec2 frag_texcoord;

out vec4 out_color;

void main() {
  vec4 color = texture(rgba, frag_texcoord);
  vec3 hsv = rgb_to_hsv(color.rgb);
  vec3 bloom = vec3(hsv.xy, max(hsv.z - color.a, 0));  // keep intensity >= alpha
  out_color = vec4(hsv_to_rgb(bloom), 0.0);
}
code
  ))

(define-singleton/context (bloom-extract-program)
  (log-pict3d-info "<engine> creating overbright extraction program")
  (make-gl-program
   "bloom-extract-program"
   empty
   (make-vao-struct)
   (list "out_color")
   (list (make-gl-shader GL_VERTEX_SHADER fullscreen-vertex-code)
         (make-gl-shader GL_FRAGMENT_SHADER bloom-extract-fragment-code))))

(define bloom-combine-fragment-code
  (string-append
   rgb-hsv-code
   #<<code
uniform sampler2D color_tex;
uniform sampler2D bloom_tex;
uniform float bloom_frac;

smooth in vec2 frag_texcoord;

out vec4 out_color;

vec3 tone_map(vec3 color) {
  return pow(color, vec3(1/2.2));
}

void main() {
  vec4 color = texture(color_tex, frag_texcoord);
  vec3 hsv = rgb_to_hsv(color.rgb);
  vec3 rgb = hsv_to_rgb(vec3(hsv.xy, min(color.a, hsv.z)));
  vec4 bloom = texture(bloom_tex, frag_texcoord) * bloom_frac;
  color = vec4(rgb + bloom.rgb, color.a);
  color = clamp(color, vec4(0), vec4(1));
  color = vec4(tone_map(color.rgb), color.a);
  color = vec4(color.rgb, max(max(color.a, color.r), max(color.g, color.b)));
  out_color = color;
}
code
   ))

(define-singleton/context (bloom-combine-program)
  (log-pict3d-info "<engine> creating bloom compositing program")
  (make-gl-program
   "bloom-combine-program"
   empty
   (make-vao-struct)
   (list "out_color")
   (list (make-gl-shader GL_VERTEX_SHADER fullscreen-vertex-code)
         (make-gl-shader GL_FRAGMENT_SHADER bloom-combine-fragment-code))))

(define blur-vert-fragment-code
  #<<code
uniform sampler2D rgba;
uniform int height;

smooth in vec2 frag_texcoord;

out vec4 out_color;

void main() {
    float s = 1.0/height;
    vec4 color = texture(rgba, frag_texcoord)*1.5
               + texture(rgba, frag_texcoord + vec2(0,-1.5*s))*2.0
               + texture(rgba, frag_texcoord + vec2(0,+1.5*s))*2.0
               + texture(rgba, frag_texcoord + vec2(0,-3.5*s))*1.0
               + texture(rgba, frag_texcoord + vec2(0,+3.5*s))*1.0
               ;
    out_color = color / 7.5;
}
code
  )

(define-singleton/context (blur-vert-program)
  (log-pict3d-info "<engine> creating vertical blur program")
  (make-gl-program
   "blur-vert-program"
   empty
   (make-vao-struct)
   (list "out_color")
   (list (make-gl-shader GL_VERTEX_SHADER fullscreen-vertex-code)
         (make-gl-shader GL_FRAGMENT_SHADER blur-vert-fragment-code))))

(define blur-horz-fragment-code
  #<<code
uniform sampler2D rgba;
uniform int width;

smooth in vec2 frag_texcoord;

out vec4 out_color;

void main() {
    float s = 1.0/width;
    vec4 color = texture(rgba, frag_texcoord)*1.5
               + texture(rgba, frag_texcoord + vec2(-1.5*s,0))*2.0
               + texture(rgba, frag_texcoord + vec2(+1.5*s,0))*2.0
               + texture(rgba, frag_texcoord + vec2(-3.5*s,0))*1.0
               + texture(rgba, frag_texcoord + vec2(+3.5*s,0))*1.0
               ;
    out_color = color / 7.5;
}
code
  )

(define-singleton/context (blur-horz-program)
  (log-pict3d-info "<engine> creating horizontal blur program")
  (make-gl-program
   "blur-horz-program"
   empty
   (make-vao-struct)
   (list "out_color")
   (list (make-gl-shader GL_VERTEX_SHADER fullscreen-vertex-code)
         (make-gl-shader GL_FRAGMENT_SHADER blur-horz-fragment-code))))

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
  (log-pict3d-info "<engine> creating ~ax~a depth-buffer" width height)
  (make-gl-texture-2d width height GL_DEPTH_COMPONENT24 GL_DEPTH_COMPONENT GL_FLOAT
                      texture-params))

(define-singleton/context (get-tran-depth-buffer [width : Natural] [height : Natural])
  (log-pict3d-info "<engine> creating ~ax~a tran-depth-buffer" width height)
  (make-gl-texture-2d width height GL_DEPTH_COMPONENT24 GL_DEPTH_COMPONENT GL_FLOAT
                      texture-params))

(define-singleton/context (get-mat-fbo [width : Natural] [height : Natural])
  (log-pict3d-info "<engine> creating ~ax~a mat-fbo" width height)
  (define nnsa (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT texture-params))
  (make-gl-framebuffer width height
                       (list (cons GL_COLOR_ATTACHMENT0 nnsa)
                             (cons GL_DEPTH_ATTACHMENT (get-depth-buffer width height)))))

(define-singleton/context (get-tran-mat-fbo [width : Natural] [height : Natural])
  (log-pict3d-info "<engine> creating ~ax~a tran-mat-fbo" width height)
  (define nnsa (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT texture-params))
  (make-gl-framebuffer width height
                       (list (cons GL_COLOR_ATTACHMENT0 nnsa)
                             (cons GL_DEPTH_ATTACHMENT (get-tran-depth-buffer width height)))))

(define-singleton/context (get-light-fbo [width : Natural] [height : Natural])
  (log-pict3d-info "<engine> creating ~ax~a light-fbo" width height)
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
  (log-pict3d-info "<engine> creating ~ax~a tran-fbo" width height)
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
  (log-pict3d-info "<engine> creating ~ax~a draw-fbo" width height)
  (define rgba (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT texture-params))
  (define fbo
    (make-gl-framebuffer width height
                         (list (cons GL_COLOR_ATTACHMENT0 rgba)
                               (cons GL_DEPTH_ATTACHMENT (get-depth-buffer width height)))))
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  fbo)

(define-singleton/context (get-reduce-fbo [width : Natural] [height : Natural])
  (log-pict3d-info "<engine> creating ~ax~a reduce-fbo" width height)
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
  (log-pict3d-info "<engine> creating ~ax~a bloom-fbo" width height)
  (define rgba (make-gl-texture-2d width height GL_RGBA16F GL_BGRA GL_FLOAT blur-texture-params))
  (make-gl-framebuffer width height (list (cons GL_COLOR_ATTACHMENT0 rgba))))

(define-singleton/context (get-blur-fbo [width : Natural] [height : Natural])
  (log-pict3d-info "<engine> creating ~ax~a blur-fbo" width height)
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

(: gl-setup-state (-> Void))
(define (gl-setup-state)
  (unless (gl-core-profile?)
    (glEnable GL_TEXTURE_2D)))

(: draw-draw-passes (-> (Vectorof draw-passes) Natural Natural Natural
                        FlAffine3- FlTransform3
                        FlVector FlVector
                        Void))
(define (draw-draw-passes passes num width height view* proj* background ambient)
  ;(define face (if (xor (flt3consistent? proj*) (flt3consistent? view*)) 'back 'front))
  (define face (if (flt3consistent? (flt3compose proj* view*)) 'back 'front))
  
  (gl-setup-state)
  
  (define view (->flprojective3 view*))
  (define proj (->flprojective3 proj*))
  ;; Gamma-correct the ambient color and multiply by its intensity
  (define intensity (flvector-ref ambient 3))
  (define ambient-rgb (flvector (* (flexpt (flvector-ref ambient 0) 2.2) intensity)
                                (* (flexpt (flvector-ref ambient 1) 2.2) intensity)
                                (* (flexpt (flvector-ref ambient 2) 2.2) intensity)))
  
  (define znear (flprojective3-z-near proj))
  (define zfar  (flprojective3-z-far  proj))
  
  (define-values (bloom-width bloom-height)
    (let ([s  (bloom-buffer-size)])
      (if (> height width)
          (values s (min (* 2 s) (max 1 (round (* height (/ s width))))))
          (values (min (* 2 s) (max 1 (round (* width (/ s height))))) s))))
  
  (define dwidth (dimension-ceiling width))
  (define dheight (dimension-ceiling height))
  (define bloom-dwidth (dimension-ceiling bloom-width))
  (define bloom-dheight (dimension-ceiling bloom-height))
  
  (define tex-width (fl (/ width dwidth)))
  (define tex-height (fl (/ height dheight)))
  
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
  
  (: standard-uniforms (HashTable Symbol Uniform))
  (define standard-uniforms
    (make-immutable-hasheq
     (list (cons 'view (uniform-mat (fltransform3-forward view) 4))
           (cons 'unview (uniform-mat (fltransform3-inverse view) 4))
           (cons 'proj (uniform-mat (fltransform3-forward proj) 4))
           (cons 'unproj (uniform-mat (fltransform3-inverse proj) 4))
           (cons 'znear (uniform-float znear))
           (cons 'zfar (uniform-float zfar))
           (cons 'log2_znear_zfar (uniform-float (fllog2 (/ znear zfar))))
           (cons 'width (uniform-int width))
           (cons 'height (uniform-int height))
           (cons 'ambient (uniform-float ambient-rgb)))))
  
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
    (draw-opaque-material-pass passes num standard-uniforms face))
  
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
              (draw-light-pass passes num standard-uniforms face)))))))
  
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
              (draw-opaque-color-pass passes num standard-uniforms face)))))))
  
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
    (draw-transparent-material-pass passes num standard-uniforms face))
  
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
              (draw-light-pass passes num standard-uniforms face)))))))
  
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
              (draw-transparent-color-pass passes num standard-uniforms face)))))))
  
  ;; ----------------------------------------------------------------------------------------------
  ;; Compositing: Draw weighted transparency output
  
  ;; With framebuffer that already contains opaque pixels and their depths
  (with-gl-framebuffer draw-fbo
    (glViewport 0 0 width height)
    ;(glEnable GL_TEXTURE_2D)
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
              (gl-program-send-uniform program "rgba" (uniform-int 0))
              (gl-program-send-uniform program "weight" (uniform-int 1))
              (draw-fullscreen-quad tex-width tex-height)))))))
  
  ;; ----------------------------------------------------------------------------------------------
  ;; Compositing: Extract overbright values and blur
  
  (with-gl-framebuffer reduce-fbo
    (glViewport 0 0 width height)
    (glDisable GL_BLEND)
    (glDisable GL_DEPTH_TEST)
    (define program (bloom-extract-program))
    (with-gl-program program
      (gl-program-send-uniform program "rgba" (uniform-int 0))
      (with-gl-texture (gl-framebuffer-texture-2d draw-fbo GL_COLOR_ATTACHMENT0)
        (draw-fullscreen-quad tex-width tex-height))))
  
  (with-gl-texture (gl-framebuffer-texture-2d reduce-fbo GL_COLOR_ATTACHMENT0)
    (when (not (gl-core-profile?))
      (glHint GL_GENERATE_MIPMAP_HINT GL_NICEST))
    (glGenerateMipmap GL_TEXTURE_2D))
  
  (define horz-program (blur-horz-program))
  (define vert-program (blur-vert-program))
  
  (define view-widths
    (let ([base-view-width  (quotient bloom-width (expt 2 (- bloom-levels 1)))])
      (build-list bloom-levels (λ ([i : Index]) (* base-view-width (expt 2 i))))))
  
  (define view-heights
    (let ([base-view-height  (quotient bloom-height (expt 2 (- bloom-levels 1)))])
      (build-list bloom-levels (λ ([i : Index]) (* base-view-height (expt 2 i))))))
  
  (with-gl-framebuffer mat-fbo
    (glViewport 0 0 width height)
    (glClearColor 0.0 0.0 0.0 0.0)
    (glClear GL_COLOR_BUFFER_BIT))
  
  (for ([view-width   (in-list view-widths)]
        [view-height  (in-list view-heights)])
    (define view-tex-width (fl (/ view-width bloom-dwidth)))
    (define view-tex-height (fl (/ view-height bloom-dheight)))
    
    (glDisable GL_BLEND)
    
    (with-gl-framebuffer bloom-fbo
      (glViewport 0 0 bloom-dwidth bloom-dheight)
      (glClear GL_COLOR_BUFFER_BIT))
    
    (with-gl-framebuffer blur-fbo
      (glViewport 0 0 bloom-dwidth bloom-dheight)
      (glClear GL_COLOR_BUFFER_BIT))
    
    (with-gl-framebuffer bloom-fbo
      (glViewport 0 0 view-width view-height)
      (define program (fullscreen-program))
      (with-gl-program program
        (gl-program-send-uniform program "rgba" (uniform-int 0))
        (with-gl-texture (gl-framebuffer-texture-2d reduce-fbo GL_COLOR_ATTACHMENT0)
          (draw-fullscreen-quad tex-width tex-height))))
    
    ;; Ping-pong horizontal and vertical blur, alternating between "bloom-fbo => blur-fbo"
    ;; and "blur-fbo => bloom-fbo"
    (for ([_  (in-range 2)])
      (with-gl-program horz-program
        ;; Write to blur-fbo
        (with-gl-framebuffer blur-fbo
          (glViewport 0 0 view-width view-height)
          (gl-program-send-uniform horz-program "rgba" (uniform-int 0))
          (gl-program-send-uniform horz-program "width"
                                   (uniform-int (gl-framebuffer-width blur-fbo)))
          ;; Read from bloom-fbo
          (with-gl-texture (gl-framebuffer-texture-2d bloom-fbo GL_COLOR_ATTACHMENT0)
            (draw-fullscreen-quad view-tex-width view-tex-height))))
      
      (with-gl-program vert-program
        ;; Write to bloom-fbo
        (with-gl-framebuffer bloom-fbo
          (glViewport 0 0 view-width view-height)
          (gl-program-send-uniform vert-program "rgba" (uniform-int 0))
          (gl-program-send-uniform vert-program "height"
                                   (uniform-int (gl-framebuffer-height bloom-fbo)))
          ;; Read from blur-fbo
          (with-gl-texture (gl-framebuffer-texture-2d blur-fbo GL_COLOR_ATTACHMENT0)
            (draw-fullscreen-quad view-tex-width view-tex-height)))))
    
    (glEnable GL_BLEND)
    (glBlendFuncSeparate GL_ONE GL_ONE GL_ONE GL_ONE)
    (with-gl-framebuffer mat-fbo
      (glViewport 0 0 width height)
      (define program (fullscreen-program))
      (with-gl-program program
        (gl-program-send-uniform program "rgba" (uniform-int 0))
        (with-gl-texture (gl-framebuffer-texture-2d bloom-fbo GL_COLOR_ATTACHMENT0)
          (draw-fullscreen-quad view-tex-width view-tex-height))))
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
  
  (define program (bloom-combine-program))
  (with-gl-program program
    (glDisable GL_DEPTH_TEST)
    (glEnable GL_BLEND)
    (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)
    
    (with-gl-active-texture GL_TEXTURE0
      (with-gl-texture (gl-framebuffer-texture-2d draw-fbo GL_COLOR_ATTACHMENT0)
        (with-gl-active-texture GL_TEXTURE1
          (with-gl-texture (gl-framebuffer-texture-2d mat-fbo GL_COLOR_ATTACHMENT0)
            (define bloom-frac (/ 1.0 (fl bloom-levels)))
            (gl-program-send-uniform program "bloom_frac" (uniform-float bloom-frac))
            (gl-program-send-uniform program "color_tex" (uniform-int 0))
            (gl-program-send-uniform program "bloom_tex" (uniform-int 1))
            (draw-fullscreen-quad tex-width tex-height))))))
  #|
  (define program (fullscreen-program))
  (with-gl-program program
    (glViewport 0 0 width height)
    (glClearColor 0.0 0.0 0.0 0.0)
    (glClear GL_COLOR_BUFFER_BIT)
    (glDisable GL_BLEND)
    (glDisable GL_DEPTH_TEST)
    (with-gl-active-texture GL_TEXTURE0
      (with-gl-texture (gl-framebuffer-texture-2d mat-fbo GL_COLOR_ATTACHMENT0)
        (gl-program-send-uniform program "rgba" (uniform-int 0))
        (draw-fullscreen-quad tex-width tex-height))))
  |#
  )
