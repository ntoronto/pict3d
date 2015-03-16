#lang typed/racket/base

;; Directional light impostors: vertex shader outputs a quad; fragment shader computes light

(require racket/match
         racket/flonum
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         "../../math/flt3.rkt"
         "../../math/flrect3.rkt"
         "../../gl.rkt"
         "../../utils.rkt"
         "../types.rkt"
         "../utils.rkt"
         "../shader-code.rkt"
         "../draw-pass.rkt"
         "types.rkt"
         "flags.rkt")

(provide make-directional-light-shape
         make-directional-light-shape-passes
         set-directional-light-shape-emitted
         directional-light-shape-rect
         directional-light-shape-easy-transform
         )

;; ===================================================================================================
;; Constructors

(: make-directional-light-shape (-> FlVector FlVector directional-light-shape))
(define (make-directional-light-shape e dv)
  (cond [(not (= 4 (flvector-length e)))
         (raise-argument-error 'make-directional-light-shape "length-4 flvector"
                               0 e dv)]
        [(not (= 3 (flvector-length dv)))
         (raise-argument-error 'make-directional-light-shape "length-3 flvector"
                               1 e dv)]
        [else
         (define fs (flags-join invisible-flag transparent-flag (color-emitting-flag e)))
         (directional-light-shape (lazy-passes) fs e dv)]))

;; ===================================================================================================
;; Set attributes

(: set-directional-light-shape-emitted (-> directional-light-shape FlVector
                                           directional-light-shape))
(define (set-directional-light-shape-emitted a e)
  (cond [(not (= 4 (flvector-length e)))
         (raise-argument-error 'set-directional-light-shape-emitted "length-4 flvector"
                               1 a e)]
        [else
         (match-define (directional-light-shape _ fs old-e dv) a)
         (cond [(equal? old-e e)  a]
               [else
                (define new-fs (flags-join (flags-subtract fs emitting-flags)
                                           (color-emitting-flag e)))
                (directional-light-shape (lazy-passes) new-fs e dv)])]))

;; ===================================================================================================
;; Program for pass 0: light

(define directional-light-vertex-attributes
  (list (attribute "" 'float/byte "vert_id")))

(define directional-light-fragment-attributes
  (list (attribute "smooth" 'vec3 "frag_dir")))

(define directional-light-vertex-code
  (make-vertex-code
   "directional-light-vertex"
   #:standard-uniforms (list (standard-uniform "" 'mat4 "unproj" 'unproj))
   #:in-attributes directional-light-vertex-attributes
   #:out-attributes directional-light-fragment-attributes
   #<<code
// output the right vertices for a triangle strip
switch (int(vert_id)) {
case 0:
  gl_Position = vec4(-1.0, -1.0, 0.0, 1.0);
  break;
case 1:
  gl_Position = vec4(+1.0, -1.0, 0.0, 1.0);
  break;
case 2:
  gl_Position = vec4(-1.0, +1.0, 0.0, 1.0);
  break;
default:
  gl_Position = vec4(+1.0, +1.0, 0.0, 1.0);
  break;
}

vec4 dir = unproj * gl_Position;
frag_dir = vec3(dir.xy / dir.z, 1.0);
code
   ))

(define directional-light-fragment-code
  (make-fragment-code
   "directional-light-fragment"
   #:includes (list light-fragment-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "unview" 'unview)
         (standard-uniform "" 'sampler2D "depth" 'depth)
         (standard-uniform "" 'sampler2D "material" 'material))
   #:program-uniforms
   (list (attribute "" 'vec3 "light_dir")
         (attribute "" 'vec3 "light_color")
         (attribute "" 'float "light_intensity"))
   #:in-attributes directional-light-fragment-attributes
   #<<code
float d = texelFetch(depth, ivec2(gl_FragCoord.xy), 0).r;
if (d == 0.0) discard;
float z = get_view_depth(d);
vec3 pos = frag_dir * z;

vec3 L = normalize(-light_dir * mat3(unview));
vec3 V = normalize(-pos);
output_light(pow(light_color, vec3(2.2)) * light_intensity, get_surface(material), L, V);
code
   ))

(define directional-light-program-code
  (make-program-code
   "directional-light-program"
   directional-light-vertex-code
   directional-light-fragment-code))

(define-singleton/context (directional-light-program)
  (log-pict3d-info "<engine> creating directional light program")
  (program-code->gl-program directional-light-program-code))

;; ===================================================================================================
;; Directional light shape passes

(define data (list->bytes '(0 1 2 3)))

(: vertex-ids (Vectorof Index))
(define vertex-ids #(0 1 2 2 1 3))

(: make-directional-light-shape-passes (-> directional-light-shape passes))
(define (make-directional-light-shape-passes a)
  (match-define (directional-light-shape _ fs e dv) a)
  
  (cond
    [(flags-subset? emitting-flag fs)
     (define color (flvector-copy e 0 3))
     (define intensity (flvector-ref e 3))
     (define uniforms
       (list (cons "light_dir" (uniform-float dv 3))
             (cons "light_color" (uniform-float color))
             (cons "light_intensity" (uniform-float intensity))))
     
     (passes
      (vector (shape-params directional-light-program uniforms #t GL_TRIANGLES
                            (vertices 4 data vertex-ids)))
      #()
      #()
      #()
      #())]
    [else  empty-passes]))

;; ===================================================================================================
;; Bounding box

(define directional-light-shape-rect
  (nonempty-flrect3 (flvector -inf.0 -inf.0 -inf.0)
                    (flvector +inf.0 +inf.0 +inf.0)))

;; ===================================================================================================
;; Transform

(: directional-light-shape-easy-transform (-> directional-light-shape Affine directional-light-shape))
(define (directional-light-shape-easy-transform a t) a)
