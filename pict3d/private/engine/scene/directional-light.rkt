#lang typed/racket/base

;; Directional light impostors: vertex shader outputs a quad; fragment shader computes light

(require racket/match
         racket/flonum
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         "../../math.rkt"
         "../../gl.rkt"
         "../../utils.rkt"
         "../utils.rkt"
         "../shader-code.rkt"
         "../types.rkt"
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

(: make-directional-light-shape (-> FlV4 FlV3 directional-light-shape))
(define (make-directional-light-shape e dv)
  (define fs (flags-join invisible-flag transparent-flag (color-emitting-flag e)))
  (directional-light-shape (lazy-passes) fs e dv))

;; ===================================================================================================
;; Set attributes

(: set-directional-light-shape-emitted (-> directional-light-shape FlV4 directional-light-shape))
(define (set-directional-light-shape-emitted a e)
  (match-define (directional-light-shape _ fs _ dv) a)
  (define new-fs (flags-join (flags-subtract fs emitting-flags)
                             (color-emitting-flag e)))
  (directional-light-shape (lazy-passes) new-fs e dv))

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
     (define-values (r g b i)
       (call/flv4-values e values))
     
     (define uniforms
       (list (cons "light_dir" (uniform-float (call/flv3-values dv flvector) 3))
             (cons "light_color" (uniform-float (flvector r g b)))
             (cons "light_intensity" (uniform-float i))))
     
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
  (flrect3 (flv3 -inf.0 -inf.0 -inf.0)
           (flv3 +inf.0 +inf.0 +inf.0)))

;; ===================================================================================================
;; Transform

(: directional-light-shape-easy-transform (-> directional-light-shape FlAffine3
                                              directional-light-shape))
(define (directional-light-shape-easy-transform a t) a)
