#lang typed/racket/base

;; Point light impostors: vertex shader outputs a quad; fragment shader computes light

(require racket/unsafe/ops
         racket/match
         racket/list
         racket/flonum
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         "../../math/flv3.rkt"
         "../../math/flt3.rkt"
         "../../math/flrect3.rkt"
         "../../utils.rkt"
         "../draw-pass.rkt"
         "../types.rkt"
         "../utils.rkt"
         "../shader-code.rkt"
         "../serialize-vertices.rkt"
         "types.rkt"
         "flags.rkt")

(provide make-point-light-shape
         make-point-light-shape-passes
         set-point-light-shape-emitted
         point-light-shape-rect
         point-light-shape-easy-transform
         )

;; ===================================================================================================
;; Constructor

(: make-point-light-shape (-> FlVector FlVector Flonum Flonum point-light-shape))
(define (make-point-light-shape e v r0 r1)
  (cond [(not (= 4 (flvector-length e)))
         (raise-argument-error 'make-point-light-shape "length-4 flvector"
                               0 e v r0 r1)]
        [(not (= 3 (flvector-length v)))
         (raise-argument-error 'make-point-light-shape "length-3 flvector"
                               1 e v r0 r1)]
        [else
         (define fs (flags-join invisible-flag transparent-flag (color-emitting-flag e)))
         (point-light-shape (lazy-passes) fs e v r0 r1)]))

;; ===================================================================================================
;; Set attributes

(: set-point-light-shape-emitted (-> point-light-shape FlVector point-light-shape))
(define (set-point-light-shape-emitted a e)
  (cond [(not (= 4 (flvector-length e)))
         (raise-argument-error 'set-point-light-shape-emitted "length-4 flvector"
                               1 a e)]
        [else
         (match-define (point-light-shape _ fs old-e v r0 r1) a)
         (cond [(equal? old-e e)  a]
               [else
                (define new-fs (flags-join (flags-subtract fs emitting-flags)
                                           (color-emitting-flag e)))
                (point-light-shape (lazy-passes) new-fs e v r0 r1)])]))

;; ===================================================================================================
;; Program for pass 0: light

(define point-light-vertex-attributes
  (list (attribute "" 'vec3 "vert_position")
        (attribute "" 'vec3 "vert_intensity_radii")
        (attribute "" 'vec3/bytes "vert_color")
        (attribute "" 'float/byte "vert_id")))

(define point-light-fragment-attributes
  (list (attribute "flat" 'vec3 "frag_position")
        (attribute "flat" 'float "frag_min_radius")
        (attribute "flat" 'float "frag_max_radius")
        (attribute "flat" 'vec3 "frag_intensity")
        (attribute "smooth" 'float "frag_is_degenerate")
        (attribute "smooth" 'vec3 "frag_dir")))

(define point-light-vertex-code
  (make-vertex-code
   "point-light-vertex"
   #:includes
   (list output-impostor-vertex-code
         model-vertex-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "view" 'view)
         (standard-uniform "" 'mat4 "proj" 'proj)
         (standard-uniform "" 'mat4 "unproj" 'unproj))
   #:in-attributes
   point-light-vertex-attributes
   #:out-attributes
   point-light-fragment-attributes
   #<<code
mat4x3 model = get_model_transform();
mat4 trans = view * a2p(model);

float min_radius = vert_intensity_radii.y;
float max_radius = vert_intensity_radii.z;
vec3 wmin = vert_position - vec3(max_radius);
vec3 wmax = vert_position + vec3(max_radius);
frag_position = (trans * vec4(vert_position,1)).xyz;
frag_min_radius = min_radius;
frag_max_radius = max_radius;
frag_intensity = pow(vert_color / 255, vec3(2.2)) * vert_intensity_radii.x;
frag_is_degenerate = output_impostor_vertex(trans, proj, wmin, wmax, int(vert_id));

vec4 dir = unproj * gl_Position;
frag_dir = vec3(dir.xy / dir.z, 1.0);
code
   ))

(define point-light-fragment-code
  (make-fragment-code
   "point-light-vertex"
   #:includes (list light-fragment-code)
   #:standard-uniforms (list (standard-uniform "" 'sampler2D "depth" 'depth)
                             (standard-uniform "" 'sampler2D "material" 'material))
   #:in-attributes point-light-fragment-attributes
   #<<code
// all fragments should discard if this one does
if (frag_is_degenerate > 0.0) discard;

float d = texelFetch(depth, ivec2(gl_FragCoord.xy), 0).r;
if (d == 0.0) discard;
float z = get_view_depth(d);
vec3 vpos = frag_dir * z;

vec3 D = frag_position - vpos;
float dist = length(D);
if (dist < frag_min_radius) discard;
if (dist > frag_max_radius) discard;  
vec3 L = D / dist;
vec3 V = normalize(-vpos);

vec3 light = attenuate_invsqr(frag_intensity, dist);
output_light(light, get_surface(material), L, V);
code
   ))

(define point-light-program-code
  (make-program-code
   "point-light-program"
   point-light-vertex-code
   point-light-fragment-code))

(define-singleton/context (point-light-program)
  (log-pict3d-info "<engine> creating point light program")
  (program-code->gl-program point-light-program-code))

;; ===================================================================================================
;; Point light shape passes

(: vertex-ids (Vectorof Index))
(define vertex-ids #(0 1 2 2 1 3))

(: make-point-light-shape-passes (-> point-light-shape passes))
(define (make-point-light-shape-passes a)
  (match-define (point-light-shape _ fs e v r0 r1) a)
  (cond
    [(flags-subset? emitting-flag fs)
     (define size (program-code-vao-size point-light-program-code))
     (define data (make-bytes (* 4 size)))
     (define data-ptr (u8vector->cpointer data))
     (let* ([i  (serialize-vec3 data 0 v)]
            [i  (serialize-float data i (flvector-ref e 3))]
            [i  (serialize-float data i r0)]
            [i  (serialize-float data i r1)]
            [i  (serialize-vec3/bytes data i e)])
       (for ([k : Nonnegative-Fixnum  (in-range 1 4)])
         (memcpy data-ptr (unsafe-fx* k size) data-ptr size _byte)
         (bytes-set! data (unsafe-fx+ (unsafe-fx* k size) i) k)))
     
     (passes
      (vector (shape-params point-light-program empty #t GL_TRIANGLES (vertices 4 data vertex-ids)))
      #()
      #()
      #()
      #())]
    [else  empty-passes]))

;; ===================================================================================================
;; Bounding box

(: point-light-shape-rect (-> point-light-shape Nonempty-FlRect3))
(define (point-light-shape-rect a)
  (define p (point-light-shape-position a))
  (define radius (point-light-shape-max-radius a))
  (define r (flvector radius radius radius))
  (nonempty-flrect3 (flv3- p r) (flv3+ p r)))

;; ===================================================================================================
;; Transform

(: point-light-shape-easy-transform (-> point-light-shape Affine point-light-shape))
(define (point-light-shape-easy-transform a t)
  (match-define (point-light-shape passes fs e v r0 r1) a)
  (define new-v (flt3apply/pos (affine-transform t) v))
  (point-light-shape (lazy-passes) fs e new-v r0 r1))
