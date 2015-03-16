#lang typed/racket/base

;; Triangles, quads, rectangles

(require racket/unsafe/ops
         racket/list
         racket/vector
         racket/match
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         math/flonum
         "../../math/flv3.rkt"
         "../../math/flt3.rkt"
         "../../math/flrect3.rkt"
         "../../gl.rkt"
         "../../utils.rkt"
         "../types.rkt"
         "../utils.rkt"
         "../shader-code.rkt"
         "../serialize-vertices.rkt"
         "../draw-pass.rkt"
         "types.rkt"
         "flags.rkt")

(provide make-triangle-shape
         set-triangle-shape-color
         set-triangle-shape-emitted
         set-triangle-shape-material
         make-triangle-shape-passes
         triangle-shape-rect
         triangle-shape-easy-transform
         triangle-shape-line-intersect
         
         make-quad-shapes
         
         make-rectangle-shape
         set-rectangle-shape-color
         set-rectangle-shape-emitted
         set-rectangle-shape-material
         make-rectangle-shape-passes
         ;rectangle-shape-rect  ; already an accessor
         rectangle-shape-transform
         rectangle-shape-line-intersect
         
         rectangle-shape->triangle-shapes
         )

;; ===================================================================================================
;; Constructors

(: well-formed-flvectors? (-> (U FlVector (Vectorof FlVector)) Index Index Boolean))
(define (well-formed-flvectors? vs n i)
  (if (flvector? vs)
      (= i (flvector-length vs))
      (and (= n (vector-length vs))
           (for/and ([v  (in-vector vs)])
             (= i (flvector-length v))))))

(: make-triangle-shape (-> (Vectorof FlVector)
                           (U FlVector (Vectorof FlVector))
                           (U FlVector (Vectorof FlVector))
                           (U FlVector (Vectorof FlVector))
                           (U material (Vectorof material))
                           Boolean
                           triangle-shape))
(define (make-triangle-shape vs ns cs es ms back?)
  (cond [(not (well-formed-flvectors? vs 3 3))
         (raise-argument-error 'make-triangle-shape
                               "length-3 flvector, or length-3 vector of length-3 flvectors"
                               0 vs ns cs es ms back?)]
        [(not (well-formed-flvectors? ns 3 3))
         (raise-argument-error 'make-triangle-shape
                               "length-3 flvector, or length-3 vector of length-3 flvectors"
                               1 vs ns cs es ms back?)]
        [(not (well-formed-flvectors? cs 3 4))
         (raise-argument-error 'make-triangle-shape
                               "length-4 flvector, or length-3 vector of length-4 flvectors"
                               2 vs ns cs es ms back?)]
        [(not (well-formed-flvectors? es 3 4))
         (raise-argument-error 'make-triangle-shape
                               "length-3 flvector, or length-3 vector of length-4 flvectors"
                               3 vs ns cs es ms back?)]
        [(not (or (material? ms) (= 3 (vector-length ms))))
         (raise-argument-error 'make-triangle-shape
                               "material, or length-3 vector of materials"
                               4 vs ns cs es ms back?)]
        [else
         (define fs (flags-join visible-flag (colors-opacity-flag cs) (colors-emitting-flag es)))
         (triangle-shape (lazy-passes) fs vs ns cs es ms back?)]))

(: take-triangle (All (A) (-> (Vectorof A) Index Index Index (Vectorof A))))
(define (take-triangle vs i1 i2 i3)
  (vector (vector-ref vs i1)
          (vector-ref vs i2)
          (vector-ref vs i3)))

(: make-quad-shapes (-> (Vectorof FlVector)
                        (U FlVector (Vectorof FlVector))
                        (U FlVector (Vectorof FlVector))
                        (U FlVector (Vectorof FlVector))
                        (U material (Vectorof material))
                        Boolean
                        (List triangle-shape triangle-shape)))
(define (make-quad-shapes vs ns cs es ms back?)
  (cond [(not (well-formed-flvectors? vs 4 3))
         (raise-argument-error 'make-quad-shape
                               "length-3 flvector, or length-4 vector of length-3 flvectors"
                               0 vs ns cs es ms back?)]
        [(not (well-formed-flvectors? ns 4 3))
         (raise-argument-error 'make-quad-shape
                               "length-3 flvector, or length-4 vector of length-3 flvectors"
                               1 vs ns cs es ms back?)]
        [(not (well-formed-flvectors? cs 4 4))
         (raise-argument-error 'make-quad-shape
                               "length-4 flvector, or length-4 vector of length-4 flvectors"
                               2 vs ns cs es ms back?)]
        [(not (well-formed-flvectors? es 4 4))
         (raise-argument-error 'make-quad-shape
                               "length-3 flvector, or length-4 vector of length-4 flvectors"
                               3 vs ns cs es ms back?)]
        [(not (or (material? ms) (= 4 (vector-length ms))))
         (raise-argument-error 'make-quad-shape
                               "material, or length-4 vector of materials"
                               4 vs ns cs es ms back?)]
        [else
         (define fs (flags-join visible-flag (colors-opacity-flag cs) (colors-emitting-flag es)))
         (list (triangle-shape (lazy-passes)
                               fs
                               (take-triangle vs 0 1 2)
                               (if (vector? ns) (take-triangle ns 0 1 2) ns)
                               (if (vector? cs) (take-triangle cs 0 1 2) cs)
                               (if (vector? es) (take-triangle es 0 1 2) es)
                               (if (vector? ms) (take-triangle ms 0 1 2) ms)
                               back?)
               (triangle-shape (lazy-passes)
                               fs
                               (take-triangle vs 2 3 0)
                               (if (vector? ns) (take-triangle ns 2 3 0) ns)
                               (if (vector? cs) (take-triangle cs 2 3 0) cs)
                               (if (vector? es) (take-triangle es 2 3 0) es)
                               (if (vector? ms) (take-triangle ms 2 3 0) ms)
                               back?))]))

(: make-rectangle-shape (-> Nonempty-FlRect3 FlVector FlVector material Boolean rectangle-shape))
(define (make-rectangle-shape b c e m back?)
  (cond [(not (= 4 (flvector-length c)))
         (raise-argument-error 'make-rectangle-shape "length-4 flvector" 1 b c e m back?)]
        [(not (= 4 (flvector-length e)))
         (raise-argument-error 'make-rectangle-shape "length-4 flvector" 2 b c e m back?)]
        [else
         (define fs (flags-join visible-flag (color-opacity-flag c) (color-emitting-flag e)))
         (rectangle-shape (lazy-passes) fs b c e m back?)]))

;; ===================================================================================================
;; Set attributes

(: set-triangle-shape-color (-> triangle-shape (U FlVector (Vectorof FlVector)) triangle-shape))
(define (set-triangle-shape-color a cs)
  (cond [(not (well-formed-flvectors? cs 3 4))
         (raise-argument-error 'set-triangle-shape-color
                               "length-4 flvector, or length-3 vector of length-4 flvectors"
                               1 a cs)]
        [else
         (match-define (triangle-shape _ fs vs ns old-cs es ms back?) a)
         (cond [(equal? old-cs cs)  a]
               [else
                (define new-fs (flags-join (flags-subtract fs opacity-flags)
                                           (colors-opacity-flag cs)))
                (triangle-shape (lazy-passes) new-fs vs ns cs es ms back?)])]))

(: set-triangle-shape-emitted (-> triangle-shape (U FlVector (Vectorof FlVector)) triangle-shape))
(define (set-triangle-shape-emitted a es)
  (cond [(not (well-formed-flvectors? es 3 4))
         (raise-argument-error 'set-triangle-shape-emitted
                               "length-4 flvector, or length-3 vector of length-4 flvectors"
                               1 a es)]
        [else
         (match-define (triangle-shape _ fs vs ns cs old-es ms back?) a)
         (cond [(equal? old-es es)  a]
               [else
                (define new-fs (flags-join (flags-subtract fs emitting-flags)
                                           (colors-emitting-flag es)))
                (triangle-shape (lazy-passes) new-fs vs ns cs es ms back?)])]))

(: set-triangle-shape-material (-> triangle-shape (U material (Vectorof material)) triangle-shape))
(define (set-triangle-shape-material a ms)
  (cond [(not (or (material? ms) (= 3 (vector-length ms))))
         (raise-argument-error 'set-triangle-shape-material
                               "material, or length-3 vector of materials"
                               1 a ms)]
        [else
         (match-define (triangle-shape _ fs vs ns cs es old-ms back?) a)
         (cond [(equal? old-ms ms)  a]
               [else  (triangle-shape (lazy-passes) fs vs ns cs es ms back?)])]))

(: set-rectangle-shape-color (-> rectangle-shape FlVector rectangle-shape))
(define (set-rectangle-shape-color a c)
  (cond [(not (= (flvector-length c) 4))
         (raise-argument-error 'set-rectangle-shape-color "length-4 flvector" 1 a c)]
        [else
         (match-define (rectangle-shape _ fs b old-c e m inside?) a)
         (cond [(equal? old-c c)  a]
               [else
                (define new-fs (flags-join (flags-subtract fs opacity-flags)
                                           (color-opacity-flag c)))
                (rectangle-shape (lazy-passes) new-fs b c e m inside?)])]))

(: set-rectangle-shape-emitted (-> rectangle-shape FlVector rectangle-shape))
(define (set-rectangle-shape-emitted a e)
  (cond [(not (= (flvector-length e) 4))
         (raise-argument-error 'set-rectangle-shape-emitted "length-4 flvector" 1 a e)]
        [else
         (match-define (rectangle-shape _ fs b c old-e m inside?) a)
         (cond [(equal? old-e e)  a]
               [else
                (define new-fs (flags-join (flags-subtract fs emitting-flags)
                                           (color-emitting-flag e)))
                (rectangle-shape (lazy-passes) new-fs b c e m inside?)])]))

(: set-rectangle-shape-material (-> rectangle-shape material rectangle-shape))
(define (set-rectangle-shape-material a m)
  (match-define (rectangle-shape _ fs b c e old-m inside?) a)
  (cond [(equal? old-m m)  a]
        [else  (rectangle-shape (lazy-passes) fs b c e m inside?)]))

;; ===================================================================================================
;; Program for pass 1: material

(define polygon-mat-fragment-attributes
  (list (attribute "smooth" 'vec4 "frag_position")
        (attribute "smooth" 'vec3 "frag_normal")
        (attribute "smooth" 'float "frag_roughness")))

(define polygon-mat-vertex-code
  (make-vertex-code
   "polygon-mat-vertex"
   #:includes
   (list model-vertex-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "view" 'view)
         (standard-uniform "" 'mat4 "unview" 'unview)
         (standard-uniform "" 'mat4 "proj" 'proj))
   #:in-attributes
   (list (attribute "" 'vec4/bytes "vert_normal_roughness")
         (attribute "" 'vec3 "vert_position"))
   #:out-attributes
   polygon-mat-fragment-attributes
   #<<code
mat4x3 model = get_model_transform();
mat4x3 unmodel = affine_inverse(model);
vec4 position = view * (a2p(model) * vec4(vert_position,1));
gl_Position = proj * position;
vec3 normal = normalize(vert_normal_roughness.xyz - vec3(127.0));
vec4 norm = (vec4(normal,0) * a2p(unmodel)) * unview;
frag_position = position;
frag_normal = normalize(norm.xyz);
frag_roughness = vert_normal_roughness.w / 255;
code
   ))

(define polygon-mat-fragment-code
  (make-fragment-code
   "polygon-mat-fragment"
   #:includes
   (list output-mat-fragment-code)
   #:in-attributes
   polygon-mat-fragment-attributes
   #<<code
output_mat(frag_normal, frag_roughness, frag_position.z);
code
   ))

(define polygon-mat-program-code
  (make-program-code
   "polygon-mat-program"
   polygon-mat-vertex-code
   polygon-mat-fragment-code))

(define-singleton/context (polygon-mat-program)
  (log-pict3d-info "<engine> creating polygon material pass program")
  (program-code->gl-program polygon-mat-program-code))

;; ===================================================================================================
;; Program for pass 2: color

(define polygon-draw-vertex-attributes
  (list (attribute "" 'vec3 "vert_position")
        (attribute "" 'vec4/bytes "vert_rcolor")    ; vec4(r, g, b, a)
        (attribute "" 'vec4/bytes "vert_ecolor")    ; vec4(r, g, intensity.lo, intensity.hi)
        (attribute "" 'vec3/bytes "vert_material")  ; vec3(ambient, diffuse, specular)
        ))

(define polygon-draw-fragment-attributes
  (list (attribute "smooth" 'vec4 "frag_position")
        (attribute "smooth" 'vec3 "frag_rcolor")
        (attribute "smooth" 'vec3 "frag_ecolor")
        (attribute "smooth" 'float "frag_alpha")
        (attribute "smooth" 'float "frag_ambient")
        (attribute "smooth" 'float "frag_diffuse")
        (attribute "smooth" 'float "frag_specular")))

(define polygon-draw-vertex-code
  (make-vertex-code
   "polygon-draw-vertex"
   #:includes
   (list model-vertex-code
         unpack-emitted-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "view" 'view)
         (standard-uniform "" 'mat4 "proj" 'proj))
   #:in-attributes
   polygon-draw-vertex-attributes
   #:out-attributes
   polygon-draw-fragment-attributes
   #<<code
mat4x3 model = get_model_transform();
vec4 position = view * (a2p(model) * vec4(vert_position,1));
gl_Position = proj * position;
frag_position = position;
frag_rcolor = pow(vert_rcolor.rgb / 255, vec3(2.2));
frag_alpha = vert_rcolor.a / 255;
frag_ecolor = unpack_emitted(vert_ecolor);
frag_ambient = vert_material.x / 255;
frag_diffuse = vert_material.y / 255;
frag_specular = vert_material.z / 255;
code
    ))

(define polygon-opaq-fragment-code
  (make-fragment-code
   "polygon-opaq-fragment"
   #:includes
   (list output-opaq-fragment-code)
   #:standard-uniforms
   (list (standard-uniform "" 'vec3 "ambient" 'ambient)
         (standard-uniform "" 'sampler2D "diffuse" 'diffuse)
         (standard-uniform "" 'sampler2D "specular" 'specular))
   #:in-attributes
   polygon-draw-fragment-attributes
   #<<code
vec3 diff = texelFetch(diffuse, ivec2(gl_FragCoord.xy), 0).rgb;
vec3 spec = texelFetch(specular, ivec2(gl_FragCoord.xy), 0).rgb;
vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
vec3 color = frag_ecolor + frag_rcolor * light;
output_opaq(color, frag_position.z);
code
   ))

(define polygon-tran-fragment-code
  (make-fragment-code
   "polygon-tran-fragment"
   #:includes
   (list output-tran-fragment-code)
   #:standard-uniforms
   (list (standard-uniform "" 'vec3 "ambient" 'ambient)
         (standard-uniform "" 'sampler2D "diffuse" 'diffuse)
         (standard-uniform "" 'sampler2D "specular" 'specular))
   #:in-attributes
   polygon-draw-fragment-attributes
   #<<code
vec3 diff = texelFetch(diffuse, ivec2(gl_FragCoord.xy), 0).rgb;
vec3 spec = texelFetch(specular, ivec2(gl_FragCoord.xy), 0).rgb;
vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
vec3 color = frag_ecolor + frag_rcolor * light;
output_tran(color, frag_alpha, frag_position.z);
code
   ))

(define polygon-opaq-program-code
  (make-program-code
   "polygon-opaq-program"
   polygon-draw-vertex-code
   polygon-opaq-fragment-code))

(define polygon-tran-program-code
  (make-program-code
   "polygon-tran-program"
   polygon-draw-vertex-code
   polygon-tran-fragment-code))

(define-singleton/context (polygon-opaq-program)
  (log-pict3d-info "<engine> creating polygon opaque color pass program")
  (program-code->gl-program polygon-opaq-program-code))

(define-singleton/context (polygon-tran-program)
  (log-pict3d-info "<engine> creating polygon transparent color pass program")
  (program-code->gl-program polygon-tran-program-code))

;; ===================================================================================================
;; Triangle shape passes

(: make-triangle-shape-passes (-> triangle-shape passes))
(define (make-triangle-shape-passes a)
  (match-define (triangle-shape _ fs vs ns cs es ms back?) a)
  (define-values (start stop step)
    (if back?
        (values 2 -1 -1)
        (values 0 3 1)))
  
  (define mat-struct-size (program-code-vao-size polygon-mat-program-code))
  (define mat-data (make-bytes (* 3 mat-struct-size)))
  (for/fold ([i : Nonnegative-Fixnum  0]) ([j  (in-range start stop step)])
    (define v (vector-ref vs j))
    (define n (if (vector? ns) (vector-ref ns j) ns))
    (define m (if (vector? ms) (vector-ref ms j) ms))
    (let* ([i  (serialize-normal/bytes mat-data i n back?)]
           [i  (serialize-float/byte mat-data i (material-roughness m))]
           [i  (serialize-vec3 mat-data i v)])
      i))
  
  (define opaq-struct-size (program-code-vao-size polygon-opaq-program-code))
  (define draw-data (make-bytes (* 3 opaq-struct-size)))
  (for/fold ([i : Nonnegative-Fixnum  0]) ([j  (in-range start stop step)])
    (define v (vector-ref vs j))
    (define c (if (vector? cs) (vector-ref cs j) cs))
    (define e (if (vector? es) (vector-ref es j) es))
    (define m (if (vector? ms) (vector-ref ms j) ms))
    (let* ([i  (serialize-vec3 draw-data i v)]
           [i  (serialize-vec4/bytes draw-data i c)]
           [i  (serialize-emitted/bytes draw-data i e)]
           [i  (serialize-material-reflectances/bytes draw-data i m)])
      i))
  
  (if (flags-subset? transparent-flag fs)
      (passes
       #()
       #()
       #()
       (vector (shape-params polygon-mat-program empty #f GL_TRIANGLES (vertices 3 mat-data #f)))
       (vector (shape-params polygon-tran-program empty #f GL_TRIANGLES (vertices 3 draw-data #f))))
      (passes
       #()
       (vector (shape-params polygon-mat-program empty #f GL_TRIANGLES (vertices 3 mat-data #f)))
       (vector (shape-params polygon-opaq-program empty #f GL_TRIANGLES (vertices 3 draw-data #f)))
       #()
       #())))

;; ===================================================================================================
;; Rectangle shape passes

(: make-rectangle-shape-passes (-> rectangle-shape passes))
(define (make-rectangle-shape-passes a)
  (define as (rectangle-shape->triangle-shapes a))
  (define ps (map make-triangle-shape-passes as))
  (passes
   #()
   (apply vector-append (map passes-opaque-material ps))
   (apply vector-append (map passes-opaque-color ps))
   (apply vector-append (map passes-transparent-material ps))
   (apply vector-append (map passes-transparent-color ps))))

;; ===================================================================================================
;; Bounding box

(: triangle-shape-rect (-> triangle-shape Nonempty-FlRect3))
(define (triangle-shape-rect a)
  (assert (flv3rect (triangle-shape-vertices a)) nonempty-flrect3?))

;; ===================================================================================================
;; Transform

(: triangle-shape-easy-transform (-> triangle-shape Affine triangle-shape))
(define (triangle-shape-easy-transform a affine-t)
  (match-define (triangle-shape passes fs vs ns cs es ms back?) a)
  (define t (affine-transform affine-t))
  (define consistent? (flt3consistent? t))
  (define transform-pos (λ ([v : FlVector]) (flt3apply/pos t v)))
  (define transform-norm (λ ([n : FlVector])
                           (if consistent?
                               (flt3apply/nrm t n)
                               (flv3neg (flt3apply/nrm t n)))))
  (triangle-shape (lazy-passes)
                  fs
                  (vector-map transform-pos vs)
                  (if (vector? ns) (vector-map transform-norm ns) (transform-norm ns))
                  cs es ms
                  (if consistent? back? (not back?))))

(: rectangle-shape-transform (-> rectangle-shape Affine (Listof triangle-shape)))
(define (rectangle-shape-transform a t)
  (map (λ ([a : triangle-shape])
         (triangle-shape-easy-transform a t))
       (rectangle-shape->triangle-shapes a)))

;; ===================================================================================================
;; Conversions

(: rectangle-shape->triangle-shapes (-> rectangle-shape (Listof triangle-shape)))
(define (rectangle-shape->triangle-shapes a)
  (match-define (rectangle-shape _ fs b c e m inside?) a)
  (match-define (vector v1 v5 v4 v8 v2 v6 v3 v7) (flrect3-corners b))
  (append*
   (list (make-quad-shapes (vector v4 v3 v2 v1) -z-flv3 c e m inside?)
         (make-quad-shapes (vector v5 v6 v7 v8) +z-flv3 c e m inside?)
         (make-quad-shapes (vector v1 v2 v6 v5) -y-flv3 c e m inside?)
         (make-quad-shapes (vector v3 v4 v8 v7) +y-flv3 c e m inside?)
         (make-quad-shapes (vector v4 v1 v5 v8) -x-flv3 c e m inside?)
         (make-quad-shapes (vector v2 v3 v7 v6) +x-flv3 c e m inside?))))

;; ===================================================================================================
;; Ray intersection

;; The maximum and minimum barycentric coordinates should be 0.0 and 1.0, but because of floating-
;; point error and whatnot, we might miss if we use those bounds, so we'll fudge a bit
;; We end up testing against slightly larger trinagles with somewhat cut off corners
(define fudge (* 128 epsilon.0))
(define coord-min (- fudge))
(define coord-max (+ 1.0 fudge))

(: triangle-intersect-time (-> FlVector FlVector FlVector FlVector FlVector (U #f Flonum)))
;; Moller-Trumbore
(define (triangle-intersect-time v0 v1 v2 o d)
  (define e1 (flv3- v1 v0))
  (define e2 (flv3- v2 v0))
  (define p (flv3cross d e2))
  (define det (flv3dot e1 p))
  (cond
    [(<= det +max-subnormal.0)  #f]
    [else
     ;; Compute first barycentric coordinate u and test
     (define t (flv3- o v0))
     (define u (/ (flv3dot t p) det))
     (cond
       [(or (< u coord-min) (> u coord-max))  #f]
       [else
        ;; Compute the other two barycentric coordinates v,w and test
        (define q (flv3cross t e1))
        (define v (/ (flv3dot d q) det))
        (define w (- 1.0 u v))
        (cond
          [(or (< (min v w) coord-min) (> (max v w) coord-max))  #f]
          [else
           (/ (flv3dot e2 q) det)])])]))

(: triangle-shape-line-intersect (-> triangle-shape FlVector FlVector (U #f line-hit)))
;; Moller-Trumbore
(define (triangle-shape-line-intersect a o d)
  (define vs (triangle-shape-vertices a))
  (match-define (vector v0 v1 v2) vs)
  (let-values ([(v0 v1)  (if (triangle-shape-back? a) (values v1 v0) (values v0 v1))])
    (define t (triangle-intersect-time v0 v1 v2 o d))
    (cond
      [(not t)  #f]
      [else
       (define back? (triangle-shape-back? a))
       (line-hit t
                 (λ () (flv3fma d t o))
                 (λ ()
                   (define norm (flv3polygon-normal vs))
                   (and norm (if back? (flv3neg norm) norm))))])))

(: rectangle-shape-line-intersect (-> rectangle-shape FlVector FlVector (U #f line-hit)))
(define (rectangle-shape-line-intersect a v dv)
  (define b (rectangle-shape-rect a))
  (define-values (tmin tmax) (flrect3-line-intersects b v dv))
  (define inside? (rectangle-shape-inside? a))
  (define t (if inside? tmax tmin))
  (cond [(not t)  #f]
        [else
         (define (lazy-point)
           (flrect3-closest-point b (flv3fma dv t v)))
         
         (: lazy-normal (-> FlVector))
         (define (lazy-normal)
           (define norm (assert (flrect3-point-normal b (line-hit-point h)) values))
           (if inside? (flv3neg norm) norm))
         
         (: h line-hit)
         (define h
           (line-hit t lazy-point lazy-normal))
         
         h]))
