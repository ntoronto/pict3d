#lang typed/racket/base

(require racket/unsafe/ops
         racket/list
         racket/vector
         racket/match
         racket/promise
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         math/flonum
         math/base
         "../math.rkt"
         "../memo.rkt"
         "../engine.rkt"
         "../soup.rkt"
         "../utils.rkt"
         "types.rkt")

(provide make-triangle-shape
         make-triangle-mesh-shape
         make-quad-triangle-mesh-shape
         (struct-out triangle-mesh-shape)
         faces->triangle-mesh-shapes
         get-triangle-mesh-shape-passes
         get-triangle-mesh-shape-bbox
         triangle-mesh-shape-ray-intersect
         triangle-mesh-shape-tessellate
         triangle-mesh-shape-deform)

(add-engine-debug-shapes! '(triangle-mesh))

(define max-chunk-size 16)

;; ===================================================================================================
;; Shape data types

(struct triangle-mesh-shape shape
  ([vtxs : (Vectorof vtx)]
   [idxs : (Vectorof Index)]
   [back? : Boolean])
  #:transparent)

;; ===================================================================================================
;; Constructors

(: make-triangle-mesh-shape (-> (Vectorof vtx) (Vectorof Index) Boolean triangle-mesh-shape))
(define (make-triangle-mesh-shape vtxs faces back?)
  (define n (vector-length vtxs))
  (define m (vector-length faces))
  (unless (and (> m 0) (zero? (modulo m 3)))
    (raise-argument-error 'make-triangle-mesh-shape
                          "(Vectorof Index) with positive, multiple-of-3 length"
                          1 vtxs faces back?))
  (for ([i  (in-range m)])
    (unless (< (vector-ref faces i) n)
      (raise-argument-error 'make-triangle-mesh-shape
                            (format "(Vectorof Index) with all indexes < ~a" n)
                            1 vtxs faces back?)))
  (triangle-mesh-shape (lazy-passes) triangle-mesh-shape-functions vtxs faces back?))

(: triangle-indexes (Vectorof Index))
(define triangle-indexes (vector 0 1 2))

(: make-triangle-shape (-> vtx vtx vtx Boolean triangle-mesh-shape))
(define (make-triangle-shape v1 v2 v3 back?)
  (triangle-mesh-shape (lazy-passes) triangle-mesh-shape-functions
                       (vector v1 v2 v3) triangle-indexes back?))

(define quad-idxs-1 ((inst vector Index) 0 1 2 2 3 0))  ; vtx1 and vtx3 closer
(define quad-idxs-2 ((inst vector Index) 1 2 3 3 0 1))  ; vtx2 and vtx4 closer

(: make-quad-triangle-mesh-shape (-> (Vectorof vtx) Boolean triangle-mesh-shape))
(define (make-quad-triangle-mesh-shape vtxs back?)
  (match-define (vector vtx1 vtx2 vtx3 vtx4) vtxs)
  (define v1 (vtx-position vtx1))
  (define v2 (vtx-position vtx2))
  (define v3 (vtx-position vtx3))
  (define v4 (vtx-position vtx4))
  (define idxs (if (< (* (flv3dist v1 v3) (- 1.0 1e-8)) (flv3dist v2 v4)) quad-idxs-1 quad-idxs-2))
  (make-triangle-mesh-shape vtxs idxs back?))

;; ===================================================================================================
;; Set attributes

(: set-triangle-mesh-shape-color (-> shape FlV4 triangle-mesh-shape))
(define (set-triangle-mesh-shape-color s c)
  (match-define (triangle-mesh-shape _ _ vtxs faces back?) s)
  (triangle-mesh-shape (lazy-passes) triangle-mesh-shape-functions
                       (vector-map (λ ([v : vtx]) (set-vtx-color v c)) vtxs)
                       faces back?))

(: set-triangle-mesh-shape-emitted (-> shape FlV4 triangle-mesh-shape))
(define (set-triangle-mesh-shape-emitted s e)
  (match-define (triangle-mesh-shape _ _ vtxs faces back?) s)
  (triangle-mesh-shape (lazy-passes) triangle-mesh-shape-functions
                       (vector-map (λ ([v : vtx]) (set-vtx-emitted v e)) vtxs)
                       faces back?))

(: set-triangle-mesh-shape-material (-> shape FlV4 triangle-mesh-shape))
(define (set-triangle-mesh-shape-material s m)
  (match-define (triangle-mesh-shape _ _ vtxs faces back?) s)
  (triangle-mesh-shape (lazy-passes) triangle-mesh-shape-functions
                       (vector-map (λ ([v : vtx]) (set-vtx-material v m)) vtxs)
                       faces back?))

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
   (list model-vertex-code
         pack-unpack-normal-code)
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
vec4 norm = (vec4(unpack_normal(vert_normal_roughness.xyz),0) * a2p(unmodel)) * unview;
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
;; Triangle mesh shape passes

(: get-triangle-mesh-shape-passes (-> shape passes))
(define (get-triangle-mesh-shape-passes s)
  (match-define (triangle-mesh-shape _ _ vtxs faces back?) s)
  (define n (vector-length vtxs))
  
  (define mat-struct-size (program-code-vao-size polygon-mat-program-code))
  (define mat-data (make-bytes (* n mat-struct-size)))
  (for/fold ([i : Nonnegative-Fixnum  0]) ([j  (in-range n)])
    (define vert (vector-ref vtxs j))
    (define v (vtx-position vert))
    (define n (vtx-normal vert))
    (define m (vtx-material vert))
    (let* ([i  (serialize-normal/bytes mat-data i n back?)]
           [i  (serialize-float/byte mat-data i (unsafe-flv4-ref m 3))]
           [i  (serialize-vec3 mat-data i v)])
      i))
  
  (define debug? (member 'triangle-mesh (current-engine-debug-shapes)))
  (define r* (if debug? (+ 0.5 (* 0.5 (random))) 1.0))
  (define g* (if debug? (+ 0.5 (* 0.5 (random))) 1.0))
  (define b* (if debug? (+ 0.5 (* 0.5 (random))) 1.0))
  
  (define opaq-struct-size (program-code-vao-size polygon-opaq-program-code))
  (define draw-data (make-bytes (* n opaq-struct-size)))
  (for/fold ([i : Nonnegative-Fixnum  0]) ([j  (in-range n)])
    (define vert (vector-ref vtxs j))
    (define v (vtx-position vert))
    (define c (cond [debug?  (call/flv4-values (vtx-color vert)
                               (λ (r g b a)
                                 (flv4 (* r r*) (* g g*) (* b b*) a)))]
                    [else  (vtx-color vert)]))
    (define e (vtx-emitted vert))
    (define m (vtx-material vert))
    (let* ([i  (serialize-vec3 draw-data i v)]
           [i  (serialize-vec4/bytes draw-data i c)]
           [i  (serialize-emitted/bytes draw-data i e)]
           [i  (serialize-material-reflectances/bytes draw-data i m)])
      i))
  
  (define transparent?
    (for/or : Boolean ([i  (in-range n)])
      (< (flv4-ref (vtx-color (vector-ref vtxs i)) 3) 1.0)))
  
  (define js (if back? (vector-reverse faces) faces))
  
  (if transparent?
      (passes
       #()
       #()
       #()
       (vector (shape-params polygon-mat-program empty #f GL_TRIANGLES (vertices n mat-data js)))
       (vector (shape-params polygon-tran-program empty #f GL_TRIANGLES (vertices n draw-data js))))
      (passes
       #()
       (vector (shape-params polygon-mat-program empty #f GL_TRIANGLES (vertices n mat-data js)))
       (vector (shape-params polygon-opaq-program empty #f GL_TRIANGLES (vertices n draw-data js)))
       #()
       #())))

;; ===================================================================================================
;; Bounding box

(: get-triangle-mesh-shape-bbox (-> shape FlAffine3 bbox))
(define (get-triangle-mesh-shape-bbox s t)
  (match-define (triangle-mesh-shape _ _ vtxs faces back?) s)
  (define-values (xmin xmax ymin ymax zmin zmax)
    (for/fold ([xmin : Flonum  +inf.0]
               [xmax : Flonum  -inf.0]
               [ymin : Flonum  +inf.0]
               [ymax : Flonum  -inf.0]
               [zmin : Flonum  +inf.0]
               [zmax : Flonum  -inf.0])
              ([v  (in-vector vtxs)])
      (call/flv3-values (flt3apply/pos t (vtx-position v))
        (λ (x y z)
          (values (min x xmin) (max x xmax)
                  (min y ymin) (max y ymax)
                  (min z zmin) (max z zmax))))))
  (bbox (flrect3 (flv3 xmin ymin zmin) (flv3 xmax ymax zmax))
        0.0))

;; ===================================================================================================
;; Transform

(: triangle-mesh-shape-transform (-> shape FlAffine3 triangle-mesh-shape))
(define (triangle-mesh-shape-transform s t)
  (match-define (triangle-mesh-shape _ _ vtxs idxs back?) s)
  
  (define consistent? (flt3consistent? t))
  (define new-back? (if consistent? back? (not back?)))
  
  (: transform-vtx (-> vtx vtx))
  (define (transform-vtx vert)
    (define v (vtx-position vert))
    (define n (vtx-normal vert))
    (set-vtx-vecs
     vert
     (flt3apply/pos t v)
     (let ([n  (flt3apply/norm t n)])
       (if consistent?
           (if n n +z-flv3)
           (if n (flv3neg n) +z-flv3)))))
  
  (triangle-mesh-shape (lazy-passes) triangle-mesh-shape-functions
                       (vector-map transform-vtx vtxs) idxs new-back?))

;; ===================================================================================================
;; Ray intersection

;; The maximum and minimum barycentric coordinates should be 0.0 and 1.0, but because of floating-
;; point error and whatnot, we might miss if we use those bounds, so we'll fudge a bit
;; We end up testing against somewhat larger triangles with slightly cut off corners
(define fudge (* 128 epsilon.0))
(define coord-min (- fudge))
(define coord-max (+ 1.0 fudge))

(: triangle-intersect-time (-> FlV3 FlV3 FlV3 FlV3 FlV3 (U #f Flonum)))
;; Moller-Trumbore
(define (triangle-intersect-time v0 v1 v2 o d)
  (let* ([e1 : FlV3  (flv3- v1 v0)]
         [e2 : FlV3  (flv3- v2 v0)]
         [p : FlV3  (flv3cross d e2)])
    (define det (flv3dot e1 p))
    (cond
      [(<= det +max-subnormal.0)  #f]
      [else
       ;; Compute first barycentric coordinate u and test
       (let ([t : FlV3  (flv3- o v0)])
         (define u (/ (flv3dot t p) det))
         (cond
           [(or (< u coord-min) (> u coord-max))  #f]
           [else
            ;; Compute the other two barycentric coordinates v,w and test
            (let ([q : FlV3  (flv3cross t e1)])
              (define v (/ (flv3dot d q) det))
              (define w (- 1.0 u v))
              (cond
                [(or (< (min v w) coord-min) (> (max v w) coord-max))  #f]
                [else
                 (/ (flv3dot e2 q) det)]))]))])))

(: triangle-mesh-shape-ray-intersect (-> shape FlV3 FlV3 Nonnegative-Flonum
                                         (Values (U #f Nonnegative-Flonum)
                                                 (U #f (Promise trace-data)))))
(define (triangle-mesh-shape-ray-intersect s o d max-time)
  (with-asserts ([s  triangle-mesh-shape?])
    (define vtxs (triangle-mesh-shape-vtxs s))
    (define idxs (triangle-mesh-shape-idxs s))
    (define back? (triangle-mesh-shape-back? s))
    (define-values (time v1 v2 v3)
      (for/fold ([best-time : (U #f Nonnegative-Flonum)  #f]
                 [best-v1 : FlV3  zero-flv3]
                 [best-v2 : FlV3  zero-flv3]
                 [best-v3 : FlV3  zero-flv3])
                ([i  (in-range 0 (vector-length idxs) 3)])
        (define v1 (vtx-position (vector-ref vtxs (vector-ref idxs i))))
        (define v2 (vtx-position (vector-ref vtxs (vector-ref idxs (+ i 1)))))
        (define v3 (vtx-position (vector-ref vtxs (vector-ref idxs (+ i 2)))))
        (let-values ([(v1 v2)  (if back? (values v2 v1) (values v1 v2))])
          (define time (triangle-intersect-time v1 v2 v3 o d))
          (if (and time (>= time 0.0) (<= time max-time) (or (not best-time) (< time best-time)))
              (values time v1 v2 v3)
              (values best-time best-v1 best-v2 best-v3)))))
    (cond [time
           (define data
             (delay (define p (flv3fma d time o))
                    ;; No need to flip normal because vertices are already reversed
                    (define n (flv3polygon-normal v1 v2 v3))
                    (trace-data p n empty)))
           (values time data)]
          [else
           (values #f #f)])))

;; ===================================================================================================
;; Tessellation

(: triangle-mesh-shape-extract-faces (-> shape (Values (Listof shape)
                                                       (Listof (face deform-data #f)))))
(define (triangle-mesh-shape-extract-faces s)
  (match-define (triangle-mesh-shape _ _ vtxs idxs back?) s)
  (values empty (mesh->faces (if back? (vector-map vtx-flip-normal vtxs) vtxs)
                             (if back? (vector-reverse idxs) idxs)
                             linear-deform-data
                             #f)))

(: tessellate-face (-> (face deform-data #f) FlAffine3 Positive-Flonum
                       (Listof (face deform-data #f))))
(define (tessellate-face f t max-edge)
  (match-define (face v1 v2 v3 (and data (deform-data blend interp)) _ _ _) f)
  
  (: make-face (-> vtx vtx vtx (face deform-data #f)))
  (define (make-face vtx1 vtx2 vtx3)
    (face vtx1 vtx2 vtx3 data #f #f #f))
  
  (: between (-> vtx vtx Flonum vtx))
  (define (between vtx1 vtx2 α)
    (interp vtx1 vtx2 (blend (vtx-position vtx1) (vtx-position vtx2) α)))
  
  (: dist (-> vtx vtx Flonum))
  (define (dist vtx1 vtx2)
    (flv3dist (flt3apply/pos t (vtx-position vtx1))
              (flt3apply/pos t (vtx-position vtx2))))
  
  (: segments (-> vtx vtx Positive-Integer))
  (define (segments v1 v2)
    (max 1 (exact-ceiling (/ (dist v1 v2) max-edge))))
  
  (: tess-line (-> vtx vtx vtx vtx Integer Integer (Listof (face deform-data #f))))
  (define (tess-line v1 v2 v3 v4 n12 n34)
    (let loop ([v1 v1] [v2 v2] [v3 v3] [v4 v4] [n12 n12] [n34 n34])
      (when (= n12 0) (set! n12 (segments v1 v2)))
      (when (= n34 0) (set! n34 (segments v3 v4)))
      (cond
        ;; Just a quad left: generate it
        [(and (= n12 1) (= n34 1))
         (if (< (dist v1 v3) (dist v2 v4))
             (list (make-face v1 v2 v3) (make-face v3 v4 v1))
             (list (make-face v1 v2 v4) (make-face v4 v2 v3)))]
        ;; Long edges are equal length: generate a triangle
        [(= n12 n34)
         (define v12 (between v1 v2 (/ (fl n12))))
         (define v34 (between v4 v3 (/ (fl n34))))
         (if (< (dist v1 v34) (dist v4 v12))
             (cons (make-face v34 v4 v1) (loop v1 v2 v3 v34 n12 (- n34 1)))
             (cons (make-face v1 v12 v4) (loop v12 v2 v3 v4 (- n12 1) n34)))]
        ;; Edge 12 is shorter than edge 34: generate a triangle
        [(< n12 n34)
         (define v12 (between v1 v2 (/ (fl n12))))
         (define v34 (between v4 v3 (/ (fl n34))))
         (cons (make-face v34 v4 v1) (loop v1 v2 v3 v34 n12 (- n34 1)))]
        ;; Edge 34 is shorter than edge 12: generate a triangle
        [else
         (define v12 (between v1 v2 (/ (fl n12))))
         (define v34 (between v4 v3 (/ (fl n34))))
         (cons (make-face v1 v12 v4) (loop v12 v2 v3 v4 (- n12 1) n34))])))
  
  (let loop ([v1 v1] [v2 v2] [v3 v3] [n12 : Integer  0] [n23 : Integer  0] [n31 : Integer  0])
    (when (= n12 0) (set! n12 (segments v1 v2)))
    (when (= n23 0) (set! n23 (segments v2 v3)))
    (when (= n31 0) (set! n31 (segments v3 v1)))
    ;(printf "~v ~v ~v~n" n12 n23 n31)
    (define c (+ (if (<= n12 1) 1 0) (if (<= n23 1) 1 0) (if (<= n31 1) 1 0)))
    (define o (+ (if (odd? n12) 1 0) (if (odd? n23) 1 0) (if (odd? n31) 1 0)))
    (cond
      ;; All three edges' counts are 1: we can't split anymore
      [(= c 3)
       (list (make-face v1 v2 v3))]
      ;; Two of the edges' counts are equal: walk up from the base, generating lines
      [(or (= n12 n23) (= n23 n31) (= n31 n12))
       (let-values ([(v1 v2 v3 n12 n23 n31)  (cond [(= n12 n23)  (values v1 v2 v3 n12 n23 n31)]
                                                   [(= n23 n31)  (values v2 v3 v1 n23 n31 n12)]
                                                   [else         (values v3 v1 v2 n31 n12 n23)])])
         (define v12 (between v1 v2 (/ (fl n12))))
         (define v23 (between v3 v2 (/ (fl n23))))
         (append (loop v12 v2 v23 (- n12 1) (- n23 1) 0)
                 (tess-line v3 v1 v12 v23 n31 0)))]
      ;; Two of the edges' counts are within 1: walk up from the base, generating lines
      [(or (= 1 (abs (- n12 n23))) (= 1 (abs (- n23 n31))) (= 1 (abs (- n31 n12))))
       (let-values ([(v1 v2 v3 n12 n23 n31)
                     (cond [(= 1 (abs (- n12 n23)))  (values v1 v2 v3 n12 n23 n31)]
                           [(= 1 (abs (- n23 n31)))  (values v2 v3 v1 n23 n31 n12)]
                           [else                     (values v3 v1 v2 n31 n12 n23)])])
         (define v12 (between v1 v2 (/ (fl n12))))
         (define v23 (between v3 v2 (/ (fl n23))))
         (append (loop v12 v2 v23 (- n12 1) (- n23 1) 0)
                 (tess-line v3 v1 v12 v23 n31 0)))]
      ;; Two of the edges have count 1: split the remaining edge in the middle
      [(= c 2)
       (let-values ([(v1 v2 v3 n12 n23 n31)  (cond [(> n12 1)  (values v1 v2 v3 n12 n23 n31)]
                                                   [(> n23 1)  (values v2 v3 v1 n23 n31 n12)]
                                                   [else       (values v3 v1 v2 n31 n12 n23)])])
         (define m12 (quotient n12 2))
         (define v12 (between v1 v2 (/ (fl m12) (fl n12))))
         (append (loop v1 v12 v3 m12 0 n31)
                 (loop v12 v2 v3 (- n12 m12) n23 0)))]
      ;; One of the edges has count 1: walk up from it, generating quads
      [(= c 1)
       (let-values ([(v1 v2 v3 n12 n23 n31)  (cond [(<= n12 1)  (values v1 v2 v3 n12 n23 n31)]
                                                   [(<= n23 1)  (values v2 v3 v1 n23 n31 n12)]
                                                   [else        (values v3 v1 v2 n31 n12 n23)])])
         (define v23 (between v2 v3 (/ (fl n23))))
         (define v31 (between v1 v3 (/ (fl n31))))
         (append (tess-line v1 v2 v23 v31 n12 0)
                 (loop v3 v31 v23 (- n31 1) 0 (- n23 1))))]
      ;; All three edges' counts are odd: slice off a line from the longest edge
      [(= o 3)
       (define mx (max n12 n23 n31))
       (let-values ([(v1 v2 v3 n12 n23 n31)  (cond [(= mx n12)  (values v1 v2 v3 n12 n23 n31)]
                                                   [(= mx n23)  (values v2 v3 v1 n23 n31 n12)]
                                                   [else        (values v3 v1 v2 n31 n12 n23)])])
         (define v23 (between v2 v3 (/ (fl n23))))
         (define v31 (between v1 v3 (/ (fl n31))))
         (append (tess-line v1 v2 v23 v31 n12 0)
                 (loop v31 v23 v3 0 (- n23 1) (- n31 1))))]
      ;; Two of the edges' counts are odd: split the corner between them and generate lines adjacent
      [(= o 2)
       (let-values ([(v1 v2 v3 n12 n23 n31)  (cond [(even? n12)  (values v1 v2 v3 n12 n23 n31)]
                                                   [(even? n23)  (values v2 v3 v1 n23 n31 n12)]
                                                   [else         (values v3 v1 v2 n31 n12 n23)])])
         (define v121 (between v1 v2 (/ (fl n12))))
         (define v122 (between v2 v1 (/ (fl n12))))
         (define v23 (between v3 v2 (/ (fl n23))))
         (define v31 (between v3 v1 (/ (fl n31))))
         (define v3* (between v23 v31 0.5))
         (append (tess-line v3 v1 v121 v3* n31 0)
                 (tess-line v2 v3 v3* v122 n23 0)
                 (loop v121 v122 v3* (- n12 2) 0 0)))]
      ;; One edge has an odd count: generate the adjacent line (often yields the o = 2 case)
      [(= o 1)
       (let-values ([(v1 v2 v3 n12 n23 n31)  (cond [(odd? n12)  (values v1 v2 v3 n12 n23 n31)]
                                                   [(odd? n23)  (values v2 v3 v1 n23 n31 n12)]
                                                   [else        (values v3 v1 v2 n31 n12 n23)])])
         (define v23 (between v2 v3 (/ (fl n23))))
         (define v31 (between v1 v3 (/ (fl n31))))
         (append (tess-line v1 v2 v23 v31 n12 0)
                 (loop v3 v31 v23 (- n31 1) 0 (- n23 1))))]
      ;; All edges have an even count > 1: quadrisect the triangle by splitting each in the middle
      [else
       (define v12 (between v1 v2 0.5))
       (define v23 (between v2 v3 0.5))
       (define v31 (between v3 v1 0.5))
       (append (loop v1 v12 v31 (quotient n12 2) 0 (quotient n31 2))
               (loop v2 v23 v12 (quotient n23 2) 0 (quotient n12 2))
               (loop v3 v31 v23 (quotient n31 2) 0 (quotient n23 2))
               (loop v12 v23 v31 0 0 0))])))

(: triangle-mesh-shape-tessellate (-> shape FlAffine3 Positive-Flonum Nonnegative-Flonum
                                      (Values (Listof shape) (Listof (face deform-data #f)))))
(define (triangle-mesh-shape-tessellate s t max-edge _)
  (match-define (triangle-mesh-shape _ _ vtxs idxs back?) s)
  (values empty
          (append*
           (map (λ ([f : (face deform-data #f)]) (tessellate-face f t max-edge))
                (mesh->faces (if back? (vector-map vtx-flip-normal vtxs) vtxs)
                             (if back? (vector-reverse idxs) idxs)
                             linear-deform-data
                             #f)))))

;; ===================================================================================================
;; Warp

(: faces->triangle-mesh-shapes (All (A B) (-> (Listof (face A B)) (Listof triangle-mesh-shape))))
(define (faces->triangle-mesh-shapes fs)
  (for/fold ([ss : (Listof triangle-mesh-shape)  empty])
            ([fs  (in-list (face-soup-chunk (make-face-soup fs) max-chunk-size))])
    (define-values (vtxs idxs) (faces->mesh fs))
    (cons (make-triangle-mesh-shape vtxs idxs #f) ss)))

(: triangle-mesh-shape-deform (-> shape FlSmooth3 (Listof triangle-mesh-shape)))
(define (triangle-mesh-shape-deform s t)
  (match-define (triangle-mesh-shape _ _ vtxs idxs back?) s)
  (define n (vector-length vtxs))
  (define-values (vtx0 d0 c0?) (fls3apply/vtx t (vector-ref vtxs 0)))
  (define new-back? (if c0? back? (not back?)))
  (define new-vtxs (make-vector n (if c0? vtx0 (vtx-flip-normal vtx0))))
  (let loop ([i : Positive-Fixnum  1])
    (cond
      [(< i n)
       (define-values (vtxi di ci?) (fls3apply/vtx t (unsafe-vector-ref vtxs i)))
       (cond [(eq? ci? c0?)
              (unsafe-vector-set! new-vtxs i (if ci? vtxi (vtx-flip-normal vtxi)))
              (loop (+ i 1))]
             [else
              (define new-idxs (if back? (vector-reverse idxs) idxs))
              (define fs (mesh->faces vtxs new-idxs linear-deform-data #f))
              (define deform ((inst make-fls3apply/face deform-data Boolean) t))
              (faces->triangle-mesh-shapes (map deform fs))])]
      [else
       (list (triangle-mesh-shape (lazy-passes) triangle-mesh-shape-functions
                                  new-vtxs
                                  idxs
                                  new-back?))])))

;; ===================================================================================================

(define triangle-mesh-shape-functions
  (deform-shape-functions
    get-triangle-mesh-shape-passes
    (λ (s kind t) (and (eq? kind 'visible) (get-triangle-mesh-shape-bbox s t)))
    triangle-mesh-shape-transform
    (λ (s t) (list (triangle-mesh-shape-transform s t)))
    triangle-mesh-shape-ray-intersect
    set-triangle-mesh-shape-color
    set-triangle-mesh-shape-emitted
    set-triangle-mesh-shape-material
    triangle-mesh-shape-extract-faces
    triangle-mesh-shape-tessellate
    triangle-mesh-shape-deform))
