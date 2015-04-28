#lang typed/racket/base

;; Triangles, quads, rectangles

(require racket/list
         racket/vector
         racket/match
         racket/promise
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         math/flonum
         "../math.rkt"
         "../memo.rkt"
         "../engine.rkt"
         "../utils.rkt")

(provide make-triangle-shape
         make-triangle-mesh-shape
         make-quad-shape
         make-rectangle-shape
         (struct-out vtx)
         (struct-out triangle-mesh-shape)
         (struct-out rectangle-shape))

;; ===================================================================================================
;; Vertex data

(struct vtx ([position : FlV3]
             [normal : FlV3]
             [color : FlV4]
             [emitted : FlV4]
             [material : FlV4])
  #:transparent)

(: set-vtx-vecs (-> vtx FlV3 FlV3 vtx))
(define (set-vtx-vecs v pos norm)
  (vtx pos norm (vtx-color v) (vtx-emitted v) (vtx-material v)))

(: set-vtx-color (-> vtx FlV4 vtx))
(define (set-vtx-color v c)
  (vtx (vtx-position v) (vtx-normal v) c (vtx-emitted v) (vtx-material v)))

(: set-vtx-emitted (-> vtx FlV4 vtx))
(define (set-vtx-emitted v e)
  (vtx (vtx-position v) (vtx-normal v) (vtx-color v) e (vtx-material v)))

(: set-vtx-material (-> vtx FlV4 vtx))
(define (set-vtx-material v m)
  (vtx (vtx-position v) (vtx-normal v) (vtx-color v) (vtx-emitted v) m))

;; ===================================================================================================
;; Shape data types

(struct triangle-mesh-shape shape
  ([vtxs : (Vectorof vtx)]
   [idxs : (Vectorof Index)]
   [back? : Boolean])
  #:transparent)

(struct rectangle-shape shape
  ([axial-rect : FlRect3]
   [color : FlV4]
   [emitted : FlV4]
   [material : FlV4]
   [inside? : Boolean])
  #:transparent)

;; ===================================================================================================
;; Constructors

(: make-triangle-mesh-shape (-> (Vectorof vtx) (Vectorof Index) Boolean triangle-mesh-shape))
(define (make-triangle-mesh-shape vtxs idxs back?)
  (define n (vector-length vtxs))
  (define m (vector-length idxs))
  (unless (and (> m 0) (zero? (modulo m 3)))
    (raise-argument-error 'make-triangle-mesh-shape
                          "(Vectorof Index) with positive, multiple-of-3 length"
                          1 vtxs idxs back?))
  (for ([i  (in-range m)])
    (unless (< (vector-ref idxs i) n)
      (raise-argument-error 'make-triangle-mesh-shape
                            (format "(Vectorof Index) with all indexes < ~a" n)
                            1 vtxs idxs back?)))
  (triangle-mesh-shape (lazy-passes) triangle-mesh-shape-functions vtxs idxs back?))

(: triangle-indexes (Vectorof Index))
(define triangle-indexes (vector 0 1 2))

(: make-triangle-shape (-> vtx vtx vtx Boolean triangle-mesh-shape))
(define (make-triangle-shape v1 v2 v3 back?)
  (triangle-mesh-shape (lazy-passes) triangle-mesh-shape-functions
                       (vector v1 v2 v3) triangle-indexes back?))

(: make-quad-shape (-> vtx vtx vtx vtx Boolean triangle-mesh-shape))
(define (make-quad-shape v1 v2 v3 v4 back?)
  (define p1 (vtx-position v1))
  (define p2 (vtx-position v2))
  (define p3 (vtx-position v3))
  (define p4 (vtx-position v4))
  ;; Maximize the minimum regularity
  (define r1 (min (flv3polygon-regularity (vector p1 p2 p3))
                  (flv3polygon-regularity (vector p3 p4 p1))))
  (define r2 (min (flv3polygon-regularity (vector p2 p3 p4))
                  (flv3polygon-regularity (vector p4 p1 p2))))
  (define vtxs (vector v1 v2 v3 v4))
  (define idxs (if (>= r1 r2)
                   ((inst vector Index) 0 1 2 2 3 0)
                   ((inst vector Index) 1 2 3 3 0 1)))
  (triangle-mesh-shape (lazy-passes) triangle-mesh-shape-functions vtxs idxs back?))

(: make-rectangle-shape (-> FlRect3 FlV4 FlV4 FlV4 Boolean rectangle-shape))
(define (make-rectangle-shape b c e m back?)
  (rectangle-shape (lazy-passes) rectangle-shape-functions b c e m back?))

;; ===================================================================================================
;; Set attributes

(: set-triangle-mesh-shape-color (-> shape FlV4 triangle-mesh-shape))
(define (set-triangle-mesh-shape-color s c)
  (match-define (triangle-mesh-shape _ _ vtxs idxs back?) s)
  (triangle-mesh-shape (lazy-passes) triangle-mesh-shape-functions
                       (vector-map (λ ([v : vtx]) (set-vtx-color v c)) vtxs)
                       idxs back?))

(: set-triangle-mesh-shape-emitted (-> shape FlV4 triangle-mesh-shape))
(define (set-triangle-mesh-shape-emitted s e)
  (match-define (triangle-mesh-shape _ _ vtxs idxs back?) s)
  (triangle-mesh-shape (lazy-passes) triangle-mesh-shape-functions
                       (vector-map (λ ([v : vtx]) (set-vtx-emitted v e)) vtxs)
                       idxs back?))

(: set-triangle-mesh-shape-material (-> shape FlV4 triangle-mesh-shape))
(define (set-triangle-mesh-shape-material s m)
  (match-define (triangle-mesh-shape _ _ vtxs idxs back?) s)
  (triangle-mesh-shape (lazy-passes) triangle-mesh-shape-functions
                       (vector-map (λ ([v : vtx]) (set-vtx-material v m)) vtxs)
                       idxs back?))

(: set-rectangle-shape-color (-> shape FlV4 rectangle-shape))
(define (set-rectangle-shape-color s c)
  (match-define (rectangle-shape _ _ b _ e m inside?) s)
  (make-rectangle-shape b c e m inside?))

(: set-rectangle-shape-emitted (-> shape FlV4 rectangle-shape))
(define (set-rectangle-shape-emitted s e)
  (match-define (rectangle-shape _ _ b c _ m inside?) s)
  (make-rectangle-shape b c e m inside?))

(: set-rectangle-shape-material (-> shape FlV4 rectangle-shape))
(define (set-rectangle-shape-material s m)
  (match-define (rectangle-shape _ _ b c e _ inside?) s)
  (make-rectangle-shape b c e m inside?))

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
;; Triangle mesh shape passes

(define (get-triangle-mesh-shape-passes s)
  (match-define (triangle-mesh-shape _ _ vtxs idxs back?) s)
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
  
  (define opaq-struct-size (program-code-vao-size polygon-opaq-program-code))
  (define draw-data (make-bytes (* n opaq-struct-size)))
  (for/fold ([i : Nonnegative-Fixnum  0]) ([j  (in-range n)])
    (define vert (vector-ref vtxs j))
    (define v (vtx-position vert))
    (define c (vtx-color vert))
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
  
  (define js (if back? (vector-reverse idxs) idxs))
  
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
;; Rectangle shape passes

(: get-rectangle-shape-passes (-> shape passes))
(define (get-rectangle-shape-passes s)
  (let ([s  (assert s rectangle-shape?)])
    (define ss (rectangle-shape->triangle-mesh-shapes s))
    (define ps (map get-triangle-mesh-shape-passes ss))
    (passes
     #()
     (apply vector-append (map passes-opaque-material ps))
     (apply vector-append (map passes-opaque-color ps))
     (apply vector-append (map passes-transparent-material ps))
     (apply vector-append (map passes-transparent-color ps)))))

;; ===================================================================================================
;; Bounding box

(: get-triangle-mesh-shape-bbox (-> shape FlAffine3 bbox))
(define (get-triangle-mesh-shape-bbox s t)
  (match-define (triangle-mesh-shape _ _ vtxs idxs back?) s)
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

(: get-rectangle-shape-bbox (-> shape FlAffine3 bbox))
(define (get-rectangle-shape-bbox s t)
  (let ([s  (assert s rectangle-shape?)])
    (bbox (flrect3-transform (rectangle-shape-axial-rect s) t)
          0.0)))

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

(: rectangle-shape-deep-transform (-> shape FlAffine3 (Listof triangle-mesh-shape)))
(define (rectangle-shape-deep-transform s t)
  (let ([s  (assert s rectangle-shape?)])
    (map (λ ([s : triangle-mesh-shape])
           (triangle-mesh-shape-transform s t))
         (rectangle-shape->triangle-mesh-shapes s))))

;; ===================================================================================================
;; Conversions

(: rectangle-shape->triangle-mesh-shapes (-> rectangle-shape (Listof triangle-mesh-shape)))
(define (rectangle-shape->triangle-mesh-shapes a)
  (match-define (rectangle-shape _ _ b c e m inside?) a)
  (define-values (v1 v5 v4 v8 v2 v6 v3 v7) (flrect3-corners b))
  
  (: do-make-quad-shape (-> FlV3 FlV3 FlV3 FlV3 FlV3 triangle-mesh-shape))
  (define (do-make-quad-shape v1 v2 v3 v4 n)
    (make-quad-shape (vtx v1 n c e m)
                     (vtx v2 n c e m)
                     (vtx v3 n c e m)
                     (vtx v4 n c e m)
                     inside?))
  
  (list (do-make-quad-shape v4 v3 v2 v1 -z-flv3)
        (do-make-quad-shape v5 v6 v7 v8 +z-flv3)
        (do-make-quad-shape v1 v2 v6 v5 -y-flv3)
        (do-make-quad-shape v3 v4 v8 v7 +y-flv3)
        (do-make-quad-shape v4 v1 v5 v8 -x-flv3)
        (do-make-quad-shape v2 v3 v7 v6 +x-flv3)))

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
                    (define n (flv3triangle-normal v1 v2 v3))
                    (trace-data p n empty)))
           (values time data)]
          [else
           (values #f #f)])))

(: rectangle-shape-ray-intersect (-> shape FlV3 FlV3 Nonnegative-Flonum
                                     (Values (U #f Nonnegative-Flonum) (U #f (Promise trace-data)))))
(define (rectangle-shape-ray-intersect s v dv max-time)
  (with-asserts ([s  rectangle-shape?])
    (define b (rectangle-shape-axial-rect s))
    (define inside? (rectangle-shape-inside? s))
    (define-values (tmin tmax)
      ;; Determine the initial interval
      (let-values ([(min-time max-time)  (cond [inside?  (values 0.0 +inf.0)]
                                               [else     (values -inf.0 max-time)])])
        (flrect3-line-intersects b v dv min-time max-time)))
    (define time (if inside? tmax tmin))
    (cond [(and time (>= time 0.0) (<= time max-time))
           (define data
             (delay (define p (flrect3-closest-point b (flv3fma dv time v)))
                    (define n (let* ([ns  (flrect3-point-normals b p)]
                                     [n   (cond
                                            [(empty? ns)  #f]
                                            [inside?  (argmax (λ ([n : FlV3]) (flv3dot n dv)) ns)]
                                            [else     (argmin (λ ([n : FlV3]) (flv3dot n dv)) ns)])])
                                (and n (if inside? (flv3neg n) n))))
                    (trace-data p n empty)))
           (values time data)]
          [else
           (values #f #f)])))

;; ===================================================================================================

(define triangle-mesh-shape-functions
  (shape-functions
   set-triangle-mesh-shape-color
   set-triangle-mesh-shape-emitted
   set-triangle-mesh-shape-material
   get-triangle-mesh-shape-passes
   (λ (s kind t) (and (eq? kind 'visible) (get-triangle-mesh-shape-bbox s t)))
   triangle-mesh-shape-transform
   (λ (s t) (list (triangle-mesh-shape-transform s t)))
   triangle-mesh-shape-ray-intersect))

(define rectangle-shape-functions
  (shape-functions
   set-rectangle-shape-color
   set-rectangle-shape-emitted
   set-rectangle-shape-material
   get-rectangle-shape-passes
   (λ (s kind t) (and (eq? kind 'visible) (get-rectangle-shape-bbox s t)))
   (λ (s t) #f)
   rectangle-shape-deep-transform
   rectangle-shape-ray-intersect))
