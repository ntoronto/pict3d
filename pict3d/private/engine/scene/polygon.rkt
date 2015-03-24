#lang typed/racket/base

;; Triangles, quads, rectangles

(require racket/unsafe/ops
         racket/list
         racket/vector
         racket/match
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         math/flonum
         "../../math.rkt"
         "../../gl.rkt"
         "../../utils.rkt"
         "../utils.rkt"
         "../shader-code.rkt"
         "../serialize-vertices.rkt"
         "../types.rkt"
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
         rectangle-shape-rect
         rectangle-shape-transform
         rectangle-shape-line-intersect
         
         rectangle-shape->triangle-shapes
         )

;; ===================================================================================================
;; Constructors

(: make-triangle-shape (-> vtx vtx vtx Boolean triangle-shape))
(define (make-triangle-shape v1 v2 v3 back?)
  (define fs (flags-join visible-flag
                         (if (and (color-opaque? (vtx-color v1))
                                  (color-opaque? (vtx-color v2))
                                  (color-opaque? (vtx-color v3)))
                             opaque-flag
                             transparent-flag)
                         (if (or (color-emitting? (vtx-emitted v1))
                                 (color-emitting? (vtx-emitted v2))
                                 (color-emitting? (vtx-emitted v3)))
                             emitting-flag
                             nonemitting-flag)))
  (triangle-shape (lazy-passes) fs v1 v2 v3 back?))

(: make-quad-shapes (-> vtx vtx vtx vtx  Boolean (List triangle-shape triangle-shape)))
(define (make-quad-shapes v1 v2 v3 v4 back?)
  (define p1 (vtx-position v1))
  (define p2 (vtx-position v2))
  (define p3 (vtx-position v3))
  (define p4 (vtx-position v4))
  ;; Maximize the minimum regularity
  (define r1 (min (flv3polygon-regularity (vector p1 p2 p3))
                  (flv3polygon-regularity (vector p3 p4 p1))))
  (define r2 (min (flv3polygon-regularity (vector p2 p3 p4))
                  (flv3polygon-regularity (vector p4 p1 p2))))
  (cond [(>= r1 r2)
         (list (make-triangle-shape v1 v2 v3 back?)
               (make-triangle-shape v3 v4 v1 back?))]
        [else
         (list (make-triangle-shape v2 v3 v4 back?)
               (make-triangle-shape v4 v1 v2 back?))]))

(: make-rectangle-shape (-> FlRect3 FlV4 FlV4 FlV4 Boolean rectangle-shape))
(define (make-rectangle-shape b c e m back?)
  (define fs (flags-join visible-flag (color-opacity-flag c) (color-emitting-flag e)))
  (rectangle-shape (lazy-passes) fs b c e m back?))

;; ===================================================================================================
;; Set attributes

(: set-triangle-shape-color (-> triangle-shape FlV4 triangle-shape))
(define (set-triangle-shape-color a c)
  (match-define (triangle-shape _ fs v1 v2 v3 back?) a)
  (define new-fs (flags-join (flags-subtract fs opacity-flags)
                             (color-opacity-flag c)))
  (triangle-shape (lazy-passes)
                  new-fs
                  (vtx-set-color v1 c)
                  (vtx-set-color v2 c)
                  (vtx-set-color v3 c)
                  back?))

(: set-triangle-shape-emitted (-> triangle-shape FlV4 triangle-shape))
(define (set-triangle-shape-emitted a e)
  (match-define (triangle-shape _ fs v1 v2 v3 back?) a)
  (define new-fs (flags-join (flags-subtract fs emitting-flags)
                             (color-emitting-flag e)))
  (triangle-shape (lazy-passes)
                  new-fs
                  (vtx-set-emitted v1 e)
                  (vtx-set-emitted v2 e)
                  (vtx-set-emitted v3 e)
                  back?))

(: set-triangle-shape-material (-> triangle-shape FlV4 triangle-shape))
(define (set-triangle-shape-material a m)
  (match-define (triangle-shape _ fs v1 v2 v3 back?) a)
  (triangle-shape (lazy-passes) fs
                  (vtx-set-material v1 m)
                  (vtx-set-material v2 m)
                  (vtx-set-material v3 m)
                  back?))

(: set-rectangle-shape-color (-> rectangle-shape FlV4 rectangle-shape))
(define (set-rectangle-shape-color a c)
  (match-define (rectangle-shape _ fs b old-c e m inside?) a)
  (define new-fs (flags-join (flags-subtract fs opacity-flags)
                             (color-opacity-flag c)))
  (rectangle-shape (lazy-passes) new-fs b c e m inside?))

(: set-rectangle-shape-emitted (-> rectangle-shape FlV4 rectangle-shape))
(define (set-rectangle-shape-emitted a e)
  (match-define (rectangle-shape _ fs b c old-e m inside?) a)
  (define new-fs (flags-join (flags-subtract fs emitting-flags)
                             (color-emitting-flag e)))
  (rectangle-shape (lazy-passes) new-fs b c e m inside?))

(: set-rectangle-shape-material (-> rectangle-shape FlV4 rectangle-shape))
(define (set-rectangle-shape-material a m)
  (match-define (rectangle-shape _ fs b c e old-m inside?) a)
  (rectangle-shape (lazy-passes) fs b c e m inside?))

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
  (match-define (triangle-shape _ fs v1 v2 v3 back?) a)
  (define-values (start stop step)
    (if back?
        (values 2 -1 -1)
        (values 0 3 1)))
  
  (define verts (vector v1 v2 v3))
  
  (define mat-struct-size (program-code-vao-size polygon-mat-program-code))
  (define mat-data (make-bytes (* 3 mat-struct-size)))
  (for/fold ([i : Nonnegative-Fixnum  0]) ([j  (in-range start stop step)])
    (define vert (vector-ref verts j))
    (define v (vtx-position vert))
    (define n (vtx-normal vert))
    (define m (vtx-material vert))
    (let* ([i  (serialize-normal/bytes mat-data i n back?)]
           [i  (serialize-float/byte mat-data i (unsafe-flv4-ref m 3))]
           [i  (serialize-vec3 mat-data i v)])
      i))
  
  (define opaq-struct-size (program-code-vao-size polygon-opaq-program-code))
  (define draw-data (make-bytes (* 3 opaq-struct-size)))
  (for/fold ([i : Nonnegative-Fixnum  0]) ([j  (in-range start stop step)])
    (define vert (vector-ref verts j))
    (define v (vtx-position vert))
    (define c (vtx-color vert))
    (define e (vtx-emitted vert))
    (define m (vtx-material vert))
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

(: triangle-shape-rect (-> triangle-shape FlAffine3 FlRect3))
(define (triangle-shape-rect a t)
  (flrect3 (flt3apply/pos t (vtx-position (triangle-shape-vtx1 a)))
           (flt3apply/pos t (vtx-position (triangle-shape-vtx2 a)))
           (flt3apply/pos t (vtx-position (triangle-shape-vtx3 a)))))

(: rectangle-shape-rect (-> rectangle-shape FlAffine3 FlRect3))
(define (rectangle-shape-rect a t)
  (flrect3-transform (rectangle-shape-axial-rect a) t))

;; ===================================================================================================
;; Transform

(: triangle-shape-easy-transform (-> triangle-shape FlAffine3 triangle-shape))
(define (triangle-shape-easy-transform a t)
  (match-define (triangle-shape passes fs v1 v2 v3 back?) a)
  (define consistent? (flt3consistent? t))
  
  (: transform-vtx (-> vtx vtx))
  (define (transform-vtx vert)
    (define v (vtx-position vert))
    (define n (vtx-normal vert))
    (vtx-set-vecs
     vert
     (flt3apply/pos t v)
     (let ([n  (flt3apply/norm t n)])
       (if consistent?
           (if n n +z-flv3)
           (if n (flv3neg n) +z-flv3)))))
  
  (triangle-shape (lazy-passes)
                  fs
                  (transform-vtx v1)
                  (transform-vtx v2)
                  (transform-vtx v3)
                  (if consistent? back? (not back?))))

(: rectangle-shape-transform (-> rectangle-shape FlAffine3 (Listof triangle-shape)))
(define (rectangle-shape-transform a t)
  (map (λ ([a : triangle-shape])
         (triangle-shape-easy-transform a t))
       (rectangle-shape->triangle-shapes a)))

;; ===================================================================================================
;; Conversions

(: rectangle-shape->triangle-shapes (-> rectangle-shape (Listof triangle-shape)))
(define (rectangle-shape->triangle-shapes a)
  (match-define (rectangle-shape _ fs b c e m inside?) a)
  (define-values (v1 v5 v4 v8 v2 v6 v3 v7) (flrect3-corners b))
  
  (: do-make-quad-shapes (-> FlV3 FlV3 FlV3 FlV3 FlV3 (List triangle-shape triangle-shape)))
  (define (do-make-quad-shapes v1 v2 v3 v4 n)
    (make-quad-shapes (vtx v1 n c e m)
                      (vtx v2 n c e m)
                      (vtx v3 n c e m)
                      (vtx v4 n c e m)
                      inside?))
  (append*
   (list (do-make-quad-shapes v4 v3 v2 v1 -z-flv3)
         (do-make-quad-shapes v5 v6 v7 v8 +z-flv3)
         (do-make-quad-shapes v1 v2 v6 v5 -y-flv3)
         (do-make-quad-shapes v3 v4 v8 v7 +y-flv3)
         (do-make-quad-shapes v4 v1 v5 v8 -x-flv3)
         (do-make-quad-shapes v2 v3 v7 v6 +x-flv3))))

;; ===================================================================================================
;; Ray intersection

;; The maximum and minimum barycentric coordinates should be 0.0 and 1.0, but because of floating-
;; point error and whatnot, we might miss if we use those bounds, so we'll fudge a bit
;; We end up testing against slightly larger triangles with somewhat cut off corners
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

(: triangle-shape-line-intersect (-> triangle-shape FlV3 FlV3 (U #f line-hit)))
;; Moller-Trumbore
(define (triangle-shape-line-intersect a o d)
  (define v0 (vtx-position (triangle-shape-vtx1 a)))
  (define v1 (vtx-position (triangle-shape-vtx2 a)))
  (define v2 (vtx-position (triangle-shape-vtx3 a)))
  (let-values ([(v0 v1)  (if (triangle-shape-back? a) (values v1 v0) (values v0 v1))])
    (define t (triangle-intersect-time v0 v1 v2 o d))
    (cond
      [(not t)  #f]
      [else
       (define back? (triangle-shape-back? a))
       (line-hit t
                 (flv3fma d t o)
                 (let ([norm  (flv3triangle-normal v0 v1 v2)])
                   (and norm (if back? (flv3neg norm) norm))))])))

(: rectangle-shape-line-intersect (-> rectangle-shape FlV3 FlV3 (U #f line-hit)))
(define (rectangle-shape-line-intersect a v dv)
  (define b (rectangle-shape-axial-rect a))
  (define-values (tmin tmax) (flrect3-line-intersects b v dv))
  (define inside? (rectangle-shape-inside? a))
  (define t (if inside? tmax tmin))
  (cond [(not t)  #f]
        [else
         (define p (flrect3-closest-point b (flv3fma dv t v)))
         (line-hit t p (let ([norm  (assert (flrect3-point-normal b p) values)])
                         (if inside? (flv3neg norm) norm)))]))
