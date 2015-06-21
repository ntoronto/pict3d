#lang typed/racket/base

(require racket/match
         racket/list
         typed/opengl
         math/flonum
         math/base
         "../math.rkt"
         "../engine.rkt"
         "../soup.rkt"
         "types.rkt"
         "sphere/sphere-type.rkt"
         (prefix-in 30: "sphere/ge_30.rkt")
         (prefix-in 32: "sphere/ge_32.rkt"))

(provide make-sphere-shape
         (struct-out sphere-shape))

;; ===================================================================================================
;; Constructors

(: make-sphere-shape (-> FlAffine3 FlV4 FlV4 FlV4 Boolean sphere-shape))
(define (make-sphere-shape t c e m inside?)
  (sphere-shape (lazy-passes) sphere-shape-functions
                t c e m inside?))

;; ===================================================================================================
;; Set attributes

(: set-sphere-shape-color (-> shape FlV4 sphere-shape))
(define (set-sphere-shape-color a c)
  (match-define (sphere-shape _ _ t _ e m inside?) a)
  (make-sphere-shape t c e m inside?))

(: set-sphere-shape-emitted (-> shape FlV4 sphere-shape))
(define (set-sphere-shape-emitted a e)
  (match-define (sphere-shape _ _ t c _ m inside?) a)
  (make-sphere-shape t c e m inside?))

(: set-sphere-shape-material (-> shape FlV4 sphere-shape))
(define (set-sphere-shape-material a m)
  (match-define (sphere-shape _ _ t c e _ inside?) a)
  (make-sphere-shape t c e m inside?))

;; ===================================================================================================
;; Drawing passes

(: get-sphere-shape-passes (-> shape passes))
(define (get-sphere-shape-passes s)
  (if (gl-version-at-least? 32)
      (32:get-sphere-shape-passes s)
      (30:get-sphere-shape-passes s)))

;; ===================================================================================================
;; Bounding box

(: get-sphere-shape-bbox (-> shape FlAffine3 bbox))
(define (get-sphere-shape-bbox s t)
  (let ([s  (assert s sphere-shape?)])
    (bbox (transformed-sphere-flrect3 (flt3compose t (sphere-shape-affine s)))
          0.0)))

;; ===================================================================================================
;; Transform

(: sphere-shape-transform (-> shape FlAffine3 sphere-shape))
(define (sphere-shape-transform s t)
  (match-define (sphere-shape _ _ t0 c e m inside?) s)
  (make-sphere-shape (flt3compose t t0) c e m inside?))

;; ===================================================================================================
;; Ray intersection

;; Minimum discriminant would normally be 0.0, but floating-point error could make rays wrongly miss
;; This makes the sphere a little fatter in the plane perpendicular to the ray to try to make up for
;; it, and also makes edge-grazing intersections more likely - don't know whether that's a good thing
(define discr-min (* -128.0 epsilon.0))

(: unit-sphere-line-intersects (-> FlV3 FlV3 (Values (U #f Flonum) (U #f Flonum))))
(define (unit-sphere-line-intersects p d)
  (define m^2 (flv3mag^2 d))
  (define b (/ (- (flv3dot p d)) m^2))
  (define c (/ (- (flv3mag^2 p) 1.0) m^2))
  (let ([discr  (- (* b b) c)])
    (if (< discr discr-min)
        (values #f #f)  ; Missed sphere
        (let* ([q  (flsqrt (max 0.0 discr))])
          (values (- b q) (+ b q))))))

(: unit-sphere-intersect-normal (-> FlV3 FlV3 Nonnegative-Flonum (U #f FlV3)))
(define (unit-sphere-intersect-normal p d time)
  (flv3normalize (flv3fma d time p)))

(: sphere-shape-ray-intersect (-> shape FlV3 FlV3 Nonnegative-Flonum
                                  (Values (U #f Nonnegative-Flonum) (U #f (Promise trace-data)))))
(define (sphere-shape-ray-intersect s v dv max-time)
  (let ([s  (assert s sphere-shape?)])
    (transformed-shape-intersect (sphere-shape-affine s)
                                 (sphere-shape-inside? s)
                                 v dv max-time
                                 unit-sphere-line-intersects
                                 unit-sphere-intersect-normal)))

;; ===================================================================================================
;; Tessellation

(: make-sphere-deform-data (-> FlAffine3 deform-data))
(define (make-sphere-deform-data t)
  (define tinv (flt3inverse t))
  (if tinv
      (deform-data
        (λ (v1 v2 α)
          (let* ([v  (flv3blend (flt3apply/pos tinv v1) (flt3apply/pos tinv v2) α)]
                 [v  (flv3normalize v)]
                 [v  (if v v zero-flv3)])
            (flt3apply/pos t v)))
        (λ (vtx1 vtx2 v)
          (let* ([v      (flt3apply/pos tinv v)]
                 [vtx1   (flt3apply/vtx tinv vtx1)]
                 [vtx2   (flt3apply/vtx tinv vtx2)]
                 [vtx12  (vtx-interpolate vtx1 vtx2 v)]
                 [vtx12  (flt3apply/vtx t vtx12)])
            vtx12)))
      linear-deform-data))

(define octahedron-vss
  (list (list +z-flv3 +x-flv3 +y-flv3)
        (list +z-flv3 +y-flv3 -x-flv3)
        (list +z-flv3 -x-flv3 -y-flv3)
        (list +z-flv3 -y-flv3 +x-flv3)
        (list -z-flv3 +y-flv3 +x-flv3)
        (list -z-flv3 -x-flv3 +y-flv3)
        (list -z-flv3 -y-flv3 -x-flv3)
        (list -z-flv3 +x-flv3 -y-flv3)))

(define reverse-octahedron-vss
  (map (λ ([vs : (Listof FlV3)])
         ;; If tessellation is asymmetric, just reversing makes it look bad
         (match-define (list v1 v2 v3) vs)
         (list v1 v3 v2))
       octahedron-vss))

(: flv3slerp (-> FlV3 FlV3 Flonum FlV3))
(define (flv3slerp v1 v2 α)
  (define angle (acos (flv3cos v1 v2)))
  (define s (sin angle))
  (define β1 (/ (sin (* (- 1.0 α) angle)) s))
  (define β2 (/ (sin (* α angle)) s))
  (if (and (< -inf.0 (min β1 β2))
           (< (max β1 β2) +inf.0))
      (flv3+ (flv3* v1 β1) (flv3* v2 β2))
      v1))

(: biased-triangle-slerp (-> FlV3 FlV3 FlV3 Flonum Flonum FlV3))
(define (biased-triangle-slerp va vb vc u v)
  (flv3slerp (flv3slerp va vb u)
             (flv3slerp va vc u)
             (if (= u 0.0) 0.0 (/ v u))))

(: rotate-slerp-args (-> FlV3 FlV3 FlV3 Flonum Flonum (Values FlV3 FlV3 FlV3 Flonum Flonum)))
(define (rotate-slerp-args va1 vb1 vc1 u1 v1)
  (define v2 (- 1.0 u1))
  (define u2 (+ v1 v2))
  (values vb1 vc1 va1 u2 v2))

(: triangle-slerp (-> FlV3 FlV3 FlV3 Flonum Flonum FlV3))
(define (triangle-slerp va1 vb1 vc1 u1 v1)
  (define-values (va2 vb2 vc2 u2 v2) (rotate-slerp-args va1 vb1 vc1 u1 v1))
  (define-values (va3 vb3 vc3 u3 v3) (rotate-slerp-args va2 vb2 vc2 u2 v2))
  (flv3mean (biased-triangle-slerp va1 vb1 vc1 u1 v1)
            (biased-triangle-slerp va2 vb2 vc2 u2 v2)
            (biased-triangle-slerp va3 vb3 vc3 u3 v3)))

(: split-sphere-face (-> (Listof FlV3) Positive-Integer (Listof (Listof FlV3))))
(define (split-sphere-face vs n)
  (match-define (list va vb vc) vs)
  
  ;; All the vertex positions for this face
  (define verts
    (list->vector
     (for/list : (Listof (Vectorof FlV3)) ([i  (in-range (+ n 1))])
       (list->vector
        (for/list : (Listof FlV3) ([j  (in-range (+ i 1))])
          (triangle-slerp va vb vc (/ (fl i) (fl n)) (/ (fl j) (fl n))))))))
  
  (append*
   (for/list : (Listof (Listof (Listof FlV3))) ([i1  (in-range n)])
     (define i2 (+ i1 1))
     (append*
      (for/list : (Listof (Listof (Listof FlV3))) ([j1  (in-range (+ i1 1))])
        (define j2 (+ j1 1))
        (define va (vector-ref (vector-ref verts i1) j1))
        (define vb (vector-ref (vector-ref verts i2) j1))
        (define vc (vector-ref (vector-ref verts i2) j2))
        (cond [(= j1 i1)  (list (list va vb vc))]
              [else
               (define vd (vector-ref (vector-ref verts i1) j2))
               (list (list va vb vc) (list va vc vd))]))))))

(: sphere-shape-tessellate (-> shape FlAffine3 Positive-Flonum Nonnegative-Flonum
                               (Values Null (Listof (face deform-data #f)))))
(define (sphere-shape-tessellate s t0 max-edge max-angle)
  (match-define (sphere-shape _ _ t c e m inside?) s)
  
  (define reverse? (if (flt3consistent? t) inside? (not inside?)))
  (define n (max 1 (exact-ceiling (/ (* 0.5 pi) (max min-angle max-angle)))))
  
  (define vss
    (let* ([vss  (if reverse? reverse-octahedron-vss octahedron-vss)]
           [vss  (append* (map (λ ([vs : (Listof FlV3)]) (split-sphere-face vs n)) vss))])
      vss))
  
  (define data (make-sphere-deform-data t))
  
  (: flv3->vtx (-> FlV3 vtx))
  (define (flv3->vtx v)
    (define n (let ([n  (assert (flt3apply/norm t v) values)])
                (if inside? (flv3neg n) n)))
    (vtx (flt3apply/pos t v) n c e m))
  
  (define fs
    (for/fold ([fs : (Listof (face deform-data #f))  empty]) ([vs  (in-list vss)])
      (match-define (list v1 v2 v3) vs)
      (cons (face (flv3->vtx v1) (flv3->vtx v2) (flv3->vtx v3) data #f #f #f) fs)))
  
  (values empty fs))

;; ===================================================================================================
;; Deform

(: sphere-shape-deform (-> shape FlSmooth3 (U Null (List sphere-shape))))
(define (sphere-shape-deform s t0)
  (match-define (sphere-shape _ _ t c e m inside?) s)
  (let ([t  (fls3apply/affine t0 t)])
    (if t (list (make-sphere-shape t c e m inside?)) empty)))

;; ===================================================================================================

(define sphere-shape-functions
  (deform-shape-functions
    get-sphere-shape-passes
    (λ (s kind t) (and (eq? kind 'visible) (get-sphere-shape-bbox s t)))
    sphere-shape-transform
    (λ (s t) (list (sphere-shape-transform s t)))
    sphere-shape-ray-intersect
    set-sphere-shape-color
    set-sphere-shape-emitted
    set-sphere-shape-material
    default-extract-faces
    sphere-shape-tessellate
    sphere-shape-deform))
