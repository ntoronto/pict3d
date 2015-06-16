#lang typed/racket/base

(require racket/unsafe/ops
         racket/list
         racket/vector
         racket/match
         racket/promise
         racket/math
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         math/flonum
         "../math.rkt"
         "../memo.rkt"
         "../engine.rkt"
         "../soup.rkt"
         "../utils.rkt"
         "types.rkt"
         "triangle-mesh.rkt")

(provide make-rectangle-shape
         (struct-out rectangle-shape))

;; ===================================================================================================
;; Shape data type

(struct rectangle-shape shape
  ([affine : FlAffine3]
   [color : FlV4]
   [emitted : FlV4]
   [material : FlV4]
   [inside? : Boolean])
  #:transparent)

;; ===================================================================================================
;; Constructors

(: make-rectangle-shape (-> FlAffine3 FlV4 FlV4 FlV4 Boolean rectangle-shape))
(define (make-rectangle-shape t c e m back?)
  (rectangle-shape (lazy-passes) rectangle-shape-functions t c e m back?))

;; ===================================================================================================
;; Conversions

(define-values (unit-v1 unit-v5 unit-v4 unit-v8 unit-v2 unit-v6 unit-v3 unit-v7)
  (flrect3-corners unit-flrect3))

(define face-idxs-1 ((inst vector Index) 0 1 2 2 3 0))
(define face-idxs-2 ((inst vector Index) 1 2 3 3 0 1))

(require racket/bool)

(: rectangle-shape->triangle-mesh-shape (-> rectangle-shape triangle-mesh-shape))
(define (rectangle-shape->triangle-mesh-shape s)
  (match-define (rectangle-shape _ _ t c e m inside?) s)
  
  (define back? (if (flt3consistent? (rectangle-shape-affine s)) inside? (not inside?)))
  
  (define v1 (flt3apply/pos t unit-v1))
  (define v2 (flt3apply/pos t unit-v2))
  (define v3 (flt3apply/pos t unit-v3))
  (define v4 (flt3apply/pos t unit-v4))
  (define v5 (flt3apply/pos t unit-v5))
  (define v6 (flt3apply/pos t unit-v6))
  (define v7 (flt3apply/pos t unit-v7))
  (define v8 (flt3apply/pos t unit-v8))
  
  (: make-face-mesh (-> FlV3 FlV3 FlV3 FlV3 FlV3 (Values (Vectorof vtx) (Vectorof Index))))
  (define (make-face-mesh v1 v2 v3 v4 n)
    (let* ([n  (assert (flt3apply/norm t n) values)]
           [n  (if (xor back? inside?) (flv3neg n) n)])
      (values (vector (vtx v1 n c e m)
                      (vtx v2 n c e m)
                      (vtx v3 n c e m)
                      (vtx v4 n c e m))
              (if (<= (* (flv3dist v1 v3) (- 1.0 1e-8)) (flv3dist v2 v4))
                  face-idxs-1
                  face-idxs-2))))
  
  (define-values (-z-vtxs -z-idxs) (make-face-mesh v4 v3 v2 v1 -z-flv3))
  (define-values (+z-vtxs +z-idxs) (make-face-mesh v5 v6 v7 v8 +z-flv3))
  (define-values (-y-vtxs -y-idxs) (make-face-mesh v1 v2 v6 v5 -y-flv3))
  (define-values (+y-vtxs +y-idxs) (make-face-mesh v3 v4 v8 v7 +y-flv3))
  (define-values (-x-vtxs -x-idxs) (make-face-mesh v4 v1 v5 v8 -x-flv3))
  (define-values (+x-vtxs +x-idxs) (make-face-mesh v2 v3 v7 v6 +x-flv3))
  
  (define vtxs (list -z-vtxs +z-vtxs -y-vtxs +y-vtxs -x-vtxs +x-vtxs))
  (define idxs (list -z-idxs +z-idxs -y-idxs +y-idxs -x-idxs +x-idxs))
  
  (let-values ([(vtxs idxs)  (indexed-vector-append vtxs idxs)])
    (make-triangle-mesh-shape vtxs idxs back?)))

;; ===================================================================================================
;; Set attributes

(: set-rectangle-shape-color (-> shape FlV4 rectangle-shape))
(define (set-rectangle-shape-color s c)
  (match-define (rectangle-shape _ _ t _ e m inside?) s)
  (make-rectangle-shape t c e m inside?))

(: set-rectangle-shape-emitted (-> shape FlV4 rectangle-shape))
(define (set-rectangle-shape-emitted s e)
  (match-define (rectangle-shape _ _ t c _ m inside?) s)
  (make-rectangle-shape t c e m inside?))

(: set-rectangle-shape-material (-> shape FlV4 rectangle-shape))
(define (set-rectangle-shape-material s m)
  (match-define (rectangle-shape _ _ t c e _ inside?) s)
  (make-rectangle-shape t c e m inside?))

;; ===================================================================================================
;; Shape passes

(: get-rectangle-shape-passes (-> shape passes))
(define (get-rectangle-shape-passes s)
  (let ([s  (assert s rectangle-shape?)])
    (get-triangle-mesh-shape-passes (rectangle-shape->triangle-mesh-shape s))))

;; ===================================================================================================
;; Bounding box

(: get-rectangle-shape-bbox (-> shape FlAffine3 bbox))
(define (get-rectangle-shape-bbox s t)
  (let ([s  (assert s rectangle-shape?)])
    (bbox (flrect3-transform unit-flrect3 (flt3compose t (rectangle-shape-affine s)))
          0.0)))

;; ===================================================================================================
;; Transform

(: rectangle-shape-transform (-> shape FlAffine3 rectangle-shape))
(define (rectangle-shape-transform s t)
  (match-define (rectangle-shape _ _ t0 c e m inside?) s)
  (make-rectangle-shape (flt3compose t t0) c e m inside?))

;; ===================================================================================================
;; Ray intersection

(: unit-rectangle-line-intersects (-> Boolean Nonnegative-Flonum
                                      (-> FlV3 FlV3 (Values (U #f Flonum) (U #f Flonum)))))
(define ((unit-rectangle-line-intersects inside? max-time) sv sdv)
  (let-values ([(min-time max-time)  (cond [inside?  (values 0.0 +inf.0)]
                                           [else     (values -inf.0 max-time)])])
    (flrect3-line-intersects unit-flrect3 sv sdv min-time max-time)))

(: unit-rectangle-intersect-normal (-> Boolean (-> FlV3 FlV3 Nonnegative-Flonum (U #f FlV3))))
(define ((unit-rectangle-intersect-normal inside?) sv sdv time)
  (define p (flrect3-closest-point unit-flrect3 (flv3fma sdv time sv)))
  (let ([ns  (flrect3-point-normals unit-flrect3 p)])
    (cond [(empty? ns)  #f]
          [inside?  (argmax (λ ([n : FlV3]) (flv3dot n sdv)) ns)]
          [else     (argmin (λ ([n : FlV3]) (flv3dot n sdv)) ns)])))

(: rectangle-shape-ray-intersect (-> shape FlV3 FlV3 Nonnegative-Flonum
                                     (Values (U #f Nonnegative-Flonum) (U #f (Promise trace-data)))))
(define (rectangle-shape-ray-intersect s v dv max-time)
  (with-asserts ([s  rectangle-shape?])
    (define t (rectangle-shape-affine s))
    (define inside? (rectangle-shape-inside? s))
    (transformed-shape-intersect t inside? v dv max-time
                                 (unit-rectangle-line-intersects inside? max-time)
                                 (unit-rectangle-intersect-normal inside?))))

;; ===================================================================================================
;; Tessellation

(: rectangle-shape-tessellate (-> shape FlAffine3 Positive-Flonum Nonnegative-Flonum
                                  (Values (Listof shape) (Listof (face deform-data #f)))))
(define (rectangle-shape-tessellate s t0 max-edge _)
  (match-define (rectangle-shape _ _ t c e m inside?) s)
  
  (define-values (xnum ynum znum)
    (let ([t0  (flt3compose t0 t)])
      (define xdist (flv3dist (flt3apply/pos t0 -x-flv3) (flt3apply/pos t0 +x-flv3)))
      (define ydist (flv3dist (flt3apply/pos t0 -y-flv3) (flt3apply/pos t0 +y-flv3)))
      (define zdist (flv3dist (flt3apply/pos t0 -z-flv3) (flt3apply/pos t0 +z-flv3)))
      (values (assert (max 1 (exact-ceiling (/ xdist max-edge))) index?)
              (assert (max 1 (exact-ceiling (/ ydist max-edge))) index?)
              (assert (max 1 (exact-ceiling (/ zdist max-edge))) index?))))
  (define xstep (/ 2.0 (fl xnum)))
  (define ystep (/ 2.0 (fl ynum)))
  (define zstep (/ 2.0 (fl znum)))
  
  (define data linear-deform-data)
  (define reverse? (if (flt3consistent? t) inside? (not inside?)))
  
  (: add-faces (-> (Listof (face deform-data #f)) vtx vtx vtx vtx (Listof (face deform-data #f))))
  (define (add-faces fs vtx1 vtx2 vtx3 vtx4)
    (append (make-quad-faces vtx1 vtx2 vtx3 vtx4 data #f #f #f #f #f #f t0 reverse?) fs))
  
  (: add-opposite-faces (-> (Listof (face deform-data #f)) FlV3 Index Flonum Index Flonum
                            (-> Flonum Flonum Flonum FlV3)
                            (Listof (face deform-data #f))))
  (define (add-opposite-faces fs n ynum ystep znum zstep swizzled-flv3)
    (define n+ (let ([n+  (assert (flt3apply/norm t n) values)])
                 (if inside? (flv3neg n+) n+)))
    (define n- (flv3neg n+))
    (for*/fold ([fs : (Listof (face deform-data #f))  fs])
               ([j  (in-range ynum)]
                [k  (in-range znum)])
      (define y0 (- (* (fl j) ystep) 1.0))
      (define y1 (- (* (fl (+ j 1)) ystep) 1.0))
      (define z0 (- (* (fl k) zstep) 1.0))
      (define z1 (- (* (fl (+ k 1)) zstep) 1.0))
      
      (define v1+ (flt3apply/pos t (swizzled-flv3 +1.0 y0 z0)))
      (define v2+ (flt3apply/pos t (swizzled-flv3 +1.0 y1 z0)))
      (define v3+ (flt3apply/pos t (swizzled-flv3 +1.0 y1 z1)))
      (define v4+ (flt3apply/pos t (swizzled-flv3 +1.0 y0 z1)))
      (define v1- (flt3apply/pos t (swizzled-flv3 -1.0 y0 z0)))
      (define v2- (flt3apply/pos t (swizzled-flv3 -1.0 y1 z0)))
      (define v3- (flt3apply/pos t (swizzled-flv3 -1.0 y1 z1)))
      (define v4- (flt3apply/pos t (swizzled-flv3 -1.0 y0 z1)))
      (define vtx1+ (vtx v1+ n+ c e m))
      (define vtx2+ (vtx v2+ n+ c e m))
      (define vtx3+ (vtx v3+ n+ c e m))
      (define vtx4+ (vtx v4+ n+ c e m))
      (define vtx1- (vtx v1- n- c e m))
      (define vtx2- (vtx v2- n- c e m))
      (define vtx3- (vtx v3- n- c e m))
      (define vtx4- (vtx v4- n- c e m))
      (let* ([fs  (add-faces fs vtx1+ vtx2+ vtx3+ vtx4+)]
             [fs  (add-faces fs vtx4- vtx3- vtx2- vtx1-)])
        fs)))
    
  (define fs
    (let* ([fs  empty]
           [fs  (add-opposite-faces fs +x-flv3 ynum ystep znum zstep flv3)]
           [fs  (add-opposite-faces fs +y-flv3 znum zstep xnum xstep (λ (y z x) (flv3 x y z)))]
           [fs  (add-opposite-faces fs +z-flv3 xnum xstep ynum ystep (λ (z x y) (flv3 x y z)))])
      fs))
  
  (values empty fs))

;; ===================================================================================================
;; Warp

(: rectangle-shape-deform (-> shape FlSmooth3 (U Null (List rectangle-shape))))
(define (rectangle-shape-deform s t0)
  (match-define (rectangle-shape _ _ t c e m inside?) s)
  (let ([t  (fls3apply/affine t0 t)])
    (if t (list (make-rectangle-shape t c e m inside?)) empty)))

;; ===================================================================================================

(define rectangle-shape-functions
  (deform-shape-functions
    get-rectangle-shape-passes
    (λ (s kind t) (and (eq? kind 'visible) (get-rectangle-shape-bbox s t)))
    rectangle-shape-transform
    (λ (s t) (list (rectangle-shape-transform s t)))
    rectangle-shape-ray-intersect
    set-rectangle-shape-color
    set-rectangle-shape-emitted
    set-rectangle-shape-material
    default-extract-faces
    rectangle-shape-tessellate
    rectangle-shape-deform))
