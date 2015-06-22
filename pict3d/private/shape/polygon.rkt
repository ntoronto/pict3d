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
         "types.rkt"
         "triangle-mesh.rkt")

(provide make-quad-shape
         (struct-out quad-shape))

;; ===================================================================================================
;; Shape data types

(struct quad-shape shape
  ([vtxs : (Vectorof vtx)]
   [back? : Boolean]
   [lazy-mesh : (Promise triangle-mesh-shape)])
  #:transparent)

;; ===================================================================================================
;; Constructors

(: make-quad-shape (-> (Vectorof vtx) Boolean quad-shape))
(define (make-quad-shape vtxs back?)
  (unless (= 4 (vector-length vtxs))
    (raise-argument-error 'make-quad-shape "length-4 vector" 0 vtxs back?))
  (: s quad-shape)
  (define s
    (quad-shape (lazy-passes) quad-shape-functions
                vtxs
                back?
                (delay (quad-shape->triangle-mesh-shape s))))
  s)

(: quad-shape->triangle-mesh-shape (-> quad-shape triangle-mesh-shape))
(define (quad-shape->triangle-mesh-shape s)
  (make-quad-triangle-mesh-shape (quad-shape-vtxs s) (quad-shape-back? s)))

(: quad-shape-mesh (-> quad-shape triangle-mesh-shape))
(define (quad-shape-mesh s)
  (force (quad-shape-lazy-mesh s)))

;; ===================================================================================================
;; Set attributes

(: set-quad-shape-color (-> shape FlV4 quad-shape))
(define (set-quad-shape-color s c)
  (match-define (quad-shape _ _ vtxs faces back?) s)
  (quad-shape (lazy-passes) quad-shape-functions
              (vector-map (λ ([v : vtx]) (set-vtx-color v c)) vtxs)
              faces back?))

(: set-quad-shape-emitted (-> shape FlV4 quad-shape))
(define (set-quad-shape-emitted s e)
  (match-define (quad-shape _ _ vtxs faces back?) s)
  (quad-shape (lazy-passes) quad-shape-functions
              (vector-map (λ ([v : vtx]) (set-vtx-emitted v e)) vtxs)
              faces back?))

(: set-quad-shape-material (-> shape FlV4 quad-shape))
(define (set-quad-shape-material s m)
  (match-define (quad-shape _ _ vtxs faces back?) s)
  (quad-shape (lazy-passes) quad-shape-functions
              (vector-map (λ ([v : vtx]) (set-vtx-material v m)) vtxs)
              faces back?))

;; ===================================================================================================
;; Shape passes

(: get-quad-shape-passes (-> shape passes))
(define (get-quad-shape-passes s)
  (get-triangle-mesh-shape-passes (quad-shape-mesh (assert s quad-shape?))))

;; ===================================================================================================
;; Bounding box

(: get-quad-shape-bbox (-> shape FlAffine3 bbox))
(define (get-quad-shape-bbox s t)
  (let ([s  (assert s quad-shape?)])
    (match-define (vector vtx1 vtx2 vtx3 vtx4) (quad-shape-vtxs s))
    (bbox (flrect3 (vtx-position vtx1)
                   (vtx-position vtx2)
                   (vtx-position vtx3)
                   (vtx-position vtx4))
          0.0)))

;; ===================================================================================================
;; Transform

(: quad-shape-transform (-> shape FlAffine3 quad-shape))
(define (quad-shape-transform s t)
  (match-define (quad-shape _ _ vtxs back? _) s)
  
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
  
  (make-quad-shape (vector-map transform-vtx vtxs) new-back?))

;; ===================================================================================================
;; Ray intersection

(: quad-shape-ray-intersect (-> shape FlV3 FlV3 Nonnegative-Flonum
                                (Values (U #f Nonnegative-Flonum)
                                        (U #f (Promise trace-data)))))
(define (quad-shape-ray-intersect s v dv max-time)
  (let ([s  (quad-shape-mesh (assert s quad-shape?))])
    (triangle-mesh-shape-ray-intersect s v dv max-time)))

;; ===================================================================================================
;; Tessellation

(: quad-shape-extract-faces (-> shape (Values (Listof shape) (Listof (face deform-data #f)))))
(define (quad-shape-extract-faces s)
  (shape-extract-faces (quad-shape-mesh (assert s quad-shape?))))

(: inverse-bilinear (-> FlV3 FlV3 FlV3 FlV3 FlV3 (Values (U #f Flonum) (U #f Flonum))))
(define (inverse-bilinear p0 p1 p2 p3 p)
  (define n (flv3polygon-perp p0 p1 p2 p3))
  ;; point-at always returns an invertible transformation
  (define plane-t (assert (flt3inverse (point-at-flt3 p n)) values))
  ;; p is 0,0,0 under this transformation
  (define-values (x0 y0 _z0) (call/flv3-values (flt3apply/pos plane-t p0) values))
  (define-values (x1 y1 _z1) (call/flv3-values (flt3apply/pos plane-t p1) values))
  (define-values (x2 y2 _z2) (call/flv3-values (flt3apply/pos plane-t p2) values))
  (define-values (x3 y3 _z3) (call/flv3-values (flt3apply/pos plane-t p3) values))
  (define A (fl2cross x0 y0 (- x0 x2) (- y0 y2)))
  (define B (* 0.5 (+ (fl2cross x0 y0 (- x1 x3) (- y1 y3))
                      (fl2cross x1 y1 (- x0 x2) (- y0 y2)))))
  (define C (fl2cross x1 y1 (- x1 x3) (- y1 y3)))
  (define discr (- (* B B) (* A C)))
  (define denom (+ A (* -2.0 B) C))
  (define s
    (cond
      [(< (abs denom) epsilon.0)
       (define s (/ A (- A C)))
       (if (flrational? s) s #f)]
      [(< discr 0.0)  #f]
      [else
       (define s0 (/ (+ (- A B) (flsqrt discr)) denom))
       (define s1 (/ (- (- A B) (flsqrt discr)) denom))
       (cond [(<= 0.0 s0 1.0)  s0]
             [(<= 0.0 s1 1.0)  s1]
             [else  (define d0 (min (abs s0) (abs (- s0 1.0))))
                    (define d1 (min (abs s1) (abs (- s1 1.0))))
                    (if (< d0 d1) s0 s1)])]))
  
  (cond
    [(not s)  (values #f #f)]
    [else
     (define denom-x (+ (* (- 1.0 s) (- x0 x2)) (* s (- x1 x3))))
     (define denom-y (+ (* (- 1.0 s) (- y0 y2)) (* s (- y1 y3))))
     (define t
       (if (> (abs denom-x) (abs denom-y))
           (/ (+ (* (- 1.0 s) x0) (* s x1)) denom-x)
           (/ (+ (* (- 1.0 s) y0) (* s y1)) denom-y)))
     (values (flclamp s 0.0 1.0) (flclamp t 0.0 1.0))]))

(: make-bilinear-deform-data (-> FlV3 FlV3 FlV3 FlV3 deform-data))
(define (make-bilinear-deform-data v11 v12 v22 v21)
  (deform-data
   (λ (v1 v2 α)
     (define v (flv3blend v1 v2 α))
     (define-values (s t) (inverse-bilinear v11 v12 v21 v22 v))
     (if (and s t)
         (flv3blend (flv3blend v11 v12 s)
                    (flv3blend v21 v22 s)
                    t)
         v))
   vtx-interpolate))

(: quad-shape-tessellate (-> shape FlAffine3 Positive-Flonum Nonnegative-Flonum
                             (Values Null (Listof (face deform-data #f)))))
(define (quad-shape-tessellate s t0 max-edge max-angle)
  (match-define (quad-shape _ _ (vector vtx1 vtx2 vtx3 vtx4) back? _) s)
  
  (define v1 (vtx-position vtx1))
  (define v2 (vtx-position vtx2))
  (define v3 (vtx-position vtx3))
  (define v4 (vtx-position vtx4))
  
  (define-values (hdist vdist)
    (let ()
      (define w1 (flt3apply/pos t0 v1))
      (define w2 (flt3apply/pos t0 v2))
      (define w3 (flt3apply/pos t0 v3))
      (define w4 (flt3apply/pos t0 v4))
      (values (max (flv3dist w1 w2) (flv3dist w3 w4))
              (max (flv3dist w2 w3) (flv3dist w4 w1)))))
  (define hnum (max 1 (exact-ceiling (/ hdist max-edge))))
  (define vnum (max 1 (exact-ceiling (/ vdist max-edge))))
  
  (define vtxss
    (for/list : (Listof (Listof vtx)) ([j  (in-range (+ hnum 1))])
      (define α (/ (fl j) (fl hnum)))
      (define vtx12 (vtx-blend vtx1 vtx2 α))
      (define vtx43 (vtx-blend vtx4 vtx3 α))
      (for/list : (Listof vtx) ([i  (in-range (+ vnum 1))])
        (define β (/ (fl i) (fl vnum)))
        (vtx-blend vtx12 vtx43 β))))
  
  (define data (make-bilinear-deform-data v1 v2 v3 v4))
  (define fs
    (append*
     (for/list : (Listof (Listof (face deform-data #f))) ([vtxs1  (in-list vtxss)]
                                                          [vtxs2  (in-list (rest vtxss))])
       (append*
        (for/list : (Listof (Listof (face deform-data #f))) ([vtx11  (in-list vtxs1)]
                                                             [vtx12  (in-list (rest vtxs1))]
                                                             [vtx21  (in-list vtxs2)]
                                                             [vtx22  (in-list (rest vtxs2))])
          (make-quad-faces vtx11 vtx21 vtx22 vtx12 data #f #f #f #f #f #f t0 back?))))))
  
  (values empty fs))

;; ===================================================================================================
;; Warp

(: quad-shape-deform (-> shape FlSmooth3 (Listof shape)))
(define (quad-shape-deform s t)
  (match-define (quad-shape _ _ vtxs back? _) s)
  (define n (vector-length vtxs))
  (define-values (vtx0 d0 c0?) (fls3apply/vtx t (vector-ref vtxs 0)))
  (define new-back? (if c0? back? (not back?)))
  (define new-vtxs (make-vector n (if c0? vtx0 (vtx-flip-normal vtx0))))
  (let loop ([i : Positive-Fixnum  1])
    (cond
      [(< i n)
       (define-values (vtxi di ci?) (fls3apply/vtx t (unsafe-vector-ref vtxs i)))
       (cond [(eq? c0? ci?)
              (unsafe-vector-set! new-vtxs i (if ci? vtxi (vtx-flip-normal vtxi)))
              (loop (+ i 1))]
             [else
              (triangle-mesh-shape-deform (quad-shape-mesh (assert s quad-shape?)) t)])]
      [else
       (list (make-quad-shape new-vtxs new-back?))])))

;; ===================================================================================================

(define quad-shape-functions
  (deform-shape-functions
    get-quad-shape-passes
    (λ (s kind t) (and (eq? kind 'visible) (get-quad-shape-bbox s t)))
    quad-shape-transform
    (λ (s t) (list (quad-shape-transform s t)))
    quad-shape-ray-intersect
    set-quad-shape-color
    set-quad-shape-emitted
    set-quad-shape-material
    quad-shape-extract-faces
    quad-shape-tessellate
    quad-shape-deform))
