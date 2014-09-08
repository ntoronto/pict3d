#lang typed/racket/base

(require (for-syntax racket/base)
         racket/match
         racket/list
         racket/vector
         math/flonum
         racket/unsafe/ops
         (only-in racket/math sqr pi)
         "ev3.rkt"
         "flonum.rkt"
         "fl2dot.rkt"
         "fast-fl2.rkt"
         "../utils.rkt")

(provide x-axis-flv3
         y-axis-flv3
         z-axis-flv3
         
         flv3 flv3? flv3-x flv3-y flv3-z flv3-values
         ev3->flv3
         flv3->ev3
         flv3ulp-error
         flv3zero?
         flv3rational?
         
         flv3dot
         flv3cross
         flv3+
         flv3-
         flv3neg
         flv3*
         flv3/
         flv3fma
         flv3mag^2
         flv3mag
         flv3normalize
         flv3dist^2
         flv3dist
         flv3equiv?
         flv3aabb-values
         
         FlPlane3 flplane3? flplane3-normal flplane3-distance
         (rename-out [flplane3* flplane3])
         
         eplane3->flplane3
         flplane3->eplane3
         flplane3-flip
         flplane3-point-dist
         flplane3-line-isect-time
         flplane3-line-isect
         flplane3-relative-dists
         
         flv3polygon-centroid
         flv3polygon-perp
         flv3polygon-normal
         flv3polygon-plane
         flv3polygon-perimeter
         flv3polygon-area
         flv3polygon-regularity
         
         flv3triangle-contains-point?
         )

(define x-axis-flv3 (flvector 1.0 0.0 0.0))
(define y-axis-flv3 (flvector 0.0 1.0 0.0))
(define z-axis-flv3 (flvector 0.0 0.0 1.0))

;; ===================================================================================================
;; 3-component flonum vectors

(: flv3 (-> Flonum Flonum Flonum FlVector))
(define (flv3 x y z)
  (flvector x y z))

(: flv3? (-> FlVector Boolean))
(define (flv3? v) (= 3 (flvector-length v)))

(: flv3-x (-> FlVector Flonum))
(define (flv3-x v)
  (if (= 3 (flvector-length v))
      (unsafe-flvector-ref v 0)
      (raise-type-error 'flv3-x "length-3 FlVector" v)))

(: flv3-y (-> FlVector Flonum))
(define (flv3-y v)
  (if (= 3 (flvector-length v))
      (unsafe-flvector-ref v 1)
      (raise-type-error 'flv3-y "length-3 FlVector" v)))

(: flv3-z (-> FlVector Flonum))
(define (flv3-z v)
  (if (= 3 (flvector-length v))
      (unsafe-flvector-ref v 2)
      (raise-type-error 'flv3-z "length-3 FlVector" v)))

;(: flv3-values (-> FlVector (Values Flonum Flonum Flonum)))
(define-syntax-rule (flv3-values v-stx)
  (let ([v : FlVector  v-stx])
    (unless (= 3 (flvector-length v))
      (raise-type-error 'flv3-values "length-3 FlVector" v))
    (values (unsafe-flvector-ref v 0)
            (unsafe-flvector-ref v 1)
            (unsafe-flvector-ref v 2))))

(: ev3->flv3 (-> ev3 FlVector))
(define (ev3->flv3 v)
  (match-define (ev3 x y z) v)
  (flvector (fl x) (fl y) (fl z)))

(: flv3->ev3 (-> FlVector ev3))
(define (flv3->ev3 v)
  (define-values (x y z) (flv3-values v))
  (cond [(and (flrational? x) (flrational? y) (flrational? z))
         (ev3 (inexact->exact x) (inexact->exact y) (inexact->exact z))]
        [else
         (raise-type-error 'flv3->ev3 "rational FlVector" v)]))

(: flv3ulp-error (-> FlVector ev3 Flonum))
(define (flv3ulp-error v rv)
  (define-values (x y z) (flv3-values v))
  (match-define (ev3 rx ry rz) rv)
  (max (flulp-error x rx) (flulp-error y ry) (flulp-error z rz)))

(: flv3rational? (-> FlVector Boolean))
(define (flv3rational? v)
  (define-values (x y z) (flv3-values v))
  (and (flrational? x) (flrational? y) (flrational? z)))

(: flv3zero? (-> FlVector Boolean))
(define (flv3zero? v)
  (define-values (x y z) (flv3-values v))
  (and (= x 0.0) (= y 0.0) (= z 0.0)))

;; ---------------------------------------------------------------------------------------------------
;; Dot product: <= 0.5ulp error

(: fast-fl3dot (-> Flonum Flonum Flonum Flonum Flonum Flonum Flonum))
(define (fast-fl3dot x1 y1 z1 x2 y2 z2)
  (let*-values ([(a-hi a-lo)  (fast-fl*/error x1 x2)]
                [(b-hi b-lo)  (fast-flfma/error y1 y2 a-hi)]
                [(c-hi c-lo)  (fast-flfma/error z1 z2 b-hi)])
    (+ c-hi (+ c-lo (+ b-lo a-lo)))))

(require "fle2.rkt")

(: slow-fl3dot (-> Flonum Flonum Flonum Flonum Flonum Flonum Flonum))
(define (slow-fl3dot x1 y1 z1 x2 y2 z2)
  (let*-values ([(x.e x.hi x.lo)  (fl*/e2 x1 x2)]
                [(y.e y.hi y.lo)  (fl*/e2 y1 y2)]
                [(z.e z.hi z.lo)  (fl*/e2 z1 z2)]
                [(a.e a.hi a.lo)  (fle2+ x.e x.hi x.lo y.e y.hi y.lo)]
                [(a.e a.hi a.lo)  (fle2+ a.e a.hi a.lo z.e z.hi z.lo)])
    (fle2->flonum a.e a.hi a.lo)))

(: flv3dot (-> FlVector FlVector Flonum))
(define (flv3dot v1 v2)
  (define-values (x1 y1 z1) (flv3-values v1))
  (define-values (x2 y2 z2) (flv3-values v2))
  (define d (fast-fl3dot x1 y1 z1 x2 y2 z2))
  (if (< +max-subnormal.0 (abs d) +inf.0) d (slow-fl3dot x1 y1 z1 x2 y2 z2)))

;; ---------------------------------------------------------------------------------------------------

(: fast-fl3dot/error (-> Flonum Flonum Flonum Flonum Flonum Flonum (Values Flonum Flonum)))
(define (fast-fl3dot/error x1 y1 z1 x2 y2 z2)
  (let*-values ([(a-hi a-lo)  (fast-fl*/error x1 x2)]
                [(b-hi b-lo)  (fast-flfma/error y1 y2 a-hi)]
                [(c-hi c-lo)  (fast-flfma/error z1 z2 b-hi)]
                [(d-hi d-lo)  (fast-fl+/error a-lo b-lo)])
    (fast-fl2+ c-hi c-lo d-hi d-lo)))

(: slow-fl3dot/error (-> Flonum Flonum Flonum Flonum Flonum Flonum (Values Flonum Flonum)))
(define (slow-fl3dot/error x1 y1 z1 x2 y2 z2)
  (let-values ([(x1 x2)  (abs-sort x1 x2)]
               [(y1 y2)  (abs-sort y1 y2)]
               [(z1 z2)  (abs-sort z1 z2)])
    (define n1 (flpow2near (/ (max (abs x1) (abs y1) (abs z1)) (flsqrt (/ +max.0 256.0)))))
    (define n2 (flpow2near (/ (max (abs x2) (abs y2) (abs z2)) (flsqrt (/ +max.0 256.0)))))
    (define-values (d-hi d-lo)
      (fast-fl3dot/error (/ x1 n1) (/ y1 n1) (/ z1 n1) (/ x2 n2) (/ y2 n2) (/ z2 n2)))
    (values (fl** d-hi n1 n2) (fl** d-lo n1 n2))))

(: fl3dot/error (-> Flonum Flonum Flonum Flonum Flonum Flonum (Values Flonum Flonum)))
(define (fl3dot/error x1 y1 z1 x2 y2 z2)
  (define-values (d-hi d-lo) (fast-fl3dot/error x1 y1 z1 x2 y2 z2))
  (if (< +max-subnormal.hi (abs (+ d-hi d-lo)) +inf.0)
      (values d-hi d-lo)
      (slow-fl3dot/error x1 y1 z1 x2 y2 z2)))

(: flv3dot/error (-> FlVector FlVector (Values Flonum Flonum)))
(define (flv3dot/error v1 v2)
  (define-values (x1 y1 z1) (flv3-values v1))
  (define-values (x2 y2 z2) (flv3-values v2))
  (fl3dot/error x1 y1 z1 x2 y2 z2))

;; ---------------------------------------------------------------------------------------------------
;; Cross product: <= 0.5ulp error, except sometimes when output is subnormal

(: fl2cross (-> Flonum Flonum Flonum Flonum Flonum))
(define (fl2cross x1 y1 x2 y2)
  (fl2dot x1 y1 y2 (- x2)))

(: fl3cross (-> Flonum Flonum Flonum Flonum Flonum Flonum (Values Flonum Flonum Flonum)))
(define (fl3cross x1 y1 z1 x2 y2 z2)
  (values (fl2cross y1 z1 y2 z2)
          (fl2cross z1 x1 z2 x2)
          (fl2cross x1 y1 x2 y2)))

(: flv3cross (-> FlVector FlVector FlVector))
(define (flv3cross v1 v2)
  (define-values (x1 y1 z1) (flv3-values v1))
  (define-values (x2 y2 z2) (flv3-values v2))
  (let-values ([(x y z)  (fl3cross x1 y1 z1 x2 y2 z2)])
    (flvector x y z)))

;; ---------------------------------------------------------------------------------------------------

(: fl2cross/error (-> Flonum Flonum Flonum Flonum (Values Flonum Flonum)))
(define (fl2cross/error x1 y1 x2 y2)
  (fl2dot/error x1 y1 y2 (- x2)))

(: fl3cross/error (-> Flonum Flonum Flonum Flonum Flonum Flonum
                      (Values Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (fl3cross/error x1 y1 z1 x2 y2 z2)
  (let-values ([(x3-hi x3-lo)  (fl2cross/error y1 z1 y2 z2)]
               [(y3-hi y3-lo)  (fl2cross/error z1 x1 z2 x2)]
               [(z3-hi z3-lo)  (fl2cross/error x1 y1 x2 y2)])
    (values x3-hi y3-hi z3-hi x3-lo y3-lo z3-lo)))

(: flv3cross/error (-> FlVector FlVector (Values FlVector FlVector)))
(define (flv3cross/error v1 v2)
  (define-values (x1 y1 z1) (flv3-values v1))
  (define-values (x2 y2 z2) (flv3-values v2))
  (let-values ([(x-hi y-hi z-hi x-lo y-lo z-lo)  (fl3cross/error x1 y1 z1 x2 y2 z2)])
    (values (flvector x-hi y-hi z-hi)
            (flvector x-lo y-lo z-lo))))

;; ---------------------------------------------------------------------------------------------------

(: fast-fl2cross/error (-> Flonum Flonum Flonum Flonum (Values Flonum Flonum)))
(define (fast-fl2cross/error x1 y1 x2 y2)
  (fast-fl2dot/error x1 y1 y2 (- x2)))

(: fast-fl3cross/error (-> Flonum Flonum Flonum Flonum Flonum Flonum
                           (Values Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (fast-fl3cross/error x1 y1 z1 x2 y2 z2)
  (let-values ([(x3-hi x3-lo)  (fast-fl2cross/error y1 z1 y2 z2)]
               [(y3-hi y3-lo)  (fast-fl2cross/error z1 x1 z2 x2)]
               [(z3-hi z3-lo)  (fast-fl2cross/error x1 y1 x2 y2)])
    (values x3-hi y3-hi z3-hi x3-lo y3-lo z3-lo)))

(: fast-flv3cross/error (-> FlVector FlVector (Values FlVector FlVector)))
(define (fast-flv3cross/error v1 v2)
  (define-values (x1 y1 z1) (flv3-values v1))
  (define-values (x2 y2 z2) (flv3-values v2))
  (let-values ([(x-hi y-hi z-hi x-lo y-lo z-lo)  (fast-fl3cross/error x1 y1 z1 x2 y2 z2)])
    (values (flvector x-hi y-hi z-hi)
            (flvector x-lo y-lo z-lo))))

;; ---------------------------------------------------------------------------------------------------
;; Vector arithmetic: <= 0.5ulp error (all are pointwise maps of <= 0.5ulp error functions)

(: flv3+ (-> FlVector FlVector FlVector))
(define (flv3+ v1 v2)
  (define-values (x1 y1 z1) (flv3-values v1))
  (define-values (x2 y2 z2) (flv3-values v2))
  (flvector (+ x1 x2) (+ y1 y2) (+ z1 z2)))

(: flv3- (-> FlVector FlVector FlVector))
(define (flv3- v1 v2)
  (define-values (x1 y1 z1) (flv3-values v1))
  (define-values (x2 y2 z2) (flv3-values v2))
  (flvector (- x1 x2) (- y1 y2) (- z1 z2)))

(: flv3neg (-> FlVector FlVector))
(define (flv3neg v)
  (flv3* v -1.0))

(: flv3* (-> FlVector Flonum FlVector))
(define (flv3* v a)
  (define-values (x y z) (flv3-values v))
  (flvector (* x a) (* y a) (* z a)))

(: flv3/ (-> FlVector Flonum FlVector))
(define (flv3/ v a)
  (define-values (x y z) (flv3-values v))
  (flvector (/ x a) (/ y a) (/ z a)))

(: flv3fma (-> FlVector Flonum FlVector FlVector))
(define (flv3fma dv a v0)
  (define-values (dx dy dz) (flv3-values dv))
  (define-values (x0 y0 z0) (flv3-values v0))
  (flvector (flfma dx a x0) (flfma dy a y0) (flfma dz a z0)))

;; ---------------------------------------------------------------------------------------------------
;; Magnitude squared: <= 0.5ulp error, except sometimes when output is subnormal

(: fast-fl3mag^2/error (-> Flonum Flonum Flonum (Values Flonum Flonum)))
(define (fast-fl3mag^2/error x y z)
  (let*-values ([(a-hi a-lo)  (fast-flsqr/error x)]
                [(b-hi b-lo)  (fast-flfsa/error y a-hi)]
                [(c-hi c-lo)  (fast-flfsa/error z b-hi)])
    (values c-hi (+ c-lo (+ b-lo a-lo)))))

(: fast-fl3mag^2 (-> Flonum Flonum Flonum Flonum))
(define (fast-fl3mag^2 x y z)
  (define-values (d-hi d-lo) (fast-fl3mag^2/error x y z))
  (+ d-hi d-lo))

(: slow-fl3mag^2 (-> Flonum Flonum Flonum Flonum))
(define (slow-fl3mag^2 x y z)
  (define n (flpow2near (/ (max (abs x) (abs y) (abs z)) (flsqrt (/ +max.0 256.0)))))
  (fl** n n (fast-fl3mag^2 (/ x n) (/ y n) (/ z n))))

(: fl3mag^2 (-> Flonum Flonum Flonum Nonnegative-Flonum))
(define (fl3mag^2 x y z)
  (define d (fast-fl3mag^2 x y z))
  (max 0.0 (if (< +max-subnormal.0 (abs d) +inf.0) d (slow-fl3mag^2 x y z))))

(: flv3mag^2 (-> FlVector Nonnegative-Flonum))
(define (flv3mag^2 v)
  (define-values (x y z) (flv3-values v))
  (fl3mag^2 x y z))

;; ---------------------------------------------------------------------------------------------------
;; Magnitude: <= 0.5ulp error

(: fast-fl3mag/error (-> Flonum Flonum Flonum (Values Flonum Flonum)))
(define (fast-fl3mag/error x y z)
  (define-values (d-hi d-lo) (fast-fl3mag^2/error x y z))
  (fast-fl2sqrt d-hi d-lo))

(: fast-fl3mag (-> Flonum Flonum Flonum Flonum))
(define (fast-fl3mag x y z)
  (define-values (d-hi d-lo) (fast-fl3mag^2/error x y z))
  (fast-flsqrt+ d-hi d-lo))

(: slow-fl3mag (-> Flonum Flonum Flonum Flonum))
(define (slow-fl3mag x y z)
  (define n (flpow2near (max (abs x) (abs y) (abs z))))
  (fl* n (fast-fl3mag (/ x n) (/ y n) (/ z n))))

(: fl3mag (-> Flonum Flonum Flonum Nonnegative-Flonum))
(define (fl3mag x y z)
  (define d (fast-fl3mag x y z))
  (max 0.0 (if (< (flsqrt +max-subnormal.0) (abs d) +inf.0) d (slow-fl3mag x y z))))

(: flv3mag (-> FlVector Nonnegative-Flonum))
(define (flv3mag v)
  (define-values (x y z) (flv3-values v))
  (fl3mag x y z))

;; ---------------------------------------------------------------------------------------------------
;; Normalization: <= 0.5ulp error, except sometimes when output is subnormal

(: fast-fl1/2->fl (-> Flonum Flonum Flonum Flonum))
(define (fast-fl1/2->fl x2 y2 y1)
  (define-values (z2 z1) (fast-fl1/2 x2 y2 y1))
  (+ z2 z1))

(: fast-fl3normalize (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum)))
(define (fast-fl3normalize x y z)
  (define-values (d-hi d-lo) (fast-fl3mag/error x y z))
  (values (fast-fl1/2->fl x d-hi d-lo)
          (fast-fl1/2->fl y d-hi d-lo)
          (fast-fl1/2->fl z d-hi d-lo)))

(: slow-fl3normalize (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum)))
(define (slow-fl3normalize x y z)
  (define n (flpow2near (/ (max (abs x) (abs y) (abs z)) (flsqrt (/ +max.0 256.0)))))
  (fast-fl3normalize (/ x n) (/ y n) (/ z n)))

(: fl3normalize (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum)))
(define (fl3normalize x y z)
  (define-values (d-hi d-lo) (fast-fl3mag/error x y z))
  (if (and (< (flsqrt +max-subnormal.hi) (abs (+ d-hi d-lo)) +inf.0)
           (< +max-subnormal.0 x)
           (< +max-subnormal.0 y)
           (< +max-subnormal.0 z))
      (values (fast-fl1/2->fl x d-hi d-lo)
              (fast-fl1/2->fl y d-hi d-lo)
              (fast-fl1/2->fl z d-hi d-lo))
      (slow-fl3normalize x y z)))

(: flv3normalize (-> FlVector (U #f FlVector)))
(define (flv3normalize v)
  (define-values (x y z) (flv3-values v))
  (cond [(and (zero? x) (zero? y) (zero? z))  #f]
        [(and (flrational? x) (flrational? y) (flrational? z))
         (let-values ([(x y z)  (fl3normalize x y z)])
           (flvector x y z))]
        [else  #f]))

;; ---------------------------------------------------------------------------------------------------
;; Distance squared: <= 2.0ulp error

(: fl3dist^2 (-> Flonum Flonum Flonum Flonum Flonum Flonum Nonnegative-Flonum))
(define (fl3dist^2 x1 y1 z1 x2 y2 z2)
  (fl3mag^2 (- x1 x2) (- y1 y2) (- z1 z2)))

(: flv3dist^2 (-> FlVector FlVector Nonnegative-Flonum))
(define (flv3dist^2 v1 v2)
  (define-values (x1 y1 z1) (flv3-values v1))
  (define-values (x2 y2 z2) (flv3-values v2))
  (fl3dist^2 x1 y1 z1 x2 y2 z2))

;; ---------------------------------------------------------------------------------------------------
;; Distance: <= 1.5ulp error

(: fl3dist (-> Flonum Flonum Flonum Flonum Flonum Flonum Nonnegative-Flonum))
(define (fl3dist x1 y1 z1 x2 y2 z2)
  (fl3mag (- x1 x2) (- y1 y2) (- z1 z2)))

(: flv3dist (-> FlVector FlVector Nonnegative-Flonum))
(define (flv3dist v1 v2)
  (define-values (x1 y1 z1) (flv3-values v1))
  (define-values (x2 y2 z2) (flv3-values v2))
  (fl3dist x1 y1 z1 x2 y2 z2))

;; ---------------------------------------------------------------------------------------------------
;; Equivalence based on relative distance

(: flv3equiv? (-> FlVector FlVector Flonum Boolean))
(define (flv3equiv? v1 v2 eps)
  (define-values (x1 y1 z1) (flv3-values v1))
  (define-values (x2 y2 z2) (flv3-values v2))
  (and (<= (abs (- x1 x2)) (* eps (max (abs x1) (abs x2))))
       (<= (abs (- y1 y2)) (* eps (max (abs y1) (abs y2))))
       (<= (abs (- z1 z2)) (* eps (max (abs z1) (abs z2))))))

;; ---------------------------------------------------------------------------------------------------
;; Bounding box

(: flv3aabb-values (-> (Vectorof FlVector) (Values Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (flv3aabb-values vs)
  (define n (vector-length vs))
  (cond [(= n 0)  (raise-type-error 'flv3aabb-values "nonempty vector" vs)]
        [else
         (define-values (x y z) (flv3-values (unsafe-vector-ref vs 0)))
         (for/fold ([xmin : Flonum  x]
                    [ymin : Flonum  y]
                    [zmin : Flonum  z]
                    [xmax : Flonum  x]
                    [ymax : Flonum  y]
                    [zmax : Flonum  z]
                    ) ([i  (in-range 1 n)])
           (define-values (x y z) (flv3-values (unsafe-vector-ref vs i)))
           (values (min xmin x) (min ymin y) (min zmin z)
                   (max xmax x) (max ymax y) (max zmax z)))]))

;; ===================================================================================================
;; Planes

(struct flplane3 ([normal : FlVector] [distance : Flonum]) #:transparent)

(define-type FlPlane3 flplane3)

(: make-flplane3 (-> FlVector Flonum (U #f FlPlane3)))
(define (make-flplane3 norm d)
  (and (not (flv3zero? norm))
       (flv3rational? norm)
       (flrational? d)
       (flplane3 norm d)))

(define-match-expander flplane3*
  (λ (stx)
    (syntax-case stx ()
      [(_ e1 e2)  (syntax/loc stx (flplane3 e1 e2))]))
  (λ (stx)
    (syntax-case stx ()
      [(_ . args)  (syntax/loc stx (make-flplane3 . args))]
      [_  (syntax/loc stx make-flplane3)])))

(: eplane3->flplane3 (-> EPlane3 (U #f FlPlane3)))
(define (eplane3->flplane3 p)
  (match-define (eplane3 norm dist) p)
  (make-flplane3 (ev3->flv3 norm) (fl dist)))

(: flplane3->eplane3 (-> FlPlane3 EPlane3))
(define (flplane3->eplane3 p)
  (match-define (flplane3 norm dist) p)
  (assert (eplane3 (flv3->ev3 norm) (inexact->exact dist)) eplane3?))

(: flplane3-flip (-> FlPlane3 FlPlane3))
(define (flplane3-flip p)
  (match-define (flplane3 norm dist) p)
  (flplane3 (flv3neg norm) (- dist)))

;; ---------------------------------------------------------------------------------------------------
;; Plane-point distance: <= 0.5ulp error

(: fast-plane-point-dist/error (-> Flonum Flonum Flonum Flonum Flonum Flonum Flonum
                                   (Values Flonum Flonum)))
(define (fast-plane-point-dist/error a b c d x y z)
  (define-values (dot-hi dot-lo) (fast-fl3dot/error a b c x y z))
  (fast-fl2+1 dot-hi dot-lo d))

(: fast-plane-point-dist (-> Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum))
(define (fast-plane-point-dist a b c d x y z)
  (define-values (dist-hi dist-lo) (fast-plane-point-dist/error a b c d x y z))
  (+ dist-hi dist-lo))

(: slow-plane-point-dist (-> Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum))
(define (slow-plane-point-dist a b c d x y z)
  (let-values ([(a x)  (abs-sort a x)]
               [(b y)  (abs-sort b y)]
               [(c z)  (abs-sort c z)])
    (define n (flpow2near (/ (max (abs a) (abs b) (abs c) (abs d)) (flsqrt (/ +max.0 256.0)))))
    (define m (flpow2near (/ (max (abs x) (abs y) (abs z) (abs d)) (flsqrt (/ +max.0 256.0)))))
    (fl** n m (fast-plane-point-dist (/ a n) (/ b n) (/ c n) (/ d n m) (/ x m) (/ y m) (/ z m)))))

(: plane-point-dist (-> Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum))
(define (plane-point-dist a b c d x y z)
  (define w (fast-plane-point-dist a b c d x y z))
  (if (< +max-subnormal.0 (abs w) +inf.0)
      w
      (slow-plane-point-dist a b c d x y z)))

(: flplane3-point-dist (-> FlPlane3 FlVector Flonum))
(define (flplane3-point-dist plane v)
  (match-define (flplane3 norm d) plane)
  (define-values (a b c) (flv3-values norm))
  (define-values (x y z) (flv3-values v))
  (plane-point-dist a b c d x y z))

;; ---------------------------------------------------------------------------------------------------
;; Relative signed distances from points to a plane: <= 2.0 ulp error

(: fast-plane-relative-dist (-> Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum))
(define (fast-plane-relative-dist a b c d x y z m)
  (/ (plane-point-dist a b c d x y z) m))

(: slow-plane-relative-dist (-> Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum))
(define (slow-plane-relative-dist a b c d x y z m)
  (let-values ([(a x)  (abs-sort a x)]
               [(b y)  (abs-sort b y)]
               [(c z)  (abs-sort c z)])
    (define s1 (flpow2near (/ (max (abs a) (abs b) (abs c) (abs d)) (flsqrt (/ +max.0 256.0)))))
    (define s2 (flpow2near (/ (max (abs x) (abs y) (abs z) (abs d)) (flsqrt (/ +max.0 256.0)))))
    (define dist
      (fl** s1 s2 (fast-plane-relative-dist (/ a s1) (/ b s1) (/ c s1) (/ d s1 s2)
                                            (/ x s2) (/ y s2) (/ z s2) m)))
    (if (flnan? dist)
        (fl (/ (+ (* (inexact->exact a) (inexact->exact x))
                  (* (inexact->exact b) (inexact->exact y))
                  (* (inexact->exact c) (inexact->exact z))
                  (inexact->exact d))
               (inexact->exact m)))
        dist)))

(: plane-relative-dist (-> Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum))
(define (plane-relative-dist a b c d x y z m)
  (define dist (fast-plane-relative-dist a b c d x y z m))
  (if (< +max-subnormal.0 (abs dist) +inf.0)
      dist
      (slow-plane-relative-dist a b c d x y z m)))

(: flplane3-relative-dists (-> FlPlane3 (Vectorof FlVector) (U #f FlVector)))
(define (flplane3-relative-dists plane vs)
  (define n (vector-length vs))
  (cond
    [(= n 0)  (flvector)]
    [else
     (define-values (xmin ymin zmin xmax ymax zmax) (flv3aabb-values vs))
     (define m (max (abs xmin) (abs xmax) (abs ymin) (abs ymax) (abs zmin) (abs zmax)))
     (cond
       [(= m 0.0)  (and (= 0.0 (flplane3-point-dist plane (unsafe-vector-ref vs 0)))
                        (make-flvector n))]
       [else
        (match-define (flplane3 norm d) plane)
        (define-values (a b c) (flv3-values norm))
        (for/flvector: ([v  (in-vector vs)])
          (define-values (x y z) (flv3-values v))
          (if (and (flrational? x) (flrational? y) (flrational? z))
              (plane-relative-dist a b c d x y z m)
              +inf.0))])]))

;; ---------------------------------------------------------------------------------------------------
;; Plane-line intersect time: <= 0.5ulp error

(: fast-plane-line-isect-time (-> Flonum Flonum Flonum Flonum
                                  Flonum Flonum Flonum
                                  Flonum Flonum Flonum
                                  (U #f Flonum)))
(define (fast-plane-line-isect-time a b c d x1 y1 z1 x2 y2 z2)
  (define-values (d1-hi d1-lo) (fast-fl3dot/error a b c x1 y1 z1))
  (define-values (d2-hi d2-lo) (fast-fl3dot/error a b c x2 y2 z2))
  (define-values (denom-hi denom-lo) (fast-fl2- d1-hi d1-lo d2-hi d2-lo))
  (if (fl2zero? denom-hi denom-lo)
      #f
      (let*-values ([(numer-hi numer-lo)  (fast-fl2+1 d1-hi d1-lo d)]
                    [(dist-hi dist-lo)    (fast-fl2/ numer-hi numer-lo denom-hi denom-lo)])
        (+ dist-hi dist-lo))))

(: slow-plane-line-isect-time (-> Flonum Flonum Flonum Flonum
                                  Flonum Flonum Flonum
                                  Flonum Flonum Flonum
                                  Any (U #f Flonum)))
(define (slow-plane-line-isect-time a b c d x1 y1 z1 x2 y2 z2 segment?)
  (define norm (ev3 (inexact->exact a) (inexact->exact b) (inexact->exact c)))
  (define plane (eplane3 norm (inexact->exact d)))
  (define v1 (ev3 (inexact->exact x1) (inexact->exact y1) (inexact->exact z1)))
  (define v2 (ev3 (inexact->exact x2) (inexact->exact y2) (inexact->exact z2)))
  (and plane (let ([t  (eplane3-line-isect-time plane v1 v2 segment?)])
               (and t (fl t)))))

(: plane-line-isect-time (-> Flonum Flonum Flonum Flonum
                             Flonum Flonum Flonum
                             Flonum Flonum Flonum
                             Any (U #f Flonum)))
(define (plane-line-isect-time a b c d x1 y1 z1 x2 y2 z2 segment?)
  (define t (fast-plane-line-isect-time a b c d x1 y1 z1 x2 y2 z2))
  (cond [(not t)  (slow-plane-line-isect-time a b c d x1 y1 z1 x2 y2 z2 segment?)]
        [segment?
         (cond [(< +max-subnormal.0 t 0.9999999999999999)  t]
               [(or (t . < . 0.0) (t . > . 1.0))  #f]
               [else  (slow-plane-line-isect-time a b c d x1 y1 z1 x2 y2 z2 segment?)])]
        [else
         (cond [(< +max-subnormal.0 (abs t) +inf.0)  t]
               [else  (slow-plane-line-isect-time a b c d x1 y1 z1 x2 y2 z2 segment?)])]))

(: flplane3-line-isect-time (->* (FlPlane3 FlVector FlVector) (Any) (U #f Flonum)))
(define (flplane3-line-isect-time plane v1 v2 [segment? #t])
  (match-define (flplane3 norm d) plane)
  (define-values (a b c) (flv3-values norm))
  (define-values (x1 y1 z1) (flv3-values v1))
  (define-values (x2 y2 z2) (flv3-values v2))
  (plane-line-isect-time a b c d x1 y1 z1 x2 y2 z2 segment?))

;; ---------------------------------------------------------------------------------------------------
;; Plane-line intersection point: <= 0.5ulp error

#|
d1 = distance of v1 from plane
d2 = distance of v2 from plane

If d1 - d2 is zero, the line is parallel
If d1 and d2 have the same sign, the segment between is on one side of the plane

Formulas for plane-line intersect:
v = ((v2 - v1) *  d1) / (d1 - d2) + v1
v = ((v1 - v2) * -d2) / (d1 - d2) + v2

Using fl2 arithmetic,
 * v2 - v1 and v1 - v2 are exact
 * Error in d1 - d2 doesn't seem to matter if it's computed from dot products instead of distances
 * formula-lhs + v1 and formula-lhs + v2 have unbounded error, so we need to ensure it's not too high
|#

(: fast-plane-line-isect (-> Flonum Flonum Flonum Flonum
                             Flonum Flonum Flonum
                             Flonum Flonum Flonum
                             Any (Values (U #f FlVector) Boolean)))
(define (fast-plane-line-isect a b c d x1 y1 z1 x2 y2 z2 segment?)
  (define-values (dot1-hi dot1-lo) (fast-fl3dot/error a b c x1 y1 z1))
  (define-values (d1-hi d1-lo)     (fast-fl2+1 dot1-hi dot1-lo d))
  (define-values (dot2-hi dot2-lo) (fast-fl3dot/error a b c x2 y2 z2))
  (define-values (d2-hi d2-lo)     (fast-fl2+1 dot2-hi dot2-lo d))
  (cond
    [(not (and (fl2rational? d1-hi d1-lo)
               (fl2rational? d2-hi d2-lo)))
     ;; Failure
     (values #f #t)]
    [(and segment? (or (and (fl2negative? d1-hi d1-lo) (fl2negative? d2-hi d2-lo))
                       (and (fl2positive? d1-hi d1-lo) (fl2positive? d2-hi d2-lo))))
     ;; Both points are on the same side of the plane
     (values #f #f)]
    [else
     (define-values (denom-hi denom-lo) (fl2- dot1-hi dot1-lo dot2-hi dot2-lo))
     (cond
       [(fl2zero? denom-hi denom-lo)
        ;; Line is parallel to the plane
        (values #f #f)]
       [(not (fl2rational? denom-hi denom-lo))
        ;; Failure
        (values #f #t)]
       [else
        (define denom-positive? (fl2positive? denom-hi denom-lo))
        ;; Choose a formula that reduces error
        (let-values ([(numer-hi numer-lo x1 x2 y1 y2 z1 z2)
                      (if (if denom-positive?
                              ;; Cheap comparisons are OK - no need to switch *exactly* in the center
                              (fl> (+ d1-hi d1-lo) (+ d2-hi d1-lo))
                              (fl< (+ d1-hi d1-lo) (+ d2-hi d1-lo)))
                          (values d1-hi d1-lo x1 x2 y1 y2 z1 z2)
                          (values (- d2-hi) (- d2-lo) x2 x1 y2 y1 z2 z1))])
          ;; Compute the formula for a coordinate, and whether the last addition's error is too large
          (: isect (-> Flonum Flonum (Values Flonum Boolean)))
          (define (isect x1 x2)
            (let*-values ([(dx-hi dx-lo)  (fast-fl-/error x2 x1)]
                          [(x1-hi x1-lo)  (fast-fl2* dx-hi dx-lo numer-hi numer-lo)]
                          [(x2-hi x2-lo)  (fast-fl2/ x1-hi x1-lo denom-hi denom-lo)]
                          [(x3-hi x3-lo)  (fast-fl2+1 x2-hi x2-lo x1)])
              ;; Error threshold 2^25 found experimentally
              (values (+ x3-hi x3-lo)
                      (> (add-error x2-hi x1) (flexpt 2.0 25.0)))))
          
          (let-values ([(x x-bad?)  (isect x1 x2)]
                       [(y y-bad?)  (isect y1 y2)]
                       [(z z-bad?)  (isect z1 z2)])
            (if (or x-bad? y-bad? z-bad?)
                (values #f #t)
                (values (flvector x y z) #f))))])]))

(: slow-plane-line-isect (-> Flonum Flonum Flonum Flonum
                             Flonum Flonum Flonum
                             Flonum Flonum Flonum
                             Any (U #f FlVector)))
(define (slow-plane-line-isect a b c d x1 y1 z1 x2 y2 z2 segment?)
  (define plane (assert (eplane3 (ev3 (inexact->exact a) (inexact->exact b) (inexact->exact c))
                                 (inexact->exact d))
                        eplane3?))
  (define v1 (ev3 (inexact->exact x1) (inexact->exact y1) (inexact->exact z1)))
  (define v2 (ev3 (inexact->exact x2) (inexact->exact y2) (inexact->exact z2)))
  (match (eplane3-line-isect plane v1 v2 segment?)
    [(ev3 x y z)  (flvector (fl x) (fl y) (fl z))]
    [_  #f]))

(: flplane3-line-isect (->* (FlPlane3 FlVector FlVector) (Any) (U #f FlVector)))
(define (flplane3-line-isect plane v1 v2 [segment? #t])
  (match-define (flplane3 norm d) plane)
  (define-values (a b c) (flv3-values norm))
  (define-values (x1 y1 z1) (flv3-values v1))
  (define-values (x2 y2 z2) (flv3-values v2))
  (define-values (v fail?) (fast-plane-line-isect a b c d x1 y1 z1 x2 y2 z2 segment?))
  (match* (v fail?)
    [(_ #t)  (slow-plane-line-isect a b c d x1 y1 z1 x2 y2 z2 segment?)]
    [((? flvector? v) #f)
     (define-values (x y z) (flv3-values v))
     (if (and (< (flsqrt +max-subnormal.0) (abs x) +inf.0)
              (< (flsqrt +max-subnormal.0) (abs y) +inf.0)
              (< (flsqrt +max-subnormal.0) (abs z) +inf.0))
         v
         (slow-plane-line-isect a b c d x1 y1 z1 x2 y2 z2 segment?))]
    [(_ #f)  v]))

;; ===================================================================================================
;; Functions of polygon vertices

;; ---------------------------------------------------------------------------------------------------
;; Centroid (average vertex): <= 0.5ulp error

(: fast-flv3polygon-centroid/error (-> (Vectorof FlVector)
                                       (Values Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (fast-flv3polygon-centroid/error vs)
  (define-values (x1 y1 z1) (flv3-values (vector-ref vs 0)))
  (define-values (x-hi y-hi z-hi x-lo y-lo z-lo n)
    (for/fold ([x1-hi : Flonum  x1]
               [y1-hi : Flonum  y1]
               [z1-hi : Flonum  z1]
               [x1-lo : Flonum  0.0]
               [y1-lo : Flonum  0.0]
               [z1-lo : Flonum  0.0]
               [n  : Flonum  1.0]
               ) ([i  (in-range 1 (vector-length vs))])
      (define-values (x2 y2 z2) (flv3-values (unsafe-vector-ref vs i)))
      (let-values ([(x1-hi x1-lo)  (fast-fl2+1 x1-hi x1-lo x2)]
                   [(y1-hi y1-lo)  (fast-fl2+1 y1-hi y1-lo y2)]
                   [(z1-hi z1-lo)  (fast-fl2+1 z1-hi z1-lo z2)])
        (values x1-hi y1-hi z1-hi x1-lo y1-lo z1-lo (+ n 1.0)))))
  (let-values ([(x-hi x-lo)  (fast-fl2/1 x-hi x-lo n)]
               [(y-hi y-lo)  (fast-fl2/1 y-hi y-lo n)]
               [(z-hi z-lo)  (fast-fl2/1 z-hi z-lo n)])
    (values x-hi y-hi z-hi x-lo y-lo z-lo)))

(: fast-flv3polygon-centroid (-> (Vectorof FlVector) FlVector))
(define (fast-flv3polygon-centroid vs)
  (define-values (x-hi y-hi z-hi x-lo y-lo z-lo) (fast-flv3polygon-centroid/error vs))
  (flvector (+ x-hi x-lo) (+ y-hi y-lo) (+ z-hi z-lo)))

(: slow-flv3polygon-centroid/error (-> (Vectorof FlVector)
                                       (Values Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (slow-flv3polygon-centroid/error vs)
  (define-values (xmin ymin zmin xmax ymax zmax) (flv3aabb-values vs))
  (define nx (flpow2near (max (abs xmin) (abs xmax))))
  (define ny (flpow2near (max (abs ymin) (abs ymax))))
  (define nz (flpow2near (max (abs zmin) (abs zmax))))
  (let-values ([(x-hi y-hi z-hi x-lo y-lo z-lo) (fast-flv3polygon-centroid/error
                                                 (vector-map (λ ([v : FlVector])
                                                               (define-values (x y z) (flv3-values v))
                                                               (flvector (/ x nx) (/ y ny) (/ z nz)))
                                                             vs))])
    (values (* x-hi nx) (* y-hi ny) (* z-hi nz) (* x-lo nx) (* y-lo ny) (* z-lo nz))))

(: slow-flv3polygon-centroid (-> (Vectorof FlVector) FlVector))
(define (slow-flv3polygon-centroid vs)
  (define-values (x-hi y-hi z-hi x-lo y-lo z-lo) (slow-flv3polygon-centroid/error vs))
  (flvector (+ x-hi x-lo) (+ y-hi y-lo) (+ z-hi z-lo)))

(: flv3polygon-centroid (-> (Vectorof FlVector) FlVector))
(define (flv3polygon-centroid vs)
  (cond [(empty? vs)  (flvector 0.0 0.0 0.0)]
        [else
         (define v (fast-flv3polygon-centroid vs))
         (define-values (x y z) (flv3-values v))
         (cond [(and (flrational? x) (flrational? y) (flrational? z))  v]
               [else  (slow-flv3polygon-centroid vs)])]))

(: flv3polygon-centroid/error (-> (Vectorof FlVector)
                                  (Values Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (flv3polygon-centroid/error vs)
  (cond [(empty? vs)  (values 0.0 0.0 0.0 0.0 0.0 0.0)]
        [else
         (define-values (x-hi y-hi z-hi x-lo y-lo z-lo) (fast-flv3polygon-centroid/error vs))
         (if (and (< +max-subnormal.0 (abs (+ x-hi x-lo)) +inf.0)
                  (< +max-subnormal.0 (abs (+ y-hi y-lo)) +inf.0)
                  (< +max-subnormal.0 (abs (+ z-hi z-lo)) +inf.0))
             (values x-hi y-hi z-hi x-lo y-lo z-lo)
             (slow-flv3polygon-centroid/error vs))]))

;; ---------------------------------------------------------------------------------------------------
;; Best-fit polygon perpendicular: <= 0.5ulp error

;; Compute a vector perpendicular to the given polygon points using Newell's method
;; If the polygon is simple, the length of the vector is twice the polygon's area

(: fast-flv3polygon-perp/error (-> (Vectorof FlVector)
                                   (Values Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (fast-flv3polygon-perp/error vs)
  (define n (vector-length vs))
  (define-values (x1 y1 z1) (flv3-values (vector-ref vs (- n 1))))
  (define-values (x-hi y-hi z-hi x-lo y-lo z-lo _x1 _y1 _z1)
    (for/fold ([x-hi : Flonum  0.0]
               [y-hi : Flonum  0.0]
               [z-hi : Flonum  0.0]
               [x-lo : Flonum  0.0]
               [y-lo : Flonum  0.0]
               [z-lo : Flonum  0.0]
               [x1 : Flonum  x1]
               [y1 : Flonum  y1]
               [z1 : Flonum  z1]
               ) ([i  (in-range n)])
      (define-values (x2 y2 z2) (flv3-values (unsafe-vector-ref vs i)))
      (define-values (x3-hi y3-hi z3-hi x3-lo y3-lo z3-lo) (fast-fl3cross/error x1 y1 z1 x2 y2 z2))
      (let-values ([(x-hi x-lo)  (fast-fl2+ x-hi x-lo x3-hi x3-lo)]
                   [(y-hi y-lo)  (fast-fl2+ y-hi y-lo y3-hi y3-lo)]
                   [(z-hi z-lo)  (fast-fl2+ z-hi z-lo z3-hi z3-lo)])
        (values x-hi y-hi z-hi x-lo y-lo z-lo x2 y2 z2))))
  (values x-hi y-hi z-hi x-lo y-lo z-lo))

(: fast-flv3polygon-perp (-> (Vectorof FlVector) (Values Flonum Flonum Flonum)))
(define (fast-flv3polygon-perp vs)
  (define-values (x-hi y-hi z-hi x-lo y-lo z-lo) (fast-flv3polygon-perp/error vs))
  (values (+ x-hi x-lo) (+ y-hi y-lo) (+ z-hi z-lo)))

(: cross-axes (-> (U 0 1 2) (Values (U 0 1 2) (U 0 1 2))))
(define (cross-axes i)
  (case i
    [(0)   (values 1 2)]
    [(1)   (values 2 0)]
    [else  (values 0 1)]))

(: fast-flv3polygon-perp-coord/error (-> (Vectorof FlVector) (U 0 1 2)
                                         (Values Flonum Flonum)))
(define (fast-flv3polygon-perp-coord/error vs i)
  (define-values (j k) (cross-axes i))
  (define n (vector-length vs))
  (define v1 (vector-ref vs (- n 1)))
  (define x1 (flvector-ref v1 j))
  (define y1 (flvector-ref v1 k))
  (define-values (z-hi z-lo _x1 _y1)
    (for/fold ([z-hi : Flonum  0.0]
               [z-lo : Flonum  0.0]
               [x1 : Flonum  x1]
               [y1 : Flonum  y1]
               ) ([l  (in-range n)])
      (define v2 (unsafe-vector-ref vs l))
      (define x2 (flvector-ref v2 j))
      (define y2 (flvector-ref v2 k))
      (let*-values ([(w-hi w-lo)  (fl2cross/error x1 y1 x2 y2)]
                    [(z-hi z-lo)  (fast-fl2+ z-hi z-lo w-hi w-lo)])
        (values z-hi z-lo x2 y2))))
  (values z-hi z-lo))

(: fast-flv3polygon-perp-coord (-> (Vectorof FlVector) (U 0 1 2) Flonum))
(define (fast-flv3polygon-perp-coord vs i)
  (define-values (z-hi z-lo) (fast-flv3polygon-perp-coord/error vs i))
  (+ z-hi z-lo))

(: newell-perp-coord-reduction (-> (Vectorof FlVector) (U 0 1 2) Flonum))
;; Finds the smallest power of 2 we can divide each coordinate j and k by without overflowing before
;; and during a Newell method perpendicular computation for coordinate i
(define (newell-perp-coord-reduction vs i)
  (define-values (j k) (cross-axes i))
  (define n (vector-length vs))
  (define v1 (vector-ref vs (- n 1)))
  (define x1 (abs (flvector-ref v1 j)))
  (define y1 (abs (flvector-ref v1 k)))
  (define-values (xmax ymax zmax _x1 _y1)
    (for/fold ([xmax : Flonum  x1]
               [ymax : Flonum  y1]
               [zmax : Flonum  0.0]
               [x1 : Flonum  x1]
               [y1 : Flonum  y1]
               ) ([l  (in-range n)])
      (define v2 (unsafe-vector-ref vs l))
      (define x2 (abs (flvector-ref v2 j)))
      (define y2 (abs (flvector-ref v2 k)))
      (values (max xmax x2)
              (max ymax y2)
              (max zmax (* (flsqrt x1) (flsqrt y2)) (* (flsqrt y1) (flsqrt x2)))
              x2 y2)))
  (flpow2near (max
               ;; The smallest number we can divide coordinates j and k by without overflowing
               (/ (max xmax ymax) 1e298)
               ;; The smallest number we can divide coordinate i by without overflowing any product
               (/ zmax (flsqrt 1e298)))))

(: slow-flv3polygon-perp-coord/error (-> (Vectorof FlVector) (U 0 1 2)
                                         (Values Flonum Flonum)))
(define (slow-flv3polygon-perp-coord/error vs i)
  (define n (newell-perp-coord-reduction vs i))
  (define-values (z-hi z-lo)
    (fast-flv3polygon-perp-coord/error (vector-map (λ ([v : FlVector]) (flv3/ v n)) vs) i))
  (values (fl** n n z-hi) (fl** n n z-lo)))

(: slow-flv3polygon-perp-coord (-> (Vectorof FlVector) (U 0 1 2) Flonum))
(define (slow-flv3polygon-perp-coord vs i)
  (define n (newell-perp-coord-reduction vs i))
  (fl** n n (fast-flv3polygon-perp-coord (vector-map (λ ([v : FlVector]) (flv3/ v n)) vs) i)))

(: flv3polygon-perp/error (-> (Vectorof FlVector)
                              (Values Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (flv3polygon-perp/error vs)
  (define n (vector-length vs))
  (cond [(< n 3)
         (values 0.0 0.0 0.0 0.0 0.0 0.0)]
        [else
         (define-values (x-hi y-hi z-hi x-lo y-lo z-lo) (fast-flv3polygon-perp/error vs))
         (let-values ([(x-hi x-lo)  (if (< +max-subnormal.hi (abs (+ x-hi x-lo)) +inf.0)
                                        (values x-hi x-lo)
                                        (slow-flv3polygon-perp-coord/error vs 0))]
                      [(y-hi y-lo)  (if (< +max-subnormal.hi (abs (+ y-hi y-lo)) +inf.0)
                                        (values y-hi y-lo)
                                        (slow-flv3polygon-perp-coord/error vs 1))]
                      [(z-hi z-lo)  (if (< +max-subnormal.hi (abs (+ z-hi z-lo)) +inf.0)
                                        (values z-hi z-lo)
                                        (slow-flv3polygon-perp-coord/error vs 2))])
           (values x-hi y-hi z-hi x-lo y-lo z-lo))]))

(: flv3polygon-perp (-> (Vectorof FlVector) FlVector))
(define (flv3polygon-perp vs)
  (define n (vector-length vs))
  (cond [(< n 3)
         (flvector 0.0 0.0 0.0)]
        [else
         (define-values (x y z) (fast-flv3polygon-perp vs))
         (let ([x  (cond [(< +max-subnormal.0 (abs x) +inf.0)  x]
                         [else  (slow-flv3polygon-perp-coord vs 0)])]
               [y  (cond [(< +max-subnormal.0 (abs y) +inf.0)  y]
                         [else  (slow-flv3polygon-perp-coord vs 1)])]
               [z  (cond [(< +max-subnormal.0 (abs z) +inf.0)  z]
                         [else  (slow-flv3polygon-perp-coord vs 2)])])
           (flvector x y z))]))

;; ---------------------------------------------------------------------------------------------------
;; Best-fit polygon normal: <= 0.5ulp error, except sometimes when output is subnormal

;; Compute a *normal* vector perpendicular to the given polygon points using Newell's method

(: newell-normal-reduction (-> (Vectorof FlVector) Flonum))
(define (newell-normal-reduction vs)
  (define n (vector-length vs))
  ;; Find the bounding box
  (define-values (xbmin ybmin zbmin xbmax ybmax zbmax) (flv3aabb-values vs))
  ;; Find the square root of the largest product computed while computing the normal
  (define-values (x1 y1 z1) (let-values ([(x y z)  (flv3-values (vector-ref vs (- n 1)))])
                              (values (flsqrt (flabs x))
                                      (flsqrt (flabs y))
                                      (flsqrt (flabs z)))))
  (define-values (xmax ymax zmax _x1 _y1 _z1)
    (for/fold ([xmax : Flonum  0.0]
               [ymax : Flonum  0.0]
               [zmax : Flonum  0.0]
               [x1 : Flonum  x1]
               [y1 : Flonum  y1]
               [z1 : Flonum  z1]
               ) ([i  (in-range n)])
      (define v2 (unsafe-vector-ref vs i))
      (define-values (x2 y2 z2) (let-values ([(x y z)  (flv3-values v2)])
                                  (values (flsqrt (flabs x))
                                          (flsqrt (flabs y))
                                          (flsqrt (flabs z)))))
      (values (max xmax (* y1 z2) (* z1 y2))
              (max ymax (* z1 x2) (* x1 z2))
              (max zmax (* x1 y2) (* y1 x2))
              x2 y2 z2)))
  (flpow2near (max
               ;; The smallest number we can divide each coordinate by without overflowing
               (/ (max (abs xbmin) (abs xbmax) (abs ybmin) (abs ybmax) (abs zbmin) (abs zbmax)) 1e298)
               ;; The smallest number we can divide each coordinate by without overflowing any product
               (/ (max xmax ymax zmax) (flsqrt 1e298)))))

(: fast-fl23normalize (-> Flonum Flonum Flonum Flonum Flonum Flonum
                          (Values Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (fast-fl23normalize x-hi y-hi z-hi x-lo y-lo z-lo)
  (let*-values ([(x^2-hi x^2-lo)  (fast-fl2sqr x-hi x-lo)]
                [(y^2-hi y^2-lo)  (fast-fl2sqr y-hi y-lo)]
                [(z^2-hi z^2-lo)  (fast-fl2sqr z-hi z-lo)]
                [(d-hi d-lo)  (fast-fl2+ x^2-hi x^2-lo y^2-hi y^2-lo)]
                [(d-hi d-lo)  (fast-fl2+ d-hi d-lo z^2-hi z^2-lo)]
                [(d-hi d-lo)  (fast-fl2sqrt d-hi d-lo)]
                [(x-hi x-lo)  (fast-fl2/ x-hi x-lo d-hi d-lo)]
                [(y-hi y-lo)  (fast-fl2/ y-hi y-lo d-hi d-lo)]
                [(z-hi z-lo)  (fast-fl2/ z-hi z-lo d-hi d-lo)])
    (values x-hi y-hi z-hi x-lo y-lo z-lo)))

(: slow-fl23normalize (-> Flonum Flonum Flonum Flonum Flonum Flonum
                          (Values Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (slow-fl23normalize x-hi y-hi z-hi x-lo y-lo z-lo)
  (define n (flpow2near (/ (max (abs (+ x-hi x-lo))
                                (abs (+ y-hi y-lo))
                                (abs (+ z-hi z-lo)))
                           (flsqrt 1e299))))
  (fast-fl23normalize (/ x-hi n) (/ y-hi n) (/ z-hi n)
                      (/ x-lo n) (/ y-lo n) (/ z-lo n)))

(: fl23normalize (-> Flonum Flonum Flonum Flonum Flonum Flonum
                     (Values Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (fl23normalize x-hi y-hi z-hi x-lo y-lo z-lo)
  (define-values (nx-hi ny-hi nz-hi nx-lo ny-lo nz-lo)
    (fast-fl23normalize x-hi y-hi z-hi x-lo y-lo z-lo))
  (if (and (< (flsqrt +max-subnormal.hi) (abs (+ nx-hi nx-lo)) +inf.0)
           (< (flsqrt +max-subnormal.hi) (abs (+ ny-hi ny-lo)) +inf.0)
           (< (flsqrt +max-subnormal.hi) (abs (+ nz-hi nz-lo)) +inf.0))
      (values nx-hi ny-hi nz-hi nx-lo ny-lo nz-lo)
      (slow-fl23normalize x-hi y-hi z-hi x-lo y-lo z-lo)))

(: fast-flv3polygon-normal/error (-> (Vectorof FlVector)
                                     (Values Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (fast-flv3polygon-normal/error vs)
  (let*-values ([(x-hi y-hi z-hi x-lo y-lo z-lo)  (fast-flv3polygon-perp/error vs)]
                [(x-hi y-hi z-hi x-lo y-lo z-lo)  (fl23normalize x-hi y-hi z-hi x-lo y-lo z-lo)])
    (values x-hi y-hi z-hi x-lo y-lo z-lo)))

(: slow-flv3polygon-normal/error (-> (Vectorof FlVector)
                                     (Values Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (slow-flv3polygon-normal/error vs)
  (define n (newell-normal-reduction vs))
  (fast-flv3polygon-normal/error (vector-map (λ ([v : FlVector]) (flv3/ v n)) vs)))

(: flv3polygon-normal/error (-> (Vectorof FlVector)
                                (Values Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (flv3polygon-normal/error vs)
  (define-values (x-hi y-hi z-hi x-lo y-lo z-lo) (fast-flv3polygon-normal/error vs))
  (if (and (< (flsqrt +max-subnormal.hi) (abs (+ x-hi x-lo)) +inf.0)
           (< (flsqrt +max-subnormal.hi) (abs (+ y-hi y-lo)) +inf.0)
           (< (flsqrt +max-subnormal.hi) (abs (+ z-hi z-lo)) +inf.0))
      (values x-hi y-hi z-hi x-lo y-lo z-lo)
      (slow-flv3polygon-normal/error vs)))

(: flv3polygon-normal (-> (Vectorof FlVector) (U #f FlVector)))
(define (flv3polygon-normal vs)
  (define n (vector-length vs))
  (cond [(< n 3)  #f]
        [else
         (define-values (x-hi y-hi z-hi x-lo y-lo z-lo) (fast-flv3polygon-normal/error vs))
         (define x (+ x-hi x-lo))
         (define y (+ y-hi y-lo))
         (define z (+ z-hi z-lo))
         (if (and (< (flsqrt +max-subnormal.0) (abs x) +inf.0)
                  (< (flsqrt +max-subnormal.0) (abs y) +inf.0)
                  (< (flsqrt +max-subnormal.0) (abs z) +inf.0))
             (flvector x y z)
             (let-values ([(x-hi y-hi z-hi x-lo y-lo z-lo)  (slow-flv3polygon-normal/error vs)])
               (define x (+ x-hi x-lo))
               (define y (+ y-hi y-lo))
               (define z (+ z-hi z-lo))
               (if (and (flrational? x) (flrational? y) (flrational? z))
                   (flvector x y z)
                   #f)))]))

;; ---------------------------------------------------------------------------------------------------
;; Best-fit plane: can't be as accurate as the above: requires normal and centroid known with
;; arbitrary precision in the worst case (i.e. requires bigfloat sqrt)

(: flv3polygon-plane (-> (Vectorof FlVector) (U #f FlPlane3)))
(define (flv3polygon-plane vs)
  (define norm (flv3polygon-normal vs))
  (and norm (flplane3 norm (- (flv3dot norm (flv3polygon-centroid vs))))))

;; ---------------------------------------------------------------------------------------------------
;; Perimeter: usually <= 2.0 ulps error (but error is a function of number of vertices)

(: flv3polygon-perimeter (-> (Vectorof FlVector) Nonnegative-Flonum))
(define (flv3polygon-perimeter vs)
  (define n (vector-length vs))
  (cond
    [(= n 0)  0.0]
    [else
     (define-values (x1 y1 z1) (flv3-values (unsafe-vector-ref vs (- n 1))))
     (define-values (sum _x1 _y1 _z1)
       (for/fold ([sum : Nonnegative-Flonum  0.0]
                  [x1 : Flonum  x1]
                  [y1 : Flonum  y1]
                  [z1 : Flonum  z1]
                  ) ([i  (in-range n)])
         (define-values (x2 y2 z2) (flv3-values (unsafe-vector-ref vs i)))
         (values (+ sum (fl3dist x1 y1 z1 x2 y2 z2)) x2 y2 z2)))
     sum]))

;; ---------------------------------------------------------------------------------------------------
;; Area of simple polygon: <= 1.25ulp error

(: fast-flv3polygon-area (-> (Vectorof FlVector) Flonum))
(define (fast-flv3polygon-area vs)
  (define-values (x y z) (fast-flv3polygon-perp vs))
  (fl3mag (* 0.5 x) (* 0.5 y) (* 0.5 z)))

(: slow-flv3polygon-area (-> (Vectorof FlVector) Flonum))
(define (slow-flv3polygon-area vs)
  (define s (newell-normal-reduction vs))
  (fl** s s (fast-flv3polygon-area (vector-map (λ ([v : FlVector]) (flv3/ v s)) vs))))

(: flv3polygon-area (-> (Vectorof FlVector) Nonnegative-Flonum))
(define (flv3polygon-area vs)
  (define n (vector-length vs))
  (cond [(< n 3)  0.0]
        [else  (let* ([a  (fast-flv3polygon-area vs)]
                      [a  (if (< (flsqrt +max-subnormal.0) (abs a) +inf.0)
                              a
                              (slow-flv3polygon-area vs))])
                 (max 0.0 a))]))

;; ---------------------------------------------------------------------------------------------------
;; Regularity measure

(: flregular-polygon-area (-> Nonnegative-Flonum Index Nonnegative-Flonum))
;; Returns the area of the regular polygon with the given perimeter and number of sides
(define (flregular-polygon-area p n)
  (cond [(< n 3)  0.0]
        [else
         (let ([n  (fl n)])
           (define denom (if (n . > . 1e9) pi (* n (fltanpix (/ 1.0 n)))))
           (max 0.0 (* (* (/ 0.25 denom) p) p)))]))

(: fast-flv3polygon-regularity (-> (Vectorof FlVector) Index (Values Flonum Flonum)))
(define (fast-flv3polygon-regularity vs n)
  (define p (flv3polygon-perimeter vs))
  (values (fast-flv3polygon-area vs)
          (flregular-polygon-area p n)))

(: slow-flv3polygon-regularity (-> (Vectorof FlVector) Index (Values Flonum Flonum)))
(define (slow-flv3polygon-regularity vs n)
  (define-values (xmin ymin zmin xmax ymax zmax) (flv3aabb-values vs))
  (define s (flpow2near (/ (max (abs xmin) (abs xmax) (abs ymin) (abs ymax) (abs zmin) (abs zmax))
                           (flsqrt (/ +max.0 256.0)))))
  (fast-flv3polygon-regularity (vector-map (λ ([v : FlVector]) (flv3/ v s)) vs) n))

(: flv3polygon-regularity (-> (Vectorof FlVector) (U #f Nonnegative-Flonum)))
;; Returns a number in [0,1] that indicates how close to regular the polygon is
;; Any polygon with fewer than 3 non-collinear points has regularity 0
(define (flv3polygon-regularity vs)
  (define n (vector-length vs))
  (cond [(< n 3)  #f]
        [else
         (let*-values ([(numer denom)  (fast-flv3polygon-regularity vs n)]
                       [(numer denom)  (if (and (< (flsqrt +max-subnormal.0) (abs numer) +inf.0)
                                                (< (flsqrt +max-subnormal.0) (abs denom) +inf.0))
                                           (values numer denom)
                                           (slow-flv3polygon-regularity vs n))])
           (and (not (zero? denom))
                (max 0.0 (min 1.0 (/ numer denom)))))]))

;; ===================================================================================================
;; Functions of triangle vertices

(: flv3triangle-contains-point? (-> FlVector FlVector FlVector FlVector Boolean))
#;
(define (flv3triangle-contains-point? v1 v2 v3 v)
  (let ([a  (flv3- v1 v)]
        [b  (flv3- v2 v)]
        [c  (flv3- v3 v)])
    (define ab (flv3dot a b))
    (define ac (flv3dot a c))
    (define bc (flv3dot b c))
    (define cc (flv3mag^2 c))
    (cond [(< (- (* bc ac) (* cc ab)) 0.0)  #f]
          [else
           (define bb (flv3mag^2 b))
           (>= (- (* ab bc) (* ac bb)) 0.0)])))

;; Alternative:
(define (flv3triangle-contains-point? v1 v2 v3 v)
  (let ([a  (flv3- v1 v)]
        [b  (flv3- v2 v)]
        [c  (flv3- v3 v)])
    (define u (flv3cross b c))
    (define v (flv3cross c a))
    (cond [(< (flv3dot u v) 0.0)  #f]
          [else
           (define w (flv3cross a b))
           (>= (flv3dot u w) 0.0)])))
