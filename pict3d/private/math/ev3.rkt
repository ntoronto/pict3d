#lang typed/racket/base

(require (for-syntax racket/base)
         racket/match
         racket/list
         racket/vector
         (only-in racket/math sqr)
         "../utils.rkt")

(provide e2dot
         
         (struct-out ev3)
         ev3dot
         ev3cross
         ev3+
         ev3-
         ev3neg
         ev3*
         ev3/
         ev3ma
         ev3mag^2
         ev3dist^2
         ev3bbox
         
         EPlane3 eplane3? eplane3-normal eplane3-distance
         (rename-out [eplane3* eplane3])
         
         eplane3-point-dist
         eplane3-line-isect-time
         eplane3-line-isect
         eplane3-relative-dists
         
         ev3polygon-centroid
         ev3polygon-perp
         ev3polygon-plane
         
         ev3triangle-contains-nearest?
         )

;; ===================================================================================================
;; 3-component exact rational vectors

(struct: ev3 ([x : Exact-Rational] [y : Exact-Rational] [z : Exact-Rational])
  #:transparent)

;; ---------------------------------------------------------------------------------------------------
;; Dot product

(: e3dot (-> Exact-Rational Exact-Rational Exact-Rational Exact-Rational Exact-Rational Exact-Rational
             Exact-Rational))
(define (e3dot x1 y1 z1 x2 y2 z2)
  (+ (+ (* x1 x2) (* y1 y2)) (* z1 z2)))

(: ev3dot (-> ev3 ev3 Exact-Rational))
(define (ev3dot v1 v2)
  (match-define (ev3 x1 y1 z1) v1)
  (match-define (ev3 x2 y2 z2) v2)
  (e3dot x1 y1 z1 x2 y2 z2))

;; ---------------------------------------------------------------------------------------------------
;; Cross product

(: e2dot (-> Exact-Rational Exact-Rational Exact-Rational Exact-Rational Exact-Rational))
(define (e2dot x1 y1 x2 y2)
  (+ (* x1 x2) (* y1 y2)))

(: e2cross (-> Exact-Rational Exact-Rational Exact-Rational Exact-Rational Exact-Rational))
(define (e2cross x1 y1 x2 y2)
  (e2dot x1 y1 y2 (- x2)))

(: e3cross (-> Exact-Rational Exact-Rational Exact-Rational 
               Exact-Rational Exact-Rational Exact-Rational 
               (Values Exact-Rational Exact-Rational Exact-Rational)))
(define (e3cross x1 y1 z1 x2 y2 z2)
  (values (e2cross y1 z1 y2 z2)
          (e2cross z1 x1 z2 x2)
          (e2cross x1 y1 x2 y2)))

(: ev3cross (-> ev3 ev3 ev3))
(define (ev3cross v1 v2)
  (match-define (ev3 x1 y1 z1) v1)
  (match-define (ev3 x2 y2 z2) v2)
  (let-values ([(x y z)  (e3cross x1 y1 z1 x2 y2 z2)])
    (ev3 x y z)))

;; ---------------------------------------------------------------------------------------------------
;; Vector arithmetic

(: ev3+ (-> ev3 ev3 ev3))
(define (ev3+ v1 v2)
  (match-define (ev3 x1 y1 z1) v1)
  (match-define (ev3 x2 y2 z2) v2)
  (ev3 (+ x1 x2) (+ y1 y2) (+ z1 z2)))

(: ev3- (-> ev3 ev3 ev3))
(define (ev3- v1 v2)
  (match-define (ev3 x1 y1 z1) v1)
  (match-define (ev3 x2 y2 z2) v2)
  (ev3 (- x1 x2) (- y1 y2) (- z1 z2)))

(: ev3neg (-> ev3 ev3))
(define (ev3neg v)
  (match-define (ev3 x y z) v)
  (ev3 (- x) (- y) (- z)))

(: ev3* (-> ev3 Exact-Rational ev3))
(define (ev3* v a)
  (match-define (ev3 x y z) v)
  (ev3 (* x a) (* y a) (* z a)))

(: ev3/ (-> ev3 Exact-Rational ev3))
(define (ev3/ v a)
  (match-define (ev3 x y z) v)
  (ev3 (/ x a) (/ y a) (/ z a)))

(: ev3ma (-> ev3 Exact-Rational ev3 ev3))
(define (ev3ma dv a v0)
  (match-define (ev3 dx dy dz) dv)
  (match-define (ev3 x0 y0 z0) v0)
  (ev3 (+ (* dx a) x0) (+ (* dy a) y0) (+ (* dz a) z0)))

;; ---------------------------------------------------------------------------------------------------
;; Magnitude (squared)

(: e3mag^2 (-> Exact-Rational Exact-Rational Exact-Rational Nonnegative-Exact-Rational))
(define (e3mag^2 x y z)
  (+ (sqr x) (sqr y) (sqr z)))

(: ev3mag^2 (-> ev3 Nonnegative-Exact-Rational))
(define (ev3mag^2 v)
  (match-define (ev3 x y z) v)
  (e3mag^2 x y z))

(: ev3zero? (-> ev3 Boolean))
(define (ev3zero? v)
  (match-define (ev3 x y z) v)
  (and (zero? x) (zero? y) (zero? z)))

(: e3dist^2 (-> Exact-Rational Exact-Rational Exact-Rational
                Exact-Rational Exact-Rational Exact-Rational
                Nonnegative-Exact-Rational))
(define (e3dist^2 x1 y1 z1 x2 y2 z2)
  (e3mag^2 (- x1 x2) (- y1 y2) (- z1 z2)))

(: ev3dist^2 (-> ev3 ev3 Nonnegative-Exact-Rational))
;; Equivalent to (flv3mag^2 (flv3- v1 v2))
(define (ev3dist^2 v1 v2)
  (match-define (ev3 x1 y1 z1) v1)
  (match-define (ev3 x2 y2 z2) v2)
  (e3dist^2 x1 y1 z1 x2 y2 z2))

(: ev3bbox (-> (Vectorof ev3) (Values Exact-Rational Exact-Rational
                                      Exact-Rational Exact-Rational
                                      Exact-Rational Exact-Rational)))
(define (ev3bbox vs)
  (define n (vector-length vs))
  (cond [(= n 0)  (raise-type-error 'ev3bbox "nonempty list" vs)]
        [else
         (match-define (ev3 x y z) (vector-ref vs 0))
         (for/fold ([xmin : Exact-Rational  x]
                    [xmax : Exact-Rational  x]
                    [ymin : Exact-Rational  y]
                    [ymax : Exact-Rational  y]
                    [zmin : Exact-Rational  z]
                    [zmax : Exact-Rational  z]
                    ) ([i  (in-range 1 n)])
           (match-define (ev3 x y z) (vector-ref vs i))
           (values (min xmin x) (max xmax x)
                   (min ymin y) (max ymax y)
                   (min zmin z) (max zmax z)))]))

;; ===================================================================================================
;; Planes

(struct eplane3 ([normal : ev3] [distance : Exact-Rational])
  #:transparent)

(define-type EPlane3 eplane3)

(: make-eplane3 (-> ev3 Exact-Rational (U #f eplane3)))
(define (make-eplane3 norm d)
  (and (not (ev3zero? norm))
       (eplane3 norm d)))

(define-match-expander eplane3*
  (λ (stx)
    (syntax-case stx ()
      [(_ e1 e2)  (syntax/loc stx (eplane3 e1 e2))]))
  (λ (stx)
    (syntax-case stx ()
      [(_ . args)  (syntax/loc stx (make-eplane3 . args))]
      [_  (syntax/loc stx eplane3*)])))

;; ===================================================================================================

(: eplane3-point-dist (-> eplane3 ev3 Exact-Rational))
(define (eplane3-point-dist plane v)
  (match-define (eplane3 norm dist) plane)
  (+ (ev3dot norm v) dist))

(: eplane3-line-isect-time (->* [eplane3 ev3 ev3] [Any] (U #f Exact-Rational)))
(define (eplane3-line-isect-time plane v1 v2 [segment? #t])
  (match-define (eplane3 norm dist) plane)
  (define d1 (ev3dot norm v1))
  (define d2 (ev3dot norm v2))
  (define denom (- d1 d2))
  (cond [(zero? denom)  #f]
        [else  (define t (/ (+ d1 dist) denom))
               (if segment?
                   (and (<= 0 t 1) t)
                   t)]))

(: eplane3-line-isect (->* [eplane3 ev3 ev3] [Any] (U #f ev3)))
(define (eplane3-line-isect plane v1 v2 [segment? #t])
  (define t (eplane3-line-isect-time plane v1 v2 segment?))
  (and t (ev3+ (ev3* v2 t) (ev3* v1 (- 1 t)))))

(: eplane3-relative-dists (-> eplane3 (Vectorof ev3) (U #f (Vectorof Exact-Rational))))
;; Returns the relative signed distances from points to a plane
(define (eplane3-relative-dists plane vs)
  (define n (vector-length vs))
  (cond
    [(= n 0)  #()]
    [else
     (define-values (xmin xmax ymin ymax zmin zmax) (ev3bbox vs))
     (define m (max (abs xmin) (abs xmax) (abs ymin) (abs ymax) (abs zmin) (abs zmax)))
     (cond [(= m 0)  (and (zero? (eplane3-point-dist plane (vector-ref vs 0)))
                          (make-vector n 0))]
           [else  (vector-map (λ ([v : ev3]) (/ (eplane3-point-dist plane v) m)) vs)])]))

(: eplane3-closest-point (-> eplane3 ev3 ev3))
(define (eplane3-closest-point plane v)
  (define norm (eplane3-normal plane))
  (define dist (/ (eplane3-point-dist plane v) (ev3mag^2 norm)))
  (ev3- v (ev3* norm dist)))

;; ===================================================================================================
;; Functions of polygon vertices

(: ev3polygon-centroid (-> (Vectorof ev3) ev3))
(define (ev3polygon-centroid vs)
  (define n (vector-length vs))
  (cond [(= n 0)  (ev3 0 0 0)]
        [else
         (match-define (ev3 x1 y1 z1) (vector-ref vs 0))
         (define-values (x y z)
           (for/fold ([x1 : Exact-Rational  x1]
                      [y1 : Exact-Rational  y1]
                      [z1 : Exact-Rational  z1]
                      ) ([i  (in-range 1 n)])
             (match-define (ev3 x2 y2 z2) (vector-ref vs i))
             (values (+ x1 x2) (+ y1 y2) (+ z1 z2))))
         (ev3 (/ x n) (/ y n) (/ z n))]))

(: ev3polygon-perp (-> (Vectorof ev3) ev3))
;; Returns a "best fit" normal for a polygon with the given points using Newell's method
;; If the polygon is simple, the length of the normal is twice the polygon's area
(define (ev3polygon-perp vs)
  (define n (vector-length vs))
  (cond [(< n 3)  (ev3 0 0 0)]
        [else
         (match-define (ev3 x1 y1 z1) (vector-ref vs (- n 1)))
         (define-values (x y z _x1 _y1 _z1)
           (for/fold ([x : Exact-Rational  0]
                      [y : Exact-Rational  0]
                      [z : Exact-Rational  0]
                      [x1 : Exact-Rational  x1]
                      [y1 : Exact-Rational  y1]
                      [z1 : Exact-Rational  z1]
                      ) ([i  (in-range n)])
             (match-define (ev3 x2 y2 z2) (vector-ref vs i))
             (values (+ x (* (- y1 y2) (+ z1 z2)))
                     (+ y (* (- z1 z2) (+ x1 x2)))
                     (+ z (* (- x1 x2) (+ y1 y2)))
                     x2 y2 z2)))
         (ev3 x y z)]))

(: ev3polygon-plane (-> (Vectorof ev3) (U #f eplane3)))
;; Returns a "best fit" plane for a polygon with the given points using Newell's method
(define (ev3polygon-plane vs)
  (define n (vector-length vs))
  (cond [(< n 3)  #f]
        [else
         (define norm (ev3polygon-perp vs))
         (make-eplane3 norm (- (ev3dot norm (ev3polygon-centroid vs))))]))

;; ===================================================================================================
;; Functions of triangle vertices

(: ev3triangle-contains-nearest? (-> ev3 ev3 ev3 ev3 Boolean))
(define (ev3triangle-contains-nearest? v1 v2 v3 v)
  (define v12 (ev3- v2 v1))
  (define v23 (ev3- v3 v2))
  (define v31 (ev3- v1 v3))
  (define n (ev3cross v12 v23))
  (define n12 (ev3cross n v12))
  (define n23 (ev3cross n v23))
  (define n31 (ev3cross n v31))
  (define d12 (ev3dot n12 (ev3- v v1)))
  (define d23 (ev3dot n23 (ev3- v v2)))
  (define d31 (ev3dot n31 (ev3- v v3)))
  ;; True iff all are nonnegative or all are nonpositive
  (>= (* (max d12 d23 d31)
         (min d12 d23 d31))
      0))

(: ev3triangle-line-isect (-> ev3 ev3 ev3 ev3 ev3 (U #f ev3)))
(define (ev3triangle-line-isect p q a b c)
  (define pq (ev3- q p))
  (define m (ev3cross pq q))
  (define s (ev3dot m (ev3- c b)))
  (define t (ev3dot m (ev3- a c)))
  (define u (+ (ev3dot pq (ev3cross c b)) s))
  (define v (+ (ev3dot pq (ev3cross a c)) t))
  (define w (- (ev3dot pq (ev3cross b a)) s t))
  (cond [(>= (* (max u v w) (min u v w)) 0)
         ;; Intersection!
         (define s (+ u v w))
         (cond [(> s 0)
                (ev3+ (ev3+ (ev3* a (/ u s))
                            (ev3* b (/ v s)))
                      (ev3* c (/ w s)))]
               [else  a])]
        [else  #f]))
