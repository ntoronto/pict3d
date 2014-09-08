#lang typed/racket

(require (only-in racket/unsafe/ops unsafe-flvector-set!)
         typed/rackunit
         math/flonum
         math/bigfloat
         math/base
         pict3d/private/math/ev3
         pict3d/private/math/fl2dot
         pict3d/private/math/flv3
         )

;; ===================================================================================================
;; "Exact" functions involving square roots (approximated using bigfloats)

(: ev3normalize (-> ev3 (U #f ev3)))
(define (ev3normalize v)
  (match-define (ev3 x y z) v)
  (define d (bigfloat->rational (bfsqrt (bf (+ (sqr x) (sqr y) (sqr z))))))
  (and (not (zero? d)) (ev3 (/ x d) (/ y d) (/ z d))))

(: ev3polygon-normal (-> (Vectorof ev3) (U #f ev3)))
(define (ev3polygon-normal vs)
  (ev3normalize (ev3polygon-perp vs)))

(: ev3polygon-area (-> (Vectorof ev3) Exact-Rational))
(define (ev3polygon-area vs)
  (* 1/2 (bigfloat->rational (bfsqrt (bf (ev3mag^2 (ev3polygon-perp vs)))))))

(: ev3mag (-> ev3 Nonnegative-Exact-Rational))
(define (ev3mag v)
  (max 0 (bigfloat->rational (bfsqrt (bf (ev3mag^2 v))))))

(: ev3dist (-> ev3 ev3 Nonnegative-Exact-Rational))
(define (ev3dist v1 v2)
  (max 0 (bigfloat->rational (bfsqrt (bf (ev3dist^2 v1 v2))))))

(: ev3polygon-perimeter (-> (Vectorof ev3) Nonnegative-Exact-Rational))
(define (ev3polygon-perimeter vs)
  (define n (vector-length vs))
  (cond
    [(= n 0)  0]
    [else
     (define v1 (vector-ref vs (- n 1)))
     (define-values (sum _v1)
       (for/fold ([sum : Nonnegative-Exact-Rational  0]
                  [v1 : ev3  v1]
                  ) ([i  (in-range n)])
         (define v2 (vector-ref vs i))
         (values (+ sum (ev3dist v1 v2)) v2)))
     sum]))

(: regular-polygon-area (-> Nonnegative-Exact-Rational Index Nonnegative-Exact-Rational))
;; Returns the area of the regular polygon with the given perimeter and number of sides
(define (regular-polygon-area p n)
  (cond [(< n 3)  0]
        [else
         (let ([p  (bf p)] [n  (bf n)])
           (define denom (bf* n (bftan (bf/ pi.bf n))))
           (max 0 (bigfloat->rational
                   (bf* (bf* (bf/ (bf 0.25) denom) p) p))))]))

(: ev3polygon-regularity (-> (Vectorof ev3) (U #f Nonnegative-Exact-Rational)))
(define (ev3polygon-regularity vs)
  (define n (vector-length vs))
  (cond [(< n 3)  #f]
        [else  (define p (ev3polygon-perimeter vs))
               (define area (ev3polygon-area vs))
               (define regular-area (regular-polygon-area p n))
               (cond [(zero? regular-area)  #f]
                     [else  (max 0 (/ area regular-area))])]))

;; ===================================================================================================
;; Random data generation and accessors

(define -max (flonum->ordinal -max.0))
(define +inf (flonum->ordinal +inf.0))
(define (random-flonum)
  (cond [((random) . < . 0.5)  (ordinal->flonum (random-integer -max +inf))]
        [else  (- (random) 0.5)]))

(: take-flv3 (-> FlVector Integer FlVector))
(define (take-flv3 xs i)
  (define n (flvector-length xs))
  (flvector (flvector-ref xs (modulo i n))
            (flvector-ref xs (modulo (+ i 1) n))
            (flvector-ref xs (modulo (+ i 2) n))))

(: take-flv3s (-> FlVector Integer Integer (Vectorof FlVector)))
(define (take-flv3s xs i n)
  (build-vector n (λ ([j : Index]) (take-flv3 xs (+ i (* j 3))))))

;; ===================================================================================================
;; Randomized testing against exact

(define acc-n 100000)
(define acc-xs (build-flvector acc-n (λ ([i : Index]) (if (i . < . 30) 0.0 (random-flonum)))))

(define (fl2dot-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range (- acc-n 4))])
     (define x1 (flvector-ref acc-xs i))
     (define y1 (flvector-ref acc-xs (+ i 1)))
     (define x2 (flvector-ref acc-xs (+ i 2)))
     (define y2 (flvector-ref acc-xs (+ i 3)))
     (define d (fl2dot x1 y1 x2 y2))
     (define rd (+ (* (inexact->exact x1) (inexact->exact x2))
                   (* (inexact->exact y1) (inexact->exact y2))))
     (define e (flulp-error d rd))
     (if (e . > . 0.5)
         (list e x1 y1 x2 y2 d (fl rd))
         #f))))

(define (flv3mag^2-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define v (take-flv3 acc-xs i))
     (define d (flv3mag^2 v))
     (define rd (ev3mag^2 (flv3->ev3 v)))
     (define e (flulp-error d rd))
     (if (e . > . 1.0)
         (list e v d (fl rd))
         #f))))

(define (flv3mag-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define v (take-flv3 acc-xs i))
     (define d (flv3mag v))
     (define rd (ev3mag (flv3->ev3 v)))
     (define e (flulp-error d rd))
     (if (e . > . 0.5)
         (list e v d (fl rd))
         #f))))

(define (flv3normalize-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define v0 (take-flv3 acc-xs i))
     (define v (flv3normalize v0))
     (define rv (ev3normalize (flv3->ev3 v0)))
     (define e (cond [(and v rv)  (flv3ulp-error v rv)]
                     [(or v rv)  +inf.0]
                     [else  0.0]))
     (if (e . > . 1.5)
         (list e v0 v (and rv (ev3->flv3 rv)))
         #f))))

(define (flv3dot-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define v1 (take-flv3 acc-xs i))
     (define v2 (take-flv3 acc-xs (+ i 3)))
     (define d (flv3dot v1 v2))
     (define rd (ev3dot (flv3->ev3 v1) (flv3->ev3 v2)))
     (define e (flulp-error d rd))
     (if (e . > . 0.5)
         (list e v1 v2 d (fl rd))
         #f))))

(define (flv3cross-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define v1 (take-flv3 acc-xs i))
     (define v2 (take-flv3 acc-xs (+ i 3)))
     (define v (flv3cross v1 v2))
     (define rv (ev3cross (flv3->ev3 v1) (flv3->ev3 v2)))
     (define e (flv3ulp-error v rv))
     (if (e . > . 1.0)
         (list e v1 v2 v (ev3->flv3 rv))
         #f))))

(define (flv3dist^2-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define v1 (take-flv3 acc-xs i))
     (define v2 (take-flv3 acc-xs (+ i 3)))
     (define d (flv3dist^2 v1 v2))
     (define rd (ev3dist^2 (flv3->ev3 v1) (flv3->ev3 v2)))
     (define e (flulp-error d rd))
     (if (e . > . 2.0)
         (list e v1 v2 d (fl rd))
         #f))))

(define (flv3dist-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define v1 (take-flv3 acc-xs i))
     (define v2 (take-flv3 acc-xs (+ i 3)))
     (define d (flv3dist v1 v2))
     (define rd (ev3dist (flv3->ev3 v1) (flv3->ev3 v2)))
     (define e (flulp-error d rd))
     (if (e . > . 1.5)
         (list e v1 v2 d (fl rd))
         #f))))

(define (flv3polygon-perp-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define n (+ 3 (random 7)))
     (define vs (take-flv3s acc-xs i n))
     (define v (flv3polygon-perp vs))
     (define rv (ev3polygon-perp (vector-map flv3->ev3 vs)))
     (define e (flv3ulp-error v rv))
     (if (e . > . 0.5)
         (list e vs v (ev3->flv3 rv))
         #f))))

(define (flv3polygon-normal-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define n (+ 3 (random 7)))
     (define vs (build-vector n (λ ([j : Index]) (take-flv3 acc-xs (+ i (* j 3))))))
     (define v (flv3polygon-normal vs))
     (define rv (ev3polygon-normal (vector-map flv3->ev3 vs)))
     (define e (cond [(and v rv)  (flv3ulp-error v rv)]
                     [(or v rv)  +inf.0]
                     [else  0.0]))
     (if (e . > . 1.0)
         (list e vs v (and rv (ev3->flv3 rv)))
         #f))))

(define (flv3polygon-centroid-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define n (+ 3 (random 7)))
     (define vs (build-vector n (λ ([j : Index]) (take-flv3 acc-xs (+ i (* j 3))))))
     (define v (flv3polygon-centroid vs))
     (define rv (ev3polygon-centroid (vector-map flv3->ev3 vs)))
     (define e (flv3ulp-error v rv))
     (if (e . > . 0.5)
         (list e vs v (ev3->flv3 rv))
         #f))))

(define (flplane3-point-dist-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define norm (take-flv3 acc-xs i))
     (define dist (flvector-ref acc-xs (modulo (+ i 3) acc-n)))
     (define p (flplane3 norm dist))
     (define v (take-flv3 acc-xs (+ i 4)))
     (define rp (and p (flplane3->eplane3 p)))
     (define rv (flv3->ev3 v))
     (define d (and p (flplane3-point-dist p v)))
     (define rd (and rp (eplane3-point-dist rp rv)))
     (define e (cond [(and d rd)  (flulp-error d rd)]
                     [(or d rd)  +inf.0]
                     [else  0.0]))
     (if (e . > . 0.5)
         (list e p v d (and rd (fl rd)))
         #f))))

(define (flplane3-line-isect-time-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define norm (take-flv3 acc-xs i))
     (define dist (flvector-ref acc-xs (modulo (+ i 3) acc-n)))
     (define p (flplane3 norm dist))
     (define v1 (take-flv3 acc-xs (+ i 4)))
     (define v2 (take-flv3 acc-xs (+ i 7)))
     (define rp (and p (flplane3->eplane3 p)))
     (define rv1 (flv3->ev3 v1))
     (define rv2 (flv3->ev3 v2))
     (define segment? ((random) . < . 0.5))
     (define d (and p (flplane3-line-isect-time p v1 v2 segment?)))
     (define rd (and rp (eplane3-line-isect-time rp rv1 rv2 segment?)))
     (define e (cond [(and d rd)  (flulp-error d rd)]
                     [(or d rd)  +inf.0]
                     [else  0.0]))
     (if (e . > . 1.0)
         (list e p v1 v2 segment? d (and rd (fl rd)))
         #f))))

(define (flplane3-line-isect-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define norm (take-flv3 acc-xs i))
     (define dist (flvector-ref acc-xs (modulo (+ i 3) acc-n)))
     (define p (flplane3 norm dist))
     (define v1 (take-flv3 acc-xs (+ i 4)))
     (define v2 (take-flv3 acc-xs (+ i 7)))
     (define rp (and p (flplane3->eplane3 p)))
     (define rv1 (flv3->ev3 v1))
     (define rv2 (flv3->ev3 v2))
     (define segment? ((random) . < . 0.5))
     (define v (and p (flplane3-line-isect p v1 v2 segment?)))
     (define rv (and rp (eplane3-line-isect rp rv1 rv2 segment?)))
     (define e (cond [(and v rv)  (flv3ulp-error v rv)]
                     [(or v rv)  +inf.0]
                     [else  0.0]))
     (if (e . > . 0.5)
         (list e p v1 v2 segment? v (and rv (ev3->flv3 rv)))
         #f))))

(define (flplane3-relative-dists-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define n (+ 3 (random 7)))
     (define vs (take-flv3s acc-xs i n))
     (define norm (take-flv3 acc-xs (+ i (* n 3))))
     (define dist (flvector-ref acc-xs (modulo (+ i (* (+ n 1) 3))
                                                acc-n)))
     (define p (flplane3 norm dist))
     (define rp (and p (flplane3->eplane3 p)))
     (define c (and p (flplane3-relative-dists p vs)))
     (define rc (and rp (eplane3-relative-dists rp (vector-map flv3->ev3 vs))))
     (define e (cond [(and c rc)  (apply max (map flulp-error (flvector->list c) (vector->list rc)))]
                     [(or c rc)  +inf.0]
                     [else  0.0]))
     (if (e . > . 2.0)
         (list e vs p c (and rc (vector-map fl rc)))
         #f))))

(define (flv3polygon-perimeter-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define n (+ 3 (random 7)))
     (define vs (take-flv3s acc-xs i n))
     (define p (flv3polygon-perimeter vs))
     (define rp (ev3polygon-perimeter (vector-map flv3->ev3 vs)))
     (define e (flulp-error p rp))
     (if (e . > . 2.5)
         (list e vs p (fl rp))
         #f))))

(define (flv3polygon-area-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define n (+ 3 (random 7)))
     (define vs (take-flv3s acc-xs i n))
     (define a (flv3polygon-area vs))
     (define ra (ev3polygon-area (vector-map flv3->ev3 vs)))
     (define e (flulp-error a ra))
     (if (e . > . 1.25)
         (list e vs a (fl ra))
         #f))))

(define (flv3polygon-regularity-errors)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range acc-n)])
     (define n (+ 3 (random 7)))
     (define vs (take-flv3s acc-xs i n))
     (define r (flv3polygon-regularity vs))
     (define rr (ev3polygon-regularity (vector-map flv3->ev3 vs)))
     (define e (cond [(and r rr)  (flulp-error r rr)]
                     [(or r rr)  +inf.0]
                     [else  0.0]))
     (if (e . > . 6.5)
         (list e vs r (and rr (fl rr)))
         #f))))

(printf "********** Accuracy tests **********~n")

(printf "Testing fl2dot~n")
(check-equal? (fl2dot-errors) empty)
(newline)
#|
(printf "Testing flv3mag^2~n")
(check-equal? (flv3mag^2-errors) empty)
(newline)

(printf "Testing flv3mag~n")
(check-equal? (flv3mag-errors) empty)
(newline)

(printf "Testing flv3dot~n")
(check-equal? (flv3dot-errors) empty)
(newline)

(printf "Testing flv3cross~n")
(check-equal? (flv3cross-errors) empty)
(newline)

(printf "Testing flv3normalize~n")
(check-equal? (flv3normalize-errors) empty)
(newline)

(printf "Testing flv3dist^2~n")
(check-equal? (flv3dist^2-errors) empty)
(newline)

(printf "Testing flv3dist~n")
(check-equal? (flv3dist-errors) empty)
(newline)

(printf "Testing flv3polygon-perp~n")
(check-equal? (flv3polygon-perp-errors) empty)
(newline)

(printf "Testing flv3polygon-normal~n")
(check-equal? (flv3polygon-normal-errors) empty)
(newline)

(printf "Testing flv3polygon-centroid~n")
(check-equal? (flv3polygon-centroid-errors) empty)
(newline)

(printf "Testing flplane3-point-dist~n")
(check-equal? (flplane3-point-dist-errors) empty)
(newline)

(printf "Testing flplane3-line-isect-time~n")
(check-equal? (flplane3-line-isect-time-errors) empty)
(newline)

(printf "Testing flplane3-line-isect~n")
(check-equal? (flplane3-line-isect-errors) empty)
(newline)

(printf "Testing flplane3-relative-dists~n")
(check-equal? (flplane3-relative-dists-errors) empty)
(newline)

(printf "Testing flv3polygon-area~n")
(check-equal? (flv3polygon-area-errors) empty)
(newline)

(printf "Testing flv3polygon-perimeter~n")
(check-equal? (flv3polygon-perimeter-errors) empty)
(newline)

(printf "Testing flv3polygon-regularity~n")
(check-equal? (flv3polygon-regularity-errors) empty)
(newline)

;; ===================================================================================================
;; Randomized testing for self-consistency

(define con-n 10000)
(define con-xs (build-flvector con-n (λ ([i : Index]) (if (i . < . 30) 0.0 (random-flonum)))))

(define (flv3polygon-plane-consistency)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range con-n)])
     (define vs (take-flv3s con-xs i 3))
     (define p (flv3polygon-plane vs))
     (define es (and p (map (λ ([v : FlVector])
                              (define es (flplane3-relative-dists p (vector v)))
                              (if es (flvector-ref es 0) +inf.0))
                            (vector->list vs))))
     (if (and es (andmap (λ ([e : Flonum]) (and (flrational? e) ((abs e) . > . epsilon.0))) es))
         (list es vs)
         #f))))

(define (flplane3-line-isect-dist-consistency)
  (filter
   (λ (r) r)
   (for/list : (Listof Any) ([i  (in-range con-n)])
     (define norm (flv3normalize (take-flv3 con-xs i)))
     (define dist (flvector-ref con-xs (modulo (+ i 3) con-n)))
     (define p (and norm (flplane3 norm dist)))
     (define v1 (take-flv3 con-xs (+ i 4)))
     (define v2 (take-flv3 con-xs (+ i 7)))
     (define v (and p (flplane3-line-isect p v1 v2 #f)))
     (define es (and v (flv3rational? v) p (flplane3-relative-dists p (vector v))))
     (define e (and es (flvector-ref es 0)))
     (if (and e (flrational? e) ((abs e) . > . epsilon.0))
         (list e p v1 v2 v)
         #f))))

(printf "********** Consistency Tests **********~n")
(newline)

(printf "Testing relative distance of triangle vertices to fitted plane~n")
(check-equal? (flv3polygon-plane-consistency) empty)
(newline)

(printf "Testing relative distance of line-plane intersection to plane~n")
(check-equal? (flplane3-line-isect-dist-consistency) empty)
(newline)
|#
;; ===================================================================================================
;; Speed tests
#|
(printf "********** Speed tests **********~n")
(newline)

(define n 1000000)

(define xs (build-flvector n (λ (_) (random-flonum) #;(- (random) 0.5))))
(define v1s (for/vector ([i  (in-range n)]) : FlVector  (take-flv3 xs i)))
(define v2s (for/vector ([i  (in-range n)]) : FlVector  (take-flv3 xs (+ i 3))))
(define v3s (for/vector ([i  (in-range n)]) : FlVector  (take-flv3 xs (+ i 6))))
(define v4s (for/vector ([i  (in-range n)]) : FlVector  (take-flv3 xs (+ i 9))))
(define: bx : Any  #f)
(define vbx (flvector 0.0))

(printf "Testing flv3mag^2~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define v1 (vector-ref v1s i))
          (flvector-set! vbx 0 (flv3mag^2 v1)))))
(newline)

(printf "Testing flv3mag~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define v1 (vector-ref v1s i))
          (flvector-set! vbx 0 (flv3mag v1)))))
(newline)

(printf "Testing flv3dot~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define v1 (vector-ref v1s i))
          (define v2 (vector-ref v2s i))
          (unsafe-flvector-set! vbx 0 (flv3dot v1 v2)))))
(newline)

(printf "Testing flv3cross~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define v1 (vector-ref v1s i))
          (define v2 (vector-ref v2s i))
          (set! bx (flv3cross v1 v2)))))
(newline)

(printf "Testing flv3normalize~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define v1 (vector-ref v1s i))
          (set! bx (flv3normalize v1)))))
(newline)

(printf "Testing flv3dist^2~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define v1 (vector-ref v1s i))
          (define v2 (vector-ref v2s i))
          (flvector-set! vbx 0 (flv3dist^2 v1 v2)))))
(newline)

(printf "Testing flv3dist~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define v1 (vector-ref v1s i))
          (define v2 (vector-ref v2s i))
          (flvector-set! vbx 0 (flv3dist v1 v2)))))
(newline)

(printf "Testing flv3polygon-perp~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define vs (vector (vector-ref v1s i) (vector-ref v2s i) (vector-ref v3s i)))
          (set! bx (flv3polygon-perp vs)))))
(newline)

(printf "Testing flv3polygon-normal~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define vs (vector (vector-ref v1s i) (vector-ref v2s i) (vector-ref v3s i)))
          (set! bx (flv3polygon-normal vs)))))
(newline)

(printf "Testing flv3polygon-centroid~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define vs (vector (vector-ref v1s i) (vector-ref v2s i) (vector-ref v3s i)))
          (set! bx (flv3polygon-centroid vs)))))
(newline)

(printf "Testing flv3polygon-plane~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define vs (vector (vector-ref v1s i) (vector-ref v2s i) (vector-ref v3s i)))
          (set! bx (flv3polygon-plane vs)))))
(newline)

(printf "Testing flplane3-point-dist~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define norm (vector-ref v1s i))
          (define v (vector-ref v2s i))
          (define dist (flvector-ref xs (modulo (+ i 6) n)))
          (define p (flplane3 norm dist))
          (set! bx (and p (flplane3-point-dist p v))))))
(newline)

(printf "Testing flplane3-line-isect-time, segment? = #t~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define v1 (vector-ref v1s i))
          (define v2 (vector-ref v2s i))
          (define norm (vector-ref v3s i))
          (define dist (flvector-ref xs (modulo (+ i 9) n)))
          (define p (flplane3 norm dist))
          (set! bx (and p (flplane3-line-isect-time p v1 v2))))))
(newline)

(printf "Testing flplane3-line-isect-time, segment? = #f~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define v1 (vector-ref v1s i))
          (define v2 (vector-ref v2s i))
          (define norm (vector-ref v3s i))
          (define dist (flvector-ref xs (modulo (+ i 9) n)))
          (define p (flplane3 norm dist))
          (set! bx (and p (flplane3-line-isect-time p v1 v2 #f))))))
(newline)

(printf "Testing flplane3-line-isect, segment? = #t~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define v1 (vector-ref v1s i))
          (define v2 (vector-ref v2s i))
          (define norm (vector-ref v3s i))
          (define dist (flvector-ref xs (modulo (+ i 9) n)))
          (define p (flplane3 norm dist))
          (set! bx (and p (flplane3-line-isect p v1 v2))))))
(newline)

(printf "Testing flplane3-line-isect, segment? = #f~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define v1 (vector-ref v1s i))
          (define v2 (vector-ref v2s i))
          (define norm (vector-ref v3s i))
          (define dist (flvector-ref xs (modulo (+ i 9) n)))
          (define p (flplane3 norm dist))
          (set! bx (and p (flplane3-line-isect p v1 v2 #f))))))
(newline)

(printf "Testing flplane3-relative-dists~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define vs (vector (vector-ref v1s i) (vector-ref v2s i) (vector-ref v3s i)))
          (define norm (vector-ref v4s i))
          (define dist (flvector-ref xs (modulo (+ i 12) n)))
          (define p (flplane3 norm dist))
          (set! bx (and p (flplane3-relative-dists p vs))))))
(newline)

(printf "Testing flv3polygon-perimeter~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define vs (vector (vector-ref v1s i) (vector-ref v2s i) (vector-ref v3s i)))
          (unsafe-flvector-set! vbx 0 (flv3polygon-perimeter vs)))))
(newline)

(printf "Testing flv3polygon-area~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define vs (vector (vector-ref v1s i) (vector-ref v2s i) (vector-ref v3s i)))
          (unsafe-flvector-set! vbx 0 (flv3polygon-area vs)))))
(newline)

(printf "Testing flv3polygon-regularity~n")
(for ([_  (in-range 5)])
  (time (for ([i  (in-range n)])
          (define vs (vector (vector-ref v1s i) (vector-ref v2s i) (vector-ref v3s i)))
          (define r (flv3polygon-regularity vs))
          (when r (unsafe-flvector-set! vbx 0 r)))))
(newline)
|#
