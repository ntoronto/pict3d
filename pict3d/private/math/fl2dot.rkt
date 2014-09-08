#lang typed/racket/base

(require math/flonum
         "fle2.rkt"
         "flonum.rkt")

(provide fast-fl2dot
         fl2dot
         fast-fl2dot/error
         fl2dot/error)

;; ===================================================================================================
;; 2D dot and cross products: : <= 0.5ulp error, except sometimes when output is subnormal

(: fast-fl2dot (-> Flonum Flonum Flonum Flonum Flonum))
(define (fast-fl2dot x1 y1 x2 y2)
  (let*-values ([(a-hi a-lo)  (fast-fl*/error x1 x2)]
                [(b-hi b-lo)  (fast-flfma/error y1 y2 a-hi)])
    (+ b-hi (+ b-lo a-lo))))

(: slow-fl2dot (-> Flonum Flonum Flonum Flonum Flonum))
(define (slow-fl2dot x1 y1 x2 y2)
  (let*-values ([(x.e x.hi x.lo)  (fl*/e2 x1 x2)]
                [(y.e y.hi y.lo)  (fl*/e2 y1 y2)]
                [(z.e z.hi z.lo)  (fle2+ x.e x.hi x.lo y.e y.hi y.lo)])
    (fle2->flonum z.e z.hi z.lo)))

(: fl2dot (-> Flonum Flonum Flonum Flonum Flonum))
(define (fl2dot x1 y1 x2 y2)
  (define d (fast-fl2dot x1 y1 x2 y2))
  (if (< +max-subnormal.0 (abs d) +inf.0) d (slow-fl2dot x1 y1 x2 y2)))

;; ---------------------------------------------------------------------------------------------------

(: fast-fl2dot/error (-> Flonum Flonum Flonum Flonum (Values Flonum Flonum)))
(define (fast-fl2dot/error x1 y1 x2 y2)
  (let*-values ([(a-hi a-lo)  (fast-fl*/error x1 x2)]
                [(b-hi b-lo)  (fast-flfma/error y1 y2 a-hi)])
    (fast-fl+/error b-hi (+ b-lo a-lo))))

(: slow-fl2dot/error (-> Flonum Flonum Flonum Flonum (Values Flonum Flonum)))
(define (slow-fl2dot/error x1 y1 x2 y2)
  (let-values ([(x1 x2)  (abs-sort x1 x2)]
               [(y1 y2)  (abs-sort y1 y2)])
    (define n1 (flpow2near (max (abs x1) (abs y1))))
    (define n2 (flpow2near (max (abs x2) (abs y2))))
    (define-values (d-hi d-lo) (fast-fl2dot/error (/ x1 n1) (/ y1 n1) (/ x2 n2) (/ y2 n2)))
    (values (fl** n1 n2 d-hi)
            (fl** n1 n2 d-lo))))

(: fl2dot/error (-> Flonum Flonum Flonum Flonum (Values Flonum Flonum)))
(define (fl2dot/error x1 y1 x2 y2)
  (define-values (d-hi d-lo) (fast-fl2dot/error x1 y1 x2 y2))
  (if (< +max-subnormal.hi (abs (+ d-hi d-lo)) +inf.0)
      (values d-hi d-lo)
      (slow-fl2dot/error x1 y1 x2 y2)))
