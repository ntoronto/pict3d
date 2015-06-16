#lang typed/racket/base

(require math/flonum
         math/base)

(provide (all-defined-out))

(define 2^256 (flexpt 2.0 256.0))
(define 2^-256 (flexpt 2.0 -256.0))
(define 2^308 (flexpt 2.0 308.0))
(define 2^-308 (flexpt 2.0 -308.0))
(define 2^512 (flexpt 2.0 512.0))
(define 2^-512 (flexpt 2.0 -512.0))
;; (sqr (* +min.0 2^564)) > +max-subnormal.0
(define 2^564 (flexpt 2.0 564.0))
(define 2^-564 (flexpt 2.0 -564.0))

;(: fast-flfma (-> Flonum Flonum Flonum Flonum))
;; Tuned to make observed error bounded by 4.0 ulp
(define-syntax-rule (fast-flfma x1-stx s-stx x2-stx)
  (let ([x1 : Flonum  x1-stx]
        [s  : Flonum  s-stx]
        [x2 : Flonum  x2-stx])
    (let ([x  (* x1 s)])
      (if (< #i-8/7 (/ x x2) #i-7/8)
          (let-values ([(x.hi x.lo)  (fast-fl*/error x1 s)])
            (+ x.hi x2 x.lo))
          (+ x x2)))))

(: flregular-polygon-area (-> Nonnegative-Flonum Index Nonnegative-Flonum))
;; Returns the area of the regular polygon with the given perimeter and number of sides
(define (flregular-polygon-area p n)
  (cond [(< n 3)  0.0]
        [else
         (let ([n  (fl n)])
           (define denom (if (n . > . 1e9) pi (* n (fltanpix (/ 1.0 n)))))
           (max 0.0 (* (* (/ 0.25 denom) p) p)))]))

;(: flblend (-> Flonum Flonum Flonum Flonum))
(define-syntax-rule (flblend x1 x2 α-stx)
  (let ([α : Flonum  α-stx])
    (+ (* x1 (- 1.0 α)) (* x2 α))))

;(: flclamp (-> Flonum Flonum Flonum Flonum))
(define-syntax-rule (flclamp x mn mx)
  (max mn (min mx x)))

(define-syntax-rule (flnear? x1 x2 eps)
  (<= (abs (- x1 x2)) eps))
