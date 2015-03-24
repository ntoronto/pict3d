#lang typed/racket/base

(require math/flonum)

(provide fl2dot
         fl2cross)

;(: fl2dot (-> Flonum Flonum Flonum Flonum Flonum))
;; Tuned to make observed error bounded by 4.0 ulp
(define-syntax-rule (fl2dot x1-stx y1-stx x2-stx y2-stx)
  (let ([x1 : Flonum  x1-stx]
        [y1 : Flonum  y1-stx]
        [x2 : Flonum  x2-stx]
        [y2 : Flonum  y2-stx])
    (let ([x  (* x1 x2)]
          [y  (* y1 y2)])
      (if (< #i-4/3 (/ x y) #i-3/4)
          (let-values ([(x.hi x.lo)  (fast-fl*/error x1 x2)]
                       [(y.hi y.lo)  (fast-fl*/error y1 y2)])
            (+ x.hi y.hi x.lo y.lo))
          (+ x y)))))

;(: fl2cross (-> Flonum Flonum Flonum Flonum Flonum))
(define-syntax-rule (fl2cross x1 y1 x2 y2)
  (fl2dot x1 y1 y2 (- x2)))
