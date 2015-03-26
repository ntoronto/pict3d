#lang typed/racket/base

(require math/flonum)

(provide fl4dot)

;(: fl4dot (-> Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum))
;; Observed error bounded by 5.0 ulp
(define-syntax-rule (fl4dot x1-stx y1-stx z1-stx w1-stx x2-stx y2-stx z2-stx w2-stx)
  (let ([x1 : Flonum  x1-stx]
        [y1 : Flonum  y1-stx]
        [z1 : Flonum  z1-stx]
        [w1 : Flonum  w1-stx]
        [x2 : Flonum  x2-stx]
        [y2 : Flonum  y2-stx]
        [z2 : Flonum  z2-stx]
        [w2 : Flonum  w2-stx])
    (let* ([x  (* x1 x2)]
           [y  (* y1 y2)]
           [a  (+ x y)]
           [z  (* z1 z2)]
           [b  (+ a z)]
           [w  (* w1 w2)])
      (if (or (< -2.0 (/ x y) -0.5)
              (< -2.0 (/ a z) -0.5)
              (< -2.0 (/ b w) -0.5))
          (let*-values ([(x.hi x.lo)  (fast-fl*/error x1 x2)]
                        [(y.hi y.lo)  (fast-fl*/error y1 y2)]
                        [(z.hi z.lo)  (fast-fl*/error z1 z2)]
                        [(w.hi w.lo)  (fast-fl*/error w1 w2)]
                        [(e.hi e.lo)  (fast-fl+/error x.hi y.hi)]
                        [(a.hi a.lo)  (fast-mono-fl+/error e.hi (+ e.lo x.lo y.lo))]
                        [(f.hi f.lo)  (fast-fl+/error z.hi w.hi)]
                        [(b.hi b.lo)  (fast-mono-fl+/error f.hi (+ f.lo z.lo w.lo))])
            (+ a.hi b.hi a.lo b.lo))
          (+ b w)))))
