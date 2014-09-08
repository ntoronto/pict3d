#lang typed/racket/base

(require math/flonum)

(provide abs-sort
         add-error
         fl**)

;(: abs-sort (-> Flonum Flonum (Values Flonum Flonum)))
(define-syntax-rule (abs-sort a-stx b-stx)
  (let ([a  a-stx] [b  b-stx])
    (if ((abs a) . < . (abs b)) (values a b) (values b a))))

;(: add-error (-> Flonum Flonum Flonum))
(define-syntax-rule (add-error a-stx b-stx)
  (let ([a  a-stx] [b  b-stx])
    (/ (+ (abs a) (abs b))
       (abs (+ a b)))))

(: fl** (-> Flonum Flonum Flonum Flonum))
;; Like (fl* (fl* x y) z), but doesn't prematurely underflow or overflow
(define (fl** x y z)
  (define n (* x y))
  (cond [(< +max-subnormal.0 (abs n) +inf.0)  (* n z)]
        [else  (define n (* y z))
               (cond [(< +max-subnormal.0 (abs n) +inf.0)  (* n x)]
                     [else  (* (* z x) y)])]))
