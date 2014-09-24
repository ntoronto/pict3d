#lang racket/base

(require racket/flonum
         racket/unsafe/ops
         typed/untyped-utils
         typed/racket/base
         (except-in "typed-flt3.rkt"
                    flt3inverse
                    flt3compose))

(require/untyped-contract
 (begin (require (only-in "typed-flt3.rkt"
                          FlTransform3)))
 "typed-flt3.rkt"
 [flt3inverse  (-> FlTransform3 FlTransform3)]
 [flt3compose  (-> FlTransform3 FlTransform3 FlTransform3)])

(provide (all-from-out "typed-flt3.rkt")
         flt3inverse
         flt3compose
         flv4-values)

;(: flv4-values (-> FlVector (Values Flonum Flonum Flonum Flonum)))
(define-syntax-rule (flv4-values v-stx)
  (let ([v : FlVector  v-stx])
    (unless (= 4 (flvector-length v))
      (raise-type-error 'flv4-values "length-4 FlVector" v))
    (values (unsafe-flvector-ref v 0)
            (unsafe-flvector-ref v 1)
            (unsafe-flvector-ref v 2)
            (unsafe-flvector-ref v 3))))
