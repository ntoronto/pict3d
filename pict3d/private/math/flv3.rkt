#lang racket/base

(require racket/flonum
         racket/unsafe/ops
         typed/racket/base
         (except-in "typed-flv3.rkt"
                    flv3-values))

(provide (all-from-out "typed-flv3.rkt")
         flv3-values)

;(: flv3-values (-> FlVector (Values Flonum Flonum Flonum)))
(define-syntax-rule (flv3-values v-stx)
  (let ([v : FlVector  v-stx])
    (unless (= 3 (flvector-length v))
      (raise-type-error 'flv3-values "length-3 FlVector" v))
    (values (unsafe-flvector-ref v 0)
            (unsafe-flvector-ref v 1)
            (unsafe-flvector-ref v 2))))
