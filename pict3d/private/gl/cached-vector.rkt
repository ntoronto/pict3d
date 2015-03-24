#lang typed/racket/base

(require "typed-context.rkt"
         "../utils.rkt")

(provide make-gl-cached-vector)

(: make-gl-cached-vector (All (A) (-> Symbol (-> Integer A) (-> A Index) (-> Integer A))))
(define (make-gl-cached-vector name make-vec vec-length)
  (: the-vec (U #f A))
  (define the-vec #f)
  
  (: get-vec (-> Integer A))
  (define (get-vec size)
    (get-current-managed-gl-context name)
    (cond [(index? size)
           (define vec the-vec)
           (cond [(and vec (<= size (vec-length vec)))  vec]
                 [else
                  (define vec (make-vec (next-pow2 size)))
                  (set! the-vec vec)
                  vec])]
          [else
           (raise-argument-error name "Index" size)]))
  
  get-vec)
