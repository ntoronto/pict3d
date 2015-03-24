#lang racket/base

(require racket/list
         racket/vector)

(provide max-scale-index
         scale-index->scale
         scale->scale-index)

(define scales
  (list->vector
   (drop-right
    (rest
     (sort
      (append*
       (for/list ([j  (in-range -5 6)])
         (list (expt 10 j)
               (/ (expt 10 j) 2)
               (* (expt 10 j) 2))))
      <))
    1)))

(define max-scale-index (- (vector-length scales) 1))

(define (scale-index->scale i)
  (vector-ref scales (max 0 (min max-scale-index i))))

(define (scale->scale-index r)
  (define i (vector-member (vector-argmin (λ (s) (abs (- (log (abs r)) (log s)))) scales) scales))
  (if i i (scale->scale-index 1)))
