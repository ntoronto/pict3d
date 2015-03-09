#lang racket/base

(require racket/list
         racket/vector
         racket/flonum
         racket/math
         "../../math/flv3.rkt"
         "../../math/flrect3.rkt"
         )

(provide max-scale-index
         scale-index->scale
         flrect3->scale-index)

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

(define (flrect3->scale b)
  ;; Don't try anything tricky until we figure out something sufficiently tricky
  1)

(define (flrect3->scale-index b)
  (define r (flrect3->scale b))
  (define scale (vector-argmin (λ (s) (abs (- r s))) scales))
  (vector-member scale scales))
