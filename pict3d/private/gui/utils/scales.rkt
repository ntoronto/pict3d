#lang racket/base

(require racket/list
         racket/vector
         racket/flonum
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

(define (flrect3->scale-index b)
  (cond [b  (define-values (x1 y1 z1) (flv3-values (flrect3-min b)))
            (define-values (x2 y2 z2) (flv3-values (flrect3-max b)))
            (define r (flexpt (* (- x2 x1) (- y2 y1) (- z2 z1)) #i1/3))
            (define scale (vector-argmin (λ (s) (abs (- r s))) scales))
            (vector-member scale scales)]
        [else  (vector-member 1 scales)]))
