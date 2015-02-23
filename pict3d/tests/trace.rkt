#lang typed/racket

(require math/flonum
         math/distributions
         pict3d/private/math/flv3
         pict3d)

(current-pict3d-width 512)
(current-pict3d-height 512)

(define shapes
  (rotate-x
   (freeze
    (combine*
     (for/list ([_  (in-range 2000)])
       (define mn (flvector (- (* 2 (random)) 1)
                            (- (* 2 (random)) 1)
                            (- (* 2 (random)) 1)))
       (define mx
         (flv3+ mn (flvector (- (* 0.2 (random)) 0.1)
                             (- (* 0.2 (random)) 0.1)
                             (- (* 0.2 (random)) 0.1))))
       
       (define r (random))
       (cond [(< r 0.5)
              (with-color '(1 0.5 0)
                (ellipsoid mn mx))]
             [else
              (with-color '(0 0.5 1.0)
                (rectangle mn mx))]))))
   30))

(: random-sphere-point (-> FlVector))
(define (random-sphere-point)
  (assert (flv3normalize (flnormal-sample 0.0 1.0 3)) values))

(define n 200)
(define vs (build-list n (λ (_) (flv3* (random-sphere-point) 2.0))))
(define dvs (map (λ ([v : FlVector])
                   (define v1 (flv3* (random-sphere-point) (+ 0.25 (* 0.25 (random)))))
                   (flv3- v1 v))
                 vs))
(define ps
  (time
   (for/list : (Listof (U #f FlVector)) ([v  (in-list vs)]
                                         [dv  (in-list dvs)])
     (trace shapes #:from v #:dir dv))))

(define traces
  (freeze
   (combine*
    (for/list ([v  (in-list vs)]
               [dv  (in-list dvs)]
               [p  (in-list ps)])
      (if p
          (combine
           (transform (with-color '(0 1 0 0.75)
                        (cylinder '(-0.002 -0.002 0) '(0.002 0.002 1) #:segments 8))
                      (point-at #:from v #:to p #:normalize? #f))
           (with-emitted '(0 1 0 3.0)
             (sphere p 0.01)))
          (transform (with-color '(1 0 0 0.75)
                        (cylinder '(-0.002 -0.002 0) '(0.002 0.002 1) #:segments 8))
                      (point-at #:from v #:dir (flv3* dv 1.5) #:normalize? #f)))))))

(combine shapes traces)
