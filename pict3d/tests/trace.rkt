#lang typed/racket

(require math/flonum
         math/distributions
         pict3d)

(current-pict3d-width 512)
(current-pict3d-height 512)

(define shapes
  (rotate-x
   (freeze
    (combine
     (for/list : (Listof Pict3D) ([_  (in-range 2000)])
       (define mn (pos (- (* 2 (random)) 1)
                       (- (* 2 (random)) 1)
                       (- (* 2 (random)) 1)))
       (define mx
         (pos+ mn (dir (- (* 0.2 (random)) 0.1)
                       (- (* 0.2 (random)) 0.1)
                       (- (* 0.2 (random)) 0.1))))
       
       (define r (random))
       (cond [(< r 0.5)
              (with-color (rgba 1 0.5 0)
                (ellipsoid mn mx))]
             [else
              (with-color (rgba 0 0.5 1.0)
                (rectangle mn mx))]))))
   30))

(: random-sphere-point (-> Dir))
(define (random-sphere-point)
  (assert (dir-normalize (dir (flnormal-sample 0.0 1.0 3))) values))

(define n 2000)
(define vs (build-list n (λ (_) (pos+ origin (dir-scale (random-sphere-point) 2.0)))))
(define dvs
  (map (λ ([v : Pos])
         (define v1 (pos+ origin (dir-scale (random-sphere-point) (+ 0.25 (* 0.25 (random))))))
         (pos- v1 v))
       vs))
(define ps
  (time
   (for/list : (Listof (U #f Pos)) ([v  (in-list vs)]
                                    [dv  (in-list dvs)])
     (trace shapes v dv))))

(define traces
  (freeze
   (combine
    (for/list : (Listof Pict3D) ([v  (in-list vs)]
                                 [dv  (in-list dvs)]
                                 [p  (in-list ps)])
      (if p
          (combine
           (transform (with-color (rgba 0 1 0 0.75)
                        (cylinder (pos -0.002 -0.002 0) (pos 0.002 0.002 1)))
                      (point-at v p #:normalize? #f))
           (with-emitted (emitted "green" 3.0)
             (sphere p 0.01)))
          (transform (with-color (rgba "red" 0.75)
                        (cylinder (pos -0.002 -0.002 0) (pos 0.002 0.002 1)))
                      (point-at v (dir-scale dv 1.5) #:normalize? #f)))))))

(combine shapes traces)

(define pict
  (let* ([pict  (group (sphere origin 1/2) 1)]
         [pict  (group (move-z pict 1) 2)]
         [pict  (group (rotate-x pict 30) 3)]
         [pict  (group (rotate-z pict 30) 4)]
         [pict  (group (move-z pict -1) 5)])
    pict))

(printf "This sphere should be more or less uniformly covered in dots:~n")
(combine
 pict
 (with-color (rgba "darkred")
   (for*/list : (Listof Pict3D) ([ρ  (in-range -85.0 86.0 5.0)]
                                 [θ  (in-range -180.0 180.0 (/ 5.0 (cos (degrees->radians ρ))))])
     (define dv (angles->dir θ ρ))
     (define v (surface pict dv))
     (cond [(not v)  (fprintf (current-error-port) "surface missed at ~v ~v ~v~n" θ ρ dv)
                     empty-pict3d]
           [else  (sphere v 0.02)]))))
