#lang typed/racket/base

(require math/flonum
         math/base
         "../math/flt3.rkt"
         "../engine/types.rkt"
         "../engine/scene.rkt")

(provide make-axis axes)

(: make-axis (-> FlTransform3 FlVector FlVector material Scene))
(define (make-axis t c e m)
  (define tinv (flt3inverse t))
  (scene-union*
   (for/list : (Listof Scene) ([a  (in-range 0 360 45)])
     (define a0 (degrees->radians (fl a)))
     (define a1 (degrees->radians (+ (fl a) 45.0)))
     (define a1/2 (* 0.5 (+ a0 a1)))
     (shape->scene
      (make-triangle-shape
       (vector (flt3apply/pos t (flvector 0.024 (* 0.02 (sin a1)) (* 0.02 (cos a1))))
               (flt3apply/pos t (flvector 0.024 (* 0.02 (sin a0)) (* 0.02 (cos a0))))
               (flt3apply/pos t (flvector 1.0 0.0 0.0)))
       (vector (flt3apply/norm tinv (flvector 0.0 (sin a1) (cos a1)))
               (flt3apply/norm tinv (flvector 0.0 (sin a0) (cos a0)))
               (flt3apply/norm tinv (flvector 1.0 0.0 0.0)))
       c e m 'front)))))

(define axis-material
  (material 0.1 0.2 0.7 0.3))

(define x-axis
  (make-axis identity-flt3
             (flvector 0.0 0.0 0.0 1.0)
             (flvector 2.0 0.125 0.125)
             axis-material))

(define y-axis
  (make-axis (rotate-z-flt3 (degrees->radians 90.0))
             (flvector 0.0 0.0 0.0 1.0)
             (flvector 0.0 1.75 0.0)
             axis-material))

(define z-axis
  (make-axis (rotate-y-flt3 (degrees->radians -90.0))
             (flvector 0.0 0.0 0.0 1.0)
             (flvector 0.25 0.25 2.5)
             axis-material))

(define axes
  (scene-union*
   (list
    (shape->scene
     (make-sphere-shape
      (scale-flt3 (flvector 0.03 0.03 0.03))
      (flvector 0.0 0.0 0.0 1.0)
      (flvector 2.0 2.0 2.0)
      axis-material
      #f))
    x-axis
    y-axis
    z-axis)))
