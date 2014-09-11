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
  
  (: transform-pos (-> FlVector FlVector))
  (define (transform-pos v)
    (flv4->pos (flt3apply t (pos->flv4 v))))
  
  (: transform-norm (-> FlVector FlVector))
  (define (transform-norm v)
    (flv4->dir (flt3tapply tinv (dir->flv4 v))))
  
  (scene-union*
   (for/list : (Listof Scene) ([t  (in-range 0 360 45)])
     (define t0 (degrees->radians (fl t)))
     (define t1 (degrees->radians (+ (fl t) 45.0)))
     (define t1/2 (* 0.5 (+ t0 t1)))
     (shape->scene
      (make-triangle-shape
       (vector (transform-pos (flvector 0.024 (* 0.02 (sin t1)) (* 0.02 (cos t1))))
               (transform-pos (flvector 0.024 (* 0.02 (sin t0)) (* 0.02 (cos t0))))
               (transform-pos (flvector 1.0 0.0 0.0)))
       (vector (transform-norm (flvector 0.0 (sin t1) (cos t1)))
               (transform-norm (flvector 0.0 (sin t0) (cos t0)))
               (transform-norm (flvector 1.0 0.0 0.0)))
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
