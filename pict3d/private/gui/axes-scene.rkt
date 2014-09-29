#lang typed/racket/base

(require math/flonum
         math/base
         "../math/flv3.rkt"
         "../math/flt3.rkt"
         "../math/flrect3.rkt"
         "../engine/types.rkt"
         "../engine/scene.rkt")

(provide axes-scene)

(: make-unit-pyramid-scene (-> FlVector FlVector material Scene))
(define (make-unit-pyramid-scene c e m)
  (scene-union
   (scene-union*
    (for/list : (Listof Scene) ([i  (in-range 4)])
      (define vs (vector (flvector 1.0 -1.0 0.0)
                         (flvector 1.0 +1.0 0.0)
                         (flvector 0.0 0.0 1.0)))
      (define norm (assert (flv3polygon-normal vs) values))
      (scene-transform-shapes
       (shape->scene (make-triangle-shape vs norm c e m #f))
       (rotate-z-flt3 (degrees->radians (* (fl i) +90.0)))
       (rotate-z-flt3 (degrees->radians (* (fl i) -90.0))))))
   (shape->scene
    (make-quad-shape
     (vector (flvector +1.0 +1.0 0.0)
             (flvector +1.0 -1.0 0.0)
             (flvector -1.0 -1.0 0.0)
             (flvector -1.0 +1.0 0.0))
     (flvector 0.0 0.0 1.0)
     c e m #f))))

(: make-unit-arrow-scene (-> FlVector FlVector material Scene))
(define (make-unit-arrow-scene c e m)
  (scene-union
   (shape->scene
    (make-rectangle-shape
     (assert (flrect3 (flvector #i-1/64 #i-1/64 #i-1/64)
                      (flvector #i1/64 #i1/64 #i56/64))
             nonempty-flrect3?)
     c e m #f))
   (let ([t  (flt3compose
              (translate-flt3 (flvector 0.0 0.0 #i56/64))
              (scale-flt3 (flvector #i2/64 #i2/64 #i8/64)))])
     (scene-transform-shapes
      (make-unit-pyramid-scene c e m)
      t (flt3inverse t)))))

(define axis-material
  (material 0.1 0.2 0.7 0.3))

(define x-axis-scene
  (scene-transform-shapes
   (make-unit-arrow-scene
    (flvector 0.0 0.0 0.0 1.0)
    (flvector 1.0 0.05 0.05 2.0)
    axis-material)
   (rotate-y-flt3 (degrees->radians +90.0))
   (rotate-y-flt3 (degrees->radians -90.0))))

(define y-axis-scene
  (scene-transform-shapes
   (make-unit-arrow-scene
    (flvector 0.0 0.0 0.0 1.0)
    (flvector 0.0 1.0 0.0 1.75)
    axis-material)
   (rotate-x-flt3 (degrees->radians -90.0))
   (rotate-x-flt3 (degrees->radians +90.0))))

(define z-axis-scene
  (make-unit-arrow-scene
   (flvector 0.0 0.0 0.0 1.0)
   (flvector 0.1 0.1 1.0 2.5)
   axis-material))

(define axes-scene
  (shape->scene
   (make-frozen-scene-shape
    (assert
     (scene-union*
      (list (shape->scene
             (make-sphere-shape
              (scale-flt3 (flvector 0.03 0.03 0.03))
              (flvector 0.0 0.0 0.0 1.0)
              (flvector 1.0 1.0 1.0 2.0)
              axis-material
              #f))
            x-axis-scene
            y-axis-scene
            z-axis-scene))
     nonempty-scene?))))
