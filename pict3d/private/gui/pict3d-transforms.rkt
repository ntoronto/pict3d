#lang typed/racket/base

(require math/base
         math/flonum
         "../math/flt3.rkt"
         "../math/flv3.rkt"
         (only-in "../engine/types.rkt" affine-transform)
         "pict3d-combinators.rkt"
         "pict3d-struct.rkt"
         "user-types.rkt")

(provide (all-defined-out))

(: pict3d-view-transform (-> Pict3D (-> Pict3D Affine) FlAffine3-))
(define (pict3d-view-transform p auto-camera)
  (let* ([t  (camera-transform p)]
         [t  (if t t (auto-camera p))])
    (flt3compose (scale-flt3 +x-y-z-flv3)
                 (flt3inverse (affine-transform t)))))

(: pict3d-canvas-proj-transform (-> Positive-Index Positive-Index
                                    Positive-Flonum Positive-Flonum Positive-Flonum
                                    FlTransform3))
(define (pict3d-canvas-proj-transform width height z-near z-far fov)
  (perspective-flt3/viewport (fl width) (fl height) (degrees->radians fov) z-near z-far))

(: pict3d-bitmap-proj-transform (-> Positive-Index Positive-Index
                                    Positive-Flonum Positive-Flonum Positive-Flonum
                                    FlTransform3))
(define (pict3d-bitmap-proj-transform width height z-near z-far fov)
  (flt3compose
   (scale-flt3 +x-y+z-flv3)  ; upside-down: OpenGL origin is lower-left
   (perspective-flt3/viewport (fl width) (fl height) (degrees->radians fov) z-near z-far)))
