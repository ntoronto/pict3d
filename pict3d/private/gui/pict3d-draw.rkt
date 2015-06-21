#lang typed/racket/base

(require racket/list
         "../math.rkt"
         "../engine.rkt"
         "pict3d-struct.rkt"
         "pict3d-combinators.rkt"
         "parameters.rkt"
         "user-types.rkt")

(provide draw-pict3ds)

(: draw-pict3ds (->* [(Listof Pict3D)]
                     [#:width Integer
                      #:height Integer
                      #:camera (U Affine (-> Pict3D Affine))
                      #:z-near Real
                      #:z-far Real
                      #:fov Real
                      #:background RGBA
                      #:ambient Emitted
                      #:bitmap? Any]
                     Void))
(define (draw-pict3ds picts
                      #:width [width (current-pict3d-width)]
                      #:height [height (current-pict3d-height)]
                      #:camera [camera (current-pict3d-auto-camera)]
                      #:z-near [z-near (current-pict3d-z-near)]
                      #:z-far [z-far (current-pict3d-z-far)]
                      #:fov [fov (current-pict3d-fov)]
                      #:background [background (current-pict3d-background)]
                      #:ambient [ambient (current-pict3d-ambient)]
                      #:bitmap? [bitmap? #f])
  (unless (empty? picts)
    (let ([width  (assert (max 1 width) index?)]
          [height  (assert (max 1 height) index?)])
      
      (define view
        (->flaffine3
         (camera->view
          (cond [(affine? camera)  camera]
                [else  (let ([t  (camera-transform (first picts))])
                         (if t t (camera (first picts))))]))))
      
      (define make-proj (if bitmap? bitmap-projective canvas-projective))
      (define proj (make-proj #:width width #:height height #:z-near z-near #:z-far z-far #:fov fov))
      
      (draw-scenes (map pict3d-scene picts) width height view proj background ambient))))
