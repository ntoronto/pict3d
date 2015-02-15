#lang typed/racket

(require pict3d
         math/flonum
         racket/gui
         racket/class)

(current-pict3d-width 512)
(current-pict3d-height 512)

(current-material '(0.1 0.4 0.5 0.1))

(define shapes
  (combine
   (with-color "crimson"
     (sphere '(1/2 0 0) 1/2))
   (with-color "chartreuse"
     (sphere '(0 1/2 0) 1/2))
   (with-color "dodgerblue"
     (sphere '(0 0 1/2) 1/2))))

(define pict
  (combine
   (scale (combine shapes (basis 'camera (point-at '(1/2 1 2) '(-1/2 -1 -1.25))))
          (make-list 3 (flexpt 2.0 -10.0)))
   (scale (move shapes '(-1.25 -1.25 -1.25))
          (make-list 3 (flexpt 2.0 21.0)))))

(pict3d->bitmap
 (combine pict
          (sunlight (flvector -0.25 -0.5 -1.0) "white" 1)
          (sunlight (flvector 0.25 0.5 1.0) "white" 0.5))
 512 512)
