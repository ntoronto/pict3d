#lang typed/racket

(require pict3d
         plot/typed
         math/flonum
         math/bigfloat)

(current-pict3d-width 512)
(current-pict3d-height 512)

(current-material '(0.1 0.4 0.5 0.1))

(define shapes
  (combine
   (with-color "crimson"
     (sphere '(1/2 0 0) 1/2)
     )
   (with-color "chartreuse"
     (sphere '(0 1/2 0) 1/2)
     )
   (with-color "dodgerblue"
     (sphere '(0 0 1/2) 1/2))
   ))

(define pict
  (combine
   (scale
    (set-basis
     shapes
     'camera
     (normal-basis '(1/2 1 2) '(-1/2 -1 -1.25)))
    (make-list 3 (flexpt 2.0 -19.0)))
   (scale
    (move shapes '(-1.25 -1.25 -1.25))
    (make-list 3 (flexpt 2.0 31.0)))))

pict
