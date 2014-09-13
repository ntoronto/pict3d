#lang typed/racket

(require pict3d
         plot/typed
         math/flonum
         math/bigfloat)

;(current-z-far 1e38)

(define shapes
  (combine
   (with-color "lightblue"
     (rectangle '(-1 -1 -1) '(1 1 1))
     ;(sphere '(0 0 0) 1)
     )
   (with-color "pink"
     (rectangle '(-1.8 -1.8 -0.5) '(0.2 0.2 1.5))
     ;(sphere '(-0.8 -0.8 0.5) 1)
     )))

(define pict
  (combine
   (scale
    (set-basis
     (combine
      (sunlight '(-1 -1/2 -1/4) "azure" 1)
      ;(light '(2 1.5 1.5) "white" 5)
      shapes)
     'camera
     (normal-basis '(1.25 1.25 2) '(-1 -1 -1)))
    (make-list 3 (flexpt 2.0 0.0)))
   (scale
    (move shapes '(-1.25 -1.25 -1.25))
    (make-list 3 (flexpt 2.0 2.0)))))

(pict3d->bitmap pict 512 512)
