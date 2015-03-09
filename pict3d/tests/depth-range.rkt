#lang typed/racket

(require pict3d
         math/flonum
         typed/racket/gui
         typed/racket/class)

(current-pict3d-width 512)
(current-pict3d-height 512)

(current-material (material #:ambient 0.1
                            #:diffuse 0.4
                            #:specular 0.5
                            #:roughness 0.1))

(define shapes
  (combine
   (with-color (rgba "crimson")
     (sphere (pos 1/2 0 0) 1/2))
   (with-color (rgba "chartreuse")
     (sphere (pos 0 1/2 0) 1/2))
   (with-color (rgba "dodgerblue")
     (sphere (pos 0 0 1/2) 1/2))))

(define pict
  (combine
   (scale (combine shapes (basis 'camera (point-at (pos 1/2 1 2) (dir -1/2 -1 -1.25))))
          (flexpt 2.0 -10.0))
   (scale (move shapes (dir -1.25 -1.25 -1.25))
          (flexpt 2.0 21.0))))

(current-pict3d-width 512)
(current-pict3d-height 512)
(current-pict3d-add-sunlight? #f)

(combine pict
         (sunlight (dir -0.25 -0.5 -1.0) (emitted "white" 1))
         (sunlight (dir 0.25 0.5 1.0) (emitted "white" 0.5)))
