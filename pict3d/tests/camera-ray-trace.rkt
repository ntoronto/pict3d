#lang typed/racket

(require pict3d)

(printf "starting...~n")

(current-pict3d-width 512)
(current-pict3d-height 512)

(define pict
  (with-color (rgba "salmon" 0.5)
    (scale
     (combine
      (for/list : (Listof Pict3D) ([_  (in-range 2000)])
        (cube (pos (- (random) 0.5) (- (random) 0.5) (- (random) 0.5)) 0.05)))
     5)))

(define t ((current-pict3d-auto-camera) pict))
(define v (affine-origin t))
(define ray-dir (camera-ray-dir t))

(define dvs
  (time
   (for*/list : (Listof Dir) ([x  (in-range 2 512 4)]
                              [y  (in-range 2 512 4)])
     (ray-dir x y))))

(define ps
  (append*
   (time
    (for/list : (Listof (Listof Pos)) ([dv  (in-list dvs)])
      (define p (trace pict v dv))
      (if p (list p) empty)))))

(define pts
  (with-color (rgba "chartreuse")
    (combine
     (for/list : (Listof Pict3D) ([p  (in-list ps)])
       (sphere p 0.025)))))

(combine (freeze pict)
         (freeze pts))
