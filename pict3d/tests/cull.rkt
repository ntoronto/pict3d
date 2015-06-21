#lang typed/racket

(require pict3d)

(require math/flonum
         math/base
         pict3d/private/math
         pict3d/private/engine)

(define sphere-vs 
  (for/list : (Listof Pos) ([_  (in-range 5000)])
    (pos (* (- (random) 0.5) 2)
         (* (- (random) 0.5) 2)
         (* (- (random) 0.5) 2))))

(define spheres
  (combine
   (for/list : (Listof Pict3D) ([v  (in-list sphere-vs)])
     (sphere v #i1/16))))

(define blue-spheres
  (with-color (rgba 1/4 1/2 1 3/4)
    (combine
     (for/list : (Listof Pict3D) ([v  (in-list sphere-vs)])
       (sphere v #i1/16)))))

(define wacky-spheres
  (rotate-z (move (scale-y (rotate-x spheres 30) 1.5) (dir -0.25 -0.25 -0.25)) 30))

(define wacky-blue-spheres
  (rotate-z (move (scale-y (rotate-x blue-spheres 30) 1.5) (dir -0.25 -0.25 -0.25)) 30))

(: frustum (-> FlTransform3 Pict3D))
(define (frustum t)
  (match-define (list v1 v2 v4 v3 v5 v6 v8 v7)
    (for*/list : (Listof Pos) ([z  (list -1.0 1.0)]
                               [y  (list -1.0 1.0)]
                               [x  (list -1.0 1.0)])
      (let* ([tinv : FlTransform3  (assert (flt3inverse t) values)]
             [v : FlV3  (flv3 x y z)]
             [v : FlV3  (flt3apply/pos tinv v)])
        (call/flv3-values v pos))))
  
  (combine
   (quad v1 v2 v3 v4)
   (quad v5 v6 v2 v1)
   (quad v6 v7 v3 v2)
   (quad v7 v8 v4 v3)
   (quad v8 v5 v1 v4)
   (quad v8 v7 v6 v5)))

;; ===================================================================================================

(define znear 0.25)
(define zfar 4.0)
(define fov-radians (degrees->radians (fl 30.0)))
(define camera (point-at (pos 1.25 1.25 1.25) origin))
(define view (affine-compose (scale (dir 1 -1 -1))
                             (affine-inverse camera)))

(define t
  (let ([proj : FlTransform3  (perspective-flt3/viewport (fl 800) (fl 600) fov-radians znear zfar)])
    (flt3compose proj view)))

(define t-frustum
  (with-color (rgba 0.75 0 0 0.75)
    (with-emitted (emitted 0.5 0 0)
      (frustum t))))

(combine
 (basis 'camera camera)
 t-frustum
 (frustum-cull spheres t)
 blue-spheres)

(combine
 (basis 'camera camera)
 t-frustum
 (frustum-cull wacky-spheres t)
 wacky-blue-spheres)

(define rect (flrect3 zero-flv3 +x+y+z-flv3))

(combine
 (rect-cull spheres rect)
 (with-color (rgba 0.75 0 0 0.5)
   (with-emitted (emitted 0.25 0 0)
     (rectangle (pos 0 0 0) (pos 1 1 1))))
 blue-spheres)

(combine
 (rect-cull wacky-spheres rect)
 (with-color (rgba 0.75 0 0 0.5)
   (with-emitted (emitted 0.25 0 0)
     (rectangle (pos 0 0 0) (pos 1 1 1))))
 wacky-blue-spheres)
