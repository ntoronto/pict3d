#lang typed/racket

(require typed/racket/gui
         typed/racket/class
         pict3d)

(require math/flonum
         math/base
         pict3d/private/math/flv3
         pict3d/private/math/flt3
         pict3d/private/math/flrect3
         pict3d/private/engine/draw-passes
         pict3d/private/engine/scene)

(define sphere-vs 
  (for/list : (Listof (Listof Flonum)) ([_  (in-range 5000)])
    (list (* (- (random) 0.5) 2)
          (* (- (random) 0.5) 2)
          (* (- (random) 0.5) 2))))

(define spheres
  (combine*
   (for/list ([v  (in-list sphere-vs)])
     (sphere v #i1/16))))

(define blue-spheres
  (with-color '(1/4 1/2 1 1/4)
    (combine*
     (for/list ([v  (in-list sphere-vs)])
       (sphere v #i1/16)))))

(define wacky-spheres
  (rotate-z (move (scale-y (rotate-x spheres 30) 1.5) '(-0.25 -0.25 -0.25)) 30))

(define wacky-blue-spheres
  (rotate-z (move (scale-y (rotate-x blue-spheres 30) 1.5) '(-0.25 -0.25 -0.25)) 30))

(: do-culls (-> Pict3D (Listof Pict3D)))
(define (do-culls spheres)
  (list
   spheres
   
   (combine
    (plane-cull spheres
                (assert (flplane3 (flvector 1.0 0.0 0.0) -0.5)))
    (with-color '(1 0 0 0.5)
      (rectangle '(0.5 -2 -2) '(2 2 2))))
   
   (combine
    (plane-cull spheres
                (assert (flplane3 (flvector 0.0 1.0 0.0) -0.5)))
    (with-color '(1 0 0 0.5)
      (rectangle '(-2 0.5 -2) '(2 2 2))))
   ))

(: frustum (-> FlTransform3 Pict3D))
(define (frustum t)
  (define tinv (flt3inverse t))
  (match-define (list v1 v2 v4 v3 v5 v6 v8 v7)
    (for*/list : (Listof FlVector) ([z  (list -1.0 1.0)]
                                    [y  (list -1.0 1.0)]
                                    [x  (list -1.0 1.0)])
      (flt3apply/pos tinv (flvector x y z))))
  
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
(define camera-basis (normal-basis '(1.25 1.25 1.25) '(-1 -1 -1)))
(define proj (perspective-flt3/viewport (fl 800) (fl 600) fov-radians znear zfar))
(define view (flt3compose (scale-flt3 (flvector 1.0 -1.0 -1.0)) (basis-inverse camera-basis)))
(define t (flt3compose proj view))

(define t-frustum
  (with-color '(0.75 0 0 0.5)
    (with-emitted '(0.25 0 0)
      (frustum t))))

(combine
 (set-basis t-frustum camera-basis)
 (frustum-cull spheres t)
 blue-spheres)

(combine
 (set-basis t-frustum camera-basis)
 (frustum-cull wacky-spheres t)
 wacky-blue-spheres)

(combine
 (rect-cull
  spheres
  (flrect3 (flvector 0.0 0.0 0.0)
           (flvector 1.0 1.0 1.0)))
 (with-color '(0.75 0 0 0.5)
   (with-emitted '(0.25 0 0)
     (rectangle '(0 0 0) '(1 1 1))))
 blue-spheres)

(combine
 (rect-cull
  wacky-spheres
  (flrect3 (flvector 0.0 0.0 0.0)
           (flvector 1.0 1.0 1.0)))
 (with-color '(0.75 0 0 0.5)
   (with-emitted '(0.25 0 0)
     (rectangle '(0 0 0) '(1 1 1))))
 wacky-blue-spheres)
