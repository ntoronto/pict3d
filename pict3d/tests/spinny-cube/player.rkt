#lang typed/racket
 
(require pict3d)

(provide player)

(define mid
  (combine
   (with-material (material #:ambient 0.01 #:diffuse 0.2 #:specular 0.79 #:roughness 0.2)
     (with-color (rgba "DeepSkyBlue")
       (with-emitted (emitted "DeepSkyBlue" 0.25)
         (cube origin 0.9))))
   (group (light origin (emitted "azure" 0.5)) 'light)))

(define face
  (combine
   (group (light (pos 0 0 0.3)) 'light)
   (with-color (rgba "black")
     (with-emitted (emitted "azure" 2)
       (ellipsoid origin (dir 0.75 0.75 1/4))))))

(define bar
  (with-material (material #:ambient 0.01 #:diffuse 0.2 #:specular 0.79 #:roughness 0.2)
    (with-color (rgba "Salmon")
      (with-emitted (emitted "yellow" 0.05)
        (cylinder origin (dir 0.2 0.2 0.8))))))

(define corner
  (combine
   (with-emitted (emitted "yellow" 0.75)
     (deform (tessellate (sphere origin 1/8) #:max-angle (/ 90 5))
       (extend (dir 1/8 1/8 1/8))))
   (group (light origin (emitted "yellow" #i1/4)) 'light)))

(define unscaled-player
  (combine
   (for*/list : (Listof Pict3D) ([dx  (list -1 0 1)]
                                 [dy  (list -1 0 1)]
                                 [dz  (list -1 0 1)])
     (define n (+ (abs dx) (abs dy) (abs dz)))
     (cond
       [(= n 0)  mid]
       [(= n 1)
        (define p face)
        (move
         (cond [(not (= dx 0))  (rotate-y p (* dx 90))]
               [(not (= dy 0))  (rotate-x p (* dy -90))]
               [else  (scale p (dir 1 1 dz))])
         (dir-scale (dir dx dy dz) 0.8))]
       [(= n 2)
        (define p bar)
        (move
         (cond [(= dx 0)  (rotate-y p 90)]
               [(= dy 0)  (rotate-x p 90)]
               [else  p])
         (dir dx dy dz))]
       [else
        (move corner (dir dx dy dz))]))))

(define-values (mn mx) (bounding-rectangle unscaled-player))
(define player
  (freeze (transform unscaled-player
                     (affine-inverse (scale (pos- (assert mx values) (assert mn values)))))))
