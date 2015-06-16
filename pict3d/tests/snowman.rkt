#lang racket

(require pict3d
         pict3d/universe)

(current-material (material #:ambient 0.25 #:diffuse 0.25 #:specular 0.5 #:roughness 0.4))

(define coal-color (rgba 0.5 0.5 0.5))
(define coal-material (material #:diffuse 0.2 #:specular 0.8 #:roughness 0.1))

(define torso
  (let ([torso  (ellipsoid (pos -0.35 -0.35 0.0) (pos 0.35 0.35 0.60))])
    (move-z
     (combine
      (basis 'neck (point-at (surface torso +z) +z))
      (basis 'right-arm (point-at (surface torso (angles->dir -90 20)) (angles->dir -80 30)
                                  #:angle 180))
      (basis 'left-arm (point-at (surface torso (angles->dir 90 10)) (angles->dir 85 45)))
      (freeze
       (combine
        torso
        (move-x
         (with-color coal-color
           (with-material coal-material
             (combine
              (for/list ([ang  (in-list '(-1 1 -1.5 1))]
                         [alt  (in-list '(-15 0 15 30))])
                (sphere (surface torso (angles->dir ang alt)) 0.035)))))
         0.01))))
     -0.2)))

(define base
  (let ([base  (ellipsoid (pos -0.5 -0.5 0) (pos 0.5 0.5 0.85))])
    (move-z
     (combine
      (basis 'hips (point-at (surface base +z) +z))
      (freeze
       (combine
        base
        (move-x
         (with-color coal-color
           (with-material coal-material
             (combine
              (for/list ([ang  (in-list '(-1 1 -1.5))]
                         [alt  (in-list '(40 30 20))])
                (sphere (surface base (angles->dir ang alt)) 0.035)))))
         0.01))))
     -0.15)))

(define nose
  (with-color (rgba "orange")
    (with-material (material #:ambient 0.2 #:diffuse 0.8)
      (cone (pos -0.04 -0.05 -0.02) (pos 0.04 0.05 0.23)))))

(define head
  (let ([head  (sphere origin 0.25)])
    (define eyes
      (move-x
       (let ([cr  (surface head (angles->dir -25 12))]
             [cl  (surface head (angles->dir 25 16))])
         (combine
          ;; Creepy red eye
          (with-color (rgba "red" 0.75)
            (sphere cr 0.05))
          (with-emitted (emitted "red" 10.0)
            (sphere cr 0.035))
          (light cr (emitted "red" 0.02))
          ;; Creepy green eye
          (with-color (rgba "green" 0.75)
            (sphere cl 0.04))
          (with-emitted (emitted "green" 10.0)
            (sphere cl 0.025))
          (light cl (emitted "green" 0.02))))
       0.01))
    
    (define mouth
      (move-x
       (with-color coal-color
         (with-material coal-material
           (combine
            (for/list ([ang  (in-range -45 46 15)])
              (sphere (surface head (angles->dir ang -20)) 0.025)))))
       0.01))
    
    (move-z
     (combine
      (basis 'crown (point-at (surface head (angles->dir -10 80)) (angles->dir -10 75)))
      (basis 'nose (point-at (surface head (angles->dir 0 -3)) (angles->dir 0 10)))
      (freeze (combine head eyes mouth)))
     0.1)))

(define hat
  (freeze
   (move-z
    (with-color (rgba "white")
      (with-material (material #:ambient 0.01 #:diffuse 0.01 #:specular 0.98 #:roughness 0.1)
        (combine
         (cylinder (pos -0.2 -0.2 0.0) (pos 0.2 0.2 0.4))
         (cylinder (pos -0.3 -0.3 -0.01) (pos 0.3 0.3 0.01)))))
    -0.05)))

(define snowman-bottom
  (pin base '(hips) torso))

(define snowman-top
  (weld (weld head '(crown) hat) '(nose) nose))

(define arm-segment
  (combine
   (move-z (scale (basis 'top (point-at origin (dir 0.25 0 1) #:angle 30)) 0.5) 0.35)
   (move-z (scale (basis 'top (point-at origin (dir -0.1 0 1) #:angle 14)) 0.4) 0.3)
   (with-color (rgba "orange")
     (with-material (material #:ambient 0.1 #:diffuse 0.4 #:specular 0.5 #:roughness 0.2)
       (cone (pos -0.03 -0.03 0.0) (pos 0.03 0.03 0.5))))))

(define arm
  (freeze (weld* arm-segment '(top) arm-segment)))

(define (snowman t)
  (let* ([snowman  (scale-z snowman-bottom (+ 1.0 (* 0.01 (sin (/ t 100.0)))))]
         [snowman  (replace-group snowman '(neck)
                                  (λ (p) (rotate-z p (* 10 (sin (/ (+ t 500.0) 1000.0))))))]
         [snowman  (pin snowman '(neck) snowman-top)]
         [snowman  (pin snowman '(left-arm) arm)]
         [snowman  (pin snowman '(right-arm) arm)]
         [snowman  (replace-group snowman '(hips)
                                  (λ (p) (rotate-z p (* 5 (sin (/ t 1000.0))))))])
    snowman))

(big-bang3d
 #f
 #:on-draw
 (λ (s n t)
   (combine
    (basis 'camera (point-at (pos 0.6 -1 1.25) (pos 0 0 1)))
    (sunlight (dir 0 0 -1) (emitted "azure" 1))
    (light (pos -0.5 -4.0 2.0) (emitted "gold" 5))
    (snowman t))))
