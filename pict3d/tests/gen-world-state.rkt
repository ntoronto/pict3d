#lang racket

(require pict3d
         pict3d/universe)

(define lights+camera
  (combine (light (pos 0 1 2) (emitted "Thistle" 3))
           (light (pos 0 -1 -2) (emitted "PowderBlue" 3))
           (basis 'camera (point-at (pos 1.5 0 0) origin))))

(define blades
  (scale
   (move-z
    (combine (cylinder origin (dir 1/8 1/8 1/8))
             (ellipsoid (pos 0 0 1/8) (dir 1 1/8 1/16))
             (ellipsoid (pos 0 0 1/8) (dir 1/8 1 1/16)))
    1/8)
   1/8))

(define spiky
  (let ([ball  (with-emitted (emitted "green" 1)
                 (combine (sphere origin 1/2)
                          (light origin (emitted "green" 0.1))))])
    (combine
     ball
     (cylinder origin (dir 1/32 1/32 1))
     (for*/list ([ρ  (in-range -81 82 18)]
                 [s  (in-value (exact-ceiling (/ 18 (cos (degrees->radians ρ)))))]
                 [θ  (in-range -180 180 s)])
       (define data (surface/data ball (angles->dir θ ρ)))
       (basis 'shaft (point-at (surface-data-pos data) (surface-data-normal data)))))))

(struct cube-state ()
  #:methods gen:world-state
  [(define (world-state-draw s n t)
     (combine (rotate-z (rotate-y (rotate-x (cube origin 1/2)
                                            (/ t 11))
                                  (/ t 13))
                        (/ t 17))
              lights+camera))
   (define (world-state-release s n t k)
     (ball-state (camera-transform lights+camera) (dir 0 0 0) (dir 0 0 0) t (set) (cons 0 0) 0 0))])

(struct ball-state (tr dv da last-t keys mouse yaw pitch)
  #:methods gen:world-state
  [(define (world-state-draw s n t)
     (combine (rectangle (pos -2 -2 -2) (pos 2 2 -1))
              (pin* spiky '(shaft) (rotate-z blades (/ t 5)))
              (sunlight (dir 0 -1/2 -1))
              (basis 'camera (ball-state-tr s))))
   
   (define (world-state-frame s n t)
     (match-define (ball-state tr dv da last-t keys mouse yaw pitch) s)
     (define frame-delay (max 1.0 (min 100.0 (- t last-t))))
     (define dt (/ frame-delay 1000.0))
     
     (define ddv
       (let* ([ddv  (dir 0 0 0)]
              [ddv  (if (set-member? keys "up")     (dir+ ddv +z) ddv)]
              [ddv  (if (set-member? keys "down")   (dir+ ddv -z) ddv)]
              [ddv  (if (set-member? keys "right")  (dir+ ddv +x) ddv)]
              [ddv  (if (set-member? keys "left")   (dir+ ddv -x) ddv)]
              [ddv  (if (set-member? keys "next")   (dir+ ddv +y) ddv)]
              [ddv  (if (set-member? keys "prior")  (dir+ ddv -y) ddv)]
              [ddv  (dir-scale ddv 25)]
              [ddv  (transform-dir ddv tr)]
              [ddv  (dir+ ddv (dir-scale dv -10))])
         ddv))
     
     (define dda
       (let* ([dda  (dir+ (dir-scale -x pitch)
                          (dir-scale +y yaw))]
              [dda  (if (set-member? keys "j") (dir+ dda (dir-scale -y 25)) dda)]
              [dda  (if (set-member? keys "l") (dir+ dda (dir-scale +y 25)) dda)]
              [dda  (if (set-member? keys "i") (dir+ dda (dir-scale +x 25)) dda)]
              [dda  (if (set-member? keys "k") (dir+ dda (dir-scale -x 25)) dda)]
              [dda  (if (set-member? keys "u") (dir+ dda (dir-scale -z 25)) dda)]
              [dda  (if (set-member? keys "o") (dir+ dda (dir-scale +z 25)) dda)]
              [dda  (dir-scale dda 25)]
              [dda  (dir+ dda (dir-scale da -10))])
         dda))
     
     (define move-tr
       (move (dir+ (dir-scale dv dt)
                   (dir-scale ddv (* 0.5 dt dt)))))
     
     (define rotate-tr
       (let* ([d  (dir+ (dir-scale da dt)
                        (dir-scale dda (* 0.5 dt dt)))]
              [m  (dir-dist d)])
         (if (< m 1e-16)
             identity-affine
             (rotate (dir-scale d (/ m)) (radians->degrees (* dt m))))))
     
     (define new-tr (affine-compose move-tr tr rotate-tr))
     (define new-dv (dir+ dv (dir-scale ddv dt)))
     (define new-da (dir+ da (dir-scale dda dt)))
     
     (ball-state new-tr new-dv new-da t keys mouse 0 0))
   
   (define (world-state-key s n t k)
     (match-define (ball-state tr dv da last-t keys mouse yaw pitch) s)
     (ball-state tr dv da last-t (set-add keys k) mouse yaw pitch))
   
   (define (world-state-release s n t k)
     (match-define (ball-state tr dv da last-t keys mouse yaw pitch) s)
     (if (equal? k " ")
         (done-state)
         (ball-state tr dv da last-t (set-remove keys k) mouse yaw pitch)))
   
   (define (world-state-mouse s n t x y e)
     (match-define (ball-state tr dv da last-t keys (cons last-x last-y) yaw pitch) s)
     (if (equal? e "drag")
         (ball-state tr dv da last-t keys (cons x y) (+ yaw (- x last-x)) (+ pitch (- y last-y)))
         (ball-state tr dv da last-t keys (cons x y) yaw pitch)))
   ])

(struct done-state ()
  #:methods gen:world-state
  [(define (world-state-stop? s n t) #t)
   (define (world-state-draw s n t)
     (combine (sphere origin 1/2)
              (light (pos 0 1 1) (emitted "azure" 2))))])

(world-state-big-bang3d (cube-state))
