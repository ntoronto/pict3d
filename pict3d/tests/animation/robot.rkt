#lang typed/racket

(require pict3d
         pict3d/universe
         "humanoid-rig.rkt")

(define rig (humanoid-rig #:neck #i1/16 #:chest #i6/16))

(define fat-rig
  (humanoid-rig
   #:neck 1/32
   #:upper-leg #i1/4
   #:lower-leg #i1/4
   #:upper-arm #i5/32
   #:lower-arm #i5/32
   #:chest #i3/16
   #:abdomen #i7/16
   #:hips #i1/8
   #:hand #i1/8
   #:foot #i5/32))

(define tall-rig
  (humanoid-rig
   #:neck #i2/16
   #:chest #i12/16
   #:abdomen #i10/16
   #:upper-leg #i1
   #:lower-leg #i1
   #:upper-arm #i10/16
   #:lower-arm #i10/16
   #:hand #i1/2
   #:foot #i10/16))

(define joint-color (rgba 10/16 9/16 8/16))
(define joint-material (material #:ambient 0.01 #:diffuse 3/4 #:specular 1/4 #:roughness 0.3))
(define shiny-material (material #:ambient 0.01 #:diffuse 1/2 #:specular 1/2 #:roughness 0.1))
(define limb-material (material #:ambient 0.01 #:diffuse 3/4 #:specular 1 #:roughness 0.4))

(: make-robot (-> Pict3D Boolean Boolean Pict3D))
(define (make-robot rig fat? tall?)
  (define pelvis
    (with-color (rgba 2/4 3/4 1)
      (with-material limb-material
        (rotate-x (cylinder origin (dir 6/16 6/16 1/2)) 90))))
  
  (define upper-leg
    (let ([s  (if tall? 1/2 1)])
      (combine
       (with-color joint-color
         (with-material joint-material
           (sphere (pos 0 0 1/2) (* s 7/32))))
       (with-color (rgba 1/4 2/4 1)
         (with-material limb-material
           (cylinder origin (dir (* s 1/4) (* s 1/4) (- 1/2 1/16))))))))
  
  (define lower-leg
    (let ([s  (if tall? 1/2 1)])
      (combine
       (with-color joint-color
         (with-material joint-material
           (combine
            (sphere (pos 0 0 1/2) (* s 6/32))
            (sphere (pos 0 0 -1/2) (* s 5/32)))))
       (with-color (rgba 2/4 3/4 1)
         (with-material limb-material
           (cylinder origin (dir (* s 7/32) (* s 7/32) (- 1/2 1/16))))))))
  
  (define foot
    (let ([s  (if tall? 1/2 1)])
      (with-color (rgba 1 1/4 1/8)
        (with-material limb-material
          (rectangle (pos 0 0 (* s -1/8)) (dir 1/2 (* s 1/4) (* s 1/8)))))))
  
  (define collar
    (combine
     (with-color (rgba 1 1/2 1/4)
       (with-material limb-material
         (rotate-x (cylinder origin (dir 3/32 3/32 1/2)) 90)))
     (with-color joint-color
       (with-material joint-material
         (sphere origin 4/32)))))
  
  (define upper-arm
    (let ([s  (if tall? 1/2 1)])
      (combine
       (with-color joint-color
         (with-material joint-material
           (sphere (pos 0 0 1/2) (* s 7/32))))
       (with-color (rgba 1 1/4 1/8)
         (with-material limb-material
           (cylinder origin (dir (* s 1/4) (* s 1/4) (- 1/2 1/16))))))))
  
  (define lower-arm
    (let ([s  (if tall? 1/2 1)])
      (combine
       (with-color joint-color
         (with-material joint-material
           (combine
            (sphere (pos 0 0 1/2) (* s 6/32))
            (sphere (pos 0 0 -1/2) (* s 5/32)))))
       (with-color (rgba 1 1/2 1/4)
         (with-material limb-material
           (cylinder origin (dir (* s 7/32) (* s 7/32) (- 1/2 1/16))))))))
  
  (define hand
    (let ([s  (if tall? 1/2 1)])
      (with-color (rgba 1 3/4 1/4)
        (with-material limb-material
          (rectangle (pos 0 (* s 1/8) 0) (dir (* s 1/4) (* s 1/8) 1/2))))))
  
  (define neck
    (let ([s  (if tall? 1/2 1)])
      (with-color (rgba 1 3/4 1/4)
        (with-material limb-material
          (cylinder origin (dir (* s 5/8) (* s 5/8) 5/8))))))
  
  (define head
    (let ([top  (with-color (rgba "white")
                  (with-material shiny-material
                    (combine
                     (ellipsoid origin (dir 3/4 3/4 1/2)))))])
      (combine
       top
       (with-color (rgba 1/4 2/4 1)
         (with-material limb-material
           (let-values ([(v n)  (surface/normal top (angles->dir +30 0))])
             (match-define (pos x y z) v)
             (match-define (dir nx ny nz) n)
             (define v1 (assert v values))
             (define n1 (assert n values))
             (define v2 (pos x (- y) z))
             (define n2 (dir nx (- ny) nz))
             (combine
              (group
               (combine
                (sphere (pos+ v1 n1 -1/8) 1/5)
                (sphere (pos+ v2 n2 -1/8) 1/5))
               'eyes)
              (group
               (set-emitted
                (combine
                 (light (pos+ v1 n1 1/3) (emitted 1 1/16))
                 (light (pos+ v2 n2 1/3) (emitted 1 1/16)))
                (emitted 1 0.000001))
               'lights)))))
       )))
  
  (define back
    (with-color joint-color
      (with-material joint-material
        (sphere origin 1/8))))
  
  (define chest
    (let ([s  (if tall? 1/2 1)])
      (with-color (rgba "white")
        (with-material shiny-material
          (ellipsoid (pos 0 0 2/32) (dir (* s 10/32) (* s 16/32) 18/32))))))
  
  (define abdomen
    (let ([s  (if tall? 1/2 1)])
      (combine
       (with-color joint-color
         (with-material joint-material
           (sphere (pos 0 0 1/2) (* s 9/32))))
       (with-color (rgba "white")
         (with-material shiny-material
           (scale-z (cone (pos 0 0 1/8) (dir (* s 5/16) (* s 5/16) (+ 1/2 1/8))) -1))))))
  
  (define fat-chest
    (with-color (rgba "white")
      (with-material shiny-material
        (ellipsoid origin (dir 3/4 1 1/2)))))
  
  (define fat-abdomen
    (with-color (rgba "white")
       (with-material shiny-material
         (ellipsoid origin (dir 1/2 10/16 10/16)))))
  
  (let* ([pict  rig]
         [pict  (pin pict '(head) head)]
         [pict  (pin pict '(neck) neck)]
         [pict  (pin pict '(collar) collar)]
         [pict  (pin pict '(upper-arm) upper-arm)]
         [pict  (pin pict '(lower-arm) lower-arm)]
         [pict  (pin pict '(hand) hand)]
         [pict  (pin pict '(pelvis) pelvis)]
         [pict  (pin pict '(upper-leg) upper-leg)]
         [pict  (pin pict '(lower-leg) lower-leg)]
         [pict  (pin pict '(foot) foot)]
         )
    (cond [fat?  (let* ([pict  (pin pict '(chest) fat-chest)]
                        [pict  (pin pict '(abdomen) fat-abdomen)])
                   pict)]
          [else  (let* ([pict  (pin pict '(chest) chest)]
                        [pict  (pin pict '(abdomen) abdomen)])
                   pict)])))

(define robot (make-robot rig #f #f))
(define fat-robot (make-robot fat-rig #t #f))
(define tall-robot (make-robot tall-rig #f #t))

robot
fat-robot
tall-robot

(transform-inside robot (stand-lower* rig #:crouch 30 #:spread 30))

(define evil-robot
  (replace-group
   (replace-group robot '(lights) (λ (p) (set-emitted p (emitted 1 1/4 1/8 1/16))))
   '(eyes)
   (λ (p)
     (set-color
      (set-emitted p (emitted 1 1/4 1/8 4))
      (rgba 0)))))

((inst big-bang3d Any)
 0
 #:frame-delay #i1000/60
 #:width 1024
 #:height 768
 #:on-draw
 (λ ([s : Any] [n : Natural] [t : Flonum])
   (combine
    (rectangle (pos -2 -2 -1) (pos 2 2 -1/16))
    (sunlight (dir -1 1 -2) (emitted "azure" 2))
    (sunlight (dir 1 -1 2) (emitted "oldlace" 1/4))
    (basis 'camera (point-at (pos 2.5 1 1) (pos 0 0 1)))
    
    (transform-inside
     fat-robot
     (compose-inner
      (stand-upper #:crouch (* 90 0.5 (+ 1 (sin (/ t 200))))
                   #:spread (* 90 0.5 (+ 1 (sin (/ t 237)))))
      (stand-lower* fat-rig
                    #:crouch (* 90 0.5 (+ 1 (sin (/ t 200))))
                    #:spread (* 90 0.5 (+ 1 (sin (/ t 237)))))
      (move (dir -0.5 1.25 0))))
    
    (transform-inside
     tall-robot
     (compose-inner
      (walk-upper (/ t 1000) #:freq 1/2 #:lean 0 #:swing 1)
      (walk-lower* tall-rig (/ t 1000) #:freq 1/2 #:lean 0 #:swing 1)
      (stand-upper #:crouch 0 #:spread 30)
      (stand-lower* tall-rig #:crouch 0 #:spread 30)
      (move (dir 0.5 -1.25 0))))
    
    (transform-inside
     robot
     (compose-inner
      ;(walk-upper (/ t 1000) #:freq 1/2 #:lean 90 #:swing 1)
      ;(walk-lower* robot-rig (/ t 1000) #:freq 1/2 #:lean 90 #:swing 1)
      (run-upper (/ t 1000) #:freq 1 #:lean 90 #:sprint 1)
      (run-lower* rig (/ t 1000) #:freq 1 #:lean 90 #:sprint 1)
      ;(run-spine robot-rig (run-lower (/ t 1000) #:freq 1 #:lean 20 #:sprint 0))
      (stand-upper #:crouch 0 #:spread 30)
      (stand-lower* rig #:crouch 0 #:spread 10)
      #;
      (stand-upper #:crouch (* 90 0.5 (+ 1 (sin (/ t 500))))
                   #:spread 30)
      #;
      (stand-upper #:crouch (* 90 0.5 (+ 1 (sin (/ t 200))))
                   #:spread (* 90 0.5 (+ 1 (sin (/ t 237)))))
      #;
      (stand-lower* robot-rig
                    #:crouch (* 90 0.5 (+ 1 (sin (/ t 200))))
                    #:spread (* 90 0.5 (+ 1 (sin (/ t 237))))))))))

(pict3d->bitmap
 (combine
  (basis 'camera (point-at (pos 1.5 0 1) (pos 0 0 1)))
  (transform-inside evil-robot (stand rig #:crouch 45 #:spread 45)))
 512 512)
