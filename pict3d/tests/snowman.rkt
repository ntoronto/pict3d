#lang racket

(require pict3d
         pict3d/universe)

(current-material (make-material 0.25 0.25 0.5 0.4))

(define torso
  (move-z
   (combine
    (basis 'neck (point-at #:from '(0 0 0.3) #:dir '(0 0 1)))
    (basis 'right-arm (point-at #:from '(0 -0.3 0.1) #:dir '(0.2 -0.3 0.3)))
    (basis 'left-arm (point-at #:from '(0 0.3 0.1) #:dir '(0.2 0.3 0.3)))
    (freeze
     (scale
      (combine
       (sphere '(0 0 0) 1/2)
       (with-color '(0.5 0.5 0.5)
         (with-material (make-material 0.0 0.2 0.8 0.1)
           (combine*
            (for/list ([t  (in-list '(-17 0 14 31))]
                       [ofs  (in-list '(0.01 -0.02 0.01 -0.03))])
              (define c (cos (degrees->radians t)))
              (define s (sin (degrees->radians t)))
              (sphere (list (* c 0.5) ofs (* s 0.5)) 0.07)))))
       )
      '(0.7 0.7 0.6))))
   0.1))

(define base
  (combine
   (basis 'hips (point-at #:from '(0 0 0.7) #:dir '(0 0 1)))
   (ellipsoid '(-0.5 -0.5 -0.15) '(0.5 0.5 0.7))
   (with-color '(0.5 0.5 0.5)
     (with-material (make-material 0.0 0.2 0.8 0.1)
       (sphere '(0.3 -0.05 0.6) 0.05)))
   ))

(define nose
  (with-color "orange"
    (with-material (make-material 0.2 0.8 0.0 0.1)
      (cone '(-0.08 -0.1 0.0) '(0.08 0.1 0.5) #:segments 12 #:smooth? #t))))

(define head
  (move-z
   (combine
    (basis 'crown (point-at #:from '(0.05 0.02 0.2) #:dir '(0.3 0.05 1)))
    (freeze
     (scale
      (combine
       (sphere '(0 0 0) 1/2)
       (with-color '(0.5 0.5 0.5)
         (with-material (make-material 0.0 0.2 0.8 0.1)
           (combine*
            (for/list ([t  (in-list '(-32 -15 2 17 30))]
                       [a  (in-list '(-17 -11 -13 -11 -15))])
              (define c (cos (degrees->radians t)))
              (define s (sin (degrees->radians t)))
              (define p (cos (degrees->radians a)))
              (define r (sin (degrees->radians a)))
              (sphere (list (* 0.5 p c) (* 0.5 p s) (* 0.5 r)) 0.05)))))
       (group
        (with-color "white"
          (with-material (make-material 0.0 0.0 1.0 0.1)
            (let ([cr  '(0.44 -0.2 0.12)]
                  [cl  '(0.43 0.2 0.14)])
              (combine
               ;; Creepy red eye
               (with-color '(1 0.25 0.0 0.75)
                 (sphere cr 0.10))
               (with-emitted '(1 0.25 0.0 10.0)
                 (sphere cr 0.07))
               (light cr '(1 0.25 0.0) 0.02)
               ;; Creepy green eye
               (with-color '(0 1 0.25 0.75)
                 (sphere cl 0.08))
               (with-emitted '(0 1 0.25 10.0)
                 (sphere cl 0.05))
               (light cl '(0 1 0.25) 0.02)))))
        'eyes)
       (move-x (rotate-y nose 80) 0.45))
      0.5)))
   0.1))

(define hat
  (freeze
   (with-color '(1 1 1)
     (with-material (make-material 0.01 0.01 0.98 0.1)
       (combine
        (cylinder '(-0.2 -0.2 0.0) '(0.2 0.2 0.4) #:segments 16)
        (cylinder '(-0.3 -0.3 -0.01) '(0.3 0.3 0.01) #:segments 16))))))

(define snowman-bottom
  (pin base 'hips torso))

(define snowman-top
  (pin head 'crown hat))

(define arm-segment
  (combine
   (move-z (scale (basis 'top (point-at #:from origin #:dir '(0.25 0 1) #:angle 30)) 0.5) 0.35)
   (move-z (scale (basis 'top (point-at #:from origin #:dir '(-0.1 0 1) #:angle 14)) 0.4) 0.3)
   (with-color "orange"
     (with-material (make-material 0.1 0.4 0.5 0.2)
       (cone '(-0.03 -0.03 0.0) '(0.03 0.03 0.5) #:segments 32 #:smooth? #t)))))

(define arm
  (freeze (weld arm-segment 'top arm-segment)))

(move arm '(0.5 0.5 0.0))


(big-bang3d
 0.0
 #:on-frame
 (λ (p n t)
   t)
 #:on-draw
 (λ (t)
   (combine
    (basis 'camera (point-at #:from '(0.6 -1 1.25) #:to '(0 0 1)))
    (sunlight '(0 0 -1) "azure" 1)
    (light (list -0.5 -4.0 2.0) "gold" 5)
    (let* ([snowman  (scale-z snowman-bottom (+ 1.0 (* 0.01 (sin (/ t 100.0)))))]
           [snowman  (replace-group snowman 'neck
                                    (λ (p) (rotate-z p (* 10 (sin (/ (+ t 500.0) 1000.0))))))]
           [snowman  (pin snowman 'neck snowman-top)]
           [snowman  (pin snowman 'left-arm arm)]
           [snowman  (pin snowman 'right-arm arm)]
           [snowman  (replace-group snowman 'hips
                                    (λ (p) (rotate-z p (* 5 (sin (/ t 1000.0))))))])
      snowman))))
