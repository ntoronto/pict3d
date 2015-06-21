#lang racket

(require pict3d
         pict3d/engine
         images/flomap)

(current-engine-debug-shapes '(triangle-mesh))

(define t (point-at (pos 1 0 1) +x))

(define b
  (let ([b  (basis 'side t)])
    (pin b '(side) (cube (pos 0 0 1) 1))))

(combine b (basis 'camera (point-at (pos -1 -1 1.5) origin)))

(combine (adaptive-deform b (twist 30))
         (basis 'camera (point-at (pos -1 -1 1.5) origin)))

(define p
  (let ([p  (pipe (pos 0 0 0) 1 #:top-radii (interval 0 0.1) #:bottom-radii unit-interval
                  #:inside? #f)])
    (define-values (v n) (surface/normal p (angles->dir 0 45)))
    (rotate-x (combine p (basis 'side (point-at v n))) 30)))

(combine p (basis 'camera (point-at
                           (pos 0.09094614884673538 -0.8542693331685399 1.1253311917175866)
                           (dir 0.03270156461460315 0.4989294616121821 -0.8660254037885581))))

(combine (adaptive-deform p (smooth-compose (smooth (λ (x) x)) (move-x 1)))
         (basis 'camera (point-at (pos 1.5 1.5 1.5) origin)))

(adaptive-deform
 (pin (combine (cylinder (pos 1 0 0) 1/2)
                (basis 'bob (point-at (pos 1.5 0 0) +x)))
       '(bob)
       (sphere (pos 0 0 1/2) 1/2))
 (smooth
  (λ (v)
    (match-define (pos x y z) v)
    (pos (* x z) y z))))

(define segment
  (let ([s  (ellipsoid (pos 0 0 1) (dir 1/2 1/2 1))])
    (define-values (v n) (surface/normal s (dir 0.1 0 1)))
    (combine s (basis 'top (point-at v n)))))

(define segments
  (pin
   (pin (pin segment '(top) segment)
        '(top top)
        segment)
   '(top top top)
   segment))

(printf "The following two Pict3Ds should look the same~n")

(combine
 (adaptive-deform segments (smooth-compose (affine +x +y (dir 0.25 0 1) origin)
                                           (smooth (λ (v) v))))
 (basis 'camera (point-at (pos 0 4.5 2.5) (dir 0 -1.0 0.15))))

(combine
 (transform segments (affine +x +y (dir 0.25 0 1) origin))
 (basis 'camera (point-at (pos 0 4.5 2.5) (dir 0 -1.0 0.15))))

;(current-adaptive-max-iters 5)

(printf "The following two Pict3Ds should look the same, except that the first has groups~n")

(define squig
  (time
   (combine
    (adaptive-deform
     segments
     (smooth
      (λ (v)
        (match-define (pos x y z) v)
        (pos (+ x (sin (* 3 z))) y z))))
    (basis 'camera (point-at (pos 0 4.5 2.5) (dir 0 -1.0 0.15))))))
squig

(time
 (combine
  (adaptive-deform
   (ungroup (ungroup (ungroup (ungroup segments '(top)) '(top)) '(top)) '(top))
   (smooth
    (λ (v)
      (match-define (pos x y z) v)
      (pos (+ x (sin (* 3 z))) y z))))
  (basis 'camera (point-at (pos 0 4.5 2.5) (dir 0 -1.0 0.15)))))

#;
(begin
  (require pict3d/universe)
  (big-bang3d
   0
   #:on-draw
   (λ (s n t)
     (combine (replace-group squig '(top top) (λ (p) (rotate-z p (/ t 13))))
              (light (pos -2 0 3) (emitted "white" 2))))))
