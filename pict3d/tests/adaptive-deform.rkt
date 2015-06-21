#lang racket

(require pict3d
         math
         profile)

(current-pict3d-add-wireframe 'color)

(define t
  (affine-compose
   (move (dir 1 1 1))
   (rotate-z 10)
   (rotate-x 10)
   (rotate-y 10)
   (scale (dir -1 1/2 0.1))))

(define sphere-pict (sphere origin 3/4 #:inside? #f))

(deform sphere-pict t)
(transform sphere-pict t)
(time (adaptive-deform sphere-pict t))
(time (adaptive-deform (group sphere-pict 'thing) t))
(time (adaptive-deform (freeze sphere-pict) t))
(time (adaptive-deform (freeze (group sphere-pict 'thing)) t))
(time (adaptive-deform (group (freeze sphere-pict) 'thing) t))

#;; Quadratic stretch
(define f
  (differentiable
   (match-lambda
     [(pos x y z)
      (pos (+ x (* (sqr x) (sgn x))) y z)])
   (match-lambda
     [(pos x y z)
      (values (dir (+ 1 (* 2 x (sgn x))) 0 0)
              (dir 0 1 0)
              (dir 0 0 1))])))

#;; Simplest inconsistent transformation
(define f
  #;
  (scale (dir -1 1 1))
  
  (differentiable
   (λ (v)
     (match-define (pos x y z) v)
     (pos (- x) y z))
   (λ (v)
     (match-define (pos x y z) v)
     (values (dir -1 0 0)
             (dir 0 1 0)
             (dir 0 0 1)))))

(define rot-t
  (point-at origin (dir 0.125 0.125 1)))

;; Off-axis twist
(define f (smooth-compose rot-t (twist 45) (affine-inverse rot-t)))

#;; Wrap around the y axis (which is a discontinuous line)
(define f
  (smooth
   (λ (v)
     (match-define (pos x y z) v)
     (pos (* (+ 2.0 z) (sin (* 0.5 pi x)))
          y
          (* (+ 2.0 z) (cos (* 0.5 pi x)))))))

(time (adaptive-deform (cone origin (dir 1 1 1)) f))
(time (adaptive-deform
       (combine (rectangle origin (dir 1 1 1))
                (light (pos 1 0 2) (emitted "white" 0.5))
                (light (pos 0 0 2) (emitted "blue" 0.5))
                (light (pos -1 0 2) (emitted "red" 0.5)))
       f))
(time (adaptive-deform (quad (pos 1 0 0) (pos 1 1 0) (pos 0 1 0) (pos 0 0 1)) f))
(time (adaptive-deform (cube origin 1) identity-affine))
(time (adaptive-deform (cylinder origin 1) identity-affine))

(sleep 2)
(profile
 (for/last ([_  (in-range 1)])
   (adaptive-deform
    (combine (ellipsoid (pos 1 0 0.1) (dir 1/2 1/2 4))
             (cone (pos 0 1 0) (dir 1/4 1/4 2))
             (rectangle (pos -1 0 0) (dir 1/8 1/8 3))
             (cylinder (pos 0 -1 0) (dir 3/4 3/4 1))
             (rectangle origin (dir 1/4 1/4 2))
             )
    f)))

(define (change-norm k l)
  (smooth
   (λ (v)
     (match-define (pos x y z) v)
     (define numer
       (flexpt (+ (flexpt (abs x) k)
                  (flexpt (abs y) k)
                  (flexpt (abs z) k))
               (/ k)))
     (define denom
       (flexpt (+ (flexpt (abs x) l)
                  (flexpt (abs y) l)
                  (flexpt (abs z) l))
               (/ l)))
     (define s
       (if (= denom 0.0) 1.0 (/ numer denom)))
     (pos (* s x) (* s y) (* s z)))))

(time (adaptive-deform (ellipsoid origin 1) (change-norm 6.0 2.0)))
(time (adaptive-deform (ellipsoid origin 1) (change-norm 2.0 6.0)))
(time (adaptive-deform (rectangle origin 1) (change-norm 12.0 2.0)))
(time (adaptive-deform (rectangle origin 1) (change-norm 2.0 6.0)))

(time (adaptive-deform
       (rectangle origin (dir 4 4 1/2))
       (displace
        (λ (x y) (* (sin x) (cos y))))))

(adaptive-deform
 (ring origin 2 #:radii (interval 0.15 1))
 (displace (λ (x y) (* (sin x) (cos y)))))

(combine
 (time
  (adaptive-deform
   (rectangle origin (dir 1 1 4))
   (smooth-between
    (twist 90)
    identity-affine
    (λ (v)
      (match-define (pos x y z) v)
      (exp (- (sqr (/ z 2.75))))))))
 (basis 'camera (point-at (pos 4 4 0.9) (pos 0 0 0.25))))
