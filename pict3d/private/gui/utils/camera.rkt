#lang racket/base

(require racket/match
         racket/class
         racket/math
         math/flonum
         "../pict3d-combinators.rkt"
         "../user-types.rkt")

(provide (all-defined-out))

(define camera%
  (class object%
    (init)
    (init-field [position origin]
                [velocity (dir 0 0 0)]
                [yaw 0.0]
                [pitch 0.0])
    
    (super-new)
    
    (define/public (get-position) position)
    (define/public (set-position v) (set! position v))
    (define/public (get-velocity) velocity)
    (define/public (set-velocity v) (set! velocity v))
    (define/public (get-yaw) yaw)
    (define/public (set-yaw v) (set! yaw v))
    (define/public (get-pitch) pitch)
    (define/public (set-pitch v) (set! pitch v))
    
    (define/public (get-basis)
      (affine-compose
       (move (pos- position origin))
       (rotate-z yaw)
       (rotate-y pitch)
       (rotate-z -90)
       (rotate-x -90)))
    
    (define/public (set-basis t)
      (define dz (affine-z-axis t))
      (define v (affine-origin t))
      (match-define (dir m02 m12 m22) dz)
      (match-define (pos m03 m13 m23) v)
      (set! position v)
      (set! yaw (radians->degrees (atan m12 m02)))
      (set! pitch (radians->degrees (- (asin (/ m22 (flsqrt (+ (sqr m02) (sqr m12) (sqr m22)))))))))
    
    (define/public (accelerate acc dt)
      (set! position (pos+ position (dir+ (dir-scale velocity dt)
                                          (dir-scale acc (* 0.5 dt dt)))))
      (set! velocity (dir+ velocity (dir-scale acc dt)))
      (define speed (dir-dist velocity))
      (when (< speed (flexpt 2.0 -20.0))
        (set! velocity (dir 0 0 0))))
    
    (define/public (rotate-direction dv)
      (transform-dir dv (get-basis)))
    
    (define/public (unrotate-direction dv)
      (transform-dir dv (affine-inverse (get-basis))))
    
    (define/public (change-angles dy dp)
      (let* ([y  (- yaw dy)]
             [p  (- pitch dp)]
             ;; Keep yaw between -180 and 180 by floating-point modulo
             [y  (- y (* 360 (round (/ y 360))))]
             ;; Keep pitch between -90 and 90 by clamping (gimball lock)
             [p  (min 90 (max -90 p))]
             )
      (set! yaw y)
      (set! pitch p)))
    ))
