#lang racket/base

(require racket/match
         racket/class
         racket/math
         math/flonum
         "../../math/flv3.rkt"
         "../../math/flt3.rkt")

(provide (all-defined-out))

#;
(define-type Camera%
  (Class (init-field [position  FlVector]
                     [velocity  FlVector]
                     [yaw    Flonum]
                     [pitch  Flonum])
         [get-position  (-> FlVector)]
         [set-position  (-> FlVector Void)]
         [get-velocity  (-> FlVector)]
         [set-velocity  (-> FlVector Void)]
         [get-view-matrix  (-> FlAffine3)]
         [change-angles  (-> Flonum Flonum Void)]
         [accelerate  (-> FlVector Flonum Void)]
         [rotate-direction  (-> FlVector FlVector)]
         [unrotate-direction  (-> FlVector FlVector)]))

#;(: camera% Camera%)
(define camera%
  (class object%
    (init)
    (init-field [position zero-flv3] [velocity zero-flv3] [yaw 0.0] [pitch 0.0])
    
    (super-new)
    
    (define/public (get-position) position)
    (define/public (set-position v) (set! position v))
    (define/public (get-velocity) velocity)
    (define/public (set-velocity v) (set! velocity v))
    (define/public (get-yaw) yaw)
    (define/public (set-yaw v) (set! yaw v))
    (define/public (get-pitch) pitch)
    (define/public (set-pitch v) (set! pitch v))
    
    #;(: get-translation-matrix (-> FlAffine3))
    (define/public (get-translation-matrix)
      (translate-flt3 (flv3neg position)))
    
    #;(: get-rotation-matrix (-> FlLinear3))
    (define/public (get-rotation-matrix)
      (flt3compose
       (flt3compose (rotate-x-flt3 (- pitch))
                    (rotate-y-flt3 (- yaw)))
       (rotate-x-flt3 (/ pi -2.0))))
    
    (define/public (get-view-matrix)
      (flt3compose (get-rotation-matrix) (get-translation-matrix)))
    
    (define/public (set-view-matrix t)
      (match-define (list m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
        (flvector->list (fltransform3-inverse t)))
      (set! position (flvector m03 m13 m23))
      (set! yaw (+ (atan m12 m02) (/ pi 2)))
      (set! pitch (- (asin (/ m22 (flsqrt (+ (sqr m02) (sqr m12) (sqr m22))))))))
    
    (define/public (accelerate acc dt)
      (set! position (flv3+ (flv3+ position (flv3* velocity dt))
                            (flv3* acc (* 0.5 dt dt))))
      (set! velocity (flv3+ velocity (flv3* acc dt)))
      (define speed (flv3mag velocity))
      (when (< speed (flexpt 2.0 -20.0))
        (set! velocity zero-flv3)))
    
    (define/public (rotate-direction v)
      (flt3apply/pos (flt3inverse (get-rotation-matrix)) v))
    
    (define/public (unrotate-direction v)
      (flt3apply/pos (get-rotation-matrix) v))
    
    (define/public (change-angles dy dp)
      (let* ([y  (- yaw dy)]
             [p  (- pitch dp)]
             ;; Keep yaw between -pi and pi by floating-point modulo
             [y  (- y (* (* 2.0 pi) (round (/ y (* 2.0 pi)))))]
             ;; Keep pitch between -pi/2 and pi/2 by clamping (gimball lock)
             [p  (min (* 0.5 pi) (max (* -0.5 pi) p))])
      (set! yaw y)
      (set! pitch p)))
    ))
