#lang racket/base

(require racket/gui
         racket/class
         math/flonum
         math/base
         "../math/flv3.rkt"
         "../math/flt3.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Timer that destroys itself unless kept alive
#;
(define-type Timeout-Timer%
  (Class (init-field [notify-callback (U #f (-> Any))]
                     [timeout-callback (U #f (-> Any))]
                     [notify-interval Integer]
                     [timeout-interval Integer])
         [notify (-> Void)]
         [timeout (-> Void)]
         [keep-alive (-> Void)]))

#;(: timeout-timer% Timeout-Timer%)
(define timeout-timer%
  (class object%
    (init-field notify-callback
                timeout-callback
                notify-interval
                timeout-interval)
    
    (super-new)
    
    #;(: notify-timer (U #f (Instance Timer%)))
    (define notify-timer
      (make-object timer% (λ () (send this notify)) notify-interval #f))
    
    #;(: timeout-timer (U #f (Instance Timer%)))
    (define timeout-timer
      (make-object timer% (λ () (send this timeout)) timeout-interval #t))
    
    (define/public (notify)
      (define notify-callback-val notify-callback)
      (when notify-callback-val
        (notify-callback-val)
        (void)))
    
    (define/public (timeout)
      ;; Stop the notify timer, and remove reference to it and its callback
      (define notify-timer-val notify-timer)
      (when notify-timer-val
        (send notify-timer-val stop)
        (set! notify-timer #f)
        (set! notify-callback #f))
      ;; Stop the timeout timer, and remove reference to it
      (define timeout-timer-val timeout-timer)
      (when timeout-timer-val
        (send timeout-timer-val stop)
        (set! timeout-timer #f))
      ;; Call the timeout callback if it exists and remove reference to it
      (define timeout-callback-val timeout-callback)
      (when timeout-callback-val
        (timeout-callback-val)
        (set! timeout-callback #f)))
    
    (define/public (keep-alive)
      (define timeout-timer-val timeout-timer)
      (when timeout-timer-val
        (send timeout-timer-val stop)
        (send timeout-timer-val start timeout-interval #t)))
    ))

;; ===================================================================================================
;; Camera
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
    (init-field [position (flvector 0.0 0.0 0.0)]
                [velocity (flvector 0.0 0.0 0.0)]
                [yaw 0.0]
                [pitch 0.0])
    
    (super-new)
    
    (define/public (get-position) position)
    (define/public (set-position v) (set! position v))
    (define/public (get-velocity) velocity)
    (define/public (set-velocity v) (set! velocity v))
    
    #;(: get-translation-matrix (-> FlAffine3))
    (define/private (get-translation-matrix)
      (translate-flt3 (flv3neg position)))
    
    #;(: get-rotation-matrix (-> FlLinear3))
    (define/private (get-rotation-matrix)
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
        (set! velocity (flvector 0.0 0.0 0.0))))
    
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

;; ===================================================================================================

#;(: snip-center-pointer (-> (Instance Snip%) (Values (U #f Integer) (U #f Integer))))
(define (snip-center-pointer snip)
  (define admin (send snip get-admin))
  (define editor (and admin (send admin get-editor)))
  (define canvas (and editor (send editor get-active-canvas)))
  (cond [(and editor canvas)
         (define loc-x0 (box 0))
         (define loc-y0 (box 0))
         (define loc-x1 (box 0))
         (define loc-y1 (box 0))
         (send editor get-snip-location snip loc-x0 loc-y0 #f)
         (send editor get-snip-location snip loc-x1 loc-y1 #t)
         (define-values (x0 y0)
           (let-values ([(x y)  (send editor editor-location-to-dc-location
                                      (unbox loc-x0) (unbox loc-y0))])
             (values (exact-floor x) (exact-floor y))))
         (define-values (x1 y1)
           (let-values ([(x y)  (send editor editor-location-to-dc-location
                                      (unbox loc-x1) (unbox loc-y1))])
             (values (exact-ceiling x) (exact-ceiling y))))
         (cond [(and (>= x0 0) (<= x1 (send canvas get-width))
                     (>= y0 0) (<= y1 (send canvas get-height)))
                (define x (quotient (+ x0 x1) 2))
                (define y (quotient (+ y0 y1) 2))
                (send canvas warp-pointer x y)
                (values x y)]
               [else
                (values #f #f)])]
        [else
         (values #f #f)]))
