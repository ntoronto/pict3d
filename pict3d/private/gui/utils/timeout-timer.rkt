#lang racket/base

(require racket/gui/base
         racket/class)

(provide (all-defined-out))

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
