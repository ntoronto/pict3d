#lang typed/racket/base

(require typed/racket/gui
         typed/racket/class
         "context.rkt")

(require/typed
 racket/base
 [call-with-semaphore  (All (A) (-> Semaphore (-> A) A))])

(provide get-master-gl-context)

;; ===================================================================================================
;; Master GL context

(define master-context-max-width 4096)
(define master-context-max-height 4096)

(define master-gl-context-mutex (make-semaphore 1))

(: master-frame (U #f (Instance Frame%)))
(define master-frame #f)

(: master-context (U #f GL-Context))
(define master-context #f)

(: get-master-gl-context (-> GL-Context))
(define (get-master-gl-context)
  (call-with-semaphore
   master-gl-context-mutex
   (λ ()
     (define ctxt master-context)
     (cond [ctxt  ctxt]
           [else
            (define config (new gl-config%))
            (send config set-legacy? #f)
            (define frame (new frame%
                               [label "Master GL context frame"]
                               [width   master-context-max-width]
                               [height  master-context-max-height]
                               [min-width   master-context-max-width]
                               [min-height  master-context-max-height]
                               [stretchable-width #f]
                               [stretchable-height #f]))
            (define canvas (new canvas% [parent frame] [style '(gl no-autoclear)] [gl-config config]))
            (send frame show #t)
            (send frame show #f)
            (sleep/yield 1)
            (define ctxt (send (send canvas get-dc) get-gl-context))
            (cond [(and ctxt (send ctxt ok?))
                   (set! master-frame frame)
                   (let ([ctxt  (managed-gl-context ctxt)])
                     (set! master-context ctxt)
                     ctxt)]
                  [else
                   (error 'get-master-gl-context "can't get a GL context")])]))))
