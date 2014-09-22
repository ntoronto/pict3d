#lang typed/racket/base

(require typed/racket/gui
         typed/racket/class)

(provide GL-Context
         managed-gl-context
         gl-context-ok?
         gl-context-context
         call-with-gl-context
         with-gl-context
         get-current-gl-context
         gl-swap-buffers
         call-with-gl-state)

;; ===================================================================================================
;; Managed OpenGL contexts

(struct gl-context ([number : Natural]) #:transparent)

(define-type GL-Context gl-context)

(: gl-contexts (HashTable gl-context (Instance GL-Context<%>)))
(define gl-contexts (make-weak-hasheq))

(: managed-gl-context (-> (Instance GL-Context<%>) gl-context))
(define managed-gl-context
  (let ([next-number : Natural  0])
    (λ (ctxt)
      (define number next-number)
      (set! next-number (+ 1 number))
      (define key (gl-context number))
      (hash-set! gl-contexts key ctxt)
      key)))

(: gl-context-context (-> gl-context (Instance GL-Context<%>)))
(define (gl-context-context key)
  (hash-ref gl-contexts key (λ () (raise-argument-error 'gl-context-context "valid gl-context" key))))

(: gl-context-ok? (-> gl-context Boolean))
(define (gl-context-ok? ctxt)
  (send (gl-context-context ctxt) ok?))

(: current-gl-context (Parameterof (U #f gl-context)))
(define current-gl-context (make-parameter #f))

(: call-with-gl-context (-> (-> Any) gl-context Any))
(define (call-with-gl-context thunk new-ctxt)
  (define ctxt (current-gl-context))
  (cond [(not ctxt)
         (send (gl-context-context new-ctxt) call-as-current
               (λ () (parameterize ([current-gl-context  new-ctxt]) (thunk))))]
        [(eq? new-ctxt ctxt)
         (send (gl-context-context new-ctxt) call-as-current thunk)]
        [else
         (error 'call-with-gl-context "already in another managed OpenGL context")]))

(define-syntax-rule (with-gl-context new-ctxt body ...)
  (call-with-gl-context (λ () body ...) new-ctxt))

(: get-current-gl-context (-> Symbol gl-context))
(define (get-current-gl-context name)
  (define ctxt (current-gl-context))
  (if ctxt ctxt (error name "not in a managed OpenGL context (use with-gl-context)")))

(: gl-swap-buffers (-> Void))
(define (gl-swap-buffers)
  (define ctxt (get-current-gl-context 'gl-swap-buffers))
  (send (gl-context-context ctxt) swap-buffers))

(define-syntax-rule (call-with-gl-state body-thunk param obj set-state!)
  (let ()
    (get-current-gl-context 'call-with-gl-state)
    (define old (param))
    (cond [(eq? old obj)  (body-thunk)]
          [else  (set-state! obj)
                 (begin0
                   (parameterize ([param obj]) (body-thunk))
                   (set-state! old))])))
