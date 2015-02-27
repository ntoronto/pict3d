#lang racket/base

(require racket/contract
         racket/class
         racket/draw
         "typed-context.rkt"
         ;; The following can't have contracts (e.g. no predicate for (Instance GL-Context<%>))
         (only-in "untyped-context.rkt"
                  managed-gl-context
                  gl-context-context))

(provide (all-from-out "typed-context.rkt")
         with-gl-context
         call-with-gl-state
         (contract-out
          [managed-gl-context  (-> (is-a?/c gl-context<%>) gl-context?)]
          [gl-context-context  (-> gl-context? (is-a?/c gl-context<%>))]))

(define-syntax-rule (with-gl-context new-ctxt body ...)
  (call-with-gl-context (λ () body ...) new-ctxt))

(define-syntax-rule (call-with-gl-state body-thunk param obj set-state!)
  (let ()
    (get-current-managed-gl-context 'call-with-gl-state)
    (define old (param))
    (cond [(eq? old obj)  (body-thunk)]
          [else  (set-state! obj)
                 (begin0
                   (parameterize ([param obj]) (body-thunk))
                   (set-state! old))])))
