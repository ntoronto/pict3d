#lang racket/base

(require racket/class
         racket/lazy-require
         racket/list
         racket/match)

(provide (all-defined-out))

;; ===================================================================================================
;; Managed OpenGL contexts

(struct gl-context (number) #:transparent)

(define gl-contexts (make-weak-hasheq))

(define managed-gl-context
  (let ([next-number  0])
    (λ (ctxt)
      (define number next-number)
      (set! next-number (+ 1 number))
      (define key (gl-context number))
      (hash-set! gl-contexts key ctxt)
      key)))

(define (gl-context-context key)
  (hash-ref gl-contexts key (λ () (raise-argument-error 'gl-context-context "valid gl-context" key))))

(define (gl-context-ok? ctxt)
  (send (gl-context-context ctxt) ok?))

(define current-gl-context (make-parameter #f))

(struct garbage (name handle delete) #:transparent)

;(: gl-context-garbage (HashTable gl-context (Listof garbage)))
(define gl-context-garbage (make-weak-hasheq))

(define (gl-delete-later ctxt name handle delete)
  (define gs (hash-ref gl-context-garbage ctxt (λ () empty)))
  (hash-set! gl-context-garbage ctxt (cons (garbage name handle delete) gs)))

(define (clean-gl-garbage ctxt)
  (define gs (hash-ref gl-context-garbage ctxt (λ () empty)))
  (for ([g  (in-list gs)])
    (match-define (garbage name handle delete) g)
    (delete handle))
  (hash-set! gl-context-garbage ctxt empty))

(define (call-with-gl-context thunk new-ctxt)
  (define ctxt (current-gl-context))
  (cond [(not ctxt)
         (send (gl-context-context new-ctxt) call-as-current
               (λ () (parameterize ([current-gl-context  new-ctxt])
                       (clean-gl-garbage new-ctxt)
                       (thunk))))]
        [(eq? new-ctxt ctxt)
         (send (gl-context-context new-ctxt) call-as-current thunk)]
        [else
         (error 'call-with-gl-context "already in another managed OpenGL context")]))

(define (get-current-managed-gl-context name)
  (define ctxt (current-gl-context))
  (if ctxt ctxt (error name "not in a managed OpenGL context (use with-gl-context)")))

(define (gl-swap-buffers)
  (define ctxt (get-current-managed-gl-context 'gl-swap-buffers))
  (send (gl-context-context ctxt) swap-buffers)
  ;; Windows' gl-context%'s swap-buffers can currently return #t
  (void))

;; ===================================================================================================
;; Master GL context

(lazy-require ["invisible-context.rkt"
               (get-bitmap-context
                get-invisible-canvas-context)])

(define master-gl-context-mutex (make-semaphore 1))
(define context-hash (make-hash))

(define (get-master-gl-context legacy?)
  (call-with-semaphore
   master-gl-context-mutex
   (λ ()
     (hash-ref!
      context-hash legacy?
      (λ ()
        ;; Don't try for bitmap contexts for now - they're too broken on Windows and possibly Mac
        (define ctxt #f #;(get-bitmap-context legacy?))
        (cond
          [ctxt  (managed-gl-context ctxt)]
          [else
           (define ctxt (get-invisible-canvas-context legacy?))
           (cond [ctxt  (managed-gl-context ctxt)]
                 [else
                  (error 'get-master-gl-context
                         "could not get at least an OpenGL 30 context (legacy? = ~a)"
                         legacy?)])]))))))
