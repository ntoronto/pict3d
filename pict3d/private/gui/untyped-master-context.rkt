#lang racket/base

(require racket/class
         racket/lazy-require
         racket/list
         racket/match
         "../gl.rkt")

(lazy-require ["invisible-context.rkt"
               (get-bitmap-context
                get-invisible-canvas-context)])

(provide (all-defined-out))

(define master-gl-context-mutex (make-semaphore 1))
(define context-hash (make-hash))

(define (get-master-gl-context legacy? check-version?)
  (call-with-semaphore
   master-gl-context-mutex
   (λ ()
     (hash-ref!
      context-hash
      (list legacy? check-version?)
      (λ ()
        ;; Don't try for bitmap contexts for now - they're too broken on Windows and possibly Mac
        (define ctxt #f #;(get-bitmap-context legacy? check-version?))
        (cond
          [ctxt  (managed-gl-context ctxt)]
          [else
           (define ctxt (get-invisible-canvas-context legacy? check-version?))
           (cond [ctxt  (managed-gl-context ctxt)]
                 [else
                  (error 'get-master-gl-context
                         "could not get at least an OpenGL 30 context (legacy? = ~a)"
                         legacy?)])]))))))
