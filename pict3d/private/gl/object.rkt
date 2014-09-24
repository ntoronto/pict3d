#lang typed/racket/base

(require (except-in typed/opengl/ffi cast ->)
         "context.rkt")

(provide (struct-out gl-object)
         get-num-gl-objects-collected
         manage-gl-object
         call-with-gl-object)

;; ===================================================================================================
;; Managed objects

(struct gl-object ([handle : Natural]) #:transparent)

(: gl-object-contexts (HashTable gl-object GL-Context))
(define gl-object-contexts (make-weak-hasheq))

(: gl-object-context (-> gl-object GL-Context))
(define (gl-object-context obj)
  (hash-ref gl-object-contexts obj
            (λ () (raise-argument-error 'get-gl-object-context "valid gl-object" obj))))

(: num-gl-objects-collected Natural)
(define num-gl-objects-collected 0)

(define (get-num-gl-objects-collected) num-gl-objects-collected)

(: manage-gl-object (-> gl-object (-> Natural Any) Void))
(define (manage-gl-object obj delete)
  (define ctxt (get-current-gl-context 'make-gl-object))
  (hash-set! gl-object-contexts obj ctxt)
  (register-finalizer
   obj
   (λ ([obj : gl-object])
     (set! num-gl-objects-collected (+ 1 num-gl-objects-collected))
     (define ctxt (with-handlers ([exn?  (λ (e) #f)])
                    (gl-object-context obj)))
     (when (and ctxt (gl-context-ok? ctxt))
       (call-with-gl-context
        (λ () (delete (gl-object-handle obj)))
        ctxt)))))

(define-syntax-rule (call-with-gl-object body-thunk param obj bind)
  (call-with-gl-state body-thunk param obj (λ (v) (bind (gl-object-handle v)))))
