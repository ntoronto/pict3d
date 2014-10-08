#lang typed/racket/base

(require (except-in typed/opengl/ffi cast ->)
         "context.rkt")

(provide (struct-out gl-object)
         manage-gl-object
         call-with-gl-object)

;; ===================================================================================================
;; Managed objects

(struct gl-object ([handle : Natural]) #:transparent)

(: manage-gl-object (-> gl-object (-> Natural Any) Void))
(define (manage-gl-object obj delete)
  (define ctxt (get-current-managed-gl-context 'make-gl-object))
  (register-finalizer
   obj
   (λ ([obj : gl-object])
     (gl-delete-later ctxt 'gl-object (gl-object-handle obj) delete))))

(define-syntax-rule (call-with-gl-object body-thunk param obj bind)
  (call-with-gl-state body-thunk param obj (λ (v) (bind (gl-object-handle v)))))
