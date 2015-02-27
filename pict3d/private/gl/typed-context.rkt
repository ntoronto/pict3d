#lang typed/racket/base

(require/typed
 "untyped-context.rkt"
 [#:struct gl-context ([number : Natural])]
 [gl-context-ok?  (-> gl-context Boolean)]
 [gl-delete-later  (-> gl-context Symbol Natural (-> Natural Any) Void)]
 [call-with-gl-context  (All (A) (-> (-> A) gl-context A))]
 [get-current-managed-gl-context  (-> Symbol gl-context)]
 [gl-swap-buffers  (-> Void)])

(define-type GL-Context gl-context)

(provide GL-Context
         gl-context?
         gl-context-ok?
         gl-delete-later
         call-with-gl-context
         get-current-managed-gl-context
         gl-swap-buffers)
