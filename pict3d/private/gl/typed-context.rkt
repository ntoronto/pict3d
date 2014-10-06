#lang typed/racket/base

(require/typed
 "untyped-context.rkt"
 [#:struct gl-context ([number : Natural])]
 [gl-context-ok?  (-> gl-context Boolean)]
 [call-with-gl-context  (All (A) (-> (-> A) gl-context A))]
 [get-current-managed-gl-context  (-> Symbol gl-context)]
 [gl-swap-buffers  (-> Void)]
 [get-master-gl-context  (-> gl-context)]
 )

(define-type GL-Context gl-context)

(provide GL-Context
         gl-context?
         gl-context-ok?
         call-with-gl-context
         get-current-managed-gl-context
         gl-swap-buffers
         get-master-gl-context)
