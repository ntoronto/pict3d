#lang typed/racket/base

(require/typed
 "untyped-context.rkt"
 [pict3d-legacy-contexts?  (Parameterof Boolean)]
 [#:struct gl-context ([number : Natural])]
 [gl-context-ok?  (-> gl-context Boolean)]
 [gl-delete-later  (-> gl-context Symbol Natural (-> Natural Any) Void)]
 [call-with-gl-context  (All (A) (-> (-> A) gl-context A))]
 [get-current-managed-gl-context  (-> Symbol gl-context)]
 [gl-swap-buffers  (-> Void)]
 [get-master-gl-context  (-> gl-context)]
 )

(define-type GL-Context gl-context)

(provide pict3d-legacy-contexts?
         GL-Context
         gl-context?
         gl-context-ok?
         gl-delete-later
         call-with-gl-context
         get-current-managed-gl-context
         gl-swap-buffers
         get-master-gl-context)
