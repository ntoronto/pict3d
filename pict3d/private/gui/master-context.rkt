#lang typed/racket/base

(require "../gl.rkt")

(require/typed
 "untyped-master-context.rkt"
 [get-master-gl-context  (-> Boolean Boolean GL-Context)])

(provide get-master-gl-context)
