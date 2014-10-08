#lang typed/racket/base

(require/typed
 ffi/cvector
 [#:opaque CVector cvector?])

(provide CVector cvector?)

