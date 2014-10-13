#lang typed/racket/base

(require (except-in "ffi.rkt" cast ->))

(provide GLVector
         GLPointer
         GLSync
         gl-vector?
         gl-pointer?
         GLsync?
         gl-vector->info
         gl-vector->cpointer
         default-gl-procedure-loader
         set-gl-procedure-loader!)

;; A "gl-vector" is any homogenous vector of a type which is used with the OpenGL API.
(define-type GLVector
  (U Bytes S8Vector U16Vector S16Vector U32Vector S32Vector F32Vector F64Vector))

(define-type GLPointer
  (U CPointer Natural GLVector))

(define-predicate gl-vector? GLVector)
(define-predicate gl-pointer? GLPointer)

(require/typed
 "untyped.rkt"
 [#:opaque GLSync GLsync?]
 [gl-vector->info  (-> GLVector (Values Natural (-> GLVector CPointer) (-> GLVector Index)))]
 [gl-vector->cpointer  (-> GLVector CPointer)]
 [default-gl-procedure-loader  (-> String (U CPointer Procedure #f))]
 [set-gl-procedure-loader!  (-> (-> String (U CPointer Procedure #f)) Void)]
 )
