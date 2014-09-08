#lang typed/racket/base

(require (for-syntax racket/base)
         (rename-in typed/racket/base [-> ->>])
         racket/include
         (except-in "ffi.rkt" cast ->))

(provide (all-defined-out)
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
 [gl-vector->info  (->> GLVector (Values Natural (->> GLVector CPointer) (->> GLVector Index)))]
 [gl-vector->cpointer  (->> GLVector CPointer)]
 [default-gl-procedure-loader  (->> String (U CPointer Procedure #f))]
 [set-gl-procedure-loader!  (->> (->> String (U CPointer Procedure #f)) Void)]
 )

;; ===================================================================================================

(define-for-syntax requires '())
(define-for-syntax provides '())

(define-syntax (do-requires+provides stx)
  (with-syntax ([([req-name type] ...)  requires]
                [(prov-name ...)  provides])
    #'(begin
        (require/typed
         "untyped.rkt"
         [req-name type] ...)
        (provide prov-name ...))))

(define-syntax define-gl
  (syntax-rules ()
    [(_ name _ _ type _)
     (begin-for-syntax
       (set! requires (cons #'[name type] requires))
       (set! provides (cons #'name provides)))]))

(define-syntax define-const
  (syntax-rules ()
    ((_ name value)
     (begin
       (: name Natural)
       (define name value)
       (begin-for-syntax
         (set! provides (cons #'name provides)))))))

(define-syntax define-enum
  (syntax-rules ()
   ((_ name _)
    (begin-for-syntax
      (set! requires (cons #'[name (-> Any Boolean)] requires))
      (set! provides (cons #'name provides))))))

(define-syntax define-bitfield
  (syntax-rules ()
   ((_ name _)
    (begin-for-syntax
      (set! requires (cons #'[name (-> Any Boolean)] requires))
      (set! provides (cons #'name provides))))))

(include "generated/gl_specs.inc")

(do-requires+provides)
