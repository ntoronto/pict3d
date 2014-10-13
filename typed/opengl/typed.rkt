#lang s-exp typed-racket/base-env/extra-env-lang

(require (for-syntax racket/base)
         (rename-in typed/racket/base [-> ->>])
         racket/include
         (except-in "ffi.rkt" cast ->)
         "typed-types.rkt"
         (for-syntax (submod "typed-types.rkt" #%type-decl)))

(provide (all-defined-out)
         (all-from-out "typed-types.rkt"))

(require (except-in "untyped.rkt"
                    GLsync?
                    gl-vector->info
                    gl-vector->cpointer
                    default-gl-procedure-loader
                    set-gl-procedure-loader!))

;; ===================================================================================================

(define-for-syntax requires '())
(define-for-syntax provides '())

(define-syntax (do-requires+provides stx)
  (with-syntax ([([req-name type] ...)  requires]
                [(prov-name ...)  provides])
    #'(begin
        (type-environment
         [req-name  (parse-type #'type)] ...)
        #;
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
       ;(: name Natural)
       ;(define name value)
       (begin-for-syntax
         (set! requires (cons #'[name Natural] requires))
         (set! provides (cons #'name provides))))
     #;
     (begin
       ;(: name Natural)
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
#|
(define-const GL_ARRAY_BUFFER #x8892)
(define-gl glBindBuffer
  2
  ((target : _int32) (buffer : _uint32) -> _void)
  (->> Integer Natural Void)
  check-gl-error)

(define-const GL_FRONT #x0404)
(define-const GL_BACK #x0405)
(define-const GL_FRONT_AND_BACK #x0408)
(define-gl glCullFace 1 ((mode : _int32) -> _void) (->> Integer Void) check-gl-error)
|#

(do-requires+provides)
