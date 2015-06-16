#lang racket/base

(require racket/contract
         typed/untyped-utils
         (rename-in "typed-pict3d-combinators.rkt"
                    [bend         typed-bend]
                    [bend-smooth  typed-bend-smooth]
                    [bend-pict3d  typed-bend-pict3d])
         "pict3d-struct.rkt"
         "user-types.rkt")

(provide (except-out
          (all-from-out "typed-pict3d-combinators.rkt")
          typed-bend
          typed-bend-pict3d
          typed-bend-smooth)
         with-color
         with-emitted
         with-material
         bend)

(define/contract untyped-bend
  (->i ([arg1  (or/c pict3d? real?)]
        [arg2  (or/c real? interval?)])
       ([zivl (arg1) (if (pict3d? arg1) interval? none/c)])
       [result (arg1 arg2) (if (pict3d? arg1) pict3d? smooth?)])
  (case-lambda
    [(arg1 arg2)
     (cond [(and (pict3d? arg1) (real? arg2))    (typed-bend-pict3d arg1 arg2)]
           [(and (real? arg1) (interval? arg2))  (typed-bend-smooth arg1 arg2)])]
    [(p angle zivl)
     (typed-bend-pict3d p angle zivl)]))

(define-typed/untyped-identifier bend
  typed-bend
  untyped-bend)

(define-syntax-rule (with-color c body ...)
  (parameterize ([current-color c]) body ...))

(define-syntax-rule (with-emitted e body ...)
  (parameterize ([current-emitted e]) body ...))

(define-syntax-rule (with-material m body ...)
  (parameterize ([current-material m]) body ...))
