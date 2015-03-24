#lang racket/base

(require "typed-pict3d-combinators.rkt")

(provide (all-from-out
          "typed-pict3d-combinators.rkt")
         with-color
         with-emitted
         with-material)

(define-syntax-rule (with-color c body ...)
  (parameterize ([current-color c]) body ...))

(define-syntax-rule (with-emitted e body ...)
  (parameterize ([current-emitted e]) body ...))

(define-syntax-rule (with-material m body ...)
  (parameterize ([current-material m]) body ...))

