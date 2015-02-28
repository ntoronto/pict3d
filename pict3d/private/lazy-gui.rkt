#lang racket/base

(require "gui/user-types.rkt"
         "gui/parameters.rkt"
         "gui/pict3d-struct.rkt"
         "gui/pict3d-combinators.rkt"
         "gui/pict3d-bitmap.rkt")

(provide (all-from-out
          "gui/user-types.rkt"
          "gui/parameters.rkt"
          "gui/pict3d-struct.rkt"
          "gui/pict3d-combinators.rkt"
          "gui/pict3d-bitmap.rkt")
         with-color
         with-emitted
         with-material)

(define-syntax-rule (with-color col body ...)
  (parameterize ([current-color col]) body ...))

(define-syntax-rule (with-emitted col body ...)
  (parameterize ([current-emitted col]) body ...))

(define-syntax-rule (with-material mat body ...)
  (parameterize ([current-material mat]) body ...))

