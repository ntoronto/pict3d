#lang racket/base

(require (only-in "private/gl/context.rkt" pict3d-legacy-contexts?)
         "private/gui/user-types.rkt"
         "private/gui/parameters.rkt"
         "private/gui/pict3d-struct.rkt"
         "private/gui/pict3d-combinators.rkt"
         "private/gui/pict3d-canvas.rkt"
         "private/gui/pict3d-bitmap.rkt"
         "private/gui/pict3d-snip.rkt")

(provide (all-from-out
          "private/gl/context.rkt"
          "private/gui/user-types.rkt"
          "private/gui/parameters.rkt"
          "private/gui/pict3d-struct.rkt"
          "private/gui/pict3d-combinators.rkt"
          "private/gui/pict3d-canvas.rkt"
          "private/gui/pict3d-bitmap.rkt"
          "private/gui/pict3d-snip.rkt")
         with-color
         with-emitted
         with-material)

(define-syntax-rule (with-color col body ...)
  (parameterize ([current-color col]) body ...))

(define-syntax-rule (with-emitted col body ...)
  (parameterize ([current-emitted col]) body ...))

(define-syntax-rule (with-material mat body ...)
  (parameterize ([current-material mat]) body ...))
