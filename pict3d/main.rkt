#lang racket/base

(require racket/lazy-require)

(lazy-require ["private/gui/pict3d-snip.rkt" (scene->pict3d)])

(require "private/gui/user-types.rkt"
         "private/gui/parameters.rkt"
         "private/gui/pict3d-struct.rkt"
         "private/gui/pict3d-combinators.rkt"
         "private/gui/pict3d-canvas.rkt"
         "private/gui/pict3d-bitmap.rkt")

(provide (all-from-out
          "private/gui/user-types.rkt"
          "private/gui/parameters.rkt"
          "private/gui/pict3d-struct.rkt"
          "private/gui/pict3d-combinators.rkt"
          "private/gui/pict3d-canvas.rkt"
          "private/gui/pict3d-bitmap.rkt")
         with-color
         with-emitted
         with-material)

(define-syntax-rule (with-color col body ...)
  (parameterize ([current-color col]) body ...))

(define-syntax-rule (with-emitted col body ...)
  (parameterize ([current-emitted col]) body ...))

(define-syntax-rule (with-material mat body ...)
  (parameterize ([current-material mat]) body ...))

(define (pict3d-custom-write scene out mode)
  (define print-it
    (cond [(eq? mode #t)  write]
          [(eq? mode #f)  display]
          [else  print]))
  (print-it (scene->pict3d scene) out))

(define (pict3d-print-converter scene recur)
  (scene->pict3d scene))

;; Set the custom printer so Pict3D instances will print nicely in Racket
(current-pict3d-custom-write pict3d-custom-write)
;; Set the print converter so Pict3D instances will print nicely in HTDP languages
(current-pict3d-print-converter pict3d-print-converter)
