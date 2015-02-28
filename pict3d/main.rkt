#lang racket/base

(require "private/lazy-gui.rkt"
         "private/gui/pict3d-canvas.rkt")

(provide (all-from-out
          "private/lazy-gui.rkt"
          "private/gui/pict3d-canvas.rkt"))

(require (only-in "private/gui/pict3d-snip.rkt" scene->pict3d%))

(define (pict3d-custom-write scene out mode)
  (define print-it
    (cond [(eq? mode #t)  write]
          [(eq? mode #f)  display]
          [else  print]))
  (print-it (scene->pict3d% scene) out))

(define (pict3d-print-converter scene recur)
  (scene->pict3d% scene))

;; Set the custom printer so Pict3D instances will print nicely in Racket
(current-pict3d-custom-write pict3d-custom-write)
;; Set the print converter so Pict3D instances will print nicely in HTDP languages
(current-pict3d-print-converter pict3d-print-converter)
