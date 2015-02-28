#lang racket/base

(require racket/list
         racket/math)

(provide (all-defined-out))

(define (remove-trailing-zeros str)
  (define m (regexp-match #rx"(-|)([0-9]*)(\\.0*$)" str))
  (if m (third m) str))

(define (format/prec x digits)
  (cond [(rational? x)
         (define e (expt 10 digits))
         (remove-trailing-zeros
          (real->decimal-string (* (exact-round (/ (inexact->exact x) e)) e)
                                (max 1 (- digits))))]
        [else
         (number->string x)]))
