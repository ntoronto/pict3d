#lang racket

(provide (all-defined-out))

(module typed-defs typed/racket/base
  (provide pict3d-logger)
  (define pict3d-logger (make-logger 'pict3d (current-logger))))

(require 'typed-defs)

;; Copied
(define-for-syntax (make-define-log mode X-logger-stx)
  (lambda (stx)
    (with-syntax ([X-logger X-logger-stx]
                  [mode mode])
      (syntax-case stx ()
        [(_ str-expr) 
         #'(let ([l X-logger])
             (when (log-level? l 'mode)
               (log-message l 'mode str-expr (current-continuation-marks))))]
        [(_ str-expr arg ...)
         #'(let ([l X-logger])
             (when (log-level? l 'mode)
               (log-message l 'mode (format str-expr arg ...) (current-continuation-marks))))]))))

(define-syntax log-pict3d-fatal   (make-define-log 'fatal #'pict3d-logger))
(define-syntax log-pict3d-error   (make-define-log 'error #'pict3d-logger))
(define-syntax log-pict3d-warning (make-define-log 'warning #'pict3d-logger))
(define-syntax log-pict3d-info    (make-define-log 'info #'pict3d-logger))
(define-syntax log-pict3d-debug   (make-define-log 'debug #'pict3d-logger))
