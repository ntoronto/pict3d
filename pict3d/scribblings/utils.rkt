#lang racket/base

(require racket/gui/dynamic)

(define eval-mode (if (gui-available?) 'record 'replay))

(when (eq? eval-mode 'record)
  (printf "RECORDING MODE~n"))

(require racket/match
         scribble/eval
         unstable/sandbox
         racket/runtime-path
         racket/draw
         "serializable-bitmap.rkt"
         (for-label (except-in racket/base
                               ...)
                    racket/gui/base
                    racket/class
                    racket/flonum
                    racket/match
                    images/flomap
                    (only-in plot plot function)
                    (only-in 2htdp/universe
                             big-bang)
                    (only-in typed/racket/base
                             U -> : Values
                             Real Any Boolean Natural
                             Symbol String Listof Pair Vectorof FlVector Flonum Integer
                             Positive-Real Positive-Index Positive-Flonum Void
                             Positive-Integer
                             Nonnegative-Real
                             Instance)
                    (only-in typed/racket/draw
                             Color%
                             Bitmap%)
                    (except-in pict3d pict3d)
                    pict3d/universe))

(provide (all-defined-out)
         (all-from-out scribble/eval)
         (all-from-out racket/runtime-path)
         (all-from-out "serializable-bitmap.rkt")
         (all-from-out racket/draw)
         (for-label (all-from-out
                     racket/base
                     racket/gui/base
                     racket/class
                     racket/flonum
                     racket/match
                     images/flomap
                     2htdp/universe
                     typed/racket/base
                     pict3d
                     pict3d/universe)))

(define (author-email) "neil.toronto@gmail.com")

(define-runtime-path log-file "pict3d-log.rktd")

(define err (current-error-port))

(define pict3d-eval
  (let ([eval  (make-log-based-eval log-file eval-mode)])
    (eval '(begin
             (require (for-syntax racket/base)
                      racket/flonum
                      racket/math
                      racket/match
                      racket/pretty
                      (rename-in pict3d/private/lazy-gui
                                 [pict3d->bitmap orig-pict3d->bitmap])
                      pict3d/scribblings/sniplike-bitmap
                      pict3d/scribblings/serializable-bitmap)
             
             (define (pict3d->bitmap v)
               (serializable-bitmap (orig-pict3d->bitmap v) 'jpeg))
             
             (define (pict3d->png-bitmap v)
               (serializable-bitmap (orig-pict3d->bitmap v) 'png))
             
             (define (render-pict3d v)
               (cond [(pict3d? v)
                      (serializable-bitmap
                       (pict3d->sniplike-bitmap v (current-pict3d-width) (current-pict3d-height))
                       'jpeg)]
                     [(is-a? v bitmap%)
                      (serializable-bitmap v 'jpeg)]
                     [else  v]))
             
             (define-syntax (render-pict3ds stx)
               (syntax-case stx ()
                 [(_ e)
                  (syntax-case (local-expand #'e 'top-level #f) (define-values #%require)
                    [(define-values . body)  #'e]
                    [(#%require . body)  #'e]
                    [_  #'(render-pict3d e)])]))
             ))
    (λ (v)
      (if (pair? v)
          (eval `(render-pict3ds ,v))
          (eval v)))))

(define normal-eval
  (let ([eval  (make-base-eval)])
    (eval '(begin
             (require pict3d/private/lazy-gui
                      racket/flonum
                      racket/math
                      racket/match)))
    eval))

(define (close-evals)
  (close-eval pict3d-eval)
  (close-eval normal-eval))
