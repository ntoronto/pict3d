#lang racket/base

(define eval-mode 'record)
;(define eval-mode 'replay)

(require racket/match
         scribble/eval
         unstable/sandbox
         racket/runtime-path
         (for-label racket
                    racket/gui/base
                    pict3d
                    pict3d/universe))

(provide (all-defined-out)
         (all-from-out scribble/eval)
         (for-label (all-from-out racket
                                  racket/gui/base
                                  pict3d
                                  pict3d/universe)))

(define (author-email) "neil.toronto@gmail.com")

(define-runtime-path log-file "pict3d-log.rktd")

(define pict3d-eval
  (let ([eval  (make-log-based-eval log-file eval-mode)])
    (eval '(begin
             (require (for-syntax racket/base)
                      racket/match
                      pict3d/private/lazy-gui
                      pict3d/private/render-client
                      pict3d/scribblings/serializable-bitmap)
             (start-render-server)
             
             (define (render-pict3d v)
               (if (pict3d? v)
                   (serializable-bitmap (request-render v #:as-snip? #t))
                   v))
             
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

(define (close-pict3d-eval)
  (pict3d-eval '(stop-render-server))
  (close-eval pict3d-eval))
