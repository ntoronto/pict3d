#lang racket/base

(require (for-syntax racket/base)
         racket/match
         racket/flonum
         (rename-in
          "gui/user-types.rkt"
          [rgba  -rgba]
          [emitted  -emitted]
          [pos  -pos]
          [dir  -dir])
         "gui/parameters.rkt"
         "gui/pict3d-struct.rkt"
         "gui/pict3d-combinators.rkt"
         "gui/pict3d-bitmap.rkt")

(provide (except-out
          (all-from-out
           "gui/user-types.rkt"
           "gui/parameters.rkt"
           "gui/pict3d-struct.rkt"
           "gui/pict3d-combinators.rkt"
           "gui/pict3d-bitmap.rkt")
          col-flvector
          vec-flvector
          make-vertex
          make-material
          -pos
          -dir
          -rgba
          -emitted)
         with-color
         with-emitted
         with-material
         pos
         dir
         rgba
         emitted)

(define-syntax-rule (with-color c body ...)
  (parameterize ([current-color c]) body ...))

(define-syntax-rule (with-emitted e body ...)
  (parameterize ([current-emitted e]) body ...))

(define-syntax-rule (with-material m body ...)
  (parameterize ([current-material m]) body ...))

(define-for-syntax (make-head-expander id)
  (λ (stx)
    (syntax-case stx ()
      [(_ . args)  (quasisyntax/loc stx (#,id . args))]
      [_  (quasisyntax/loc stx #,id)])))

(define-match-expander pos
  (λ (stx)
    (syntax-case stx ()
      [(_ x y z)
       (syntax/loc stx
         (? pos?
            (app (λ (p)
                   (let ([v  (vec-flvector p)])
                     (values (flvector-ref v 0)
                             (flvector-ref v 1)
                             (flvector-ref v 2))))
                 x y z)))]))
  (make-head-expander #'-pos))

(define-match-expander dir
  (λ (stx)
    (syntax-case stx ()
      [(_ x y z)
       (syntax/loc stx
         (? dir?
            (app (λ (p)
                   (let ([v  (vec-flvector p)])
                     (values (flvector-ref v 0)
                             (flvector-ref v 1)
                             (flvector-ref v 2))))
                 x y z)))]))
  (make-head-expander #'-dir))

(define-match-expander rgba
  (λ (stx)
    (syntax-case stx ()
      [(_ r g b a)
       (syntax/loc stx
         (? rgba?
            (app (λ (p)
                   (let ([v  (col-flvector p)])
                     (values (flvector-ref v 0)
                             (flvector-ref v 1)
                             (flvector-ref v 2)
                             (flvector-ref v 3))))
                 r g b a)))]))
  (make-head-expander #'-rgba))

(define-match-expander emitted
  (λ (stx)
    (syntax-case stx ()
      [(_ r g b i)
       (syntax/loc stx
         (? emitted?
            (app (λ (p)
                   (let ([v  (col-flvector p)])
                     (values (flvector-ref v 0)
                             (flvector-ref v 1)
                             (flvector-ref v 2)
                             (flvector-ref v 3))))
                 r g b i)))]))
  (make-head-expander #'-emitted))
