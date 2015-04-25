#lang racket/base

(require (for-syntax racket/base)
         racket/match
         racket/flonum
         (rename-in
          "typed-user-types.rkt"
          [rgba  -rgba]
          [emitted  -emitted]
          [pos  -pos]
          [dir  -dir]
          [affine  -affine]))

(provide (except-out
          (all-from-out "typed-user-types.rkt")
          flv4-values
          flv4->rgba
          flv4->emitted
          flv3-values
          flv3->pos
          flv3->dir
          flaffine3->affine
          trace-data->surface-data
          make-material
          make-vertex
          make-surface-data
          -pos
          -dir
          -rgba
          -emitted
          -affine
          affine->cols*)
         pos
         dir
         rgba
         emitted
         affine)

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
         (? pos? (app flv3-values x y z)))]))
  (make-head-expander #'-pos))

(define-match-expander dir
  (λ (stx)
    (syntax-case stx ()
      [(_ x y z)
       (syntax/loc stx
         (? dir? (app flv3-values x y z)))]))
  (make-head-expander #'-dir))

(define-match-expander rgba
  (λ (stx)
    (syntax-case stx ()
      [(_ r g b a)
       (syntax/loc stx
         (? rgba? (app flv4-values r g b a)))]))
  (make-head-expander #'-rgba))

(define-match-expander emitted
  (λ (stx)
    (syntax-case stx ()
      [(_ r g b i)
       (syntax/loc stx
         (? emitted? (app flv4-values r g b i)))]))
  (make-head-expander #'-emitted))

(define-match-expander affine
  (λ (stx)
    (syntax-case stx ()
      [(_ dx dy dz p)
       (syntax/loc stx
         (? affine? (app affine->cols* dx dy dz p)))]))
  (make-head-expander #'-affine))
