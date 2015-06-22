#lang racket/base

(require (for-syntax racket/base)
         racket/match
         racket/flonum
         (rename-in
          "typed-user-types.rkt"
          [interval -interval]
          [arc -arc]
          [rgba  -rgba]
          [emitted  -emitted]
          [pos  -pos]
          [dir  -dir]
          [linear  -linear]
          [affine  -affine]))

(provide (except-out
          (all-from-out "typed-user-types.rkt")
          interval-values
          arc-values
          flv4-values
          flv4->rgba
          flv4->emitted
          flv4->material
          flv3-values
          flv3->pos
          flv3->dir
          vtx->vertex
          face->vertices
          fllinear3->linear
          flafflin3->affine
          flaffine3->affine
          fldiff3->smooth
          flsmooth3->smooth
          trace-data->surface-data
          make-material
          make-vertex
          make-surface-data
          -interval
          -arc
          -pos
          -dir
          -rgba
          -emitted
          -linear
          -affine
          linear->cols*
          affine->cols*)
         interval
         arc
         pos
         dir
         rgba
         emitted
         linear
         affine)

(define-for-syntax (make-head-expander id)
  (λ (stx)
    (syntax-case stx ()
      [(_ . args)  (quasisyntax/loc stx (#,id . args))]
      [_  (quasisyntax/loc stx #,id)])))

(define-match-expander interval
  (λ (stx)
    (syntax-case stx ()
      [(_ x y)
       (syntax/loc stx
         (? interval? (app interval-values x y)))]))
  (make-head-expander #'-interval))

(define-match-expander arc
  (λ (stx)
    (syntax-case stx ()
      [(_ x y)
       (syntax/loc stx
         (? arc? (app arc-values x y)))]))
  (make-head-expander #'-arc))

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

(define-match-expander linear
  (λ (stx)
    (syntax-case stx ()
      [(_ dx dy dz)
       (syntax/loc stx
         (? linear? (app linear->cols* dx dy dz)))]))
  (make-head-expander #'-linear))

(define-match-expander affine
  (λ (stx)
    (syntax-case stx ()
      [(_ dx dy dz p)
       (syntax/loc stx
         (? affine? (app affine->cols* dx dy dz p)))]))
  (make-head-expander #'-affine))
