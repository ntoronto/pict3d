#lang racket/base

(require racket/flonum
         racket/unsafe/ops
         typed/untyped-utils
         typed/racket/base
         (except-in "typed-flt3.rkt"
                    flt3inverse
                    flt3compose))

(require/untyped-contract
 (begin (require (only-in "typed-flt3.rkt"
                          FlTransform3)))
 "typed-flt3.rkt"
 [flt3inverse  (-> FlTransform3 FlTransform3)]
 [flt3compose  (-> FlTransform3 FlTransform3 FlTransform3)])

(provide (all-from-out "typed-flt3.rkt")
         flt3inverse
         flt3compose)
