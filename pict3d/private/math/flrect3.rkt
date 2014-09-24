#lang racket/base

(require typed/untyped-utils
         (except-in "typed-flrect3.rkt"
                    flrect3-center))

(require/untyped-contract
 (begin (require (only-in "typed-flrect3.rkt"
                          FlRect3)))
 "typed-flrect3.rkt"
 [flrect3-center  (-> FlRect3 (U #f FlVector))])

(provide (all-from-out
          "typed-flrect3.rkt")
         flrect3-center)
