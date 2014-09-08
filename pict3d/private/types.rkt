#lang typed/racket/base

(provide (all-defined-out))

(define-type Box-Plane-Sides (U 'negzero 'poszero 'both))

(define-type Plane-Sides (U 'neg 'negzero 'zero 'poszero 'pos 'nonzero))
