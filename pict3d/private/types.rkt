#lang typed/racket/base

(provide (all-defined-out))

(define-type Rect-Plane-Sides (U 'neg 'negzero 'zero 'poszero 'pos 'both))

(define-type Plane-Sides (U 'neg 'negzero 'zero 'poszero 'pos 'nonzero))
