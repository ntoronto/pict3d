#lang info

(define collection 'multi)

(define deps '(("base" #:version "6.1.1")
               "draw-lib"
               "srfi-lite-lib"
               "typed-racket-lib"
               "typed-racket-more"
               "math-lib"
               "scribble-lib"
               "gui-lib"
               "pconvert-lib"
               "pict-lib" ; why?
               "profile-lib" ; for tests
               ))

(define build-deps '("draw-doc"
                     "gui-doc"
                     "gui-lib"
                     "racket-doc"
                     "plot-lib"  ; why?
                     "htdp-doc"
                     "htdp-lib"  ; why?
                     "pict-doc"
                     "typed-racket-doc"
                     ))

(define pkg-desc "Pict3D: Functional 3D Scenes")
(define pkg-authors '(ntoronto))
