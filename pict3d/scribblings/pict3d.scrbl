#lang scribble/manual

@(require "utils.rkt")

@title{Pict3D: Functional 3D Scenes}

@defmodule[pict3d]

@examples[#:eval pict3d-eval
                 (define x 3)
                 (require math/flonum)
                 (current-pict3d-add-sunlight? #f)
                 (current-pict3d-width 512)
                 (current-pict3d-height 512)
                 (combine (rectangle (pos -4 -4 -1) (pos 4 4 0))
                          (for/list ([_  (in-range 15)])
                            (light (pos (- (* 8 (random)) 4)
                                        (- (* 8 (random)) 4)
                                        1)
                                   (emitted "lightgreen")))
                          (basis 'camera (point-at (pos 4 5 1.5) origin)))]

@(close-pict3d-eval)
