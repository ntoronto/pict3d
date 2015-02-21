#lang s-exp typed-racket/base-env/extra-env-lang

(require "untyped-pict3d-canvas.rkt"
         "typed-pict3d-canvas.rkt"
         (for-syntax (submod "typed-pict3d-canvas.rkt" #%type-decl)))

(provide Pict3D-Canvas%)

(type-environment
 [pict3d-canvas%  (parse-type #'Pict3D-Canvas%)]
 )
