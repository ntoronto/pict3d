#lang s-exp typed-racket/base-env/extra-env-lang

(require (only-in "untyped-pict3d-bitmap.rkt" pict3d->bitmap)
         (only-in "pict3d-struct.rkt" Pict3D)
         (only-in typed/racket/base ->* Integer Instance)
         (only-in typed/racket/private/gui-types Bitmap%)
         (for-syntax (submod "pict3d-struct.rkt" #%type-decl)))

(type-environment
 [pict3d->bitmap  (parse-type #'(->* [Pict3D] [Integer Integer] (Instance Bitmap%)))]
 )
