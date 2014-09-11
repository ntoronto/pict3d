#lang s-exp typed-racket/base-env/extra-env-lang

(require (only-in ffi/unsafe ptr-set!)
         typed/racket/base
         typed/opengl/ffi-types
         (for-syntax (submod typed/opengl/ffi-types #%type-decl)))

(type-environment
 [ptr-set!  (parse-type #'(-> CPointer CType Integer Any Void))]
 )
