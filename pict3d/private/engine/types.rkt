#lang typed/racket/base

(require "gl.rkt")

(provide (all-defined-out))

(define-type (List-Hash A B) (Listof (Pair A B)))

(struct program-spec ([program : gl-object]
                      [uniforms : (List-Hash String (U Symbol Uniform))]
                      [struct : vao-struct])
  #:transparent)

(struct material ([ambient : Flonum]
                  [diffuse : Flonum]
                  [specular : Flonum]
                  [roughness : Flonum])
  #:transparent)
