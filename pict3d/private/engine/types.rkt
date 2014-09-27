#lang typed/racket/base

(require "../gl.rkt"
         "../utils.rkt")

(provide (all-defined-out))

(struct program-spec ([program : gl-program]
                      [uniforms : (List-Hash String (U Symbol Uniform))])
  #:transparent)

(struct material ([ambient : Flonum]
                  [diffuse : Flonum]
                  [specular : Flonum]
                  [roughness : Flonum])
  #:transparent)
