#lang typed/racket/base

(require "../../../math.rkt"
         "../../scene.rkt")

(provide (all-defined-out))

(struct sphere-shape shape
  ([affine : FlAffine3]
   [color : FlV4]
   [emitted : FlV4]
   [material : FlV4]
   [inside? : Boolean])
  #:transparent)
