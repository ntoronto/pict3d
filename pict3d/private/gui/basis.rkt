#lang typed/racket/base

(require racket/promise
         math/flonum
         "../math/flt3.rkt"
         "../engine/scene.rkt"
         "axes-scene.rkt"
         )

(provide (rename-out [-Basis Basis]
                     [Basis? basis?]
                     [Basis-forward basis-forward]
                     [Basis-inverse basis-inverse]
                     [Basis-scene basis-scene])
         basis)

(struct Basis ([forward : FlAffine3-]
               [inverse : FlAffine3-]
               [scene : (Promise Scene)]))

(define-type -Basis Basis)

(define smaller-flt3 (scale-flt3 (flvector 0.5 0.5 0.5)))
(define bigger-flt3 (scale-flt3 (flvector 2.0 2.0 2.0)))

(: basis (->* [FlAffine3-] [FlAffine3-] Basis))
(define (basis t [tinv (flt3inverse t)])
  (Basis t
         tinv
         (delay (scene-transform
                 axes-scene
                 (flt3compose t smaller-flt3)
                 (flt3compose bigger-flt3 tinv)))))
