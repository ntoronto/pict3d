#lang typed/racket/base

(require racket/promise
         math/flonum
         "../math/flt3.rkt"
         "../engine/scene.rkt"
         "axes-scene.rkt"
         )

(provide (rename-out [-Basis Basis]
                     [Basis? basis?]
                     [Basis-transform basis-transform]
                     [Basis-scene basis-scene])
         basis)

(struct Basis ([transform : FlAffine3-]
               [scene : (Promise Scene)]))

(define-type -Basis Basis)

(define smaller-flt3 (scale-flt3 (flvector 0.5 0.5 0.5)))

(: basis (-> FlAffine3- Basis))
(define (basis t)
  (Basis t
         (delay (scene-transform
                 axes-scene
                 (flt3compose t smaller-flt3)))))
