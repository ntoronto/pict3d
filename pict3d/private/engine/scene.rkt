#lang racket/base

(require typed/untyped-utils
         (except-in "scene/types.rkt"
                    scene-count)
         (except-in "scene/scene.rkt"
                    scene-rect)
         "scene/shape.rkt")

(require/untyped-contract
 (begin (require (only-in "scene/types.rkt" Scene)))
 "scene/types.rkt"
 [scene-count  (-> Scene Nonnegative-Fixnum)])

(require/untyped-contract
 (begin (require (only-in "scene/types.rkt" Scene)
                 (only-in "../math/flrect3.rkt" FlRect3)))
 "scene/scene.rkt"
 [scene-rect  (-> Scene FlRect3)])

(provide (all-from-out
          "scene/types.rkt"
          "scene/shape.rkt"
          "scene/scene.rkt")
         scene-rect
         scene-count)
