#lang racket/base

(require typed/untyped-utils
         (except-in "scene/types.rkt"
                    scene-rect
                    scene-count)
         "scene/shape.rkt"
         "scene/scene.rkt")

(require/untyped-contract
 (begin (require (only-in "scene/types.rkt"                          
                          Scene)
                 (only-in "../math/flrect3.rkt"
                          FlRect3)))
 "scene/types.rkt"
 [scene-rect  (-> Scene FlRect3)]
 [scene-count  (-> Scene Nonnegative-Fixnum)])

(provide (all-from-out
          "scene/types.rkt"
          "scene/shape.rkt"
          "scene/scene.rkt")
         scene-rect
         scene-count)
