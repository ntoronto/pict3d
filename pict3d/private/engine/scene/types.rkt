#lang typed/racket/base

(require racket/match
         racket/list
         "../../math/flt3.rkt"
         "../../math/flrect3.rkt"
         "../../utils.rkt"
         "../draw-pass.rkt"
         "../affine.rkt"
         "../gl.rkt"
         "../types.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Shape types

(struct shape ([lazy-passes : (Lazy-Box Passes)]) #:transparent)

(struct solid-shape shape () #:transparent)

(struct triangle-shape solid-shape
  ([vertices : (Vectorof FlVector)]
   [normals : (U FlVector (Vectorof FlVector))]
   [colors : (U FlVector (Vectorof FlVector))]
   [emitted-colors : (U FlVector (Vectorof FlVector))]
   [materials : (U material (Vectorof material))]
   [face : Face])
  #:transparent)

(struct quad-shape solid-shape
  ([vertices : (Vectorof FlVector)]
   [normals : (U FlVector (Vectorof FlVector))]
   [colors : (U FlVector (Vectorof FlVector))]
   [emitted-colors : (U FlVector (Vectorof FlVector))]
   [materials : (U material (Vectorof material))]
   [face : Face])
  #:transparent)

(struct rectangle-shape solid-shape
  ([rect : Nonempty-FlRect3]
   [color : FlVector]
   [emitted-color : FlVector]
   [material : material]
   [face : Face])
  #:transparent)

(struct sphere-shape solid-shape
  ([affine : affine]
   [color : FlVector]
   [emitted-color : FlVector]
   [material : material]
   [inside? : Boolean])
  #:transparent)

(struct light-shape shape ([color : FlVector] [intensity : Flonum]) #:transparent)
(struct directional-light-shape light-shape ([direction : FlVector]) #:transparent)
(struct point-light-shape light-shape ([position : FlVector] [radius : Flonum]) #:transparent)

(struct frozen-scene-shape shape
  ([scene : Nonempty-Scene])
  #:transparent)

(define-type Shape (U triangle-shape
                      quad-shape
                      rectangle-shape
                      sphere-shape
                      directional-light-shape
                      point-light-shape
                      frozen-scene-shape))

;; ===================================================================================================
;; Scene types

(struct scene () #:transparent)

(struct Empty-Scene scene () #:transparent)

(struct nonempty-scene scene
  ([rect : Nonempty-FlRect3]
   [count : Positive-Fixnum])
  #:transparent)

(struct scene-leaf nonempty-scene
  ([shape : Shape])
  #:transparent)

(struct scene-node nonempty-scene
  ([neg : Nonempty-Scene]
   [pos : Nonempty-Scene])
  #:transparent)

(struct scene-tran nonempty-scene
  ([transform : FlAffine3-]
   [inverse : FlAffine3-]
   [scene : Nonempty-Scene])
  #:transparent)

(define-type Nonempty-Scene
  (U scene-leaf
     scene-node
     scene-tran))

(define-type Scene (U Empty-Scene Nonempty-Scene))

(define empty-scene (Empty-Scene))
(define empty-scene? Empty-Scene?)

(: scene-rect (case-> (-> Nonempty-Scene Nonempty-FlRect3)
                      (-> Scene FlRect3)))
(define (scene-rect s)
  (if (empty-scene? s) empty-flrect3 (nonempty-scene-rect s)))

(: scene-count (case-> (-> Nonempty-Scene Positive-Fixnum)
                       (-> Scene Nonnegative-Fixnum)))
(define (scene-count s)
  (if (empty-scene? s) 0 (nonempty-scene-count s)))
