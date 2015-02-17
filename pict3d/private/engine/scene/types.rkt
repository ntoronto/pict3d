#lang typed/racket/base

(require racket/match
         racket/list
         "../../math/flt3.rkt"
         "../../math/flrect3.rkt"
         "../../utils.rkt"
         "../../gl.rkt"
         "../draw-pass.rkt"
         "../types.rkt"
         "tags.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Shape types

(struct shape ([lazy-passes : (HashTable GL-Context Passes)]))

(: lazy-passes (-> (HashTable GL-Context Passes)))
(define lazy-passes make-weak-hasheq)

(struct solid-shape shape () #:transparent)

(struct triangle-shape solid-shape
  ([vertices : (Vectorof FlVector)]
   [normals : (U FlVector (Vectorof FlVector))]
   [colors : (U FlVector (Vectorof FlVector))]
   [emitted-colors : (U FlVector (Vectorof FlVector))]
   [materials : (U material (Vectorof material))]
   [back? : Boolean])
  #:transparent)

(struct rectangle-shape solid-shape
  ([rect : Nonempty-FlRect3]
   [color : FlVector]
   [emitted-color : FlVector]
   [material : material]
   [inside? : Boolean])
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
  ([rect : Nonempty-FlRect3])
  #:transparent)

(struct container-scene nonempty-scene
  ([count : Nonnegative-Fixnum]
   [tags : Tags])
  #:transparent)

(struct leaf-scene nonempty-scene
  ([shape : Shape])
  #:transparent)

(struct node-scene container-scene
  ([neg : Nonempty-Scene]
   [pos : Nonempty-Scene])
  #:transparent)

(struct trans-scene container-scene
  ([affine : FlAffine3-]
   [scene : Nonempty-Scene])
  #:transparent)

(struct group-scene container-scene
  ([tag : Tag]
   [scene : Scene])
  #:transparent)

(define-type Nonempty-Scene
  (U leaf-scene
     node-scene
     trans-scene
     group-scene))

(define-type Scene (U Empty-Scene Nonempty-Scene))

(define empty-scene (Empty-Scene))
(define empty-scene? Empty-Scene?)

(: scene-rect (case-> (-> Nonempty-Scene Nonempty-FlRect3)
                      (-> Scene FlRect3)))
(define (scene-rect s)
  (if (empty-scene? s) empty-flrect3 (nonempty-scene-rect s)))

(: scene-count (-> Scene Nonnegative-Fixnum))
(define (scene-count s)
  (cond [(empty-scene? s)  0]
        [(leaf-scene? s)  1]
        [else  (container-scene-count s)]))

(: scene-tags (-> Scene Tags))
(define (scene-tags s)
  (cond [(empty-scene? s)  empty-tags]
        [(leaf-scene? s)  empty-tags]
        [else  (container-scene-tags s)]))
