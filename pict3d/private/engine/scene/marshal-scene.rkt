#lang typed/racket/base

(require racket/flonum
         "marshal.rkt"
         "../../math/flt3.rkt"
         "../../math/flv3.rkt"
         "../../math/flrect3.rkt"
         "../types.rkt"
         "../scene.rkt"
         "tags.rkt")

(provide (all-defined-out))

(define material/m
  (struct/m
   material
   ([flonum/m Flonum values]
    [flonum/m Flonum values]
    [flonum/m Flonum values]
    [flonum/m Flonum values])))

(define flidentity3/m
  (singleton/m FlIdentity3 identity-flt3))

(define fllinear3/m
  (struct/m
   FlLinear3
   ([flvector/m FlVector (λ (v) (= 9 (flvector-length v)))]
    [flvector/m FlVector (λ (v) (= 9 (flvector-length v)))]
    [flonum/m Flonum values]
    [flonum/m Flonum values])))

(define flaffine3/m
  (struct/m
   FlAffine3
   ([flvector/m FlVector (λ (v) (= 12 (flvector-length v)))]
    [flvector/m FlVector (λ (v) (= 12 (flvector-length v)))]
    [flonum/m Flonum values]
    [flonum/m Flonum values])))

(define flprojective3/m
  (struct/m
   FlProjective3
   ([flvector/m FlVector (λ (v) (= 16 (flvector-length v)))]
    [flvector/m FlVector (λ (v) (= 16 (flvector-length v)))]
    [flonum/m Flonum values]
    [flonum/m Flonum values])))

(define fllinear3-/m
  (union/m ([fllinear3/m FlLinear3 fllinear3?]
            [flidentity3/m FlIdentity3 values])))

(define flaffine3-/m
  (union/m ([flaffine3/m FlAffine3 flaffine3?]
            [fllinear3-/m FlLinear3- values])))

(define fltransform3/m
  (union/m ([flprojective3/m FlProjective3 flprojective3?]
            [flaffine3-/m FlAffine3- values])))

(define affine/m
  (opaque/m Affine affine ([flaffine3-/m FlAffine3- affine-transform])))

(define nonempty-flrect3/m
  (opaque/m
   Nonempty-FlRect3
   nonempty-flrect3
   ([flvector/m FlVector nonempty-flrect3-min]
    [flvector/m FlVector nonempty-flrect3-max])))

;; ===================================================================================================
;; Shapes

(define vertices/m
  (vectorof/m [flvector/m FlVector values]))

(define materials/m
  (vectorof/m [material/m material values]))

(define vertex-data/m
  (union/m ([flvector/m FlVector flvector?]
            [vertices/m (Vectorof FlVector) values])))

(define vertex-material/m
  (union/m ([material/m material material?]
            [materials/m (Vectorof material) values])))

(define triangle-shape/m
  (opaque/m
   triangle-shape
   make-triangle-shape
   ([vertices/m (Vectorof FlVector) triangle-shape-vertices]
    [vertex-data/m (U FlVector (Vectorof FlVector)) triangle-shape-normals]
    [vertex-data/m (U FlVector (Vectorof FlVector)) triangle-shape-colors]
    [vertex-data/m (U FlVector (Vectorof FlVector)) triangle-shape-emitteds]
    [vertex-material/m (U material (Vectorof material)) triangle-shape-materials]
    [boolean/m Boolean triangle-shape-back?])))

(define rectangle-shape/m
  (opaque/m
   rectangle-shape
   make-rectangle-shape
   ([nonempty-flrect3/m Nonempty-FlRect3 rectangle-shape-rect]
    [flvector/m FlVector rectangle-shape-color]
    [flvector/m FlVector rectangle-shape-emitted]
    [material/m material rectangle-shape-material]
    [boolean/m Boolean rectangle-shape-inside?])))

(define sphere-shape/m
  (opaque/m
   sphere-shape
   make-sphere-shape
   ([affine/m Affine sphere-shape-affine]
    [flvector/m FlVector sphere-shape-color]
    [flvector/m FlVector sphere-shape-emitted]
    [material/m material sphere-shape-material]
    [boolean/m Boolean sphere-shape-inside?])))

(define directional-light-shape/m
  (opaque/m
   directional-light-shape
   make-directional-light-shape
   ([flvector/m FlVector light-shape-emitted]
    [flvector/m FlVector directional-light-shape-direction])))

(define point-light-shape/m
  (opaque/m
   point-light-shape
   make-point-light-shape
   ([flvector/m FlVector light-shape-emitted]
    [flvector/m FlVector point-light-shape-position]
    [flonum/m Flonum point-light-shape-min-radius]
    [flonum/m Flonum point-light-shape-max-radius])))

(define point-light-shell-shape/m
  (opaque/m
   point-light-shell-shape
   make-point-light-shell-shape
   ([flvector/m FlVector point-light-shell-shape-emitted]
    [flvector/m FlVector point-light-shell-shape-position]
    [flonum/m Flonum point-light-shell-shape-min-radius]
    [flonum/m Flonum point-light-shell-shape-max-radius])))

(: frozen-scene-shape/m (Marshaller frozen-scene-shape))
(define frozen-scene-shape/m
  (delay/m
   [(opaque/m
     frozen-scene-shape
     make-frozen-scene-shape
     ([nonempty-scene/m Nonempty-Scene frozen-scene-shape-scene]))
    frozen-scene-shape]))

(: shape/m (Marshaller Shape))
(define shape/m
  (union/m ([triangle-shape/m triangle-shape triangle-shape?]
            [rectangle-shape/m rectangle-shape rectangle-shape?]
            [sphere-shape/m sphere-shape sphere-shape?]
            [directional-light-shape/m directional-light-shape directional-light-shape?]
            [point-light-shape/m point-light-shape point-light-shape?]
            [point-light-shell-shape/m point-light-shell-shape point-light-shell-shape?]
            [frozen-scene-shape/m frozen-scene-shape frozen-scene-shape?])))

;; ===================================================================================================
;; Scenes

(define tag/m
  (union/m ([symbol/m Symbol symbol?]
            [integer/m Integer values])))

(define empty-scene/m
  (singleton/m Empty-Scene empty-scene))

(: nonempty-scene/m (Marshaller Nonempty-Scene))
(define nonempty-scene/m
  (delay/m
   [(union/m ([leaf-scene/m leaf-scene leaf-scene?]
              [node-scene/m node-scene node-scene?]
              [trans-scene/m trans-scene trans-scene?]
              [group-scene/m group-scene group-scene?]))
    Nonempty-Scene]))

(: scene/m (Marshaller Scene))
(define scene/m
  (union/m ([empty-scene/m Empty-Scene empty-scene?]
            [nonempty-scene/m Nonempty-Scene nonempty-scene?])))

(define leaf-scene/m
  (opaque/m
   leaf-scene
   shape->scene
   ([shape/m Shape leaf-scene-shape])))

(: node-scene/m (Marshaller node-scene))
(define node-scene/m
  (opaque/m
   node-scene
   make-nonempty-node-scene
   ([nonempty-scene/m Nonempty-Scene node-scene-neg]
    [nonempty-scene/m Nonempty-Scene node-scene-pos])))

(: trans-scene/m (Marshaller trans-scene))
(define trans-scene/m
  (opaque/m
   trans-scene
   make-simple-trans-scene
   ([affine/m Affine trans-scene-affine]
    [nonempty-scene/m Nonempty-Scene trans-scene-scene])))

(: group-scene/m (Marshaller group-scene))
(define group-scene/m
  (opaque/m
   group-scene
   make-group-scene
   ([tag/m Tag group-scene-tag]
    [scene/m Scene group-scene-scene])))

;; ===================================================================================================
;; Untyped entry points

(: marshal-scene (-> Scene Marshalled-Value))
(define (marshal-scene s)
  (marshal* scene/m s))

(: unmarshal-scene (-> Marshalled-Value Scene))
(define (unmarshal-scene s)
  (unmarshal* scene/m s))
