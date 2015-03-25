#lang typed/racket/base

(require (for-syntax racket/base)
         racket/match
         racket/list
         "../../math.rkt"
         "../../gl.rkt"
         "../draw-pass.rkt"
         "../types.rkt"
         "tags.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Polygon vertices

(struct vtx ([position : FlV3]
             [normal : FlV3]
             [color : FlV4]
             [emitted : FlV4]
             [material : FlV4])
  #:transparent)

(: vtx-set-vecs (-> vtx FlV3 FlV3 vtx))
(define (vtx-set-vecs v pos norm)
  (vtx pos norm (vtx-color v) (vtx-emitted v) (vtx-material v)))

(: vtx-set-color (-> vtx FlV4 vtx))
(define (vtx-set-color v c)
  (vtx (vtx-position v) (vtx-normal v) c (vtx-emitted v) (vtx-material v)))

(: vtx-set-emitted (-> vtx FlV4 vtx))
(define (vtx-set-emitted v e)
  (vtx (vtx-position v) (vtx-normal v) (vtx-color v) e (vtx-material v)))

(: vtx-set-material (-> vtx FlV4 vtx))
(define (vtx-set-material v m)
  (vtx (vtx-position v) (vtx-normal v) (vtx-color v) (vtx-emitted v) m))

;; ===================================================================================================
;; Shape types

(struct shape ([lazy-passes : (HashTable GL-Context passes)]))

(: lazy-passes (-> (HashTable GL-Context passes)))
(define lazy-passes make-weak-hasheq)

(struct solid-shape shape () #:transparent)

(struct triangle-shape solid-shape
  ([vtx1 : vtx]
   [vtx2 : vtx]
   [vtx3 : vtx]
   [back? : Boolean])
  #:transparent)

(struct rectangle-shape solid-shape
  ([axial-rect : FlRect3]
   [color : FlV4]
   [emitted : FlV4]
   [material : FlV4]
   [inside? : Boolean])
  #:transparent)

(struct sphere-shape solid-shape
  ([affine : FlAffine3]
   [color : FlV4]
   [emitted : FlV4]
   [material : FlV4]
   [inside? : Boolean])
  #:transparent)

(struct light-shape shape ([emitted : FlV4]) #:transparent)
(struct directional-light-shape light-shape ([direction : FlV3]) #:transparent)
(struct point-light-shape light-shape ([affine : FlAffine3]
                                       [min-radius : Flonum]
                                       [max-radius : Flonum])
  #:transparent)

(struct indicator-shape shape () #:transparent)
(struct point-light-shell-shape indicator-shape
  ([emitted : FlV4]
   [affine : FlAffine3]
   [min-radius : Flonum]
   [max-radius : Flonum])
  #:transparent)

(struct frozen-scene-shape shape
  ([scene : Nonempty-Scene])
  #:transparent)

(define-type Shape
  (U triangle-shape
     rectangle-shape
     sphere-shape
     directional-light-shape
     point-light-shape
     point-light-shell-shape
     frozen-scene-shape))

;; ===================================================================================================
;; Bounding boxes

(define axial-tol 1e-14)
(define tight-badness 1e-14)

(struct bbox ([rect : FlRect3] [badness : Nonnegative-Flonum])
  #:transparent
  #:mutable)

(define zero-bbox (bbox zero-flrect3 0.0))

(: maybe-bbox-rect (-> (U #f bbox) (U #f FlRect3)))
(define (maybe-bbox-rect b)
  (and b (bbox-rect b)))

(: maybe-bbox-badness (-> (U #f bbox) Nonnegative-Flonum))
(define (maybe-bbox-badness b)
  (if b (bbox-badness b) 0.0))

(: bbox-transform (-> bbox FlAffine3 bbox))
(define (bbox-transform b t)
  (define-values (new-r new-badness)
    (flrect3-transform/badness (bbox-rect b) t))
  (bbox new-r (+ (bbox-badness b) new-badness)))

(: maybe-bbox-transform (-> (U #f bbox) FlAffine3 (U #f bbox)))
(define (maybe-bbox-transform b t)
  (and b (bbox-transform b t)))

(: bbox-join (-> bbox bbox bbox))
(define (bbox-join b1 b2)
  (bbox (flrect3-join (bbox-rect b1) (bbox-rect b2))
        (max (bbox-badness b1) (bbox-badness b2))))

(: maybe-bbox-join (-> (U #f bbox) (U #f bbox) (U #f bbox)))
(define (maybe-bbox-join b1 b2)
  (cond [(not b1)  b2]
        [(not b2)  b1]
        [else  (bbox-join b1 b2)]))

(: bbox-appx-contains-bbox? (-> bbox bbox Boolean))
(define (bbox-appx-contains-bbox? b1 b2)
  (flrect3-contains-rect? (bbox-rect b1) (bbox-rect b2)))

(: maybe-bbox-appx-contains-bbox? (-> (U #f bbox) (U #f bbox) Boolean))
(define (maybe-bbox-appx-contains-bbox? b1 b2)
  (cond [(not b2)  #t]
        [(not b1)  #f]
        [else  (bbox-appx-contains-bbox? b1 b2)]))

(: bbox-appx-line-intersects (-> bbox FlV3 FlV3 (Values (U #f Flonum) (U #f Flonum))))
(define (bbox-appx-line-intersects b v dv)
  (flrect3-line-intersects (bbox-rect b) v dv))

(: maybe-bbox-appx-line-intersects (-> (U #f bbox) FlV3 FlV3 (Values (U #f Flonum) (U #f Flonum))))
(define (maybe-bbox-appx-line-intersects b v dv)
  (if b (bbox-appx-line-intersects b v dv) (values #f #f)))

(: bbox-appx-classify/planes (-> bbox (Listof FlPlane3) (U 'inside 'outside 'both)))
(define (bbox-appx-classify/planes b planes)
  (flrect3-classify/planes (bbox-rect b) planes))

(: maybe-bbox-appx-classify/planes (-> (U #f bbox) (Listof FlPlane3) (U 'inside 'outside 'both)))
(define (maybe-bbox-appx-classify/planes b planes)
  (if b (bbox-appx-classify/planes b planes) 'inside))

(: bbox-appx-plane-side (-> bbox FlPlane3 Rect-Plane-Sides))
(define (bbox-appx-plane-side b plane)
  (flrect3-plane-side (bbox-rect b) plane))

(: maybe-bbox-appx-plane-side (-> (U #f bbox) FlPlane3 (U #f Rect-Plane-Sides)))
(define (maybe-bbox-appx-plane-side b plane)
  (and b (bbox-appx-plane-side b plane)))

(: bbox-appx-disjoint? (-> bbox bbox Boolean))
(define (bbox-appx-disjoint? b1 b2)
  (flrect3-disjoint? (bbox-rect b1) (bbox-rect b2)))

(: maybe-bbox-appx-disjoint? (-> (U #f bbox) (U #f bbox) Boolean))
(define (maybe-bbox-appx-disjoint? b1 b2)
  (if (and b1 b2)
      (bbox-appx-disjoint? b1 b2)
      #t))

;; ===================================================================================================
;; Scene types

(struct Empty-Scene () #:transparent)
(define empty-scene (Empty-Scene))
(define-syntax empty-scene? (make-rename-transformer #'Empty-Scene?))

(struct nonempty-scene () #:transparent)

(struct node-scene nonempty-scene
  ([visible-bbox : (U #f bbox)]
   [invisible-bbox : (U #f bbox)]
   [count : Nonnegative-Fixnum]
   [child-tags : Tags]
   [neg : Nonempty-Scene]
   [pos : Nonempty-Scene])
  #:transparent)

(struct trans-scene nonempty-scene
  ([scene : Nonempty-Scene]
   [affine : FlAffine3])
  #:transparent)

(struct group-scene nonempty-scene
  ([scene : Scene]
   [tag : Tag])
  #:transparent)

(define-type Nonempty-Scene
  (U Shape
     node-scene
     trans-scene
     group-scene))

(define-type Scene (U Empty-Scene Nonempty-Scene))

(: scene-count (-> Scene Nonnegative-Fixnum))
(define (scene-count s)
  (cond [(empty-scene? s)  0]
        [(node-scene? s)  (node-scene-count s)]
        [(trans-scene? s)  (scene-count (trans-scene-scene s))]
        [(group-scene? s)  (scene-count (group-scene-scene s))]
        [else  1]))

(: scene-tags (-> Scene Tags))
(define (scene-tags s)
  (cond [(empty-scene? s)  empty-tags]
        [(node-scene? s)  (node-scene-child-tags s)]
        [(trans-scene? s)  (scene-tags (trans-scene-scene s))]
        [(group-scene? s)  (tags-add (scene-tags (group-scene-scene s))
                                     (group-scene-tag s))]
        [else  empty-tags]))

;; ===================================================================================================
;; Ray-scene intersection types

(struct line-hit ([distance : Flonum]
                  [point : FlV3]
                  [normal : (U #f FlV3)])
  #:transparent
  #:mutable)
