#lang typed/racket/base

(require (for-syntax racket/base)
         racket/match
         racket/list
         racket/vector
         racket/promise
         "../../math.rkt"
         "../../math/flt3-unboxed-ops.rkt"
         "../../gl.rkt"
         "../draw.rkt"
         "tags.rkt")

(provide (all-defined-out))

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
;; Collision detection

(struct trace-data
  ([pos : FlV3]
   [norm : (U #f FlV3)]
   [path : (Listof Tag)])
  #:transparent)

;; ===================================================================================================
;; Shape types (most basic nonempty scenes)

(struct shape-functions
  ([get-passes : (-> shape passes)]
   [get-bbox : (-> shape (U 'visible 'invisible) FlAffine3 (U #f bbox))]
   [fast-transform : (-> shape FlAffine3 (U #f shape))]
   [deep-transform : (-> shape FlAffine3 (Listof shape))]
   [ray-intersect : (-> shape FlV3 FlV3 Nonnegative-Flonum
                        (Values (U #f Nonnegative-Flonum) (U #f (Promise trace-data))))])
  #:transparent)

(struct Empty-Scene () #:transparent)
(define empty-scene (Empty-Scene))
(define-syntax empty-scene? (make-rename-transformer #'Empty-Scene?))

(struct nonempty-scene () #:transparent)

(struct shape nonempty-scene
  ([lazy-passes : (HashTable GL-Context passes)]
   [vtable : shape-functions]))

(: lazy-passes (-> (HashTable GL-Context passes)))
(define lazy-passes make-weak-hasheq)

(: default-get-passes (-> shape passes))
(define (default-get-passes s) empty-passes)

(: default-get-bbox (-> shape (U 'visible 'invisible) FlAffine3 #f))
(define (default-get-bbox s kind t) #f)

(: default-fast-transform (-> shape FlAffine3 #f))
(define (default-fast-transform s t) #f)

(: default-deep-transform (-> shape FlAffine3 (List shape)))
(define (default-deep-transform s t) (list s))

(: default-ray-intersect (-> shape FlV3 FlV3 Nonnegative-Flonum (Values #f #f)))
(define (default-ray-intersect s v dv max-time) (values #f #f))

(: shape-passes (-> shape passes))
(define (shape-passes s)
  (hash-ref!
   (shape-lazy-passes s)
   (get-current-managed-gl-context 'shape-passes)
   (λ () ((shape-functions-get-passes (shape-vtable s)) s))))

(: shape-visible-bbox (-> shape FlAffine3 (U #f bbox)))
(define (shape-visible-bbox s t)
  ((shape-functions-get-bbox (shape-vtable s)) s 'visible t))

(: shape-invisible-bbox (-> shape FlAffine3 (U #f bbox)))
(define (shape-invisible-bbox s t)
  ((shape-functions-get-bbox (shape-vtable s)) s 'invisible t))

(: shape-fast-transform (-> shape FlAffine3 (U #f shape)))
(define (shape-fast-transform s t)
  ((shape-functions-fast-transform (shape-vtable s)) s t))

(: shape-deep-transform (-> shape FlAffine3 (Listof shape)))
(define (shape-deep-transform s t)
  ((shape-functions-deep-transform (shape-vtable s)) s t))

(: shape-ray-intersect (-> shape FlV3 FlV3 Nonnegative-Flonum
                           (Values (U #f Nonnegative-Flonum) (U #f (Promise trace-data)))))
(define (shape-ray-intersect s v dv max-time)
  ((shape-functions-ray-intersect (shape-vtable s)) s v dv max-time))

;; ===================================================================================================
;; Other nonempty scene types

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
  (U shape
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
  (cond [(node-scene? s)  (node-scene-child-tags s)]
        [(trans-scene? s)  (scene-tags (trans-scene-scene s))]
        [(group-scene? s)  (tags-add (scene-tags (group-scene-scene s))
                                     (group-scene-tag s))]
        [else  empty-tags]))

(: scene-visible-bbox (-> Scene (U #f bbox)))
(define (scene-visible-bbox s)
  (cond [(empty-scene? s)  #f]
        [(node-scene? s)
         (node-scene-visible-bbox s)]
        [(trans-scene? s)
         (maybe-bbox-transform (scene-visible-bbox (trans-scene-scene s))
                               (trans-scene-affine s))]
        [(group-scene? s)
         (define s0 (group-scene-scene s))
         (if (empty-scene? s0) zero-bbox (scene-visible-bbox s0))]
        [else
         (shape-visible-bbox s identity-flaffine3)]))

(: scene-invisible-bbox (-> Scene (U #f bbox)))
(define (scene-invisible-bbox s)
  (cond [(empty-scene? s)  #f]
        [(node-scene? s)
         (node-scene-invisible-bbox s)]
        [(trans-scene? s)
         (maybe-bbox-transform (scene-invisible-bbox (trans-scene-scene s))
                               (trans-scene-affine s))]
        [(group-scene? s)
         (define s0 (group-scene-scene s))
         (if (empty-scene? s0) zero-bbox (scene-invisible-bbox s0))]
        [else
         (shape-invisible-bbox s identity-flaffine3)]))

(: scene-bbox (-> Scene (U #f bbox)))
(define (scene-bbox s)
  (maybe-bbox-join (scene-visible-bbox s)
                   (scene-invisible-bbox s)))
