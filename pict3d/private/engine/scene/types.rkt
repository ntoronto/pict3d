#lang typed/racket/base

(require racket/match
         racket/list
         "../../math.rkt"
         "../../gl.rkt"
         "../draw-pass.rkt"
         "../types.rkt"
         "tags.rkt"
         "flags.rkt")

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

(struct solid-shape shape ([flags : Flags]) #:transparent)

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

(struct light-shape shape ([flags : Flags] [emitted : FlV4]) #:transparent)
(struct directional-light-shape light-shape ([direction : FlV3]) #:transparent)
(struct point-light-shape light-shape ([affine : FlAffine3]
                                       [min-radius : Flonum]
                                       [max-radius : Flonum])
  #:transparent)

(struct indicator-shape shape ([flags : Flags]) #:transparent)
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

(struct bbox ([rect : FlRect3] [tight? : Boolean])
  #:transparent
  #:mutable)

(define zero-bbox (bbox zero-flrect3 #t))

(: maybe-bbox-rect (-> (U #f bbox) (U #f FlRect3)))
(define (maybe-bbox-rect b)
  (and b (bbox-rect b)))

(: maybe-bbox-tight? (-> (U #f bbox) Boolean))
(define (maybe-bbox-tight? b)
  (if b (bbox-tight? b) #t))

(: bbox-transform (-> bbox FlAffine3 bbox))
(define (bbox-transform b t)
  (bbox (flrect3-transform (bbox-rect b) t)
        (and (bbox-tight? b) (or (identity-flaffine3? t) (flt3axial? t axial-tol)))))

(: maybe-bbox-transform (-> (U #f bbox) FlAffine3 (U #f bbox)))
(define (maybe-bbox-transform b t)
  (and b (bbox-transform b t)))

(: bbox-join (-> bbox bbox bbox))
(define (bbox-join b1 b2)
  (bbox (flrect3-join (bbox-rect b1) (bbox-rect b2))
        (and (bbox-tight? b1) (bbox-tight? b2))))

(: maybe-bbox-join (-> (U #f bbox) (U #f bbox) (U #f bbox)))
(define (maybe-bbox-join b1 b2)
  (cond [(not b1)  b2]
        [(not b2)  b1]
        [else  (bbox-join b1 b2)]))

(: bbox-contains-bbox? (-> bbox bbox (U Boolean 'unknown)))
(define (bbox-contains-bbox? b1 b2)
  (define r1 (bbox-rect b1))
  (define r2 (bbox-rect b2))
  (cond [(and (bbox-tight? b1) (bbox-tight? b2))
         (flrect3-contains-rect? r1 r2)]
        [(flrect3-disjoint? r1 r2)
         #f]
        [(bbox-tight? b1)
         (flrect3-contains-rect? r1 r2)]
        [else
         'unknown]))

(: maybe-bbox-contains-bbox? (-> (U #f bbox) (U #f bbox) (U Boolean 'unknown)))
(define (maybe-bbox-contains-bbox? b1 b2)
  (cond [(not b2)  #t]
        [(not b1)  (if (bbox-tight? b2) #f 'unknown)]
        [else  (bbox-contains-bbox? b1 b2)]))

(: bbox-disjoint? (-> bbox bbox (U Boolean 'unknown)))
(define (bbox-disjoint? b1 b2)
  (cond [(and (bbox-tight? b1) (bbox-tight? b2))
         (flrect3-disjoint? (bbox-rect b1) (bbox-rect b2))]
        [(flrect3-disjoint? (bbox-rect b1) (bbox-rect b2))  #t]
        [else  'unknown]))

(: maybe-bbox-disjoint? (-> (U #f bbox) (U #f bbox) (U Boolean 'unknown)))
(define (maybe-bbox-disjoint? b1 b2)
  (if (and b1 b2)
      (bbox-disjoint? b1 b2)
      #t))

(: bbox-line-intersects (-> bbox FlV3 FlV3 (Values (U #f 'unknown Flonum) (U #f 'unknown Flonum))))
(define (bbox-line-intersects b v dv)
  (cond [(bbox-tight? b)  (flrect3-line-intersects (bbox-rect b) v dv)]
        [else
         (define-values (t1 t2) (flrect3-line-intersects (bbox-rect b) v dv))
         (if (and (not t1) (not t1))
             (values #f #f)
             (values 'unknown 'unknown))]))

(: maybe-bbox-line-intersects (-> (U #f bbox) FlV3 FlV3
                                  (Values (U #f 'unknown Flonum) (U #f 'unknown Flonum))))
(define (maybe-bbox-line-intersects b v dv)
  (if b (bbox-line-intersects b v dv) (values #f #f)))


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

(struct scene () #:transparent)

(struct Empty-Scene scene () #:transparent)

(struct nonempty-scene scene
  ([visible-bbox : (U #f bbox)]
   [invisible-bbox : (U #f bbox)])
  #:transparent)

(struct container-scene nonempty-scene
  ([count : Nonnegative-Fixnum]
   [child-tags : Tags]
   [child-flags : Flags])
  #:transparent)

(struct leaf-scene nonempty-scene
  ([shape : Shape])
  #:transparent)

(struct node-scene container-scene
  ([neg : Nonempty-Scene]
   [pos : Nonempty-Scene])
  #:transparent)

(struct trans-scene container-scene
  ([affine : FlAffine3]
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

(: scene-count (-> Scene Nonnegative-Fixnum))
(define (scene-count s)
  (cond [(empty-scene? s)  0]
        [(leaf-scene? s)  1]
        [else  (container-scene-count s)]))

(: scene-tags (-> Scene Tags))
(define (scene-tags s)
  (cond [(empty-scene? s)  empty-tags]
        [(leaf-scene? s)  empty-tags]
        [else  (container-scene-child-tags s)]))

(: scene-visible-bbox (-> Scene (U #f bbox)))
(define (scene-visible-bbox s)
  (cond [(empty-scene? s)  #f]
        [else  (nonempty-scene-visible-bbox s)]))

(: scene-invisible-bbox (-> Scene (U #f bbox)))
(define (scene-invisible-bbox s)
  (cond [(empty-scene? s)  #f]
        [else  (nonempty-scene-invisible-bbox s)]))

(: scene-bbox (-> Scene (U #f bbox)))
(define (scene-bbox s)
  (maybe-bbox-join (scene-visible-bbox s)
                   (scene-invisible-bbox s)))

;; ===================================================================================================
;; Ray-scene intersection types

(struct line-hit ([distance : Flonum]
                  [point : FlV3]
                  [normal : (U #f FlV3)])
  #:transparent
  #:mutable)
