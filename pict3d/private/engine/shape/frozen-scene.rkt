#lang typed/racket/base

;; Scenes, frozen into single shapes

(require racket/list
         "../../math.rkt"
         "../draw.rkt"
         "../scene.rkt")

(provide make-frozen-scene-shape
         (struct-out frozen-scene-shape))

(struct frozen-scene-shape shape
  ([scene : Nonempty-Scene])
  #:transparent)

;; ===================================================================================================
;; Constructor

(: make-frozen-scene-shape/transformed (-> Nonempty-Scene frozen-scene-shape))
(define (make-frozen-scene-shape/transformed s)
  (frozen-scene-shape (lazy-passes) frozen-scene-shape-functions s))

(: make-frozen-scene-shape (->* [Nonempty-Scene] [FlAffine3] frozen-scene-shape))
;; Do a deep transform on all the shapes
;; The only kind of subscenes left should be shapes and union nodes; no transformations or groups
(define (make-frozen-scene-shape s [t identity-flaffine3])
  (let ([s  (scene-deep-transform s t)])
    (if (scene-flattened? s)
        (make-frozen-scene-shape/transformed (assert s nonempty-scene?))
        (error 'make-frozen-scene-shape
               "internal error: expected scene-deep-transform to return a flattened scene; got ~e"
               s))))

;; ===================================================================================================
;; Set attributes

(: set-frozen-scene-shape-color (-> shape FlV4 frozen-scene-shape))
(define (set-frozen-scene-shape-color s c)
  (let ([s  (frozen-scene-shape-scene (assert s frozen-scene-shape?))])
    (define new-s (scene-map-shapes s (λ ([s : shape]) (shape-set-color s c))))
    (make-frozen-scene-shape/transformed (assert new-s nonempty-scene?))))

(: set-frozen-scene-shape-emitted (-> shape FlV4 frozen-scene-shape))
(define (set-frozen-scene-shape-emitted s e)
  (let ([s  (frozen-scene-shape-scene (assert s frozen-scene-shape?))])
    (define new-s (scene-map-shapes s (λ ([s : shape]) (shape-set-emitted s e))))
    (make-frozen-scene-shape/transformed (assert new-s nonempty-scene?))))

(: set-frozen-scene-shape-material (-> shape FlV4 frozen-scene-shape))
(define (set-frozen-scene-shape-material s m)
  (let ([s  (frozen-scene-shape-scene (assert s frozen-scene-shape?))])
    (define new-s (scene-map-shapes s (λ ([s : shape]) (shape-set-material s m))))
    (make-frozen-scene-shape/transformed (assert new-s nonempty-scene?))))

;; ===================================================================================================
;; Passes

(: get-frozen-scene-shape-passes (-> shape passes))
(define (get-frozen-scene-shape-passes s)
  (let ([s  (frozen-scene-shape-scene (assert s frozen-scene-shape?))])
    (merge-passes
     (append* (scene-extract s empty (λ ([s : shape] [t : FlAffine3])
                                       (map shape-passes (shape-deep-transform s t))))))))

;; ===================================================================================================
;; Bounding box

(: get-frozen-scene-shape-visible-bbox (-> frozen-scene-shape FlAffine3 (U #f bbox)))
(define (get-frozen-scene-shape-visible-bbox s t)
  (maybe-bbox-transform (scene-visible-bbox (frozen-scene-shape-scene s)) t))

(: get-frozen-scene-shape-invisible-bbox (-> frozen-scene-shape FlAffine3 (U #f bbox)))
(define (get-frozen-scene-shape-invisible-bbox s t)
  (maybe-bbox-transform (scene-invisible-bbox (frozen-scene-shape-scene s)) t))

(: get-frozen-scene-shape-bbox (-> shape (U 'visible 'invisible) FlAffine3 (U #f bbox)))
(define (get-frozen-scene-shape-bbox s kind t)
  (let ([s  (assert s frozen-scene-shape?)])
    (if (eq? kind 'visible)
        (get-frozen-scene-shape-visible-bbox s t)
        (get-frozen-scene-shape-invisible-bbox s t))))

;; ===================================================================================================
;; Transform

(: frozen-scene-shape-deep-transform (-> shape FlAffine3 (Listof shape)))
(define (frozen-scene-shape-deep-transform s t)  
  (let ([s  (frozen-scene-shape-scene (assert s frozen-scene-shape?))])
    (append* (scene-extract (scene-deep-transform s t) empty shape-deep-transform))))

;; ===================================================================================================
;; Ray intersection

(: frozen-scene-shape-line-intersect (-> shape FlV3 FlV3 (Values (U #f Flonum)
                                                                 (U #f (Promise surface-data)))))
(define (frozen-scene-shape-line-intersect s v dv)
  (let ([s  (assert s frozen-scene-shape?)])
    (nonempty-scene-ray-intersect (frozen-scene-shape-scene s) v dv)))

;; ===================================================================================================

(define frozen-scene-shape-functions
  (shape-functions
   set-frozen-scene-shape-color
   set-frozen-scene-shape-emitted
   set-frozen-scene-shape-material
   get-frozen-scene-shape-passes
   get-frozen-scene-shape-bbox
   (λ (s t) #f)  ; no fast, tight transform
   frozen-scene-shape-deep-transform
   frozen-scene-shape-line-intersect))
