#lang typed/racket/base

#|
Scenes: tree-structured databases that
 * Approximately self-balance
 * Store shapes in the leaves
 * Store bounding information in the nodes and leaves

Scenes should allow O(log(n))
 * Approximate plane culling (i.e. for view and shadow frustum culling)
 * Collision queries (i.e. point, sphere, or box tracing)
 * Touching queries and other spatial searches

This module also contains code for `frozen-scene-shape` that would normally go in its own module. But
moving this code to another module would cause a cycle, and the only way to break it is to
parameterize scenes on shapes and shape operations instead of having them baked in. That used to be
how the code was organized, but it made the predicate for the scene type O(n). As it is now, with no
parametric polymorphism and no higher-order types, Typed Racket generates an O(1) flat contract.
|#

(require racket/unsafe/ops
         racket/promise
         racket/flonum
         racket/vector
         racket/list
         racket/fixnum
         (except-in typed/opengl/ffi -> cast)
         "../../math.rkt"
         "../../gl.rkt"
         "../../utils.rkt"
         "../types.rkt"
         "../draw-passes.rkt"
         "../merge-passes.rkt"
         "../utils.rkt"
         "tags.rkt"
         "types.rkt"
         "shape.rkt")

(provide (all-defined-out))

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

;; ===================================================================================================
;; Scene constructors

(: make-nonempty-node-scene (-> Nonempty-Scene Nonempty-Scene node-scene))
(define (make-nonempty-node-scene s1 s2)
  (node-scene (maybe-bbox-join (scene-visible-bbox s1) (scene-visible-bbox s2))
              (maybe-bbox-join (scene-invisible-bbox s1) (scene-invisible-bbox s2))
              (fx+ (scene-count s1) (scene-count s2))
              (tags-union (scene-tags s1) (scene-tags s2))
              s1
              s2))

(: make-node-scene (-> Scene Scene Scene))
(define (make-node-scene s1 s2)
  (cond [(empty-scene? s1)  s2]
        [(empty-scene? s2)  s1]
        [else  (make-nonempty-node-scene s1 s2)]))

(: make-nonempty-trans-scene (-> Nonempty-Scene FlAffine3 Nonempty-Scene))
(define (make-nonempty-trans-scene s t)
  (cond [(identity-flaffine3? t)  s]
        [(shape? s)
         (define a (shape-easy-transform s t))
         (if a a (trans-scene s t))]
        [(trans-scene? s)
         (make-nonempty-trans-scene (trans-scene-scene s)
                                    (flt3compose t (trans-scene-affine s)))]
        [else
         (trans-scene s t)]))

(: make-trans-scene (-> Scene FlAffine3 Scene))
(define (make-trans-scene s t)
  (if (empty-scene? s) s (make-nonempty-trans-scene s t)))

(: make-group-scene (-> Scene Tag group-scene))
(define (make-group-scene s n)
  (group-scene s n))

;; ===================================================================================================
;; Scene union with an attempt at on-the-fly rebalancing

(: scene-rebalance-split (-> Nonempty-Scene Index Flonum (Values Scene Scene)))
(define (scene-rebalance-split s i x)
  (let loop ([s s])
    (define r (maybe-bbox-rect (scene-bbox s)))
    (cond
      [(not r)  (values s empty-scene)]
      [else
       (define xmin (unsafe-flv3-ref (flrect3-min r) i))
       (define xmax (unsafe-flv3-ref (flrect3-max r) i))
       (cond
         [(<= x xmin)  (values empty-scene s)]
         [(<= xmax x)  (values s empty-scene)]
         [(or (shape? s) (trans-scene? s) (group-scene? s))
          (if (< (- x xmin) (- xmax x))
              (values s empty-scene)
              (values empty-scene s))]
         [(node-scene? s)
          (define s1 (node-scene-neg s))
          (define s2 (node-scene-pos s))
          (define-values (s11 s12) (loop s1))
          (define-values (s21 s22) (loop s2))
          (let ([s1  (if (and (eq? s11 s1) (eq? s21 s2)) s (make-node-scene s11 s21))]
                [s2  (if (and (eq? s12 s1) (eq? s22 s2)) s (make-node-scene s12 s22))])
            (values s1 s2))])])))

(: nonempty-scene-union/rebalance (-> Nonempty-Scene Nonempty-Scene Nonempty-Scene))
(define (nonempty-scene-union/rebalance s1 s2)
  (define bv (maybe-bbox-join (scene-visible-bbox s1)
                              (scene-visible-bbox s2)))
  (define bi (maybe-bbox-join (scene-invisible-bbox s1)
                              (scene-invisible-bbox s2)))
  (define r (maybe-bbox-rect (maybe-bbox-join bv bi)))
  (define c (unsafe-fx+ (scene-count s1) (scene-count s2)))
  (define ms (tags-union (scene-tags s1) (scene-tags s2)))
  (cond
    [(not r)
     (node-scene bv bi c ms s1 s2)]
    [else
     (define-values (i x) (flrect3-longest-axis/center r))
     (define-values (s11 s12) (scene-rebalance-split s1 i x))
     (define-values (s21 s22) (scene-rebalance-split s2 i x))
     (cond [(empty-scene? s11)
            (if (or (empty-scene? s21) (empty-scene? s22))
                (node-scene bv bi c ms s1 s2)
                (node-scene bv bi c ms s21 (nonempty-scene-union/rebalance s1 s22)))]
           [(empty-scene? s12)
            (if (or (empty-scene? s21) (empty-scene? s22))
                (node-scene bv bi c ms s1 s2)
                (node-scene bv bi c ms (nonempty-scene-union/rebalance s1 s21) s22))]
           [(empty-scene? s21)
            (node-scene bv bi c ms s11 (nonempty-scene-union/rebalance s12 s2))]
           [(empty-scene? s22)
            (node-scene bv bi c ms (nonempty-scene-union/rebalance s11 s2) s12)]
           [else
            (node-scene bv bi c ms
                        (nonempty-scene-union/rebalance s11 s21)
                        (nonempty-scene-union/rebalance s12 s22))])]))

(: node-scene-insert (-> node-scene Nonempty-Scene Nonempty-Scene))
;; Insert s2 into s1, if possible; otherwise rebalance
(define (node-scene-insert s1 s2)
  (define s11 (node-scene-neg s1))
  (define s12 (node-scene-pos s1))
  (cond [(and (maybe-bbox-appx-contains-bbox? (scene-visible-bbox s11)
                                              (scene-visible-bbox s2))
              (maybe-bbox-appx-contains-bbox? (scene-invisible-bbox s11)
                                              (scene-invisible-bbox s2)))
         (node-scene (scene-visible-bbox s1)
                     (scene-invisible-bbox s1)
                     (unsafe-fx+ (scene-count s1) (scene-count s2))
                     (tags-union (scene-tags s1) (scene-tags s2))
                     (nonempty-scene-union s11 s2)
                     s12)]
        [(and (maybe-bbox-appx-contains-bbox? (scene-visible-bbox s12)
                                              (scene-visible-bbox s2))
              (maybe-bbox-appx-contains-bbox? (scene-invisible-bbox s12)
                                              (scene-invisible-bbox s2)))
         (node-scene (scene-visible-bbox s1)
                     (scene-invisible-bbox s1)
                     (unsafe-fx+ (scene-count s1) (scene-count s2))
                     (tags-union (scene-tags s1) (scene-tags s2))
                     s11
                     (nonempty-scene-union s12 s2))]
        [else
         (nonempty-scene-union/rebalance s1 s2)]))

(: nonempty-scene-union (-> Nonempty-Scene Nonempty-Scene Nonempty-Scene))
;; Compute the nonempty union of two nonempty scenes
(define (nonempty-scene-union s1 s2)
  (define bv1 (scene-visible-bbox s1))
  (define bi1 (scene-invisible-bbox s1))
  (define bv2 (scene-visible-bbox s2))
  (define bi2 (scene-invisible-bbox s2))
  (let ([bv1  (if (and bv1 (> (bbox-badness bv1) 16.0)) (scene-visible-bbox/badness s1 16.0) bv1)]
        [bi1  (if (and bi1 (> (bbox-badness bi1) 16.0)) (scene-invisible-bbox/badness s1 16.0) bi1)]
        [bv2  (if (and bv2 (> (bbox-badness bv2) 16.0)) (scene-visible-bbox/badness s2 16.0) bv2)]
        [bi2  (if (and bi2 (> (bbox-badness bi2) 16.0)) (scene-invisible-bbox/badness s2 16.0) bi2)]
        )
    (cond [(and (node-scene? s1)
                (maybe-bbox-appx-contains-bbox? bv1 bv2)
                (maybe-bbox-appx-contains-bbox? bi1 bi2))
           ;; Try to insert s2 into one of s1's children
           (node-scene-insert s1 s2)]
          [(and (node-scene? s2)
                (maybe-bbox-appx-contains-bbox? bv2 bv1)
                (maybe-bbox-appx-contains-bbox? bi2 bi1))
           ;; Try to insert s1 into one of s2's children
           (node-scene-insert s2 s1)]
          [else
           ;; Give up and rebalance
           (nonempty-scene-union/rebalance s1 s2)])))

(: scene-union (-> Scene Scene Scene))
;; Compute the union of two scenes
(define (scene-union s1 s2)
  (cond [(empty-scene? s1)  s2]
        [(empty-scene? s2)  s1]
        [else  (nonempty-scene-union s1 s2)]))

(: scene-union* (-> (Listof Scene) Scene))
;; TODO: for each split, sort by min edge on longest axis?
(define (scene-union* ss)
  (cond
    [(empty? ss)  empty-scene]
    [else
     (define n (length ss))
     (let loop : Scene ([ss : (Listof+1 Scene)  ss] [n : Index  (length ss)])
       (define n/2 (fxquotient n 2))
       (define ss1 (take ss n/2))
       (define ss2 (drop ss n/2))
       (cond [(empty? ss1)  (first ss2)]
             [(empty? ss2)  (first ss1)]  ; can't happen
             [else
              (let ([s1  (loop ss1 n/2)]
                    [s2  (loop ss2 (assert (- n n/2) index?))])
                (scene-union s1 s2))]))]))

;; ===================================================================================================
;; Map over shapes (needed for setting frozen scene attributes)

(: scene-map-shapes (-> Scene (-> Shape Shape) Scene))
(define (scene-map-shapes s f)
  (let loop ([s s])
    (cond
      [(empty-scene? s)  s]
      [(shape? s)  (f s)]
      [(node-scene? s)
       (define s1 (node-scene-neg s))
       (define s2 (node-scene-pos s))
       (define new-s1 (loop s1))
       (define new-s2 (loop s2))
       (cond [(and (eq? new-s1 s1) (eq? new-s2 s2))  s]
             [else  (make-node-scene new-s1 new-s2)])]
      [(trans-scene? s)
       (define s0 (trans-scene-scene s))
       (define new-s0 (loop s0))
       (cond [(eq? new-s0 s0)  s]
             [else  (make-trans-scene new-s0 (trans-scene-affine s))])]
      [(group-scene? s)
       (define s0 (group-scene-scene s))
       (define new-s0 (loop s0))
       (cond [(eq? new-s0 s0)  s]
             [else  (make-group-scene new-s0 (group-scene-tag s))])])))

;; ===================================================================================================
;; Tight bounding boxes

(: scene-recompute-bbox (-> Scene (U 'invisible 'visible) Nonnegative-Flonum (U #f bbox)))
(define (scene-recompute-bbox s kind max-badness)
  (: node-scene-bbox (-> node-scene (U #f bbox)))
  (define (node-scene-bbox s)
    (if (eq? kind 'visible)
        (node-scene-visible-bbox s)
        (node-scene-invisible-bbox s)))
  
  (: set-node-scene-bbox! (-> node-scene bbox Void))
  (define (set-node-scene-bbox! s b)
    (define target-b (node-scene-bbox s))
    (when target-b
      (set-bbox-rect! target-b (bbox-rect b))
      (set-bbox-badness! target-b (bbox-badness b))))
  
  (: shape-bbox (-> Shape FlAffine3 (U #f bbox)))
  (define (shape-bbox a t)
    (if (eq? kind 'visible)
        (shape-visible-bbox a t)
        (shape-invisible-bbox a t)))
  
  (let loop ([s s] [t identity-flaffine3] [max-badness max-badness])
    (cond
      [(empty-scene? s)  #f]
      [(shape? s)  (shape-bbox s t)]
      [(node-scene? s)
       (define b (let ([b  (node-scene-bbox s)])
                   (if b (bbox-transform b t) #f)))
       (cond [(< (maybe-bbox-badness b) max-badness)  b]
             [else
              (define new-b
                (maybe-bbox-join (loop (node-scene-neg s) t max-badness)
                                 (loop (node-scene-pos s) t max-badness)))
              (when new-b
                (let ([new-b  (bbox-transform new-b (flt3inverse t))])
                  (when (and (< (bbox-badness new-b) max-badness)
                             (< (bbox-badness (bbox-transform new-b t)) max-badness))
                    (set-node-scene-bbox! s new-b))))
              new-b])]
      [(trans-scene? s)
       (loop (trans-scene-scene s)
             (flt3compose t (trans-scene-affine s))
             max-badness)]
      [(group-scene? s)
       (define s0 (group-scene-scene s))
       (cond [(empty-scene? s0)  (bbox-transform zero-bbox t)]
             [else  (loop s0 t max-badness)])]))
  )

(: scene-visible-bbox/badness (-> Scene Nonnegative-Flonum (U #f bbox)))
(define (scene-visible-bbox/badness s max-badness)
  (scene-recompute-bbox s 'visible max-badness))

(: scene-invisible-bbox/badness (-> Scene Nonnegative-Flonum (U #f bbox)))
(define (scene-invisible-bbox/badness s max-badness)
  (scene-recompute-bbox s 'invisible max-badness))

(: scene-bbox/badness (-> Scene Nonnegative-Flonum (U #f bbox)))
(define (scene-bbox/badness s max-badness)
  (maybe-bbox-join (scene-recompute-bbox s 'visible max-badness)
                   (scene-recompute-bbox s 'invisible max-badness)))

;; ===================================================================================================
;; Tastes almost-but-not-quite-entirely-unlike map

(: transform-planes (-> FlAffine3 (Listof FlPlane3) (Listof FlPlane3)))
(define (transform-planes t0 planes)
  (if (identity-flaffine3? t0)
      planes
      (let ([tinv0 : FlAffine3  (flt3inverse t0)])
        (for/fold ([planes : (Listof FlPlane3)  empty]) ([p  (in-list planes)])
          (define new-p (flt3apply/plane tinv0 p))
          (if new-p (cons new-p planes) planes)))))

(: nonempty-scene-for-each! (-> Nonempty-Scene
                                (Listof FlPlane3)
                                (-> Shape FlAffine3 Nonnegative-Fixnum Any)
                                Nonnegative-Fixnum
                                Nonnegative-Fixnum))
(define (nonempty-scene-for-each! s planes f start)
  (let loop ([s s]
             [t  identity-flaffine3]
             [parent-planes : (Listof FlPlane3)  planes]
             [i : Nonnegative-Fixnum  start])
    
    (: side (U 'inside 'outside 'both))
    (define side
      (cond [(empty? parent-planes)  'inside]
            [else
             (define b (scene-bbox s))
             (define side (maybe-bbox-appx-classify/planes b parent-planes))
             (if (and b (eq? side 'both) (> (bbox-badness b) 16.0))
                 (let ([b  (scene-bbox/badness s 16.0)])
                   (maybe-bbox-appx-classify/planes b parent-planes))
                 side)]))
    
    (define planes (if (eq? side 'inside) empty parent-planes))
    
    (cond
      [(eq? side 'outside)   i]
      [(shape? s)
       (f s t i)
       (unsafe-fx+ i 1)]
      [(node-scene? s)
       (let* ([i  (loop (node-scene-neg s) t planes i)]
              [i  (loop (node-scene-pos s) t planes i)])
         i)]
      [(trans-scene? s)
       (define t0 (trans-scene-affine s))
       (loop (trans-scene-scene s) (flt3compose t t0) (transform-planes t0 planes) i)]
      [(group-scene? s)
       (define s0 (group-scene-scene s))
       (cond [(empty-scene? s0)  i]
             [else  (loop s0 t planes i)])])))

(: scene-for-each! (-> Scene
                       (Listof FlPlane3)
                       (-> Shape FlAffine3 Nonnegative-Fixnum Any)
                       Nonnegative-Fixnum
                       Nonnegative-Fixnum))
(define (scene-for-each! s planes f start)
  (if (empty-scene? s)
      start
      (nonempty-scene-for-each! s planes f start)))

(: scene-extract (All (B) (-> Scene (Listof FlPlane3) (-> Shape FlAffine3 B) (Listof B))))
(define (scene-extract s planes f)
  (: bs (Listof B))
  (define bs empty)
  (scene-for-each! s
                   planes
                   (λ ([a : Shape] [t : FlAffine3] [i : Nonnegative-Fixnum])
                     (set! bs (cons (f a t) bs)))
                   0)
  bs)

;; ===================================================================================================
;; ===================================================================================================
;; ===================================================================================================
;; Shape operations

;; ===================================================================================================
;; Extracting data for drawing passes

(: shape-passes (-> Shape passes))
(define (shape-passes a)
  (hash-ref!
   (shape-lazy-passes a)
   (get-current-managed-gl-context 'shape-passes)
   (λ ()
     (cond
       [(solid-shape? a)
        (cond
          [(triangle-shape? a)   (make-triangle-shape-passes a)]
          [(rectangle-shape? a)  (make-rectangle-shape-passes a)]
          [(sphere-shape? a)     (make-sphere-shape-passes a)])]
       [(light-shape? a)
        (cond
          [(directional-light-shape? a)  (make-directional-light-shape-passes a)]
          [(point-light-shape? a)        (make-point-light-shape-passes a)])]
       [(indicator-shape? a)
        (cond
          [(point-light-shell-shape? a)  (make-point-light-shell-shape-passes a)])]
       [(frozen-scene-shape? a)
        (make-frozen-scene-shape-passes a)]))))

;; ===================================================================================================
;; Shape visible and invisible bounding boxes

(: shape-visible-bbox (-> Shape FlAffine3 (U #f bbox)))
(define (shape-visible-bbox a t)
  (cond
    [(solid-shape? a)
     (cond
       [(triangle-shape? a)   (bbox (triangle-shape-rect a t) 0.0)]
       [(rectangle-shape? a)  (bbox (rectangle-shape-rect a t) 0.0)]
       [(sphere-shape? a)     (bbox (sphere-shape-rect a t) 0.0)])]
    [(light-shape? a)      #f]
    [(indicator-shape? a)  #f]
    [(frozen-scene-shape? a)
     (frozen-scene-shape-visible-bbox a t)]))

(: shape-invisible-bbox (-> Shape FlAffine3 (U #f bbox)))
(define (shape-invisible-bbox a t)
  (cond
    [(solid-shape? a)  #f]
    [(light-shape? a)
     (cond
       [(directional-light-shape? a)  (bbox directional-light-shape-rect 0.0)]
       [(point-light-shape? a)        (bbox (point-light-shape-rect a t) 0.0)])]
    [(indicator-shape? a)
     (cond
       [(point-light-shell-shape? a)  (bbox (point-light-shell-shape-rect a t) 0.0)])]
    [(frozen-scene-shape? a)
     (frozen-scene-shape-invisible-bbox a t)]))

;; ===================================================================================================
;; Shape and scene transformation (forced, not lazy)

(: shape-easy-transform (-> Shape FlAffine3 (U #f Shape)))
(define (shape-easy-transform a t)
  (cond
    [(identity-flaffine3? t)  a]
    [(solid-shape? a)
     (cond
       [(triangle-shape? a)   (triangle-shape-easy-transform a t)]
       [(rectangle-shape? a)  #f]
       [(sphere-shape? a)     (sphere-shape-easy-transform a t)])]
    [(light-shape? a)
     (cond
       [(directional-light-shape? a)  (directional-light-shape-easy-transform a t)]
       [(point-light-shape? a)        (point-light-shape-easy-transform a t)])]
    [(indicator-shape? a)
     (cond
       [(point-light-shell-shape? a)  (point-light-shell-shape-easy-transform a t)])]
    ;; Frozen scene
    [(frozen-scene-shape? a)  #f]))

(: shape-transform (-> Shape FlAffine3 (Listof Shape)))
(define (shape-transform a t)
  (cond
    [(identity-flaffine3? t)  (list a)]
    [(solid-shape? a)
     (cond
       [(triangle-shape? a)   (list (triangle-shape-easy-transform a t))]
       [(rectangle-shape? a)  (rectangle-shape-transform a t)]
       [(sphere-shape? a)     (list (sphere-shape-easy-transform a t))])]
    [(light-shape? a)
     (cond
       [(directional-light-shape? a)  (list (directional-light-shape-easy-transform a t))]
       [(point-light-shape? a)        (list (point-light-shape-easy-transform a t))])]
    [(indicator-shape? a)
     (cond
       [(point-light-shell-shape? a)  (list (point-light-shell-shape-easy-transform a t))])]
    ;; Frozen scene
    [(frozen-scene-shape? a)
     (frozen-scene-shape-transform a t)]))

(: scene-transform-shapes (-> Scene FlAffine3 Scene))
(define (scene-transform-shapes s t)
  (cond
    [(empty-scene? s)  s]
    [(shape? s)  (scene-union* (shape-transform s t))]
    [(node-scene? s)
     (make-node-scene (scene-transform-shapes (node-scene-neg s) t)
                      (scene-transform-shapes (node-scene-pos s) t))]
    [(trans-scene? s)
     (scene-transform-shapes (trans-scene-scene s) (flt3compose t (trans-scene-affine s)))]
    [(group-scene? s)
     (scene-transform-shapes (group-scene-scene s) t)]))

;; ===================================================================================================
;; Set shape attributes

(: shape-set-color (-> Shape FlV4 Shape))
(define (shape-set-color a c)
  (cond
    [(solid-shape? a)
     (cond
       [(triangle-shape? a)   (set-triangle-shape-color a c)]
       [(rectangle-shape? a)  (set-rectangle-shape-color a c)]
       [(sphere-shape? a)     (set-sphere-shape-color a c)])]
    [(frozen-scene-shape? a)  (set-frozen-scene-shape-color a c)]
    [else  a]))

(: shape-set-emitted (-> Shape FlV4 Shape))
(define (shape-set-emitted a e)
  (cond
    [(solid-shape? a)
     (cond
       [(triangle-shape? a)   (set-triangle-shape-emitted a e)]
       [(rectangle-shape? a)  (set-rectangle-shape-emitted a e)]
       [(sphere-shape? a)     (set-sphere-shape-emitted a e)])]
    [(light-shape? a)
     (cond
       [(directional-light-shape? a)  (set-directional-light-shape-emitted a e)]
       [(point-light-shape? a)        (set-point-light-shape-emitted a e)])]
    [(frozen-scene-shape? a)
     (set-frozen-scene-shape-emitted a e)]
    [else  a]))

(: shape-set-material (-> Shape FlV4 Shape))
(define (shape-set-material a m)
  (cond
    [(solid-shape? a)
     (cond
       [(triangle-shape? a)   (set-triangle-shape-material a m)]
       [(rectangle-shape? a)  (set-rectangle-shape-material a m)]
       [(sphere-shape? a)     (set-sphere-shape-material a m)])]
    [(frozen-scene-shape? a)  (set-frozen-scene-shape-material a m)]
    [else  a]))

;; ===================================================================================================
;; ===================================================================================================
;; ===================================================================================================
;; Frozen scene shape

;; ===================================================================================================
;; Constructors

(: make-frozen-scene-shape (-> Nonempty-Scene frozen-scene-shape))
(define (make-frozen-scene-shape s)
  (frozen-scene-shape (lazy-passes) s))

;; ===================================================================================================
;; Passes

(: make-frozen-scene-shape-passes (-> frozen-scene-shape passes))
(define (make-frozen-scene-shape-passes a)
  (define s (scene-transform-shapes (frozen-scene-shape-scene a) identity-flaffine3))
  (merge-passes
   (append* (scene-extract s empty (λ ([s : Shape] [t : FlAffine3])
                                     (map shape-passes (shape-transform s t)))))))

;; ===================================================================================================
;; Set attributes

(: set-frozen-scene-shape-color (-> frozen-scene-shape FlV4 frozen-scene-shape))
(define (set-frozen-scene-shape-color a c)
  (define s (frozen-scene-shape-scene a))
  (define new-s (scene-map-shapes s (λ ([a : Shape]) (shape-set-color a c))))
  (make-frozen-scene-shape (assert new-s nonempty-scene?)))

(: set-frozen-scene-shape-emitted (-> frozen-scene-shape FlV4 frozen-scene-shape))
(define (set-frozen-scene-shape-emitted a e)
  (define s (frozen-scene-shape-scene a))
  (define new-s (scene-map-shapes s (λ ([a : Shape]) (shape-set-emitted a e))))
  (make-frozen-scene-shape (assert new-s nonempty-scene?)))

(: set-frozen-scene-shape-material (-> frozen-scene-shape FlV4 frozen-scene-shape))
(define (set-frozen-scene-shape-material a m)
  (define s (frozen-scene-shape-scene a))
  (define new-s (scene-map-shapes s (λ ([a : Shape]) (shape-set-material a m))))
  (make-frozen-scene-shape (assert new-s nonempty-scene?)))

;; ===================================================================================================
;; Bounding box

(: frozen-scene-shape-visible-bbox (-> frozen-scene-shape FlAffine3 (U #f bbox)))
(define (frozen-scene-shape-visible-bbox a t)
  (define s (frozen-scene-shape-scene a))
  (let* ([b  (scene-visible-bbox s)]
         [b  (if (and b (> (bbox-badness b) tight-badness))
                 (scene-visible-bbox/badness s tight-badness)
                 b)])
    (maybe-bbox-transform b t)))

(: frozen-scene-shape-invisible-bbox (-> frozen-scene-shape FlAffine3 (U #f bbox)))
(define (frozen-scene-shape-invisible-bbox a t)
  (define s (frozen-scene-shape-scene a))
  (let* ([b  (scene-invisible-bbox s)]
         [b  (if (and b (> (bbox-badness b) tight-badness))
                 (scene-invisible-bbox/badness s tight-badness)
                 b)])
    (maybe-bbox-transform b t)))

;; ===================================================================================================
;; Transform

(: frozen-scene-shape-transform (-> frozen-scene-shape FlAffine3 (Listof Shape)))
(define (frozen-scene-shape-transform a t)
  (define s (scene-transform-shapes (frozen-scene-shape-scene a) t))
  (append* (scene-extract s empty shape-transform)))

;; ===================================================================================================
;; ===================================================================================================
;; ===================================================================================================
;; Scene operations

;; ===================================================================================================
;; Filter over shapes

(: scene-filter-shapes (-> Scene (-> Shape Boolean) Scene))
(define (scene-filter-shapes s p?)
  (let loop ([s s])
    (cond
      [(empty-scene? s)  s]
      [(shape? s)  (if (p? s) s empty-scene)]
      [(node-scene? s)
       (define s1 (node-scene-neg s))
       (define s2 (node-scene-pos s))
       (define new-s1 (loop s1))
       (define new-s2 (loop s2))
       (cond [(and (eq? new-s1 s1) (eq? new-s2 s2))  s]
             [else  (make-node-scene new-s1 new-s2)])]
      [(trans-scene? s)
       (define s0 (trans-scene-scene s))
       (define new-s0 (loop s0))
       (cond [(eq? new-s0 s0)  s]
             [else  (make-trans-scene new-s0 (trans-scene-affine s))])]
      [(group-scene? s)
       (define s0 (group-scene-scene s))
       (define new-s0 (loop s0))
       (cond [(eq? new-s0 s0)  s]
             [else  (make-group-scene new-s0 (group-scene-tag s))])])))

;; ===================================================================================================
;; Mapping over groups

(: scene-map-group/transform (All (A) (-> Scene (Listof+1 Tag) (-> FlAffine3 group-scene A)
                                          (Listof A))))
(define (scene-map-group/transform s ns f)
  (let loop ([t : FlAffine3  identity-flaffine3]
             [s s]
             [ns : (Listof+1 Tag)  ns]
             [as : (Listof A)  empty])
    (cond
      [(empty-scene? s)  as]
      [(shape? s)  as]
      [(let ([tags  (scene-tags s)])
         (not (andmap (λ ([n : Tag]) (tags-contain? tags n)) ns)))
       as]
      [(node-scene? s)
       (loop t (node-scene-pos s) ns
             (loop t (node-scene-neg s) ns as))]
      [(trans-scene? s)
       (loop (flt3compose t (trans-scene-affine s)) (trans-scene-scene s) ns as)]
      [(group-scene? s)
       (cond [(equal? (group-scene-tag s) (first ns))
              (let ([ns  (rest ns)])
                (cond [(empty? ns)  (cons (f t s) as)]
                      [else  (loop t (group-scene-scene s) ns as)]))]
             [else  (loop t (group-scene-scene s) ns as)])])))

(: scene-map-group (All (A) (-> Scene (Listof+1 Tag) (-> group-scene A) (Listof A))))
(define (scene-map-group s ns f)
  (let loop ([s s]
             [ns : (Listof+1 Tag)  ns]
             [as : (Listof A)  empty])
    (cond
      [(empty-scene? s)  as]
      [(shape? s)  as]
      [(let ([tags  (scene-tags s)])
         (not (andmap (λ ([n : Tag]) (tags-contain? tags n)) ns)))
       as]
      [(node-scene? s)
       (loop (node-scene-pos s) ns
             (loop (node-scene-neg s) ns as))]
      [(trans-scene? s)
       (loop (trans-scene-scene s) ns as)]
      [(group-scene? s)
       (cond [(equal? (group-scene-tag s) (first ns))
              (let ([ns  (rest ns)])
                (cond [(empty? ns)  (cons (f s) as)]
                      [else  (loop (group-scene-scene s) ns as)]))]
             [else
              (loop (group-scene-scene s) ns as)])])))

;; ===================================================================================================
;; Retrieve every group's transform

(: scene-group-transforms (-> Scene (U 'empty 'nonempty 'all) (Listof (Pair Tag FlAffine3))))
(define (scene-group-transforms s which)
  (let loop ([t  identity-flaffine3] [s s])
    (cond
      [(empty-scene? s)  empty]
      [(shape? s)  empty]
      [(node-scene? s)
       (append (loop t (node-scene-neg s))
               (loop t (node-scene-pos s)))]
      [(trans-scene? s)
       (loop (flt3compose t (trans-scene-affine s)) (trans-scene-scene s))]
      [(group-scene? s)
       (define n0 (group-scene-tag s))
       (define s0 (group-scene-scene s))
       (case which
         [(empty)     (if (empty-scene? s0) (list (cons n0 t)) (loop t s0))]
         [(nonempty)  (if (empty-scene? s0) empty (cons (cons n0 t) (loop t s0)))]
         [(all)       (cons (cons n0 t) (loop t s0))])])))

;; ===================================================================================================
;; Replace groups - like a fold for Scene, but just on groups

(: scene-replace-group (-> Scene (Listof+1 Tag) (-> group-scene Scene) Scene))
(define (scene-replace-group s ns f)
  (let loop ([s s] [ns : (Listof+1 Tag)  ns])
    (cond
      [(empty-scene? s)  s]
      [(shape? s)  s]
      [(let ([tags  (scene-tags s)])
         (not (andmap (λ ([n : Tag]) (tags-contain? tags n)) ns)))
       s]
      [(node-scene? s)
       (define s1 (node-scene-neg s))
       (define s2 (node-scene-pos s))
       (define new-s1 (loop s1 ns))
       (define new-s2 (loop s2 ns))
       (cond [(and (eq? new-s1 s1) (eq? new-s2 s2))  s]
             [else  (make-node-scene new-s1 new-s2)])]
      [(trans-scene? s)
       (define s0 (trans-scene-scene s))
       (define new-s0 (loop s0 ns))
       (cond [(eq? new-s0 s0)  s]
             [else  (make-trans-scene new-s0 (trans-scene-affine s))])]
      [(group-scene? s)
       (define n0 (group-scene-tag s))
       (cond [(equal? n0 (first ns))
              (let ([ns  (rest ns)])
                (cond [(empty? ns)  (f s)]
                      [else
                       (define s0 (group-scene-scene s))
                       (define new-s0 (loop s0 ns))
                       (cond [(eq? new-s0 s0)  s]
                             [else  (make-group-scene new-s0 n0)])]))]
             [else
              (define s0 (group-scene-scene s))
              (define new-s0 (loop s0 ns))
              (cond [(eq? new-s0 s0)  s]
                    [else  (make-group-scene new-s0 n0)])])])))

;; ===================================================================================================
;; Scene-ray-intersection

(: shape-line-intersect (-> Shape FlV3 FlV3 (U #f line-hit)))
(define (shape-line-intersect a v dv)
  (cond
    [(solid-shape? a)
     (cond
       [(triangle-shape? a)   (triangle-shape-line-intersect a v dv)]
       [(rectangle-shape? a)  (rectangle-shape-line-intersect a v dv)]
       [(sphere-shape? a)     (sphere-shape-line-intersect a v dv)])]
    [else  #f]))

(: transform-line-hit (-> line-hit FlAffine3 line-hit))
(define (transform-line-hit h t)
  (cond [(identity-flaffine3? t)  h]
        [else
         (line-hit (line-hit-distance h)
                   (flt3apply/pos t (line-hit-point h))
                   (let ([n  (line-hit-normal h)])
                     (and n (flt3apply/norm t n))))]))

(: nonempty-scene-ray-intersect (-> Nonempty-Scene FlV3 FlV3 (U #f line-hit)))
(define (nonempty-scene-ray-intersect s v dv)
  (let loop ([s s] [v v] [dv dv] [t  identity-flaffine3])
    (define-values (tmin tmax)
      (let* ([b  (scene-bbox s)]
             [r  (maybe-bbox-rect b)])
        (define-values (tmin tmax) (maybe-flrect3-line-intersects r v dv))
        (if (and b tmin tmax (> (bbox-badness b) 16.0))
            (let ([r  (maybe-bbox-rect (scene-bbox/badness s 16.0))])
              (maybe-flrect3-line-intersects r v dv))
            (values tmin tmax))))
    (cond
      [(or (not tmin) (not tmax) (and (< tmin 0.0) (< tmax 0.0)))  #f]
      [(shape? s)
       (cond [(frozen-scene-shape? s)
              (loop (frozen-scene-shape-scene s) v dv t)]
             [else
              (define h (shape-line-intersect s v dv))
              (if (or (not h) (< (line-hit-distance h) 0.0))
                  #f
                  (transform-line-hit h (flt3inverse t)))])]
      [(node-scene? s)
       (define h1 (loop (node-scene-neg s) v dv t))
       (define h2 (loop (node-scene-pos s) v dv t))
       (if (and h1 h2)
           (if (<= (line-hit-distance h1) (line-hit-distance h2)) h1 h2)
           (if h1 h1 h2))]
      [(trans-scene? s)
       (let* ([t0 : FlAffine3  (flt3inverse (trans-scene-affine s))]
              [v  : FlV3  (flt3apply/pos t0 v)]
              [dv : FlV3  (flt3apply/dir t0 dv)]
              [t  : FlAffine3  (flt3compose t0 t)])
         (loop (trans-scene-scene s) v dv t))]
      [(group-scene? s)
       (define s0 (group-scene-scene s))
       (cond [(empty-scene? s0)  #f]
             [else  (loop s0 v dv t)])])))

(: scene-ray-intersect (-> Scene FlV3 FlV3 (U #f line-hit)))
(define (scene-ray-intersect s v dv)
  (if (empty-scene? s) #f (nonempty-scene-ray-intersect s v dv)))

;; ===================================================================================================
;; Scene drawing

(define get-scene-draw-passes
  (make-gl-cached-vector
   'get-scene-draw-passes
   (λ ([n : Integer])
     (log-pict3d-info "<engine> creating draw-passes vector for ~v shapes" n)
     (build-vector n (λ (_) (draw-passes empty-passes identity-flaffine3))))
   vector-length))

(: draw-scenes (-> (Listof Scene) Natural Natural FlAffine3 FlTransform3 FlV4 FlV4 Void))
(define (draw-scenes ss width height view proj background ambient)
  ;; Clipping planes
  (define planes (flprojective3-frustum-planes (->flprojective3 (flt3compose proj view))))
  ;; Number of shapes
  (define num
    (for/fold ([num : Nonnegative-Fixnum  0]) ([s  (in-list ss)])
      (unsafe-fx+ num (scene-count s))))
  ;; Build vector of draw passes
  (define bs (get-scene-draw-passes num))
  (define end
    (for/fold ([end : Nonnegative-Fixnum  0]) ([s  (in-list ss)])
      (scene-for-each!
       s
       planes
       (λ ([a : Shape] [t : FlAffine3] [i : Nonnegative-Fixnum])
         (define b (vector-ref bs i))
         (set-draw-passes-passes! b (shape-passes a))
         (set-draw-passes-affine! b t))
       end)))
  (draw-draw-passes bs end width height view proj background ambient))

;; ===================================================================================================
;; Scene plane culling

(: scene-plane-cull (-> Scene FlPlane3 Scene))
(define (scene-plane-cull s p)
  (let loop ([s s] [p p])
    (cond
      [(empty-scene? s)  s]
      [else
       (define b (scene-bbox s))
       (define r (and b (bbox-rect b)))
       (define side (maybe-flrect3-plane-side r p))
       (cond
         [(or (eq? side 'pos) (eq? side 'poszero) (eq? side 'zero))  s]
         [(eq? side 'neg)  empty-scene]
         ;; side is either 'negzero or 'both
         [(shape? s)  s]
         [(node-scene? s)
          (define s1 (node-scene-neg s))
          (define s2 (node-scene-pos s))
          (define new-s1 (loop s1 p))
          (define new-s2 (loop s2 p))
          (cond [(and (eq? new-s1 s1) (eq? new-s2 s2))  s]
                [else  (make-node-scene new-s1 new-s2)])]
         [(trans-scene? s)
          (define t0 (trans-scene-affine s))
          (define s0 (trans-scene-scene s))
          (let* ([inv-t0 : FlAffine3  (flt3inverse t0)]
                 [new-p : (U #f FlPlane3)  (flt3apply/plane inv-t0 p)])
            (cond [new-p  (define new-s0 (loop s0 new-p))
                          (cond [(eq? new-s0 s0)  s]
                                [else  (make-trans-scene new-s0 t0)])]
                  [else  empty-scene]))]
         [(group-scene? s)
          (define s0 (group-scene-scene s))
          (define new-s0 (loop s0 p))
          (cond [(eq? new-s0 s0)  s]
                [else  (make-group-scene new-s0 (group-scene-tag s))])])])))

;; ===================================================================================================
;; Scene rect culling

(: scene-rect-cull* (-> Scene FlRect3 Scene))
(define (scene-rect-cull* s r)
  (define orig-b (bbox r 0.0))
  (let loop ([s s] [t  identity-flaffine3] [b orig-b])
    (cond
      [(empty-scene? s)  s]
      [(maybe-bbox-appx-contains-bbox? b (scene-bbox s))  s]
      [(maybe-bbox-appx-disjoint? b (scene-bbox s))  empty-scene]
      ;; The shape's bounding box is partly inside and partly outside
      [(shape? s)  s]
      [(node-scene? s)
       (define s1 (node-scene-neg s))
       (define s2 (node-scene-pos s))
       (define new-s1 (loop s1 t b))
       (define new-s2 (loop s2 t b))
       (cond [(and (eq? new-s1 s1) (eq? new-s2 s2))  s]
             [else  (make-node-scene new-s1 new-s2)])]
      [(trans-scene? s)
       (define t0 (trans-scene-affine s))
       (define s0 (trans-scene-scene s))
       (define new-t (flt3compose (flt3inverse t0) t))
       (define new-b (bbox-transform orig-b new-t))
       (define new-s0 (loop s0 new-t new-b))
       (cond [(eq? new-s0 s0)  s]
             [else  (make-trans-scene new-s0 t0)])]
      [(group-scene? s)
       (define s0 (group-scene-scene s))
       (define new-s0 (loop s0 t b))
       (cond [(eq? new-s0 s0)  s]
             [else  (make-group-scene new-s0 (group-scene-tag s))])])))

(: scene-rect-cull (-> Scene FlRect3 Scene))
(define (scene-rect-cull s b)
  (if (empty-scene? s)
      empty-scene
      (let-values ([(p1 p2 p3 p4 p5 p6)  (flrect3-inside-planes b)])
        (let* ([s  (scene-rect-cull* s b)]
               [s  (scene-plane-cull s p1)]
               [s  (scene-plane-cull s p2)]
               [s  (scene-plane-cull s p3)]
               [s  (scene-plane-cull s p4)]
               [s  (scene-plane-cull s p5)]
               [s  (scene-plane-cull s p6)])
          s))))

;; ===================================================================================================
;; Scene frustum culling

(: scene-frustum-cull (-> Scene FlTransform3 Scene))
(define (scene-frustum-cull s t)
  (define planes (flprojective3-frustum-planes (->flprojective3 t)))
  (for/fold ([s : Scene  s]) ([p  (in-list planes)])
    (scene-plane-cull s p)))
