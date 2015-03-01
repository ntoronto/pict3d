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
         "../../math/flv3.rkt"
         "../../math/flt3.rkt"
         "../../math/flrect3.rkt"
         "../../gl.rkt"
         "../../utils.rkt"
         "../draw-pass.rkt"
         "../draw-passes.rkt"
         "../merge-passes.rkt"
         "../types.rkt"
         "../utils.rkt"
         "tags.rkt"
         "flags.rkt"
         "types.rkt"
         "shape.rkt")

(provide (all-defined-out))

(define-syntax-rule (maybe-force e)
  (let ([x e])
    (if (promise? x) (force x) x)))

(: value-if-forced (-> Lazy-FlRect3 Lazy-FlRect3))
(define (value-if-forced b)
  (if (and (promise? b) (promise-forced? b)) (force b) b))

(: lazy-flrect3-join (-> Lazy-FlRect3 Lazy-FlRect3 Lazy-FlRect3))
(define (lazy-flrect3-join b1 b2)
  (let ([b1  (value-if-forced b1)]
        [b2  (value-if-forced b2)])
    (if (and (flrect3? b1) (flrect3? b2))
        (flrect3-join b1 b2)
        (delay (flrect3-join (maybe-force b1) (maybe-force b2))))))

(: lazy-flrect3-trans (-> (U 'visible 'invisible) Lazy-FlRect3 Affine Nonempty-Scene Lazy-FlRect3))
(define (lazy-flrect3-trans kind b0 t0 s0)
  (let ([b0  (value-if-forced b0)])
    (define t (affine-transform t0))
    (if (flt3axial? t 1e-14)
        (if (flrect3? b0)
            (flrect3-transform b0 t)
            (delay (flrect3-transform (force b0) t)))
        (delay (nonempty-scene-trans-rect kind t0 s0)))))

;; ===================================================================================================
;; Scene constructors

(: shape->scene (-> Shape leaf-scene))
(define (shape->scene a)
  (leaf-scene (shape-visible-rect a)
              (shape-invisible-rect a)
              a))

(: make-nonempty-node-scene (-> Nonempty-Scene Nonempty-Scene node-scene))
(define (make-nonempty-node-scene s1 s2)
  (define bv (lazy-flrect3-join (nonempty-scene-lazy-visible-rect s1)
                                (nonempty-scene-lazy-visible-rect s2)))
  (define bi (lazy-flrect3-join (nonempty-scene-lazy-invisible-rect s1)
                                (nonempty-scene-lazy-invisible-rect s2)))
  (define c (fx+ (scene-count s1) (scene-count s2)))
  (define ms (tags-union (scene-tags s1) (scene-tags s2)))
  (define fs (flags-join (scene-flags s1) (scene-flags s2)))
  (node-scene bv bi c ms fs s1 s2))

(: make-node-scene (-> Scene Scene Scene))
(define (make-node-scene s1 s2)
  (cond [(empty-scene? s1)  s2]
        [(empty-scene? s2)  s1]
        [else  (make-nonempty-node-scene s1 s2)]))

(: make-simple-trans-scene (-> Affine Nonempty-Scene trans-scene))
(define (make-simple-trans-scene t0 s0)
  (define bv0 (nonempty-scene-lazy-visible-rect s0))
  (define bi0 (nonempty-scene-lazy-invisible-rect s0))
  (define bv (lazy-flrect3-trans 'visible bv0 t0 s0))
  (define bi (lazy-flrect3-trans 'invisible bi0 t0 s0))
  (trans-scene bv bi (scene-count s0) (scene-tags s0) (scene-flags s0) t0 s0))

(: make-nonempty-trans-scene (-> Affine Nonempty-Scene Nonempty-Scene))
(define (make-nonempty-trans-scene t s)
  (cond [(eq? t identity-affine)  s]
        [(leaf-scene? s)
         (define a (shape-easy-transform (leaf-scene-shape s) t))
         (if a (shape->scene a) (make-simple-trans-scene t s))]
        [(node-scene? s)
         (make-nonempty-node-scene
          (make-nonempty-trans-scene t (node-scene-neg s))
          (make-nonempty-trans-scene t (node-scene-pos s)))]
        [(trans-scene? s)
         (make-nonempty-trans-scene (affine-compose t (trans-scene-affine s))
                                    (trans-scene-scene s))]
        [else
         (make-simple-trans-scene t s)]))

(: make-trans-scene (-> Affine Scene Scene))
(define (make-trans-scene t s)
  (if (empty-scene? s) s (make-nonempty-trans-scene t s)))

(: make-group-scene (-> Tag Scene group-scene))
(define (make-group-scene n s)
  (cond [(empty-scene? s)
         (group-scene zero-flrect3 zero-flrect3 0 (singleton-tags n) empty-flags n s)]
        [else
         (group-scene (nonempty-scene-lazy-visible-rect s)
                      (nonempty-scene-lazy-invisible-rect s)
                      (scene-count s)
                      (tags-add (scene-tags s) n)
                      (scene-flags s)
                      n s)]))

;; ===================================================================================================
;; Shape bounding box

(: shape-visible-rect (-> Shape FlRect3))
(define (shape-visible-rect a)
  (cond
    [(solid-shape? a)
     (cond
       [(triangle-shape? a)   (triangle-shape-rect a)]
       [(rectangle-shape? a)  (rectangle-shape-rect a)]
       [(sphere-shape? a)     (sphere-shape-rect a)])]
    [(light-shape? a)      empty-flrect3]
    [(indicator-shape? a)  empty-flrect3]
    [(frozen-scene-shape? a)
     (frozen-scene-shape-visible-rect a)]))

(: shape-invisible-rect (-> Shape FlRect3))
(define (shape-invisible-rect a)
  (cond
    [(solid-shape? a)  empty-flrect3]
    [(light-shape? a)
     (cond
       [(directional-light-shape? a)  directional-light-shape-rect]
       [(point-light-shape? a)        (point-light-shape-rect a)])]
    [(indicator-shape? a)
     (cond
       [(point-light-shell-shape? a)  (point-light-shell-shape-rect a)])]
    [(frozen-scene-shape? a)
     (frozen-scene-shape-invisible-rect a)]))

(: nonempty-scene-trans-rect (-> (U 'visible 'invisible) Affine Nonempty-Scene FlRect3))
(define (nonempty-scene-trans-rect kind t s)
  (: nonempty-scene-lazy-rect (-> Nonempty-Scene Lazy-FlRect3))
  (define (nonempty-scene-lazy-rect s)
    (if (eq? kind 'visible)
        (nonempty-scene-lazy-visible-rect s)
        (nonempty-scene-lazy-invisible-rect s)))
  
  (let loop ([t t] [s s])
    (cond
      [(leaf-scene? s)
       (define b (maybe-force (nonempty-scene-lazy-rect s)))
       (if (empty-flrect3? b)
           empty-flrect3
           (flrect3-transform b (affine-transform t)))]
      [(node-scene? s)
       (define s1 (node-scene-neg s))
       (define s2 (node-scene-pos s))
       (flrect3-join (loop t s1)
                     (loop t s2))]
      [(trans-scene? s)
       (define t0 (trans-scene-affine s))
       (define s0 (trans-scene-scene s))
       (loop (affine-compose t t0) s0)]
      [(group-scene? s)
       (define s0 (group-scene-scene s))
       (cond [(empty-scene? s0)  (flrect3-transform zero-flrect3 (affine-transform t))]
             [else  (loop t s0)])])))

(: scene-rect (-> Scene FlRect3))
(define (scene-rect s)
  (cond [(empty-scene? s)  empty-flrect3]
        [else  (flrect3-join (scene-visible-rect s)
                             (scene-invisible-rect s))]))

(: scene-visible-rect (-> Scene FlRect3))
(define (scene-visible-rect s)
  (cond [(empty-scene? s)  empty-flrect3]
        [else  (maybe-force (nonempty-scene-lazy-visible-rect s))]))

(: scene-invisible-rect (-> Scene FlRect3))
(define (scene-invisible-rect s)
  (cond [(empty-scene? s)  empty-flrect3]
        [else  (maybe-force (nonempty-scene-lazy-invisible-rect s))]))

;; ===================================================================================================
;; Shape flags

(: scene-flags (-> Scene Flags))
(define (scene-flags s)
  (cond [(empty-scene? s)  empty-flags]
        [(leaf-scene? s)   (shape-flags (leaf-scene-shape s))]
        [else  (container-scene-child-flags s)]))

(: shape-flags (-> Shape Flags))
(define (shape-flags a)
  (cond [(solid-shape? a)  (solid-shape-flags a)]
        [(light-shape? a)  (light-shape-flags a)]
        [(indicator-shape? a)  (indicator-shape-flags a)]
        [(frozen-scene-shape? a)  (frozen-scene-shape-flags a)]))

;; ===================================================================================================
;; Scene union with rebalancing

(: node-scene-insert (-> node-scene Nonempty-Scene Nonempty-Scene))
(define (node-scene-insert s1 s2)
  (define bv1 (nonempty-scene-lazy-visible-rect s1))
  (define bi1 (nonempty-scene-lazy-invisible-rect s1))
  (define c1 (container-scene-count s1))
  (define ms1 (container-scene-child-tags s1))
  (define fs1 (container-scene-child-flags s1))
  (define s11 (node-scene-neg s1))
  (define s12 (node-scene-pos s1))
  (define bv2 (scene-visible-rect s2))
  (define bi2 (scene-invisible-rect s2))
  (cond [(and (flrect3-contains-rect? (scene-visible-rect s11) bv2)
              (flrect3-contains-rect? (scene-invisible-rect s11) bi2))
         (node-scene bv1 bi1
                     (unsafe-fx+ c1 (scene-count s2))
                     (tags-union ms1 (scene-tags s2))
                     (flags-join fs1 (scene-flags s2))
                     (nonempty-scene-union s11 s2)
                     s12)]
        [(and (flrect3-contains-rect? (scene-visible-rect s12) bv2)
              (flrect3-contains-rect? (scene-invisible-rect s12) bi2))
         (node-scene bv1 bi1
                     (unsafe-fx+ c1 (scene-count s2))
                     (tags-union ms1 (scene-tags s2))
                     (flags-join fs1 (scene-flags s2))
                     s11
                     (nonempty-scene-union s12 s2))]
        [else
         (nonempty-scene-union/rebalance s1 s2)]))

(: nonempty-scene-union (-> Nonempty-Scene Nonempty-Scene Nonempty-Scene))
(define (nonempty-scene-union s1 s2)
  (define bv1 (scene-visible-rect s1))
  (define bi1 (scene-invisible-rect s1))
  (define bv2 (scene-visible-rect s2))
  (define bi2 (scene-invisible-rect s2))
  (cond [(and (node-scene? s1)
              (flrect3-contains-rect? bv1 bv2)
              (flrect3-contains-rect? bi1 bi2))
         (node-scene-insert s1 s2)]
        [(and (node-scene? s2)
              (flrect3-contains-rect? bv2 bv1)
              (flrect3-contains-rect? bi2 bi1))
         (node-scene-insert s2 s1)]
        [else
         (nonempty-scene-union/rebalance s1 s2)]))

(: scene-union (-> Scene Scene Scene))
(define (scene-union s1 s2)
  (cond [(empty-scene? s1)  s2]
        [(empty-scene? s2)  s1]
        [else  (nonempty-scene-union s1 s2)]))

(: scene-union* (-> (Listof Scene) Scene))
(define (scene-union* ss)
  (define n (length ss))
  (let loop : Scene ([ss : (Listof Scene)  ss] [n : Index  (length ss)])
    (define n/2 (fxquotient n 2))
    (define ss1 (take ss n/2))
    (define ss2 (drop ss n/2))
    (cond [(empty? ss1)  (first ss2)]
          [(empty? ss2)  (first ss1)]  ; can't happen
          [else
           (let ([s1  (loop ss1 n/2)]
                 [s2  (loop ss2 (assert (- n n/2) index?))])
             (scene-union s1 s2))])))

;; ===================================================================================================
;; Rebalance

(: scene-rebalance-split (-> Nonempty-Scene Index Flonum (Values Scene Scene)))
(define (scene-rebalance-split s i x)
  (let loop ([s s])
    (define r (scene-rect s))
    (define xmin (flvector-ref (flrect3-min r) i))
    (define xmax (flvector-ref (flrect3-max r) i))
    (cond
      [(<= x xmin)  (values empty-scene s)]
      [(<= xmax x)  (values s empty-scene)]
      [(or (leaf-scene? s) (trans-scene? s) (group-scene? s))
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
         (values s1 s2))])))

(: nonempty-scene-union/rebalance (-> Nonempty-Scene Nonempty-Scene Nonempty-Scene))
(define (nonempty-scene-union/rebalance s1 s2)
  (define bv (flrect3-join (scene-visible-rect s1) (scene-visible-rect s2)))
  (define bi (flrect3-join (scene-invisible-rect s1) (scene-invisible-rect s2)))
  (define b (flrect3-join bv bi))
  (define c (unsafe-fx+ (scene-count s1) (scene-count s2)))
  (define ms (tags-union (scene-tags s1) (scene-tags s2)))
  (define fs (flags-join (scene-flags s1) (scene-flags s2)))
  (cond
    [(empty-flrect3? b)
     (node-scene bv bi c ms fs s1 s2)]
    [else
     (define-values (i x) (flrect3-longest-axis/center b))
     (define-values (s11 s12) (scene-rebalance-split s1 i x))
     (define-values (s21 s22) (scene-rebalance-split s2 i x))
     (cond [(empty-scene? s11)
            (if (or (empty-scene? s21) (empty-scene? s22))
                (node-scene bv bi c ms fs s1 s2)
                (node-scene bv bi c ms fs s21 (nonempty-scene-union/rebalance s1 s22)))]
           [(empty-scene? s12)
            (if (or (empty-scene? s21) (empty-scene? s22))
                (node-scene bv bi c ms fs s1 s2)
                (node-scene bv bi c ms fs (nonempty-scene-union/rebalance s1 s21) s22))]
           [(empty-scene? s21)
            (node-scene bv bi c ms fs s11 (nonempty-scene-union/rebalance s12 s2))]
           [(empty-scene? s22)
            (node-scene bv bi c ms fs (nonempty-scene-union/rebalance s11 s2) s12)]
           [else
            (node-scene bv bi c ms fs
                        (nonempty-scene-union/rebalance s11 s21)
                        (nonempty-scene-union/rebalance s12 s22))])]))

;; ===================================================================================================
;; Map and filter

(: scene-filter-shapes (-> Scene (-> Shape Boolean) Scene))
(define (scene-filter-shapes s p?)
  (let loop ([s s])
    (cond
      [(empty-scene? s)  s]
      [(leaf-scene? s)
       (if (p? (leaf-scene-shape s)) s empty-scene)]
      [(node-scene? s)
       (define s1 (node-scene-neg s))
       (define s2 (node-scene-pos s))
       (define new-s1 (loop s1))
       (define new-s2 (loop s2))
       (cond [(and (eq? new-s1 s1) (eq? new-s2 s2))  s]
             [else  (make-node-scene new-s1 new-s2)])]
      [(trans-scene? s)
       (define t0 (trans-scene-affine s))
       (define s0 (trans-scene-scene s))
       (define new-s0 (loop s0))
       (cond [(eq? new-s0 s0)  s]
             [else  (make-trans-scene t0 new-s0)])]
      [(group-scene? s)
       (define n0 (group-scene-tag s))
       (define s0 (group-scene-scene s))
       (define new-s0 (loop s0))
       (cond [(eq? new-s0 s0)  s]
             [else  (make-group-scene n0 new-s0)])])))

(: scene-map-shapes (-> Scene (-> Shape Shape) Scene))
(define (scene-map-shapes s f)
  (let loop ([s s])
    (cond
      [(empty-scene? s)  s]
      [(leaf-scene? s)
       (define a (leaf-scene-shape s))
       (define new-a (f a))
       (cond [(eq? new-a a)  s]
             [else  (shape->scene new-a)])]
      [(node-scene? s)
       (define s1 (node-scene-neg s))
       (define s2 (node-scene-pos s))
       (define new-s1 (loop s1))
       (define new-s2 (loop s2))
       (cond [(and (eq? new-s1 s1) (eq? new-s2 s2))  s]
             [else  (make-node-scene new-s1 new-s2)])]
      [(trans-scene? s)
       (define t0 (trans-scene-affine s))
       (define s0 (trans-scene-scene s))
       (define new-s0 (loop s0))
       (cond [(eq? new-s0 s0)  s]
             [else  (make-trans-scene t0 new-s0)])]
      [(group-scene? s)
       (define n0 (group-scene-tag s))
       (define s0 (group-scene-scene s))
       (define new-s0 (loop s0))
       (cond [(eq? new-s0 s0)  s]
             [else  (make-group-scene n0 new-s0)])])))

;; ===================================================================================================
;; Mapping over groups

(: scene-map-group/transform (All (A) (-> Scene Tag (-> Affine group-scene A) (Listof A))))
(define (scene-map-group/transform s n f)
  (reverse
   (let loop ([t : Affine  identity-affine] [s s] [as : (Listof A)  empty])
     (cond
       [(empty-scene? s)  as]
       [(leaf-scene? s)  as]
       [(not (tags-contain? (container-scene-child-tags s) n))  as]
       [(node-scene? s)
        (define s1 (node-scene-neg s))
        (define s2 (node-scene-pos s))
        (loop t s2 (loop t s1 as))]
       [(trans-scene? s)
        (define t0 (trans-scene-affine s))
        (define s0 (trans-scene-scene s))
        (loop (affine-compose t t0) s0 as)]
       [(group-scene? s)
        (define n0 (group-scene-tag s))
        (define s0 (group-scene-scene s))
        (cond [(equal? n0 n)  (cons (f t s) as)]
              [else  (loop t s0 as)])]))))

(: scene-map-group (All (A) (-> Scene Tag (-> group-scene A) (Listof A))))
(define (scene-map-group s n f)
  (reverse
   (let loop ([s s] [as : (Listof A)  empty])
     (cond
       [(empty-scene? s)  as]
       [(leaf-scene? s)  as]
       [(not (tags-contain? (container-scene-child-tags s) n))  as]
       [(node-scene? s)
        (define s1 (node-scene-neg s))
        (define s2 (node-scene-pos s))
        (loop s2 (loop s1 as))]
       [(trans-scene? s)
        (define s0 (trans-scene-scene s))
        (loop s0 as)]
       [(group-scene? s)
        (define n0 (group-scene-tag s))
        (define s0 (group-scene-scene s))
        (cond [(equal? n0 n)  (cons (f s) as)]
              [else  (loop s0 as)])]))))

;; ===================================================================================================

(: scene-group-transforms (-> Scene (U 'empty 'nonempty 'all) (Listof (Pair Tag Affine))))
(define (scene-group-transforms s which)
  (let loop ([t : Affine  identity-affine] [s s])
    (cond
      [(empty-scene? s)  empty]
      [(leaf-scene? s)  empty]
      [(node-scene? s)
       (define s1 (node-scene-neg s))
       (define s2 (node-scene-pos s))
       (append (loop t s1) (loop t s2))]
      [(trans-scene? s)
       (define t0 (trans-scene-affine s))
       (define s0 (trans-scene-scene s))
       (loop (affine-compose t t0) s0)]
      [(group-scene? s)
       (define n0 (group-scene-tag s))
       (define s0 (group-scene-scene s))
       (case which
         [(empty)     (if (empty-scene? s0) (list (cons n0 t)) (loop t s0))]
         [(nonempty)  (if (empty-scene? s0) empty (cons (cons n0 t) (loop t s0)))]
         [(all)       (cons (cons n0 t) (loop t s0))])])))

;; ===================================================================================================
;; Replace groups or within groups - like a fold for Scene, but just on groups

(: scene-replace-group (-> Scene Tag (-> group-scene Scene) Scene))
(define (scene-replace-group s n f)
  (let loop ([s s])
    (cond
      [(empty-scene? s)  s]
      [(leaf-scene? s)  s]
      [(not (tags-contain? (container-scene-child-tags s) n))  s]
      [(node-scene? s)
       (define s1 (node-scene-neg s))
       (define s2 (node-scene-pos s))
       (define new-s1 (loop s1))
       (define new-s2 (loop s2))
       (cond [(and (eq? new-s1 s1) (eq? new-s2 s2))  s]
             [else  (make-node-scene new-s1 new-s2)])]
      [(trans-scene? s)
       (define t0 (trans-scene-affine s))
       (define s0 (trans-scene-scene s))
       (define new-s0 (loop s0))
       (cond [(eq? new-s0 s0)  s]
             [else  (make-trans-scene t0 new-s0)])]
      [(group-scene? s)
       (define n0 (group-scene-tag s))
       (define s0 (group-scene-scene s))
       (cond [(equal? n0 n)  (f s)]
             [else
              (define new-s0 (loop s0))
              (cond [(eq? new-s0 s0)  s]
                    [else  (make-group-scene n0 new-s0)])])])))

(: scene-replace-in-group (-> Scene Tag (-> Scene Scene) Scene))
(define (scene-replace-in-group s n f)
  (scene-replace-group
   s n (λ (s)
         (define n0 (group-scene-tag s))
         (define s0 (group-scene-scene s))
         (define new-s0 (f s0))
         (cond [(eq? new-s0 s0)  s]
               [else  (make-group-scene n0 new-s0)]))))

;; ===================================================================================================
;; Scene-ray-intersection

(: shape-line-intersect (-> Shape FlVector FlVector (U #f line-hit)))
(define (shape-line-intersect a v dv)
  (cond
    [(solid-shape? a)
     (cond
       [(triangle-shape? a)   (triangle-shape-line-intersect a v dv)]
       [(rectangle-shape? a)  (rectangle-shape-line-intersect a v dv)]
       [(sphere-shape? a)     (sphere-shape-line-intersect a v dv)])]
    [else  #f]))

(: transform-line-hit (-> line-hit FlAffine3- line-hit))
(define (transform-line-hit h t)
  (cond [(flidentity3? t)  h]
        [else
         (line-hit
          (line-hit-distance h)
          (λ () (flt3apply/pos t (line-hit-point h)))
          (λ () (let ([n  (line-hit-normal h)])
                  (and n (flt3apply/nrm t n)))))]))

(: nonempty-scene-ray-intersect (-> Nonempty-Scene FlVector FlVector (U #f line-hit)))
(define (nonempty-scene-ray-intersect s v dv)
  (let loop ([s s] [v v] [dv dv] [t : FlAffine3-  identity-flt3])
    (define tv (flt3apply/pos t v))
    (define tdv (flt3apply/dir t dv))
    (define bb (scene-rect s))
    (define-values (tmin tmax) (flrect3-line-intersects bb tv tdv))
    (cond
      [(or (not tmin) (not tmax) (and (< tmin 0.0) (< tmax 0.0)))  #f]
      [(leaf-scene? s)
       (define a (leaf-scene-shape s))
       (cond [(frozen-scene-shape? a)
              (loop (frozen-scene-shape-scene a) v dv t)]
             [else
              (define h (shape-line-intersect a tv tdv))
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
       (define t0 (flt3inverse (affine-transform (trans-scene-affine s))))
       (define s0 (trans-scene-scene s))
       (loop s0 v dv (flt3compose t0 t))]
      [(group-scene? s)
       (define s0 (group-scene-scene s))
       (cond [(empty-scene? s0)  #f]
             [else  (loop s0 v dv t)])])))

(: scene-ray-intersect (-> Scene FlVector FlVector (U #f line-hit)))
(define (scene-ray-intersect s v dv)
  (if (empty-scene? s) #f (nonempty-scene-ray-intersect s v dv)))

;; ===================================================================================================
;; Tastes almost-but-not-quite-entirely-unlike map

(: transform-planes (-> Affine (Listof FlPlane3) (Listof FlPlane3)))
(define (transform-planes t0 planes)
  (if (flidentity3? t0)
      planes
      (let ([tinv0  (flt3inverse (affine-transform t0))])
        (for/fold ([planes : (Listof FlPlane3)  empty]) ([p  (in-list planes)])
          (define new-p (flt3apply/pln tinv0 p))
          (if new-p (cons new-p planes) planes)))))

(: nonempty-scene-for-each! (-> Nonempty-Scene
                                (Listof FlPlane3)
                                (-> Shape FlRect3 Affine Nonnegative-Fixnum Any)
                                Nonnegative-Fixnum
                                Nonnegative-Fixnum))
(define (nonempty-scene-for-each! s planes f start)
  (let loop ([s s]
             [t : Affine  identity-affine]
             [parent-planes : (Listof FlPlane3)  planes]
             [i : Nonnegative-Fixnum  start])
    (: side (U 'inside 'outside 'both))
    (define side
      (cond [(empty? parent-planes)  'inside]
            [(< (scene-count s) 4)  'inside]
            [else  (flrect3-classify/planes (scene-rect s) parent-planes)]))
    (define planes (if (eq? side 'inside) empty parent-planes))
    (cond
      [(eq? side 'outside)   i]
      [(leaf-scene? s)
       (f (leaf-scene-shape s) (scene-rect s) t i)
       (unsafe-fx+ i 1)]
      [(node-scene? s)
       (let* ([i  (loop (node-scene-neg s) t planes i)]
              [i  (loop (node-scene-pos s) t planes i)])
         i)]
      [(trans-scene? s)
       (define s0 (trans-scene-scene s))
       (define t0 (trans-scene-affine s))
       (loop s0
             (affine-compose t t0)
             (transform-planes t0 planes)
             i)]
      [(group-scene? s)
       (define s0 (group-scene-scene s))
       (cond [(empty-scene? s0)  i]
             [else  (loop s0 t planes i)])])))

(: scene-for-each! (-> Scene
                       (Listof FlPlane3)
                       (-> Shape FlRect3 Affine Nonnegative-Fixnum Any)
                       Nonnegative-Fixnum
                       Nonnegative-Fixnum))
(define (scene-for-each! s planes f start)
  (if (empty-scene? s)
      start
      (nonempty-scene-for-each! s planes f start)))

(: scene-extract (All (B) (-> Scene (Listof FlPlane3) (-> Shape FlRect3 Affine B) (Listof B))))
(define (scene-extract s planes f)
  (: bs (Listof B))
  (define bs empty)
  (scene-for-each! s
                   planes
                   (λ ([a : Shape] [r : FlRect3] [t : Affine] [i : Nonnegative-Fixnum])
                     (set! bs (cons (f a r t) bs)))
                   0)
  bs)

;; ===================================================================================================
;; Scene plane culling

(: scene-plane-cull (-> Scene FlPlane3 Scene))
(define (scene-plane-cull s p)
  (let loop ([s s] [p p])
    (cond
      [(empty-scene? s)  s]
      [else
       (define side (flrect3-plane-side (scene-rect s) p))
       (cond
         [(or (eq? side 'pos) (eq? side 'poszero) (eq? side 'zero))  s]
         [(eq? side 'neg)  empty-scene]
         ;; side is either 'negzero or 'both
         [(leaf-scene? s)  s]
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
          (define new-p (flt3apply/pln (flt3inverse (affine-transform t0)) p))
          (cond [new-p  (define new-s0 (loop s0 new-p))
                        (cond [(eq? new-s0 s0)  s]
                              [else  (make-trans-scene t0 new-s0)])]
                [else  empty-scene])]
         [(group-scene? s)
          (define n0 (group-scene-tag s))
          (define s0 (group-scene-scene s))
          (define new-s0 (loop s0 p))
          (cond [(eq? new-s0 s0)  s]
                [else  (make-group-scene n0 new-s0)])])])))

;; ===================================================================================================
;; Scene rect culling

(: scene-rect-cull* (-> Scene FlRect3 Scene))
(define (scene-rect-cull* s orig-b)
  (let loop ([s s] [t : Affine  identity-affine] [b orig-b])
    (cond
      [(empty-scene? s)  s]
      [(flrect3-contains-rect? b (scene-rect s))  s]
      [(flrect3-disjoint? b (scene-rect s))  empty-scene]
      ;; The shape's bounding box is partly inside and partly outside
      [(leaf-scene? s)  s]
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
       (define new-t (affine-compose (affine-inverse t0) t))
       (define new-b (flrect3-transform orig-b (affine-transform new-t)))
       (define new-s0 (loop s0 new-t new-b))
       (cond [(eq? new-s0 s0)  s]
             [else  (make-trans-scene t0 new-s0)])]
      [(group-scene? s)
       (define n0 (group-scene-tag s))
       (define s0 (group-scene-scene s))
       (define new-s0 (loop s0 t b))
       (cond [(eq? new-s0 s0)  s]
             [else  (make-group-scene n0 new-s0)])])))

(: scene-rect-cull (-> Scene FlRect3 Scene))
(define (scene-rect-cull s b)
  (if (empty-scene? s)
      empty-scene
      (for/fold ([s : Scene  (scene-rect-cull* s b)])
                ([p  (in-list (flrect3-inside-planes b))])
        (scene-plane-cull s p))))

;; ===================================================================================================
;; Scene frustum culling

(: scene-frustum-cull (-> Scene FlTransform3 Scene))
(define (scene-frustum-cull s t)
  (define planes (flprojective3-frustum-planes (->flprojective3 t)))
  (for/fold ([s : Scene  s]) ([p  (in-list planes)])
    (scene-plane-cull s p)))

;; ===================================================================================================
;; Shape and scene transformation (forced, not lazy)

(: shape-easy-transform (-> Shape Affine (U #f Shape)))
(define (shape-easy-transform a t)
  (cond
    [(flidentity3? t)  a]
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

(: shape-transform (-> Shape Affine (Listof Shape)))
(define (shape-transform a t)
  (cond
    [(flidentity3? t)  (list a)]
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

(: scene-transform-shapes (-> Scene Affine Scene))
(define (scene-transform-shapes s t)
  (cond
    [(empty-scene? s)  s]
    [(leaf-scene? s)
     (define a (leaf-scene-shape s))
     (scene-union* (map shape->scene (shape-transform a t)))]
    [(node-scene? s)
     (define s1 (node-scene-neg s))
     (define s2 (node-scene-pos s))
     (make-node-scene (scene-transform-shapes s1 t)
                      (scene-transform-shapes s2 t))]
    [(trans-scene? s)
     (define t0 (trans-scene-affine s))
     (define s0 (trans-scene-scene s))
     (scene-transform-shapes s0 (affine-compose t t0))]
    [(group-scene? s)
     (define s0 (group-scene-scene s))
     (scene-transform-shapes s0 t)]))

;; ===================================================================================================
;; Scene drawing

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

(define get-scene-draw-passes
  (make-gl-cached-vector
   'get-scene-draw-passes
   (λ ([n : Integer])
     (log-pict3d-info "<engine> creating draw-passes vector for ~v shapes" n)
     (build-vector n (λ (_) (draw-passes empty-passes identity-affine))))
   vector-length))

(: draw-scene (-> Scene Natural Natural FlAffine3- FlTransform3 FlVector FlVector Void))
(define (draw-scene s width height view proj background ambient)
  (define t (flt3compose proj view))
  (define planes (flprojective3-frustum-planes (->flprojective3 t)))
  (define bs (get-scene-draw-passes (scene-count s)))
  (define end
    (scene-for-each! s
                     planes
                     (λ ([a : Shape] [_ : FlRect3] [t : Affine] [i : Nonnegative-Fixnum])
                       (define b (vector-ref bs i))
                       (set-draw-passes-passes! b (shape-passes a))
                       (set-draw-passes-affine! b t))
                     0))
  (draw-draw-passes bs end width height view proj background ambient))

(: draw-scenes (-> (Listof Scene) Natural Natural FlAffine3- FlTransform3 FlVector FlVector Void))
(define (draw-scenes ss width height view proj background ambient)
  (define t (flt3compose proj view))
  (define planes (flprojective3-frustum-planes (->flprojective3 t)))
  (define num
    (for/fold ([num : Nonnegative-Fixnum  0]) ([s  (in-list ss)])
      (unsafe-fx+ num (scene-count s))))
  (define bs (get-scene-draw-passes num))
  (define end
    (for/fold ([end : Nonnegative-Fixnum  0]) ([s  (in-list ss)])
      (scene-for-each! s
                       planes
                       (λ ([a : Shape] [_ : FlRect3] [t : Affine] [i : Nonnegative-Fixnum])
                         (define b (vector-ref bs i))
                         (set-draw-passes-passes! b (shape-passes a))
                         (set-draw-passes-affine! b t))
                       end)))
  (draw-draw-passes bs end width height view proj background ambient))

;; ===================================================================================================
;; Set shape attributes

(: shape-set-color (-> Shape FlVector Shape))
(define (shape-set-color a c)
  (cond
    [(solid-shape? a)
     (cond
       [(triangle-shape? a)   (set-triangle-shape-color a c)]
       [(rectangle-shape? a)  (set-rectangle-shape-color a c)]
       [(sphere-shape? a)     (set-sphere-shape-color a c)])]
    [(frozen-scene-shape? a)  (set-frozen-scene-shape-color a c)]
    [else  a]))

(: shape-set-emitted (-> Shape FlVector Shape))
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

(: shape-set-material (-> Shape material Shape))
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
  (merge-passes
   (append* (scene-extract (scene-transform-shapes (frozen-scene-shape-scene a)
                                                   identity-affine)
                           empty
                           (λ ([s : Shape] [_ : FlRect3] [t : Affine])
                             (map shape-passes (shape-transform s t)))))))

;; ===================================================================================================
;; Set attributes

(: set-frozen-scene-shape-color (-> frozen-scene-shape FlVector frozen-scene-shape))
(define (set-frozen-scene-shape-color a c)
  (cond [(not (= (flvector-length c) 4))
         (raise-argument-error 'set-frozen-scene-shape-color "length-4 flvector" 1 a c)]
        [else
         (define s (frozen-scene-shape-scene a))
         (define new-s (scene-map-shapes s (λ ([a : Shape]) (shape-set-color a c))))
         (cond [(eq? new-s s)  a]
               [else  (make-frozen-scene-shape (assert new-s nonempty-scene?))])]))

(: set-frozen-scene-shape-emitted (-> frozen-scene-shape FlVector frozen-scene-shape))
(define (set-frozen-scene-shape-emitted a e)
  (cond [(not (= (flvector-length e) 4))
         (raise-argument-error 'set-frozen-scene-shape-emitted "length-4 flvector" 1 a e)]
        [else
         (define s (frozen-scene-shape-scene a))
         (define new-s (scene-map-shapes s (λ ([a : Shape]) (shape-set-emitted a e))))
         (cond [(eq? new-s s)  a]
               [else  (make-frozen-scene-shape (assert new-s nonempty-scene?))])]))

(: set-frozen-scene-shape-material (-> frozen-scene-shape material frozen-scene-shape))
(define (set-frozen-scene-shape-material a m)
  (define s (frozen-scene-shape-scene a))
  (define new-s (scene-map-shapes s (λ ([a : Shape]) (shape-set-material a m))))
  (cond [(eq? new-s s)  a]
        [else  (make-frozen-scene-shape (assert new-s nonempty-scene?))]))

;; ===================================================================================================
;; Bounding box

(: frozen-scene-shape-visible-rect (-> frozen-scene-shape FlRect3))
(define (frozen-scene-shape-visible-rect a)
  (scene-visible-rect (frozen-scene-shape-scene a)))

(: frozen-scene-shape-invisible-rect (-> frozen-scene-shape FlRect3))
(define (frozen-scene-shape-invisible-rect a)
  (scene-invisible-rect (frozen-scene-shape-scene a)))

;; ===================================================================================================
;; Flags

(: frozen-scene-shape-flags (-> frozen-scene-shape Flags))
(define (frozen-scene-shape-flags a)
  (scene-flags (frozen-scene-shape-scene a)))

;; ===================================================================================================
;; Transform

(: frozen-scene-shape-transform (-> frozen-scene-shape Affine (Listof Shape)))
(define (frozen-scene-shape-transform a t)
  (append*
   (scene-extract (scene-transform-shapes (frozen-scene-shape-scene a) t)
                  empty
                  (λ ([a : Shape] [_ : FlRect3] [t : Affine])
                    (shape-transform a t)))))
