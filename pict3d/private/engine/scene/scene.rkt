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
parametric polymorphism and no higher-order types, Typed Racket should generate an O(1) flat contract
for `Scene`.
|#

(require racket/unsafe/ops
         racket/match
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
         "../affine.rkt"
         "../types.rkt"
         "../utils.rkt"
         "tags.rkt"
         "types.rkt"
         "shape.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Scene constructors

(: make-nonempty-node-scene (-> Nonempty-Scene Nonempty-Scene Nonempty-Scene))
(define (make-nonempty-node-scene s1 s2)
  (define b (flrect3-join (nonempty-scene-rect s1) (nonempty-scene-rect s2)))
  (define c (fx+ (scene-count s1) (scene-count s2)))
  (define ms (tags-union (scene-tags s1) (scene-tags s2)))
  (node-scene b c ms s1 s2))

(: make-node-scene (-> Scene Scene Scene))
(define (make-node-scene s1 s2)
  (cond [(empty-scene? s1)  s2]
        [(empty-scene? s2)  s1]
        [else  (make-nonempty-node-scene s1 s2)]))

(: make-nonempty-trans-scene (-> FlAffine3- Nonempty-Scene Nonempty-Scene))
(define (make-nonempty-trans-scene t s)
  (cond [(flidentity3? t)  s]
        [(trans-scene? s)
         (make-nonempty-trans-scene (flt3compose t (trans-scene-affine s))
                                    (trans-scene-scene s))]
        [else
         (trans-scene (flrect3-transform (nonempty-scene-rect s) t)
                      (scene-count s)
                      (scene-tags s)
                      t s)]))

(: make-trans-scene (-> FlAffine3- Scene Scene))
(define (make-trans-scene t s)
  (if (empty-scene? s) s (make-nonempty-trans-scene t s)))

(define zero-rect (nonempty-flrect3 (flvector 0.0 0.0 0.0) (flvector 0.0 0.0 0.0)))

(: make-group-scene (-> Tag Scene Nonempty-Scene))
(define (make-group-scene n s)
  (cond [(empty-scene? s)
         (group-scene zero-rect 0 (singleton-tags n) n s)]
        [else
         (group-scene (nonempty-scene-rect s)
                      (scene-count s)
                      (tags-add (scene-tags s) n)
                      n s)]))

;; ===================================================================================================
;; Scene union with rebalancing

(: node-scene-insert (-> node-scene Nonempty-Scene Nonempty-Scene))
(define (node-scene-insert s1 s2)
  (match-define (node-scene r1 c1 ms1 s11 s12) s1)
  (define r2 (nonempty-scene-rect s2))
  (cond [(flrect3-contains-rect? (nonempty-scene-rect s11) r2)
         (node-scene r1
                     (unsafe-fx+ c1 (scene-count s2))
                     (tags-union ms1 (scene-tags s2))
                     (nonempty-scene-union s11 s2)
                     s12)]
        [(flrect3-contains-rect? (nonempty-scene-rect s12) r2)
         (node-scene r1
                     (unsafe-fx+ c1 (scene-count s2))
                     (tags-union ms1 (scene-tags s2))
                     s11
                     (nonempty-scene-union s12 s2))]
        [else
         (nonempty-scene-union/rebalance s1 s2)]))

(: nonempty-scene-union (-> Nonempty-Scene Nonempty-Scene Nonempty-Scene))
(define (nonempty-scene-union s1 s2)
  (define r1 (nonempty-scene-rect s1))
  (define r2 (nonempty-scene-rect s2))
  (cond [(and (node-scene? s1) (flrect3-contains-rect? r1 r2))
         (node-scene-insert s1 s2)]
        [(and (node-scene? s2) (flrect3-contains-rect? r2 r1))
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
  (define r (flrect3-join (nonempty-scene-rect s1) (nonempty-scene-rect s2)))
  (define c (unsafe-fx+ (scene-count s1) (scene-count s2)))
  (define ms (tags-union (scene-tags s1) (scene-tags s2)))
  (define-values (i x) (flrect3-longest-axis/center r))
  (define-values (s11 s12) (scene-rebalance-split s1 i x))
  (define-values (s21 s22) (scene-rebalance-split s2 i x))
  (cond [(empty-scene? s11)
         (if (or (empty-scene? s21) (empty-scene? s22))
             (node-scene r c ms s1 s2)
             (node-scene r c ms s21 (nonempty-scene-union/rebalance s1 s22)))]
        [(empty-scene? s12)
         (if (or (empty-scene? s21) (empty-scene? s22))
             (node-scene r c ms s1 s2)
             (node-scene r c ms (nonempty-scene-union/rebalance s1 s21) s22))]
        [(empty-scene? s21)
         (node-scene r c ms s11 (nonempty-scene-union/rebalance s12 s2))]
        [(empty-scene? s22)
         (node-scene r c ms (nonempty-scene-union/rebalance s11 s2) s12)]
        [else
         (node-scene r c ms
                     (nonempty-scene-union/rebalance s11 s21)
                     (nonempty-scene-union/rebalance s12 s22))]))

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

(: scene-map-group/transform (All (A) (-> Scene Tag (-> FlAffine3- group-scene A) (Listof A))))
(define (scene-map-group/transform s n f)
  (reverse
   (let loop ([t : FlAffine3-  identity-flt3] [s s] [as : (Listof A)  empty])
     (cond
       [(empty-scene? s)  as]
       [(leaf-scene? s)  as]
       [(not (tags-contain? (container-scene-tags s) n))  as]
       [(node-scene? s)
        (define s1 (node-scene-neg s))
        (define s2 (node-scene-pos s))
        (loop t s2 (loop t s1 as))]
       [(trans-scene? s)
        (define t0 (trans-scene-affine s))
        (define s0 (trans-scene-scene s))
        (loop (flt3compose t t0) s0 as)]
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
       [(not (tags-contain? (container-scene-tags s) n))  as]
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

(: scene-all-group-transforms (-> Scene (Listof FlAffine3-)))
(define (scene-all-group-transforms s)
  (let loop ([t : FlAffine3-  identity-flt3] [s s])
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
       (loop (flt3compose t t0) s0)]
      [(group-scene? s)
       (define n0 (group-scene-tag s))
       (define s0 (group-scene-scene s))
       (cons t (loop t s0))])))

;; ===================================================================================================
;; Replace groups or within groups - like a fold for Scene, but just on groups

(: scene-replace-group (-> Scene Tag (-> group-scene Scene) Scene))
(define (scene-replace-group s n f)
  (let loop ([s s])
    (cond
      [(empty-scene? s)  s]
      [(leaf-scene? s)  s]
      [(not (tags-contain? (container-scene-tags s) n))  s]
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
       (match-define (group-scene b c ms n0 s0) s)
       (cond [(equal? n0 n)  (f s)]
             [else
              (define new-s0 (loop s0))
              (cond [(eq? new-s0 s0)  s]
                    [else  (make-group-scene n0 new-s0)])])])))

(: scene-replace-in-group (-> Scene Tag (-> Scene Scene) Scene))
(define (scene-replace-in-group s n f)
  (scene-replace-group
   s n (λ (s)
         (match-define (group-scene b c ms n0 s0) s)
         (define new-s0 (f s0))
         (cond [(eq? new-s0 s0)  s]
               [else  (make-group-scene n0 new-s0)]))))

;; ===================================================================================================
;; Tastes almost-but-not-quite-entirely-unlike map

(: transform-planes (-> FlAffine3- (Listof FlPlane3) (Listof FlPlane3)))
(define (transform-planes t0 planes)
  (if (flidentity3? t0)
      planes
      (let ([tinv0  (flt3inverse t0)])
        (for/fold ([planes : (Listof FlPlane3)  empty]) ([p  (in-list planes)])
          (define new-p (flt3apply/pln tinv0 p))
          (if new-p (cons new-p planes) planes)))))

(: nonempty-scene-for-each! (-> Nonempty-Scene
                                (Listof FlPlane3)
                                (-> Shape affine Nonnegative-Fixnum Any)
                                Nonnegative-Fixnum
                                Nonnegative-Fixnum))
(define (nonempty-scene-for-each! s planes f start)
  (let loop ([s s]
             [t : affine  identity-affine]
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
       (f (leaf-scene-shape s) t i)
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
                       (-> Shape affine Nonnegative-Fixnum Any)
                       Nonnegative-Fixnum
                       Nonnegative-Fixnum))
(define (scene-for-each! s planes f start)
  (if (empty-scene? s)
      start
      (nonempty-scene-for-each! s planes f start)))

(: scene-extract (All (B) (-> Scene (Listof FlPlane3) (-> Shape affine B) (Listof B))))
(define (scene-extract s planes f)
  (: bs (Listof B))
  (define bs empty)
  (scene-for-each! s
                   planes
                   (λ ([a : Shape] [t : affine] [i : Nonnegative-Fixnum])
                     (set! bs (cons (f a t) bs)))
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
          (define new-p (flt3apply/pln (flt3inverse t0) p))
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
  (let loop ([s s] [t : FlAffine3-  identity-flt3] [b orig-b])
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
       (define new-t (flt3compose (flt3inverse t0) t))
       (define new-b (flrect3-transform orig-b new-t))
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

(: shape-transform (-> Shape FlAffine3- (Listof Shape)))
(define (shape-transform a t)
  (cond
    [(flidentity3? t)  (list a)]
    [(solid-shape? a)
     (cond
       [(triangle-shape? a)   (triangle-shape-transform a t)]
       [(rectangle-shape? a)  (rectangle-shape-transform a t)]
       [(sphere-shape? a)     (sphere-shape-transform a t)])]
    [(light-shape? a)
     (cond
       [(directional-light-shape? a)  (directional-light-shape-transform a t)]
       [(point-light-shape? a)        (point-light-shape-transform a t)])]
    ;; Frozen scene
    [(frozen-scene-shape? a)
     (frozen-scene-shape-transform a t)]))

(: scene-transform-shapes (-> Scene FlAffine3- Scene))
(define (scene-transform-shapes s t)
  (cond
    [(empty-scene? s)  s]
    [(leaf-scene? s)
     (match-define (leaf-scene b a) s)
     (scene-union* (map shape->scene (shape-transform a t)))]
    [(node-scene? s)
     (define s1 (node-scene-neg s))
     (define s2 (node-scene-pos s))
     (make-node-scene (scene-transform-shapes s1 t)
                      (scene-transform-shapes s2 t))]
    [(trans-scene? s)
     (define t0 (trans-scene-affine s))
     (define s0 (trans-scene-scene s))
     (scene-transform-shapes s0 (flt3compose t t0))]
    [(group-scene? s)
     (define s0 (group-scene-scene s))
     (scene-transform-shapes s0 t)]))

;; ===================================================================================================
;; Scene drawing

(: shape-passes (-> Shape Passes))
(define (shape-passes a)
  (lazy-box-ref!
   (shape-lazy-passes a)
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
       [(frozen-scene-shape? a)
        (make-frozen-scene-shape-passes a)]))))

(define get-scene-draw-passes
  (make-cached-vector 'get-scene-draw-passes
                      (λ ([n : Integer])
                        (log-pict3d-info "<engine> creating draw-passes vector for ~v shapes" n)
                        (build-vector n (λ (_) (draw-passes #() identity-affine))))
                      vector-length))

(: draw-scene (-> Scene Natural Natural FlAffine3- FlTransform3 FlVector FlVector Flonum Void))
(define (draw-scene s width height view proj background ambient-color ambient-intensity)
  (define t (flt3compose proj view))
  (define planes (flprojective3-frustum-planes (->flprojective3 t)))
  (define bs (get-scene-draw-passes (scene-count s)))
  (define end
    (scene-for-each! s
                     planes
                     (λ ([a : Shape] [t : affine] [i : Nonnegative-Fixnum])
                       (define b (vector-ref bs i))
                       (set-draw-passes-passes! b (shape-passes a))
                       (set-draw-passes-transform! b t))
                     0))
  (draw-draw-passes bs end width height
                    view proj
                    background ambient-color ambient-intensity))

(: draw-scenes
   (-> (Listof Scene) Natural Natural FlAffine3- FlTransform3 FlVector FlVector Flonum Void))
(define (draw-scenes ss width height view proj background ambient-color ambient-intensity)
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
                       (λ ([a : Shape] [t : affine] [i : Nonnegative-Fixnum])
                         (define b (vector-ref bs i))
                         (set-draw-passes-passes! b (shape-passes a))
                         (set-draw-passes-transform! b t))
                       end)))
  (draw-draw-passes bs end width height
                    view proj
                    background ambient-color ambient-intensity))

;; ===================================================================================================
;; Shape bounding box

(: shape->scene (-> Shape Nonempty-Scene))
(define (shape->scene a)
  (leaf-scene (shape-rect a) a))

(: shape-rect (-> Shape Nonempty-FlRect3))
(define (shape-rect a)
  (cond
    [(solid-shape? a)
     (cond
       [(triangle-shape? a)   (triangle-shape-rect a)]
       [(rectangle-shape? a)  (rectangle-shape-rect a)]
       [(sphere-shape? a)     (sphere-shape-rect a)])]
    [(light-shape? a)
     (cond
       [(directional-light-shape? a)  directional-light-shape-rect]
       [(point-light-shape? a)        (point-light-shape-rect a)])]
    [(frozen-scene-shape? a)
     (frozen-scene-shape-rect a)]))

;; ===================================================================================================
;; Shape color

(: shape-set-color (-> Shape FlVector Shape))
(define (shape-set-color a c)
  (cond
    [(solid-shape? a)
     (cond
       [(triangle-shape? a)   (set-triangle-shape-color a c)]
       [(rectangle-shape? a)  (set-rectangle-shape-color a c)]
       [(sphere-shape? a)     (set-sphere-shape-color a c)])]
    [else  a]))

(: shape-set-emitted (-> Shape FlVector Shape))
(define (shape-set-emitted a e)
  (cond
    [(solid-shape? a)
     (cond
       [(triangle-shape? a)   (set-triangle-shape-emitted a e)]
       [(rectangle-shape? a)  (set-rectangle-shape-emitted a e)]
       [(sphere-shape? a)     (set-sphere-shape-emitted a e)])]
    [else  a]))

(: shape-set-material (-> Shape material Shape))
(define (shape-set-material a m)
  (cond
    [(solid-shape? a)
     (cond
       [(triangle-shape? a)   (set-triangle-shape-material a m)]
       [(rectangle-shape? a)  (set-rectangle-shape-material a m)]
       [(sphere-shape? a)     (set-sphere-shape-material a m)])]
    [else  a]))

;; ===================================================================================================
;; ===================================================================================================
;; ===================================================================================================
;; Frozen scene shape

;; ===================================================================================================
;; Constructors

(: make-frozen-scene-shape (-> Nonempty-Scene Shape))
(define (make-frozen-scene-shape s)
  (frozen-scene-shape (box 'lazy) s))

;; ===================================================================================================
;; Passes

(: get-vertex-count (-> Boolean
                        (Vectorof shape-params)
                        Nonnegative-Fixnum
                        Nonnegative-Fixnum
                        Nonnegative-Fixnum))
(define (get-vertex-count indexed? ps start end)
  (for/fold ([vertex-count : Nonnegative-Fixnum  0]) ([i  (in-range start end)])
    (define v (shape-params-vertices (unsafe-vector-ref ps i)))
    (if (eq? indexed? (and (vertices-indexes v) #t))
        (unsafe-fx+ vertex-count (vertices-vertex-count v))
        vertex-count)))

(: get-index-count (-> (Vectorof shape-params)
                       Nonnegative-Fixnum
                       Nonnegative-Fixnum
                       Nonnegative-Fixnum))
(define (get-index-count ps start end)
  (for/fold ([index-count : Nonnegative-Fixnum  0]) ([i  (in-range start end)])
    (define v (shape-params-vertices (unsafe-vector-ref ps i)))
    (define indexes (vertices-indexes v))
    (if indexes
        (unsafe-fx+ index-count (vector-length indexes))
        index-count)))

(: merge-vertex-data (-> program-spec
                         Boolean
                         (Vectorof shape-params)
                         Nonnegative-Fixnum
                         Nonnegative-Fixnum
                         Nonnegative-Fixnum
                         Bytes))
(define (merge-vertex-data pd indexed? ps start end vertex-count)
  (define struct-size (vao-struct-size (gl-program-struct (program-spec-program pd))))
  (define buffer-size (unsafe-fx* vertex-count struct-size))
  (define all-vertex-data (make-bytes buffer-size))
  (define all-vertex-data-ptr (u8vector->cpointer all-vertex-data))
  ;; Copy the vertex data into the buffer
  (for/fold ([vertex-num : Nonnegative-Fixnum  0]) ([i  (in-range start end)])
    (define v (shape-params-vertices (unsafe-vector-ref ps i)))
    (cond
      [(eq? indexed? (and (vertices-indexes v) #t))
       (define vertex-count (vertices-vertex-count v))
       (define vertex-data (vertices-vertex-data v))
       (memcpy all-vertex-data-ptr
               (unsafe-fx* vertex-num struct-size)
               (u8vector->cpointer vertex-data)
               (unsafe-fx* vertex-count struct-size)
               _byte)
       (unsafe-fx+ vertex-num vertex-count)]
      [else
       vertex-num]))
  all-vertex-data)

(: merge-indexes (-> (Vectorof shape-params)
                     Nonnegative-Fixnum
                     Nonnegative-Fixnum
                     (Vectorof Index)))
(define (merge-indexes ps start end)
  (define index-count (get-index-count ps start end))
  (define all-indexes ((inst make-vector Index) index-count))
  
  ;; Copy the index data into the buffer, shifted
  (for/fold ([vertex-num : Nonnegative-Fixnum  0]
             [index-num : Nonnegative-Fixnum 0]
             ) ([i  (in-range start end)])
    (define v (shape-params-vertices (unsafe-vector-ref ps i)))
    (define indexes (vertices-indexes v))
    (cond
      [indexes
       (define vertex-count (vertices-vertex-count v))
       (define index-count (vector-length indexes))
       (for ([j  (in-range index-count)])
         (define idx (unsafe-vector-ref indexes j))
         (vector-set! all-indexes
                      (unsafe-fx+ index-num j)
                      (assert (unsafe-fx+ vertex-num idx) index?)))
       (values (unsafe-fx+ vertex-num vertex-count)
               (unsafe-fx+ index-num index-count))]
      [else
       (values vertex-num
               index-num)]))
  all-indexes)

(: merge-vertices (-> program-spec
                      Boolean
                      (List-Hash String (U Symbol Uniform))
                      Boolean
                      Integer
                      (Vectorof shape-params)
                      Nonnegative-Fixnum
                      Nonnegative-Fixnum
                      (Listof shape-params)))
(define (merge-vertices pd indexed? uniforms two-sided? mode ps start end)
  (define vertex-count (get-vertex-count indexed? ps start end))
  (cond
    [(> vertex-count max-shape-vertex-count)
     (define mid (unsafe-fxquotient (unsafe-fx+ start end) 2))
     (when (or (= start mid) (= end mid))
       (error 'merge-vertices
              "cannot merge a single shape with more than ~a vertices; given ~a vertices"
              max-shape-vertex-count
              vertex-count))
     (append
      (merge-vertices pd indexed? uniforms two-sided? mode ps start mid)
      (merge-vertices pd indexed? uniforms two-sided? mode ps mid end))]
    [(> vertex-count 0)
     ;; Allocate enough space for all the vertex data
     (define all-vertex-data (merge-vertex-data pd indexed? ps start end vertex-count))
     (define all-indexes (if indexed? (merge-indexes ps start end) #f))
     
     (define verts (vertices (assert vertex-count index?) all-vertex-data all-indexes))
     (list (shape-params (λ () pd) uniforms two-sided? mode verts))]
    [else
     empty]))

(: make-frozen-scene-shape-passes (-> frozen-scene-shape Passes))
(define (make-frozen-scene-shape-passes a)
  (define s (frozen-scene-shape-scene a))
  
  (: transformed-passes (-> Shape affine (Listof Passes)))
  (define (transformed-passes s t)
    (map shape-passes (shape-transform s (affine-transform t))))
  
  (define ps (append* (scene-extract (scene-transform-shapes s identity-flt3)
                                     empty
                                     transformed-passes)))
  (define num-passes (apply max (map vector-length ps)))
  
  (: get-swap-params (-> Integer (Vectorof shape-params)))
  (define (get-swap-params n)
    (make-vector n empty-shape-params))
  
  (list->vector
   (for/list : (Listof (Vectorof shape-params)) ([pass  (in-range num-passes)])
     (let* ([ps  (map (λ ([p : Passes])
                        (if (< pass (vector-length p)) (vector-ref p pass) #()))
                      ps)]
            [ps  (apply vector-append ps)])
       (list->vector
        (append*
         (for*/list : (Listof (Listof shape-params))
           ([ks  (in-list (group-by-key! ps 0 (vector-length ps) shape-params-program-spec))]
            [pd  (in-value ((car ks)))]
            [s   (in-value (cdr ks))]
            [ks  (in-list (group-by-key! ps (span-start s) (span-end s) shape-params-uniforms))]
            [uniforms  (in-value (car ks))]
            [s  (in-value (cdr ks))]
            [ks  (in-list (group-by-key! ps (span-start s) (span-end s) shape-params-two-sided?))]
            [face  (in-value (car ks))]
            [s  (in-value (cdr ks))]
            [ks  (in-list (group-by-key! ps (span-start s) (span-end s) shape-params-mode))]
            [mode  (in-value (car ks))]
            [s  (in-value (cdr ks))])
           (append
            (merge-vertices pd #f uniforms face mode ps (span-start s) (span-end s))
            (merge-vertices pd #t uniforms face mode ps (span-start s) (span-end s))))))))))

;; ===================================================================================================
;; Bounding box

(: frozen-scene-shape-rect (-> frozen-scene-shape Nonempty-FlRect3))
(define (frozen-scene-shape-rect a)
  (scene-rect (frozen-scene-shape-scene a)))

;; ===================================================================================================
;; Transform

(: frozen-scene-shape-transform (-> frozen-scene-shape FlAffine3- (Listof Shape)))
(define (frozen-scene-shape-transform a t)
  (append*
   (scene-extract (scene-transform-shapes (frozen-scene-shape-scene a) t)
                  empty
                  (λ ([a : Shape] [t : affine])
                    (shape-transform a (affine-transform t))))))
