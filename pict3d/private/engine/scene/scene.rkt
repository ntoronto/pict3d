#lang typed/racket/base

#|
Scenes: tree-structured databases that
 * Approximately self-balance
 * Store shapes in the leaves
 * Store bounding information in the nodes

Scenes should allow O(log(n))
 * Approximate plane culling (i.e. for view and shadow frustum culling)
 * Collision queries (i.e. point, sphere, or box tracing)
 * Touching queries and other spatial searches
|#

(require racket/unsafe/ops
         racket/list
         racket/fixnum
         "../../math.rkt"
         "../../utils.rkt"
         "tags.rkt"
         "types.rkt")

(provide (all-defined-out))

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
         (define a (shape-fast-transform s t))
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

(: nonempty-scene-union/rebalance (-> Nonempty-Scene Nonempty-Scene node-scene))
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

(: node-scene-insert (-> node-scene Nonempty-Scene node-scene))
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

(: nonempty-scene-union (-> Nonempty-Scene Nonempty-Scene node-scene))
;; Compute the nonempty union of two nonempty scenes
(define (nonempty-scene-union s1 s2)
  (define bv1 (scene-visible-bbox s1))
  (define bi1 (scene-invisible-bbox s1))
  (define bv2 (scene-visible-bbox s2))
  (define bi2 (scene-invisible-bbox s2))
  (let ([bv1  (if (and bv1 (> (bbox-badness bv1) 16.0)) (scene-visible-bbox/badness s1 16.0) bv1)]
        [bi1  (if (and bi1 (> (bbox-badness bi1) 16.0)) (scene-invisible-bbox/badness s1 16.0) bi1)]
        [bv2  (if (and bv2 (> (bbox-badness bv2) 16.0)) (scene-visible-bbox/badness s2 16.0) bv2)]
        [bi2  (if (and bi2 (> (bbox-badness bi2) 16.0)) (scene-invisible-bbox/badness s2 16.0) bi2)])
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
;; Computing tighter bounding boxes

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
  
  (: shape-bbox (-> shape FlAffine3 (U #f bbox)))
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
       ;; If the existing bounding box, transformed into world-space coordinates, isn't terrible...
       (cond [(< (maybe-bbox-badness b) max-badness)
              ;; ... we can just keep the node's bbox and return the transformed one
              b]
             [else
              ;; Otherwise, get a new bounding box in world-space coordinates
              (define new-b
                (maybe-bbox-join (loop (node-scene-neg s) t max-badness)
                                 (loop (node-scene-pos s) t max-badness)))
              ;; If we got one...
              (when new-b
                ;; ... transform it back into this node's coordinate space...
                (let* ([tinv   (flt3inverse t)]
                       [new-b  (and tinv (bbox-transform new-b tinv))])
                  ;; ... and if it's not terrible, and transforming it into world-space coordinates
                  ;; also isn't terrible...
                  (when (and new-b
                             (< (bbox-badness new-b) max-badness)
                             (< (bbox-badness (bbox-transform new-b t)) max-badness))
                    ;; ... then we might as well keep the awful thing
                    (set-node-scene-bbox! s new-b))))
              ;; Return the world-space bbox
              new-b])]
      [(trans-scene? s)
       (loop (trans-scene-scene s)
             (flt3compose t (trans-scene-affine s))
             max-badness)]
      [(group-scene? s)
       (define s0 (group-scene-scene s))
       (cond [(empty-scene? s0)  (bbox-transform zero-bbox t)]
             [else  (loop s0 t max-badness)])])))

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
