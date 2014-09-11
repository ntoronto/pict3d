#lang typed/racket/base

(require racket/flonum
         racket/match
         racket/list
         racket/fixnum
         "../math/flv3.rkt"
         "../math/flt3.rkt"
         "../math/flaabb3.rkt"
         "../types.rkt"
         "../utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Types

(struct flscene3 () #:transparent)

(struct Empty-FlScene3 flscene3 () #:transparent)

(struct (A) nonempty-flscene3 flscene3
  ([aabb : FlAABB3]
   [count : Positive-Fixnum])
  #:transparent)

(struct (A) flscene3-leaf nonempty-flscene3
  ([shape : A])
  #:transparent)

(struct (A) flscene3-node nonempty-flscene3
  ([neg : (Nonempty-FlScene3 A)]
   [pos : (Nonempty-FlScene3 A)])
  #:transparent)

(struct (A) flscene3-tran nonempty-flscene3
  ([transform : FlAffine3-]
   [inverse : FlAffine3-]
   [scene : (Nonempty-FlScene3 A)])
  #:transparent)

(define-type (Nonempty-FlScene3 A)
  (U (flscene3-leaf A)
     (flscene3-node A)
     (flscene3-tran A)))

(define-type (FlScene3 A) (U Empty-FlScene3 (Nonempty-FlScene3 A)))

(define empty-flscene3 (Empty-FlScene3))
(define empty-flscene3? Empty-FlScene3?)
(define flscene3-aabb nonempty-flscene3-aabb)

(: flscene3-count (All (A) (-> (FlScene3 A) Nonnegative-Fixnum)))
(define (flscene3-count s)
  (if (empty-flscene3? s) 0 (nonempty-flscene3-count s)))

;; ===================================================================================================
;; Scene constructors

(: make-nonempty-flscene3-node (All (A) (-> (Nonempty-FlScene3 A)
                                            (Nonempty-FlScene3 A)
                                            (Nonempty-FlScene3 A))))
(define (make-nonempty-flscene3-node s1 s2)
  (define c (fx+ (nonempty-flscene3-count s1) (nonempty-flscene3-count s2)))
  (define b (flaabb3-join (flscene3-aabb s1) (flscene3-aabb s2)))
  (flscene3-node b c s1 s2))

(: make-flscene3-node (All (A) (-> (FlScene3 A) (FlScene3 A) (FlScene3 A))))
(define (make-flscene3-node s1 s2)
  (cond [(empty-flscene3? s1)  s2]
        [(empty-flscene3? s2)  s1]
        [else  (make-nonempty-flscene3-node s1 s2)]))

(: make-nonempty-flscene3-tran (All (A) (-> FlAffine3- FlAffine3- (Nonempty-FlScene3 A)
                                            (Nonempty-FlScene3 A))))
(define (make-nonempty-flscene3-tran t tinv s)
  (if (flscene3-tran? s)
      (make-nonempty-flscene3-tran (flt3compose t (flscene3-tran-transform s))
                                   (flt3compose (flscene3-tran-inverse s) tinv)
                                   (flscene3-tran-scene s))
      (flscene3-tran (flaabb3-transform (flscene3-aabb s) t)
                     (nonempty-flscene3-count s)
                     t tinv s)))

(: flscene3-transform (All (A) (-> (FlScene3 A) FlAffine3- FlAffine3- (FlScene3 A))))
(define (flscene3-transform s t tinv)
  (if (empty-flscene3? s) s (make-nonempty-flscene3-tran t tinv s)))

;; ===================================================================================================
;; Scene union with rebalancing

(: nonempty-flscene3-union (All (A) (-> (Nonempty-FlScene3 A)
                                        (Nonempty-FlScene3 A)
                                        (Nonempty-FlScene3 A))))
(define (nonempty-flscene3-union s1 s2)
  (define s (make-nonempty-flscene3-node s1 s2))
  (define c (nonempty-flscene3-count s))
  (cond [(<= c 4)  s]
        [(> (integer-length c)
            (integer-length (max (nonempty-flscene3-count s1)
                                 (nonempty-flscene3-count s2))))
         (flscene3-rebalance s)]
        [else
         (flscene3-rebalance s 2)]))

(: flscene3-union (All (A) (-> (FlScene3 A) (FlScene3 A) (FlScene3 A))))
(define (flscene3-union s1 s2)
  (cond [(empty-flscene3? s1)  s2]
        [(empty-flscene3? s2)  s1]
        [else  (nonempty-flscene3-union s1 s2)]))

(: flscene3-union* (All (A) (-> (Listof (FlScene3 A)) (FlScene3 A))))
(define (flscene3-union* ss)
  (cond
    [(empty? ss)  empty-flscene3]
    [else
     (: s (FlScene3 A))
     (define s
       (let loop ([ss : (Pair (FlScene3 A) (Listof (FlScene3 A)))  ss])
         (define n/2 (fxquotient (length ss) 2))
         (define ss1 (take ss n/2))
         (define ss2 (drop ss n/2))
         (cond [(empty? ss1)  (first ss2)]
               [(empty? ss2)  (first ss1)]  ; can't happen
               [else
                (define s (make-flscene3-node (loop ss1) (loop ss2)))
                (if (or (empty-flscene3? s)
                        (<= (nonempty-flscene3-count s) 4))
                    s
                    (flscene3-rebalance s 2))])))
     
     (define c (flscene3-count s))
     (cond [(or (empty-flscene3? s) (<= c 4))  s]
           [(> (integer-length c)
               (integer-length (for/fold ([mx : Nonnegative-Fixnum  0]) ([s  (in-list ss)])
                                 (max mx (flscene3-count s)))))
            (flscene3-rebalance s)]
           [else  s])]))

;; ===================================================================================================
;; Rebalance

(define max-index 268435455)

(: flscene3-rebalance-split (All (A) (-> (Nonempty-FlScene3 A) Index Flonum Index
                                         (Values (FlScene3 A)
                                                 (FlScene3 A)))))
(define (flscene3-rebalance-split s i x d)
  (let loop ([s s] [d d])
    (cond
      [(or (= d 0) (flscene3-leaf? s) (flscene3-tran? s))
       (define b (flscene3-aabb s))
       (define xmin (flvector-ref (flaabb3-min b) i))
       (define xmax (flvector-ref (flaabb3-max b) i))
       (if (< (- x xmin) (- xmax x))
           (values s empty-flscene3)
           (values empty-flscene3 s))]
      [(flscene3-node? s)
       (define-values (s11 s12) (loop (flscene3-node-neg s) (- d 1)))
       (define-values (s21 s22) (loop (flscene3-node-pos s) (- d 1)))
       (values (make-flscene3-node s11 s21)
               (make-flscene3-node s12 s22))])))

(: flscene3-rebalance-children (All (A) (-> (Nonempty-FlScene3 A) Index (Nonempty-FlScene3 A))))
(define (flscene3-rebalance-children s d)
  (match s
    [(? flscene3-leaf? s)  s]
    [(? flscene3-tran? s)  s]
    [(flscene3-node b c s1 s2)
     (flscene3-node b c
                    (flscene3-rebalance s1 d)
                    (flscene3-rebalance s2 d))]))

(: flscene3-rebalance (All (A) (->* [(Nonempty-FlScene3 A)] [Index] (Nonempty-FlScene3 A))))
(define (flscene3-rebalance s [d max-index])
  (cond
    [(or (= d 0) (flscene3-leaf? s) (flscene3-tran? s))  s]
    [(flscene3-node? s)
     (match-define (flscene3-node b c s1 s2) s)
     (define-values (i x) (flaabb3-longest-axis/center b))
     (define-values (s11 s12) (flscene3-rebalance-split s1 i x d))
     (define-values (s21 s22) (flscene3-rebalance-split s2 i x d))
     (cond
       [(and s11 (not s12) (not s21) s22)  s]
       [(and (not s11) s12 s21 (not s22))  s]
       [else
        (let ([s1  (make-flscene3-node s11 s21)]
              [s2  (make-flscene3-node s12 s22)])
          (cond [(empty-flscene3? s1)  (flscene3-rebalance-children (assert s2 nonempty-flscene3?)
                                                                    (- d 1))]
                [(empty-flscene3? s2)  (flscene3-rebalance-children s1 (- d 1))]
                [else  (flscene3-node b c
                                      (flscene3-rebalance s1 (- d 1))
                                      (flscene3-rebalance s2 (- d 1)))]))])]))

;; ===================================================================================================

(: flscene3-extract (All (A B T) (-> (FlScene3 A) T (-> T FlAffine3- T) (-> A T B) (Listof B))))
(define (flscene3-extract s t-id t-compose f)
  (let loop ([s s] [t : T  t-id])
    (match s
      [(? empty-flscene3? s)  empty]
      [(flscene3-leaf b c a)
       (list (f a t))]
      [(flscene3-node b c s1 s2)
       (append (loop s1 t) (loop s2 t))]
      [(flscene3-tran b c t0 tinv0 s0)
       (loop s0 (t-compose t t0))])))

(: flscene3-filter (All (A) (-> (FlScene3 A) (-> A Boolean) (FlScene3 A))))
(define (flscene3-filter s p?)
  (let loop ([s s])
    (match s
      [(? empty-flscene3? s)  s]
      [(flscene3-leaf b c a)
       (if (p? a) s empty-flscene3)]
      [(flscene3-node b c s1 s2)
       (define new-s1 (loop s1))
       (define new-s2 (loop s2))
       (cond [(and (eq? new-s1 s1) (eq? new-s2 s2))  s]
             [(empty-flscene3? new-s1)  new-s2]
             [(empty-flscene3? new-s2)  new-s1]
             [else  (make-nonempty-flscene3-node new-s1 new-s2)])]
      [(flscene3-tran b c t0 tinv0 s0)
       (define new-s0 (loop s0))
       (cond [(eq? new-s0 s0)  s]
             [(empty-flscene3? new-s0)  new-s0]
             [else  (make-nonempty-flscene3-tran t0 tinv0 new-s0)])])))

;; ===================================================================================================
;; Operations parameterized on particular shape operations

(define make-flscene3-ops
  (plambda: (A) ([shape-aabb : (-> A FlAABB3)]
                 [shape-transform : (-> A FlAffine3- FlAffine3- (Listof A))])
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Convert shape to scene
    
    (: shape->flscene3 (-> A (Nonempty-FlScene3 A)))
    (define (shape->flscene3 a)
      (flscene3-leaf (shape-aabb a) 1 a))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Transform a scene
    
    (: nonempty-transform-shapes (-> (Nonempty-FlScene3 A) FlAffine3- FlAffine3- (FlScene3 A)))
    (define (nonempty-transform-shapes s t tinv)
      (match s
        [(flscene3-leaf b c a)
         (flscene3-union* (map shape->flscene3 (shape-transform a t tinv)))]
        [(flscene3-node b c s1 s2)
         (make-flscene3-node (nonempty-transform-shapes s1 t tinv)
                             (nonempty-transform-shapes s2 t tinv))]
        [(flscene3-tran b c t0 tinv0 s0)
         (nonempty-transform-shapes s0 (flt3compose t t0) (flt3compose tinv0 tinv))]))
      
    
    (: flscene3-transform-shapes (-> (FlScene3 A) FlAffine3- FlAffine3- (FlScene3 A)))
    (define (flscene3-transform-shapes s t tinv)
      (if (empty-flscene3? s) s (nonempty-transform-shapes s t tinv)))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Simple collision detection
    
    (: flscene3-trace (-> (FlScene3 A) FlVector FlVector (Values (U FlVector #f)
                                                                 (U FlPlane3 #f))))
    (define (flscene3-trace s v1 v2)
      (error 'unimplemented))
    
    ;; -----------------------------------------------------------------------------------------------
    
    (values shape->flscene3
            flscene3-transform-shapes)))
