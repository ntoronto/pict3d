#lang typed/racket/base
#|
(require racket/flonum
         racket/match
         racket/list
         racket/fixnum
         "../math/flv3.rkt"
         "../math/flaabb3.rkt"
         "../types.rkt"
         "../utils.rkt")

(provide (all-defined-out))

(: flaabb3-longest-axis (-> FlAABB3 (Values Index Flonum)))
(define (flaabb3-longest-axis b)
  (define-values (xmin ymin zmin xmax ymax zmax) (flaabb3-values b))
  (define xsize (- xmax xmin))
  (define ysize (- ymax ymin))
  (define zsize (- zmax zmin))
  (cond [(>= xsize (max ysize zsize))  (values 0 (* 0.5 (+ xmin xmax)))]
        [(>= ysize (max xsize zsize))  (values 1 (* 0.5 (+ ymin ymax)))]
        [else                          (values 2 (* 0.5 (+ zmin zmax)))]))

;; ===================================================================================================
;; Types

(define-type (Splitting-Planes A) (U (Listof FlPlane3)
                                     (-> (Pair (Splitting-Planes A)
                                               (Splitting-Planes A)))))

(struct (A) flscene3 ([aabb : FlAABB3] [count : Nonnegative-Fixnum]) #:transparent)
(struct (A) flscene3-leaf flscene3 ([shape : A]) #:transparent)
(struct (A) flscene3-node flscene3 ([neg : (FlScene3 A)]
                                    [pos : (FlScene3 A)]) #:transparent)

(define-type (FlScene3 A) (U (flscene3-leaf A)
                             (flscene3-node A)))

(struct (A) flbsp3-leaf ([shape : (Listof A)]) #:transparent)
(struct (A) flbsp3-node ([plane : FlPlane3]
                         [neg  : (U (FlBSP3 A) #f)]
                         [zero : (U (FlBSP3 A) #f)]
                         [pos  : (U (FlBSP3 A) #f)])
  #:transparent)

(define-type (FlBSP3 A) (U (flbsp3-leaf A)
                           (flbsp3-node A)))

;; ===================================================================================================
;; Scene constructors

(: make-flscene3-node (All (A) (-> (FlScene3 A) (FlScene3 A) (FlScene3 A))))
(define (make-flscene3-node s1 s2)
  (define b (flaabb3-join (flscene3-aabb s1) (flscene3-aabb s2)))
  (define c (fx+ (flscene3-count s1) (flscene3-count s2)))
  (flscene3-node b c s1 s2))

(: maybe-make-flscene3-node (All (A) (-> (U (FlScene3 A) #f)
                                         (U (FlScene3 A) #f)
                                         (U (FlScene3 A) #f))))
(define (maybe-make-flscene3-node s1 s2)
  (cond [(and s1 s2)  (make-flscene3-node s1 s2)]
        [s1  s1]
        [s2  s2]
        [else  #f]))

(: two-true? (-> Any Any Any Any))
(define (two-true? s- s0 s+)
  (or (and s- s+)
      (and s- s0)
      (and s+ s0)))

(: maybe-make-flbsp3-node (All (A) (-> FlPlane3
                                       (U (FlBSP3 A) #f)
                                       (U (FlBSP3 A) #f)
                                       (U (FlBSP3 A) #f)
                                       (U (FlBSP3 A) #f))))
(define (maybe-make-flbsp3-node p0 s- s0 s+)
  (and (or s- s0 s+) (flbsp3-node p0 s- s0 s+)))

(: make-flbsp3-node (All (A) (-> FlPlane3
                                 (U (FlBSP3 A) #f)
                                 (U (FlBSP3 A) #f)
                                 (U (FlBSP3 A) #f)
                                 (FlBSP3 A))))
(define (make-flbsp3-node p0 s- s0 s+)
  (define s (maybe-make-flbsp3-node p0 s- s0 s+))
  (if s s (error 'make-flbsp3-node "expected one non-#f scene; given ~e ~e ~e" s- s0 s+)))

(: flbsp3-count (All (A) (-> (FlBSP3 A) Nonnegative-Fixnum)))
(define (flbsp3-count s)
  (match s
    [(flbsp3-leaf as)  (length as)]
    [(flbsp3-node p0 s- s0 s+)
     (fx+ (if s0 (flbsp3-count s0) 0)
          (fx+ (if s- (flbsp3-count s-) 0)
               (if s+ (flbsp3-count s+) 0)))]))

;; ===================================================================================================
;; Scene union with rebalancing

(: flscene3-union (All (A) (-> (FlScene3 A) (FlScene3 A) (FlScene3 A))))
(define (flscene3-union s1 s2)
  (define s (make-flscene3-node s1 s2))
  (define c (flscene3-count s))
  (cond [(<= c 4)  s]
        [(> (integer-length c)
            (integer-length (max (flscene3-count s1)
                                 (flscene3-count s2))))
         (flscene3-rebalance s)]
        [else
         (flscene3-rebalance s 2)]))

(: flscene3-union* (All (A) (-> (Listof (FlScene3 A)) (U (FlScene3 A) #f))))
(define (flscene3-union* ss)
  (cond
    [(empty? ss)  #f]
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
                (define c (flscene3-count s))
                (if (<= c 4) s (flscene3-rebalance s 2))])))
     
     (define c (flscene3-count s))
     (cond [(<= c 4)  s]
           [(> (integer-length c)
               (integer-length (for/fold ([mx : Nonnegative-Fixnum  0]) ([s  (in-list ss)])
                                 (max mx (flscene3-count s)))))
            (flscene3-rebalance s)]
           [else  s])]))

;; ===================================================================================================
;; Extract all scene shapes

(: flscene3-shapes (All (A) (-> (FlScene3 A) (Listof A))))
(define (flscene3-shapes s)
  (match s
    [(flscene3-leaf b c a)
     (list a)]
    [(flscene3-node b c s1 s2)
     (append (flscene3-shapes s1)
             (flscene3-shapes s2))]))

;; ===================================================================================================
;; Filter a scene using a predicate

(: flscene3-filter (All (A) (-> (FlScene3 A) (-> A Boolean) (U (FlScene3 A) #f))))
;; If new-s is (flscene3-filter pred? s), then (equal? s new-s) if and only if (eq? s new-s)
(define (flscene3-filter s pred?)
  (let loop ([s s])
    (match s
      [(flscene3-leaf b c a)
       (if (pred? a) s #f)]
      [(flscene3-node b c s1 s2)
       (define new-s1 (loop s1))
       (define new-s2 (loop s2))
       (cond [(and (eq? new-s1 s1) (eq? new-s2 s2))  s]
             [else  (maybe-make-flscene3-node new-s1 new-s2)])])))

;; ===================================================================================================
;; Rebalance

(define max-index 268435455)

(: flscene3-rebalance-split (All (A) (-> (FlScene3 A) Index Flonum Index
                                         (Values (U (FlScene3 A) #f)
                                                 (U (FlScene3 A) #f)))))
(define (flscene3-rebalance-split s i x d)
  (let loop ([s s] [d d])
    (cond [(or (= d 0) (flscene3-leaf? s))
           (define b (flscene3-aabb s))
           (define xmin (flvector-ref (flaabb3-min b) i))
           (define xmax (flvector-ref (flaabb3-max b) i))
           (if (< (- x xmin) (- xmax x))
               (values s #f)
               (values #f s))]
          [else
           (define-values (s11 s12) (loop (flscene3-node-neg s) (- d 1)))
           (define-values (s21 s22) (loop (flscene3-node-pos s) (- d 1)))
           (values (maybe-make-flscene3-node s11 s21)
                   (maybe-make-flscene3-node s12 s22))])))

(: flscene3-rebalance-children (All (A) (-> (FlScene3 A) Index (FlScene3 A))))
(define (flscene3-rebalance-children s d)
  (match s
    [(? flscene3-leaf? s)  s]
    [(flscene3-node b c s1 s2)
     (flscene3-node b c
                    (flscene3-rebalance s1 d)
                    (flscene3-rebalance s2 d))]))

(: flscene3-rebalance (All (A) (->* [(FlScene3 A)] [Index] (FlScene3 A))))
(define (flscene3-rebalance s [d max-index])
  (cond
    [(or (= d 0) (flscene3-leaf? s))  s]
    [else
     (match-define (flscene3-node b c s1 s2) s)
     (define-values (i x) (flaabb3-longest-axis b))
     (define-values (s11 s12) (flscene3-rebalance-split s1 i x d))
     (define-values (s21 s22) (flscene3-rebalance-split s2 i x d))
     (cond
       [(and s11 (not s12) (not s21) s22)  s]
       [(and (not s11) s12 s21 (not s22))  s]
       [else
        (let ([s1  (maybe-make-flscene3-node s11 s21)]
              [s2  (maybe-make-flscene3-node s12 s22)])
          (cond [(and s1 s2)  (flscene3-node b c
                                             (flscene3-rebalance s1 (- d 1))
                                             (flscene3-rebalance s2 (- d 1)))]
                [(or s1 s2) => (λ (s) (flscene3-rebalance-children s (- d 1)))]
                [else  (error 'flscene3-rebalance "internal error: empty scene")]))])]))

;; ===================================================================================================
;; Extract BSP shapes

(: flbsp3-shapes (All (A) (-> (FlBSP3 A) (Listof A))))
(define (flbsp3-shapes s)
  (let loop ([s s])
    (match s
      [(flbsp3-leaf as)  as]
      [(flbsp3-node p0 s- s0 s+)
       (define as- (if s- (loop s-) empty))
       (define as0 (if s0 (loop s0) empty))
       (define as+ (if s+ (loop s+) empty))
       (append as- as0 as+)])))

(: flbsp3-sorted-shapes/pos (All (A) (-> (FlBSP3 A) FlVector (Listof A))))
(define (flbsp3-sorted-shapes/pos s pos)
  (let loop ([s s])
    (match s
      [(flbsp3-leaf as)  as]
      [(flbsp3-node p0 s- s0 s+)
       (define as- (if s- (loop s-) empty))
       (define as0 (if s0 (loop s0) empty))
       (define as+ (if s+ (loop s+) empty))
       (define d (flplane3-point-dist p0 pos))
       (cond [(< d 0.0)
              ;; On the negative side of the plane; return the positive side first
              (append as+ as0 as-)]
             [(> d 0.0)
              ;; On the positive side of the plane: return the negative side first
              (append as- as0 as+)]
             [else
              ;; On the plane: doesn't matter which side is drawn first; draw nothing on the plane
              (append as- as+)])])))

(: flbsp3-sorted-shapes/dir (All (A) (-> (FlBSP3 A) FlVector (Listof A))))
(define (flbsp3-sorted-shapes/dir s dir)
  (let loop ([s s])
    (match s
      [(flbsp3-leaf as)  as]
      [(flbsp3-node p0 s- s0 s+)
       (define as- (if s- (loop s-) empty))
       (define as0 (if s0 (loop s0) empty))
       (define as+ (if s+ (loop s+) empty))
       (define d (flv3dot (flplane3-normal p0) dir))
       (cond [(< d 0.0)
              ;; Plane normal points opposite the direction of view: return the negative side first
              (append as- as0 as+)]
             [(> d 0.0)
              ;; It points in the same direction: return the positive-side shapes first
              (append as+ as0 as-)]
             [else
              ;; Direction is orthogonal to the normal (i.e. along the plane); doesn't matter what
              ;; order the shapes are drawn, but don't draw the shapes in the plane
              (append as- as+)])])))

;; ===================================================================================================
;; Operations parameterized on particular shape operations

(: bsp-depth (Parameterof Integer))
(define bsp-depth (make-parameter 50))

(define make-flscene3-ops
  (plambda: (A) ([shape-aabb : (-> A FlAABB3)]
                 [shape-clip : (-> A FlPlane3 Plane-Sides (Listof A))]
                 [shape-split : (-> A FlPlane3 (Values (Listof A)
                                                       (Listof A)
                                                       (Listof A)))]
                 [shape-splitting-planes : (-> A (Listof FlPlane3))])
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Convert shape to scene
    
    (: shape->flscene3 (-> A (FlScene3 A)))
    (define (shape->flscene3 a)
      (flscene3-leaf (shape-aabb a) 1 a))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Clip scene against a plane
    
    (: flscene3-clip (-> (FlScene3 A) FlPlane3 Plane-Sides (U (FlScene3 A) #f)))
    (define (flscene3-clip s p keep)
      (define b (flscene3-aabb s))
      (define side (flaabb3-plane-side b p))
      (cond
        [(eq? side keep)  s]
        [(eq? side 'both)
         (match s
           [(flscene3-leaf b c a)
            (let ([as  (shape-clip a p keep)])
              (flscene3-union* (map shape->flscene3 as)))]
           [(flscene3-node b c s1 s2)
            (let ([s1  (flscene3-clip s1 p keep)]
                  [s2  (flscene3-clip s2 p keep)])
              (maybe-make-flscene3-node s1 s2))])]
        [else  #f]))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Split a scene with a plane
    
    (: flscene3-split (-> (FlScene3 A) FlPlane3 (Values (U (FlScene3 A) #f)
                                                        (U (FlScene3 A) #f)
                                                        (U (FlScene3 A) #f))))
    (define (flscene3-split s p)
      (define b (flscene3-aabb s))
      (case (flaabb3-plane-side b p)
        [(neg)  (values s #f #f)]
        [(pos)  (values #f #f s)]
        [else
         (match s
           [(flscene3-leaf b c a)
            (define-values (as- as0 as+) (shape-split a p))
            (values (flscene3-union* (map shape->flscene3 as-))
                    (flscene3-union* (map shape->flscene3 as0))
                    (flscene3-union* (map shape->flscene3 as+)))]
           [(flscene3-node b c s1 s2)
            (define-values (s1- s10 s1+) (flscene3-split s1 p))
            (define-values (s2- s20 s2+) (flscene3-split s2 p))
            (values (maybe-make-flscene3-node s1- s2-)  ; neg side of p
                    (maybe-make-flscene3-node s10 s20)  ; on p
                    (maybe-make-flscene3-node s1+ s2+)  ; pos side of p
                    )])]))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Collect splitting planes for BSP
    
    (: flscene3-splitting-planes (-> (FlScene3 A) (Splitting-Planes A)))
    (define (flscene3-splitting-planes s)
      (define v (flaabb3-center (flscene3-aabb s)))
      (let loop ([s s])
        (match s
          [(flscene3-leaf b c a)
           (shape-splitting-planes a)]
          [(flscene3-node b c s1 s2)
           (define b1 (flscene3-aabb s1))
           (define b2 (flscene3-aabb s2))
           (define p (flaabb3-separating-plane b1 b2))
           (cond [p  (list p)]
                 [else  (λ ()
                          (define v1 (flaabb3-center b1))
                          (define v2 (flaabb3-center b2))
                          (define ps1 (loop s1))
                          (define ps2 (loop s2))
                          (if (< (flv3dist v v1) (flv3dist v v2))
                              (cons ps1 ps2)
                              (cons ps2 ps1)))])])))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; BSP build
    
    (: flat-bsp (-> (FlScene3 A) (FlBSP3 A)))
    (define (flat-bsp s)
      (flbsp3-leaf (flscene3-shapes s)))
    
    (: try-splits/planes (-> (FlScene3 A)
                             (Listof FlPlane3)
                             (Listof FlPlane3)
                             (-> (FlBSP3 A))
                             (FlBSP3 A)))
    (define (try-splits/planes s used-ps ps fail)
      (let loop ([ps  (remove* used-ps ps)])
        (cond [(empty? ps)  (fail)]
              [else
               (define p (first ps))
               (define-values (s- s0 s+) (flscene3-split s p))
               (cond [(or (two-true? s- s0 s+) s0)
                      (make-flbsp3-node p
                                        (and s- (flscene3-bsp* s- used-ps))
                                        (and s0 (flat-bsp s0))
                                        (and s+ (flscene3-bsp* s+ used-ps)))]
                     [else  (loop (rest ps))])])))
    
    (: try-splits/branch (-> (FlScene3 A)
                             (Listof FlPlane3)
                             (Pair (Splitting-Planes A)
                                   (Splitting-Planes A))
                             (-> (FlBSP3 A))
                             (FlBSP3 A)))
    (define (try-splits/branch s used-ps ps fail)
      (match-define (cons ps1 ps2) ps)
      (try-splits s used-ps ps1 (λ () (try-splits s used-ps ps2 fail))))
    
    (: try-splits (-> (FlScene3 A)
                      (Listof FlPlane3)
                      (Splitting-Planes A)
                      (-> (FlBSP3 A))
                      (FlBSP3 A)))
    (define (try-splits s used-ps ps fail)
      (if (or (empty? ps) (pair? ps))
          (try-splits/planes s used-ps ps fail)
          (try-splits/branch s used-ps (ps) fail)))
    
    (: build-bsp (-> (FlScene3 A)
                     (Listof FlPlane3)
                     (-> (FlBSP3 A))
                     (FlBSP3 A)))
    (define (build-bsp s used-ps fail)
      (if (flscene3-leaf? s)
          (flbsp3-leaf (list (flscene3-leaf-shape s)))
          (try-splits s used-ps (flscene3-splitting-planes s) fail)))
    
    (: flscene3-bsp* (-> (FlScene3 A)
                         (Listof FlPlane3)
                         (FlBSP3 A)))
    (define (flscene3-bsp* s used-ps)
      ;(printf "bsp-depth = ~v~n" (bsp-depth))
      (cond
        [(<= (bsp-depth) 0)  (flat-bsp s)]
        [else
         (parameterize ([bsp-depth  (- (bsp-depth) 1)])
           (build-bsp s used-ps (λ () (flat-bsp s))))]))
    
    (: flscene3-bsp (-> (FlScene3 A) (FlBSP3 A)))
    (define (flscene3-bsp s)
      (flscene3-bsp* s empty))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Simple collision detection
    
    (: flscene3-trace (-> (FlScene3 A) FlVector FlVector (Values (U FlVector #f)
                                                                 (U FlPlane3 #f))))
    (define (flscene3-trace s v1 v2)
      (error 'unimplemented))
    
    ;; -----------------------------------------------------------------------------------------------
    
    (values shape->flscene3
            flscene3-clip
            flscene3-split
            flscene3-bsp)))

;; ===================================================================================================
;; Tests
#|
(require math/flonum)

(: flv3-aabb (-> FlVector FlAABB3))
(define (flv3-aabb v)
  (assert (flv3aabb (vector v)) values))

(: flv3-clip (-> FlVector FlPlane3 Plane-Sides (Listof FlVector)))
(define (flv3-clip t p side)
  (define-values (ts- ts0 ts+) (flv3-split t p))
  (case side
    [(neg)  ts-]
    [(pos)  ts+]
    [(negzero)  (append ts0 ts-)]
    [(poszero)  (append ts0 ts+)]
    [(zero)  ts0]
    [(nonzero)  (append ts- ts+)]))

(: flv3-split (-> FlVector FlPlane3 (Values (Listof FlVector) (Listof FlVector) (Listof FlVector))))
(define (flv3-split v p)
  (define ds (flplane3-relative-dists p (vector v)))
  (cond [(not ds)  (values empty empty empty)]
        [else
         (define d (flvector-ref ds 0))
         (cond [(< d 0.0)  (values (list v) empty empty)]
               [(> d 0.0)  (values empty empty (list v))]
               [else       (values empty (list v) empty)])]))

(: flv3-splitting-planes (-> FlVector (Listof FlPlane3)))
(define (flv3-splitting-planes v)
  empty)

(define-values (v->flscene3
                v-flscene3-clip
                v-flscene3-split
                v-flscene3-bsp)
  ((inst make-flscene3-ops FlVector)
   flv3-aabb
   flv3-clip
   flv3-split
   flv3-splitting-planes))

(for ([n  (in-range 0 10000 1000)])
  (time (for ([_  (in-range 5)])
          (flscene3-union*
           (for/list : (Listof (FlScene3 FlVector)) ([x  (in-range (+ n 1))])
             (v->flscene3 (flvector (fl x) 0.0 0.0)))))))
|#
|#
