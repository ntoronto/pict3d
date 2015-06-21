#lang typed/racket/base

#|
TODO

Speed up face-soup-add and face-soup-combine

Speed up edge queries by storing a vertex index

? Move vtx outward
|#

(require racket/unsafe/ops
         racket/match
         racket/list
         racket/promise
         math/flonum
         "../math.rkt"
         "../utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Vertex data

(struct vtx ([position : FlV3]
             [normal : FlV3]
             [color : FlV4]
             [emitted : FlV4]
             [material : FlV4])
  #:transparent)

(: set-vtx-vecs (-> vtx FlV3 FlV3 vtx))
(define (set-vtx-vecs v p n)
  (vtx p n (vtx-color v) (vtx-emitted v) (vtx-material v)))

(: set-vtx-attributes (-> vtx FlV4 FlV4 FlV4 vtx))
(define (set-vtx-attributes v c e m)
  (vtx (vtx-position v) (vtx-normal v) c e m))

(: set-vtx-position (-> vtx FlV3 vtx))
(define (set-vtx-position v p)
  (set-vtx-vecs v p (vtx-normal v)))

(: set-vtx-normal (-> vtx FlV3 vtx))
(define (set-vtx-normal v n)
  (set-vtx-vecs v (vtx-position v) n))

(: set-vtx-color (-> vtx FlV4 vtx))
(define (set-vtx-color v c)
  (set-vtx-attributes v c (vtx-emitted v) (vtx-material v)))

(: set-vtx-emitted (-> vtx FlV4 vtx))
(define (set-vtx-emitted v e)
  (set-vtx-attributes v (vtx-color v) e (vtx-material v)))

(: set-vtx-material (-> vtx FlV4 vtx))
(define (set-vtx-material v m)
  (set-vtx-attributes v (vtx-color v) (vtx-emitted v) m))

(: vtx-flip-normal (-> vtx vtx))
(define (vtx-flip-normal v)
  (set-vtx-normal v (flv3neg (vtx-normal v))))

(define zero-vtx (vtx zero-flv3 zero-flv3 zero-flv4 zero-flv4 zero-flv4))

(: vtx-blend (-> vtx vtx Flonum vtx))
(define (vtx-blend vtx1 vtx2 α)
  (define 1-α (- 1.0 α))
  (match-define (vtx v1 n1 c1 e1 m1) vtx1)
  (match-define (vtx v2 n2 c2 e2 m2) vtx2)
  (define v (flv3+ (flv3* v1 1-α) (flv3* v2 α)))
  (define n (flv3normalize (flv3+ (flv3* n1 1-α) (flv3* n2 α))))
  (define c (flv4+ (flv4* c1 1-α) (flv4* c2 α)))
  (define e (flv4+ (flv4* e1 1-α) (flv4* e2 α)))
  (define m (flv4+ (flv4* m1 1-α) (flv4* m2 α)))
  (vtx v (if n n zero-flv3) c e m))

(: vtx-average (-> (Listof+1 vtx) vtx))
(define (vtx-average vtxs)
  (match-define (vtx p0 n0 c0 e0 m0) (first vtxs))
  (define-values (p n c e m)
    (for/fold ([p0 : FlV3  p0]
               [n0 : FlV3  n0]
               [c0 : FlV4  c0]
               [e0 : FlV4  e0]
               [m0 : FlV4  m0])
              ([v  (in-list (rest vtxs))])
      (match-define (vtx p n c e m) v)
      (values (flv3+ p0 p)
              (flv3+ n0 n)
              (flv4+ c0 c)
              (flv4+ e0 e)
              (flv4+ m0 m))))
  (define l (fl (length vtxs)))
  (let ([n  (flv3normalize n)])
    (vtx (flv3/ p l)
         (if n n zero-flv3)
         (flv4/ c l)
         (flv4/ e l)
         (flv4/ m l))))

(: vtx-interpolate (-> vtx vtx FlV3 vtx))
(define (vtx-interpolate vtx1 vtx2 v)
  (match-define (vtx v1 n1 c1 e1 m1) vtx1)
  (match-define (vtx v2 n2 c2 e2 m2) vtx2)
  (define α (let ([α  (flv3alpha v1 v2 v)])
              (if α α 0.5)))
  (define n (flv3normalize (flv3blend n1 n2 α)))
  (define c (flv4blend c1 c2 α))
  (define e (flv4blend e1 e2 α))
  (define m (flv4blend m1 m2 α))
  (vtx v (if n n zero-flv3) c e m))

(: vtx-similar? (-> vtx vtx Flonum Boolean))
(define (vtx-similar? vtx1 vtx2 eps)
  (match-define (vtx _ n1 c1 e1 m1) vtx1)
  (match-define (vtx _ n2 c2 e2 m2) vtx2)
  (and (flv3near? n1 n2 eps)
       (flv4near? c1 c2 eps)
       (flv4near? e1 e2 eps)
       (flv4near? m1 m2 eps)))

(: flt3apply/vtx (-> FlAffine3 vtx vtx))
(define (flt3apply/vtx t v)
  (match-define (vtx p n c e m) v)
  (let ([p  (flt3apply/pos t p)]
        [n  (flt3apply/norm t n)])
    (vtx p (if n n zero-flv3) c e m)))

(: make-flt3apply/vtx (-> FlAffine3 (-> vtx vtx)))
(define (make-flt3apply/vtx t)
  (define memo ((inst make-hasheq vtx vtx)))
  (λ (v) (hash-ref! memo v (λ () (flt3apply/vtx t v)))))

(: fls3apply/vtx (-> FlSmooth3 vtx (Values vtx Flonum Boolean)))
(define (fls3apply/vtx t v)
  (match-define (vtx p n c e m) v)
  (let-values ([(p n d c?)  (fls3apply/all t p n)])
    (values (vtx p (if n n zero-flv3) c e m) d c?)))

(: make-fls3apply/vtx (-> FlSmooth3 (-> vtx (Values vtx Flonum Boolean))))
(define (make-fls3apply/vtx t)
  (define memo ((inst make-hasheq vtx (Vector vtx Flonum Boolean))))
  (λ (v)
    (define vals (hash-ref! memo v (λ () (let-values ([(v d c?)  (fls3apply/vtx t v)])
                                           (vector v d c?)))))
    (values (unsafe-vector-ref vals 0)
            (unsafe-vector-ref vals 1)
            (unsafe-vector-ref vals 2))))

;; ===================================================================================================
;; Faces

(struct (A B) face ([vtx1 : vtx]
                    [vtx2 : vtx]
                    [vtx3 : vtx]
                    [data : A]
                    [edge-data12 : B]
                    [edge-data23 : B]
                    [edge-data31 : B])
  #:transparent)

;(: face-vtxs (All (A) (-> (face A) (Values vtx vtx vtx))))
(define-syntax-rule (face-vtxs f-stx)
  (let ([f f-stx])
    (values (face-vtx1 f)
            (face-vtx2 f)
            (face-vtx3 f))))

;(: face-flv3s (All (A) (-> (face A) (Values FlV3 FlV3 FlV3))))
(define-syntax-rule (face-flv3s f-stx)
  (let ([f f-stx])
    (values (vtx-position (face-vtx1 f))
            (vtx-position (face-vtx2 f))
            (vtx-position (face-vtx3 f)))))

;(: face-normals (All (A) (-> (face A) (Values FlV3 FlV3 FlV3))))
(define-syntax-rule (face-normals f-stx)
  (let ([f f-stx])
    (values (vtx-normal (face-vtx1 f))
            (vtx-normal (face-vtx2 f))
            (vtx-normal (face-vtx3 f)))))

(: face-set-first-vertex (All (A B) (-> (face A B) (U 1 2 3) (face A B))))
(define (face-set-first-vertex f i)
  (match-define (face vtx1 vtx2 vtx3 d d12 d23 d31) f)
  (case i
    [(1)   (face vtx1 vtx2 vtx3 d d12 d23 d31)]
    [(2)   (face vtx2 vtx3 vtx1 d d23 d31 d12)]
    [else  (face vtx3 vtx1 vtx2 d d31 d12 d23)]))

(: face-aligned-vertices (All (A B) (-> (face A B) (face A B) (Values vtx vtx vtx vtx vtx vtx))))
(define (face-aligned-vertices f1 f2)
  (define-values (va1 va2 va3) (face-vtxs f1))
  (define-values (vb1 vb2 vb3) (face-vtxs f2))
  (let a-loop ([va1 va1] [va2 va2] [va3 va3] [m 0])
    (cond
      [(= m 3)  (error 'face-aligned-vertices "no shared vertices")]
      [else
       (let b-loop ([vb1 vb1] [vb2 vb2] [vb3 vb3] [n 0])
         (cond [(= n 3)  (a-loop va2 va3 va1 (+ m 1))]
               [(and (flv3=? (vtx-position va1) (vtx-position vb2))
                     (flv3=? (vtx-position va2) (vtx-position vb1)))
                (values va1 va2 va3 vb1 vb2 vb3)]
               [else  (b-loop vb2 vb3 vb1 (+ n 1))]))])))

(: fix-face-normals (-> vtx vtx vtx (Values vtx vtx vtx)))
(define (fix-face-normals vtx1 vtx2 vtx3)
  (define n (flv3polygon-normal (vtx-position vtx1) (vtx-position vtx2) (vtx-position vtx3)))
  (cond [(not n)  (values vtx1 vtx2 vtx3)]
        [else  (values (if (flv3zero? (vtx-normal vtx1)) (set-vtx-normal vtx1 n) vtx1)
                       (if (flv3zero? (vtx-normal vtx2)) (set-vtx-normal vtx2 n) vtx2)
                       (if (flv3zero? (vtx-normal vtx3)) (set-vtx-normal vtx3 n) vtx3))]))

(: transform-face (All (A B) (-> (-> vtx vtx) Boolean (face A B) (face A B))))
(define (transform-face transform reverse? f)
  (match-define (face vtx1 vtx2 vtx3 data data12 data23 data31) f)
  (let ([vtx1  (transform vtx1)]
        [vtx2  (transform vtx2)]
        [vtx3  (transform vtx3)])
    (if reverse?
        (let-values ([(vtx3 vtx2 vtx1)  (fix-face-normals vtx3 vtx2 vtx1)])
          (face vtx3 vtx2 vtx1 data data23 data12 data31))
        (let-values ([(vtx1 vtx2 vtx3)  (fix-face-normals vtx1 vtx2 vtx3)])
          (face vtx1 vtx2 vtx3 data data12 data23 data31)))))

(: flt3apply/face (All (A B) (-> FlAffine3 (face A B) (face A B))))
(define (flt3apply/face t f)
  (transform-face (λ ([v : vtx]) (flt3apply/vtx t v))
                  (not (flaffine3-consistent? t))
                  f))

(: make-flt3apply/face (All (A B) (-> FlAffine3 (-> (face A B) (face A B)))))
(define (make-flt3apply/face t)
  (define transform (make-flt3apply/vtx t))
  (define reverse? (not (flaffine3-consistent? t)))
  (λ (f) (transform-face transform reverse? f)))

(: deform-face (All (A B) (-> (-> vtx (values vtx Flonum Boolean)) (face A B) (face A B))))
(define (deform-face deform f)
  (match-define (face vtx1 vtx2 vtx3 data data12 data23 data31) f)
  (let-values ([(vtx1 d1 c1?)  (deform vtx1)]
               [(vtx2 d2 c2?)  (deform vtx2)]
               [(vtx3 d3 c3?)  (deform vtx3)])
    (define (pos) (let*-values ([(vtx1)  (if c1? vtx1 (vtx-flip-normal vtx1))]
                                [(vtx2)  (if c2? vtx2 (vtx-flip-normal vtx2))]
                                [(vtx3)  (if c3? vtx3 (vtx-flip-normal vtx3))]
                                [(vtx1 vtx2 vtx3)  (fix-face-normals vtx1 vtx2 vtx3)])
                    (face vtx1 vtx2 vtx3 data data12 data23 data31)))
    (define (neg) (let*-values ([(vtx1)  (if c1? (vtx-flip-normal vtx1) vtx1)]
                                [(vtx2)  (if c2? (vtx-flip-normal vtx2) vtx2)]
                                [(vtx3)  (if c3? (vtx-flip-normal vtx3) vtx3)]
                                [(vtx1 vtx3 vtx2)  (fix-face-normals vtx1 vtx3 vtx2)])
                    (face vtx3 vtx2 vtx1 data data23 data12 data31)))
    (define mn (min d1 d2 d3))
    (define mx (max d1 d2 d3))
    (define m (max (abs mn) (abs mx)))
    (cond [(>= mn (* m -1e-8))  (pos)]
          [(<= mx (* m +1e-8))  (neg)]
          [else
           (define-values (vtx123 d123 c123?)
             (deform (vtx-average (list (face-vtx1 f) (face-vtx2 f) (face-vtx3 f)))))
           (if c123? (pos) (neg))])))

(: fls3apply/face (All (A B) (-> FlSmooth3 (face A B) (face A B))))
(define (fls3apply/face t f)
  (deform-face (λ ([v : vtx]) (fls3apply/vtx t v)) f))

(: make-fls3apply/face (All (A B) (-> FlSmooth3 (-> (face A B) (face A B)))))
(define (make-fls3apply/face t)
  (define deform (make-fls3apply/vtx t))
  (λ (f) (deform-face deform f)))

(: make-triangle-face (All (A B) (-> vtx vtx vtx A B B B Boolean (Listof (face A B)))))
(define (make-triangle-face vtx1 vtx2 vtx3 d d12 d23 d31 reverse?)
  (cond [(or (eq? vtx1 vtx2) (eq? vtx2 vtx3) (eq? vtx3 vtx1))  empty]
        [reverse?  (list (face vtx3 vtx2 vtx1 d d23 d12 d31))]
        [else      (list (face vtx1 vtx2 vtx3 d d12 d23 d31))]))

(: make-quad-faces (All (A B) (-> vtx vtx vtx vtx A B B B B B B FlAffine3 Boolean
                                  (Listof (face A B)))))
(define (make-quad-faces vtx1 vtx2 vtx3 vtx4 d d12 d23 d34 d41 d13 d24 t0 reverse?)
  (if (< (* (flv3dist (flt3apply/pos t0 (vtx-position vtx1))
                      (flt3apply/pos t0 (vtx-position vtx3)))
            (- 1.0 1e-8))
         (flv3dist (flt3apply/pos t0 (vtx-position vtx2))
                   (flt3apply/pos t0 (vtx-position vtx4))))
      (append (make-triangle-face vtx1 vtx2 vtx3 d d12 d23 d13 reverse?)
              (make-triangle-face vtx1 vtx3 vtx4 d d13 d34 d41 reverse?))
      (append (make-triangle-face vtx2 vtx4 vtx1 d d24 d41 d12 reverse?)
              (make-triangle-face vtx2 vtx3 vtx4 d d23 d34 d24 reverse?))))

;; ===================================================================================================
;; Edges

(struct (B) edge ([vtx1 : vtx]
                  [vtx2 : vtx]
                  [data : B])
  #:transparent)

(: edge-vtxs (All (B) (-> (edge B) (Values vtx vtx))))
(define (edge-vtxs e)
  (values (edge-vtx1 e)
          (edge-vtx2 e)))

(: edge-flv3s (All (B) (-> (edge B) (Values FlV3 FlV3))))
(define (edge-flv3s e)
  (define-values (vtx1 vtx2) (edge-vtxs e))
  (values (vtx-position vtx1)
          (vtx-position vtx2)))

;; ===================================================================================================
;; Vertices

(struct (C) corner ([vtx : vtx]
                    [data : C])
  #:transparent)

(: corner-flv3 (All (C) (-> (corner C) FlV3)))
(define (corner-flv3 v)
  (vtx-position (corner-vtx v)))

;; ===================================================================================================
;; Face soup

(define-type Face-Index (U 1 2 3))
(define-type (FlV3-Face-Hash A B) (HashTable FlV3 (Listof (Pair Face-Index (face A B)))))

(struct (A B) face-soup ([faces : (Listof (face A B))]
                         [size : Index]
                         [flv3-face-hash : (FlV3-Face-Hash A B)])
  #:transparent)

(: merge-face-vertices (All (A B) (-> (Listof (face A B)) Positive-Flonum (Listof (face A B)))))
(define (merge-face-vertices fs eps)
  (let ([fs  (remove-duplicates fs eq?)])
    (define h
      (flv3merge-hash*
       (for/fold ([vs : (Listof FlV3)  empty]) ([f  (in-list fs)])
         (list* (vtx-position (face-vtx1 f))
                (vtx-position (face-vtx2 f))
                (vtx-position (face-vtx3 f))
                vs))
       eps))
    
    (: fix-vtx (-> vtx vtx))
    (define (fix-vtx vtx0)
      (match-define (vtx v n c e m) vtx0)
      (vtx (hash-ref h v) n c e m))
    
    (for/fold ([fs : (Listof (face A B))  empty]) ([f  (in-list fs)])
      (match-define (face vtx1 vtx2 vtx3 data data12 data23 data31) f)
      (let ([vtx1  (fix-vtx vtx1)]
            [vtx2  (fix-vtx vtx2)]
            [vtx3  (fix-vtx vtx3)])
        (define v1 (vtx-position vtx1))
        (define v2 (vtx-position vtx2))
        (define v3 (vtx-position vtx3))
        (if (or (eq? v1 v2) (eq? v2 v3) (eq? v3 v1))
            fs
            (cons (face vtx1 vtx2 vtx3 data data12 data23 data31) fs))))))

(: make-flv3-face-hash (All (A B) (-> (Listof (face A B)) (FlV3-Face-Hash A B))))
(define (make-flv3-face-hash fs)
  (for/fold ([h : (FlV3-Face-Hash A B)  (make-immutable-hasheq)]) ([f  (in-list fs)])
    (define-values (v1 v2 v3) (face-flv3s f))
    (let* ([h  (hash-set h v1 (cons (cons (ann 1 Face-Index) f) (hash-ref h v1 (λ () empty))))]
           [h  (hash-set h v2 (cons (cons (ann 2 Face-Index) f) (hash-ref h v2 (λ () empty))))]
           [h  (hash-set h v3 (cons (cons (ann 3 Face-Index) f) (hash-ref h v3 (λ () empty))))])
      h)))

(: make-face-soup (All (A B) (-> (Listof (face A B)) (face-soup A B))))
(define (make-face-soup fs)
  (let ([fs  (merge-face-vertices fs 1e-8)])
    (face-soup fs (length fs) (make-flv3-face-hash fs))))

(: face-subsoup (All (A B) (-> (face-soup A B) (Listof (face A B)) (face-soup A B))))
(define (face-subsoup fsoup sub-fs)
  (define face-hash ((inst make-hasheq (face A B) #t)))
  (for ([f  (in-list sub-fs)]) (hash-set! face-hash f #t))
  
  (let ([new-fs  (filter (λ ([f : (face A B)]) (hash-ref face-hash f #f))
                         (face-soup-faces fsoup))])
    (face-soup new-fs (length new-fs) (make-flv3-face-hash new-fs))))

(: face-soup-add (All (A B) (-> (face-soup A B) (Listof (face A B)) (face-soup A B))))
(define (face-soup-add faces fs)
  (cond [(empty? fs)  faces]
        [else (make-face-soup (append (face-soup-faces faces) fs))]))

(: face-soup-combine (All (A B) (-> (face-soup A B) (face-soup A B) (face-soup A B))))
(define (face-soup-combine faces1 faces2)
  (cond [(= 0 (face-soup-size faces2))  faces1]
        [(= 0 (face-soup-size faces1))  faces2]
        [else  (make-face-soup (append (face-soup-faces faces1)
                                       (face-soup-faces faces2)))]))

(: face-soup-flv3s (All (A B) (-> (face-soup A B) (Listof FlV3))))
(define (face-soup-flv3s faces)
  (hash-keys (face-soup-flv3-face-hash faces)))

;; ===================================================================================================
;; Edge soup

(define-type (FlV3-Edge-Hash B) (HashTable FlV3 (Listof (edge B))))

(struct (B) edge-soup ([edges : (Listof (edge B))]
                       [size : Index]
                       [lazy-flv3-edge-hash : (Promise (FlV3-Edge-Hash B))])
  #:transparent)

(: make-flv3-edge-hash (All (B) (-> (Listof (edge B)) (FlV3-Edge-Hash B))))
(define (make-flv3-edge-hash edges)
  (for/fold ([h : (FlV3-Edge-Hash B)  (make-immutable-hash)]) ([e  (in-list edges)])
    (define v1 (vtx-position (edge-vtx1 e)))
    (define v2 (vtx-position (edge-vtx2 e)))
    (let* ([h  (hash-set h v1 (cons e (hash-ref h v1 (λ () empty))))]
           [h  (hash-set h v2 (cons e (hash-ref h v2 (λ () empty))))])
      h)))

(: make-edge-soup (All (B) (-> (Listof (edge B)) (edge-soup B))))
(define (make-edge-soup edges)
  (let ([edges  (remove-duplicates edges eq?)])
    (edge-soup edges
               (length edges)
               (delay (make-flv3-edge-hash edges)))))

(: edge-soup-flv3-edge-hash (All (B) (-> (edge-soup B) (FlV3-Edge-Hash B))))
(define (edge-soup-flv3-edge-hash edges)
  (force (edge-soup-lazy-flv3-edge-hash edges)))

(: edge-soup-union (All (B) (-> (edge-soup B) (edge-soup B) (edge-soup B))))
(define (edge-soup-union edges1 edges2)
  (make-edge-soup (append (edge-soup-edges edges1)
                          (edge-soup-edges edges2))))

;; ===================================================================================================
;; Vertex soup

(define-type (FlV3-Corner-Hash C) (HashTable FlV3 (Listof (corner C))))

(struct (C) corner-soup ([vertices : (Listof (corner C))]
                         [size : Index]
                         [lazy-flv3-corner-hash : (Promise (FlV3-Corner-Hash C))])
  #:transparent)

(: make-flv3-corner-hash (All (C) (-> (Listof (corner C)) (FlV3-Corner-Hash C))))
(define (make-flv3-corner-hash vertices)
  (: h (FlV3-Corner-Hash C))
  (define h (make-hash))
  (for ([vert  (in-list vertices)])
    (define v (corner-flv3 vert))
    (hash-set! h v (cons vert (hash-ref h v (λ () empty)))))
  h)

(: make-corner-soup (All (C) (-> (Listof (corner C)) (corner-soup C))))
(define (make-corner-soup vertices)
  (let ([vertices  (remove-duplicates vertices eq?)])
    (corner-soup vertices
                 (length vertices)
                 (delay (make-flv3-corner-hash vertices)))))

(: corner-soup-flv3-corner-hash (All (C) (-> (corner-soup C) (FlV3-Corner-Hash C))))
(define (corner-soup-flv3-corner-hash vertices)
  (force (corner-soup-lazy-flv3-corner-hash vertices)))

;; ===================================================================================================

(define-type Blend (-> FlV3 FlV3 Flonum FlV3))
(define-type Interp (-> vtx vtx FlV3 vtx))

(struct deform-data ([blend : Blend] [interp : Interp]) #:transparent)

(define linear-deform-data (deform-data flv3blend vtx-interpolate))
