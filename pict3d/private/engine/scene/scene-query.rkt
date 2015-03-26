#lang typed/racket/base

(require (only-in racket/unsafe/ops
                  unsafe-fx+)
         racket/list
         racket/promise
         "../../math.rkt"
         "../../gl.rkt"
         "../../utils.rkt"
         "../draw.rkt"
         "tags.rkt"
         "types.rkt"
         "scene.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Map over shapes

(: scene-map-shapes (-> Scene (-> shape shape) Scene))
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
                                (-> shape FlAffine3 Nonnegative-Fixnum Any)
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
                       (-> shape FlAffine3 Nonnegative-Fixnum Any)
                       Nonnegative-Fixnum
                       Nonnegative-Fixnum))
(define (scene-for-each! s planes f start)
  (if (empty-scene? s)
      start
      (nonempty-scene-for-each! s planes f start)))

(: scene-extract (All (B) (-> Scene (Listof FlPlane3) (-> shape FlAffine3 B) (Listof B))))
(define (scene-extract s planes f)
  (: bs (Listof B))
  (define bs empty)
  (scene-for-each! s
                   planes
                   (λ ([a : shape] [t : FlAffine3] [i : Nonnegative-Fixnum])
                     (set! bs (cons (f a t) bs)))
                   0)
  bs)

;; ===================================================================================================
;; Transforming scenes by transforming all shapes

(: scene-deep-transform (-> Scene FlAffine3 Scene))
(define (scene-deep-transform s t)
  (cond
    [(empty-scene? s)  s]
    [(shape? s)
     (scene-union* (shape-deep-transform s t))]
    [(node-scene? s)
     (make-node-scene (scene-deep-transform (node-scene-neg s) t)
                      (scene-deep-transform (node-scene-pos s) t))]
    [(trans-scene? s)
     (scene-deep-transform (trans-scene-scene s)
                           (flt3compose t (trans-scene-affine s)))]
    [(group-scene? s)
     (scene-deep-transform (group-scene-scene s) t)]))

(: scene-flattened? (-> Scene Boolean))
(define (scene-flattened? s)
  (cond
    [(empty-scene? s)  #t]
    [(shape? s)  #t]
    [(node-scene? s)  (and (scene-flattened? (node-scene-neg s))
                           (scene-flattened? (node-scene-pos s)))]
    [(trans-scene? s)  #f]
    [(group-scene? s)  #f]))

;; ===================================================================================================
;; Filter over shapes

(: scene-filter-shapes (-> Scene (-> shape Boolean) Scene))
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

(: transform-surface-data (-> (Promise surface-data) FlAffine3 (Promise surface-data)))
(define (transform-surface-data data t)
  (let ([data  (force data)])
    (delay (surface-data (flt3apply/pos t (surface-data-point data))
                         (let ([n  (surface-data-normal data)])
                           (and n (flt3apply/norm t n)))))))

(: nonempty-scene-ray-intersect (-> Nonempty-Scene FlV3 FlV3 Flonum
                                    (Values (U #f Flonum) (U #f (Promise surface-data)))))
(define (nonempty-scene-ray-intersect s v dv max-time)
  (let loop ([s s] [v v] [dv dv] [t identity-flaffine3] [max-time max-time])
    (cond
      [(shape? s)
       (define-values (time data) (shape-line-intersect s v dv max-time))
       (if (or (not time) (not data) (< time 0.0))
           (values #f #f)
           (values time (if (identity-flaffine3? t)
                            data
                            (transform-surface-data data (flt3inverse t)))))]
      [(node-scene? s)
       (define b (node-scene-visible-bbox s))
       (cond
         [(not b)  (values #f #f)]
         [else
          (define r (bbox-rect b))
          (define-values (tmin tmax)
            (let-values ([(tmin tmax)  (flrect3-line-intersects r v dv max-time)])
              (if (and tmin tmax (> (bbox-badness b) 16.0))
                  (let ([r  (maybe-bbox-rect (scene-bbox/badness s 16.0))])
                    (maybe-flrect3-line-intersects r v dv max-time))
                  (values tmin tmax))))
          (cond
            [(or (not tmin) (not tmax) (and (< tmin 0.0) (< tmax 0.0)))  (values #f #f)]
            [else
             (define s1 (node-scene-neg s))
             (define s2 (node-scene-pos s))
             
             (define brute-force-fallback
               (λ ([s1 : Nonempty-Scene] [s2 : Nonempty-Scene])
                 (define-values (time1 data1) (loop s1 v dv t max-time))
                 (let ([max-time  (if time1 (min time1 max-time) max-time)])
                   (define-values (time2 data2) (loop s2 v dv t max-time))
                   (if (and time1 time2 data1 data2)
                       (if (<= time1 time2)
                           (values time1 data1)
                           (values time2 data2))
                       (if (and time1 data1)
                           (values time1 data1)
                           (values time2 data2))))))
             
             (cond
               [(and (node-scene? s1) (node-scene? s2))
                (define b1 (node-scene-visible-bbox s1))
                (define b2 (node-scene-visible-bbox s2))
                (cond
                  [(and b1 b2)
                   (define plane (flrect3-separating-plane (bbox-rect b1) (bbox-rect b2)))
                   (cond
                     [plane
                      (let-values ([(s1 s2)  (if (<= (flplane3-point-dist plane v) 0.0)
                                                 (values s1 s2)
                                                 (values s2 s1))])
                        (define-values (time data) (loop s1 v dv t max-time))
                        (cond [(and time data)  (values time data)]
                              [else  (loop s2 v dv t max-time)]))]
                     [else
                      (let-values ([(s1 s2)  (if (<= (flv3dist v (flrect3-center (bbox-rect b1)))
                                                     (flv3dist v (flrect3-center (bbox-rect b2))))
                                                 (values s1 s2)
                                                 (values s2 s1))])
                        (brute-force-fallback s1 s2))])]
                  [b1  (loop s1 v dv t max-time)]
                  [b2  (loop s2 v dv t max-time)]
                  [else  (values #f #f)])]
               [else
                (brute-force-fallback s1 s2)])])])]
      [(trans-scene? s)
       (define t0 (flt3inverse (trans-scene-affine s)))
       (loop (trans-scene-scene s)
             (flt3apply/pos t0 v)
             (flt3apply/dir t0 dv)
             (flt3compose t0 t)
             max-time)]
      [(group-scene? s)
       (define s0 (group-scene-scene s))
       (cond [(empty-scene? s0)  (values #f #f)]
             [else  (loop s0 v dv t max-time)])])))

(: scene-ray-intersect (-> Scene FlV3 FlV3 (Values (U #f Flonum) (U #f (Promise surface-data)))))
(define (scene-ray-intersect s v dv)
  (if (empty-scene? s) (values #f #f) (nonempty-scene-ray-intersect s v dv +inf.0)))

(: scene-line-intersect (-> Scene FlV3 FlV3 (Values (U #f Flonum) (U #f (Promise surface-data)))))
(define (scene-line-intersect s v dv)
  (if (empty-scene? s)
      (values #f #f)
      (let-values ([(time data)  (nonempty-scene-ray-intersect s v dv 1.0)])
        (if (and time (<= time 1.0))
            (values time data)
            (values #f #f)))))

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
       (λ ([a : shape] [t : FlAffine3] [i : Nonnegative-Fixnum])
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
