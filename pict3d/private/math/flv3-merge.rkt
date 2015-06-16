#lang typed/racket/base

(require racket/list
         racket/performance-hint
         racket/unsafe/ops
         math/flonum
         "flv3.rkt")

(provide flv3merge-hash*
         flv3equivalence-lists*)

(: flv3merge-hash* (-> (Listof FlV3) Positive-Flonum (HashTable FlV3 FlV3)))
(define (flv3merge-hash* vs eps)
  (define vss (flv3equivalence-lists* vs eps))
  (define h ((inst make-hasheq FlV3 FlV3)))
  (for ([vs  (in-list vss)])
    (define v* (flv3mean* vs))
    (for ([v  (in-list vs)])
      (hash-set! h v v*)))
  h)

(: flv3equivalence-lists* (-> (Listof FlV3) Positive-Flonum (Listof (Listof FlV3))))
(define (flv3equivalence-lists* all-vs orig-eps)
  (when (> orig-eps 0.25)
    (raise-argument-error 'equivalent-vertices "positive flonum <= 0.25" orig-eps))
  
  (define bits (flceiling (fllog2 orig-eps)))
  (define eps (flexp2 bits))
  
  ;; 500 elements seems to be about where the O(n^2) naive algorithm starts becoming worse than
  ;; the overhead in the O(n) hash-based algorithm
  (define use-hash? (>= (length all-vs) 500))
  
  (begin-encourage-inline
    
    (: key (-> Flonum Flonum))
    (define (key x)
      (define b (flexp2 (+ 2.0 (+ bits (flfloor (fllog2 (abs x)))))))
      (* (round (/ x b)) b))
    
    (: next-key (-> Flonum Flonum))
    (define (next-key x)
      (+ x (flexp2 (+ (+ 1.5 (* 0.5 (flsgn x)))  ; equiv. if x < 0 then 1.0 else 2.0
                      (+ bits (flfloor (fllog2 (abs x))))))))
    
    (: prev-key (-> Flonum Flonum))
    (define (prev-key x) (- (next-key (- x))))
    
    (: adjacent-keys! (-> Flonum FlVector Index))
    (define (adjacent-keys! x ks)
      (define k2 (key x))
      (cond [(< k2 x)
             (unsafe-flvector-set! ks 0 k2)
             (unsafe-flvector-set! ks 1 (next-key k2))
             (unsafe-flvector-set! ks 2 +nan.0)
             2]
            [(> k2 x)
             (let ([k1  (prev-key k2)])
               (unsafe-flvector-set! ks 0 (prev-key k1))
               (unsafe-flvector-set! ks 1 k1)
               (unsafe-flvector-set! ks 2 k2)
               3)]
            [else
             (unsafe-flvector-set! ks 0 k2)
             (unsafe-flvector-set! ks 1 +nan.0)
             (unsafe-flvector-set! ks 2 +nan.0)
             1]))
    
    (: hash3 (-> Flonum Flonum Flonum Flonum))
    ;; Using flonums for hash keys is 10x faster than using flvectors, probably because it avoids
    ;; allocation and a slow hash key path - but it requires a hashing function
    (define (hash3 kx ky kz)
      ;; The following formula seems to be chaotic enough to work well
      (+ (sin (* kx 7.750282108797385e32))
         (sin (* ky 8.676657198170363e31))
         (sin (* kz 3.919010235731054e30))))
    
    (: near? (-> Flonum Flonum Boolean))
    (define (near? x1 x2)
      (define d (abs (- x1 x2)))
      (or (<= d (* orig-eps (abs x1)))
          (<= d (* orig-eps (abs x2)))))
    
    (: flv3near? (-> FlV3 FlV3 Boolean))
    (define (flv3near? v1 v2)
      (call/flv3-values v1
        (λ (x1 y1 z1)
          (call/flv3-values v2
            (λ (x2 y2 z2)
              (and (near? x1 x2) (near? y1 y2) (near? z1 z2)))))))
    
    )  ; begin-encourage-inline
  
  (define h ((inst make-hash Flonum (Listof FlV3))))
  
  (when use-hash?
    (for ([v  (in-list all-vs)])
      (call/flv3-values v
        (λ (x y z)
          (define k (hash3 (key x) (key y) (key z)))
          (hash-set! h k (cons v (hash-ref! h k (λ () empty)))))))
    #|
    (define ls
      (for/list : (Listof Index) ([(k vs)  (in-hash h)])
        (length vs)))
    (printf "mean ls = ~v, stddev ls = ~v~n"
            (fl (mean ls))
            (fl (stddev ls)))
    |#
    )
  
  (define visited ((inst make-hasheq FlV3 #t)))
  (define kxs (make-flvector 3))
  (define kys (make-flvector 3))
  (define kzs (make-flvector 3))
  
  (let outer-loop ([vs all-vs] [acc-vss : (Listof (Listof FlV3))  empty])
    (cond
      [(empty? vs)  acc-vss]
      [else
       (let inner-loop ([stack : (Listof FlV3)  (list (first vs))]
                        [acc-vs : (Listof FlV3)  empty])
         (cond
           [(empty? stack)
            (outer-loop (rest vs) (if (empty? acc-vs) acc-vss (cons acc-vs acc-vss)))]
           [else
            (let-values ([(v stack)  (values (first stack) (rest stack))])
              (cond
                [(hash-ref visited v #f)  (inner-loop stack acc-vs)]
                [else
                 (hash-set! visited v #t)
                 (define adj-vs
                   (if use-hash?
                       (call/flv3-values v
                         (λ (x y z)
                           (define nx (adjacent-keys! x kxs))
                           (define ny (adjacent-keys! y kys))
                           (define nz (adjacent-keys! z kzs))
                           (for*/fold ([adj-vs : (Listof FlV3)  empty])
                                      ([ix  (in-range nx)]
                                       [kx  (in-value (unsafe-flvector-ref kxs ix))]
                                       [iy  (in-range ny)]
                                       [ky  (in-value (unsafe-flvector-ref kys iy))]
                                       [iz  (in-range nz)]
                                       [kz  (in-value (unsafe-flvector-ref kzs iz))]
                                       [v2  (in-list (hash-ref h (hash3 kx ky kz) (λ () empty)))])
                             (if (and (not (eq? v v2))
                                      (flv3near? v v2)
                                      (not (hash-ref visited v2 #f)))
                                 (cons v2 adj-vs)
                                 adj-vs))))
                       (filter (λ ([v2 : FlV3]) (and (not (eq? v v2))
                                                     (flv3near? v v2)
                                                     (not (hash-ref visited v2 #f))))
                               all-vs)))
                 (inner-loop (append adj-vs stack) (cons v acc-vs))]))]))])))
