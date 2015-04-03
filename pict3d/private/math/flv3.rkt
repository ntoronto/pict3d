#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax)
         (only-in racket/unsafe/ops
                  unsafe-flvector-ref
                  unsafe-flvector-set!
                  unsafe-vector-ref)
         racket/performance-hint
         racket/list
         math/flonum
         "fl.rkt"
         "fl3.rkt"
         "../utils.rkt")

(provide (except-out
          (all-defined-out)
          define/provide-unit-vectors))

;; ===================================================================================================
;; FlV3 struct type

(define print-flv3
  (make-constructor-style-printer
   (λ ([v : FlV3]) 'flv3)
   (λ ([v : FlV3]) (call/flv3-values v list))))

(struct FlV3 ([flvector : FlVector])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-flv3)

(define-syntax flv3? (make-rename-transformer #'FlV3?))

;(: call/flv3-values (All (A) (-> FlV3 (-> Flonum Flonum Flonum A) A)))
(define-syntax-rule (call/flv3-values v f)
  (let ([vs  (FlV3-flvector (ann v FlV3))])
    (f (unsafe-flvector-ref vs 0)
       (unsafe-flvector-ref vs 1)
       (unsafe-flvector-ref vs 2))))

(begin-encourage-inline
  
  (: flv3 (-> Flonum Flonum Flonum FlV3))
  (define (flv3 x y z)
    (if (and (<= -inf.0 (min x y z))
             (<= (max x y z) +inf.0))
        (FlV3 (flvector x y z))
        (error 'flv3 "expected non-NaNs; given ~e ~e ~e~n" x y z)))
  
  (: flnorm3 (-> Flonum Flonum Flonum (U #f FlV3)))
  (define (flnorm3 x y z)
    (call/fl3normalize x y z
      (λ ([x : Flonum] [y : Flonum] [z : Flonum])
        (if (= 0.0 (max (abs x) (abs y) (abs z)))
            #f
            (flv3 x y z)))))
  
  (: unsafe-flv3-ref (-> FlV3 Index Flonum))
  (define (unsafe-flv3-ref v i)
    (unsafe-flvector-ref (FlV3-flvector v) i))
  
  (: flv3-ref (-> FlV3 (U 0 1 2) Flonum))
  (define flv3-ref unsafe-flv3-ref)
  
  )  ; begin-encourage-inline

;; ===================================================================================================
;; FlV3 constants

(define zero-flv3 (flv3 0.0 0.0 0.0))

(define-syntax (define/provide-unit-vectors stx)
  (define/with-syntax (val ...)
    (for*/list ([dx  (in-list '(-1.0 0.0 1.0))]
                [dy  (in-list '(-1.0 0.0 1.0))]
                [dz  (in-list '(-1.0 0.0 1.0))]
                #:unless (= dx dy dz 0.0))
      #`(flv3 #,dx #,dy #,dz)))
  (define/with-syntax (name ...)
    (for*/list ([nx  (in-list '("-x" "" "+x"))]
                [ny  (in-list '("-y" "" "+y"))]
                [nz  (in-list '("-z" "" "+z"))]
                #:unless (and (equal? nx "") (equal? ny "") (equal? nz "")))
      (format-id stx "~a-flv3" (string-append nx ny nz))))
  #'(begin
      (define name val) ...
      (provide name ...)))

(define/provide-unit-vectors)

;; ===================================================================================================
;; FlV3 operations

(: flv3rational? (-> FlV3 Boolean))
(define (flv3rational? v)
  (call/flv3-values v
    (λ (x y z)
      (and (flrational? x) (flrational? y) (flrational? z)))))

(: flv3zero? (-> FlV3 Boolean))
(define (flv3zero? v)
  (call/flv3-values v
    (λ (x y z)
      (= 0.0 (min (abs x) (abs y) (abs z))))))

(: flv3equiv? (-> FlV3 FlV3 Nonnegative-Flonum Boolean))
(define (flv3equiv? v1 v2 tol)
  (call/flv3-values v1
    (λ (x1 y1 z1)
      (call/flv3-values v2
        (λ (x2 y2 z2)
          (and (flequiv? x1 x2 tol)
               (flequiv? y1 y2 tol)
               (flequiv? z1 z2 tol)))))))

(: flv3+ (-> FlV3 FlV3 FlV3))
(define (flv3+ v1 v2)
  (call/flv3-values v1
    (λ (x1 y1 z1)
      (call/flv3-values v2
        (λ (x2 y2 z2)
          (flv3 (+ x1 x2) (+ y1 y2) (+ z1 z2)))))))

(: flv3- (-> FlV3 FlV3 FlV3))
(define (flv3- v1 v2)
  (call/flv3-values v1
    (λ (x1 y1 z1)
      (call/flv3-values v2
        (λ (x2 y2 z2)
          (flv3 (- x1 x2) (- y1 y2) (- z1 z2)))))))

(: flv3neg (-> FlV3 FlV3))
(define (flv3neg v)
  (call/flv3-values v
    (λ (x y z)
      (flv3 (- x) (- y) (- z)))))

(: flv3* (-> FlV3 Flonum FlV3))
(define (flv3* v a)
  (call/flv3-values v
    (λ (x y z)
      (flv3 (* x a) (* y a) (* z a)))))

(: flv3/ (-> FlV3 Flonum FlV3))
(define (flv3/ v a)
  (call/flv3-values v
    (λ (x y z)
      (flv3 (/ x a) (/ y a) (/ z a)))))

(: flv3fma (-> FlV3 Flonum FlV3 FlV3))
(define (flv3fma dv a v0)
  (call/flv3-values dv
    (λ (dx dy dz)
      (call/flv3-values v0
        (λ (x0 y0 z0)
          (call/fl3fma dx dy dz a x0 y0 z0 flv3))))))

(: flv3cross (-> FlV3 FlV3 FlV3))
(define (flv3cross v1 v2)
  (call/flv3-values v1
    (λ (x1 y1 z1)
      (call/flv3-values v2
        (λ (x2 y2 z2)
          (call/fl3cross x1 y1 z1 x2 y2 z2 flv3))))))

;(: flv3dot (-> FlV3 FlV3 Flonum))
(define-syntax-rule (flv3dot v1 v2)
  (call/flv3-values v1
    (λ (x1 y1 z1)
      (call/flv3-values v2
        (λ (x2 y2 z2)
          (fl3dot x1 y1 z1 x2 y2 z2))))))

;(: flv3mag^2 (-> FlV3 Nonnegative-Flonum))
(define-syntax-rule (flv3mag^2 v)
  (call/flv3-values v fl3mag^2))

;(: flv3mag (-> FlV3 Nonnegative-Flonum))
(define-syntax-rule (flv3mag v)
  (call/flv3-values v fl3mag))

(: flv3normalize (-> FlV3 (U #f FlV3)))
(define (flv3normalize v)
  (call/flv3-values v flnorm3))

;(: flv3dist^2 (-> FlV3 FlV3 Nonnegative-Flonum))
(define-syntax-rule (flv3dist^2 v1 v2)
  (call/flv3-values v1
    (λ (x1 y1 z1)
      (call/flv3-values v2
        (λ (x2 y2 z2)
          (fl3mag^2 (- x2 x1) (- y2 y1) (- z2 z1)))))))

;(: flv3dist (-> FlV3 FlV3 Nonnegative-Flonum))
(define-syntax-rule (flv3dist v1 v2)
  (call/flv3-values v1
    (λ (x1 y1 z1)
      (call/flv3-values v2
        (λ (x2 y2 z2)
          (fl3mag (- x2 x1) (- y2 y1) (- z2 z1)))))))

;; ===================================================================================================
;; Functions of polygon vertices

(: flv3triangle-normal (-> FlV3 FlV3 FlV3 (U #f FlV3)))
(define (flv3triangle-normal v1 v2 v3)
  (call/flv3-values v1
    (λ (x1 y1 z1)
      (call/flv3-values v2
        (λ (x2 y2 z2)
          (call/flv3-values v3
            (λ (x3 y3 z3)
              (call/fl3cross (- x3 x2) (- y3 y2) (- z3 z2) (- x1 x2) (- y1 y2) (- z1 z2)
                flnorm3))))))))

(: flv3polygon-perp (-> (Vectorof FlV3) FlV3))
;; Compute a best-fit perpendicular using Newell's method
(define (flv3polygon-perp vs)
  (define n (vector-length vs))
  (cond
    [(< n 3)  zero-flv3]
    [else
     (call/flv3-values (vector-ref vs (- n 1))
       (λ (x1 y1 z1)
         (define-values (x y z _x1 _y1 _z1)
           (for/fold ([x : Flonum  0.0]
                      [y : Flonum  0.0]
                      [z : Flonum  0.0]
                      [x1 : Flonum  x1]
                      [y1 : Flonum  y1]
                      [z1 : Flonum  z1])
                     ([i : Nonnegative-Fixnum  (in-range n)])
             (call/flv3-values (unsafe-vector-ref vs i)
               (λ (x2 y2 z2)
                 (call/fl3cross x1 y1 z1 x2 y2 z2
                   (λ (x3 y3 z3)
                     (values (+ x x3) (+ y y3) (+ z z3) x2 y2 z2)))))))
         (flv3 x y z)))]))

(: flv3polygon-normal (-> (Vectorof FlV3) (U #f FlV3)))
(define (flv3polygon-normal vs)
  (flv3normalize (flv3polygon-perp vs)))

(: flv3polygon-area (-> (Vectorof FlV3) Nonnegative-Flonum))
(define (flv3polygon-area vs)
  (call/flv3-values (flv3polygon-perp vs)
    (λ (x y z)
      (fl3mag (* 0.5 x) (* 0.5 y) (* 0.5 z)))))

(: flv3polygon-perimeter (-> (Vectorof FlV3) Nonnegative-Flonum))
(define (flv3polygon-perimeter vs)
  (define n (vector-length vs))
  (cond
    [(= n 0)  0.0]
    [else
     (call/flv3-values (unsafe-vector-ref vs (- n 1))
       (λ (x1 y1 z1)
         (define-values (sum _x1 _y1 _z1)
           (for/fold ([sum : Nonnegative-Flonum  0.0]
                      [x1 : Flonum  x1]
                      [y1 : Flonum  y1]
                      [z1 : Flonum  z1])
                     ([i : Nonnegative-Fixnum  (in-range n)])
             (call/flv3-values (unsafe-vector-ref vs i)
               (λ (x2 y2 z2)
                 (values (+ sum (fl3mag (- x1 x2) (- y1 y2) (- z1 z2))) x2 y2 z2)))))
         sum))]))

(: flv3polygon-centroid (-> (Vectorof FlV3) FlV3))
(define (flv3polygon-centroid vs)
  (define n (vector-length vs))
  (cond
    [(= n 0)  zero-flv3]
    [else
     (call/flv3-values (vector-ref vs 0)
       (λ (x1 y1 z1)
         (define-values (x y z m)
           (for/fold ([x1 : Flonum  x1]
                      [y1 : Flonum  y1]
                      [z1 : Flonum  z1]
                      [m  : Flonum  1.0])
                     ([i  (in-range 1 n)])
             (call/flv3-values (unsafe-vector-ref vs i)
               (λ (x2 y2 z2)
                 (values (+ x1 x2) (+ y1 y2) (+ z1 z2) (+ m 1.0))))))
         (flv3 (/ x m) (/ y m) (/ z m))))]))

(: flv3polygon-regularity (-> (Vectorof FlV3) Nonnegative-Flonum))
;; Returns a number in [0,1] that indicates how close to regular the polygon is
;; Any polygon with fewer than 3 non-collinear points has regularity 0
(define (flv3polygon-regularity vs)
  (define n (vector-length vs))
  (cond [(< n 3)  0.0]
        [else
         (define p (flv3polygon-perimeter vs))
         (define denom (flregular-polygon-area p n))
         (if (<= denom 0.0)
             0.0
             (max 0.0 (min 1.0 (/ (flv3polygon-area vs) (abs denom)))))]))
