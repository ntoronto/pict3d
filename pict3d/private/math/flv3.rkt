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

(begin-encourage-inline
  
  (: flv3=? (-> FlV3 FlV3 Boolean))
  (define (flv3=? v1 v2)
    (or (eq? v1 v2)
        (call/flv3-values v1
          (λ (x1 y1 z1)
            (call/flv3-values v2
              (λ (x2 y2 z2)
                (and (= x1 x2) (= y1 y2) (= z1 z2))))))))
  
  (: flv3rational? (-> FlV3 Boolean))
  (define (flv3rational? v)
    (call/flv3-values v
      (λ (x y z)
        (and (< -inf.0 (min x y z))
             (< (max x y z) +inf.0)))))
  
  (: flv3zero? (-> FlV3 Boolean))
  (define (flv3zero? v)
    (or (eq? v zero-flv3)
        (call/flv3-values v
          (λ (x y z)
            (= 0.0 (max (abs x) (abs y) (abs z)))))))
  
  (: flv3near? (-> FlV3 FlV3 Flonum Boolean))
  (define (flv3near? v1 v2 eps)
    (call/flv3-values v1
      (λ (x1 y1 z1)
        (call/flv3-values v2
          (λ (x2 y2 z2)
            (and (flnear? x1 x2 eps)
                 (flnear? y1 y2 eps)
                 (flnear? z1 z2 eps)))))))
  
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
  
  (: flv3blend (-> FlV3 FlV3 Flonum FlV3))
  (define (flv3blend v1 v2 α)
    (call/flv3-values v1
      (λ (x1 y1 z1)
        (call/flv3-values v2
          (λ (x2 y2 z2)
            (flv3 (flblend x1 x2 α) (flblend y1 y2 α) (flblend z1 z2 α)))))))
  
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
  
  (: flv3cos (-> FlV3 FlV3 Flonum))
  (define (flv3cos v1 v2)
    (flclamp (/ (flv3dot v1 v2) (flv3mag v1) (flv3mag v2)) -1.0 1.0))
  
  (: flv3bend-cos (-> FlV3 FlV3 FlV3 Flonum))
  (define (flv3bend-cos v1 v2 v3)
    (flv3cos (flv3- v1 v2) (flv3- v2 v3)))
  
  (: flv3corner-cos (-> FlV3 FlV3 FlV3 Flonum))
  (define (flv3corner-cos v1 v2 v3)
    (flv3cos (flv3- v1 v2) (flv3- v3 v2)))
  
  (: flv3proj (-> FlV3 FlV3 (U #f FlV3)))
  (define (flv3proj v1 v2)
    (define m (/ (flv3dot v1 v2) (flv3mag^2 v2)))
    (if (< -inf.0 m +inf.0) (flv3* v2 m) #f))
  
  (: flv3sproj (-> FlV3 FlV3 (U #f Flonum)))
  (define (flv3sproj v1 v2)
    (define s (/ (flv3dot v1 v2) (flv3mag v2)))
    (if (< -inf.0 s +inf.0) s #f))
  
  (: flv3rej (-> FlV3 FlV3 Flonum FlV3))
  (define (flv3rej v1 v2 s)
    (define v (flv3proj v1 v2))
    (if v (flv3- v1 (flv3* v s)) v1))
  
  (: flv3refl (-> FlV3 FlV3 FlV3))
  (define (flv3refl v1 v2)
    (flv3rej v1 v2 2.0))
  
  (: flv3alpha (-> FlV3 FlV3 FlV3 (U #f Flonum)))
  (define (flv3alpha v1 v2 v)
    (define dv2 (flv3- v2 v1))
    (define s (flv3sproj (flv3- v v1) dv2))
    (and s (let ([α  (/ s (flv3mag dv2))])
             (and (< -inf.0 α +inf.0) α))))
  
  )  ; begin-encourage-inline

;; ===================================================================================================
;; Functions of polygon vertices

(: flv3polygon-perp* (-> (Listof FlV3) FlV3))
;; Compute a best-fit perpendicular using Newell's method
(define (flv3polygon-perp* vs)
  (define n (length vs))
  (cond
    [(< n 3)  zero-flv3]
    [else
     (call/flv3-values (last vs)
       (λ (x1 y1 z1)
         (define-values (_x1 _y1 _z1 x y z)
           (for/fold ([x1 : Flonum  x1]
                      [y1 : Flonum  y1]
                      [z1 : Flonum  z1]
                      [x : Flonum  0.0]
                      [y : Flonum  0.0]
                      [z : Flonum  0.0])
                     ([v2  (in-list vs)])
             (call/flv3-values v2
               (λ (x2 y2 z2)
                 (call/fl3cross x1 y1 z1 x2 y2 z2
                   (λ (x3 y3 z3)
                     (values x2 y2 z2 (+ x x3) (+ y y3) (+ z z3))))))))
         (flv3 x y z)))]))

(: flv3polygon-normal* (-> (Listof FlV3) (U #f FlV3)))
(define (flv3polygon-normal* vs)
  (flv3normalize (flv3polygon-perp* vs)))

(: flv3polygon-area* (-> (Listof FlV3) Nonnegative-Flonum))
(define (flv3polygon-area* vs)
  (* 0.5 (call/flv3-values (flv3polygon-perp* vs) fl3mag)))

(: flv3polygon-perimeter* (-> (Listof FlV3) Nonnegative-Flonum))
(define (flv3polygon-perimeter* vs)
  (cond
    [(empty? vs)  0.0]
    [else
     (call/flv3-values (last vs)
       (λ (x1 y1 z1)
         (define-values (_x1 _y1 _z1 sum)
           (for/fold ([x1 : Flonum  x1]
                      [y1 : Flonum  y1]
                      [z1 : Flonum  z1]
                      [sum : Nonnegative-Flonum  0.0])
                     ([v2  (in-list vs)])
             (call/flv3-values v2
               (λ (x2 y2 z2)
                 (values x2 y2 z2 (+ sum (fl3mag (- x1 x2) (- y1 y2) (- z1 z2))))))))
         sum))]))

(: flv3mean* (-> (Listof FlV3) FlV3))
(define (flv3mean* vs)
  (cond
    [(empty? vs)  zero-flv3]
    [else
     (call/flv3-values (first vs)
       (λ (x1 y1 z1)
         (define-values (x y z m)
           (for/fold ([x1 : Flonum  x1]
                      [y1 : Flonum  y1]
                      [z1 : Flonum  z1]
                      [m  : Flonum  1.0])
                     ([v2  (in-list (rest vs))])
             (call/flv3-values v2
               (λ (x2 y2 z2)
                 (values (+ x1 x2) (+ y1 y2) (+ z1 z2) (+ m 1.0))))))
         (flv3 (/ x m) (/ y m) (/ z m))))]))

(: flv3polygon-regularity* (-> (Listof FlV3) Nonnegative-Flonum))
(define (flv3polygon-regularity* vs)
  (define n (length vs))
  (cond
    [(< n 3)  0.0]
    [else
     (define p (flv3polygon-perimeter* vs))
     (define denom (flregular-polygon-area p n))
     (if (> denom 0.0)
         (flclamp (/ (flv3polygon-area* vs) denom) 0.0 1.0)
         0.0)]))

(begin-encourage-inline
  
  (: flv3polygon-perp (-> FlV3 * FlV3))
  (define flv3polygon-perp
    (case-lambda
      [()  zero-flv3]
      [(v1)  zero-flv3]
      [(v1 v2)  zero-flv3]
      [(v1 v2 v3)
       (call/flv3-values v1
         (λ (x1 y1 z1)
           (call/flv3-values v2
             (λ (x2 y2 z2)
               (call/flv3-values v3
                 (λ (x3 y3 z3)
                   (call/fl3cross (- x3 x2) (- y3 y2) (- z3 z2) (- x1 x2) (- y1 y2) (- z1 z2)
                     flv3)))))))]
      [vs
       (flv3polygon-perp* vs)]))
  
  (: flv3polygon-normal (-> FlV3 * (U #f FlV3)))
  (define flv3polygon-normal
    (case-lambda
      [()  #f]
      [(v1)  #f]
      [(v1 v2)  #f]
      [(v1 v2 v3)
       (call/flv3-values v1
         (λ (x1 y1 z1)
           (call/flv3-values v2
             (λ (x2 y2 z2)
               (call/flv3-values v3
                 (λ (x3 y3 z3)
                   (call/fl3cross (- x3 x2) (- y3 y2) (- z3 z2) (- x1 x2) (- y1 y2) (- z1 z2)
                     flnorm3)))))))]
      [vs
       (flv3polygon-normal* vs)]))
  
  (: flv3polygon-area (-> FlV3 * Nonnegative-Flonum))
  (define flv3polygon-area
    (case-lambda
      [()  0.0]
      [(v1)  0.0]
      [(v1 v2)  0.0]
      [(v1 v2 v3)
       (* 0.5 (call/flv3-values (flv3polygon-perp v1 v2 v3) fl3mag))]
      [vs
       (flv3polygon-area* vs)]))
  
  (: flv3polygon-perimeter (-> FlV3 * Nonnegative-Flonum))
  (define flv3polygon-perimeter
    (case-lambda
      [()  0.0]
      [(v1)  0.0]
      [(v1 v2)
       (* 2.0 (flv3dist v1 v2))]
      [(v1 v2 v3)
       (call/flv3-values v1
         (λ (x1 y1 z1)
           (call/flv3-values v2
             (λ (x2 y2 z2)
               (call/flv3-values v3
                 (λ (x3 y3 z3)
                   (+ (fl3mag (- x2 x1) (- y2 y1) (- z2 z1))
                      (fl3mag (- x3 x2) (- y3 y2) (- z3 z2))
                      (fl3mag (- x1 x3) (- y1 y3) (- z1 z3)))))))))]
      [vs
       (flv3polygon-perimeter* vs)]))
  
  (: flv3mean (-> FlV3 * FlV3))
  (define flv3mean
    (case-lambda
      [()  zero-flv3]
      [(v1)  v1]
      [(v1 v2)
       (flv3* (flv3+ v1 v2) 0.5)]
      [(v1 v2 v3)
       (call/flv3-values v1
         (λ (x1 y1 z1)
           (call/flv3-values v2
             (λ (x2 y2 z2)
               (call/flv3-values v3
                 (λ (x3 y3 z3)
                   (flv3 (* #i1/3 (+ x1 x2 x3))
                         (* #i1/3 (+ y1 y2 y3))
                         (* #i1/3 (+ z1 z2 z3)))))))))]
      [vs
       (flv3mean* vs)]))
  
  (: flv3polygon-regularity (-> FlV3 * Nonnegative-Flonum))
  ;; Returns a number in [0,1] that indicates how close to regular the polygon is
  ;; Any polygon with fewer than 3 non-collinear points has regularity 0
  (define flv3polygon-regularity
    (case-lambda
      [()  0.0]
      [(v1)  0.0]
      [(v1 v2)  0.0]
      [(v1 v2 v3)
       (define p (flv3polygon-perimeter v1 v2 v3))
       (define denom (flregular-polygon-area p 3))
       (if (> denom 0.0)
           (flclamp (/ (flv3polygon-area v1 v2 v3) denom) 0.0 1.0)
           0.0)]
      [vs
       (flv3polygon-regularity* vs)]))
  
  )  ; begin-encourage-inline
