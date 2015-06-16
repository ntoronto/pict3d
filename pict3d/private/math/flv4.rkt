#lang typed/racket/base

;; Homogeneous coordinate vectors, colors

(require (for-syntax racket/base)
         (only-in racket/unsafe/ops
                  unsafe-flvector-ref)
         racket/performance-hint
         math/flonum
         "../utils.rkt"
         "fl.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; FlV4 struct type

(define print-flv4
  (make-constructor-style-printer
   (λ ([v : FlV4]) 'flv4)
   (λ ([v : FlV4]) (call/flv4-values v list))))

(struct FlV4 ([flvector : FlVector])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-flv4)

(define-syntax flv4? (make-rename-transformer #'FlV4?))

;(: call/flv4-values (All (A) (-> FlV4 (-> Flonum Flonum Flonum Flonum A) A)))
(define-syntax-rule (call/flv4-values v f)
  (let ([vs  (FlV4-flvector (ann v FlV4))])
    (f (unsafe-flvector-ref vs 0)
       (unsafe-flvector-ref vs 1)
       (unsafe-flvector-ref vs 2)
       (unsafe-flvector-ref vs 3))))

(begin-encourage-inline
  
  (: flv4 (-> Flonum Flonum Flonum Flonum FlV4))
  (define (flv4 x y z w)
    (if (and (<= -inf.0 (min x y z w))
             (<= (max x y z w) +inf.0))
        (FlV4 (flvector x y z w))
        (error 'flv4 "expected non-NaNs; given ~e ~e ~e ~e" x y z w)))

  (: unsafe-flv4-ref (-> FlV4 Index Flonum))
  (define (unsafe-flv4-ref v i)
    (unsafe-flvector-ref (FlV4-flvector v) i))

  (: flv4-ref (-> FlV4 (U 0 1 2 3) Flonum))
  (define flv4-ref unsafe-flv4-ref)
  
  )  ; begin-encourage-inline

(define zero-flv4 (flv4 0.0 0.0 0.0 0.0))

;; ===================================================================================================
;; FlV4 operations

(: flv4rational? (-> FlV4 Boolean))
(define (flv4rational? v)
  (call/flv4-values v
    (λ (x y z w)
      (and (< -inf.0 (min x y z w))
           (< (max x y z w) +inf.0)))))

(: flv4zero? (-> FlV4 Boolean))
(define (flv4zero? v)
  (call/flv4-values v
    (λ (x y z w)
      (= 0.0 (min (abs x) (abs y) (abs z) (abs w))))))

(: flv4near? (-> FlV4 FlV4 Flonum Boolean))
(define (flv4near? v1 v2 eps)
  (call/flv4-values v1
    (λ (x1 y1 z1 w1)
      (call/flv4-values v2
        (λ (x2 y2 z2 w2)
          (and (flnear? x1 x2 eps)
               (flnear? y1 y2 eps)
               (flnear? z1 z2 eps)
               (flnear? w1 w2 eps)))))))

(: flv4+ (-> FlV4 FlV4 FlV4))
(define (flv4+ v1 v2)
  (call/flv4-values v1
    (λ (x1 y1 z1 w1)
      (call/flv4-values v2
        (λ (x2 y2 z2 w2)
          (flv4 (+ x1 x2) (+ y1 y2) (+ z1 z2) (+ w1 w2)))))))

(: flv4- (-> FlV4 FlV4 FlV4))
(define (flv4- v1 v2)
  (call/flv4-values v1
    (λ (x1 y1 z1 w1)
      (call/flv4-values v2
        (λ (x2 y2 z2 w2)
          (flv4 (- x1 x2) (- y1 y2) (- z1 z2) (- w1 w2)))))))

(: flv4neg (-> FlV4 FlV4))
(define (flv4neg v)
  (call/flv4-values v
    (λ (x y z w)
      (flv4 (- x) (- y) (- z) (- w)))))

(: flv4* (-> FlV4 Flonum FlV4))
(define (flv4* v a)
  (call/flv4-values v
    (λ (x y z w)
      (flv4 (* x a) (* y a) (* z a) (* w a)))))

(: flv4/ (-> FlV4 Flonum FlV4))
(define (flv4/ v a)
  (call/flv4-values v
    (λ (x y z w)
      (flv4 (/ x a) (/ y a) (/ z a) (/ w a)))))

(: flv4blend (-> FlV4 FlV4 Flonum FlV4))
(define (flv4blend v1 v2 α)
  (call/flv4-values v1
    (λ (x1 y1 z1 w1)
      (call/flv4-values v2
        (λ (x2 y2 z2 w2)
          (flv4 (flblend x1 x2 α) (flblend y1 y2 α) (flblend z1 z2 α) (flblend w1 w2 α)))))))
