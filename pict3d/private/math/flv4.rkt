#lang typed/racket/base

;; Homogeneous coordinate vectors, colors

(require (for-syntax racket/base)
         (only-in racket/unsafe/ops
                  unsafe-flvector-ref)
         racket/performance-hint
         math/flonum
         "../utils.rkt")

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
    (FlV4 (flvector x y z w)))

  (: unsafe-flv4-ref (-> FlV4 Index Flonum))
  (define (unsafe-flv4-ref v i)
    (unsafe-flvector-ref (FlV4-flvector v) i))

  (: flv4-ref (-> FlV4 (U 0 1 2 3) Flonum))
  (define flv4-ref unsafe-flv4-ref)
  
  )  ; begin-encourage-inline

(define zero-flv4 (flv4 0.0 0.0 0.0 0.0))
