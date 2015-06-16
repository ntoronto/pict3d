#lang typed/racket/base

(require (for-syntax racket/base)
         (only-in racket/unsafe/ops
                  unsafe-flvector-ref
                  unsafe-flvector-set!)
         racket/performance-hint
         racket/list
         racket/flonum
         "fl3.rkt"
         "flv3.rkt"
         "../utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; FlPlane3 struct type

(define print-flplane3
  (make-constructor-style-printer
   (λ ([p : FlPlane3]) 'unsafe-flplane3)
   (λ ([p : FlPlane3]) (call/flplane3-values p list))))

(struct FlPlane3 ([flvector : FlVector])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-flplane3)

(define-syntax flplane3? (make-rename-transformer #'FlPlane3?))

;(: call/flplane3-values (All (A) (-> FlPlane3 (-> Flonum Flonum Flonum Flonum A) A)))
(define-syntax-rule (call/flplane3-values p f)
  (let ([vs  (FlPlane3-flvector (ann p FlPlane3))])
    (f (unsafe-flvector-ref vs 0)
       (unsafe-flvector-ref vs 1)
       (unsafe-flvector-ref vs 2)
       (unsafe-flvector-ref vs 3))))

(begin-encourage-inline
  
  (: unsafe-flplane3 (-> Flonum Flonum Flonum Flonum FlPlane3))
  (define (unsafe-flplane3 a b c d)
    (FlPlane3 (flvector a b c d)))
  
  (: make-flplane3 (-> Flonum Flonum Flonum Flonum (U #f FlPlane3)))
  (define (make-flplane3 a b c d)
    (call/fl3plane-normalize a b c d
      (λ ([a : Flonum] [b : Flonum] [c : Flonum] [d : Flonum])
        (if (or (= 0.0 (max (abs a) (abs b) (abs c)))
                (not (and (< -inf.0 (min a b c d))
                          (< (max a b c d) +inf.0))))
            #f
            (unsafe-flplane3 a b c d)))))
  
  (: flplane3 (-> FlV3 Flonum (U #f FlPlane3)))
  (define (flplane3 n d)
    (call/flv3-values n
      (λ (a b c)
        (make-flplane3 a b c d))))
  
  (: flplane3-normal (-> FlPlane3 FlV3))
  (define (flplane3-normal p)
    (call/flplane3-values p
      (λ (a b c d)
        (flv3 a b c))))
  
  (: flplane3-distance (-> FlPlane3 Flonum))
  (define (flplane3-distance p)
    (unsafe-flvector-ref (FlPlane3-flvector p) 3))
  
  )  ; begin-encourage-inline

;; ===================================================================================================
;; FlPlane3 operations

(: flplane3-flip (-> FlPlane3 FlPlane3))
(define (flplane3-flip p)
  (call/flplane3-values p
    (λ (a b c d)
      (unsafe-flplane3 (- a) (- b) (- c) (- d)))))

;(: flplane3-point-dist (-> FlPlane3 FlV3 Flonum))
(define-syntax-rule (flplane3-point-dist p v)
  (call/flplane3-values p
    (λ (a b c d)
      (call/flv3-values v
        (λ (x y z)
          (fl3plane-point-dist a b c d x y z))))))

(: flv3polygon-plane* (-> (Listof FlV3) (U #f FlPlane3)))
(define (flv3polygon-plane* vs)
  (define norm (flv3polygon-normal* vs))
  (and norm (call/flv3-values norm
              (λ (a b c)
                (call/flv3-values (flv3mean* vs)
                  (λ (x y z)
                    (unsafe-flplane3 a b c (- (fl3dot a b c x y z)))))))))

(begin-encourage-inline
  
  (: flv3polygon-plane (-> FlV3 * (U #f FlPlane3)))
  (define flv3polygon-plane
    (case-lambda
      [()  #f]
      [(v1)  #f]
      [(v1 v2)  #f]
      [(v1 v2 v3)
       (define norm (flv3polygon-normal v1 v2 v3))
       (and norm (call/flv3-values norm
                   (λ (a b c)
                     (call/flv3-values v2
                       (λ (x y z)
                         (unsafe-flplane3 a b c (- (fl3dot a b c x y z))))))))]
      [vs
       (flv3polygon-plane* vs)]))
  
  )  ; begin-encourage-inline
