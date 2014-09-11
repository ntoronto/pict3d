#lang typed/racket/base

;; Joined line segments

(require (for-syntax racket/base)
         racket/list
         racket/match
         "../math/flaabb3.rkt"
         "../utils.rkt")

(provide FlLines3 fllines3? fllines3-data fllines3-vertices fllines3-segments
         (rename-out [fllines3* fllines3])
         fllines3-length
         fllines3-aabb)

(struct fllines3 ([data : Any]
                  [vertices : (Vectorof FlVector)]
                  [segments : (Vectorof Any)])
  #:transparent)

(define-type FlLines3 fllines3)

(: make-fllines3 (-> Any (Vectorof FlVector) (Vectorof Any) fllines3))
(define (make-fllines3 data vs ss)
  (define n (vector-length vs))
  (cond [(< n 2)
         (raise-type-error 'fllines3 "vector of length at least 2" 1 data vs ss)]
        [(= (- n 1) (vector-length ss))
         (fllines3 data vs ss)]
        [else
         (raise-type-error 'fllines3 (format "vector of length ~a" (- n 1)) 2 data vs ss)]))

(define-match-expander fllines3*
  (λ (stx)
    (syntax-case stx ()
      [(_ e1 e2 e3)  (syntax/loc stx (fllines3 e1 e2 e3))]))
  (λ (stx)
    (syntax-case stx ()
      [(_ . args)  (syntax/loc stx (make-fllines3 . args))]
      [_  (syntax/loc stx make-fllines3)])))

(: fllines3-length (-> fllines3 Index))
(define (fllines3-length p)
  (vector-length (fllines3-vertices p)))

(: fllines3-aabb (-> fllines3 FlAABB3))
(define (fllines3-aabb p)
  (assert (flv3aabb (fllines3-vertices p)) values))
