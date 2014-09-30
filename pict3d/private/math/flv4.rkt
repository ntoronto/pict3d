#lang typed/racket/base

;; Homogeneous coordinate vectors

(require racket/flonum
         racket/unsafe/ops
         "flv3.rkt")

(provide (all-defined-out))

(: flv4? (-> FlVector Boolean))
(define (flv4? v) (= 4 (flvector-length v)))

;(: flv4-values (-> FlVector (Values Flonum Flonum Flonum Flonum)))
(define-syntax-rule (flv4-values v-stx)
  (let ([v : FlVector  v-stx])
    (unless (= 4 (flvector-length v))
      (raise-type-error 'flv4-values "length-4 FlVector" v))
    (values (unsafe-flvector-ref v 0)
            (unsafe-flvector-ref v 1)
            (unsafe-flvector-ref v 2)
            (unsafe-flvector-ref v 3))))

(: pos->flv4 (-> FlVector FlVector))
(define (pos->flv4 v)
  (define-values (x y z) (flv3-values v))
  (flvector x y z 1.0))

(: norm->flv4 (-> FlVector FlVector))
(define (norm->flv4 v)
  (define-values (x y z) (flv3-values v))
  (flvector x y z 0.0))

(: flplane3->flv4 (-> FlPlane3 FlVector))
(define (flplane3->flv4 p)
  (define-values (x y z) (flv3-values (flplane3-normal p)))
  (define d (flplane3-distance p))
  (flvector x y z d))

(: flv4->pos (-> FlVector FlVector))
(define (flv4->pos v)
  (define-values (x y z w) (flv4-values v))
  (flvector (/ x w) (/ y w) (/ z w)))

(: flv4->norm (-> FlVector FlVector))
(define (flv4->norm v)
  (define-values (x y z _) (flv4-values v))
  (flvector x y z))

(: flv4->flplane3 (-> FlVector (U #f FlPlane3)))
(define (flv4->flplane3 v)
  (define-values (x y z w) (flv4-values v))
  (flplane3 (flvector x y z) w))
