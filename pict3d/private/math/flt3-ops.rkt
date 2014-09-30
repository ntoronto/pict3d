#lang typed/racket/base

(require (for-syntax racket/base)
         racket/flonum
         racket/unsafe/ops
         "../utils.rkt"
         "flt3-unboxed-ops.rkt")

(provide (all-defined-out))

(define-syntax (wrap-unboxed-flvector-fun stx)
  (syntax-case stx ()
    [(_ name arg-size ... #:value)
     (andmap exact-nonnegative-integer? (syntax->datum #'(arg-size ...)))
     (let ([sizes  (syntax->datum #'(arg-size ...))])
       (with-syntax ([((i ...) ...)  (map (λ (n) (generate-temporaries (build-list n values)))
                                          sizes)]
                     [(arg ...)  (generate-temporaries sizes)])
         (quasisyntax/loc stx
           (λ ([arg : FlVector] ...)
             (let-values ([(i ...)  (flvector-values arg arg-size)] ...)
               (name i ... ...))))))]
    [(_ name arg-size ... ret-size)
     (and (andmap exact-nonnegative-integer? (syntax->datum #'(arg-size ...)))
          (exact-nonnegative-integer? (syntax->datum #'ret-size)))
     (let ([sizes  (syntax->datum #'(arg-size ...))]
           [ret-size  (syntax->datum #'ret-size)])
       (with-syntax ([((i ...) ...)  (map (λ (n) (generate-temporaries (build-list n values)))
                                          sizes)]
                     [(j ...)  (generate-temporaries (build-list ret-size values))]
                     [(arg ...)  (generate-temporaries sizes)])
         (quasisyntax/loc stx
           (λ ([arg : FlVector] ...)
             (let-values ([(i ...)  (flvector-values arg arg-size)] ...)
               (let-values ([(j ...)  (name i ... ...)])
                 (flvector j ...)))))))]))

(define flv-linear3-inverse (wrap-unboxed-flvector-fun linear3-inverse 9 9))
(define flv-affine3-inverse (wrap-unboxed-flvector-fun affine3-inverse 12 12))
(define flv-projective3-inverse (wrap-unboxed-flvector-fun projective3-inverse 16 16))

(define flv-linear3-apply (wrap-unboxed-flvector-fun linear3-apply 9 4 4))
(define flv-affine3-apply (wrap-unboxed-flvector-fun affine3-apply 12 4 4))
(define flv-projective3-apply (wrap-unboxed-flvector-fun projective3-apply 16 4 4))

(define flv-linear3-tapply (wrap-unboxed-flvector-fun linear3-tapply 9 4 4))
(define flv-affine3-tapply (wrap-unboxed-flvector-fun affine3-tapply 12 4 4))
(define flv-projective3-tapply (wrap-unboxed-flvector-fun projective3-tapply 16 4 4))

(define flv-linear3-compose (wrap-unboxed-flvector-fun linear3-compose 9 9 9))
(define flv-affine3-compose (wrap-unboxed-flvector-fun affine3-compose 12 12 12))
(define flv-projective3-compose (wrap-unboxed-flvector-fun projective3-compose 16 16 16))

(define flv-linear3-determinant (wrap-unboxed-flvector-fun linear3-determinant 9 #:value))
(define flv-affine3-determinant (wrap-unboxed-flvector-fun affine3-determinant 12 #:value))
(define flv-projective3-determinant (wrap-unboxed-flvector-fun projective3-determinant 16 #:value))

(define flv-linear3-identity
  (flvector 1.0 0.0 0.0
            0.0 1.0 0.0
            0.0 0.0 1.0))

(define flv-affine3-identity
  (flvector 1.0 0.0 0.0 0.0
            0.0 1.0 0.0 0.0
            0.0 0.0 1.0 0.0))

(define flv-projective3-identity
  (flvector 1.0 0.0 0.0 0.0
            0.0 1.0 0.0 0.0
            0.0 0.0 1.0 0.0
            0.0 0.0 0.0 1.0))

(: flv-linear3->affine3 (-> FlVector FlVector))
(define (flv-linear3->affine3 m)
  (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22) (flvector-values m 9))
  (flvector m00 m01 m02 0.0
            m10 m11 m12 0.0
            m20 m21 m22 0.0))

(: flv-linear3->projective3 (-> FlVector FlVector))
(define (flv-linear3->projective3 m)
  (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22) (flvector-values m 9))
  (flvector m00 m01 m02 0.0
            m10 m11 m12 0.0
            m20 m21 m22 0.0
            0.0 0.0 0.0 1.0))

(: flv-affine3->projective3 (-> FlVector FlVector))
(define (flv-affine3->projective3 m)
  (define-values (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23) (flvector-values m 12))
  (flvector m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            0.0 0.0 0.0 1.0))

(define flv-linear3-consistent? (λ ([m : FlVector]) (> (flv-linear3-determinant m) 0.0)))
(define flv-affine3-consistent? (λ ([m : FlVector]) (> (flv-affine3-determinant m) 0.0)))
(define flv-projective3-consistent? (λ ([m : FlVector]) (> (flv-projective3-determinant m) 0.0)))
