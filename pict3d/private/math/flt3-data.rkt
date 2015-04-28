#lang typed/racket/base

(require racket/unsafe/ops
         racket/flonum
         (except-in typed/opengl/ffi cast ->)
         "../ffi.rkt")

(provide (all-defined-out))

#;
(: call/flv12-values (All (A) (-> FlVector
                                  (-> Flonum Flonum Flonum Flonum
                                      Flonum Flonum Flonum Flonum
                                      Flonum Flonum Flonum Flonum
                                      A)
                                  A)))
(define-syntax-rule (call/flv12-values vs-stx f)
  (let ([vs  (ann vs-stx FlVector)])
    (f (unsafe-flvector-ref vs  0)
       (unsafe-flvector-ref vs  1)
       (unsafe-flvector-ref vs  2)
       (unsafe-flvector-ref vs  3)
       (unsafe-flvector-ref vs  4)
       (unsafe-flvector-ref vs  5)
       (unsafe-flvector-ref vs  6)
       (unsafe-flvector-ref vs  7)
       (unsafe-flvector-ref vs  8)
       (unsafe-flvector-ref vs  9)
       (unsafe-flvector-ref vs 10)
       (unsafe-flvector-ref vs 11))))

(: flv12->f32vector (-> FlVector F32Vector))
(define (flv12->f32vector vs)
  (define vec (make-f32vector 12))
  (define ptr (f32vector->cpointer vec))
  (call/flv12-values vs
    (λ (v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11)
      (ptr-set! ptr _float  0 v0)
      (ptr-set! ptr _float  1 v1)
      (ptr-set! ptr _float  2 v2)
      (ptr-set! ptr _float  3 v3)
      (ptr-set! ptr _float  4 v4)
      (ptr-set! ptr _float  5 v5)
      (ptr-set! ptr _float  6 v6)
      (ptr-set! ptr _float  7 v7)
      (ptr-set! ptr _float  8 v8)
      (ptr-set! ptr _float  9 v9)
      (ptr-set! ptr _float 10 v10)
      (ptr-set! ptr _float 11 v11)))
  vec)

#;
(: call/flv16-values (All (A) (-> FlVector
                                  (-> Flonum Flonum Flonum Flonum
                                      Flonum Flonum Flonum Flonum
                                      Flonum Flonum Flonum Flonum
                                      Flonum Flonum Flonum Flonum
                                      A)
                                  A)))
(define-syntax-rule (call/flv16-values vs-stx f)
  (let ([vs  (ann vs-stx FlVector)])
    (f (unsafe-flvector-ref vs  0)
       (unsafe-flvector-ref vs  1)
       (unsafe-flvector-ref vs  2)
       (unsafe-flvector-ref vs  3)
       (unsafe-flvector-ref vs  4)
       (unsafe-flvector-ref vs  5)
       (unsafe-flvector-ref vs  6)
       (unsafe-flvector-ref vs  7)
       (unsafe-flvector-ref vs  8)
       (unsafe-flvector-ref vs  9)
       (unsafe-flvector-ref vs 10)
       (unsafe-flvector-ref vs 11)
       (unsafe-flvector-ref vs 12)
       (unsafe-flvector-ref vs 13)
       (unsafe-flvector-ref vs 14)
       (unsafe-flvector-ref vs 15))))

(: flv16->f32vector (-> FlVector F32Vector))
(define (flv16->f32vector vs)
  (define vec (make-f32vector 16))
  (define ptr (f32vector->cpointer vec))
  (call/flv16-values vs
    (λ (v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
      (ptr-set! ptr _float  0 v0)
      (ptr-set! ptr _float  1 v1)
      (ptr-set! ptr _float  2 v2)
      (ptr-set! ptr _float  3 v3)
      (ptr-set! ptr _float  4 v4)
      (ptr-set! ptr _float  5 v5)
      (ptr-set! ptr _float  6 v6)
      (ptr-set! ptr _float  7 v7)
      (ptr-set! ptr _float  8 v8)
      (ptr-set! ptr _float  9 v9)
      (ptr-set! ptr _float 10 v10)
      (ptr-set! ptr _float 11 v11)
      (ptr-set! ptr _float 12 v12)
      (ptr-set! ptr _float 13 v13)
      (ptr-set! ptr _float 14 v14)
      (ptr-set! ptr _float 15 v15)))
  vec)
