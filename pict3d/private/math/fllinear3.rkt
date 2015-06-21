#lang typed/racket/base

(require (for-syntax racket/base)
         math/flonum
         "flv3.rkt"
         "flplane3.rkt"
         "flv4.rkt"
         "flt3-data.rkt"
         "flt3-unboxed-ops.rkt")

(provide (except-out
          (all-defined-out)
          ;; This isn't safe because it doesn't rename arguments
          fllinear3))

;; ===================================================================================================
;; Linear transforms

(struct FlLinear3 ([forward : FlVector]
                   [inverse : FlVector]
                   [forward-det : Flonum]
                   [inverse-den : Flonum])
  #:transparent)

(define-syntax fllinear3? (make-rename-transformer #'FlLinear3?))

(define-syntax-rule (call/fllinear3-forward t f)
  (call/flv9-values (FlLinear3-forward t) f))

(define-syntax-rule (call/fllinear3-inverse t f)
  (call/flv9+1-values (FlLinear3-inverse t) (FlLinear3-inverse-den t) f))

(define-syntax fllinear3-determinant (make-rename-transformer #'FlLinear3-forward-det))

;; ===================================================================================================

(define-syntax-rule
  (fllinear3 m00 m01 m02
             m10 m11 m12
             m20 m21 m22
             n00 n01 n02
             n10 n11 n12
             n20 n21 n22
             det-stx den-stx)
  (let ([det : Flonum  det-stx]
        [den : Flonum  den-stx])
    (if (< (max (abs det) (abs den)) +inf.0)
        (FlLinear3
         (check-flvector 'fllinear3 (flvector m00 m01 m02 m10 m11 m12 m20 m21 m22))
         (check-flvector 'fllinear3 (flvector n00 n01 n02 n10 n11 n12 n20 n21 n22))
         det den)
        (error 'fllinear3 "expected rational determinant and denominator; given ~e ~e" det den))))

(define identity-fllinear3
  (fllinear3 1.0 0.0 0.0
             0.0 1.0 0.0
             0.0 0.0 1.0
             1.0 0.0 0.0
             0.0 1.0 0.0
             0.0 0.0 1.0
             1.0 1.0))

;(: identity-fllinear3? (-> Any Boolean : FlLinear3))
(define (identity-fllinear3? t)
  (eq? t identity-fllinear3))

(: entries->fllinear3 (-> Flonum Flonum Flonum
                          Flonum Flonum Flonum
                          Flonum Flonum Flonum
                          FlLinear3))
(define (entries->fllinear3 m00 m01 m02 m10 m11 m12 m20 m21 m22)
  (call/linear3-inverse*det m00 m01 m02 m10 m11 m12 m20 m21 m22
    (λ (n00 n01 n02 n10 n11 n12 n20 n21 n22 det)
      (define a00 (/ n00 det))
      (define a01 (/ n01 det))
      (define a02 (/ n02 det))
      (define a10 (/ n10 det))
      (define a11 (/ n11 det))
      (define a12 (/ n12 det))
      (define a20 (/ n20 det))
      (define a21 (/ n21 det))
      (define a22 (/ n22 det))
      (if (< (max (abs a00) (abs a01) (abs a02)
                  (abs a10) (abs a11) (abs a12)
                  (abs a20) (abs a21) (abs a22))
             +inf.0)
          (fllinear3 m00 m01 m02 m10 m11 m12 m20 m21 m22
                     a00 a01 a02 a10 a11 a12 a20 a21 a22
                     det 1.0)
          (fllinear3 m00 m01 m02 m10 m11 m12 m20 m21 m22
                     n00 n01 n02 n10 n11 n12 n20 n21 n22
                     det det)))))
  

(: cols->fllinear3 (-> FlV3 FlV3 FlV3 FlLinear3))
(define (cols->fllinear3 x y z)
  (call/flv3-values x
    (λ (m00 m10 m20)
      (call/flv3-values y
        (λ (m01 m11 m21)
          (call/flv3-values z
            (λ (m02 m12 m22)
              (entries->fllinear3 m00 m01 m02 m10 m11 m12 m20 m21 m22))))))))

(: fllinear3->cols (-> FlLinear3 (Values FlV3 FlV3 FlV3)))
(define (fllinear3->cols t)
  (call/fllinear3-forward t
    (λ (m00 m01 m02 m10 m11 m12 m20 m21 m22)
      (values (flv3 m00 m10 m20)
              (flv3 m01 m11 m21)
              (flv3 m02 m12 m22)))))

;; ===================================================================================================
;; Handedness consistency

(: fllinear3-cols-consistent? (-> FlLinear3  Boolean))
;; Consistency determination for rank-deficient linear transforms
(define (fllinear3-cols-consistent? t)
  (define-values (dx dy dz) (fllinear3->cols t))
  (or (let ([nx  (fllinear3-apply/norm t +x-flv3)])
        (and nx (> (flv3dot nx (flv3cross dy dz)) 0.0)))
      (let ([ny  (fllinear3-apply/norm t +y-flv3)])
        (and ny (> (flv3dot ny (flv3cross dz dx)) 0.0)))
      (let ([nz  (fllinear3-apply/norm t +z-flv3)])
        (and nz (> (flv3dot nz (flv3cross dx dy)) 0.0)))))

(: fllinear3-consistent? (-> FlLinear3 Boolean))
(define (fllinear3-consistent? t)
  (define det (FlLinear3-forward-det t))
  (cond [(> det 0.0)  #t]
        [(< det 0.0)  #f]
        [else  (fllinear3-cols-consistent? t)]))

;; ===================================================================================================
;; Composition

(: fllinear3-compose (-> FlLinear3 FlLinear3 FlLinear3))
(define (fllinear3-compose t1 t2)
  (define det1 (FlLinear3-forward-det t1))
  (define det2 (FlLinear3-forward-det t2))
  (call/fllinear3-forward t1
    (λ (s00 s01 s02 s10 s11 s12 s20 s21 s22)
      (call/fllinear3-forward t2
        (λ (n00 n01 n02 n10 n11 n12 n20 n21 n22)
          (call/linear3-compose
            s00 s01 s02
            s10 s11 s12
            s20 s21 s22
            n00 n01 n02
            n10 n11 n12
            n20 n21 n22
            (λ (m00 m01 m02 m10 m11 m12 m20 m21 m22)
              (call/fllinear3-inverse t1
                (λ (s00 s01 s02 s10 s11 s12 s20 s21 s22 den1)
                  (call/fllinear3-inverse t2
                    (λ (n00 n01 n02 n10 n11 n12 n20 n21 n22 den2)
                      (call/linear3-compose
                        n00 n01 n02
                        n10 n11 n12
                        n20 n21 n22
                        s00 s01 s02
                        s10 s11 s12
                        s20 s21 s22
                        (λ (n00 n01 n02 n10 n11 n12 n20 n21 n22)
                          (fllinear3 m00 m01 m02 m10 m11 m12 m20 m21 m22
                                     n00 n01 n02 n10 n11 n12 n20 n21 n22
                                     (* det1 det2)
                                     (* den1 den2)))))))))))))))

;; ===================================================================================================
;; Transformation constructors

(: scale-x-flt3 (-> Flonum FlLinear3))
(define (scale-x-flt3 v)
  (define 1/v (/ v))
  (let-values ([(1/v one den)  (if (< (abs 1/v) +inf.0) (values 1/v 1.0 1.0) (values 1.0 v v))])
    (fllinear3  v   0.0  0.0
               0.0  1.0  0.0
               0.0  0.0  1.0
               1/v  0.0  0.0
               0.0  one  0.0
               0.0  0.0  one
               v den)))

(: scale-y-flt3 (-> Flonum FlLinear3))
(define (scale-y-flt3 v)
  (define 1/v (/ v))
  (let-values ([(1/v one den)  (if (< (abs 1/v) +inf.0) (values 1/v 1.0 1.0) (values 1.0 v v))])
    (fllinear3 1.0  0.0  0.0
               0.0   v   0.0
               0.0  0.0  1.0
               one  0.0  0.0
               0.0  1/v  0.0
               0.0  0.0  one
               v den)))

(: scale-z-flt3 (-> Flonum FlLinear3))
(define (scale-z-flt3 v)
  (define 1/v (/ v))
  (let-values ([(1/v one den)  (if (< (abs 1/v) +inf.0) (values 1/v 1.0 1.0) (values 1.0 v v))])
    (fllinear3 1.0  0.0  0.0
               0.0  1.0  0.0
               0.0  0.0   v 
               one  0.0  0.0
               0.0  one  0.0
               0.0  0.0  1/v
               v den)))

(: uniform-scale-flt3 (-> Flonum FlLinear3))
(define (uniform-scale-flt3 v)
  (define det (* v v v))
  (define 1/v (/ v))
  (let-values ([(1/v den)  (if (and (< (abs 1/v) +inf.0) (> (abs det) 0.0))
                               (values 1/v 1.0)
                               (values 1.0 v))])
    (fllinear3  v   0.0  0.0
               0.0   v   0.0
               0.0  0.0   v 
               1/v  0.0  0.0
               0.0  1/v  0.0
               0.0  0.0  1/v
               det den)))

(: axial-scale-flt3 (-> Flonum Flonum Flonum FlLinear3))
(define (axial-scale-flt3 x y z)
  (define det (* x y z))
  (define 1/x (/ x))
  (define 1/y (/ y))
  (define 1/z (/ z))
  (if (and (< (max (abs 1/x) (abs 1/y) (abs 1/z)) +inf.0) (> (abs det) 0.0))
      (fllinear3  x   0.0  0.0
                 0.0   y   0.0
                 0.0  0.0   z 
                 1/x  0.0  0.0
                 0.0  1/y  0.0
                 0.0  0.0  1/z
                 det 1.0)
      ;; This is probably a lost cause...
      (fllinear3-compose
       (scale-z-flt3 z)
       (fllinear3-compose
        (scale-y-flt3 y)
        (scale-x-flt3 x)))))

(: scale-flt3 (-> (U Flonum FlV3) FlLinear3))
(define (scale-flt3 v)
  (if (flonum? v)
      (uniform-scale-flt3 v)
      (call/flv3-values v axial-scale-flt3)))

(: rotate-x-flt3 (-> Flonum FlLinear3))
(define (rotate-x-flt3 rho)
  (define c (flcos rho))
  (define s (flsin rho))
  (define -s (- s))
  (fllinear3 1.0  0.0  0.0 
             0.0   c   -s
             0.0   s    c
             1.0  0.0  0.0
             0.0   c    s
             0.0  -s    c
             1.0 1.0))

(: rotate-y-flt3 (-> Flonum FlLinear3))
(define (rotate-y-flt3 phi)
  (define c (flcos phi))
  (define s (flsin phi))
  (define -s (- s))
  (fllinear3  c   0.0   s 
             0.0  1.0  0.0
             -s   0.0   c 
              c   0.0  -s
             0.0  1.0  0.0
              s   0.0   c 
             1.0 1.0))

(: rotate-z-flt3 (-> Flonum FlLinear3))
(define (rotate-z-flt3 theta)
  (define c (flcos theta))
  (define s (flsin theta))
  (define -s (- s))
  (fllinear3  c   -s   0.0
              s    c   0.0
             0.0  0.0  1.0
              c    s   0.0
             -s    c   0.0
             0.0  0.0  1.0
             1.0 1.0))

(: rotate-flt3 (-> FlV3 Flonum FlLinear3))
(define (rotate-flt3 axis angle)
  (call/flv3-values axis
    (λ (x y z)
      (define c (flcos angle))
      (define s (flsin angle))
      (define t (- 1.0 c))
      (fllinear3 (+ (* t x x) c)        (- (* t x y) (* z s))  (+ (* t x z) (* y s))
                 (+ (* t x y) (* z s))  (+ (* t y y) c)        (- (* t y z) (* x s))
                 (- (* t x z) (* y s))  (+ (* t y z) (* x s))  (+ (* t z z) c)      
                 (+ (* t x x) c)        (+ (* t x y) (* z s))  (- (* t x z) (* y s))
                 (- (* t x y) (* z s))  (+ (* t y y) c)        (+ (* t y z) (* x s))
                 (+ (* t x z) (* y s))  (- (* t y z) (* x s))  (+ (* t z z) c)      
                 1.0 1.0))))

;; ===================================================================================================
;; Inversion

(: fllinear3-inverse (-> FlLinear3 (U #f FlLinear3)))
(define (fllinear3-inverse t)
  (define 1/det (/ (FlLinear3-forward-det t)))
  (cond
    [(not (< (abs 1/det) +inf.0))  #f]
    [(= (FlLinear3-inverse-den t) 1.0)
     ;; Common case: clearly invertible
     (FlLinear3 (FlLinear3-inverse t)
                (FlLinear3-forward t)
                1/det 1.0)]
    [else
     (call/fllinear3-inverse t
       (λ (n00 n01 n02 n10 n11 n12 n20 n21 n22 den)
         (define a00 (/ n00 den))
         (define a01 (/ n01 den))
         (define a02 (/ n02 den))
         (define a10 (/ n10 den))
         (define a11 (/ n11 den))
         (define a12 (/ n12 den))
         (define a20 (/ n20 den))
         (define a21 (/ n21 den))
         (define a22 (/ n22 den))
         (if (< (max (abs a00) (abs a01) (abs a02)
                     (abs a10) (abs a11) (abs a12)
                     (abs a20) (abs a21) (abs a22))
                +inf.0)
             ;; This could happen...
             (FlLinear3 (flvector a00 a01 a02 a10 a11 a12 a20 a21 a22)
                        (FlLinear3-forward t)
                        1/det 1.0)
             #f)))]))

;; ===================================================================================================
;; Application

(: fllinear3-apply (-> FlLinear3 FlV4 FlV4))
(define (fllinear3-apply t v)
  (call/fllinear3-forward t
    (λ (m00 m01 m02 m10 m11 m12 m20 m21 m22)
      (call/flv4-values v
        (λ (s0 s1 s2 s3)
          (call/affine3-apply
            m00 m01 m02 0.0
            m10 m11 m12 0.0
            m20 m21 m22 0.0
            s0 s1 s2 s3
            flv4))))))

(: fllinear3-apply/pos (-> FlLinear3 FlV3 FlV3))
;; Application to positions: set w = 1.0 and divide by w'
(define (fllinear3-apply/pos t v)
  (call/fllinear3-forward t
    (λ (m00 m01 m02 m10 m11 m12 m20 m21 m22)
      (call/flv3-values v
        (λ (s0 s1 s2)
          (call/linear3-apply
            m00 m01 m02
            m10 m11 m12
            m20 m21 m22
            s0 s1 s2
            ;; No need to divide because w' = 1
            flv3))))))

(: fllinear3-apply/dir (-> FlLinear3 FlV3 FlV3))
;; Application to directions: set w = 0.0 and ignore w'
(define (fllinear3-apply/dir t v)
  (call/fllinear3-forward t
    (λ (m00 m01 m02 m10 m11 m12 m20 m21 m22)
      (call/flv3-values v
        (λ (s0 s1 s2)
          (call/linear3-apply
            m00 m01 m02
            m10 m11 m12
            m20 m21 m22
            s0 s1 s2
            flv3))))))

(: fllinear3-apply/norm (-> FlLinear3 FlV3 (U #f FlV3)))
;; Application to normals: set w = 0.0, apply inverse transpose, normalize
(define (fllinear3-apply/norm t v)
  (call/fllinear3-inverse t
    (λ (m00 m01 m02 m10 m11 m12 m20 m21 m22 _den)  ; normalization makes den unnecessary
      (call/flv3-values v
        (λ (s0 s1 s2)
          (call/linear3-tapply
            m00 m01 m02
            m10 m11 m12
            m20 m21 m22
            s0 s1 s2
            flnorm3))))))

(: fllinear3-apply/plane (-> FlLinear3 FlPlane3 (U #f FlPlane3)))
;; Application to planes: apply inverse transpose, normalize
(define (fllinear3-apply/plane t p)
  (call/fllinear3-inverse t
    (λ (m00 m01 m02 m10 m11 m12 m20 m21 m22 _den)  ; normalization makes den unnecessary
      (call/flplane3-values p
        (λ (s0 s1 s2 s3)
          (call/affine3-tapply
            m00 m01 m02 0.0
            m10 m11 m12 0.0
            m20 m21 m22 0.0
            s0 s1 s2 s3
            make-flplane3))))))
