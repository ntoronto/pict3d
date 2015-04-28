#lang racket/base

;; BEWARE: These macros don't rename arguments and may use some of them more than once

;; TODO: Increase precision of affine inverse origin column
;; TODO: If projective3-inverse is ever used, increase final computations' precisions

(require "fl2.rkt"
         "fl3.rkt"
         "fl4.rkt")

(provide (all-defined-out))

(define-syntax-rule (det2 a b c d)
  (fl2cross a b c d))

(define-syntax-rule (v3dot x1 y1 z1 x2 y2 z2)
  (fl3dot x1 y1 z1 x2 y2 z2))

(define-syntax-rule (v4dot m0 m1 m2 m3 v0 v1 v2 v3)
  (fl4dot m0 m1 m2 m3 v0 v1 v2 v3))

;; ===================================================================================================
;; Inversion

(define-syntax-rule (call/linear3-inverse*det
                      m00 m01 m02
                      m10 m11 m12
                      m20 m21 m22
                      k)
  (let ([s0  (det2 m01 m02 m11 m12)]
        [s1  (det2 m02 m00 m12 m10)]
        [s2  (det2 m00 m01 m10 m11)])
    (let ([det  (v3dot s0 s1 s2 m20 m21 m22)])
      (k (det2 m11 m12 m21 m22) (det2 m02 m01 m22 m21) s0
         (det2 m12 m10 m22 m20) (det2 m00 m02 m20 m22) s1
         (det2 m10 m11 m20 m21) (det2 m01 m00 m21 m20) s2
         det))))

(define-syntax-rule (call/affine3-inverse*det
                      m00 m01 m02 m03
                      m10 m11 m12 m13
                      m20 m21 m22 m23
                      k)
  (call/linear3-inverse*det m00 m01 m02 m10 m11 m12 m20 m21 m22
    (λ (n00 n01 n02 n10 n11 n12 n20 n21 n22 det)
      (k n00 n01 n02 (- (v3dot n00 n01 n02 m03 m13 m23))
         n10 n11 n12 (- (v3dot n10 n11 n12 m03 m13 m23))
         n20 n21 n22 (- (v3dot n20 n21 n22 m03 m13 m23))
         det))))

#|
;; Code derived from http://www.geometrictools.com/Documentation/LaplaceExpansionTheorem.pdf
(define-syntax-rule (projective3-inverse m00 m01 m02 m03
                                         m10 m11 m12 m13
                                         m20 m21 m22 m23
                                         m30 m31 m32 m33)
  (let ([s0  (det2 m00 m01 m10 m11)]
        [c5  (det2 m22 m23 m32 m33)]
        [s1  (det2 m00 m02 m10 m12)]
        [c4  (det2 m21 m23 m31 m33)]
        [s2  (det2 m00 m03 m10 m13)]
        [c3  (det2 m21 m22 m31 m32)]
        [s3  (det2 m01 m02 m11 m12)]
        [c2  (det2 m20 m23 m30 m33)]
        [s4  (det2 m01 m03 m11 m13)]
        [c1  (det2 m20 m22 m30 m32)]
        [s5  (det2 m02 m03 m12 m13)]
        [c0  (det2 m20 m21 m30 m31)])
    (let ([det  (+ (* s0 c5) (- (* s4 c1)) (* s2 c3) (* s3 c2) (- (* s1 c4)) (* s5 c0))])
      (values
       ;; Row 0
       (/ (+    (* m11 c5)  (- (* m12 c4))    (* m13 c3))  det)
       (/ (+ (- (* m01 c5))    (* m02 c4)  (- (* m03 c3))) det)
       (/ (+    (* m31 s5)  (- (* m32 s4))    (* m33 s3))  det)
       (/ (+ (- (* m21 s5))    (* m22 s4)  (- (* m23 s3))) det)
       ;; Row 1
       (/ (+ (- (* m10 c5))    (* m12 c2)  (- (* m13 c1))) det)
       (/ (+    (* m00 c5)  (- (* m02 c2))    (* m03 c1))  det)
       (/ (+ (- (* m30 s5))    (* m32 s2)  (- (* m33 s1))) det)
       (/ (+    (* m20 s5)  (- (* m22 s2))    (* m23 s1))  det)
       ;; Row 2
       (/ (+    (* m10 c4)  (- (* m11 c2))    (* m13 c0))  det)
       (/ (+ (- (* m00 c4))    (* m01 c2)  (- (* m03 c0))) det)
       (/ (+    (* m30 s4)  (- (* m31 s2))    (* m33 s0))  det)
       (/ (+ (- (* m20 s4))    (* m21 s2)  (- (* m23 s0))) det)
       ;; Row 3
       (/ (+ (- (* m10 c3))    (* m11 c1)  (- (* m12 c0))) det)
       (/ (+    (* m00 c3)  (- (* m01 c1))    (* m02 c0))  det)
       (/ (+ (- (* m30 s3))    (* m31 s1)  (- (* m32 s0))) det)
       (/ (+    (* m20 s3)  (- (* m21 s1))    (* m22 s0))  det)))))
|#

;; ===================================================================================================
;; Application

(define-syntax-rule (call/linear3-apply
                      m00 m01 m02
                      m10 m11 m12
                      m20 m21 m22
                      x y z
                      k)
  (k (v3dot m00 m01 m02 x y z)
     (v3dot m10 m11 m12 x y z)
     (v3dot m20 m21 m22 x y z)))

(define-syntax-rule (call/affine3-apply 
                      m00 m01 m02 m03
                      m10 m11 m12 m13
                      m20 m21 m22 m23
                      x y z w
                      k)
  (k (v4dot m00 m01 m02 m03 x y z w)
     (v4dot m10 m11 m12 m13 x y z w)
     (v4dot m20 m21 m22 m23 x y z w)
     w))

(define-syntax-rule (call/projective3-apply
                      m00 m01 m02 m03
                      m10 m11 m12 m13
                      m20 m21 m22 m23
                      m30 m31 m32 m33
                      x y z w
                      k)
  (k (v4dot m00 m01 m02 m03 x y z w)
     (v4dot m10 m11 m12 m13 x y z w)
     (v4dot m20 m21 m22 m23 x y z w)
     (v4dot m30 m31 m32 m33 x y z w)))

;; ===================================================================================================
;; Transpose application

(define-syntax-rule (call/affine3-tapply
                      m00 m01 m02 m03
                      m10 m11 m12 m13
                      m20 m21 m22 m23
                      x y z w
                      k)
  (k (v3dot m00 m10 m20 x y z)
     (v3dot m01 m11 m21 x y z)
     (v3dot m02 m12 m22 x y z)
     (+ (v3dot m03 m13 m23 x y z) w)))

(define-syntax-rule (call/projective3-tapply
                      m00 m01 m02 m03
                      m10 m11 m12 m13
                      m20 m21 m22 m23
                      m30 m31 m32 m33
                      x y z w
                      k)
  (call/projective3-apply
    m00 m10 m20 m30
    m01 m11 m21 m31
    m02 m12 m22 m32
    m03 m13 m23 m33
    x y z w
    k))

;; ===================================================================================================
;; Composition

(define-syntax-rule (call/affine3-compose
                      m00 m01 m02 m03
                      m10 m11 m12 m13
                      m20 m21 m22 m23
                      n00 n01 n02 n03
                      n10 n11 n12 n13
                      n20 n21 n22 n23
                      k)
  (call/linear3-apply m00 m01 m02 m10 m11 m12 m20 m21 m22 n00 n10 n20
    (λ (a00 a10 a20)
      (call/linear3-apply m00 m01 m02 m10 m11 m12 m20 m21 m22 n01 n11 n21
        (λ (a01 a11 a21)
          (call/linear3-apply m00 m01 m02 m10 m11 m12 m20 m21 m22 n02 n12 n22
            (λ (a02 a12 a22)
              (call/affine3-apply m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 n03 n13 n23 1.0
                (λ (a03 a13 a23 _)
                  (k a00 a01 a02 a03
                     a10 a11 a12 a13
                     a20 a21 a22 a23))))))))))

(define-syntax-rule (call/projective3-compose
                      m00 m01 m02 m03
                      m10 m11 m12 m13
                      m20 m21 m22 m23
                      m30 m31 m32 m33
                      n00 n01 n02 n03
                      n10 n11 n12 n13
                      n20 n21 n22 n23
                      n30 n31 n32 n33
                      k)
  (call/projective3-apply
    m00 m01 m02 m03
    m10 m11 m12 m13
    m20 m21 m22 m23
    m30 m31 m32 m33
    n00 n10 n20 n30
    (λ (a00 a10 a20 a30)
      (call/projective3-apply
        m00 m01 m02 m03
        m10 m11 m12 m13
        m20 m21 m22 m23
        m30 m31 m32 m33
        n01 n11 n21 n31
        (λ (a01 a11 a21 a31)
          (call/projective3-apply
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            m30 m31 m32 m33
            n02 n12 n22 n32
            (λ (a02 a12 a22 a32)
              (call/projective3-apply
                m00 m01 m02 m03
                m10 m11 m12 m13
                m20 m21 m22 m23
                m30 m31 m32 m33
                n03 n13 n23 n33
                (λ (a03 a13 a23 a33)
                  (k a00 a01 a02 a03
                     a10 a11 a12 a13
                     a20 a21 a22 a23
                     a30 a31 a32 a33))))))))))
