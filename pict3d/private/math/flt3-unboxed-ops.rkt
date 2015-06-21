#lang typed/racket/base

;; BEWARE: These macros don't rename arguments and may use some of them more than once

;; TODO: If projective3-inverse is ever used, increase final computations' precisions

(require math/flonum
         "fl2.rkt"
         "fl3.rkt"
         "fl4.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Inversion

(define-syntax-rule (fast-fl2- x.hi x.lo y.hi y.lo)
  (let-values ([(e.hi e.lo)  (fast-fl-/error x.hi y.hi)])
    (fast-mono-fl+/error e.hi (- (+ e.lo x.lo) y.lo))))

;(: fast-fl2* (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum)))
(define-syntax-rule (fast-fl2* x2-stx x1 y2-stx y1)
  (let* ([x2 : Flonum  x2-stx]
         [y2 : Flonum  y2-stx]
         [up  (fl* x2 (fl+ 1.0 (flexpt 2.0 27.0)))]
         [vp  (fl* y2 (fl+ 1.0 (flexpt 2.0 27.0)))]
         [u1  (fl+ (fl- x2 up) up)]
         [v1  (fl+ (fl- y2 vp) vp)]
         [u2  (fl- x2 u1)]
         [v2  (fl- y2 v1)]
         [m2  (fl* x2 y2)]
         [m1  (fl+ (fl+ (fl+ (fl+ (fl+ (fl- (fl* u1 v1) m2)
                                       (fl* u1 v2))
                                  (fl* u2 v1))
                             (fl* u2 v2))
                        (fl* x2 (ann y1 Flonum)))
                   (fl* (ann x1 Flonum) y2))]
         [z2  (fl+ m2 m1)])
    (values z2 (fl+ (fl- m2 z2) m1))))

;; Less than 1.25 ulp error and about 3x slower than the direct implementation in tests
;; Concretely, this tends to compute about 7 million determinants per second
(define-syntax-rule (linear3-det m00 m01 m02
                                 m10 m11 m12
                                 m20 m21 m22)
  #;; Direct implementation:
  (+ (- (* m01 m12 m20) (* m02 m11 m20))
     (- (* m02 m10 m21) (* m00 m12 m21))
     (- (* m00 m11 m22) (* m01 m10 m22)))
  ;; Accurate implementation:
  (let-values ([(m01m12.hi m01m12.lo)  (fast-fl*/error m01 m12)]
               [(m11m02.hi m11m02.lo)  (fast-fl*/error m11 m02)]
               [(m02m10.hi m02m10.lo)  (fast-fl*/error m02 m10)]
               [(m12m00.hi m12m00.lo)  (fast-fl*/error m12 m00)]
               [(m00m11.hi m00m11.lo)  (fast-fl*/error m00 m11)]
               [(m10m01.hi m10m01.lo)  (fast-fl*/error m10 m01)])
    (let*-values ([(x.hi x.lo)  (fast-fl2- m01m12.hi m01m12.lo m11m02.hi m11m02.lo)]
                  [(y.hi y.lo)  (fast-fl2- m02m10.hi m02m10.lo m12m00.hi m12m00.lo)]
                  [(z.hi z.lo)  (fast-fl2- m00m11.hi m00m11.lo m10m01.hi m10m01.lo)]
                  [(x.hi x.lo)  (fast-fl2* x.hi x.lo m20 0.0)]
                  [(y.hi y.lo)  (fast-fl2* y.hi y.lo m21 0.0)]
                  [(z.hi z.lo)  (fast-fl2* z.hi z.lo m22 0.0)]
                  [(e.hi e.lo)  (fast-fl+/error x.hi y.hi)]
                  [(w.hi w.lo)  (fast-mono-fl+/error e.hi (+ e.lo y.lo x.lo))])
      (+ w.hi z.hi w.lo z.lo))))

;; Less than 4.0 ulp error, 2.2 million inversions per second
(define-syntax-rule (call/linear3-inverse*det
                      m00 m01 m02
                      m10 m11 m12
                      m20 m21 m22
                      k)
  (let-values ([(m01m12.hi m01m12.lo)  (fast-fl*/error m01 m12)]
               [(m11m02.hi m11m02.lo)  (fast-fl*/error m11 m02)]
               [(m02m10.hi m02m10.lo)  (fast-fl*/error m02 m10)]
               [(m12m00.hi m12m00.lo)  (fast-fl*/error m12 m00)]
               [(m00m11.hi m00m11.lo)  (fast-fl*/error m00 m11)]
               [(m10m01.hi m10m01.lo)  (fast-fl*/error m10 m01)])
    (let*-values ([(s0.hi s0.lo)  (fast-fl2- m01m12.hi m01m12.lo m11m02.hi m11m02.lo)]
                  [(s1.hi s1.lo)  (fast-fl2- m02m10.hi m02m10.lo m12m00.hi m12m00.lo)]
                  [(s2.hi s2.lo)  (fast-fl2- m00m11.hi m00m11.lo m10m01.hi m10m01.lo)]
                  [(x.hi x.lo)  (fast-fl2* s0.hi s0.lo m20 0.0)]
                  [(y.hi y.lo)  (fast-fl2* s1.hi s1.lo m21 0.0)]
                  [(z.hi z.lo)  (fast-fl2* s2.hi s2.lo m22 0.0)]
                  [(e.hi e.lo)  (fast-fl+/error x.hi y.hi)]
                  [(w.hi w.lo)  (fast-mono-fl+/error e.hi (+ e.lo y.lo x.lo))])
      (k (fl2cross m11 m12 m21 m22) (fl2cross m02 m01 m22 m21) s0.hi
         (fl2cross m12 m10 m22 m20) (fl2cross m00 m02 m20 m22) s1.hi
         (fl2cross m10 m11 m20 m21) (fl2cross m01 m00 m21 m20) s2.hi
         (+ w.hi z.hi w.lo z.lo)))))

;; Less than 4.0 ulp error, 2 million inversions per second
(define-syntax-rule (call/affine3-inverse*det
                      m00 m01 m02 m03
                      m10 m11 m12 m13
                      m20 m21 m22 m23
                      k)
  (call/linear3-inverse*det m00 m01 m02 m10 m11 m12 m20 m21 m22
    (λ (n00 n01 n02 n10 n11 n12 n20 n21 n22 det)
      (k n00 n01 n02 (- (fl3dot n00 n01 n02 m03 m13 m23))
         n10 n11 n12 (- (fl3dot n10 n11 n12 m03 m13 m23))
         n20 n21 n22 (- (fl3dot n20 n21 n22 m03 m13 m23))
         det))))

#|
;; Code derived from http://www.geometrictools.com/Documentation/LaplaceExpansionTheorem.pdf
(define-syntax-rule (projective3-inverse m00 m01 m02 m03
                                         m10 m11 m12 m13
                                         m20 m21 m22 m23
                                         m30 m31 m32 m33)
  (let ([s0  (fl2cross m00 m01 m10 m11)]
        [c5  (fl2cross m22 m23 m32 m33)]
        [s1  (fl2cross m00 m02 m10 m12)]
        [c4  (fl2cross m21 m23 m31 m33)]
        [s2  (fl2cross m00 m03 m10 m13)]
        [c3  (fl2cross m21 m22 m31 m32)]
        [s3  (fl2cross m01 m02 m11 m12)]
        [c2  (fl2cross m20 m23 m30 m33)]
        [s4  (fl2cross m01 m03 m11 m13)]
        [c1  (fl2cross m20 m22 m30 m32)]
        [s5  (fl2cross m02 m03 m12 m13)]
        [c0  (fl2cross m20 m21 m30 m31)])
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
  (k (fl3dot m00 m01 m02 x y z)
     (fl3dot m10 m11 m12 x y z)
     (fl3dot m20 m21 m22 x y z)))

(define-syntax-rule (call/affine3-apply 
                      m00 m01 m02 m03
                      m10 m11 m12 m13
                      m20 m21 m22 m23
                      x y z w
                      k)
  (k (fl4dot m00 m01 m02 m03 x y z w)
     (fl4dot m10 m11 m12 m13 x y z w)
     (fl4dot m20 m21 m22 m23 x y z w)
     w))

(define-syntax-rule (call/projective3-apply
                      m00 m01 m02 m03
                      m10 m11 m12 m13
                      m20 m21 m22 m23
                      m30 m31 m32 m33
                      x y z w
                      k)
  (k (fl4dot m00 m01 m02 m03 x y z w)
     (fl4dot m10 m11 m12 m13 x y z w)
     (fl4dot m20 m21 m22 m23 x y z w)
     (fl4dot m30 m31 m32 m33 x y z w)))

;; ===================================================================================================
;; Transpose application

(define-syntax-rule (call/linear3-tapply
                      m00 m01 m02
                      m10 m11 m12
                      m20 m21 m22
                      x y z
                      k)
  (k (fl3dot m00 m10 m20 x y z)
     (fl3dot m01 m11 m21 x y z)
     (fl3dot m02 m12 m22 x y z)))

(define-syntax-rule (call/affine3-tapply
                      m00 m01 m02 m03
                      m10 m11 m12 m13
                      m20 m21 m22 m23
                      x y z w
                      k)
  (k (fl3dot m00 m10 m20 x y z)
     (fl3dot m01 m11 m21 x y z)
     (fl3dot m02 m12 m22 x y z)
     (fl4dot m03 m13 m23 1.0 x y z w)))

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

(define-syntax-rule (call/linear3-compose
                      m00 m01 m02
                      m10 m11 m12
                      m20 m21 m22
                      n00 n01 n02
                      n10 n11 n12
                      n20 n21 n22
                      k)
  (call/linear3-apply m00 m01 m02 m10 m11 m12 m20 m21 m22 n00 n10 n20
    (λ (a00 a10 a20)
      (call/linear3-apply m00 m01 m02 m10 m11 m12 m20 m21 m22 n01 n11 n21
        (λ (a01 a11 a21)
          (call/linear3-apply m00 m01 m02 m10 m11 m12 m20 m21 m22 n02 n12 n22
            (λ (a02 a12 a22)
              (k a00 a01 a02
                 a10 a11 a12
                 a20 a21 a22))))))))

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
