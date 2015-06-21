#lang typed/racket/base

(require (for-syntax racket/base)
         math/flonum
         math/base
         (except-in typed/opengl/ffi -> cast)
         "flv3.rkt"
         "flplane3.rkt"
         "flv4.rkt"
         "flt3-data.rkt"
         "flt3-unboxed-ops.rkt"
         "fllinear3.rkt")

(provide (except-out
          (all-defined-out)
          ;; This isn't safe because it doesn't rename arguments
          flaffine3))

;; ===================================================================================================
;; Affine transforms

(struct FlAffine3 ([forward : FlVector]
                   [inverse : FlVector]
                   [forward-det : Flonum]
                   [inverse-den : Flonum]
                   [forward-data-vec : (U #f F32Vector)]
                   [inverse-data-vec : (U #f F32Vector)])
  #:transparent
  #:mutable)

(define-syntax flaffine3? (make-rename-transformer #'FlAffine3?))

(define-syntax-rule (call/flaffine3-forward t f)
  (call/flv12-values (FlAffine3-forward t) f))

(define-syntax-rule (call/flaffine3-inverse t f)
  (call/flv12+1-values (FlAffine3-inverse t) (FlAffine3-inverse-den t) f))

(define-syntax flaffine3-determinant (make-rename-transformer #'FlAffine3-forward-det))

;; ===================================================================================================

(define-syntax-rule
  (flaffine3 m00 m01 m02 m03
             m10 m11 m12 m13
             m20 m21 m22 m23
             n00 n01 n02 n03
             n10 n11 n12 n13
             n20 n21 n22 n23
             det-stx den-stx)
  (let ([det : Flonum  det-stx]
        [den : Flonum  den-stx])
    (if (< (max (abs det) (abs den)) +inf.0)
        (FlAffine3
         (check-flvector 'flaffine3 (flvector m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23))
         (check-flvector 'flaffine3 (flvector n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23))
         det den #f #f)
        (error 'flaffine3 "expected rational determinant and denominator; given ~e ~e" det den))))

(define identity-flaffine3
  (flaffine3 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             1.0 1.0))

;(: identity-flaffine3? (-> Any Boolean : FlAffine3))
(define (identity-flaffine3? t)
  (eq? t identity-flaffine3))

(: entries->flaffine3 (-> Flonum Flonum Flonum Flonum
                          Flonum Flonum Flonum Flonum
                          Flonum Flonum Flonum Flonum
                          FlAffine3))
(define (entries->flaffine3 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
  (call/affine3-inverse*det m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23
    (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23 det)
      (define a00 (/ n00 det))
      (define a01 (/ n01 det))
      (define a02 (/ n02 det))
      (define a03 (/ n03 det))
      (define a10 (/ n10 det))
      (define a11 (/ n11 det))
      (define a12 (/ n12 det))
      (define a13 (/ n13 det))
      (define a20 (/ n20 det))
      (define a21 (/ n21 det))
      (define a22 (/ n22 det))
      (define a23 (/ n23 det))
      (if (< (max (abs a00) (abs a01) (abs a02) (abs a03)
                  (abs a10) (abs a11) (abs a12) (abs a13)
                  (abs a20) (abs a21) (abs a22) (abs a23))
             +inf.0)
          (flaffine3 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23
                     a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23
                     det 1.0)
          (flaffine3 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23
                     n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23
                     det det)))))
  
(: cols->flaffine3 (-> FlV3 FlV3 FlV3 FlV3 FlAffine3))
(define (cols->flaffine3 x y z p)
  (call/flv3-values x
    (λ (m00 m10 m20)
      (call/flv3-values y
        (λ (m01 m11 m21)
          (call/flv3-values z
            (λ (m02 m12 m22)
              (call/flv3-values p
                (λ (m03 m13 m23)
                  (entries->flaffine3 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23))))))))))

(: flaffine3->cols (-> FlAffine3 (Values FlV3 FlV3 FlV3 FlV3)))
(define (flaffine3->cols t)
  (call/flaffine3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (values (flv3 m00 m10 m20)
              (flv3 m01 m11 m21)
              (flv3 m02 m12 m22)
              (flv3 m03 m13 m23)))))

(: ->flaffine3 (-> (U FlLinear3 FlAffine3) FlAffine3))
(define (->flaffine3 t)
  (cond
    [(fllinear3? t)
     (define det (FlLinear3-forward-det t))
     (call/fllinear3-forward t
       (λ (m00 m01 m02 m10 m11 m12 m20 m21 m22)
         (call/fllinear3-inverse t
           (λ (n00 n01 n02 n10 n11 n12 n20 n21 n22 den)
             (FlAffine3 (flvector m00 m01 m02 0.0
                                  m10 m11 m12 0.0
                                  m20 m21 m22 0.0)
                        (flvector n00 n01 n02 0.0
                                  n10 n11 n12 0.0
                                  n20 n21 n22 0.0)
                        det den #f #f)))))]
    [else  t]))

(: flaffine3-linear-part (-> FlAffine3 FlLinear3))
(define (flaffine3-linear-part t)
  (define det (FlAffine3-forward-det t))
  (call/flaffine3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (call/flaffine3-inverse t
        (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23 den)
          (FlLinear3 (flvector m00 m01 m02 m10 m11 m12 m20 m21 m22)
                     (flvector n00 n01 n02 n10 n11 n12 n20 n21 n22)
                     det den))))))

(: fllinear3-augment (-> FlLinear3 FlV3 FlAffine3))
(define (fllinear3-augment t v)
  (call/fllinear3-forward t
    (λ (m00 m01 m02 m10 m11 m12 m20 m21 m22)
      (call/flv3-values v
        (λ (m03 m13 m23)
          (entries->flaffine3 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23))))))

;; ===================================================================================================
;; Handedness consistency

(: flaffine3-consistent? (-> FlAffine3 Boolean))
(define (flaffine3-consistent? t)
  (define det (FlAffine3-forward-det t))
  (cond [(> det 0.0)  #t]
        [(< det 0.0)  #f]
        [else  (fllinear3-cols-consistent? (flaffine3-linear-part t))]))

;; ===================================================================================================
;; Composition

(: flaffine3-compose (-> FlAffine3 FlAffine3 FlAffine3))
(define (flaffine3-compose t1 t2)
  (define det1 (FlAffine3-forward-det t1))
  (define det2 (FlAffine3-forward-det t2))
  (call/flaffine3-forward t1
    (λ (s00 s01 s02 s03 s10 s11 s12 s13 s20 s21 s22 s23)
      (call/flaffine3-forward t2
        (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23)
          (call/affine3-compose
            s00 s01 s02 s03
            s10 s11 s12 s13
            s20 s21 s22 s23
            n00 n01 n02 n03
            n10 n11 n12 n13
            n20 n21 n22 n23
            (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
              (call/flaffine3-inverse t1
                (λ (s00 s01 s02 s03 s10 s11 s12 s13 s20 s21 s22 s23 den1)
                  (call/flaffine3-inverse t2
                    (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23 den2)
                      (call/affine3-compose
                        n00 n01 n02 n03
                        n10 n11 n12 n13
                        n20 n21 n22 n23
                        s00 s01 s02 s03
                        s10 s11 s12 s13
                        s20 s21 s22 s23
                        (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23)
                          (flaffine3 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23
                                     n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23
                                     (* det1 det2)
                                     (* den1 den2)))))))))))))))

;; ===================================================================================================
;; Transformation constructors

(: move-x-flt3 (-> Flonum FlAffine3))
(define (move-x-flt3 v)
  (if (< (abs v) +inf.0)
      (FlAffine3 (flvector 1.0 0.0 0.0   v
                           0.0 1.0 0.0  0.0
                           0.0 0.0 1.0  0.0)
                 (flvector 1.0 0.0 0.0 (- v)
                           0.0 1.0 0.0  0.0
                           0.0 0.0 1.0  0.0)
                 1.0 1.0 #f #f)
      (raise-argument-error 'move-x-flt3 "rational?" v)))

(: move-y-flt3 (-> Flonum FlAffine3))
(define (move-y-flt3 v)
  (if (< (abs v) +inf.0)
      (FlAffine3 (flvector 1.0 0.0 0.0  0.0
                           0.0 1.0 0.0   v
                           0.0 0.0 1.0  0.0)
                 (flvector 1.0 0.0 0.0  0.0
                           0.0 1.0 0.0 (- v)
                           0.0 0.0 1.0  0.0)
                 1.0 1.0 #f #f)
      (raise-argument-error 'move-y-flt3 "rational?" v)))

(: move-z-flt3 (-> Flonum FlAffine3))
(define (move-z-flt3 v)
  (if (< (abs v) +inf.0)
      (FlAffine3 (flvector 1.0 0.0 0.0  0.0
                           0.0 1.0 0.0  0.0
                           0.0 0.0 1.0   v)
                 (flvector 1.0 0.0 0.0  0.0
                           0.0 1.0 0.0  0.0
                           0.0 0.0 1.0 (- v))
                 1.0 1.0 #f #f)
      (raise-argument-error 'move-z-flt3 "rational?" v)))

(: move-flt3 (-> FlV3 FlAffine3))
(define (move-flt3 v)
  (call/flv3-values v
    (λ (x y z)
      (if (< (max (abs x) (abs y) (abs z)) +inf.0)
          (FlAffine3 (flvector 1.0 0.0 0.0   x
                               0.0 1.0 0.0   y
                               0.0 0.0 1.0   z)
                     (flvector 1.0 0.0 0.0 (- x)
                               0.0 1.0 0.0 (- y)
                               0.0 0.0 1.0 (- z))
                     1.0 1.0 #f #f)
          (raise-argument-error 'move-flt3 "rational vector" v)))))

(: orthographic-flt3 (-> Flonum Flonum Flonum Flonum Flonum Flonum FlAffine3))
;; Similar to glOrtho
(define (orthographic-flt3 l r b t n f)
  (define l-r (- l r))
  (define b-t (- b t))
  (define n-f (- n f))
  (define l+r (+ l r))
  (define b+t (+ b t))
  (define n+f (+ n f))
  (define denom (* b-t n-f l-r))
  (flaffine3 (/ -2.0 l-r)       0.0          0.0      (/ l+r l-r)
                  0.0      (/ -2.0 b-t)      0.0      (/ b+t b-t)
                  0.0           0.0      (/ 2.0 n-f)  (/ n+f n-f)
             (/ l-r -2.0)       0.0          0.0      (/ l+r 2.0)
                  0.0      (/ b-t -2.0)      0.0      (/ b+t 2.0)
                  0.0           0.0      (/ n-f 2.0)  (/ n+f -2.0)
             (/ 8.0 denom)
             (/ denom 8.0)))

(: invent-orthogonal-axes (-> FlV3 (Values FlV3 FlV3)))
(define (invent-orthogonal-axes z)
  (call/flv3-values z
    (λ (dx dy dz)
      (cond
        [(= 0.0 (min (abs dx) (abs dy) (abs dz)))
         (values +x-flv3 +y-flv3)]
        [else
         (let* ([x : FlV3  (flv3 dz dx dy)]
                [y : FlV3  (flv3cross z x)]
                [x : FlV3  (flv3cross y z)])
           (values (assert (flv3normalize x) values)
                   (assert (flv3normalize y) values)))]))))

(: point-at-flt3 (->* [FlV3 FlV3] [Flonum FlV3 Boolean] FlAffine3))
(define (point-at-flt3 from z-axis [angle 0.0] [up +z-flv3] [normalize? #t])
  (let ([z-axis : (U #f FlV3)  (if normalize? (flv3normalize z-axis) z-axis)]
        [up     : (U #f FlV3)  (flv3normalize up)])
    (let ([z-axis  (if (not z-axis) +x-flv3 z-axis)]
          [up      (if (not up)     +z-flv3 up)])
      (let* ([x-axis : FlV3         (flv3cross z-axis up)]
             [x-axis : (U #f FlV3)  (flv3normalize x-axis)])
        (define (fail)
          (define-values (x-axis y-axis) (invent-orthogonal-axes z-axis))
          (let ([x-axis : FlV3  x-axis]
                [y-axis : FlV3  y-axis])
            (assert (cols->flaffine3 x-axis y-axis z-axis from) values)))
        (define t
          (cond
            [x-axis
             (let* ([y-axis : FlV3         (flv3cross z-axis x-axis)]
                    [y-axis : (U #f FlV3)  (flv3normalize y-axis)])
               (cond
                 [y-axis  (assert (cols->flaffine3 x-axis y-axis z-axis from) values)]
                 [else  (fail)]))]
            [else
             (fail)]))
        (let ([t  : FlAffine3  t]
              [t0 : FlAffine3  (->flaffine3 (rotate-z-flt3 angle))])
          (flaffine3-compose t t0))))))

;; ===================================================================================================
;; Inversion

(: flaffine3-inverse (-> FlAffine3 (U #f FlAffine3)))
(define (flaffine3-inverse t)
  (define 1/det (/ (FlAffine3-forward-det t)))
  (cond
    [(not (< (abs 1/det) +inf.0))  #f]
    [(= (FlAffine3-inverse-den t) 1.0)
     ;; Common case: clearly invertible
     (FlAffine3 (FlAffine3-inverse t)
                (FlAffine3-forward t)
                1/det
                1.0
                (FlAffine3-inverse-data-vec t)
                (FlAffine3-forward-data-vec t))]
    [else
     (call/flaffine3-inverse t
       (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23 den)
         (define a00 (/ n00 den))
         (define a01 (/ n01 den))
         (define a02 (/ n02 den))
         (define a03 (/ n03 den))
         (define a10 (/ n10 den))
         (define a11 (/ n11 den))
         (define a12 (/ n12 den))
         (define a13 (/ n13 den))
         (define a20 (/ n20 den))
         (define a21 (/ n21 den))
         (define a22 (/ n22 den))
         (define a23 (/ n23 den))
         (if (< (max (abs a00) (abs a01) (abs a02) (abs a03)
                     (abs a10) (abs a11) (abs a12) (abs a13)
                     (abs a20) (abs a21) (abs a22) (abs a23))
                +inf.0)
             ;; This could happen...
             (FlAffine3 (flvector a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23)
                        (FlAffine3-forward t)
                        1/det
                        1.0
                        #f
                        (FlAffine3-forward-data-vec t))
             #f)))]))

;; ===================================================================================================
;; Application

(: flaffine3-apply (-> FlAffine3 FlV4 FlV4))
(define (flaffine3-apply t v)
  (call/flaffine3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (call/flv4-values v
        (λ (s0 s1 s2 s3)
          (call/affine3-apply
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            s0 s1 s2 s3
            flv4))))))

(: flaffine3-apply/pos (-> FlAffine3 FlV3 FlV3))
;; Application to positions: set w = 1.0 and divide by w'
(define (flaffine3-apply/pos t v)
  (call/flaffine3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (call/flv3-values v
        (λ (s0 s1 s2)
          (call/affine3-apply
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            s0 s1 s2 1.0
            ;; No need to divide because w' = 1
            (λ (x y z _) (flv3 x y z))))))))

(: flaffine3-apply/dir (-> FlAffine3 FlV3 FlV3))
;; Application to directions: set w = 0.0 and ignore w'
(define (flaffine3-apply/dir t v)
  (call/flaffine3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (call/flv3-values v
        (λ (s0 s1 s2)
          (call/affine3-apply
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            s0 s1 s2 0.0
            (λ (x y z _) (flv3 x y z))))))))

(: flaffine3-apply/norm (-> FlAffine3 FlV3 (U #f FlV3)))
;; Application to normals: set w = 0.0, apply inverse transpose, normalize
(define (flaffine3-apply/norm t v)
  (call/flaffine3-inverse t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 _den)  ; normalization makes den unnecessary
      (call/flv3-values v
        (λ (s0 s1 s2)
          (call/affine3-tapply
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            s0 s1 s2 0.0
            (λ (x y z _) (flnorm3 x y z))))))))

(: flaffine3-apply/plane (-> FlAffine3 FlPlane3 (U #f FlPlane3)))
;; Application to planes: apply inverse transpose, normalize
(define (flaffine3-apply/plane t p)
  (call/flaffine3-inverse t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 _den)  ; normalization makes den unnecessary
      (call/flplane3-values p
        (λ (s0 s1 s2 s3)
          (call/affine3-tapply
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            s0 s1 s2 s3
            make-flplane3))))))

;; ===================================================================================================
;; Serialization

(: flaffine3-forward-data (-> FlAffine3 F32Vector))
(define (flaffine3-forward-data t)
  (define vec (FlAffine3-forward-data-vec t))
  (if vec vec (let ([vec  (flv12->f32vector (FlAffine3-forward t))])
                (set-FlAffine3-forward-data-vec! t vec)
                vec)))

;; ===================================================================================================

(: flaffine3-ellipse-angle-zero (-> FlAffine3 Flonum))
;; Finds angle 0 for an ellipse defined by an affine transformation from the unit circle
(define (flaffine3-ellipse-angle-zero t)
  (call/flaffine3-forward t
    (λ (m00 m01 _m02 _m03 m10 m11 _m12 _m13 m20 m21 _m22 _m23)
      (let ([p  (+ (- (sqr m00) (sqr m01))
                   (- (sqr m10) (sqr m11))
                   (- (sqr m20) (sqr m21)))]
            [q  (+ (* m00 m01) (* m10 m11) (* m20 m21))])
        (define a (atan (+ p (flsqrt (+ (sqr p) (* 4.0 (sqr q)))))
                        (* -2.0 q)))
        (if (< a 0.0)
            (+ a (* 0.5 pi))
            (- a (* 0.5 pi)))))))
