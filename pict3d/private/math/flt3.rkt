#lang typed/racket/base

#|
TODO

Higher precision (try to guarantee 2.5 ulps?)
|#

(require (for-syntax racket/base)
         racket/list
         racket/match
         racket/unsafe/ops
         math/flonum
         "flv3.rkt")

(provide
 ;; Homogeneous coordinate vectors
 flv4? flv4-values
 pos->flv4 norm->flv4 flplane3->flv4
 flv4->pos flv4->norm flv4->flplane3
 ;; Types, basic constructors and basic accessors
 FlIdentity3
 flidentity3?
 FlLinear3
 fllinear3?
 fllinear3-entries
 fllinear3-values
 FlAffine3
 flaffine3?
 flaffine3-entries
 flaffine3-values
 FlProjective3
 flprojective3?
 flprojective3-entries
 flprojective3-values
 FlLinear3-
 FlAffine3-
 FlTransform3
 (rename-out [make-fllinear3 fllinear3]
             [make-flaffine3 flaffine3]
             [make-flprojective3 flprojective3])
 ->fllinear3
 ->flaffine3
 ->flprojective3
 basis->fllinear3
 basis->flaffine3
 ;; Transformation constructors
 identity-flt3
 rotate-x-flt3
 rotate-y-flt3
 rotate-z-flt3
 rotate-flt3
 scale-flt3
 translate-flt3
 frustum-flt3
 perspective-flt3
 perspective-flt3/x-fov
 perspective-flt3/y-fov
 perspective-flt3/viewport
 orthographic-flt3
 ;; Operations
 flt3inverse
 flt3apply
 flt3tapply
 flt3apply/pos
 flt3apply/norm
 flt3apply/plane
 flt3compose
 flt3consistent?
 ;; Frustum utils
 clip-frustum-plane-x-
 clip-frustum-plane-x+
 clip-frustum-plane-y-
 clip-frustum-plane-y+
 clip-frustum-plane-z-
 clip-frustum-plane-z+
 clip-frustum-planes
 flprojective3-z-near
 flprojective3-z-far
 flprojective3-frustum-planes
 )

(define-syntax-rule (define-make-fltype3 make-fltype3 fltype3 num)
  (begin
    (: make-fltype3 (-> FlVector fltype3))
    (define (make-fltype3 ms)
      (cond [(= (flvector-length ms) num)  (fltype3 ms)]
            [else  (raise-argument-error 'fltype3 (format "FlVector of length ~a" num) ms)]))))

(define-syntax-rule (define-fltype3-values fltype3-values fltype3 fltype3-entries num)
  (define-syntax (fltype3-values stx)
    (syntax-case stx ()
      [(_ v-stx)
       (with-syntax ([(i (... ...))  (build-list num values)])
         (syntax/loc stx
           (let* ([v : fltype3  v-stx]
                  [es  (fltype3-entries v)])
             (values (unsafe-flvector-ref es i) (... ...)))))])))

(define-syntax-rule (det2 a b c d)
  (- (* a d) (* b c)))

(define-syntax-rule (v3dot x1 y1 z1 x2 y2 z2)
  (+ (* x1 x2) (* y1 y2) (* z1 z2)))

(define-syntax-rule (v4dot m0 m1 m2 m3 v0 v1 v2 v3)
  (+ (* m0 v0) (* m1 v1) (* m2 v2) (* m3 v3)))

;; ===================================================================================================
;; Homogeneous coordinate vectors

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

;; ===================================================================================================
;; Types, basic constructors and basic accessors

(struct flprojective3 ([entries : FlVector]) #:transparent)
(struct flaffine3 ([entries : FlVector]) #:transparent)
(struct fllinear3 ([entries : FlVector]) #:transparent)
(struct flidentity3 () #:transparent)

(define-type FlIdentity3 flidentity3)
(define-type FlLinear3 fllinear3)
(define-type FlAffine3 flaffine3)
(define-type FlProjective3 flprojective3)

(define-type FlLinear3-
  (U flidentity3
     fllinear3))

(define-type FlAffine3-
  (U FlLinear3-
     flaffine3))

(define-type FlTransform3
  (U FlAffine3-
     flprojective3))

(define-make-fltype3 make-fllinear3     fllinear3      9)
(define-make-fltype3 make-flaffine3     flaffine3     12)
(define-make-fltype3 make-flprojective3 flprojective3 16)

(define-fltype3-values fllinear3-values     fllinear3     fllinear3-entries      9)
(define-fltype3-values flaffine3-values     flaffine3     flaffine3-entries     12)
(define-fltype3-values flprojective3-values flprojective3 flprojective3-entries 16)

(: ->fllinear3 (-> FlLinear3- fllinear3))
(define (->fllinear3 m)
  (cond [(flidentity3? m)
         (fllinear3
          (flvector 1.0 0.0 0.0
                    0.0 1.0 0.0
                    0.0 0.0 1.0))]
        [else  m]))

(: ->flaffine3 (-> FlAffine3- flaffine3))
(define (->flaffine3 m)
  (cond [(flidentity3? m)
         (flaffine3
          (flvector 1.0 0.0 0.0 0.0
                    0.0 1.0 0.0 0.0
                    0.0 0.0 1.0 0.0))]
        [(fllinear3? m)
         (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22) (fllinear3-values m))
         (flaffine3
          (flvector m00 m01 m02 0.0
                    m10 m11 m12 0.0
                    m20 m21 m22 0.0))]
        [else  m]))

(: ->flprojective3 (-> FlTransform3 flprojective3))
(define (->flprojective3 m)
  (cond [(flidentity3? m)
         (flprojective3
          (flvector 1.0 0.0 0.0 0.0
                    0.0 1.0 0.0 0.0
                    0.0 0.0 1.0 0.0
                    0.0 0.0 0.0 1.0))]
        [(fllinear3? m)
         (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22) (fllinear3-values m))
         (flprojective3
          (flvector m00 m01 m02 0.0
                    m10 m11 m12 0.0
                    m20 m21 m22 0.0
                    0.0 0.0 0.0 1.0))]
        [(flaffine3? m)
         (define-values (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23) (flaffine3-values m))
         (flprojective3
          (flvector m00 m01 m02 m03
                    m10 m11 m12 m13
                    m20 m21 m22 m23
                    0.0 0.0 0.0 1.0))]
        [else  m]))

(: basis->fllinear3 (-> FlVector FlVector FlVector FlLinear3))
(define (basis->fllinear3 x y z)
  (define-values (x0 x1 x2) (flv3-values x))
  (define-values (y0 y1 y2) (flv3-values y))
  (define-values (z0 z1 z2) (flv3-values z))
  (fllinear3 (flvector x0 y0 z0 x1 y1 z1 x2 y2 z2)))

(: basis->flaffine3 (-> FlVector FlVector FlVector FlVector FlAffine3))
(define (basis->flaffine3 x y z p)
  (define-values (x0 x1 x2) (flv3-values x))
  (define-values (y0 y1 y2) (flv3-values y))
  (define-values (z0 z1 z2) (flv3-values z))
  (define-values (p0 p1 p2) (flv3-values p))
  (flaffine3 (flvector x0 y0 z0 p0 x1 y1 z1 p1 x2 y2 z2 p2)))

;; ===================================================================================================
;; Transformation constructors

(: identity-flt3 flidentity3)
(define identity-flt3 (flidentity3))

(: scale-flt3 (-> FlVector fllinear3))
(define (scale-flt3 v)
  (define-values (x y z) (flv3-values v))
  (fllinear3
   (flvector  x  0.0 0.0
             0.0  y  0.0
             0.0 0.0  z )))

(: rotate-x-flt3 (-> Flonum fllinear3))
(define (rotate-x-flt3 rho)
  (define c (flcos rho))
  (define s (flsin rho))
  (fllinear3
   (flvector 1.0  0.0   0.0
             0.0   c  (- s)
             0.0   s     c)))

(: rotate-y-flt3 (-> Flonum fllinear3))
(define (rotate-y-flt3 phi)
  (define c (flcos phi))
  (define s (flsin phi))
  (fllinear3
   (flvector    c   0.0   s
               0.0  1.0  0.0
             (- s)  0.0   c)))

(: rotate-z-flt3 (-> Flonum fllinear3))
(define (rotate-z-flt3 theta)
  (define c (flcos theta))
  (define s (flsin theta))
  (fllinear3
   (flvector  c  (- s)  0.0
              s     c   0.0
             0.0   0.0  1.0)))

(: rotate-flt3 (-> FlVector Flonum FlLinear3))
(define (rotate-flt3 axis angle)
  (define-values (x y z) (flv3-values axis))
  (define c (flcos angle))
  (define s (flsin angle))
  (define t (- 1.0 c))
  (fllinear3
   (flvector (+ (* t x x) c)        (- (* t x y) (* z s))  (+ (* t x z) (* y s))
             (+ (* t x y) (* z s))  (+ (* t y y) c)        (- (* t y z) (* x s))
             (- (* t x z) (* y s))  (+ (* t y z) (* x s))  (+ (* t z z) c))))

(: translate-flt3 (-> FlVector flaffine3))
(define (translate-flt3 v)
  (define-values (x y z) (flv3-values v))
  (flaffine3
   (flvector 1.0 0.0 0.0  x
             0.0 1.0 0.0  y
             0.0 0.0 1.0  z
             0.0 0.0 0.0 1.0)))

(: frustum-flt3 (-> Flonum Flonum Flonum Flonum Flonum Flonum flprojective3))
;; Similar to glFrustum
(define (frustum-flt3 l r b t n f)
  (define r-l (- r l))
  (define t-b (- t b))
  (define n-f (- n f))
  (flprojective3
   (flvector (/ (* 2.0 n) r-l)          0.0        (/ (+ r l) r-l)          0.0
                     0.0        (/ (* 2.0 n) t-b)  (/ (+ t b) t-b)          0.0
                     0.0                0.0        (/ (+ n f) n-f)  (/ (* 2.0 n f) n-f)
                     0.0                0.0             -1.0                0.0)))

(: perspective-flt3 (-> Flonum Flonum Flonum Flonum flprojective3))
(define (perspective-flt3 xs ys n f)
  (define n-f (- n f))
  (flprojective3
   (flvector   xs  0.0        0.0                0.0
              0.0   ys        0.0                0.0
              0.0  0.0  (/ (+ n f) n-f)  (/ (* 2.0 n f) n-f)
              0.0  0.0       -1.0                0.0)))

(: perspective-flt3/x-fov (-> Flonum Flonum Flonum Flonum flprojective3))
(define (perspective-flt3/x-fov x-fov aspect n f)
  (define t (/ 1.0 (fltan (* 0.5 x-fov))))
  (perspective-flt3 t (* t aspect) n f))

(: perspective-flt3/y-fov (-> Flonum Flonum Flonum Flonum flprojective3))
;; Similar to gluPerspective
(define (perspective-flt3/y-fov y-fov aspect n f)
  (define t (/ 1.0 (fltan (* 0.5 y-fov))))
  (perspective-flt3 (/ t aspect) t n f))

(: perspective-flt3/viewport (-> Flonum Flonum Flonum Flonum Flonum flprojective3))
(define (perspective-flt3/viewport width height fov n f)
  (define aspect (/ width height))
  (if (> (abs width) (abs height))
      (perspective-flt3/y-fov fov aspect n f)
      (perspective-flt3/x-fov fov aspect n f)))

(: orthographic-flt3 (-> Flonum Flonum Flonum Flonum Flonum Flonum flprojective3))
;; Similar to glOrtho
(define (orthographic-flt3 l r b t n f)
  (define l-r (- l r))
  (define b-t (- b t))
  (define n-f (- n f))
  (flprojective3
   (flvector (/ -2.0 l-r)       0.0          0.0      (/ (+ l r) l-r)
                  0.0      (/ -2.0 b-t)      0.0      (/ (+ b t) b-t)
                  0.0           0.0      (/ 2.0 n-f)  (/ (+ n f) n-f)
                  0.0           0.0          0.0            1.0)))

;; ===================================================================================================
;; Inversion

(define-syntax-rule (linear3-inverse s00 s01 s02 s10 s11 s12 s20 s21 s22)
  (let-values ([(m00 m01 m02 m10 m11 m12 m20 m21 m22)
                (values s00 s01 s02 s10 s11 s12 s20 s21 s22)])
    (define s0 (det2 m01 m02 m11 m12))
    (define s1 (det2 m02 m00 m12 m10))
    (define s2 (det2 m00 m01 m10 m11))
    (define det (+ (* s0 m20) (* s1 m21) (* s2 m22)))
    (values (/ (det2 m11 m12 m21 m22) det)  (/ (det2 m02 m01 m22 m21) det)  (/ s0 det)
            (/ (det2 m12 m10 m22 m20) det)  (/ (det2 m00 m02 m20 m22) det)  (/ s1 det)
            (/ (det2 m10 m11 m20 m21) det)  (/ (det2 m01 m00 m21 m20) det)  (/ s2 det))))

(define-syntax-rule (affine3-inverse m00 m01 m02 s03 m10 m11 m12 s13 m20 m21 m22 s23)
  (let-values ([(m03 m13 m23)  (values s03 s13 s23)]
               [(n00 n01 n02 n10 n11 n12 n20 n21 n22)
                (linear3-inverse m00 m01 m02 m10 m11 m12 m20 m21 m22)])
    (values n00 n01 n02 (- (v3dot n00 n01 n02 m03 m13 m23))
            n10 n11 n12 (- (v3dot n10 n11 n12 m03 m13 m23))
            n20 n21 n22 (- (v3dot n20 n21 n22 m03 m13 m23)))))

;; Code derived from http://www.geometrictools.com/Documentation/LaplaceExpansionTheorem.pdf
(define-syntax-rule (projective3-inverse s00 s01 s02 s03 s10 s11 s12 s13
                                         s20 s21 s22 s23 s30 s31 s32 s33)
  (let-values ([(m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
                (values s00 s01 s02 s03 s10 s11 s12 s13 s20 s21 s22 s23 s30 s31 s32 s33)])
    (define s0 (det2 m00 m01 m10 m11))
    (define c5 (det2 m22 m23 m32 m33))
    (define s1 (det2 m00 m02 m10 m12))
    (define c4 (det2 m21 m23 m31 m33))
    (define s2 (det2 m00 m03 m10 m13))
    (define c3 (det2 m21 m22 m31 m32))
    (define s3 (det2 m01 m02 m11 m12))
    (define c2 (det2 m20 m23 m30 m33))
    (define s4 (det2 m01 m03 m11 m13))
    (define c1 (det2 m20 m22 m30 m32))
    (define s5 (det2 m02 m03 m12 m13))
    (define c0 (det2 m20 m21 m30 m31))
    (define det (+ (* s0 c5) (- (* s4 c1)) (* s2 c3) (* s3 c2) (- (* s1 c4)) (* s5 c0)))
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
     (/ (+    (* m20 s3)  (- (* m21 s1))    (* m22 s0))  det))))

(: fllinear3-inverse (-> fllinear3 fllinear3))
(define (fllinear3-inverse m)
  (let*-values ([(m00 m01 m02 m10 m11 m12 m20 m21 m22)  (fllinear3-values m)]
                [(m00 m01 m02 m10 m11 m12 m20 m21 m22)
                 (linear3-inverse m00 m01 m02 m10 m11 m12 m20 m21 m22)])
    (fllinear3 (flvector m00 m01 m02 m10 m11 m12 m20 m21 m22))))

(: flaffine3-inverse (-> flaffine3 flaffine3))
(define (flaffine3-inverse m)
  (let*-values ([(m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)  (flaffine3-values m)]
                [(m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
                 (affine3-inverse m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)])
    (flaffine3 (flvector m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23))))

(: flprojective3-inverse (-> flprojective3 flprojective3))
(define (flprojective3-inverse m)
  (let*-values
      ([(m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)  (flprojective3-values m)]
       [(m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
        (projective3-inverse m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)])
    (flprojective3 (flvector m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33))))

(define-syntax (flt3inverse stx)
  (syntax-case stx ()
    [(_ m-stx)
     (syntax/loc stx
       (let ([m  m-stx])
         (cond [(flidentity3? m)    m]
               [(fllinear3? m)      (fllinear3-inverse m)]
               [(flaffine3? m)      (flaffine3-inverse m)]
               [(flprojective3? m)  (flprojective3-inverse m)]
               [else  (ann m FlTransform3)])))]
    [(_ . es)
     (syntax/loc stx (flt3inverse-fun . es))]
    [_
     (syntax/loc stx flt3inverse-fun)]))

(: flt3inverse-fun
   (case-> (-> flidentity3   flidentity3)
           (-> fllinear3     fllinear3)
           (-> flaffine3     flaffine3)
           (-> flprojective3 flprojective3)
           (-> FlLinear3-     FlLinear3-)
           (-> FlAffine3-     FlAffine3-)
           (-> FlTransform3 FlTransform3)))
(define (flt3inverse-fun m)
  (flt3inverse m))

;; ===================================================================================================
;; Application

(define-syntax-rule (linear3-apply m00 m01 m02 m10 m11 m12 m20 m21 m22 s0 s1 s2)
  (let-values ([(x y z)  (values s0 s1 s2)])
    (values (v3dot m00 m01 m02 x y z)
            (v3dot m10 m11 m12 x y z)
            (v3dot m20 m21 m22 x y z))))

(define-syntax-rule (affine3-apply m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 s0 s1 s2 s3)
  (let-values ([(x y z w)  (values s0 s1 s2 s3)])
    (values (v4dot m00 m01 m02 m03 x y z w)
            (v4dot m10 m11 m12 m13 x y z w)
            (v4dot m20 m21 m22 m23 x y z w)
            w)))

(define-syntax-rule (projective3-apply m00 m01 m02 m03 m10 m11 m12 m13
                                       m20 m21 m22 m23 m30 m31 m32 m33
                                       s0 s1 s2 s3)
  (let-values ([(x y z w)  (values s0 s1 s2 s3)])
    (values (v4dot m00 m01 m02 m03 x y z w)
            (v4dot m10 m11 m12 m13 x y z w)
            (v4dot m20 m21 m22 m23 x y z w)
            (v4dot m30 m31 m32 m33 x y z w))))

(: fllinear3-apply (-> fllinear3 FlVector FlVector))
(define (fllinear3-apply m v)
  (let*-values ([(m00 m01 m02 m10 m11 m12 m20 m21 m22)  (fllinear3-values m)]
                [(x y z w)  (flv4-values v)]
                [(x y z)    (linear3-apply m00 m01 m02 m10 m11 m12 m20 m21 m22 x y z)])
    (flvector x y z w)))

(: flaffine3-apply (-> flaffine3 FlVector FlVector))
(define (flaffine3-apply m v)
  (let*-values ([(m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)  (flaffine3-values m)]
                [(x y z w)  (flv4-values v)]
                [(x y z w)  (affine3-apply m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 x y z w)])
    (flvector x y z w)))

(: flprojective3-apply (-> flprojective3 FlVector FlVector))
(define (flprojective3-apply m v)
  (let*-values
      ([(m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)  (flprojective3-values m)]
       [(x y z w)  (flv4-values v)]
       [(x y z w)  (projective3-apply m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33
                                      x y z w)])
    (flvector x y z w)))

(define-syntax (flt3apply stx)
  (syntax-case stx ()
    [(_ m-stx v-stx)
     (syntax/loc stx
       (let ([m  m-stx]
             [v  v-stx])
         (cond [(flidentity3? m)    (ann v FlVector)]
               [(fllinear3? m)      (fllinear3-apply m v)]
               [(flaffine3? m)      (flaffine3-apply m v)]
               [(flprojective3? m)  (flprojective3-apply m v)]
               [else  (ann m FlTransform3)
                      (ann v FlVector)])))]
    [(_ . es)
     (syntax/loc stx (flt3apply-fun . es))]
    [_
     (syntax/loc stx flt3apply-fun)]))

(: flt3apply-fun (-> FlTransform3 FlVector FlVector))
(define (flt3apply-fun m v)
  (flt3apply m v))

;; ===================================================================================================
;; Transpose application

(define-syntax-rule (affine3-tapply m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 s0 s1 s2 s3)
  (let-values ([(x y z)  (values s0 s1 s2)])
    (values (v3dot m00 m10 m20 x y z)
            (v3dot m01 m11 m21 x y z)
            (v3dot m02 m12 m22 x y z)
            (+ s3 (v3dot m03 m13 m23 x y z)))))

(: fllinear3-tapply (-> fllinear3 FlVector FlVector))
(define (fllinear3-tapply m v)
  (let*-values ([(m00 m01 m02 m10 m11 m12 m20 m21 m22)  (fllinear3-values m)]
                [(x y z w)  (flv4-values v)]
                [(x y z)    (linear3-apply m00 m10 m20 m01 m11 m21 m02 m12 m22 x y z)])
    (flvector x y z w)))

(: flaffine3-tapply (-> flaffine3 FlVector FlVector))
(define (flaffine3-tapply m v)
  (let*-values ([(m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)  (flaffine3-values m)]
                [(x y z w)  (flv4-values v)]
                [(x y z w)  (affine3-tapply m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 x y z w)])
    (flvector x y z w)))

(: flprojective3-tapply (-> flprojective3 FlVector FlVector))
(define (flprojective3-tapply m v)
  (let*-values
      ([(m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)  (flprojective3-values m)]
       [(x y z w)  (flv4-values v)]
       [(x y z w)  (projective3-apply m00 m10 m20 m30 m01 m11 m21 m31 m02 m12 m22 m32 m03 m13 m23 m33
                    x y z w)])
    (flvector x y z w)))

(define-syntax (flt3tapply stx)
  (syntax-case stx ()
    [(_ m-stx v-stx)
     (syntax/loc stx
       (let ([m  m-stx]
             [v  v-stx])
         (cond [(flidentity3? m)    (ann v FlVector)]
               [(fllinear3? m)      (fllinear3-tapply m v)]
               [(flaffine3? m)      (flaffine3-tapply m v)]
               [(flprojective3? m)  (flprojective3-tapply m v)]
               [else  (ann m FlTransform3)
                      (ann v FlVector)])))]
    [(_ . es)
     (syntax/loc stx (flt3tapply-fun . es))]
    [_
     (syntax/loc stx flt3tapply-fun)]))

(: flt3tapply-fun (-> FlTransform3 FlVector FlVector))
(define (flt3tapply-fun m v)
  (flt3tapply m v))

;; ===================================================================================================

(: flt3apply/pos (-> FlTransform3 FlVector FlVector))
(define (flt3apply/pos t v)
  (flv4->pos (flt3apply t (pos->flv4 v))))

(: flt3apply/norm (-> FlTransform3 FlVector FlVector))
(define (flt3apply/norm tinv v)
  (let ([v  (flv3normalize (flv4->norm (flt3tapply tinv (norm->flv4 v))))])
    (if v v (flvector 0.0 0.0 0.0))))

(: flt3apply/plane (-> FlTransform3 FlPlane3 (U #f FlPlane3)))
(define (flt3apply/plane tinv p)
  (flv4->flplane3 (flt3tapply tinv (flplane3->flv4 p))))

;; ===================================================================================================
;; Composition

(: fllinear3-compose (-> fllinear3 fllinear3 fllinear3))
(define (fllinear3-compose m n)
  (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22) (fllinear3-values m))
  (define-values (n00 n01 n02 n10 n11 n12 n20 n21 n22) (fllinear3-values n))
  (define-values (a00 a10 a20) (linear3-apply m00 m01 m02 m10 m11 m12 m20 m21 m22 n00 n10 n20))
  (define-values (a01 a11 a21) (linear3-apply m00 m01 m02 m10 m11 m12 m20 m21 m22 n01 n11 n21))
  (define-values (a02 a12 a22) (linear3-apply m00 m01 m02 m10 m11 m12 m20 m21 m22 n02 n12 n22))
  (fllinear3 (flvector a00 a01 a02 a10 a11 a12 a20 a21 a22)))

(: flaffine3-compose (-> flaffine3 flaffine3 flaffine3))
(define (flaffine3-compose m n)
  (define-values (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23) (flaffine3-values m))
  (define-values (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23) (flaffine3-values n))
  (define-values (a00 a10 a20) (linear3-apply m00 m01 m02 m10 m11 m12 m20 m21 m22 n00 n10 n20))
  (define-values (a01 a11 a21) (linear3-apply m00 m01 m02 m10 m11 m12 m20 m21 m22 n01 n11 n21))
  (define-values (a02 a12 a22) (linear3-apply m00 m01 m02 m10 m11 m12 m20 m21 m22 n02 n12 n22))
  (define-values (a03 a13 a23 _) (affine3-apply m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23
                                                n03 n13 n23 1.0))
  (flaffine3 (flvector a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23)))

(: flprojective3-compose (-> flprojective3 flprojective3 flprojective3))
(define (flprojective3-compose m n)
  (define-values (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
    (flprojective3-values m))
  (define-values (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23 n30 n31 n32 n33)
    (flprojective3-values n))
  
  (define-values (a00 a10 a20 a30)
    (projective3-apply m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33
                       n00 n10 n20 n30))
  (define-values (a01 a11 a21 a31)
    (projective3-apply m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33
                       n01 n11 n21 n31))
  (define-values (a02 a12 a22 a32)
    (projective3-apply m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33
                       n02 n12 n22 n32))
  (define-values (a03 a13 a23 a33)
    (projective3-apply m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33
                       n03 n13 n23 n33))
  (flprojective3 (flvector a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33)))

(: flt3compose (case-> (-> flidentity3 flidentity3 flidentity3)
                       (-> FlLinear3- fllinear3 fllinear3)
                       (-> fllinear3 FlLinear3- fllinear3)
                       (-> FlLinear3- FlLinear3- FlLinear3-)
                       (-> FlAffine3- flaffine3 flaffine3)
                       (-> flaffine3 FlAffine3- flaffine3)
                       (-> FlAffine3- FlAffine3- FlAffine3-)
                       (-> FlTransform3 flprojective3 flprojective3)
                       (-> flprojective3 FlTransform3 flprojective3)
                       (-> FlTransform3 FlTransform3 FlTransform3)))
(define (flt3compose m n)
  (cond [(flidentity3? m)  n]
        [(flidentity3? n)  m]
        [(flprojective3? m)  (flprojective3-compose m (->flprojective3 n))]
        [(flprojective3? n)  (flprojective3-compose (->flprojective3 m) n)]
        [(fllinear3? m)
         (cond [(fllinear3? n)  (fllinear3-compose m n)]
               [else  (flaffine3-compose (->flaffine3 m) n)])]
        [else
         (cond [(fllinear3? n)  (flaffine3-compose m (->flaffine3 n))]
               [else  (flaffine3-compose m n)])]))

;; ===================================================================================================
;; Handedness consistency

(define-syntax-rule (linear3-determinant s00 s01 s02 s10 s11 s12 s20 s21 s22)
  (let-values ([(m00 m01 m02 m10 m11 m12 m20 m21 m22)
                (values s00 s01 s02 s10 s11 s12 s20 s21 s22)])
    (define s0 (det2 m01 m02 m11 m12))
    (define s1 (det2 m02 m00 m12 m10))
    (define s2 (det2 m00 m01 m10 m11))
    (+ (* s0 m20) (* s1 m21) (* s2 m22))))

(: fllinear3-consistent? (-> fllinear3 Boolean))
(define (fllinear3-consistent? m)
  (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22) (fllinear3-values m))
  (>= (linear3-determinant m00 m01 m02 m10 m11 m12 m20 m21 m22) 0.0))

(: flaffine3-consistent? (-> flaffine3 Boolean))
(define (flaffine3-consistent? m)
  (define-values (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23) (flaffine3-values m))
  (>= (linear3-determinant m00 m01 m02 m10 m11 m12 m20 m21 m22) 0.0))

(: flt3consistent? (-> FlAffine3- Boolean))
(define (flt3consistent? m)
  (cond [(flidentity3? m)  #t]
        [(fllinear3? m)  (fllinear3-consistent? m)]
        [else  (flaffine3-consistent? m)]))

;; ===================================================================================================
;; Frustum utils

(define clip-frustum-plane-x- (assert (flplane3 (flvector +1.0 0.0 0.0) 1.0) values))
(define clip-frustum-plane-x+ (assert (flplane3 (flvector -1.0 0.0 0.0) 1.0) values))
(define clip-frustum-plane-y- (assert (flplane3 (flvector 0.0 +1.0 0.0) 1.0) values))
(define clip-frustum-plane-y+ (assert (flplane3 (flvector 0.0 -1.0 0.0) 1.0) values))
(define clip-frustum-plane-z- (assert (flplane3 (flvector 0.0 0.0 +1.0) 1.0) values))
(define clip-frustum-plane-z+ (assert (flplane3 (flvector 0.0 0.0 -1.0) 1.0) values))
(define clip-frustum-planes
  (list clip-frustum-plane-x- clip-frustum-plane-x+
        clip-frustum-plane-y- clip-frustum-plane-y+
        clip-frustum-plane-z- clip-frustum-plane-z+))

(: flprojective3-z-near (-> flprojective3 Flonum))
(define (flprojective3-z-near t)
  (define p (flt3apply/plane t clip-frustum-plane-z-))
  (cond [p  (- (flplane3-distance p))]
        [else  (error 'flprojective3-z-near "cannot get z-near distance from ~a" t)]))

(: flprojective3-z-far (-> flprojective3 Flonum))
(define (flprojective3-z-far t)
  (define p (flt3apply/plane t clip-frustum-plane-z+))
  (cond [p  (flplane3-distance p)]
        [else  (error 'flprojective3-z-far "cannot get z-far distance from ~a" t)]))

(: flprojective3-frustum-planes (-> flprojective3 (Listof FlPlane3)))
(define (flprojective3-frustum-planes t)
  (for/fold ([planes : (Listof FlPlane3)  empty]) ([p  (in-list clip-frustum-planes)])
    (let ([p  (flt3apply/plane t p)])
      (if p (cons p planes) planes))))
