#lang typed/racket/base

#|
TODO

Don't just give up on inverting something with a denominator
|#

(require (for-syntax racket/base)
         racket/list
         racket/unsafe/ops
         math/flonum
         math/base
         (except-in typed/opengl/ffi -> cast)
         "flv3.rkt"
         "flplane3.rkt"
         "flv4.rkt"
         "flt3-data.rkt"
         "flt3-unboxed-ops.rkt"
         "fllinear3.rkt"
         "flaffine3.rkt"
         "../utils.rkt"
         "../ffi.rkt")

(provide (except-out
          (all-defined-out)
          ;; This isn't safe because it doesn't rename arguments
          flprojective3))

;; ===================================================================================================
;; Projective transforms

(struct FlProjective3 ([forward : FlVector]
                       [inverse : FlVector]
                       [forward-det : Flonum]
                       [inverse-den : Flonum]
                       [forward-data-vec : (U #f F32Vector)]
                       [inverse-data-vec : (U #f F32Vector)])
  #:transparent
  #:mutable)

(define-syntax flprojective3? (make-rename-transformer #'FlProjective3?))

(define-syntax-rule (call/flprojective3-forward t f)
  (call/flv16-values (FlProjective3-forward t) f))

(define-syntax-rule (call/flprojective3-inverse t f)
  (call/flv16+1-values (FlProjective3-inverse t) (FlProjective3-inverse-den t) f))

(define-syntax flprojective3-determinant (make-rename-transformer #'FlProjective3-forward-det))

;; ===================================================================================================

(define-syntax-rule
  (flprojective3 m00 m01 m02 m03
                 m10 m11 m12 m13
                 m20 m21 m22 m23
                 m30 m31 m32 m33
                 n00 n01 n02 n03
                 n10 n11 n12 n13
                 n20 n21 n22 n23
                 n30 n31 n32 n33
                 det-stx den-stx)
  (let ([det : Flonum  det-stx]
        [den : Flonum  den-stx])
    (if (< (max (abs det) (abs den)) +inf.0)
        (FlProjective3
         (check-flvector 'flprojective3
                         (flvector m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33))
         (check-flvector 'flprojective3
                         (flvector n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23 n30 n31 n32 n33))
         det den #f #f)
        (error 'flprojective3 "expected rational determinant and denominator; given ~e ~e" det den))))

(define identity-flprojective3
  (flprojective3 1.0 0.0 0.0 0.0
                 0.0 1.0 0.0 0.0
                 0.0 0.0 1.0 0.0
                 0.0 0.0 0.0 1.0
                 1.0 0.0 0.0 0.0
                 0.0 1.0 0.0 0.0
                 0.0 0.0 1.0 0.0
                 0.0 0.0 0.0 1.0
                 1.0 1.0))

;(: identity-flprojective? (-> Any Boolean : FlProjective3))
(define (identity-flprojective? t)
  (eq? t identity-flaffine3))

(: ->flprojective3 (-> (U FlLinear3 FlAffine3 FlProjective3) FlProjective3))
(define (->flprojective3 t)
  (cond
    [(fllinear3? t)
     (define det (FlLinear3-forward-det t))
     (call/fllinear3-forward t
       (λ (m00 m01 m02 m10 m11 m12 m20 m21 m22)
         (call/fllinear3-inverse t
           (λ (n00 n01 n02 n10 n11 n12 n20 n21 n22 den)
             (FlProjective3
              (flvector m00 m01 m02 0.0 m10 m11 m12 0.0 m20 m21 m22 0.0 0.0 0.0 0.0 1.0)
              (flvector n00 n01 n02 0.0 n10 n11 n12 0.0 n20 n21 n22 0.0 0.0 0.0 0.0 1.0)
              det den #f #f)))))]
    [(flaffine3? t)
     (define det (FlAffine3-forward-det t))
     (call/flaffine3-forward t
       (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
         (call/flaffine3-inverse t
           (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23 den)
             (FlProjective3
              (flvector m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 0.0 0.0 0.0 1.0)
              (flvector n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23 0.0 0.0 0.0 1.0)
              det den #f #f)))))]
    [else  t]))

;; ===================================================================================================
;; Handedness consistency

(: flprojective3-consistent? (-> FlProjective3 Boolean))
(define (flprojective3-consistent? t)
  (> (FlProjective3-forward-det t) 0.0))

;; ===================================================================================================
;; Transformation constructors

(: frustum-flt3 (-> Flonum Flonum Flonum Flonum Flonum Flonum FlProjective3))
;; Similar to glFrustum
(define (frustum-flt3 l r b t n f)
  (define 2*n (* 2.0 n))
  (define r-l (- r l))
  (define t-b (- t b))
  (define n-f (- n f))
  (define r+l (+ r l))
  (define t+b (+ t b))
  (define n+f (+ n f))
  (define n*f (* n f))
  (define numer (* 4.0 2*n n n*f))
  (define denom (* t-b n-f r-l))
  (flprojective3 (/ 2*n r-l)       0.0     (/ r+l r-l)          0.0
                      0.0     (/ 2*n t-b)  (/ t+b t-b)          0.0
                      0.0          0.0     (/ n+f n-f)  (/ (* 2*n f) n-f)
                      0.0          0.0         -1.0             0.0
                 (/ r-l 2*n)       0.0          0.0        (/ r+l 2*n)
                      0.0     (/ t-b 2*n)       0.0        (/ t+b 2*n)
                      0.0          0.0          0.0            -1.0
                      0.0          0.0     (/ n-f n*f)     (/ n+f n*f)
                 (/ numer denom)
                 1.0))

(: perspective-flt3 (-> Flonum Flonum Flonum Flonum FlProjective3))
(define (perspective-flt3 x y n f)
  (define n-f (- n f))
  (define n+f (+ n f))
  (define 2*n*f (* 2.0 n f))
  (define numer (* 2*n*f x y))
  (flprojective3   x   0.0       0.0          0.0
                  0.0   y        0.0          0.0
                  0.0  0.0  (/ n+f n-f)  (/ 2*n*f n-f)
                  0.0  0.0      -1.0          0.0
                 (/ x) 0.0       0.0            0.0
                  0.0 (/ y)      0.0            0.0
                  0.0  0.0       0.0           -1.0
                  0.0  0.0  (/ n-f 2*n*f)  (/ n+f 2*n*f)
                 (/ numer n-f)
                 1.0))

(: perspective-flt3/x-fov (-> Flonum Flonum Flonum Flonum FlProjective3))
(define (perspective-flt3/x-fov x-fov aspect n f)
  (define t (/ 1.0 (fltan (* 0.5 x-fov))))
  (perspective-flt3 t (* t aspect) n f))

(: perspective-flt3/y-fov (-> Flonum Flonum Flonum Flonum FlProjective3))
;; Similar to gluPerspective
(define (perspective-flt3/y-fov y-fov aspect n f)
  (define t (/ 1.0 (fltan (* 0.5 y-fov))))
  (perspective-flt3 (/ t aspect) t n f))

(: perspective-flt3/viewport (-> Flonum Flonum Flonum Flonum Flonum FlProjective3))
(define (perspective-flt3/viewport width height fov n f)
  (define aspect (/ width height))
  (if (> (abs width) (abs height))
      (perspective-flt3/y-fov fov aspect n f)
      (perspective-flt3/x-fov fov aspect n f)))

;; ===================================================================================================
;; Inversion

(: flprojective3-inverse (-> FlProjective3 (U #f FlProjective3)))
(define (flprojective3-inverse t)
  (define 1/det (/ (FlProjective3-forward-det t)))
  (if (and (< (abs 1/det) +inf.0)
           (= (FlProjective3-inverse-den t) 1.0))
      ;; Common case: clearly invertible
      (FlProjective3 (FlProjective3-inverse t)
                     (FlProjective3-forward t)
                     1/det
                     1.0
                     (FlProjective3-inverse-data-vec t)
                     (FlProjective3-forward-data-vec t))
      ;; Just give up
      #f))

;; ===================================================================================================
;; Application

(: flprojective3-apply (-> FlProjective3 FlV4 FlV4))
(define (flprojective3-apply t v)
  (call/flprojective3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
      (call/flv4-values v
        (λ (s0 s1 s2 s3)
          (call/projective3-apply
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            m30 m31 m32 m33
            s0 s1 s2 s3
            flv4))))))

(: flprojective3-apply/pos (-> FlProjective3 FlV3 FlV3))
;; Application to positions: set w = 1.0 and divide by w'
(define (flprojective3-apply/pos t v)
  (call/flprojective3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
      (call/flv3-values v
        (λ (s0 s1 s2)
          (call/projective3-apply
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            m30 m31 m32 m33
            s0 s1 s2 1.0
            (λ (x y z w) (flv3 (/ x w) (/ y w) (/ z w)))))))))

(: flprojective3-apply/dir (-> FlProjective3 FlV3 FlV3))
;; Application to directions: set w = 0.0 and ignore w'
(define (flprojective3-apply/dir t v)
  (call/flprojective3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
      (call/flv3-values v
        (λ (s0 s1 s2)
          (call/projective3-apply
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            m30 m31 m32 m33
            s0 s1 s2 0.0
            (λ (x y z _) (flv3 x y z))))))))

(: flprojective3-apply/norm (-> FlProjective3 FlV3 (U #f FlV3)))
;; Application to normals: set w = 0.0, apply inverse transpose, normalize
(define (flprojective3-apply/norm t v)
  (call/flprojective3-inverse t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33 _den)
      (call/flv3-values v
        (λ (s0 s1 s2)
          (call/projective3-tapply
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            m30 m31 m32 m33
            s0 s1 s2 0.0
            (λ (x y z _) (flnorm3 x y z))))))))

(: flprojective3-apply/plane (-> FlProjective3 FlPlane3 (U #f FlPlane3)))
;; Application to planes: apply inverse transpose, normalize
(define (flprojective3-apply/plane t p)
  (call/flprojective3-inverse t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33 _den)
      (call/flplane3-values p
        (λ (s0 s1 s2 s3)
          (call/projective3-tapply
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            m30 m31 m32 m33
            s0 s1 s2 s3
            make-flplane3))))))

;; ===================================================================================================
;; Composition

(: flprojective3-compose (-> FlProjective3 FlProjective3 FlProjective3))
(define (flprojective3-compose t1 t2)
  (define det1 (FlProjective3-forward-det t1))
  (define det2 (FlProjective3-forward-det t2))
  (call/flprojective3-forward t1
    (λ (s00 s01 s02 s03 s10 s11 s12 s13 s20 s21 s22 s23 s30 s31 s32 s33)
      (call/flprojective3-forward t2
        (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23 n30 n31 n32 n33)
          (call/projective3-compose
            s00 s01 s02 s03
            s10 s11 s12 s13
            s20 s21 s22 s23
            s30 s31 s32 s33
            n00 n01 n02 n03
            n10 n11 n12 n13
            n20 n21 n22 n23
            n30 n31 n32 n33
            (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
              (call/flprojective3-inverse t1
                (λ (s00 s01 s02 s03 s10 s11 s12 s13 s20 s21 s22 s23 s30 s31 s32 s33 den1)
                  (call/flprojective3-inverse t2
                    (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23 n30 n31 n32 n33 den2)
                      (call/projective3-compose
                        n00 n01 n02 n03
                        n10 n11 n12 n13
                        n20 n21 n22 n23
                        n30 n31 n32 n33
                        s00 s01 s02 s03
                        s10 s11 s12 s13
                        s20 s21 s22 s23
                        s30 s31 s32 s33
                        (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23 n30 n31 n32 n33)
                          (flprojective3
                           m00 m01 m02 m03
                           m10 m11 m12 m13
                           m20 m21 m22 m23
                           m30 m31 m32 m33
                           n00 n01 n02 n03
                           n10 n11 n12 n13
                           n20 n21 n22 n23
                           n30 n31 n32 n33
                           (* det1 det2)
                           (* den1 den2)))))))))))))))

;; ===================================================================================================
;; Frustum utils

(define clip-frustum-plane-x- (assert (flplane3 +x-flv3 1.0) values))
(define clip-frustum-plane-x+ (assert (flplane3 -x-flv3 1.0) values))
(define clip-frustum-plane-y- (assert (flplane3 +y-flv3 1.0) values))
(define clip-frustum-plane-y+ (assert (flplane3 -y-flv3 1.0) values))
(define clip-frustum-plane-z- (assert (flplane3 +z-flv3 1.0) values))
(define clip-frustum-plane-z+ (assert (flplane3 -z-flv3 1.0) values))
(define clip-frustum-planes
  (list clip-frustum-plane-x- clip-frustum-plane-x+
        clip-frustum-plane-y- clip-frustum-plane-y+
        clip-frustum-plane-z- clip-frustum-plane-z+))

(: flprojective3-z-near (-> FlProjective3 Flonum))
(define (flprojective3-z-near t)
  (define tinv (flprojective3-inverse t))
  (define p (and tinv (flprojective3-apply/plane tinv clip-frustum-plane-z-)))
  (cond [p  (- (flplane3-distance p))]
        [else  (error 'flprojective3-z-near "cannot get z-near distance from ~a" t)]))

(: flprojective3-z-far (-> FlProjective3 Flonum))
(define (flprojective3-z-far t)
  (define tinv (flprojective3-inverse t))
  (define p (and tinv (flprojective3-apply/plane tinv clip-frustum-plane-z+)))
  (cond [p  (flplane3-distance p)]
        [else  (error 'flprojective3-z-far "cannot get z-far distance from ~a" t)]))

(: flprojective3-frustum-planes (-> FlProjective3 (Listof FlPlane3)))
(define (flprojective3-frustum-planes t)
  (define tinv (flprojective3-inverse t))
  (if tinv
      (for/fold ([planes : (Listof FlPlane3)  empty]) ([p  (in-list clip-frustum-planes)])
        (let ([p  (flprojective3-apply/plane tinv p)])
          (if p (cons p planes) planes)))
      empty))

;; ===================================================================================================
;; Serialization

(: flprojective3-forward-data (-> FlProjective3 F32Vector))
(define (flprojective3-forward-data t)
  (define vec (FlProjective3-forward-data-vec t))
  (if vec vec (let ([vec  (flv16->f32vector (FlProjective3-forward t))])
                (set-FlProjective3-forward-data-vec! t vec)
                vec)))
