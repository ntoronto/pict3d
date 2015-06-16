#lang typed/racket/base

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
         "../utils.rkt"
         "../ffi.rkt")

(provide (except-out
          (all-defined-out)
          ;; These aren't proper abstractions because they don't rename arguments
          flaffine3
          flprojective3))

;; ===================================================================================================
;; Affine transforms

(define print-flaffine3
  (make-constructor-style-printer
   (λ ([t : FlAffine3]) 'flaffine3)
   (λ ([t : FlAffine3])
     (let ([fwd  (call/flaffine3-forward t list)]
           [inv  (call/flaffine3-inverse t list)]
           [dets  (list (FlAffine3-determinant t)
                        (FlAffine3-1/determinant t))])
       (append fwd inv dets)))))

(struct FlAffine3 ([forward : FlVector]
                   [inverse : FlVector]
                   [determinant : Flonum]
                   [1/determinant : Flonum]
                   [forward-data-vec : (U #f F32Vector)]
                   [inverse-data-vec : (U #f F32Vector)])
  #:transparent
  #:mutable
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-flaffine3)

(define-syntax flaffine3? (make-rename-transformer #'FlAffine3?))

(define-syntax-rule (call/flaffine3-forward t f)
  (call/flv12-values (FlAffine3-forward t) f))

(define-syntax-rule (call/flaffine3-inverse t f)
  (call/flv12-values (FlAffine3-inverse t) f))

;; ===================================================================================================
;; Projective transforms

(define print-flprojective3
  (make-constructor-style-printer
   (λ ([t : FlProjective3]) 'flprojective3)
   (λ ([t : FlProjective3])
     (let ([fwd  (call/flprojective3-forward t list)]
           [inv  (call/flprojective3-inverse t list)]
           [dets  (list (FlProjective3-determinant t)
                        (FlProjective3-1/determinant t))])
       (append fwd inv dets)))))

(struct FlProjective3 ([forward : FlVector]
                       [inverse : FlVector]
                       [determinant : Flonum]
                       [1/determinant : Flonum]
                       [forward-data-vec : (U #f F32Vector)]
                       [inverse-data-vec : (U #f F32Vector)])
  #:transparent
  #:mutable
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-flprojective3)

(define-syntax flprojective3? (make-rename-transformer #'FlProjective3?))

(define-syntax-rule (call/flprojective3-forward t f)
  (call/flv16-values (FlProjective3-forward t) f))

(define-syntax-rule (call/flprojective3-inverse t f)
  (call/flv16-values (FlProjective3-inverse t) f))

;; ===================================================================================================

(define-type FlTransform3 (U FlAffine3 FlProjective3))

(define fltransform3? (λ (v) (or (flaffine3? v) (flprojective3? v))))

;; ===================================================================================================

(define-syntax flaffine3-determinant (make-rename-transformer #'FlAffine3-determinant))
(define-syntax flaffine3-1/determinant (make-rename-transformer #'FlAffine3-1/determinant))

(define-syntax flprojective3-determinant (make-rename-transformer #'FlProjective3-determinant))
(define-syntax flprojective3-1/determinant (make-rename-transformer #'FlProjective3-1/determinant))

(: fltransform3-determinant (-> FlTransform3 Flonum))
(define (fltransform3-determinant t)
  (if (flaffine3? t)
      (flaffine3-determinant t)
      (flprojective3-determinant t)))

(: fltransform3-1/determinant (-> FlTransform3 Flonum))
(define (fltransform3-1/determinant t)
  (if (flaffine3? t)
      (flaffine3-1/determinant t)
      (flprojective3-1/determinant t)))

;; ===================================================================================================

(: check-flvector (-> Symbol FlVector FlVector))
(define (check-flvector name vs)
  (for ([i : Nonnegative-Fixnum  (in-range (flvector-length vs))])
    (define v (unsafe-flvector-ref vs i))
    (unless (< -inf.0 v +inf.0)
      (error name "expected rationals; given ~e" vs)))
  vs)

#;
(: flaffine3
   (-> Flonum Flonum Flonum Flonum 
       Flonum Flonum Flonum Flonum 
       Flonum Flonum Flonum Flonum 
       Flonum Flonum Flonum Flonum 
       Flonum Flonum Flonum Flonum 
       Flonum Flonum Flonum Flonum 
       Flonum
       Flonum
       FlAffine3))
(define-syntax-rule
  (flaffine3 m00 m01 m02 m03
             m10 m11 m12 m13
             m20 m21 m22 m23
             n00 n01 n02 n03
             n10 n11 n12 n13
             n20 n21 n22 n23
             det-stx 1/det-stx)
  (let ([  det : Flonum    det-stx]
        [1/det : Flonum  1/det-stx])
    (if (and (< -inf.0 (min det 1/det))
             (< (max det 1/det) +inf.0))
        (FlAffine3
         (check-flvector 'flaffine3 (flvector m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23))
         (check-flvector 'flaffine3 (flvector n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23))
         det
         1/det
         #f
         #f)
        (error 'flaffine3 "expected rational determinants; given ~e ~e" det 1/det))))

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

#;
(: flprojective3
   (-> Flonum Flonum Flonum Flonum 
       Flonum Flonum Flonum Flonum 
       Flonum Flonum Flonum Flonum 
       Flonum Flonum Flonum Flonum 
       Flonum Flonum Flonum Flonum 
       Flonum Flonum Flonum Flonum 
       Flonum Flonum Flonum Flonum 
       Flonum Flonum Flonum Flonum 
       Flonum
       Flonum
       FlProjective3))
(define-syntax-rule
  (flprojective3 m00 m01 m02 m03
                 m10 m11 m12 m13
                 m20 m21 m22 m23
                 m30 m31 m32 m33
                 n00 n01 n02 n03
                 n10 n11 n12 n13
                 n20 n21 n22 n23
                 n30 n31 n32 n33
                 det-stx 1/det-stx)
  (let ([  det : Flonum    det-stx]
        [1/det : Flonum  1/det-stx])
    (if (and (< -inf.0 (min det 1/det))
             (< (max det 1/det) +inf.0))
        (FlProjective3
         (check-flvector 'flprojective3
                         (flvector m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33))
         (check-flvector 'flprojective3
                         (flvector n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23 n30 n31 n32 n33))
         det
         1/det
         #f
         #f)
        (error 'flprojective3 "expected rational determinants; given ~e ~e" det 1/det))))

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

;; ===================================================================================================
;; Conversion

(: ->flprojective3 (-> FlTransform3 FlProjective3))
(define (->flprojective3 t)
  (cond
    [(flaffine3? t)
     (call/flaffine3-forward t
       (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
         (call/flaffine3-inverse t
           (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23)
             (flprojective3 m00 m01 m02 m03
                            m10 m11 m12 m13
                            m20 m21 m22 m23
                            0.0 0.0 0.0 1.0
                            n00 n01 n02 n03
                            n10 n11 n12 n13
                            n20 n21 n22 n23
                            0.0 0.0 0.0 1.0
                            (flaffine3-determinant t)
                            (flaffine3-1/determinant t))))))]
    [else  t]))

(: cols->flaffine3 (-> FlV3 FlV3 FlV3 FlV3 (U #f FlAffine3)))
(define (cols->flaffine3 x y z p)
  (call/flv3-values x
    (λ (m00 m10 m20)
      (call/flv3-values y
        (λ (m01 m11 m21)
          (call/flv3-values z
            (λ (m02 m12 m22)
              (call/flv3-values p
                (λ (m03 m13 m23)
                  (call/affine3-inverse*det m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23
                    (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23 det)
                      (define 1/det (/ det))
                      (and (< -inf.0 (min det 1/det))
                           (< (max det 1/det) +inf.0)
                           (flaffine3
                            m00 m01 m02 m03
                            m10 m11 m12 m13
                            m20 m21 m22 m23
                            (/ n00 det) (/ n01 det) (/ n02 det) (/ n03 det)
                            (/ n10 det) (/ n11 det) (/ n12 det) (/ n13 det)
                            (/ n20 det) (/ n21 det) (/ n22 det) (/ n23 det)
                            det (/ det))))))))))))))

;; ===================================================================================================
;; Transformation constructors

(: scale-x-flt3 (-> Flonum FlAffine3))
(define (scale-x-flt3 v)
  (flaffine3   v   0.0  0.0  0.0
              0.0  1.0  0.0  0.0
              0.0  0.0  1.0  0.0
             (/ v) 0.0  0.0  0.0
              0.0  1.0  0.0  0.0
              0.0  0.0  1.0  0.0
              v (/ v)))

(: scale-y-flt3 (-> Flonum FlAffine3))
(define (scale-y-flt3 v)
  (flaffine3 1.0  0.0  0.0  0.0
             0.0   v  0.0  0.0
             0.0  0.0  1.0  0.0
             1.0  0.0  0.0  0.0
             0.0 (/ v) 0.0  0.0
             0.0  0.0  1.0  0.0
             v (/ v)))

(: scale-z-flt3 (-> Flonum FlAffine3))
(define (scale-z-flt3 v)
  (flaffine3 1.0  0.0  0.0  0.0
             0.0  1.0  0.0  0.0
             0.0  0.0   v   0.0
             1.0  0.0  0.0  0.0
             0.0  1.0  0.0  0.0
             0.0  0.0 (/ v) 0.0
             v (/ v)))

(: scale-flt3 (-> (U Flonum FlV3) FlAffine3))
(define (scale-flt3 v)
  (if (flonum? v)
      (let ([det  (* v v v)])
        (flaffine3   v   0.0  0.0  0.0
                    0.0   v   0.0  0.0
                    0.0  0.0   v   0.0
                   (/ v) 0.0  0.0  0.0
                    0.0 (/ v) 0.0  0.0
                    0.0  0.0 (/ v) 0.0
                    det (/ det)))
      (call/flv3-values v
        (λ (x y z)
          (define det (* x y z))
          (flaffine3   x   0.0  0.0  0.0
                      0.0   y   0.0  0.0
                      0.0  0.0   z   0.0
                     (/ x) 0.0  0.0  0.0
                      0.0 (/ y) 0.0  0.0
                      0.0  0.0 (/ z) 0.0
                      det (/ det))))))

(: rotate-x-flt3 (-> Flonum FlAffine3))
(define (rotate-x-flt3 rho)
  (define c (flcos rho))
  (define s (flsin rho))
  (flaffine3 1.0  0.0  0.0  0.0
             0.0   c  (- s) 0.0
             0.0   s    c   0.0
             1.0  0.0  0.0  0.0
             0.0   c    s   0.0
             0.0 (- s)  c   0.0
             1.0 1.0))

(: rotate-y-flt3 (-> Flonum FlAffine3))
(define (rotate-y-flt3 phi)
  (define c (flcos phi))
  (define s (flsin phi))
  (flaffine3   c   0.0   s   0.0
              0.0  1.0  0.0  0.0
             (- s) 0.0   c   0.0
               c   0.0 (- s) 0.0
              0.0  1.0  0.0  0.0
               s   0.0   c   0.0
             1.0 1.0))

(: rotate-z-flt3 (-> Flonum FlAffine3))
(define (rotate-z-flt3 theta)
  (define c (flcos theta))
  (define s (flsin theta))
  (flaffine3   c  (- s) 0.0  0.0
               s    c   0.0  0.0
              0.0  0.0  1.0  0.0
               c    s   0.0  0.0
             (- s)  c   0.0  0.0
              0.0  0.0  1.0  0.0
             1.0 1.0))

(: rotate-flt3 (-> FlV3 Flonum FlAffine3))
(define (rotate-flt3 axis angle)
  (call/flv3-values axis
    (λ (x y z)
      (define c (flcos angle))
      (define s (flsin angle))
      (define t (- 1.0 c))
      (flaffine3 (+ (* t x x) c)        (- (* t x y) (* z s))  (+ (* t x z) (* y s))  0.0
                 (+ (* t x y) (* z s))  (+ (* t y y) c)        (- (* t y z) (* x s))  0.0
                 (- (* t x z) (* y s))  (+ (* t y z) (* x s))  (+ (* t z z) c)        0.0
                 (+ (* t x x) c)        (+ (* t x y) (* z s))  (- (* t x z) (* y s))  0.0
                 (- (* t x y) (* z s))  (+ (* t y y) c)        (+ (* t y z) (* x s))  0.0
                 (+ (* t x z) (* y s))  (- (* t y z) (* x s))  (+ (* t z z) c)        0.0
                 1.0 1.0))))

(: move-x-flt3 (-> Flonum FlAffine3))
(define (move-x-flt3 v)
  (flaffine3 1.0 0.0 0.0   v
             0.0 1.0 0.0  0.0
             0.0 0.0 1.0  0.0
             1.0 0.0 0.0 (- v)
             0.0 1.0 0.0  0.0
             0.0 0.0 1.0  0.0
             1.0 1.0))

(: move-y-flt3 (-> Flonum FlAffine3))
(define (move-y-flt3 v)
  (flaffine3 1.0 0.0 0.0  0.0
             0.0 1.0 0.0   v
             0.0 0.0 1.0  0.0
             1.0 0.0 0.0  0.0
             0.0 1.0 0.0 (- v)
             0.0 0.0 1.0  0.0
             1.0 1.0))

(: move-z-flt3 (-> Flonum FlAffine3))
(define (move-z-flt3 v)
  (flaffine3 1.0 0.0 0.0  0.0
             0.0 1.0 0.0  0.0
             0.0 0.0 1.0   v
             1.0 0.0 0.0  0.0
             0.0 1.0 0.0  0.0
             0.0 0.0 1.0 (- v)
             1.0 1.0))

(: move-flt3 (-> FlV3 FlAffine3))
(define (move-flt3 v)
  (call/flv3-values v
    (λ (x y z)
      (flaffine3 1.0 0.0 0.0   x
                 0.0 1.0 0.0   y
                 0.0 0.0 1.0   z
                 1.0 0.0 0.0 (- x)
                 0.0 1.0 0.0 (- y)
                 0.0 0.0 1.0 (- z)
                 1.0 1.0))))

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
                 (/ denom numer)))

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
                 (/ n-f numer)))

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
              [t0 : FlAffine3  (rotate-z-flt3 angle)])
          (flt3compose t t0))))))

;; ===================================================================================================
;; Inversion

(: flaffine3-inverse (-> FlAffine3 FlAffine3))
(define (flaffine3-inverse t)
  (FlAffine3
   (FlAffine3-inverse t)
   (FlAffine3-forward t)
   (flaffine3-1/determinant t)
   (flaffine3-determinant t)
   (FlAffine3-inverse-data-vec t)
   (FlAffine3-forward-data-vec t)))

(: flprojective3-inverse (-> FlProjective3 FlProjective3))
(define (flprojective3-inverse t)
  (FlProjective3
   (FlProjective3-inverse t)
   (FlProjective3-forward t)
   (flprojective3-1/determinant t)
   (flprojective3-determinant t)
   (FlProjective3-inverse-data-vec t)
   (FlProjective3-forward-data-vec t)))

(: flt3inverse (case-> (-> FlAffine3      FlAffine3)
                       (-> FlProjective3  FlProjective3)
                       (-> FlTransform3   FlTransform3)))
(define (flt3inverse t)
  (if (flaffine3? t)
      (flaffine3-inverse t)
      (flprojective3-inverse t)))

;; ===================================================================================================
;; FlV4 application

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

(: flt3apply (-> FlTransform3 FlV4 FlV4))
(define (flt3apply t v)
  (if (flaffine3? t)
      (flaffine3-apply t v)
      (flprojective3-apply t v)))

;; ===================================================================================================
;; Application to positions: set w = 1.0 and divide by w'

(: flaffine3-apply/pos (-> FlAffine3 FlV3 FlV3))
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

(: flprojective3-apply/pos (-> FlProjective3 FlV3 FlV3))
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

(: flt3apply/pos (-> FlTransform3 FlV3 FlV3))
(define (flt3apply/pos t v)
  (if (flaffine3? t)
      (flaffine3-apply/pos t v)
      (flprojective3-apply/pos t v)))

;; ===================================================================================================
;; Application to directions: set w = 0.0 and ignore w'

(: flaffine3-apply/dir (-> FlAffine3 FlV3 FlV3))
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

(: flprojective3-apply/dir (-> FlProjective3 FlV3 FlV3))
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

(: flt3apply/dir (-> FlTransform3 FlV3 FlV3))
(define (flt3apply/dir t v)
  (if (flaffine3? t)
      (flaffine3-apply/dir t v)
      (flprojective3-apply/dir t v)))

;; ===================================================================================================
;; Application to normals: set w = 0.0, apply inverse transpose, normalize

(: flaffine3-apply/norm (-> FlAffine3 FlV3 (U #f FlV3)))
(define (flaffine3-apply/norm t v)
  (call/flaffine3-inverse t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (call/flv3-values v
        (λ (s0 s1 s2)
          (call/affine3-tapply
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            s0 s1 s2 0.0
            (λ (x y z _) (flnorm3 x y z))))))))

(: flprojective3-apply/norm (-> FlProjective3 FlV3 (U #f FlV3)))
(define (flprojective3-apply/norm t v)
  (call/flprojective3-inverse t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
      (call/flv3-values v
        (λ (s0 s1 s2)
          (call/projective3-tapply
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            m30 m31 m32 m33
            s0 s1 s2 0.0
            (λ (x y z _) (flnorm3 x y z))))))))

(: flt3apply/norm (-> FlTransform3 FlV3 (U #f FlV3)))
(define (flt3apply/norm t v)
  (if (flaffine3? t)
      (flaffine3-apply/norm t v)
      (flprojective3-apply/norm t v)))

;; ===================================================================================================
;; Application to planes: apply inverse transpose, normalize

(: flaffine3-apply/plane (-> FlAffine3 FlPlane3 (U #f FlPlane3)))
(define (flaffine3-apply/plane t p)
  (call/flaffine3-inverse t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (call/flplane3-values p
        (λ (s0 s1 s2 s3)
          (call/affine3-tapply
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            s0 s1 s2 s3
            make-flplane3))))))

(: flprojective3-apply/plane (-> FlProjective3 FlPlane3 (U #f FlPlane3)))
(define (flprojective3-apply/plane t p)
  (call/flprojective3-inverse t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
      (call/flplane3-values p
        (λ (s0 s1 s2 s3)
          (call/projective3-tapply
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            m30 m31 m32 m33
            s0 s1 s2 s3
            make-flplane3))))))

(: flt3apply/plane (-> FlTransform3 FlPlane3 (U #f FlPlane3)))
(define (flt3apply/plane t p)
  (if (flaffine3? t)
      (flaffine3-apply/plane t p)
      (flprojective3-apply/plane t p)))

;; ===================================================================================================
;; Composition

(: flaffine3-compose (-> FlAffine3 FlAffine3 FlAffine3))
(define (flaffine3-compose t1 t2)
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
                (λ (s00 s01 s02 s03 s10 s11 s12 s13 s20 s21 s22 s23)
                  (call/flaffine3-inverse t2
                    (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23)
                      (call/affine3-compose
                        n00 n01 n02 n03
                        n10 n11 n12 n13
                        n20 n21 n22 n23
                        s00 s01 s02 s03
                        s10 s11 s12 s13
                        s20 s21 s22 s23
                        (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23)
                          (flaffine3
                           m00 m01 m02 m03
                           m10 m11 m12 m13
                           m20 m21 m22 m23
                           n00 n01 n02 n03
                           n10 n11 n12 n13
                           n20 n21 n22 n23
                           (* (flaffine3-determinant t1)
                              (flaffine3-determinant t2))
                           (* (flaffine3-1/determinant t1)
                              (flaffine3-1/determinant t2))))))))))))))))

(: flprojective3-compose (-> FlProjective3 FlProjective3 FlProjective3))
(define (flprojective3-compose t1 t2)
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
                (λ (s00 s01 s02 s03 s10 s11 s12 s13 s20 s21 s22 s23 s30 s31 s32 s33)
                  (call/flprojective3-inverse t2
                    (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23 n30 n31 n32 n33)
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
                           (* (flprojective3-determinant t1)
                              (flprojective3-determinant t2))
                           (* (flprojective3-1/determinant t1)
                              (flprojective3-1/determinant t2))))))))))))))))

(: flt3compose (case-> (-> FlAffine3     FlAffine3     FlAffine3)
                       (-> FlTransform3  FlProjective3 FlProjective3)
                       (-> FlProjective3 FlTransform3  FlProjective3)
                       (-> FlTransform3  FlTransform3  FlTransform3)))
(define (flt3compose m n)
  (cond [(flprojective3? m)  (flprojective3-compose m (->flprojective3 n))]
        [(flprojective3? n)  (flprojective3-compose (->flprojective3 m) n)]
        [else  (flaffine3-compose m n)]))

;; ===================================================================================================

(: v3axis? (-> Flonum Flonum Flonum Nonnegative-Flonum Boolean))
(define (v3axis? x y z tol)
  (define m (/ 1.0 (max (abs x) (abs y) (abs z))))
  (cond [(< -inf.0 m +inf.0)
         (cond [(= (abs (* x m)) 1.0)  (< (max (abs (* y m)) (abs (* z m))) tol)]
               [(= (abs (* y m)) 1.0)  (< (max (abs (* z m)) (abs (* x m))) tol)]
               [else                   (< (max (abs (* x m)) (abs (* y m))) tol)])]
        [else  #f]))

(: flt3axial? (-> FlAffine3 Nonnegative-Flonum Boolean))
(define (flt3axial? t tol)
  (call/flaffine3-forward t
    (λ (m00 m01 m02 _m03 m10 m11 m12 _m13 m20 m21 m22 _m23)
      (and (v3axis? m00 m10 m20 tol)
           (v3axis? m01 m11 m21 tol)
           (v3axis? m02 m12 m22 tol)))))

;; ===================================================================================================
;; Handedness consistency

(: flt3consistent? (-> FlTransform3 Boolean))
(define (flt3consistent? m)
  (or (> (fltransform3-determinant m) 0.0)
      (> (fltransform3-1/determinant m) 0.0)))

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
  (let* ([tinv : FlTransform3  (flt3inverse t)]
         [p : (U #f FlPlane3)  (flt3apply/plane tinv clip-frustum-plane-z-)])
    (cond [p  (- (flplane3-distance p))]
          [else  (error 'flprojective3-z-near "cannot get z-near distance from ~a" t)])))

(: flprojective3-z-far (-> FlProjective3 Flonum))
(define (flprojective3-z-far t)
  (let* ([tinv : FlTransform3  (flt3inverse t)]
         [p : (U #f FlPlane3)  (flt3apply/plane tinv clip-frustum-plane-z+)])
    (cond [p  (flplane3-distance p)]
          [else  (error 'flprojective3-z-far "cannot get z-far distance from ~a" t)])))

(: flprojective3-frustum-planes (-> FlProjective3 (Listof FlPlane3)))
(define (flprojective3-frustum-planes t)
  (let ([tinv : FlTransform3  (flt3inverse t)])
    (for/fold ([planes : (Listof FlPlane3)  empty]) ([p  (in-list clip-frustum-planes)])
      (let ([p  (flt3apply/plane tinv p)])
        (if p (cons p planes) planes)))))

;; ===================================================================================================
;; Serialization

(: flaffine3-forward-data (-> FlAffine3 F32Vector))
(define (flaffine3-forward-data t)
  (define vec (FlAffine3-forward-data-vec t))
  (if vec vec (let ([vec  (flv12->f32vector (FlAffine3-forward t))])
                (set-FlAffine3-forward-data-vec! t vec)
                vec)))

(: flaffine3-inverse-data (-> FlAffine3 F32Vector))
(define (flaffine3-inverse-data t)
  (define vec (FlAffine3-inverse-data-vec t))
  (if vec vec (let ([vec  (flv12->f32vector (FlAffine3-inverse t))])
                (set-FlAffine3-inverse-data-vec! t vec)
                vec)))

(: flprojective3-forward-data (-> FlProjective3 F32Vector))
(define (flprojective3-forward-data t)
  (define vec (FlProjective3-forward-data-vec t))
  (if vec vec (let ([vec  (flv16->f32vector (FlProjective3-forward t))])
                (set-FlProjective3-forward-data-vec! t vec)
                vec)))

(: flprojective3-inverse-data (-> FlProjective3 F32Vector))
(define (flprojective3-inverse-data t)
  (define vec (FlProjective3-inverse-data-vec t))
  (if vec vec (let ([vec  (flv16->f32vector (FlProjective3-inverse t))])
                (set-FlProjective3-inverse-data-vec! t vec)
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
