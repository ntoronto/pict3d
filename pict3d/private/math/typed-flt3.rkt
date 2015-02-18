#lang typed/racket/base

#|
TODO

Higher precision (try to guarantee 2.5 ulps?)
|#

(require (for-syntax racket/base)
         racket/list
         racket/bool
         racket/match
         racket/unsafe/ops
         math/flonum
         "flv3.rkt"
         "flv4.rkt"
         "flt3-ops.rkt")

(provide
 ;; Types, basic constructors and basic accessors
 FlIdentity3
 flidentity3?
 FlLinear3
 fllinear3?
 FlAffine3
 flaffine3?
 FlProjective3
 flprojective3?
 FlLinear3-
 FlAffine3-
 FlTransform3
 fltransform3-forward
 fltransform3-inverse
 fltransform3-determinant
 fltransform3-1/determinant
 ->fllinear3
 ->flaffine3
 ->flprojective3
 flvector->fllinear3
 cols->fllinear3
 rows->fllinear3
 flvector->flaffine3
 cols->flaffine3
 rows->flaffine3
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
 flt3apply-invtrans
 flt3apply/pos
 flt3apply/nrm
 flt3apply/pln
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

;; ===================================================================================================
;; Types, basic constructors and basic accessors

(struct fltransform3 ([forward : FlVector]
                      [inverse : FlVector]
                      [determinant : Flonum]
                      [1/determinant : Flonum]))

(struct FlProjective3 fltransform3 () #:transparent)
(struct FlAffine3 fltransform3 () #:transparent)
(struct FlLinear3 fltransform3 () #:transparent)
(struct FlIdentity3 () #:transparent)

(define flprojective3 FlProjective3)
(define flaffine3 FlAffine3)
(define fllinear3 FlLinear3)

(define flprojective3? FlProjective3?)
(define flaffine3? FlAffine3?)
(define fllinear3? FlLinear3?)
(define flidentity3? FlIdentity3?)

(define-type FlLinear3-
  (U FlIdentity3
     FlLinear3))

(define-type FlAffine3-
  (U FlLinear3-
     FlAffine3))

(define-type FlTransform3
  (U FlAffine3-
     FlProjective3))

;; ===================================================================================================
;; Conversion

(: ->fllinear3 (-> FlLinear3- FlLinear3))
(define (->fllinear3 m)
  (cond [(flidentity3? m)
         (fllinear3 flv-linear3-identity
                    flv-linear3-identity
                    1.0
                    1.0)]
        [else  m]))

(: ->flaffine3 (-> FlAffine3- FlAffine3))
(define (->flaffine3 m)
  (cond [(flidentity3? m)
         (flaffine3 flv-affine3-identity
                    flv-affine3-identity
                    1.0
                    1.0)]
        [(fllinear3? m)
         (flaffine3 (flv-linear3->affine3 (fltransform3-forward m))
                    (flv-linear3->affine3 (fltransform3-inverse m))
                    (fltransform3-determinant m)
                    (fltransform3-1/determinant m))]
        [else  m]))

(: ->flprojective3 (-> FlTransform3 FlProjective3))
(define (->flprojective3 m)
  (cond [(flidentity3? m)
         (flprojective3 flv-projective3-identity
                        flv-projective3-identity
                        1.0
                        1.0)]
        [(fllinear3? m)
         (flprojective3 (flv-linear3->projective3 (fltransform3-forward m))
                        (flv-linear3->projective3 (fltransform3-inverse m))
                        (fltransform3-determinant m)
                        (fltransform3-1/determinant m))]
        [(flaffine3? m)
         (flprojective3 (flv-affine3->projective3 (fltransform3-forward m))
                        (flv-affine3->projective3 (fltransform3-inverse m))
                        (fltransform3-determinant m)
                        (fltransform3-1/determinant m))]
        [else  m]))

(: flvector->fllinear3 (-> FlVector FlLinear3))
(define (flvector->fllinear3 m)
  (define det (flv-linear3-determinant m))
  (fllinear3 m (flv-linear3-inverse m) det (/ det)))

(: cols->fllinear3 (-> FlVector FlVector FlVector FlLinear3))
(define (cols->fllinear3 x y z)
  (define-values (x0 x1 x2) (flv3-values x))
  (define-values (y0 y1 y2) (flv3-values y))
  (define-values (z0 z1 z2) (flv3-values z))
  (flvector->fllinear3 (flvector x0 y0 z0 x1 y1 z1 x2 y2 z2)))

(: rows->fllinear3 (-> FlVector FlVector FlVector FlLinear3))
(define (rows->fllinear3 r0 r1 r2)
  (define-values (x0 y0 z0) (flv3-values r0))
  (define-values (x1 y1 z1) (flv3-values r1))
  (define-values (x2 y2 z2) (flv3-values r2))
  (flvector->fllinear3 (flvector x0 y0 z0 x1 y1 z1 x2 y2 z2)))

(: flvector->flaffine3 (-> FlVector FlAffine3))
(define (flvector->flaffine3 m)
  (define det (flv-affine3-determinant m))
  (flaffine3 m (flv-affine3-inverse m) det (/ det)))

(: cols->flaffine3 (-> FlVector FlVector FlVector FlVector FlAffine3))
(define (cols->flaffine3 x y z p)
  (define-values (x0 x1 x2) (flv3-values x))
  (define-values (y0 y1 y2) (flv3-values y))
  (define-values (z0 z1 z2) (flv3-values z))
  (define-values (p0 p1 p2) (flv3-values p))
  (flvector->flaffine3 (flvector x0 y0 z0 p0 x1 y1 z1 p1 x2 y2 z2 p2)))

(: rows->flaffine3 (-> FlVector FlVector FlVector FlAffine3))
(define (rows->flaffine3 r0 r1 r2)
  (define-values (x0 y0 z0 p0) (flv4-values r0))
  (define-values (x1 y1 z1 p1) (flv4-values r1))
  (define-values (x2 y2 z2 p2) (flv4-values r2))
  (flvector->flaffine3 (flvector x0 y0 z0 p0 x1 y1 z1 p1 x2 y2 z2 p2)))

;; ===================================================================================================
;; Transformation constructors

(: identity-flt3 FlIdentity3)
(define identity-flt3 (FlIdentity3))

(: scale-flt3 (-> FlVector FlLinear3))
(define (scale-flt3 v)
  (define-values (x y z) (flv3-values v))
  (fllinear3
   (flvector  x  0.0 0.0
             0.0  y  0.0
             0.0 0.0  z )
   (flvector (/ x)  0.0   0.0
              0.0  (/ y)  0.0
              0.0   0.0  (/ z))
   (* x y z)
   (/ (* x y z))))

(: rotate-x-flt3 (-> Flonum FlLinear3))
(define (rotate-x-flt3 rho)
  (define c (flcos rho))
  (define s (flsin rho))
  (fllinear3
   (flvector 1.0  0.0  0.0
             0.0   c  (- s)
             0.0   s    c)
   (flvector 1.0  0.0  0.0
             0.0   c    s
             0.0 (- s)  c)
   1.0
   1.0))

(: rotate-y-flt3 (-> Flonum FlLinear3))
(define (rotate-y-flt3 phi)
  (define c (flcos phi))
  (define s (flsin phi))
  (fllinear3
   (flvector   c   0.0   s
              0.0  1.0  0.0
             (- s) 0.0   c)
   (flvector  c   0.0 (- s)
             0.0  1.0  0.0
              s   0.0   c)
   1.0
   1.0))

(: rotate-z-flt3 (-> Flonum FlLinear3))
(define (rotate-z-flt3 theta)
  (define c (flcos theta))
  (define s (flsin theta))
  (fllinear3
   (flvector  c  (- s)  0.0
              s     c   0.0
             0.0   0.0  1.0)
   (flvector   c    s   0.0
             (- s)  c   0.0
              0.0  0.0  1.0)
   1.0
   1.0))

(: rotate-flt3 (-> FlVector Flonum FlLinear3))
(define (rotate-flt3 axis angle)
  (define-values (x y z) (flv3-values axis))
  (define c (flcos angle))
  (define s (flsin angle))
  (define t (- 1.0 c))
  (fllinear3
   (flvector (+ (* t x x) c)        (- (* t x y) (* z s))  (+ (* t x z) (* y s))
             (+ (* t x y) (* z s))  (+ (* t y y) c)        (- (* t y z) (* x s))
             (- (* t x z) (* y s))  (+ (* t y z) (* x s))  (+ (* t z z) c))
   (flvector (+ (* t x x) c)        (+ (* t x y) (* z s))  (- (* t x z) (* y s))
             (- (* t x y) (* z s))  (+ (* t y y) c)        (+ (* t y z) (* x s))
             (+ (* t x z) (* y s))  (- (* t y z) (* x s))  (+ (* t z z) c))
   1.0
   1.0))

(: translate-flt3 (-> FlVector FlAffine3))
(define (translate-flt3 v)
  (define-values (x y z) (flv3-values v))
  (flaffine3
   (flvector 1.0 0.0 0.0  x
             0.0 1.0 0.0  y
             0.0 0.0 1.0  z)
   (flvector 1.0 0.0 0.0 (- x)
             0.0 1.0 0.0 (- y)
             0.0 0.0 1.0 (- z))
   1.0
   1.0))

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
  (flprojective3
   (flvector (/ 2*n r-l)       0.0     (/ r+l r-l)          0.0
                  0.0     (/ 2*n t-b)  (/ t+b t-b)          0.0
                  0.0          0.0     (/ n+f n-f)  (/ (* 2*n f) n-f)
                  0.0          0.0         -1.0             0.0)
   (flvector (/ r-l 2*n)       0.0          0.0        (/ r+l 2*n)
                  0.0     (/ t-b 2*n)       0.0        (/ t+b 2*n)
                  0.0          0.0          0.0            -1.0
                  0.0          0.0     (/ n-f n*f)     (/ n+f n*f))
   (/ numer denom)
   (/ denom numer)))

(: perspective-flt3 (-> Flonum Flonum Flonum Flonum FlProjective3))
(define (perspective-flt3 x y n f)
  (define n-f (- n f))
  (define n+f (+ n f))
  (define 2*n*f (* 2.0 n f))
  (define numer (* 2*n*f x y))
  (flprojective3
   (flvector   x   0.0       0.0          0.0
              0.0   y        0.0          0.0
              0.0  0.0  (/ n+f n-f)  (/ 2*n*f n-f)
              0.0  0.0      -1.0          0.0)
   (flvector (/ x) 0.0       0.0            0.0
              0.0 (/ y)      0.0            0.0
              0.0  0.0       0.0           -1.0
              0.0  0.0  (/ n-f 2*n*f)  (/ n+f 2*n*f))
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
  (flaffine3
   (flvector (/ -2.0 l-r)       0.0          0.0      (/ l+r l-r)
                  0.0      (/ -2.0 b-t)      0.0      (/ b+t b-t)
                  0.0           0.0      (/ 2.0 n-f)  (/ n+f n-f))
   (flvector (/ l-r -2.0)       0.0          0.0      (/ l+r 2.0)
                  0.0      (/ b-t -2.0)      0.0      (/ b+t 2.0)
                  0.0           0.0      (/ n-f 2.0)  (/ n+f -2.0))
   (/ 8.0 denom)
   (/ denom 8.0)))

;; ===================================================================================================
;; Inversion

(define-syntax-rule (make-inverse make T)
  (λ ([m : T])
    (make (fltransform3-inverse m)
          (fltransform3-forward m)
          (fltransform3-1/determinant m)
          (fltransform3-determinant m))))

(define fllinear3-inverse (make-inverse fllinear3 FlLinear3))
(define flaffine3-inverse (make-inverse flaffine3 FlAffine3))
(define flprojective3-inverse (make-inverse flprojective3 FlProjective3))

(: flt3inverse
   (case-> (-> FlIdentity3    FlIdentity3)
           (-> FlLinear3      FlLinear3)
           (-> FlAffine3      FlAffine3)
           (-> FlProjective3  FlProjective3)
           (-> FlLinear3-     FlLinear3-)
           (-> FlAffine3-     FlAffine3-)
           (-> FlTransform3   FlTransform3)))
(define (flt3inverse m)
  (cond [(flidentity3? m)    m]
        [(fllinear3? m)      (fllinear3-inverse m)]
        [(flaffine3? m)      (flaffine3-inverse m)]
        [(flprojective3? m)  (flprojective3-inverse m)]))

;; ===================================================================================================
;; Application

(define-syntax-rule (make-apply apply T)
  (λ ([m : T] [v : FlVector])
    (apply (fltransform3-forward m) v)))

(define fllinear3-apply (make-apply flv-linear3-apply FlLinear3))
(define flaffine3-apply (make-apply flv-affine3-apply FlAffine3))
(define flprojective3-apply (make-apply flv-projective3-apply FlProjective3))

(: flt3apply (-> FlTransform3 FlVector FlVector))
(define (flt3apply m v)
  (cond [(flidentity3? m)    v]
        [(fllinear3? m)      (fllinear3-apply m v)]
        [(flaffine3? m)      (flaffine3-apply m v)]
        [(flprojective3? m)  (flprojective3-apply m v)]))

;; ===================================================================================================
;; Inverse-transpose application

(define-syntax-rule (make-invtrans-apply tapply T)
  (λ ([m : T] [v : FlVector])
    (tapply (fltransform3-inverse m) v)))

(define fllinear3-invtrans-apply (make-invtrans-apply flv-linear3-tapply FlLinear3))
(define flaffine3-invtrans-apply (make-invtrans-apply flv-affine3-tapply FlAffine3))
(define flprojective3-invtrans-apply (make-invtrans-apply flv-projective3-tapply FlProjective3))

(: flt3apply-invtrans (-> FlTransform3 FlVector FlVector))
(define (flt3apply-invtrans m v)
  (cond [(flidentity3? m)    v]
        [(fllinear3? m)      (fllinear3-invtrans-apply m v)]
        [(flaffine3? m)      (flaffine3-invtrans-apply m v)]
        [(flprojective3? m)  (flprojective3-invtrans-apply m v)]))

;; ===================================================================================================

(: flt3apply/pos (-> FlTransform3 FlVector FlVector))
(define (flt3apply/pos t v)
  (flv4->pos (flt3apply t (pos->flv4 v))))

(: flt3apply/nrm (-> FlTransform3 FlVector FlVector))
(define (flt3apply/nrm t v)
  (let ([v  (flv3normalize (flv4->norm (flt3apply-invtrans t (norm->flv4 v))))])
    (if v v (flvector 0.0 0.0 0.0))))

(: flt3apply/pln (-> FlTransform3 FlPlane3 (U #f FlPlane3)))
(define (flt3apply/pln t p)
  (flv4->flplane3 (flt3apply-invtrans t (flplane3->flv4 p))))

;; ===================================================================================================
;; Composition

(define-syntax-rule (make-compose make compose T)
  (λ ([m : T] [n : T])
    (make (compose (fltransform3-forward m)
                   (fltransform3-forward n))
          (compose (fltransform3-inverse n)
                   (fltransform3-inverse m))
          (* (fltransform3-determinant m)
             (fltransform3-determinant n))
          (* (fltransform3-1/determinant n)
             (fltransform3-1/determinant m)))))

(define fllinear3-compose (make-compose fllinear3 flv-linear3-compose FlLinear3))
(define flaffine3-compose (make-compose flaffine3 flv-affine3-compose FlAffine3))
(define flprojective3-compose (make-compose flprojective3 flv-projective3-compose FlProjective3))

(: flt3compose (case-> (-> FlIdentity3   FlIdentity3   FlIdentity3)
                       (-> FlLinear3-    FlLinear3     FlLinear3)
                       (-> FlLinear3     FlLinear3-    FlLinear3)
                       (-> FlLinear3-    FlLinear3-    FlLinear3-)
                       (-> FlAffine3-    FlAffine3     FlAffine3)
                       (-> FlAffine3     FlAffine3-    FlAffine3)
                       (-> FlAffine3-    FlAffine3-    FlAffine3-)
                       (-> FlTransform3  FlProjective3 FlProjective3)
                       (-> FlProjective3 FlTransform3  FlProjective3)
                       (-> FlTransform3  FlTransform3  FlTransform3)))
(define (flt3compose m n)
  (cond [(flidentity3? m)  n]
        [(flidentity3? n)  m]
        [(flprojective3? m)  (flprojective3-compose m (->flprojective3 n))]
        [(flprojective3? n)  (flprojective3-compose (->flprojective3 m) n)]
        [(fllinear3? m)
         (if (fllinear3? n)
             (fllinear3-compose m n)
             (flaffine3-compose (->flaffine3 m) n))]
        [else
         (if (fllinear3? n)
             (flaffine3-compose m (->flaffine3 n))
             (flaffine3-compose m n))]))

;; ===================================================================================================
;; Handedness consistency

(: flt3consistent? (-> FlTransform3 Boolean))
(define (flt3consistent? m)
  (or (flidentity3? m)
      (> (fltransform3-determinant m) 0.0)
      (> (fltransform3-1/determinant m) 0.0)))

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

(: flprojective3-z-near (-> FlProjective3 Flonum))
(define (flprojective3-z-near t)
  (define p (flt3apply/pln (flt3inverse t) clip-frustum-plane-z-))
  (cond [p  (- (flplane3-distance p))]
        [else  (error 'flprojective3-z-near "cannot get z-near distance from ~a" t)]))

(: flprojective3-z-far (-> FlProjective3 Flonum))
(define (flprojective3-z-far t)
  (define p (flt3apply/pln (flt3inverse t) clip-frustum-plane-z+))
  (cond [p  (flplane3-distance p)]
        [else  (error 'flprojective3-z-far "cannot get z-far distance from ~a" t)]))

(: flprojective3-frustum-planes (-> FlProjective3 (Listof FlPlane3)))
(define (flprojective3-frustum-planes t)
  (define tinv (flt3inverse t))
  (for/fold ([planes : (Listof FlPlane3)  empty]) ([p  (in-list clip-frustum-planes)])
    (let ([p  (flt3apply/pln tinv p)])
      (if p (cons p planes) planes))))
