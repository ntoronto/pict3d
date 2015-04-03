#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         typed/opengl
         math/flonum
         "../../../math.rkt"
         "../../scene.rkt"
         "../../draw.rkt"
         "sphere-type.rkt"
         (prefix-in 30: "ge_30.rkt")
         (prefix-in 32: "ge_32.rkt"))

(provide make-sphere-shape
         (struct-out sphere-shape))

;; ===================================================================================================
;; Constructors

(: make-sphere-shape (-> FlAffine3 FlV4 FlV4 FlV4 Boolean sphere-shape))
(define (make-sphere-shape t c e m inside?)
  (sphere-shape (lazy-passes) sphere-shape-functions
                t c e m inside?))

;; ===================================================================================================
;; Set attributes

(: set-sphere-shape-color (-> shape FlV4 sphere-shape))
(define (set-sphere-shape-color a c)
  (match-define (sphere-shape _ _ t _ e m inside?) a)
  (make-sphere-shape t c e m inside?))

(: set-sphere-shape-emitted (-> shape FlV4 sphere-shape))
(define (set-sphere-shape-emitted a e)
  (match-define (sphere-shape _ _ t c _ m inside?) a)
  (make-sphere-shape t c e m inside?))

(: set-sphere-shape-material (-> shape FlV4 sphere-shape))
(define (set-sphere-shape-material a m)
  (match-define (sphere-shape _ _ t c e _ inside?) a)
  (make-sphere-shape t c e m inside?))

;; ===================================================================================================
;; Drawing passes

(: get-sphere-shape-passes (-> shape passes))
(define (get-sphere-shape-passes s)
  (if (gl-version-at-least? 32)
      (32:get-sphere-shape-passes s)
      (30:get-sphere-shape-passes s)))

;; ===================================================================================================
;; Bounding box

(: get-sphere-shape-bbox (-> shape FlAffine3 bbox))
(define (get-sphere-shape-bbox s t)
  (let ([s  (assert s sphere-shape?)])
    (bbox (transformed-sphere-flrect3 (flt3compose t (sphere-shape-affine s)))
          0.0)))

;; ===================================================================================================
;; Transform

(: sphere-shape-transform (-> shape FlAffine3 sphere-shape))
(define (sphere-shape-transform s t)
  (match-define (sphere-shape _ _ t0 c e m inside?) s)
  (make-sphere-shape (flt3compose t t0) c e m inside?))

;; ===================================================================================================
;; Ray intersection

;; Minimum discriminant would normally be 0.0, but floating-point error could make rays wrongly miss
;; This makes the sphere a little fatter in the plane perpendicular to the ray to try to make up for
;; it, and also makes edge-grazing intersections more likely - don't know whether that's a good thing
(define discr-min (* -128.0 epsilon.0))

(: unit-sphere-line-intersects (-> FlV3 FlV3 (Values (U #f Flonum) (U #f Flonum))))
(define (unit-sphere-line-intersects p d)
  (define m^2 (flv3mag^2 d))
  (define b (/ (- (flv3dot p d)) m^2))
  (define c (/ (- (flv3mag^2 p) 1.0) m^2))
  (let ([discr  (- (* b b) c)])
    (if (< discr discr-min)
        (values #f #f)  ; Missed sphere
        (let* ([q  (flsqrt (max 0.0 discr))])
          (values (- b q) (+ b q))))))

(: sphere-shape-ray-intersect (-> shape FlV3 FlV3 Nonnegative-Flonum
                                  (Values (U #f Nonnegative-Flonum) (U #f (Promise trace-data)))))
(define (sphere-shape-ray-intersect s v dv max-time)
  (let ([s  (assert s sphere-shape?)])
    (define t (sphere-shape-affine s))
    (define inside? (sphere-shape-inside? s))
    ;; Convert ray to local coordinates
    (define tinv (flt3inverse t))
    (define sv (flt3apply/pos tinv v))
    (define sdv (flt3apply/dir tinv dv))
    ;; Compute intersection
    (define-values (tmin tmax) (unit-sphere-line-intersects sv sdv))
    (define time (if inside? tmax tmin))
    (cond [(and time (>= time 0.0) (<= time max-time))
           ;; Note: time can't be +nan.0 (which can happen when dv is zero-flv3)
           (define data
             (delay (define p (flv3fma dv time v))
                    (define n (let ([n  (flv3normalize (flv3fma sdv time sv))])
                                (and n (flt3apply/norm t (if inside? (flv3neg n) n)))))
                    (trace-data p n empty)))
           (values time data)]
          [else
           (values #f #f)])))

;; ===================================================================================================

(define sphere-shape-functions
  (shape-functions
   set-sphere-shape-color
   set-sphere-shape-emitted
   set-sphere-shape-material
   get-sphere-shape-passes
   (λ (s kind t) (and (eq? kind 'visible) (get-sphere-shape-bbox s t)))
   sphere-shape-transform
   (λ (s t) (list (sphere-shape-transform s t)))
   sphere-shape-ray-intersect))
