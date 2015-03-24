#lang typed/racket/base

(require racket/match
         typed/opengl
         math/flonum
         "../../math.rkt"
         "../../gl.rkt"
         "../../utils.rkt"
         "../types.rkt"
         "types.rkt"
         "flags.rkt")

(provide make-sphere-shape
         set-sphere-shape-color
         set-sphere-shape-emitted
         set-sphere-shape-material
         make-sphere-shape-passes
         sphere-shape-rect
         sphere-shape-easy-transform
         sphere-shape-line-intersect
         )

;; ===================================================================================================
;; Constructors

(: make-sphere-shape (-> FlAffine3 FlV4 FlV4 FlV4 Boolean sphere-shape))
(define (make-sphere-shape t c e m inside?)
  (define fs (flags-join visible-flag (color-opacity-flag c) (color-emitting-flag e)))
  (sphere-shape (lazy-passes) fs t c e m inside?))

;; ===================================================================================================
;; Set attributes

(: set-sphere-shape-color (-> sphere-shape FlV4 sphere-shape))
(define (set-sphere-shape-color a c)
  (match-define (sphere-shape _ fs t old-c e m inside?) a)
  (define new-fs (flags-join (flags-subtract fs opacity-flags)
                             (color-opacity-flag c)))
  (sphere-shape (lazy-passes) new-fs t c e m inside?))

(: set-sphere-shape-emitted (-> sphere-shape FlV4 sphere-shape))
(define (set-sphere-shape-emitted a e)
  (match-define (sphere-shape _ fs t c old-e m inside?) a)
  (define new-fs (flags-join (flags-subtract fs emitting-flags)
                             (color-emitting-flag e)))
  (sphere-shape (lazy-passes) new-fs t c e m inside?))

(: set-sphere-shape-material (-> sphere-shape FlV4 sphere-shape))
(define (set-sphere-shape-material a m)
  (match-define (sphere-shape _ fs t c e old-m inside?) a)
  (sphere-shape (lazy-passes) fs t c e m inside?))

;; ===================================================================================================
;; Drawing passes

(require (prefix-in 30: "sphere-passes/ge_30.rkt")
         (prefix-in 32: "sphere-passes/ge_32.rkt"))

(: make-sphere-shape-passes (-> sphere-shape passes))
(define (make-sphere-shape-passes a)
  (if (gl-version-at-least? 32)
      (32:make-sphere-shape-passes a)
      (30:make-sphere-shape-passes a)))

;; ===================================================================================================
;; Bounding box

(: sphere-shape-rect (-> sphere-shape FlAffine3 FlRect3))
(define (sphere-shape-rect a t)
  (transformed-sphere-flrect3 (flt3compose t (sphere-shape-affine a))))

;; ===================================================================================================
;; Transform

(: sphere-shape-easy-transform (-> sphere-shape FlAffine3 sphere-shape))
(define (sphere-shape-easy-transform a t)
  (match-define (sphere-shape passes fs t0 c e m inside?) a)
  (sphere-shape (lazy-passes) fs (flt3compose t t0) c e m inside?))

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

(: sphere-shape-line-intersect (-> sphere-shape FlV3 FlV3 (U #f line-hit)))
(define (sphere-shape-line-intersect a v dv)
  (let* ([s : FlAffine3  (flt3inverse (sphere-shape-affine a))]
         [sv : FlV3  (flt3apply/pos s v)]
         [sdv : FlV3  (flt3apply/dir s dv)])
    (define-values (tmin tmax) (unit-sphere-line-intersects sv sdv))
    (define inside? (sphere-shape-inside? a))
    (define t (if inside? tmax tmin))
    (and t (line-hit t
                     (flv3fma dv t v)
                     (let ([norm  (flv3normalize (flv3fma sdv t sv))])
                       (and norm (flt3apply/norm (flt3inverse s)
                                                 (if inside? (flv3neg norm) norm))))))))
