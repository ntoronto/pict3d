#lang typed/racket/base

(require racket/match
         typed/opengl
         math/flonum
         "../../math/flv3.rkt"
         "../../math/flt3.rkt"
         "../../math/flrect3.rkt"
         "../../gl.rkt"
         "../../utils.rkt"
         "../types.rkt"
         "../draw-pass.rkt"
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

(: make-sphere-shape (-> Affine FlVector FlVector material Boolean sphere-shape))
(define (make-sphere-shape t c e m inside?)
  (cond [(not (= 4 (flvector-length c)))
         (raise-argument-error 'make-rectangle-shape "length-4 flvector" 1 t c e m inside?)]
        [(not (= 4 (flvector-length e)))
         (raise-argument-error 'make-rectangle-shape "length-4 flvector" 2 t c e m inside?)]
        [else
         (define fs (flags-join visible-flag (color-opacity-flag c) (color-emitting-flag e)))
         (sphere-shape (lazy-passes) fs t c e m inside?)]))

;; ===================================================================================================
;; Set attributes

(: set-sphere-shape-color (-> sphere-shape FlVector sphere-shape))
(define (set-sphere-shape-color a c)
  (cond [(not (= (flvector-length c) 4))
         (raise-argument-error 'set-sphere-shape-color "length-4 flvector" 1 a c)]
        [else
         (match-define (sphere-shape _ fs t old-c e m inside?) a)
         (cond [(equal? old-c c)  a]
               [else
                (define new-fs (flags-join (flags-subtract fs opacity-flags)
                                           (color-opacity-flag c)))
                (sphere-shape (lazy-passes) new-fs t c e m inside?)])]))

(: set-sphere-shape-emitted (-> sphere-shape FlVector sphere-shape))
(define (set-sphere-shape-emitted a e)
  (cond [(not (= (flvector-length e) 4))
         (raise-argument-error 'set-sphere-shape-emitted "length-4 flvector" 1 a e)]
        [else
         (match-define (sphere-shape _ fs t c old-e m inside?) a)
         (cond [(equal? old-e e)  a]
               [else
                (define new-fs (flags-join (flags-subtract fs emitting-flags)
                                           (color-emitting-flag e)))
                (sphere-shape (lazy-passes) new-fs t c e m inside?)])]))

(: set-sphere-shape-material (-> sphere-shape material sphere-shape))
(define (set-sphere-shape-material a m)
  (match-define (sphere-shape _ fs t c e old-m inside?) a)
  (cond [(equal? old-m m)  a]
        [else  (sphere-shape (lazy-passes) fs t c e m inside?)]))

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

(: transformed-sphere-rect (-> Affine Nonempty-FlRect3))
(define (transformed-sphere-rect t)
  (define-values (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
    (flvector-values (fltransform3-forward (->flaffine3 (affine-transform t))) 12))
  (define dx (flsqrt (+ (* m00 m00) (* m01 m01) (* m02 m02))))
  (define dy (flsqrt (+ (* m10 m10) (* m11 m11) (* m12 m12))))
  (define dz (flsqrt (+ (* m20 m20) (* m21 m21) (* m22 m22))))
  (nonempty-flrect3 (flvector (- m03 dx) (- m13 dy) (- m23 dz))
                    (flvector (+ m03 dx) (+ m13 dy) (+ m23 dz))))

(: sphere-shape-rect (-> sphere-shape Nonempty-FlRect3))
(define (sphere-shape-rect a)
  (transformed-sphere-rect (sphere-shape-affine a)))

;; ===================================================================================================
;; Transform

(: sphere-shape-easy-transform (-> sphere-shape Affine sphere-shape))
(define (sphere-shape-easy-transform a t)
  (match-define (sphere-shape passes fs t0 c e m inside?) a)
  (sphere-shape (lazy-passes) fs (affine-compose t t0) c e m inside?))

;; ===================================================================================================
;; Ray intersection

(: unit-sphere-line-intersects (-> FlVector FlVector (Values (U #f Flonum) (U #f Flonum))))
(define (unit-sphere-line-intersects p d)
  (define m^2 (flv3mag^2 d))
  (define b (/ (- (flv3dot p d)) m^2))
  (define c (/ (- (flv3mag^2 p) 1.0) m^2))
  (let ([discr  (- (* b b) c)])
    (if (< discr 0.0)
        (values #f #f)  ; Missed sphere
        (let* ([q  (flsqrt discr)])
          (values (- b q) (+ b q))))))

(: sphere-shape-line-intersect (-> sphere-shape FlVector FlVector (U #f Flonum)))
(define (sphere-shape-line-intersect a v dv)
  (define t (flt3inverse (affine-transform (sphere-shape-affine a))))
  (define-values (tmin tmax)
    (let ([v  (flt3apply/pos t v)]
          [dv  (flt3apply/dir t dv)])
      (unit-sphere-line-intersects v dv)))
  (if (sphere-shape-inside? a) tmax tmin))
