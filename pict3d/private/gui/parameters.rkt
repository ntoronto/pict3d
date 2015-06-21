#lang typed/racket/base

(require math/flonum
         "../math.rkt"
         "../engine.rkt"
         "pict3d-struct.rkt"
         "user-types.rkt"
         (only-in "typed-user-types.rkt"
                  flaffine3->affine))

(provide (all-defined-out))

(: current-pict3d-legacy? (Parameterof Boolean))
(define current-pict3d-legacy? (make-parameter #f))

(: current-pict3d-check-version? (Parameterof Boolean))
(define current-pict3d-check-version? (make-parameter #t))

(define default-pict3d-width 256)
(define default-pict3d-height 256)
(define default-pict3d-z-near (assert (flexpt 2.0 -20.0) positive?))
(define default-pict3d-z-far  (assert (flexpt 2.0 +32.0) positive?))
(define default-pict3d-fov 90.0)
(define default-pict3d-background (rgba 0.0 0.0 0.0 1.0))
(define default-pict3d-ambient (emitted 1.0 1.0 1.0 1.0))

(: default-pict3d-auto-camera (-> Pict3D Affine))
(define (default-pict3d-auto-camera p)
  (define b (maybe-bbox-rect (scene-visible-bbox/badness (pict3d-scene p) tight-badness)))
  (define-values (v dv)
    (cond
      [(not b)
       (values +x+y+z-flv3 -x-y-z-flv3)]
      [else
       (define mn (flrect3-min b))
       (define mx (flrect3-max b))
       (let* ([dv : FlV3  (flv3- mn mx)]
              [norm : (U #f FlV3)  (flv3normalize dv)])
         (cond
           [norm
            (call/flv3-values dv
              (λ (dx dy dz)
                (define r (* 0.25 (min (abs dx) (abs dy) (abs dz))))
                (values (flv3fma norm (- r) mx) dv)))]
           [else
            (values +x+y+z-flv3 -x-y-z-flv3)]))]))
    (flaffine3->affine (point-at-flt3 v dv)))

(: current-pict3d-width (Parameterof Integer Positive-Index))
(define current-pict3d-width
  (make-parameter default-pict3d-width
                  (λ ([n : Integer]) (assert (max 1 (min 1024 n)) index?))))

(: current-pict3d-height (Parameterof Integer Positive-Index))
(define current-pict3d-height
  (make-parameter default-pict3d-height
                  (λ ([n : Integer]) (assert (max 1 (min 1024 n)) index?))))

(: current-pict3d-z-near (Parameterof Real Positive-Flonum))
(define current-pict3d-z-near
  (make-parameter default-pict3d-z-near
                  (λ ([z : Real])
                    (flclamp (fl z) default-pict3d-z-near default-pict3d-z-far))))

(: current-pict3d-z-far (Parameterof Real Positive-Flonum))
(define current-pict3d-z-far
  (make-parameter default-pict3d-z-far
                  (λ ([z : Real])
                    (flclamp (fl z) default-pict3d-z-near default-pict3d-z-far))))

(: current-pict3d-fov (Parameterof Positive-Real Positive-Flonum))
(define current-pict3d-fov
  (make-parameter default-pict3d-fov
                  (λ ([z : Positive-Real])
                    (flclamp (fl z) 1.0 179.0))))

(: current-pict3d-background (Parameterof RGBA))
(define current-pict3d-background (make-parameter default-pict3d-background))

(: current-pict3d-ambient (Parameterof Emitted))
(define current-pict3d-ambient (make-parameter default-pict3d-ambient))

(: current-pict3d-add-sunlight? (Parameterof Boolean))
(define current-pict3d-add-sunlight? (make-parameter #t))

(: current-pict3d-add-indicators? (Parameterof Boolean))
(define current-pict3d-add-indicators? (make-parameter #t))

(: current-pict3d-add-grid? (Parameterof Boolean))
(define current-pict3d-add-grid? (make-parameter #f))

(: current-pict3d-add-wireframe (Parameterof (U #f 'color 'emitted)))
(define current-pict3d-add-wireframe (make-parameter #f))

(: current-pict3d-auto-camera (Parameterof (-> Pict3D Affine)))
(define current-pict3d-auto-camera (make-parameter default-pict3d-auto-camera))
