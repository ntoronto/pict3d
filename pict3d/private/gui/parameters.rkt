#lang typed/racket/base

(require math/flonum
         "user-types.rkt")

(provide (all-defined-out))

(define default-pict3d-width 256)
(define default-pict3d-height 256)
(define default-pict3d-z-near (assert (flexpt 2.0 -20.0) positive?))
(define default-pict3d-z-far  (assert (flexpt 2.0 +32.0) positive?))
(define default-pict3d-fov-degrees 90.0)
(define default-pict3d-background (flvector 0.0 0.0 0.0 1.0))
(define default-pict3d-ambient-color (flvector 1.0 1.0 1.0))
(define default-pict3d-ambient-intensity 1.0)

(: current-pict3d-width (Parameterof Integer Positive-Index))
(define current-pict3d-width
  (make-parameter default-pict3d-width
                  (λ ([n : Integer]) (assert (min 4096 (max 1 n)) index?))))

(: current-pict3d-height (Parameterof Integer Positive-Index))
(define current-pict3d-height
  (make-parameter default-pict3d-height
                  (λ ([n : Integer]) (assert (min 4096 (max 1 n)) index?))))

(: current-pict3d-z-near (Parameterof Real Positive-Flonum))
(define current-pict3d-z-near
  (make-parameter default-pict3d-z-near
                  (λ ([z : Real])
                    (max default-pict3d-z-near (min default-pict3d-z-far (fl z))))))

(: current-pict3d-z-far (Parameterof Real Positive-Flonum))
(define current-pict3d-z-far
  (make-parameter default-pict3d-z-far
                  (λ ([z : Real])
                    (max default-pict3d-z-near (min default-pict3d-z-far (fl z))))))

(: current-pict3d-fov-degrees (Parameterof Positive-Real Positive-Flonum))
(define current-pict3d-fov-degrees
  (make-parameter default-pict3d-fov-degrees
                  (λ ([z : Positive-Real])
                    (max 1.0 (min 179.0 (fl z))))))

(: current-pict3d-background (Parameterof Pict3D-Color FlVector))
(define current-pict3d-background
  (make-parameter default-pict3d-background
                  (λ ([col : Pict3D-Color]) (->flcolor4 'current-pict3d-background col))))

(: current-pict3d-ambient-color (Parameterof Pict3D-Color FlVector))
(define current-pict3d-ambient-color
  (make-parameter default-pict3d-ambient-color
                  (λ ([col : Pict3D-Color]) (->flcolor3 'current-pict3d-ambient-color col))))

(: current-pict3d-ambient-intensity (Parameterof Nonnegative-Real Nonnegative-Flonum))
(define current-pict3d-ambient-intensity
  (make-parameter default-pict3d-ambient-intensity fl))
