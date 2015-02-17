#lang typed/racket/base

;; Affine transformations with GL data lazily cached in a C array of floats

(require (except-in typed/opengl/ffi cast ->)
         "../math/flt3.rkt"
         "../utils.rkt"
         "../ffi.rkt")

(provide (except-out
          (all-defined-out)
          identity-transform
          get-transform-data))

(struct affine ([transform : FlAffine3-]
                [lazy-data : (Lazy-Box F32Vector)])
  #:transparent)

(define affine-size (* 12 4))

(define identity-transform (f32vector 1.0 0.0 0.0 0.0
                                      0.0 1.0 0.0 0.0
                                      0.0 0.0 1.0 0.0))

(define identity-affine
  (affine identity-flt3
          (lazy-box F32Vector identity-transform)))

(: make-affine (-> FlAffine3- affine))
(define (make-affine t)
  (if (flidentity3? t)
      identity-affine
      (affine t (box 'lazy))))

(: ->affine (-> (U affine FlAffine3-) affine))
(define (->affine t)
  (if (affine? t) t (make-affine t)))

(: affine-compose (-> (U affine FlAffine3-) (U affine FlAffine3-) affine))
(define (affine-compose t1 t2)
  (let ([t1  (if (affine? t1) (affine-transform t1) t1)]
        [t2  (if (affine? t2) (affine-transform t2) t2)])
    (make-affine (flt3compose t1 t2))))

(: get-transform-data (-> FlAffine3- F32Vector))
(define (get-transform-data t)
  (define-values (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
    (cond [(flidentity3? t)
           (values 1.0 0.0 0.0 0.0
                   0.0 1.0 0.0 0.0
                   0.0 0.0 1.0 0.0)]
          [(fllinear3? t)
           (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22)
             (flvector-values (fltransform3-forward t) 9))
           (values m00 m01 m02 0.0
                   m10 m11 m12 0.0
                   m20 m21 m22 0.0)]
          [(flaffine3? t)
           (flvector-values (fltransform3-forward t) 12)]))
  (define vec (make-f32vector 12))
  (define p (f32vector->cpointer vec))
  (ptr-set! p _float 0 m00)
  (ptr-set! p _float 1 m01)
  (ptr-set! p _float 2 m02)
  (ptr-set! p _float 3 m03)
  (ptr-set! p _float 4 m10)
  (ptr-set! p _float 5 m11)
  (ptr-set! p _float 6 m12)
  (ptr-set! p _float 7 m13)
  (ptr-set! p _float 8 m20)
  (ptr-set! p _float 9 m21)
  (ptr-set! p _float 10 m22)
  (ptr-set! p _float 11 m23)
  vec)

(: affine-data (-> affine F32Vector))
;; This is not thread-safe! But that's okay if it's only called during drawing
(define (affine-data m)
  (lazy-box-ref!
   (affine-lazy-data m)
   (λ () (get-transform-data (affine-transform m)))))

(: affine-consistent? (-> affine Boolean))
(define (affine-consistent? m)
  (flt3consistent? (affine-transform m)))
