#lang typed/racket/base

(require racket/pretty
         mzlib/pconvert-prop
         (except-in typed/opengl/ffi cast ->)
         "../math/flt3.rkt"
         "../utils.rkt"
         "../ffi.rkt")

(provide
 current-affine-custom-write
 (struct-out material)
 affine-data-size
 (rename-out [-Affine Affine])
 affine
 affine?
 affine-transform
 identity-affine
 affine-compose
 affine-inverse
 affine-data
 affine-consistent?
 )

;; ===================================================================================================
;; Materials

(struct material ([ambient : Flonum]
                  [diffuse : Flonum]
                  [specular : Flonum]
                  [roughness : Flonum])
  #:transparent)

;; ===================================================================================================
;; Affine transformations with GL data lazily cached in an f32vector

(define affine-data-size (* 12 4))

(: default-affine-custom-write (-> Affine Output-Port (U #t #f 0 1) Any))
(define (default-affine-custom-write p port mode)
  (write-string "#<Affine>" port))

(: current-affine-custom-write (Parameterof (-> Affine Output-Port (U #t #f 0 1) Any)))
(define current-affine-custom-write (make-parameter default-affine-custom-write))

(: convert-affine (-> Affine (-> Any Any) Any))
(define (convert-affine t _)
  (define port (open-output-string))
  ((current-affine-custom-write) t port 0)
  (get-output-string port)
  (define-values (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
    (flvector-values (fltransform3-forward (->flaffine3 (Affine-transform t))) 12))
  `(cols->affine
    (dir ,m00 ,m10 ,m20)
    (dir ,m01 ,m11 ,m21)
    (dir ,m02 ,m12 ,m22)
    (pos ,m03 ,m13 ,m23)))

(: affine-equal? (-> Affine Affine (-> Any Any Boolean) Boolean))
(define (affine-equal? t1 t2 _)
  (equal? (fltransform3-forward (->flaffine3 (Affine-transform t1)))
          (fltransform3-forward (->flaffine3 (Affine-transform t1)))))

(: affine-hash (-> Affine (-> Any Integer) Integer))
(define (affine-hash t hash)
  (hash (fltransform3-forward (->flaffine3 (Affine-transform t)))))

(struct Affine ([transform : FlAffine3-]
                [lazy-data : (Lazy-Box F32Vector)])
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (λ (t port mode) ((current-affine-custom-write) t port mode))
  #:property prop:print-converter convert-affine
  #:property prop:equal+hash (list affine-equal? affine-hash affine-hash))

(define-type -Affine Affine)
(define affine? Affine?)
(define affine-transform Affine-transform)

(define identity-transform
  (f32vector 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0))

(define identity-affine
  (Affine identity-flt3
          (lazy-box F32Vector identity-transform)))

(: affine (-> FlAffine3- Affine))
(define (affine t)
  (if (flidentity3? t)
      identity-affine
      (Affine t (box 'lazy))))

(: affine-compose (-> Affine Affine Affine))
(define (affine-compose t1 t2)
  (affine (flt3compose (affine-transform t1)
                       (affine-transform t2))))

(: affine-inverse (-> Affine Affine))
(define (affine-inverse t)
  (affine (flt3inverse (affine-transform t))))

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

(: affine-data (-> Affine F32Vector))
;; This is not thread-safe! But that's okay if it's only called during drawing
(define (affine-data m)
  (lazy-box-ref!
   (Affine-lazy-data m)
   (λ () (get-transform-data (affine-transform m)))))

(: affine-consistent? (-> Affine Boolean))
(define (affine-consistent? m)
  (flt3consistent? (affine-transform m)))
