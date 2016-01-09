#lang typed/racket/base

(require racket/match
         racket/list
         math/flonum
         "../math.rkt"
         "../utils.rkt"
         "pos-dir.rkt")

(provide
 ;; Linear transforms
 (rename-out [-Linear Linear])
 fllinear3->linear
 linear?
 identity-linear
 linear
 linear->cols*
 linear-x-axis
 linear-y-axis
 linear-z-axis
 linear-compose
 linear-inverse
 linear-singular?
 linear-consistent?
 ;; Affine transforms
 Affine
 flaffine3->affine
 flafflin3->affine
 affine?
 identity-affine
 affine
 affine->cols*
 affine-x-axis
 affine-y-axis
 affine-z-axis
 affine-origin
 affine-compose
 affine-inverse
 affine-singular?
 affine-consistent?
 transform-pos
 transform-dir
 transform-norm
 ;; Smooth functions
 Smooth
 fldiff3->smooth
 flsmooth3->smooth
 smooth?
 identity-smooth
 smooth
 smooth-function
 smooth-jacobian
 smooth-compose
 smooth-singular?
 smooth-consistent?
 smooth-approximate
 smooth-between
 deform-pos
 deform-dir
 deform-norm
 deform-affine
 )

;; ===================================================================================================
;; Linear transforms

(define print-linear
  (make-constructor-style-printer
   (λ ([t : Linear]) 'linear)
   (λ ([t : Linear])
     (list (linear-x-axis t)
           (linear-y-axis t)
           (linear-z-axis t)))))

(: linear-equal? (-> Linear Linear (-> Any Any Boolean) Boolean))
(define (linear-equal? t1 t2 _)
  (equal? (FlLinear3-forward t1)
          (FlLinear3-forward t2)))

(: linear-hash (-> Linear (-> Any Integer) Integer))
(define (linear-hash t hash)
  (hash (FlLinear3-forward t)))

(struct Linear FlLinear3 ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-linear
  #:property prop:equal+hash (list linear-equal? linear-hash linear-hash))

(define-type -Linear Linear)
(define linear? Linear?)

(: fllinear3->linear (-> FlLinear3 Linear))
(define (fllinear3->linear t)
  (if (linear? t)
      t
      (Linear (FlLinear3-forward t)
              (FlLinear3-inverse t)
              (FlLinear3-forward-det t)
              (FlLinear3-inverse-den t))))

(define identity-linear (fllinear3->linear identity-fllinear3))

(: linear-compose2 (-> Linear Linear Linear))
(define (linear-compose2 t1 t2)
  (fllinear3->linear (flt3compose t1 t2)))

(: linear-compose (-> Linear * Linear))
(define (linear-compose . ts)
  (if (empty? ts)
      identity-linear
      (let ([t1  (first ts)]
            [ts  (rest ts)])
        (if (empty? ts)
            t1
            (let loop ([t1 t1] [t2  (first ts)] [ts  (rest ts)])
              (if (empty? ts)
                  (linear-compose2 t1 t2)
                  (loop (linear-compose2 t1 t2) (first ts) (rest ts))))))))

(: linear-inverse (-> Linear Linear))
(define (linear-inverse t)
  (define tinv (flt3inverse t))
  (cond [tinv  (fllinear3->linear tinv)]
        [else  (raise-argument-error 'linear-inverse "invertible Linear" t)]))

(: linear-singular? (-> Linear Boolean))
(define (linear-singular? t)
  (flt3singular? t))

(: linear-consistent? (-> Linear Boolean))
(define (linear-consistent? t)
  (flt3consistent? t))

(: linear (-> Dir Dir Dir Linear))
(define (linear x y z)
  (fllinear3->linear (cols->fllinear3 x y z)))

(: linear->cols* (-> Linear (Values Dir Dir Dir)))
(define (linear->cols* t)
  (call/fllinear3-forward t
    (λ (m00 m01 m02 m10 m11 m12 m20 m21 m22)
      (values (dir m00 m10 m20)
              (dir m01 m11 m21)
              (dir m02 m12 m22)))))

(: linear-x-axis (-> Linear Dir))
(define (linear-x-axis t)
  (call/fllinear3-forward t
    (λ (m00 m01 m02 m10 m11 m12 m20 m21 m22)
      (dir m00 m10 m20))))

(: linear-y-axis (-> Linear Dir))
(define (linear-y-axis t)
  (call/fllinear3-forward t
    (λ (m00 m01 m02 m10 m11 m12 m20 m21 m22)
      (dir m01 m11 m21))))

(: linear-z-axis (-> Linear Dir))
(define (linear-z-axis t)
  (call/fllinear3-forward t
    (λ (m00 m01 m02 m10 m11 m12 m20 m21 m22)
      (dir m02 m12 m22))))

;; ===================================================================================================
;; Affine transforms

(define print-affine
  (make-constructor-style-printer
   (λ ([t : Only-Affine]) 'affine)
   (λ ([t : Only-Affine])
     (list (affine-x-axis t)
           (affine-y-axis t)
           (affine-z-axis t)
           (affine-origin t)))))

(: affine-equal? (-> Only-Affine Only-Affine (-> Any Any Boolean) Boolean))
(define (affine-equal? t1 t2 _)
  (equal? (FlAffine3-forward t1)
          (FlAffine3-forward t2)))

(: affine-hash (-> Only-Affine (-> Any Integer) Integer))
(define (affine-hash t hash)
  (hash (FlAffine3-forward t)))

(struct Only-Affine FlAffine3 ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-affine
  #:property prop:equal+hash (list affine-equal? affine-hash affine-hash))

(define-type Affine (U Linear Only-Affine))
(define affine? (λ (v) (or (Linear? v) (Only-Affine? v))))

(: flaffine3->affine (-> FlAffine3 Affine))
(define (flaffine3->affine t)
  (if (Only-Affine? t)
      t
      (Only-Affine (FlAffine3-forward t)
                   (FlAffine3-inverse t)
                   (FlAffine3-forward-det t)
                   (FlAffine3-inverse-den t)
                   (FlAffine3-forward-data-vec t)
                   (FlAffine3-inverse-data-vec t))))

(: flafflin3->affine (-> FlAffLin3 Affine))
(define (flafflin3->affine t)
  (if (fllinear3? t)
      (fllinear3->linear t)
      (flaffine3->affine t)))

(: identity-affine Affine)
(define identity-affine identity-linear)

(: affine-compose2 (-> Affine Affine Affine))
(define (affine-compose2 t1 t2)
  (flafflin3->affine (flt3compose t1 t2)))

(: affine-compose (-> Affine * Affine))
(define (affine-compose . ts)
  (if (empty? ts)
      identity-affine
      (let ([t1  (first ts)]
            [ts  (rest ts)])
        (if (empty? ts)
            t1
            (let loop ([t1 t1] [t2  (first ts)] [ts  (rest ts)])
              (if (empty? ts)
                  (affine-compose2 t1 t2)
                  (loop (affine-compose2 t1 t2) (first ts) (rest ts))))))))

(: affine-inverse (-> Affine Affine))
(define (affine-inverse t)
  (define tinv (flt3inverse t))
  (cond [tinv  (flafflin3->affine tinv)]
        [else  (raise-argument-error 'affine-inverse "invertible Affine" t)]))

(: affine-singular? (-> Affine Boolean))
(define (affine-singular? t)
  (flt3singular? t))

(: affine-consistent? (-> Affine Boolean))
(define (affine-consistent? t)
  (flt3consistent? t))

(: affine (-> Dir Dir Dir Pos Affine))
(define (affine x y z p)
  (flaffine3->affine (cols->flaffine3 x y z p)))

(: affine->cols* (-> Affine (Values Dir Dir Dir Pos)))
(define (affine->cols* t)
  (cond [(linear? t)
         (define-values (dx dy dz) (linear->cols* t))
         (values dx dy dz origin)]
        [else
         (call/flaffine3-forward t
           (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
             (values (dir m00 m10 m20)
                     (dir m01 m11 m21)
                     (dir m02 m12 m22)
                     (pos m03 m13 m23))))]))

(: affine-x-axis (-> Affine Dir))
(define (affine-x-axis t)
  (if (linear? t)
      (linear-x-axis t)
      (call/flaffine3-forward t
        (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
          (dir m00 m10 m20)))))

(: affine-y-axis (-> Affine Dir))
(define (affine-y-axis t)
  (if (linear? t)
      (linear-y-axis t)
      (call/flaffine3-forward t
        (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
          (dir m01 m11 m21)))))

(: affine-z-axis (-> Affine Dir))
(define (affine-z-axis t)
  (if (linear? t)
      (linear-z-axis t)
      (call/flaffine3-forward t
        (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
          (dir m02 m12 m22)))))

(: affine-origin (-> Affine Pos))
(define (affine-origin t)
  (if (linear? t)
      origin
      (call/flaffine3-forward t
        (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
          (pos m03 m13 m23)))))

(: transform-pos (-> Pos Affine Pos))
(define (transform-pos v t)
  (flv3->pos (flt3apply/pos t v)))

(: transform-dir (-> Dir Affine Dir))
(define (transform-dir v t)
  (flv3->dir (flt3apply/dir t v)))

(: transform-norm (-> Dir Affine (U #f Dir)))
(define (transform-norm v t)
  (let ([n  (flt3apply/norm t v)])
    (and n (flv3->dir n))))

;; ===================================================================================================
;; Smooth functions

(: print-smooth (-> Only-Smooth Output-Port (U #t #f 0 1) Void))
(define (print-smooth t out mode)
  (write-string "#<smooth>" out)
  (void))

(struct Only-Smooth FlDiff3 ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-smooth)

(: fldiff3->smooth (-> FlDiff3 Only-Smooth))
(define (fldiff3->smooth t)
  (if (Only-Smooth? t)
      t
      (match-let ([(FlDiff3 f j j-given?)  t])
        (Only-Smooth f j j-given?))))

(define-type Smooth (U Affine Only-Smooth))
(define smooth? (λ (v) (or (affine? v) (Only-Smooth? v))))

(: pos-pos->flv3-flv3 (-> (-> Pos Pos) (-> FlV3 FlV3)))
(define ((pos-pos->flv3-flv3 f) v)
  (f (flv3->pos v)))

(: flv3-flv3->pos-pos (-> (-> FlV3 FlV3) (-> Pos Pos)))
(define ((flv3-flv3->pos-pos f) v)
  (flv3->pos (f v)))

(: pos-cols->flv3-fllinear3 (-> (-> Pos Linear) (-> FlV3 FlLinear3)))
(define ((pos-cols->flv3-fllinear3 f) v)
  (f (flv3->pos v)))

(: flv3-fllinear3->pos-cols (-> (-> FlV3 FlLinear3) (-> Pos Linear)))
(define ((flv3-fllinear3->pos-cols f) v)
  (fllinear3->linear (f v)))

(: smooth (->* [(-> Pos Pos)] [(U #f (-> Pos Linear))] Smooth))
(define (smooth f [j #f])
  (if j
      (Only-Smooth (pos-pos->flv3-flv3 f)
                   (pos-cols->flv3-fllinear3 j)
                   #t)
      (let ([f  (pos-pos->flv3-flv3 f)])
        (Only-Smooth f (make-jacobian f) #f))))

(: identity-smooth Smooth)
(define identity-smooth identity-affine)

(: smooth-function (-> Smooth (-> Pos Pos)))
(define (smooth-function t)
  (if (affine? t)
      (λ (v) (transform-pos v t))
      (flv3-flv3->pos-pos (FlDiff3-function t))))

(: smooth-jacobian (-> Smooth (-> Pos Linear)))
(define (smooth-jacobian t)
  (cond [(linear? t)  (λ (_) t)]
        [(affine? t)  (let ([t  (fllinear3->linear (flaffine3-linear-part t))])
                        (λ (_) t))]
        [else         (flv3-fllinear3->pos-cols (FlDiff3-jacobian t))]))

(: flsmooth3->smooth (-> FlSmooth3 Smooth))
(define (flsmooth3->smooth t)
  (cond [(FlLinear3? t)  (fllinear3->linear t)]
        [(FlAffine3? t)  (flaffine3->affine t)]
        [else            (fldiff3->smooth t)]))

(: smooth-compose2 (-> Smooth Smooth Smooth))
(define (smooth-compose2 t1 t2)
  (flsmooth3->smooth (fls3compose t1 t2)))

(: smooth-compose (-> Smooth * Smooth))
(define (smooth-compose . ts)
  (if (empty? ts)
      identity-affine
      (let ([t1  (first ts)]
            [ts  (rest ts)])
        (if (empty? ts)
            t1
            (let loop ([t1 t1] [t2  (first ts)] [ts  (rest ts)])
              (if (empty? ts)
                  (smooth-compose2 t1 t2)
                  (loop (smooth-compose2 t1 t2) (first ts) (rest ts))))))))

(: smooth-singular? (-> Smooth Pos Boolean))
(define (smooth-singular? t v)
  (fls3singular? t v))

(: smooth-consistent? (-> Smooth Pos Boolean))
(define (smooth-consistent? t v)
  (fls3consistent? t v))

(: smooth-approximate (-> Smooth Pos Affine))
(define (smooth-approximate t v)
  (flafflin3->affine (fls3approximate t v)))

(: smooth-between (-> Smooth Smooth (U Real (-> Pos Real)) Smooth))
(define (smooth-between t0 t1 fα)
  (let ([t0  (if (flafflin3? t0) (flafflin3->fldiff3 t0) t0)]
        [t1  (if (flafflin3? t1) (flafflin3->fldiff3 t1) t1)]
        [fα  (cond [(real? fα)  (λ ([_ : Pos]) fα)]
                   [else  fα])])
    
    (match-define (FlDiff3 f0 j0 given0?) t0)
    (match-define (FlDiff3 f1 j1 given1?) t1)
    
    (: f (-> FlV3 FlV3))
    (define (f v)
      (define v0 (f0 v))
      (define v1 (f1 v))
      (define α (fl (fα (flv3->pos v))))
      (flv3blend v0 v1 α))
    
    (: j (-> FlV3 FlLinear3))
    (define (j v)
      (call/fllinear3-forward (j0 v)
        (λ (m00 m01 m02 m10 m11 m12 m20 m21 m22)
          (call/fllinear3-forward (j1 v)
            (λ (n00 n01 n02 n10 n11 n12 n20 n21 n22)
              (define α (fl (fα (flv3->pos v))))
              (entries->fllinear3
               (flblend m00 n00 α) (flblend m01 n01 α) (flblend m02 n02 α)
               (flblend m10 n10 α) (flblend m11 n11 α) (flblend m12 n12 α)
               (flblend m20 n20 α) (flblend m21 n21 α) (flblend m22 n22 α)))))))
    
    (Only-Smooth f j (or given0? given1?))))

(: deform-pos (-> Pos Smooth Pos))
(define (deform-pos v t)
  (flv3->pos (fls3apply/pos t v)))

(: deform-dir (-> Pos Dir Smooth Dir))
(define (deform-dir v dv t)
  (flv3->dir (fls3apply/dir t v dv)))

(: deform-norm (-> Pos Dir Smooth (U #f Dir)))
(define (deform-norm v dv t)
  (define n (fls3apply/norm t v dv))
  (and n (flv3->dir n)))

(: deform-affine (-> Affine Smooth Affine))
(define (deform-affine t1 t2)
  (flafflin3->affine (fls3apply/affine t2 t1)))

