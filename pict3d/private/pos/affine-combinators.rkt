#lang typed/racket/base

(require math/flonum
         math/base
         "pos-dir.rkt"
         "linear-affine-smooth.rkt"
         "../math.rkt"
         )

(provide
 scale-x
 scale-y
 scale-z
 scale
 rotate-x
 rotate-y
 rotate-z
 rotate
 move-x
 move-y
 move-z
 move
 relocate
 local-transform
 )

(: make-transformer
   (All (A) (case-> (-> (-> A FlLinear3) (-> A Linear))
                    (-> (-> A FlAffLin3) (-> A Affine)))))
(define ((make-transformer f) v)
  (define t (f v))
  (if (fllinear3? t)
      (fllinear3->linear t)
      (flaffine3->affine t)))

;; ---------------------------------------------------------------------------------------------------
;; Scale

(: check-real-scale (-> Symbol Real Flonum))
(define (check-real-scale name v)
  (let ([v  (fl v)])
    (if (= v 0.0)
        (raise-argument-error name "nonzero scale factor" v)
        v)))

(: check-dir-scale (-> Symbol Dir FlV3))
(define (check-dir-scale name v)
  (call/flv3-values v
    (λ (x y z)
      (if (or (= x 0.0) (= y 0.0) (= z 0.0))
          (raise-argument-error name "scale direction with nonzero components" v)
          v))))

(: check-scale (case-> (-> Symbol Real Flonum)
                       (-> Symbol Dir FlV3)
                       (-> Symbol (U Real Dir) (U Flonum FlV3))))
(define (check-scale name v)
  (if (real? v)
      (check-real-scale name v)
      (check-dir-scale name v)))

(define scale-x (make-transformer (λ ([v : Real]) (scale-x-flt3 (check-real-scale 'scale-x v)))))
(define scale-y (make-transformer (λ ([v : Real]) (scale-y-flt3 (check-real-scale 'scale-y v)))))
(define scale-z (make-transformer (λ ([v : Real]) (scale-z-flt3 (check-real-scale 'scale-z v)))))
(define scale (make-transformer (λ ([v : (U Real Dir)]) (scale-flt3 (check-scale 'scale v)))))

;; ---------------------------------------------------------------------------------------------------
;; Translate

(define move-x
  (make-transformer (λ ([v : Real])
                      (let ([v  (fl v)])
                        (if (= v 0.0) identity-affine (move-x-flt3 v))))))

(define move-y
  (make-transformer (λ ([v : Real])
                      (let ([v  (fl v)])
                        (if (= v 0.0) identity-affine (move-y-flt3 v))))))

(define move-z
  (make-transformer (λ ([v : Real])
                      (let ([v  (fl v)])
                        (if (= v 0.0) identity-affine (move-z-flt3 v))))))

(define move ((inst make-transformer Dir) move-flt3))

;; ---------------------------------------------------------------------------------------------------
;; Rotate

(: check-axis (-> Symbol Dir FlV3))
(define (check-axis name orig-v)
  (define v (flv3normalize orig-v))
  (if v v (raise-argument-error name "nonzero axis vector" v)))

(define rotate-x
  (make-transformer (λ ([a : Real])
                      (let ([a  (fl a)])
                        (cond [(= a 0.0)  identity-linear]
                              [else  (rotate-x-flt3 (degrees->radians a))])))))

(define rotate-y
  (make-transformer (λ ([a : Real])
                      (let ([a  (fl a)])
                        (cond [(= a 0.0)  identity-linear]
                              [else  (rotate-y-flt3 (degrees->radians a))])))))

(define rotate-z
  (make-transformer (λ ([a : Real])
                      (let ([a  (fl a)])
                        (cond [(= a 0.0)  identity-linear]
                              [else  (rotate-z-flt3 (degrees->radians a))])))))

(: rotate (-> Dir Real Linear))
(define (rotate v a)
  (let ([a  (fl a)]
        [v : FlV3  (check-axis 'rotate v)])
    (cond [(= a 0.0)  identity-linear]
          [else  (fllinear3->linear (rotate-flt3 v (degrees->radians a)))])))

;; ---------------------------------------------------------------------------------------------------
;; Change of basis

(: relocate (-> Affine Affine Affine))
(define (relocate t1 t2)
  (affine-compose t2 (affine-inverse t1)))

(: local-transform (-> Affine Affine Affine))
(define (local-transform t local-t)
  (affine-compose local-t (relocate local-t t)))


