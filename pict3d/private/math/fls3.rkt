#lang typed/racket/base

;; TODO: Quadratic fit for numerical differentiation?

(require racket/match
         racket/unsafe/ops
         math/flonum
         "flv3.rkt"
         "flt3.rkt"
         "flt3-unboxed-ops.rkt")

(provide (all-defined-out))

(define-type FlFunction3 (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum)))
(define-type FlJacobian3 (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum
                                                          Flonum Flonum Flonum
                                                          Flonum Flonum Flonum)))

(struct FlDiff3 ([function : FlFunction3]
                 [jacobian : FlJacobian3]
                 [jacobian-given? : Boolean])
  #:transparent)

(define fldiff3 FlDiff3)
(define fldiff3? FlDiff3?)

(define-type FlSmooth3 (U FlAffine3 FlDiff3))

#|
This is awesomely consistent in its precision, but very slow

(: central-diff (-> (-> Flonum Flonum) Flonum Flonum Flonum))
(define (central-diff f h x)
  (define y (f x))
  (define x1 (- x h))
  (define x2 (+ x h))
  (define y1 (f x1))
  (define y2 (f x2))
  (define 1? (and (< -inf.0 (min x1 y1)) (< (max x1 y1) +inf.0)))
  (define 2? (and (< -inf.0 (min x2 y2)) (< (max x2 y2) +inf.0)))
  (cond
    [(and 1? 2?)  (/ (- y2 y1) (* 2.0 h))]
    [2?  (/ (- y2 y) h)]
    [1?  (/ (- y y1) h)]
    [else  +nan.0]))

(: diff (-> (-> Flonum Flonum) Flonum Flonum))
(define (diff f x)
  (let loop ([h  (* (flexpt epsilon.0 #i1/3) (max 1.0 (abs x)))]
             [n : Index  0])
    ;(printf "h = ~v~n" h)
    (define dy/dx (central-diff f h x))
    (cond
      [(or (>= n 42) (not (flrational? dy/dx)))
       dy/dx]
      [else
       (define s (/ (* (f x) (flexpt epsilon.0 #i1/2)) dy/dx))
       (define new-h (* 0.5 (abs (+ (- s x) (+ s x)))))
       (cond
         [(or (= new-h 0.0) (not (flrational? new-h)))
          dy/dx]
         [(< (abs (- new-h h)) new-h)
          ;(printf "h = ~v~n" new-h)
          ;(newline)
          (central-diff f new-h x)]
         [else
          (loop new-h (+ n 1))])])))

(: make-jacobian (-> FlFunction3 Positive-Flonum Positive-Flonum Positive-Flonum FlJacobian3))
(define ((make-jacobian f eps-x eps-y eps-z) x y z)
  (values (diff (λ (x) (let-values ([(x y z) (f x y z)]) x)) x)
          (diff (λ (y) (let-values ([(x y z) (f x y z)]) x)) y)
          (diff (λ (z) (let-values ([(x y z) (f x y z)]) x)) z)
          (diff (λ (x) (let-values ([(x y z) (f x y z)]) y)) x)
          (diff (λ (y) (let-values ([(x y z) (f x y z)]) y)) y)
          (diff (λ (z) (let-values ([(x y z) (f x y z)]) y)) z)
          (diff (λ (x) (let-values ([(x y z) (f x y z)]) z)) x)
          (diff (λ (y) (let-values ([(x y z) (f x y z)]) z)) y)
          (diff (λ (z) (let-values ([(x y z) (f x y z)]) z)) z)))
|#

(: make-jacobian (-> FlFunction3 FlJacobian3))
;; Numerical differentiation to get the Jacobian
(define ((make-jacobian f) x y z)
  (define hx (* (flexpt epsilon.0 #i1/3) (max 1.0 (abs x))))
  (define hy (* (flexpt epsilon.0 #i1/3) (max 1.0 (abs y))))
  (define hz (* (flexpt epsilon.0 #i1/3) (max 1.0 (abs z))))
  
  (: central (-> Flonum Flonum Flonum Flonum Flonum))
  (define (central x0 x1 x2 s)
    (define 0? (flrational? x0))
    (define 2? (flrational? x2))
    (cond
      [(not (flrational? x1))  +nan.0]
      [(and 0? 2?)  (* s (- x2 x0))]
      [0?  (* 2.0 s (- x1 x0))]
      [2?  (* 2.0 s (- x2 x1))]
      [else  +nan.0]))
  
  (define-values (x1 y1 z1) (f x y z))
  
  (define-values (m00 m10 m20)
    (let-values ([(x2 y2 z2)  (f (+ x hx) y z)]
                 [(x0 y0 z0)  (f (- x hx) y z)])
      (define s (/ 0.5 hx))
      (values (central x0 x1 x2 s) (central y0 y1 y2 s) (central z0 z1 z2 s))))
  
  (define-values (m01 m11 m21)
    (let-values ([(x2 y2 z2)  (f x (+ y hy) z)]
                 [(x0 y0 z0)  (f x (- y hy) z)])
      (define s (/ 0.5 hy))
      (values (central x0 x1 x2 s) (central y0 y1 y2 s) (central z0 z1 z2 s))))
  
  (define-values (m02 m12 m22)
    (let-values ([(x2 y2 z2)  (f x y (+ z hz))]
                 [(x0 y0 z0)  (f x y (- z hz))])
      (define s (/ 0.5 hz))
      (values (central x0 x1 x2 s) (central y0 y1 y2 s) (central z0 z1 z2 s))))
  
  (values m00 m01 m02
          m10 m11 m12
          m20 m21 m22))

(: flaffine3->fldiff3 (-> FlAffine3 FlDiff3))
(define (flaffine3->fldiff3 t)
  (call/flaffine3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (fldiff3 (λ (x y z)
                 (call/affine3-apply
                   m00 m01 m02 m03
                   m10 m11 m12 m13
                   m20 m21 m22 m23
                   x y z 1.0
                   ;; No need to divide because w' = 1
                   (λ (x y z _1.0) (values x y z))))
               (λ (x y z)
                 (values m00 m01 m02
                         m10 m11 m12
                         m20 m21 m22))
               #t))))

(: fldiff3-apply/pos (-> FlDiff3 FlV3 FlV3))
(define (fldiff3-apply/pos t v)
  (let-values ([(x y z)  (call/flv3-values v (FlDiff3-function t))])
    (flv3 x y z)))

(: fldiff3-apply/dir (-> FlDiff3 FlV3 FlV3 FlV3))
(define (fldiff3-apply/dir t v dv)
  (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22)
    (call/flv3-values v (FlDiff3-jacobian t)))
  ;; Apply Jacobian
  (call/flv3-values dv
    (λ (dx dy dz)
      (call/affine3-apply
        m00 m01 m02 0.0
        m10 m11 m12 0.0
        m20 m21 m22 0.0
        dx dy dz 0.0
        (λ (dx dy dz _) (flv3 dx dy dz))))))

(: fldiff3-apply/norm+det (-> FlDiff3 FlV3 FlV3 (Values (U #f FlV3) Flonum)))
(define (fldiff3-apply/norm+det t v n)
  (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22)
    (call/flv3-values v (FlDiff3-jacobian t)))
  ;; Apply inverse transpose, then normalize
  (call/affine3-inverse*det m00 m01 m02 0.0 m10 m11 m12 0.0 m20 m21 m22 0.0
    (λ (m00 m01 m02 _m03 m10 m11 m12 _m13 m20 m21 m22 _m23 det)
      (call/flv3-values n
        (λ (nx ny nz)
          (call/affine3-tapply
            (/ m00 det) (/ m01 det) (/ m02 det) 0.0
            (/ m10 det) (/ m11 det) (/ m12 det) 0.0
            (/ m20 det) (/ m21 det) (/ m22 det) 0.0
            nx ny nz 0.0
            (λ (nx ny nz _)
              (values (and (< -inf.0 (min nx ny nz))
                           (< (max nx ny nz) +inf.0)
                           (flnorm3 nx ny nz))
                      det))))))))

(: fldiff3-apply/norm (-> FlDiff3 FlV3 FlV3 (U #f FlV3)))
(define (fldiff3-apply/norm t v n)
  (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22)
    (call/flv3-values v (FlDiff3-jacobian t)))
  ;; Apply inverse transpose, then normalize
  (call/affine3-inverse*det m00 m01 m02 0.0 m10 m11 m12 0.0 m20 m21 m22 0.0
    (λ (m00 m01 m02 _m03 m10 m11 m12 _m13 m20 m21 m22 _m23 det)
      (call/flv3-values n
        (λ (nx ny nz)
          (call/affine3-tapply
            (/ m00 det) (/ m01 det) (/ m02 det) 0.0
            (/ m10 det) (/ m11 det) (/ m12 det) 0.0
            (/ m20 det) (/ m21 det) (/ m22 det) 0.0
            nx ny nz 0.0
            (λ (nx ny nz _)
              (and (< -inf.0 (min nx ny nz))
                   (< (max nx ny nz) +inf.0)
                   (flnorm3 nx ny nz)))))))))

(: fldiff3-compose (-> FlDiff3 FlDiff3 FlDiff3))
(define (fldiff3-compose t1 t0)
  (match-define (FlDiff3 f j j-given?) t1)
  (match-define (FlDiff3 g k k-given?) t0)
  (cond
    [(and (not j-given?) (not k-given?))
     ;; Both have numerical Jacobians, so compose the functions and differentiate numerically
     
     (: h (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum)))
     (define (h x y z)
       (let-values ([(x y z)  (g x y z)])
         (f x y z)))
     
     (fldiff3 h (make-jacobian h) #f)]
    [else
     ;; One Jacobian is given, so preserve its precision (or weirdness)
     (fldiff3
      (λ (x y z)
        (let-values ([(x y z)  (g x y z)])
          (f x y z)))
      (λ (x y z)
        (define-values (n00 n01 n02 n10 n11 n12 n20 n21 n22) (k x y z))
        (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22)
          (let-values ([(x y z)  (g x y z)])
            (j x y z)))
        (call/linear3-compose
          m00 m01 m02
          m10 m11 m12
          m20 m21 m22
          n00 n01 n02
          n10 n11 n12
          n20 n21 n22
          values))
      #t)]))

(: fldiff3-determinant (-> FlDiff3 FlV3 Flonum))
(define (fldiff3-determinant t v)
  (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22)
    (call/flv3-values v (FlDiff3-jacobian t)))
  (linear3-det m00 m01 m02 m10 m11 m12 m20 m21 m22))

(: fls3apply/pos (-> FlSmooth3 FlV3 FlV3))
(define (fls3apply/pos t v)
  (if (flaffine3? t)
      (flaffine3-apply/pos t v)
      (fldiff3-apply/pos t v)))

(: fls3apply/dir (-> FlSmooth3 FlV3 FlV3 FlV3))
(define (fls3apply/dir t v dv)
  (if (flaffine3? t)
      (flaffine3-apply/dir t dv)
      (fldiff3-apply/dir t v dv)))

(: fls3apply/norm (-> FlSmooth3 FlV3 FlV3 (U #f FlV3)))
(define (fls3apply/norm t v dv)
  (if (flaffine3? t)
      (flaffine3-apply/norm t dv)
      (fldiff3-apply/norm t v dv)))

(: fls3apply/pos+norm+det (-> FlSmooth3 FlV3 FlV3 (Values FlV3 (U #f FlV3) Flonum)))
(define (fls3apply/pos+norm+det t v dv)
  (if (flaffine3? t)
      (let ([v  (flaffine3-apply/pos t v)]
            [n  (flaffine3-apply/norm t dv)])
        (values v n (flaffine3-determinant t)))
      (let-values ([(v)    (fldiff3-apply/pos t v)]
                   [(n d)  (fldiff3-apply/norm+det t v dv)])
        (values v n d))))

(: make-fls3apply/pos (-> FlSmooth3 (-> FlV3 FlV3)))
(define (make-fls3apply/pos t)
  (define memo ((inst make-hasheq FlV3 FlV3)))
  (λ (v) (hash-ref! memo v (λ () (fls3apply/pos t v)))))

(: make-fls3apply/dir (-> FlSmooth3 (-> FlV3 FlV3 FlV3)))
(define (make-fls3apply/dir t)
  (define memo ((inst make-hasheq FlV3 (HashTable FlV3 FlV3))))
  (λ (v n)
    (define h (hash-ref! memo v (λ () ((inst make-hasheq FlV3 FlV3)))))
    (hash-ref! h n (λ () (fls3apply/dir t v n)))))

(: make-fls3apply/norm (-> FlSmooth3 (-> FlV3 FlV3 (U #f FlV3))))
(define (make-fls3apply/norm t)
  (define memo ((inst make-hasheq FlV3 (HashTable FlV3 (U #f FlV3)))))
  (λ (v n)
    (define h (hash-ref! memo v (λ () ((inst make-hasheq FlV3 (U #f FlV3))))))
    (hash-ref! h n (λ () (fls3apply/norm t v n)))))

(: make-fls3apply/pos+norm+det (-> FlSmooth3 (-> FlV3 FlV3 (Values FlV3 (U #f FlV3) Flonum))))
(define (make-fls3apply/pos+norm+det t)
  (define memo ((inst make-hasheq FlV3 (HashTable FlV3 (Vector FlV3 (U #f FlV3) Flonum)))))
  (λ (v n)
    (define h (hash-ref! memo v (λ () ((inst make-hasheq FlV3 (Vector FlV3 (U #f FlV3) Flonum))))))
    (define vals (hash-ref! h n (λ () (let-values ([(v n d)  (fls3apply/pos+norm+det t v n)])
                                        (vector v n d)))))
    (let ([v  (unsafe-vector-ref vals 0)]
          [n  (unsafe-vector-ref vals 1)]
          [d  (unsafe-vector-ref vals 2)])
      (values v n d))))

(: fls3compose (case-> (-> FlAffine3 FlAffine3 FlAffine3)
                       (-> FlSmooth3 FlSmooth3 FlSmooth3)))
(define (fls3compose t1 t0)
  (if (flaffine3? t1)
      (if (flaffine3? t0)
          (flaffine3-compose t1 t0)
          (fldiff3-compose (flaffine3->fldiff3 t1) t0))
      (if (flaffine3? t0)
          (fldiff3-compose t1 (flaffine3->fldiff3 t0))
          (fldiff3-compose t1 t0))))

(: fls3determinant (-> FlSmooth3 FlV3 Flonum))
(define (fls3determinant t v)
  (if (flaffine3? t)
      (flaffine3-determinant t)
      (fldiff3-determinant t v)))

(: fls3consistent? (-> FlSmooth3 FlV3 Boolean))
(define (fls3consistent? t v)
  (define det (fls3determinant t v))
  (or (> det 0.0) (> (/ det) 0.0)))

(: fls3approximate (-> FlSmooth3 FlV3 (U #f FlAffine3)))
(define (fls3approximate t v)
  (cond [(flaffine3? t)  t]
        [else
         (call/flv3-values v
           (λ (x y z)
             (define-values (m03 m13 m23) ((FlDiff3-function t) x y z))
             (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22) ((FlDiff3-jacobian t) x y z))
             (cols->flaffine3 (flv3 m00 m10 m20)
                              (flv3 m01 m11 m21)
                              (flv3 m02 m12 m22)
                              (flv3 (- m03 x) (- m13 y) (- m23 z)))))]))

#|
(: fls3apply/affine (-> FlSmooth3 FlAffine3 FlAffine3))
;; Straightforward implementation
(define (fls3apply/affine t0 t)
  (call/flaffine3-forward t
    (λ (m00 m01 m02 x m10 m11 m12 y m20 m21 m22 z)
      (cols->flaffine3
       (fls3apply/dir t0 (flv3 x y z) (flv3 m00 m10 m20))
       (fls3apply/dir t0 (flv3 x y z) (flv3 m01 m11 m21))
       (fls3apply/dir t0 (flv3 x y z) (flv3 m02 m12 m22))
       (fls3apply/pos t0 (flv3 x y z))))))
|#

(: fls3apply/affine (-> FlSmooth3 FlAffine3 (U #f FlAffine3)))
;; Faster but equivalent implementation
(define (fls3apply/affine t0 t)
  (cond
    [(flaffine3? t0)  (flaffine3-compose t0 t)]
    [else
     (call/flaffine3-forward t
       (λ (m00 m01 m02 x m10 m11 m12 y m20 m21 m22 z)
         (define-values (n03 n13 n23) ((FlDiff3-function t0) x y z))
         (define-values (n00 n01 n02 n10 n11 n12 n20 n21 n22) ((FlDiff3-jacobian t0) x y z))
         (call/affine3-compose
           n00 n01 n02 n03
           n10 n11 n12 n13
           n20 n21 n22 n23
           m00 m01 m02 0.0
           m10 m11 m12 0.0
           m20 m21 m22 0.0
           (λ (n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23)
             (cols->flaffine3 (flv3 n00 n10 n20)
                              (flv3 n01 n11 n21)
                              (flv3 n02 n12 n22)
                              (flv3 n03 n13 n23))))))]))
