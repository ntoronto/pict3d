#lang typed/racket/base

(require racket/match
         racket/unsafe/ops
         math/flonum
         "flv3.rkt"
         "flt3.rkt"
         "flt3-unboxed-ops.rkt")

(provide (all-defined-out))

(struct FlDiff3 ([function : (-> FlV3 FlV3)]
                 [jacobian : (-> FlV3 FlLinear3)]
                 [jacobian-given? : Boolean])
  #:transparent)

(define fldiff3 FlDiff3)
(define fldiff3? FlDiff3?)

(define-type FlSmooth3 (U FlLinear3 FlAffine3 FlDiff3))

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

(: make-jacobian (-> (-> FlV3 FlV3) (-> FlV3 FlLinear3)))
;; Numerical differentiation to get the Jacobian
(define ((make-jacobian f) v)
  (call/flv3-values v
    (λ (x y z)
      (define hx (* (flexpt epsilon.0 #i1/3) (max 1.0 (abs x))))
      (define hy (* (flexpt epsilon.0 #i1/3) (max 1.0 (abs y))))
      (define hz (* (flexpt epsilon.0 #i1/3) (max 1.0 (abs z))))
      
      (: central (-> Flonum Flonum Flonum Flonum Flonum))
      ;; Compute a central difference robustly
      (define (central x0 x1 x2 s)
        (define 0? (flrational? x0))
        (define 2? (flrational? x2))
        (cond
          [(not (flrational? x1))  +nan.0]
          [(and 0? 2?)  (* s (- x2 x0))]
          [0?  (* 2.0 s (- x1 x0))]
          [2?  (* 2.0 s (- x2 x1))]
          [else  +nan.0]))
      
      (call/flv3-values (f v)
        (λ (x1 y1 z1)
          (define dx
            (call/flv3-values (f (flv3 (+ x hx) y z))
              (λ (x2 y2 z2)
                (call/flv3-values (f (flv3 (- x hx) y z))
                  (λ (x0 y0 z0)
                    (define s (/ 0.5 hx))
                    (flv3 (central x0 x1 x2 s) (central y0 y1 y2 s) (central z0 z1 z2 s)))))))
          
          (define dy
            (call/flv3-values (f (flv3 x (+ y hy) z))
              (λ (x2 y2 z2)
                (call/flv3-values (f (flv3 x (- y hy) z))
                  (λ (x0 y0 z0)
                    (define s (/ 0.5 hy))
                    (flv3 (central x0 x1 x2 s) (central y0 y1 y2 s) (central z0 z1 z2 s)))))))
          
          (define dz
            (call/flv3-values (f (flv3 x y (+ z hz)))
              (λ (x2 y2 z2)
                (call/flv3-values (f (flv3 x y (- z hz)))
                  (λ (x0 y0 z0)
                    (define s (/ 0.5 hz))
                    (flv3 (central x0 x1 x2 s) (central y0 y1 y2 s) (central z0 z1 z2 s)))))))
          
          (cols->fllinear3 dx dy dz))))))

(: fllinear3->fldiff3 (-> FlLinear3 FlDiff3))
(define (fllinear3->fldiff3 t)
  (fldiff3 (λ (v) (fllinear3-apply/pos t v))
           (λ (_) t)
           #t))

(: flaffine3->fldiff3 (-> FlAffine3 FlDiff3))
(define (flaffine3->fldiff3 t)
  (define linear-t (flaffine3-linear-part t))
  (fldiff3 (λ (v) (flaffine3-apply/pos t v))
           (λ (_) linear-t)
           #t))

(: flafflin3->fldiff3 (-> FlAffLin3 FlDiff3))
(define (flafflin3->fldiff3 t)
  (if (fllinear3? t)
      (fllinear3->fldiff3 t)
      (flaffine3->fldiff3 t)))

(: fldiff3-apply/pos (-> FlDiff3 FlV3 FlV3))
(define (fldiff3-apply/pos t v)
  ((FlDiff3-function t) v))

(: fldiff3-apply/dir (-> FlDiff3 FlV3 FlV3 FlV3))
(define (fldiff3-apply/dir t v dv)
  (fllinear3-apply/dir ((FlDiff3-jacobian t) v) dv))

(: fldiff3-apply/all (-> FlDiff3 FlV3 FlV3 (Values FlV3 (U #f FlV3) Flonum Boolean)))
(define (fldiff3-apply/all t v n)
  (define v0 ((FlDiff3-function t) v))
  (define j ((FlDiff3-jacobian t) v))
  (values v0
          (fllinear3-apply/norm j n)
          (fllinear3-determinant j)
          (fllinear3-consistent? j)))

(: fldiff3-apply/norm (-> FlDiff3 FlV3 FlV3 (U #f FlV3)))
(define (fldiff3-apply/norm t v n)
  (fllinear3-apply/norm ((FlDiff3-jacobian t) v) n))

(: fldiff3-compose (-> FlDiff3 FlDiff3 FlDiff3))
(define (fldiff3-compose t1 t0)
  (match-define (FlDiff3 f j j-given?) t1)
  (match-define (FlDiff3 g k k-given?) t0)
  (cond
    [(and (not j-given?) (not k-given?))
     ;; Both have numerical Jacobians, so compose the functions and differentiate numerically
     (define h (λ ([v : FlV3]) (f (g v))))
     (fldiff3 h (make-jacobian h) #f)]
    [else
     ;; One Jacobian is given, so preserve its precision (or weirdness)
     (fldiff3 (λ (v) (f (g v)))
              (λ (v) (fllinear3-compose (j (g v)) (k v)))
              #t)]))

(: fldiff3-determinant (-> FlDiff3 FlV3 Flonum))
(define (fldiff3-determinant t v)
  (fllinear3-determinant ((FlDiff3-jacobian t) v)))

(: fls3apply/pos (-> FlSmooth3 FlV3 FlV3))
(define (fls3apply/pos t v)
  (cond [(fllinear3? t)  (fllinear3-apply/pos t v)]
        [(flaffine3? t)  (flaffine3-apply/pos t v)]
        [else            (fldiff3-apply/pos t v)]))

(: fls3apply/dir (-> FlSmooth3 FlV3 FlV3 FlV3))
(define (fls3apply/dir t v dv)
  (cond [(fllinear3? t)  (fllinear3-apply/dir t dv)]
        [(flaffine3? t)  (flaffine3-apply/dir t dv)]
        [else            (fldiff3-apply/dir t v dv)]))

(: fls3apply/norm (-> FlSmooth3 FlV3 FlV3 (U #f FlV3)))
(define (fls3apply/norm t v dv)
  (cond [(fllinear3? t)  (fllinear3-apply/norm t dv)]
        [(flaffine3? t)  (flaffine3-apply/norm t dv)]
        [else            (fldiff3-apply/norm t v dv)]))

(: fls3apply/all (-> FlSmooth3 FlV3 FlV3 (Values FlV3 (U #f FlV3) Flonum Boolean)))
(define (fls3apply/all t v dv)
  (cond [(fllinear3? t)
         (let ([v  (fllinear3-apply/pos t v)]
               [n  (fllinear3-apply/norm t dv)])
           (values v n (fllinear3-determinant t) (fllinear3-consistent? t)))]
        [(flaffine3? t)
         (let ([v  (flaffine3-apply/pos t v)]
               [n  (flaffine3-apply/norm t dv)])
           (values v n (flaffine3-determinant t) (flaffine3-consistent? t)))]
        [else
         (fldiff3-apply/all t v dv)]))

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

(: make-fls3apply/all (-> FlSmooth3 (-> FlV3 FlV3 (Values FlV3 (U #f FlV3) Flonum Boolean))))
(define (make-fls3apply/all t)
  (define-type All (Vector FlV3 (U #f FlV3) Flonum Boolean))
  (define memo ((inst make-hasheq FlV3 (HashTable FlV3 All))))
  (λ (v n)
    (define h (hash-ref! memo v (λ () ((inst make-hasheq FlV3 All)))))
    (define vals (hash-ref! h n (λ () (let-values ([(v n d c?)  (fls3apply/all t v n)])
                                        (vector v n d c?)))))
    (let ([v  (unsafe-vector-ref vals 0)]
          [n  (unsafe-vector-ref vals 1)]
          [d  (unsafe-vector-ref vals 2)]
          [c? (unsafe-vector-ref vals 3)])
      (values v n d c?))))

(: fls3compose (case-> (-> FlLinear3 FlLinear3 FlLinear3)
                       (-> FlLinear3 FlAffine3 FlAffine3)
                       (-> FlLinear3 FlDiff3   FlDiff3)
                       (-> FlAffine3 FlLinear3 FlAffine3)
                       (-> FlAffine3 FlAffine3 FlAffine3)
                       (-> FlAffine3 FlDiff3   FlDiff3)
                       (-> FlDiff3   FlLinear3 FlDiff3)
                       (-> FlDiff3   FlAffine3 FlDiff3)
                       (-> FlDiff3   FlDiff3   FlDiff3)
                       (-> FlSmooth3 FlSmooth3 FlSmooth3)))
(define (fls3compose t1 t0)
  (cond [(fllinear3? t1)
         (cond [(fllinear3? t0)  (fllinear3-compose t1 t0)]
               [(flaffine3? t0)  (flaffine3-compose (->flaffine3 t1) t0)]
               [else             (fldiff3-compose (fllinear3->fldiff3 t1) t0)])]
        [(flaffine3? t1)
         (cond [(fllinear3? t0)  (flaffine3-compose t1 (->flaffine3 t0))]
               [(flaffine3? t0)  (flaffine3-compose t1 t0)]
               [else             (fldiff3-compose (flaffine3->fldiff3 t1) t0)])]
        [else
         (cond [(fllinear3? t0)  (fldiff3-compose t1 (fllinear3->fldiff3 t0))]
               [(flaffine3? t0)  (fldiff3-compose t1 (flaffine3->fldiff3 t0))]
               [else             (fldiff3-compose t1 t0)])]))

(: fls3determinant (-> FlSmooth3 FlV3 Flonum))
(define (fls3determinant t v)
  (cond [(fllinear3? t)  (fllinear3-determinant t)]
        [(flaffine3? t)  (flaffine3-determinant t)]
        [else            (fldiff3-determinant t v)]))

(: fls3singular? (-> FlSmooth3 FlV3 Boolean))
(define (fls3singular? t v)
  (define det (fls3determinant t v))
  (or (= det 0.0)
      (= (abs (/ det)) +inf.0)))

(: fls3consistent? (-> FlSmooth3 FlV3 Boolean))
(define (fls3consistent? t v)
  (cond [(fllinear3? t)  (fllinear3-consistent? t)]
        [(flaffine3? t)  (flaffine3-consistent? t)]
        [else  (flt3consistent? ((FlDiff3-jacobian t) v))]))

(: fls3approximate (-> FlSmooth3 FlV3 (U FlLinear3 FlAffine3)))
(define (fls3approximate t v)
  (if (or (fllinear3? t) (flaffine3? t))
      t
      (fllinear3-augment ((FlDiff3-jacobian t) v)
                         (flv3- ((FlDiff3-function t) v) v))))

(: fls3apply/affine (case-> (-> FlLinear3 FlLinear3 FlLinear3)
                            (-> FlLinear3 FlAffine3 FlAffine3)
                            (-> FlAffine3 FlLinear3 FlAffine3)
                            (-> FlAffine3 FlAffine3 FlAffine3)
                            (-> FlDiff3   FlLinear3 FlAffine3)
                            (-> FlDiff3   FlAffine3 FlAffine3)
                            (-> FlSmooth3 FlLinear3 (U FlLinear3 FlAffine3))
                            (-> FlSmooth3 FlAffine3 FlAffine3)
                            (-> FlSmooth3 (U FlLinear3 FlAffine3) (U FlLinear3 FlAffine3))))
(define (fls3apply/affine t0 t)
  (cond
    [(fllinear3? t0)  (if (fllinear3? t) (flt3compose t0 t) (flt3compose t0 t))]
    [(flaffine3? t0)  (if (fllinear3? t) (flt3compose t0 t) (flt3compose t0 t))]
    [(fllinear3? t)
     (define-values (dx dy dz) (fllinear3->cols t))
     (define j ((FlDiff3-jacobian t0) zero-flv3))
     (cols->flaffine3 (fllinear3-apply/dir j dx)
                      (fllinear3-apply/dir j dy)
                      (fllinear3-apply/dir j dz)
                      ((FlDiff3-function t0) zero-flv3))]
    [else
     (define-values (dx dy dz v) (flaffine3->cols t))
     (define j ((FlDiff3-jacobian t0) v))
     (cols->flaffine3 (fllinear3-apply/dir j dx)
                      (fllinear3-apply/dir j dy)
                      (fllinear3-apply/dir j dz)
                      ((FlDiff3-function t0) v))]))
