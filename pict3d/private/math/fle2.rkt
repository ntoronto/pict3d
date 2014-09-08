#lang typed/racket

#|
Represent numbers (x.hi+x.lo)*2^(e*1024), where x.hi ∈ [2^-512,2^512)
|#

(require math/flonum
         math/bigfloat
         racket/fixnum
         (only-in racket/unsafe/ops
                  unsafe-vector-ref unsafe-fx+ unsafe-fx-
                  unsafe-fxrshift unsafe-fxlshift)
         "fast-fl2.rkt")

(provide fle2?
         fle2-normalize
         flonum->fle2
         rational->fle2
         fle2
         fle2->flonum
         fle2->real
         fle2->bigfloat
         fle2->fl2
         fl+/e2 fl-/e2 fl*/e2 fl//e2 flsqr/e2 flsqrt/e2
         fle2+ fle2- fle2* fle2/ fle2sqr fle2sqrt
         fle2= fle2< fle2<= fle2> fle2>=)

(define-syntax-rule (shift-1024 x)
  (* (ann x Flonum) (flexpt 2.0 -1024.0)))

(define-syntax-rule (shift+1024 x-stx)
  (let ([x : Flonum  x-stx])
    (* (+ x x) (flexpt 2.0 +1023.0))))

(: fle2? (-> Fixnum Flonum Flonum Boolean))
(define (fle2? x.e x.hi x.lo)
  (or (not (flrational? (+ x.hi x.lo)))
      (and (fl2? x.hi x.lo)
           (let ([w  (abs (+ x.hi x.lo))])
             (or (= 0.0 w)
                 (and (>= w (flexpt 2.0 -512.0))
                      (< w (flexpt 2.0 +512.0))))))))

(: fle2-normalize (-> Fixnum Flonum Flonum (Values Fixnum Flonum Flonum)))
(define (fle2-normalize x.e x.hi x.lo)
  (define ax (abs (+ x.hi x.lo)))
  (cond [(<= (flexpt 2.0 +512.0) ax)
         (if (= ax +inf.0)
             (values 0 (+ x.hi x.lo) 0.0)
             (values (fx+ x.e 1) (shift-1024 x.hi) (shift-1024 x.lo)))]
        [(<= (flexpt 2.0 -512.0) ax)
         (values x.e x.hi x.lo)]
        [(<= 0.0 ax)
         (if (= ax 0.0)
             (values 0 (+ x.hi x.lo) 0.0)
             (values (fx- x.e 1) (shift+1024 x.hi) (shift+1024 x.lo)))]
        [else
         (values 0 +nan.0 0.0)]))

(: flonum->fle2 (-> Flonum (Values Fixnum Flonum Flonum)))
(define (flonum->fle2 x)
  (define ax (abs x))
  (cond [(not (< 0.0 ax +inf.0))
         ;; x is a Zero, NaN or Inf
         (values 0 x 0.0)]
        [(<= (flexpt 2.0 +512.0) ax)
         (values +1 (shift-1024 x) 0.0)]
        [(<= (flexpt 2.0 -512.0) ax)
         (values 0 x 0.0)]
        [else
         (values -1 (shift+1024 x) 0.0)]))

(: flonum->fle (-> Flonum (Values Fixnum Flonum)))
(define (flonum->fle x)
  (define ax (abs x))
  (cond [(<= (flexpt 2.0 +512.0) ax)
         (if (= ax +inf.0)
             (values 0 x)
             (values 1 (shift-1024 x)))]
        [(<= (flexpt 2.0 -512.0) ax)
         (values 0 x)]
        [(<= 0.0 ax)
         (if (= ax 0.0)
             (values 0 x)
             (values -1 (shift+1024 x)))]
        [else
         (values 0 +nan.0)]))

(: rational->fle2 (-> Exact-Rational (Values Fixnum Flonum Flonum)))
(define (rational->fle2 x)
  (cond [(zero? x)  (values 0 0.0 0.0)]
        [else
         (define s (sgn x))
         (define r (abs x))
         (define e
           (quotient (- (integer-length (numerator r))
                        (integer-length (denominator r)))
                     1024))
         (let loop ([r : Nonnegative-Exact-Rational  (/ r (expt 2 (* 1024 e)))]
                    [e e])
           (cond [(< r (expt 2 -512))  (loop (* r (expt 2 1024)) (- e 1))]
                 [(>= r (expt 2 512))  (loop (/ r (expt 2 1024)) (+ e 1))]
                 [else
                  (define-values (x.hi x.lo) (fl2 (* s r)))
                  (values (assert e fixnum?) x.hi x.lo)]))]))

(: fle2 (-> Real (Values Fixnum Flonum Flonum)))
(define (fle2 x)
  (cond [(flonum? x)  (flonum->fle2 x)]
        [(single-flonum? x)  (flonum->fle2 (fl x))]
        [else  (rational->fle2 x)]))

(: fle->flonum (-> Flonum Fixnum Flonum))
(define (fle->flonum x e)
  (define ax (abs x))
  (cond [(not (< 0.0 ax +inf.0))  x]
        [(> e 0)  (fle->flonum (shift+1024 x) (- e 1))]
        [(< e 0)  (fle->flonum (shift-1024 x) (+ e 1))]
        [else  x]))

(: fle2->flonum (-> Fixnum Flonum Flonum Flonum))
(define (fle2->flonum x.e x.hi x.lo)
  (fle->flonum (+ x.hi x.lo) x.e))

(: fle2->real (-> Fixnum Flonum Flonum Real))
(define (fle2->real x.e x.hi x.lo)
  (define x (+ x.hi x.lo))
  (cond [(flrational? x)  (* (+ (inexact->exact x.hi) (inexact->exact x.lo)) (expt 2 (* 1024 x.e)))]
        [else  x]))

(: fle2->bigfloat (-> Fixnum Flonum Flonum Bigfloat))
(define (fle2->bigfloat x.e x.hi x.lo)
  (define x (+ x.hi x.lo))
  (cond [(flrational? x)  (bf* (fl2->bigfloat x.hi x.lo) (bfexp2 (bf (* x.e 1024))))]
        [else  (bf x)]))

(: fle2->fl2 (-> Fixnum Flonum Flonum (Values Flonum Flonum)))
(define (fle2->fl2 x.e x.hi x.lo)
  (cond [(< 0.0 (abs (+ x.hi x.lo)) +inf.0)
         (cond [(> x.e 1)  (values (* x.hi +inf.0) 0.0)]
               [(= x.e 1)  (values (shift+1024 x.hi) (shift+1024 x.lo))]
               [(= x.e 0)  (values x.hi x.lo)]
               [(= x.e -1)  (values (shift-1024 x.hi) (shift-1024 x.lo))]
               [else  (values 0.0 0.0)])]
        [else
         (values (+ x.hi x.lo) 0.0)]))

;; ---------------------------------------------------------------------------------------------------
;; Multiplication

(: fle2* (-> Fixnum Flonum Flonum
             Fixnum Flonum Flonum
             (Values Fixnum Flonum Flonum)))
(define (fle2* x.e x.hi x.lo y.e y.hi y.lo)
  (define w (abs (+ (* x.lo y.lo) (* x.lo y.hi) (* x.hi y.lo) (* x.hi y.hi))))
  (cond [(<= (flexpt 2.0 +512.0) w)
         (define-values (z.hi z.lo)
           (fast-fl2* (* x.hi (flexpt 2.0 -512.0)) (* x.lo (flexpt 2.0 -512.0))
                      (* y.hi (flexpt 2.0 -512.0)) (* y.lo (flexpt 2.0 -512.0))))
         (values (fx+ (fx+ x.e y.e) 1) z.hi z.lo)]
        [(<= (flexpt 2.0 -512.0) w)
         (define-values (z.hi z.lo) (fast-fl2* x.hi x.lo y.hi y.lo))
         (cond [(<= (flexpt 2.0 +512.0) (abs z.hi))
                (values (fx+ (fx+ x.e y.e) 1) (shift-1024 z.hi) (shift-1024 z.lo))]
               [else
                (values (fx+ x.e y.e) z.hi z.lo)])]
        [(= w 0.0)
         (values 0 0.0 0.0)]
        [else
         (define-values (z.hi z.lo)
           (fast-fl2* (* x.hi (flexpt 2.0 +512.0)) (* x.lo (flexpt 2.0 +512.0))
                      (* y.hi (flexpt 2.0 +512.0)) (* y.lo (flexpt 2.0 +512.0))))
         (cond [(<= (flexpt 2.0 +512.0) (abs z.hi))
                (values (fx+ x.e y.e) (shift-1024 z.hi) (shift-1024 z.lo))]
               [else
                (values (fx- (fx+ x.e y.e) 1) z.hi z.lo)])]))

(: fl*/e2 (-> Flonum Flonum (Values Fixnum Flonum Flonum)))
(define (fl*/e2 x y)
  (define-values (x.e x.hi) (flonum->fle x))
  (define-values (y.e y.hi) (flonum->fle y))
  (define w (abs (* x.hi y.hi)))
  (cond [(<= (flexpt 2.0 +512.0) w)
         (define-values (z.hi z.lo) (fast-fl*/error (* x.hi (flexpt 2.0 -512.0))
                                                    (* y.hi (flexpt 2.0 -512.0))))
         (values (unsafe-fx+ (unsafe-fx+ x.e y.e) 1) z.hi z.lo)]
        [(<= (flexpt 2.0 -512.0) w)
         (define-values (z.hi z.lo) (fast-fl*/error x.hi y.hi))
         (values (unsafe-fx+ x.e y.e) z.hi z.lo)]
        [(= w 0.0)
         (values 0 0.0 0.0)]
        [else
         (define-values (z.hi z.lo) (fast-fl*/error (* x.hi (flexpt 2.0 +512.0))
                                                    (* y.hi (flexpt 2.0 +512.0))))
         (values (unsafe-fx- (unsafe-fx+ x.e y.e) 1) z.hi z.lo)]))

;; ---------------------------------------------------------------------------------------------------
;; Squaring

(: fle2sqr (-> Fixnum Flonum Flonum
               (Values Fixnum Flonum Flonum)))
(define (fle2sqr x.e x.hi x.lo)
  (define w (abs (+ (* x.lo x.lo) (* x.lo x.hi 2.0) (* x.hi x.hi))))
  (cond [(<= (flexpt 2.0 +512.0) w)
         (define-values (z.hi z.lo)
           (fast-fl2sqr (* x.hi (flexpt 2.0 -512.0)) (* x.lo (flexpt 2.0 -512.0))))
         (values (fx+ (fxlshift x.e 1) 1) z.hi z.lo)]
        [(<= (flexpt 2.0 -512.0) w)
         (define-values (z.hi z.lo) (fast-fl2sqr x.hi x.lo))
         (values (fxlshift x.e 1) z.hi z.lo)]
        [(= w 0.0)
         (values 0 0.0 0.0)]
        [else
         (define-values (z.hi z.lo)
           (fast-fl2sqr (* x.hi (flexpt 2.0 +512.0)) (* x.lo (flexpt 2.0 +512.0))))
         (values (fx- (fxlshift x.e 1) 1) z.hi z.lo)]))

(: flsqr/e2 (-> Flonum (Values Fixnum Flonum Flonum)))
(define (flsqr/e2 x)
  (define-values (x.e x.hi) (flonum->fle x))
  (define w (abs (* x.hi x.hi)))
  (cond [(<= (flexpt 2.0 +512.0) w)
         (define-values (z.hi z.lo) (fast-flsqr/error (* x.hi (flexpt 2.0 -512.0))))
         (values (unsafe-fx+ (unsafe-fxlshift x.e 1) 1) z.hi z.lo)]
        [(<= (flexpt 2.0 -512.0) w)
         (define-values (z.hi z.lo) (fast-flsqr/error x.hi))
         (values (unsafe-fxlshift x.e 1) z.hi z.lo)]
        [(= w 0.0)
         (values 0 0.0 0.0)]
        [else
         (define-values (z.hi z.lo) (fast-flsqr/error (* x.hi (flexpt 2.0 +512.0))))
         (values (unsafe-fx- (unsafe-fxlshift x.e 1) 1) z.hi z.lo)]))

;; ---------------------------------------------------------------------------------------------------
;; Division

(: fle2/ (-> Fixnum Flonum Flonum
             Fixnum Flonum Flonum
             (Values Fixnum Flonum Flonum)))
(define (fle2/ x.e x.hi x.lo y.e y.hi y.lo)
  (define w (abs (/ (+ x.hi x.lo) (+ y.hi y.lo))))
  (cond [(< (flexpt 2.0 +512.0) w)
         (define-values (z.hi z.lo)
           (fast-fl2/ (* x.hi (flexpt 2.0 -512.0)) (* x.lo (flexpt 2.0 -512.0))
                      (* y.hi (flexpt 2.0 +512.0)) (* y.lo (flexpt 2.0 +512.0))))
         (values (fx+ (fx- x.e y.e) 1) z.hi z.lo)]
        [(< (flexpt 2.0 -512.0) w)
         (define-values (z.hi z.lo) (fast-fl2/ x.hi x.lo y.hi y.lo))
         (cond [(<= (flexpt 2.0 +512.0) (abs z.hi))
                (values (fx+ (fx- x.e y.e) 1) (shift-1024 z.hi) (shift-1024 z.lo))]
               [else
                (values (fx- x.e y.e) z.hi z.lo)])]
        [(= w 0.0)
         (values 0 0.0 0.0)]
        [else
         (define-values (z.hi z.lo)
           (fast-fl2/ (* x.hi (flexpt 2.0 +512.0)) (* x.lo (flexpt 2.0 +512.0))
                      (* y.hi (flexpt 2.0 -512.0)) (* y.lo (flexpt 2.0 -512.0))))
         (cond [(<= (flexpt 2.0 +512.0) (abs z.hi))
                (values (fx- x.e y.e) (shift-1024 z.hi) (shift-1024 z.lo))]
               [else
                (values (fx- (fx- x.e y.e) 1) z.hi z.lo)])]))

(: fl//e2 (-> Flonum Flonum (Values Fixnum Flonum Flonum)))
(define (fl//e2 x y)
  (define-values (x.e x.hi) (flonum->fle x))
  (define-values (y.e y.hi) (flonum->fle y))
  (define w (abs (/ x.hi y.hi)))
  (cond [(<= (flexpt 2.0 +512.0) w)
         (define-values (z.hi z.lo) (fast-fl//error (* x.hi (flexpt 2.0 -512.0))
                                                    (* y.hi (flexpt 2.0 +512.0))))
         (values (unsafe-fx+ (unsafe-fx- x.e y.e) 1) z.hi z.lo)]
        [(<= (flexpt 2.0 -512.0) w)
         (define-values (z.hi z.lo) (fast-fl//error x.hi y.hi))
         (values (unsafe-fx- x.e y.e) z.hi z.lo)]
        [(= w 0.0)
         (values 0 0.0 0.0)]
        [else
         (define-values (z.hi z.lo) (fast-fl//error (* x.hi (flexpt 2.0 +512.0))
                                                    (* y.hi (flexpt 2.0 -512.0))))
         (values (unsafe-fx- (unsafe-fx- x.e y.e) 1) z.hi z.lo)]))

;; ---------------------------------------------------------------------------------------------------
;; Square root

(: fle2sqrt (-> Fixnum Flonum Flonum
                (Values Fixnum Flonum Flonum)))
(define (fle2sqrt x.e x.hi x.lo)
  (define-values (y.hi y.lo) (fast-fl2sqrt x.hi x.lo))
  (cond [(zero? (bitwise-and x.e 1))  ; (even? x.e)
         (values (unsafe-fxrshift x.e 1) y.hi y.lo)]
        [(<= 1.0 y.hi)
         (values (unsafe-fx+ (unsafe-fxrshift x.e 1) 1)
                 (* y.hi (flexpt 2.0 -512.0))
                 (* y.lo (flexpt 2.0 -512.0)))]
        [else
         (values (unsafe-fxrshift x.e 1)
                 (* y.hi (flexpt 2.0 +512.0))
                 (* y.lo (flexpt 2.0 +512.0)))]))

(: flsqrt/e2 (-> Flonum (Values Fixnum Flonum Flonum)))
(define (flsqrt/e2 x)
  (define-values (x.e x.hi) (flonum->fle x))
  (define-values (y.hi y.lo) (fast-flsqrt/error x.hi))
  (cond [(zero? (bitwise-and x.e 1))  ; (even? x.e)
         (values (unsafe-fxrshift x.e 1) y.hi y.lo)]
        [(<= 1.0 y.hi)
         (values (unsafe-fx+ (unsafe-fxrshift x.e 1) 1)
                 (* y.hi (flexpt 2.0 -512.0))
                 (* y.lo (flexpt 2.0 -512.0)))]
        [else
         (values (unsafe-fxrshift x.e 1)
                 (* y.hi (flexpt 2.0 +512.0))
                 (* y.lo (flexpt 2.0 +512.0)))]))

;; ---------------------------------------------------------------------------------------------------
;; Addition and subtraction

(: fle2+ (-> Fixnum Flonum Flonum
             Fixnum Flonum Flonum
             (Values Fixnum Flonum Flonum)))
(define (fle2+ x.e x.hi x.lo y.e y.hi y.lo)
  (cond [(= x.e y.e)
         (define-values (z.hi z.lo) (fast-fl2+ x.hi x.lo y.hi y.lo))
         (fle2-normalize x.e z.hi z.lo)]
        [(= (+ x.hi x.lo) 0.0)  (values y.e y.hi y.lo)]
        [(= (+ y.hi y.lo) 0.0)  (values x.e x.hi x.lo)]
        [else
         (define d (unsafe-fx- x.e y.e))
         (cond
           [(= d -1)
            (fle2+ y.e (shift-1024 x.hi) (shift-1024 x.lo) y.e y.hi y.lo)]
           [(= d +1)
            (fle2+ x.e x.hi x.lo x.e (shift-1024 y.hi) (shift-1024 y.lo))]
           [(< d -1)
            (values y.e y.hi y.lo)]
           [else
            (values x.e x.hi x.lo)])]))

(: fl+/e2 (-> Flonum Flonum (Values Fixnum Flonum Flonum)))
(define (fl+/e2 x y)
  (define-values (x.e x.hi) (flonum->fle x))
  (define-values (y.e y.hi) (flonum->fle y))
  (cond [(= x.e y.e)
         (define-values (z.hi z.lo) (fast-fl+/error x.hi y.hi))
         (fle2-normalize x.e z.hi z.lo)]
        [(= x 0.0)  (values y.e y.hi 0.0)]
        [(= y 0.0)  (values x.e x.hi 0.0)]
        [else
         (define d (unsafe-fx- x.e y.e))
         (cond
           [(= d -1)
            (define-values (z.hi z.lo) (fast-fl+/error (shift-1024 x.hi) y.hi))
            (fle2-normalize y.e z.hi z.lo)]
           [(= d +1)
            (define-values (z.hi z.lo) (fast-fl+/error x.hi (shift-1024 y.hi)))
            (fle2-normalize x.e z.hi z.lo)]
           [(< d -1)
            (values y.e y.hi 0.0)]
           [else
            (values x.e x.hi 0.0)])]))

(: fle2- (-> Fixnum Flonum Flonum
             Fixnum Flonum Flonum
             (Values Fixnum Flonum Flonum)))
(define (fle2- x.e x.hi x.lo y.e y.hi y.lo)
  (fle2+ x.e x.hi x.lo y.e (- y.hi) (- y.lo)))

(: fl-/e2 (-> Flonum Flonum (Values Fixnum Flonum Flonum)))
(define (fl-/e2 x y)
  (fl+/e2 x (- y)))

;; ---------------------------------------------------------------------------------------------------
;; Comparisons

(: fle2= (-> Fixnum Flonum Flonum
             Fixnum Flonum Flonum
             Boolean))
(define (fle2= x.e x.hi x.lo y.e y.hi y.lo)
  (and (= x.e y.e) (= x.hi y.hi) (= x.lo y.lo)))

(: fle2< (-> Fixnum Flonum Flonum
             Fixnum Flonum Flonum
             Boolean))
(define (fle2< x.e x.hi x.lo y.e y.hi y.lo)
  (define sx (flsgn (+ x.hi x.lo)))
  (define sy (flsgn (+ y.hi y.lo)))
  (cond [(< sx sy)  #t]
        [(< sy sx)  #f]
        [(= sx 0.0)  #f]
        [(< x.e y.e)  (> sx 0.0)]
        [(< y.e x.e)  (< sx 0.0)]
        [else
         (define-values (z.hi z.lo) (fast-fl2- x.hi x.lo y.hi y.lo))
         (< (+ z.hi z.lo) 0.0)]))

(: fle2<= (-> Fixnum Flonum Flonum
              Fixnum Flonum Flonum
              Boolean))
(define (fle2<= x.e x.hi x.lo y.e y.hi y.lo)
  (or (fle2= x.e x.hi x.lo y.e y.hi y.lo)
      (fle2< x.e x.hi x.lo y.e y.hi y.lo)))

(: fle2> (-> Fixnum Flonum Flonum
             Fixnum Flonum Flonum
             Boolean))
(define (fle2> x.e x.hi x.lo y.e y.hi y.lo)
  (not (fle2<= x.e x.hi x.lo y.e y.hi y.lo)))

(: fle2>= (-> Fixnum Flonum Flonum
              Fixnum Flonum Flonum
              Boolean))
(define (fle2>= x.e x.hi x.lo y.e y.hi y.lo)
  (not (fle2< x.e x.hi x.lo y.e y.hi y.lo)))

;; ===================================================================================================
#|
(require math/base
         plot/typed
         (only-in racket/unsafe/ops unsafe-flvector-set!))

(plot3d (contour-intervals3d
         (λ (x y)
           (let ([x  (fl x)] [y  (fl y)])
             (define-values (z.e z.hi z.lo) (fl*/e2 x y))
             (define err
               (relative-error (fle2->real z.e z.hi z.lo)
                               (* (inexact->exact x)
                                  (inexact->exact y))))
             (when (> err 1.5e-30)
               (printf "~a: ~a ~a~n~a ~a ~a~n~n"
                       (fl err)
                       x
                       y
                       z.e z.hi z.lo))
             err))
         #e-1.1e+155 #e1.11e+155 #e-1.1e+155 #e1.11e+155))

(plot (function
       (λ (x)
         (define-values (x.e x.hi x.lo) (fle2 x))
         (define-values (y.e y.hi y.lo) (fle2sqrt x.e x.hi x.lo))
         (define err
           (relative-error (fle2->real y.e y.hi y.lo)
                           (bigfloat->real (bfsqrt (fle2->bigfloat x.e x.hi x.lo)))))
         (when (> err 1.5e-30)
           (printf "~a:~n~a ~a ~a~n~a ~a ~a~n~n"
                   (fl err)
                   x.e x.hi x.lo
                   y.e y.hi y.lo))
         err)
       0 #e1.11e+310))

(plot (function
       (λ (x)
         (let ([x  (fl x)])
           (define-values (y.e y.hi y.lo) (flsqr/e2 x))
           (define err
             (relative-error (fle2->real y.e y.hi y.lo)
                             (bigfloat->real (bfsqr (bf x)))))
           (when (> err 1.5e-30)
             (printf "~a: ~a~n~a ~a ~a~n~n"
                     (fl err)
                     x
                     y.e y.hi y.lo))
           err))
       0 +max.0))

(plot3d (contour-intervals3d
         (λ (x y)
           (define-values (x.e x.hi x.lo) (fle2 x))
           (define-values (y.e y.hi y.lo) (fle2 y))
           (define-values (z.e z.hi z.lo) (fle2+ x.e x.hi x.lo y.e y.hi y.lo))
           (define err
             (relative-error (fle2->real z.e z.hi z.lo)
                             (+ (fle2->real x.e x.hi x.lo)
                                (fle2->real y.e y.hi y.lo))))
           (when (> err 1.5e-30)
             (printf "~a:~n~a ~a ~a~n~a ~a ~a~n~a ~a ~a~n~n"
                     (fl err)
                     x.e x.hi x.lo
                     y.e y.hi y.lo
                     z.e z.hi z.lo))
           err)
         #e-1.1e+155 #e1.11e+155 #e-1.1e+155 #e1.11e+155))

(define flmin-ord (flonum->ordinal -max.0))
(define flmax-ord (flonum->ordinal +inf.0))

(: random-flonum (-> Flonum))
(define (random-flonum)
  (ordinal->flonum (random-integer flmin-ord flmax-ord)))

(define min-ord (flonum->ordinal (flexpt 2.0 -512.0)))
(define max-ord (flonum->ordinal (flexpt 2.0 +512.0)))

(: random-fl2 (-> (Values Flonum Flonum)))
(define (random-fl2)
  (define x.hi
    (let ([r  (random)])
      (* (cond [(< r 0.05)  0.0]
               [(< r 0.10)  (flexpt 2.0 -512.0)]
               [(< r 0.15)  (flexpt 2.0 +512.0)]
               [(< r 0.20)  (flexpt 2.0 -256.0)]
               [(< r 0.25)  (flexpt 2.0 +256.0)]
               [else  (ordinal->flonum (random-integer min-ord max-ord))])
         (if (< (random) 0.5) -1.0 1.0))))
  
  (define x.lo
    (* (cond [(= x.hi 0.0)  0.0]
             [else  (* (random) (flulp x.hi))])
       (if (< (random) 0.5) -1.0 1.0)))
  
  (let-values ([(x.hi x.lo)  (fl+/error x.hi x.lo)])
    (define-values (a.hi a.lo) (fl2abs x.hi x.lo))
    (cond [(and (or (< a.hi (flexpt 2.0 -512.0))
                    (>= a.hi (flexpt 2.0 +512.0)))
                (not (= 0.0 (+ a.hi a.lo))))
           (random-fl2)]
          [else
           (values x.hi x.lo)])))

(: random-fle2 (-> (Values Fixnum Flonum Flonum)))
(define (random-fle2)
  (define-values (x.hi x.lo) (random-fl2))
  (values (fx- (random 5) 2) x.hi x.lo))

(define m 10)
(define n 200000)
(: xs (Listof (Vector Fixnum Flonum Flonum)))
(define xs (build-list n (λ (_)
                           (define-values (x.e x.hi x.lo) (random-fle2))
                           (: v (Vector Fixnum Flonum Flonum))
                           (define v (vector x.e x.hi x.lo))
                           v)))

(define fs (build-list n (λ (_) (random-flonum))))

(: test-unary-op/e2 (-> (-> Flonum (Values Fixnum Flonum Flonum))
                        (-> Real Real)
                        Void))
(define (test-unary-op/e2 flop/e2 op)
  (printf "Testing ~a~n" flop/e2)
  (printf "-------------------------------------------~n")
  (for ([x  (in-list fs)])
    (define z (op (inexact->exact x)))
    (define-values (z.e z.hi z.lo) (flop/e2 x))
    (when (not (fle2? z.e z.hi z.lo))
      (printf "not fle2: ~a ~a ~a~n" z.e z.hi z.lo)
      (printf "~a~n~n" x))
    (define err (fl (relative-error (fle2->real z.e z.hi z.lo) z)))
    (when (and (> err 1.5e-30)
               (not (and (not (rational? z))
                         (not (flrational? (+ z.hi z.lo))))))
      (printf "~a: ~a~n" err x)
      (printf "~a vs ~a~n" (fle2->flonum z.e z.hi z.lo) (fl z))
      (newline)))
  (printf "-------------------------------------------~n~n"))

(: test-binary-op/e2 (-> (-> Flonum Flonum (Values Fixnum Flonum Flonum))
                         (-> Real Real Real)
                         Void))
(define (test-binary-op/e2 flop/e2 op)
  (printf "Testing ~a~n" flop/e2)
  (printf "-------------------------------------------~n")
  (for ([x  (in-list fs)]
        [y  (in-list (rest fs))])
    (define z (op (inexact->exact x) (inexact->exact y)))
    (define-values (z.e z.hi z.lo) (flop/e2 x y))
    (when (not (fle2? z.e z.hi z.lo))
      (printf "not fle2: ~a ~a ~a~n" z.e z.hi z.lo)
      (printf "~a ~a~n~n" x y))
    (define err (fl (relative-error (fle2->real z.e z.hi z.lo) z)))
    (when (and (> err 1.5e-30)
               (not (and (not (rational? z))
                         (not (flrational? (+ z.hi z.lo))))))
      (printf "~a: ~a ~a~n" err x y)
      (printf "~a vs ~a~n" (fle2->flonum z.e z.hi z.lo) (fl z))
      (newline)))
  (printf "-------------------------------------------~n~n"))

(: test-unary-op (-> (-> Fixnum Flonum Flonum
                         (Values Fixnum Flonum Flonum))
                     (-> Real Real)
                     Void))
(define (test-unary-op fle2-op op)
  (printf "Testing ~a~n" fle2-op)
  (printf "-------------------------------------------~n")
  (for ([x  (in-list xs)])
    (define x.e (vector-ref x 0))
    (define x.hi (vector-ref x 1))
    (define x.lo (vector-ref x 2))
    (define z (op (fle2->real x.e x.hi x.lo)))
    (define-values (z.e z.hi z.lo) (fle2-op x.e x.hi x.lo))
    (when (not (fle2? z.e z.hi z.lo))
      (printf "not fle2: ~a ~a ~a~n" z.e z.hi z.lo)
      (printf "~a ~a ~a~n~n" x.e x.hi x.lo))
    (define err (fl (relative-error (fle2->real z.e z.hi z.lo) z)))
    (when (and (> err 1.5e-30)
               (not (and (not (rational? z))
                         (not (flrational? (+ z.hi z.lo))))))
      (printf "~a:~n~a ~a ~a~n" err x.e x.hi x.lo)
      (printf "~a vs ~a~n" (fle2->flonum z.e z.hi z.lo) (fl z))
      (newline)))
  (printf "-------------------------------------------~n~n"))

(: test-binary-op (-> (-> Fixnum Flonum Flonum
                          Fixnum Flonum Flonum
                          (Values Fixnum Flonum Flonum))
                      (-> Real Real Real)
                      Void))
(define (test-binary-op fle2-op op)
  (printf "Testing ~a~n" fle2-op)
  (printf "-------------------------------------------~n")
  (for ([x  (in-list xs)]
        [y  (in-list (rest xs))])
    (define x.e (vector-ref x 0))
    (define x.hi (vector-ref x 1))
    (define x.lo (vector-ref x 2))
    (define y.e (vector-ref y 0))
    (define y.hi (vector-ref y 1))
    (define y.lo (vector-ref y 2))
    (define z (op (fle2->real x.e x.hi x.lo)
                  (fle2->real y.e y.hi y.lo)))
    (define-values (z.e z.hi z.lo) (fle2-op x.e x.hi x.lo y.e y.hi y.lo))
    (when (not (fle2? z.e z.hi z.lo))
      (printf "not fle2: ~a ~a ~a~n" z.e z.hi z.lo)
      (printf "~a ~a ~a~n" x.e x.hi x.lo)
      (printf "~a ~a ~a~n~n" y.e y.hi y.lo))
    (define err (fl (relative-error (fle2->real z.e z.hi z.lo) z)))
    (when (and (> err 1.5e-30)
               (not (and (not (rational? z))
                         (not (flrational? (+ z.hi z.lo))))))
      (printf "~a:~n~a ~a ~a~n~a ~a ~a~n" err x.e x.hi x.lo y.e y.hi y.lo)
      (printf "~a vs ~a~n" (fle2->flonum z.e z.hi z.lo) (fl z))
      (newline)))
  (printf "-------------------------------------------~n~n"))

(: test-compare-op (-> (-> Fixnum Flonum Flonum
                           Fixnum Flonum Flonum
                           Boolean)
                       (-> Real Real Boolean)
                       Void))
(define (test-compare-op fle2-op op)
  (printf "Testing ~a~n" fle2-op)
  (printf "-------------------------------------------~n")
  (for ([x  (in-list xs)]
        [y  (in-list (rest xs))])
    (define x.e (vector-ref x 0))
    (define x.hi (vector-ref x 1))
    (define x.lo (vector-ref x 2))
    (define y.e (vector-ref y 0))
    (define y.hi (vector-ref y 1))
    (define y.lo (vector-ref y 2))
    (define a (op (fle2->real x.e x.hi x.lo)
                  (fle2->real y.e y.hi y.lo)))
    (define b (fle2-op x.e x.hi x.lo y.e y.hi y.lo))
    (unless (eq? a b)
      (printf "~a ~a ~a~n~a ~a ~a~n" x.e x.hi x.lo y.e y.hi y.lo)
      (printf "~a vs ~a~n" a b)
      (newline)))
  (printf "-------------------------------------------~n~n"))

;(test-unary-op/e2 flsqr/e2 sqr)
;(test-unary-op/e2 flsqrt/e2 (λ (x) (bigfloat->real (bfsqrt (bf x)))))
;(test-binary-op/e2 fl*/e2 *)
;(test-binary-op/e2 fl//e2 (λ (x y) (if (= y 0) (/ (fl x) (fl y)) (/ x y))))
(test-binary-op/e2 fl+/e2 +)

;(test-binary-op fle2* *)
;(test-binary-op fle2/ (λ (x y) (if (= y 0) (/ (fl x) (fl y)) (/ x y))))
;(test-binary-op fle2+ +)
;(test-unary-op fle2sqr sqr)
;(test-unary-op fle2sqrt (λ (x) (bigfloat->real (bfsqrt (bf x)))))
;(test-compare-op fle2< <)

;; ===================================================================================================
;; Speed tests

(define bx (flvector 0.0))
(define: fxnum : Fixnum  0)
(define: bool : Boolean  #f)

#;
(for ([_  (in-range 5)])
  (time
   (for ([_  (in-range m)])
     (for ([x  (in-list fs)])
       (define-values (z.e z.hi z.lo) (flsqrt/e2 x))
       (unsafe-flvector-set! bx 0 z.hi)
       (unsafe-flvector-set! bx 0 z.lo)
       (set! fxnum z.e)))))
(newline)


(for ([_  (in-range 5)])
  (time
   (for ([_  (in-range m)])
     (for ([x  (in-list fs)]
           [y  (in-list (rest fs))])
       (define-values (z.e z.hi z.lo) (fl+/e2 x y))
       (unsafe-flvector-set! bx 0 z.hi)
       (unsafe-flvector-set! bx 0 z.lo)
       (set! fxnum z.e)))))
(newline)

#;
(for ([_  (in-range 5)])
  (time
   (for ([_  (in-range m)])
     (for ([x  (in-list xs)]
           [y  (in-list (rest xs))])
       (define x.e (vector-ref x 0))
       (define x.hi (vector-ref x 1))
       (define x.lo (vector-ref x 2))
       (define y.e (vector-ref y 0))
       (define y.hi (vector-ref y 1))
       (define y.lo (vector-ref y 2))
       (define-values (z.e z.hi z.lo) (fle2+ x.e x.hi x.lo y.e y.hi y.lo))
       (unsafe-flvector-set! bx 0 z.hi)
       (unsafe-flvector-set! bx 0 z.lo)
       (set! fxnum z.e)))))
(newline)

#;
(for ([_  (in-range 5)])
  (time
   (for ([_  (in-range m)])
     (for ([x  (in-list xs)]
           [y  (in-list (rest xs))])
       (define x.e (vector-ref x 0))
       (define x.hi (vector-ref x 1))
       (define x.lo (vector-ref x 2))
       (define y.e (vector-ref y 0))
       (define y.hi (vector-ref y 1))
       (define y.lo (vector-ref y 2))
       (define a (fle2< x.e x.hi x.lo y.e y.hi y.lo))
       (set! bool a)))))
(newline)

#;
(for ([_  (in-range 5)])
  (time
   (for ([_  (in-range m)])
     (for ([x  (in-list xs)])
       (define x.e (vector-ref x 0))
       (define x.hi (vector-ref x 1))
       (define x.lo (vector-ref x 2))
       (define-values (y.e y.hi y.lo) (fle2sqrt x.e x.hi x.lo))
       (unsafe-flvector-set! bx 0 y.hi)
       (unsafe-flvector-set! bx 0 y.lo)
       (set! fxnum y.e)))))
|#
