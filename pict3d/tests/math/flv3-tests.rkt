#lang typed/racket

(require math/flonum
         math/base
         math/distributions
         math/bigfloat
         (only-in racket/unsafe/ops
                  unsafe-flvector-set!)
         pict3d/private/math)

(define num-accuracy   50000)
(define num-speed   10000000)

(: fl3ulp-error (-> Flonum Flonum Flonum Real Real Real Flonum))
(define (fl3ulp-error x y z x* y* z*)
  (max (flulp-error x x*)
       (flulp-error y y*)
       (flulp-error z z*)))

(define +speed-max.0 (flexpt 2.0 32.0))
(define -speed-max.0 (- +speed-max.0))
(define +speed-max (flonum->ordinal +speed-max.0))
(define -speed-max (flonum->ordinal -speed-max.0))

(define +acc-max.0 (flexpt 2.0 511.0))
(define -acc-max.0 (- +acc-max.0))
(define +acc-max (flonum->ordinal +acc-max.0))
(define -acc-max (flonum->ordinal -acc-max.0))

(define -max-subnormal (flonum->ordinal (- +max-subnormal.0)))
(define +max-subnormal (flonum->ordinal +max-subnormal.0))

(define dstr (normal-dist 0 10))

(define (random-speed-flonum)
  (define r (random))
  (cond [(< r 0.5)  (* (- (random) 0.5) 2.0 +speed-max.0)]
        [else  (ordinal->flonum (random-integer -speed-max +speed-max))]))

(define (random-speed-flv3)
  (flv3 (random-speed-flonum) (random-speed-flonum) (random-speed-flonum)))

(define (random-acc-flonum)
  (define r (random))
  (cond [(< r 0.5)  (* (- (random) 0.5) 2.0 +acc-max.0)]
        [(< r 0.6)  (ordinal->flonum (random-integer -max-subnormal +max-subnormal))]
        [(< r 0.7)  (if (< (random) 0.5) -0.0 0.0)]
        [(< r 0.8)  (sample dstr)]
        [else  (ordinal->flonum (random-integer -acc-max +acc-max))]))

(define acc-xs
  (time
   (build-list (+ num-accuracy 8) (λ (_) (random-acc-flonum)))))

(define speed-vs
  (time
   (let ([vs  (build-list 10000 (λ (_) (random-speed-flv3)))])
     (append* (make-list (quotient num-speed 10000) vs)))))

(define speed-xs
  (time
   (let ([xs  (build-list 10000 (λ (_) (random-speed-flonum)))])
     (append* (make-list (quotient num-speed 10000) xs)))))

(define speed-ps
  (time
   (let ([ps  (build-list 10000 (λ (_)
                                  (define a (random-speed-flonum))
                                  (define b (random-speed-flonum))
                                  (define c (random-speed-flonum))
                                  (define d (random-speed-flonum))
                                  (assert (flplane3 (flv3 a b c) d) values)))])
     (append* (make-list (quotient num-speed 10000) ps)))))

(define speed-ts
  (time
   (let ([vs  (build-list 10000 (λ (_) (flv3 (- (random) 0.5) (- (random) 0.5) (- (random) 0.5))))])
     (define ts
       (for/list : (Listof FlAffine3) ([dx  (in-list vs)]
                                       [dy  (in-list (rest vs))]
                                       [dz  (in-list (rest (rest vs)))]
                                       [p   (in-list (rest (rest (rest vs))))])
         (cols->flaffine3 dx dy dz p)))
     (append* (make-list (quotient num-speed 10000) ts)))))

(define vbx (make-flvector 4))
(define: bx : Any  #f)

;; ===================================================================================================
;; fl.rkt

(define (test-fast-flfma)
  (printf "Testing fast-flfma~n")
  
  (for ([_  (in-range 5)])
    (time (for ([x1  (in-list speed-xs)]
                [s   (in-list (rest speed-xs))]
                [x2  (in-list (rest (rest speed-xs)))])
            (unsafe-flvector-set! vbx 0 (fast-flfma x1 s x2))))
    (printf "heap: ~v~n" (current-memory-use)))
  
  (for ([x1  (in-list acc-xs)]
        [s   (in-list (rest acc-xs))]
        [x2  (in-list (rest (rest acc-xs)))]
        [i  (in-naturals 1)])
    (when (zero? (modulo i 10000))
      (printf "Accuracy iteration i = ~v~n" i))
    (define x (fast-flfma x1 s x2))
    (define x* (+ (* (inexact->exact x1) (inexact->exact s)) (inexact->exact x2)))
    (define e (flulp-error x x*))
    (when (> e 4.0)
      (eprintf "~v: ~v ~v ~v~n    ~v    ~v~n~n" e x1 s x2 x (fl x*))))
  
  (newline))

;; ===================================================================================================
;; fl2.rkt

(define (test-fl2dot)
  (printf "Testing fl2dot~n")
  
  (for ([_  (in-range 5)])
    (time (for ([x1  (in-list speed-xs)]
                [y1  (in-list (rest speed-xs))]
                [x2  (in-list (rest (rest speed-xs)))]
                [y2  (in-list (rest (rest (rest speed-xs))))])
            (unsafe-flvector-set! vbx 0 (fl2dot x1 y1 x2 y2))))
    (printf "heap: ~v~n" (current-memory-use)))
  
  (for ([x1  (in-list acc-xs)]
        [y1  (in-list (rest acc-xs))]
        [x2  (in-list (rest (rest acc-xs)))]
        [y2  (in-list (rest (rest (rest acc-xs))))]
        [i  (in-naturals 1)])
    (when (zero? (modulo i 10000))
      (printf "Accuracy iteration i = ~v~n" i))
    (define m (fl2dot x1 y1 x2 y2))
    (define m* (+ (* (inexact->exact x1) (inexact->exact x2))
                  (* (inexact->exact y1) (inexact->exact y2))))
    (define e (flulp-error m m*))
    (when (> e 4.0)
      (eprintf "~v: ~v ~v ~v ~v~n    ~v ~v~n~n" e x1 y1 x2 y2 m (fl m*))))
  
  (newline))

;; ===================================================================================================
;; fl3.rkt

(define (test-call/fl3fma)
  (printf "Testing call/fl3fma~n")
  (for ([_  (in-range 5)])
    (time (for ([x1  (in-list speed-xs)]
                [y1  (in-list (rest speed-xs))]
                [z1  (in-list (rest (rest speed-xs)))]
                [x2  (in-list (rest (rest (rest speed-xs))))]
                [y2  (in-list (rest (rest (rest (rest speed-xs)))))]
                [z2  (in-list (rest (rest (rest (rest (rest speed-xs))))))]
                [s   (in-list (rest (rest (rest (rest (rest (rest speed-xs)))))))])
            (call/fl3fma x1 y1 z1 s x2 y2 z2
              (λ (x y z)
                (unsafe-flvector-set! vbx 0 x)
                (unsafe-flvector-set! vbx 1 y)
                (unsafe-flvector-set! vbx 2 z)))))
    (printf "heap: ~v~n" (current-memory-use)))
  
  (for ([x1  (in-list acc-xs)]
        [y1  (in-list (rest acc-xs))]
        [z1  (in-list (rest (rest acc-xs)))]
        [x2  (in-list (rest (rest (rest acc-xs))))]
        [y2  (in-list (rest (rest (rest (rest acc-xs)))))]
        [z2  (in-list (rest (rest (rest (rest (rest acc-xs))))))]
        [s   (in-list (rest (rest (rest (rest (rest (rest acc-xs)))))))]
        [i  (in-naturals 1)])
    (when (zero? (modulo i 10000))
      (printf "Accuracy iteration i = ~v~n" i))
    (define x* (+ (* (inexact->exact x1) (inexact->exact s)) (inexact->exact x2)))
    (define y* (+ (* (inexact->exact y1) (inexact->exact s)) (inexact->exact y2)))
    (define z* (+ (* (inexact->exact z1) (inexact->exact s)) (inexact->exact z2)))
    (call/fl3fma x1 y1 z1 s x2 y2 z2
      (λ (x y z)
        (define e (fl3ulp-error x y z x* y* z*))
        (when (> e 4.0)
          (eprintf "~v: ~v ~v ~v ~v ~v ~v ~v~n    ~v ~v ~v~n    ~v ~v ~v~n~n"
                   e x1 y1 z1 s x2 y2 z2 x y z (fl x*) (fl y*) (fl z*))))))
  
  (newline))

(define (test-fl3dot)
  (printf "Testing fl3dot~n")
  
  (for ([_  (in-range 5)])
    (time (for ([x1  (in-list speed-xs)]
                [y1  (in-list (rest speed-xs))]
                [z1  (in-list (rest (rest speed-xs)))]
                [x2  (in-list (rest (rest (rest speed-xs))))]
                [y2  (in-list (rest (rest (rest (rest speed-xs)))))]
                [z2  (in-list (rest (rest (rest (rest (rest speed-xs))))))])
            (unsafe-flvector-set! vbx 0 (fl3dot x1 y1 z1 x2 y2 z2))))
    (printf "heap: ~v~n" (current-memory-use)))
  
  (for ([x1  (in-list acc-xs)]
        [y1  (in-list (rest acc-xs))]
        [z1  (in-list (rest (rest acc-xs)))]
        [x2  (in-list (rest (rest (rest acc-xs))))]
        [y2  (in-list (rest (rest (rest (rest acc-xs)))))]
        [z2  (in-list (rest (rest (rest (rest (rest acc-xs))))))]
        [i  (in-naturals 1)])
    (when (zero? (modulo i 10000))
      (printf "Accuracy iteration i = ~v~n" i))
    (define m (fl3dot x1 y1 z1 x2 y2 z2))
    (define m* (+ (* (inexact->exact x1) (inexact->exact x2))
                  (* (inexact->exact y1) (inexact->exact y2))
                  (* (inexact->exact z1) (inexact->exact z2))))
    (define e (flulp-error m m*))
    (when (> e 3.75)
      (eprintf "~v: ~v ~v ~v ~v~n    ~v ~v~n~n" e x1 y1 x2 y2 m (fl m*))))
  
  (newline))

(define (test-call/fl3cross)
  (printf "Testing call/fl3cross~n")
  (for ([_  (in-range 5)])
    (time (for ([x1  (in-list speed-xs)]
                [y1  (in-list (rest speed-xs))]
                [z1  (in-list (rest (rest speed-xs)))]
                [x2  (in-list (rest (rest (rest speed-xs))))]
                [y2  (in-list (rest (rest (rest (rest speed-xs)))))]
                [z2  (in-list (rest (rest (rest (rest (rest speed-xs))))))])
            (call/fl3cross x1 y1 z1 x2 y2 z2
              (λ (x y z)
                (unsafe-flvector-set! vbx 0 x)
                (unsafe-flvector-set! vbx 1 y)
                (unsafe-flvector-set! vbx 2 z)))))
    (printf "heap: ~v~n" (current-memory-use)))
  (newline))

(define (test-fl3mag^2)
  (printf "Testing fl3mag^2~n")
  
  (for ([_  (in-range 5)])
    (time (for ([x1  (in-list speed-xs)]
                [y1  (in-list (rest speed-xs))]
                [z1  (in-list (rest (rest speed-xs)))])
            (unsafe-flvector-set! vbx 0 (fl3mag^2 x1 y1 z1))))
    (printf "heap: ~v~n" (current-memory-use)))
  
  (for ([x1  (in-list acc-xs)]
        [y1  (in-list (rest acc-xs))]
        [z1  (in-list (rest (rest acc-xs)))]
        [i  (in-naturals 1)])
    (when (zero? (modulo i 10000))
      (printf "Accuracy iteration i = ~v~n" i))
    (define m (fl3mag^2 x1 y1 z1))
    (define m* (+ (sqr (inexact->exact x1)) (sqr (inexact->exact y1)) (sqr (inexact->exact z1))))
    (define e (flulp-error m m*))
    (when (> e 1.75)
      (eprintf "~v: ~v ~v ~v~n    ~v ~v~n~n" e x1 y1 z1 m (fl m*))))
  
  (newline))

(define (test-fl3mag)
  (printf "Testing fl3mag~n")
  
  (for ([_  (in-range 5)])
    (time (for ([x1  (in-list speed-xs)]
                [y1  (in-list (rest speed-xs))]
                [z1  (in-list (rest (rest speed-xs)))])
            (unsafe-flvector-set! vbx 0 (fl3mag x1 y1 z1))))
    (printf "heap: ~v~n" (current-memory-use)))
  
  (for ([x1  (in-list acc-xs)]
        [y1  (in-list (rest acc-xs))]
        [z1  (in-list (rest (rest acc-xs)))]
        [i  (in-naturals 1)])
    (when (zero? (modulo i 10000))
      (printf "Accuracy iteration i = ~v~n" i))
    (define m (fl3mag x1 y1 z1))
    (define m*^2 (+ (sqr (inexact->exact x1)) (sqr (inexact->exact y1)) (sqr (inexact->exact z1))))
    (define m* (bigfloat->real (bfsqrt (bf m*^2))))
    (define e (flulp-error m m*))
    (when (> e 1.5)
      (eprintf "~v: ~v ~v ~v~n    ~v ~v~n~n" e x1 y1 z1 m (fl m*))))
  
  (newline))

(define (test-call/fl3normalize)
  (printf "Testing call/fl3normalize~n")
  
  (for ([_  (in-range 5)])
    (time (for ([x1  (in-list speed-xs)]
                [y1  (in-list (rest speed-xs))]
                [z1  (in-list (rest (rest speed-xs)))])
            (call/fl3normalize x1 y1 z1
              (λ ([x : Flonum] [y : Flonum] [z : Flonum])
                (unsafe-flvector-set! vbx 0 x)
                (unsafe-flvector-set! vbx 1 y)
                (unsafe-flvector-set! vbx 2 z)))))
    (printf "heap: ~v~n" (current-memory-use)))
  
  (for ([x1  (in-list acc-xs)]
        [y1  (in-list (rest acc-xs))]
        [z1  (in-list (rest (rest acc-xs)))]
        [i  (in-naturals 1)])
    (when (zero? (modulo i 10000))
      (printf "Accuracy iteration i = ~v~n" i))
    (define m*^2 (+ (sqr (inexact->exact x1))
                    (sqr (inexact->exact y1))
                    (sqr (inexact->exact z1))))
    (define m* (bigfloat->real (bfsqrt (bf m*^2))))
    (define x* (if (zero? m*) 0 (/ (inexact->exact x1) m*)))
    (define y* (if (zero? m*) 0 (/ (inexact->exact y1) m*)))
    (define z* (if (zero? m*) 0 (/ (inexact->exact z1) m*)))
    (call/fl3normalize x1 y1 z1
      (λ ([x : Flonum] [y : Flonum] [z : Flonum])
        (define e (fl3ulp-error x y z x* y* z*))
        (when (> e 2.5)
          (eprintf "~v: ~v ~v ~v~n    ~v ~v ~v~n    ~v ~v ~v~n~n"
                   e x1 y1 z1 x y z (fl x*) (fl y*) (fl z*))))))
  
  (newline))

(define (test-call/fl3plane-normalize)
  (printf "Testing call/fl3plane-normalize~n")
  
  (for ([_  (in-range 5)])
    (time (for ([x1  (in-list speed-xs)]
                [y1  (in-list (rest speed-xs))]
                [z1  (in-list (rest (rest speed-xs)))]
                [d1  (in-list (rest (rest (rest speed-xs))))])
            (call/fl3plane-normalize x1 y1 z1 d1
              (λ ([x : Flonum] [y : Flonum] [z : Flonum] [d : Flonum])
                (unsafe-flvector-set! vbx 0 x)
                (unsafe-flvector-set! vbx 1 y)
                (unsafe-flvector-set! vbx 2 z)
                (unsafe-flvector-set! vbx 3 d)))))
    (printf "heap: ~v~n" (current-memory-use)))
  
  (for ([x1  (in-list acc-xs)]
        [y1  (in-list (rest acc-xs))]
        [z1  (in-list (rest (rest acc-xs)))]
        [d1  (in-list (rest (rest (rest acc-xs))))]
        [i  (in-naturals 1)])
    (when (zero? (modulo i 10000))
      (printf "Accuracy iteration i = ~v~n" i))
    (define m*^2 (+ (sqr (inexact->exact x1))
                    (sqr (inexact->exact y1))
                    (sqr (inexact->exact z1))))
    (define m* (bigfloat->real (bfsqrt (bf m*^2))))
    (define x* (if (zero? m*) 0 (/ (inexact->exact x1) m*)))
    (define y* (if (zero? m*) 0 (/ (inexact->exact y1) m*)))
    (define z* (if (zero? m*) 0 (/ (inexact->exact z1) m*)))
    (define d* (if (zero? m*) 0 (/ (inexact->exact d1) m*)))
    (call/fl3plane-normalize x1 y1 z1 d1
      (λ ([x : Flonum] [y : Flonum] [z : Flonum] [d : Flonum])
        (define e (max (fl3ulp-error x y z x* y* z*) (flulp-error d d*)))
        (when (> e 2.5)
          (eprintf "~v: ~v ~v ~v ~v~n    ~v ~v ~v ~v~n    ~v ~v ~v ~v~n~n"
                   e x1 y1 z1 d1 x y z d (fl x*) (fl y*) (fl z*) (fl d*))))))
  
  (newline))

(define (test-fl3plane-point-dist)
  (printf "Testing fl3plane-point-dist~n")
  
  (for ([_  (in-range 5)])
    (time (for ([a  (in-list speed-xs)]
                [b  (in-list (rest speed-xs))]
                [c  (in-list (rest (rest speed-xs)))]
                [d  (in-list (rest (rest (rest speed-xs))))]
                [x  (in-list (rest (rest (rest (rest speed-xs)))))]
                [y  (in-list (rest (rest (rest (rest (rest speed-xs))))))]
                [z  (in-list (rest (rest (rest (rest (rest (rest speed-xs)))))))])
            (unsafe-flvector-set! vbx 0 (fl3plane-point-dist a b c d x y z))))
    (printf "heap: ~v~n" (current-memory-use)))
  
  (for ([a  (in-list acc-xs)]
        [b  (in-list (rest acc-xs))]
        [c  (in-list (rest (rest acc-xs)))]
        [d  (in-list (rest (rest (rest acc-xs))))]
        [x  (in-list (rest (rest (rest (rest acc-xs)))))]
        [y  (in-list (rest (rest (rest (rest (rest acc-xs))))))]
        [z  (in-list (rest (rest (rest (rest (rest (rest acc-xs)))))))]
        [i  (in-naturals 1)])
    (when (zero? (modulo i 10000))
      (printf "Accuracy iteration i = ~v~n" i))
    (call/fl3plane-normalize a b c d
      (λ ([a : Flonum] [b : Flonum] [c : Flonum] [d : Flonum])
        (when (fl3plane? a b c d)
          (define dist (fl3plane-point-dist a b c d x y z))
          (define dist* (+ (* (inexact->exact a) (inexact->exact x))
                           (* (inexact->exact b) (inexact->exact y))
                           (* (inexact->exact c) (inexact->exact z))
                           (inexact->exact d)))
          (define e (flulp-error dist dist*))
          (when (> e 3.5)
            (eprintf "~v: ~v ~v ~v ~v ~v ~v ~v~n    ~v    ~v~n~n"
                     e a b c d x y z dist (fl dist*)))))))
  
  (newline))

;; ===================================================================================================
;; fl4.rkt

(define (test-fl4dot)
  (printf "Testing fl4dot~n")
  
  (for ([_  (in-range 5)])
    (time (for ([x1  (in-list speed-xs)]
                [y1  (in-list (rest speed-xs))]
                [z1  (in-list (rest (rest speed-xs)))]
                [w1  (in-list (rest (rest (rest speed-xs))))]
                [x2  (in-list (rest (rest (rest (rest speed-xs)))))]
                [y2  (in-list (rest (rest (rest (rest (rest speed-xs))))))]
                [z2  (in-list (rest (rest (rest (rest (rest (rest speed-xs)))))))]
                [w2  (in-list (rest (rest (rest (rest (rest (rest (rest speed-xs))))))))])
            (unsafe-flvector-set! vbx 0 (fl4dot x1 y1 z1 w1 x2 y2 z2 w2))))
    (printf "heap: ~v~n" (current-memory-use)))
  
  (for ([x1  (in-list acc-xs)]
        [y1  (in-list (rest acc-xs))]
        [z1  (in-list (rest (rest acc-xs)))]
        [w1  (in-list (rest (rest (rest acc-xs))))]
        [x2  (in-list (rest (rest (rest (rest acc-xs)))))]
        [y2  (in-list (rest (rest (rest (rest (rest acc-xs))))))]
        [z2  (in-list (rest (rest (rest (rest (rest (rest acc-xs)))))))]
        [w2  (in-list (rest (rest (rest (rest (rest (rest (rest acc-xs))))))))]
        [i  (in-naturals 1)])
    (when (zero? (modulo i 10000))
      (printf "Accuracy iteration i = ~v~n" i))
    (define m (fl4dot x1 y1 z1 w1 x2 y2 z2 w2))
    (define m* (+ (* (inexact->exact x1) (inexact->exact x2))
                  (* (inexact->exact y1) (inexact->exact y2))
                  (* (inexact->exact z1) (inexact->exact z2))
                  (* (inexact->exact w1) (inexact->exact w2))))
    (define e (flulp-error m m*))
    (when (> e 3.5)
      (eprintf "~v: ~v ~v ~v ~v ~v ~v ~v ~v~n    ~v ~v~n~n" e x1 y1 z1 w1 x2 y2 z2 w2 m (fl m*))))
  
  (newline))

;; ===================================================================================================
;; FlV3 tests

(define (test-flv3+)
  (printf "Testing flv3+~n")
  (for ([_  (in-range 5)])
    (time (for ([v1  (in-list speed-vs)]
                [v2  (in-list (rest speed-vs))])
            (set! bx (flv3+ v1 v2)))))
  (newline))

(define (test-flv3-)
  (printf "Testing flv3-~n")
  (for ([_  (in-range 5)])
    (time (for ([v1  (in-list speed-vs)]
                [v2  (in-list (rest speed-vs))])
            (set! bx (flv3- v1 v2)))))
  (newline))

(define (test-flv3neg)
  (printf "Testing flv3neg~n")
  (for ([_  (in-range 5)])
    (time (for ([v1  (in-list speed-vs)])
            (set! bx (flv3neg v1)))))
  (newline))

(define (test-flv3*)
  (printf "Testing flv3*~n")
  (for ([_  (in-range 5)])
    (time (for ([v1  (in-list speed-vs)]
                [x1  (in-list speed-xs)])
            (set! bx (flv3* v1 x1)))))
  (newline))

(define (test-flv3/)
  (printf "Testing flv3/~n")
  (for ([_  (in-range 5)])
    (time (for ([v1  (in-list speed-vs)]
                [x1  (in-list speed-xs)])
            (set! bx (flv3/ v1 x1)))))
  (newline))

(define (test-flv3fma)
  (printf "Testing flv3fma~n")
  (for ([_  (in-range 5)])
    (time (for ([v1  (in-list speed-vs)]
                [v2  (in-list (rest speed-vs))]
                [x1  (in-list speed-xs)])
            (set! bx (flv3fma v1 x1 v2)))))
  (newline))

(define (test-flv3dot)
  (printf "Testing flv3dot~n")
  (for ([_  (in-range 5)])
    (time (for ([v1  (in-list speed-vs)]
                [v2  (in-list (rest speed-vs))])
            (unsafe-flvector-set! vbx 0 (flv3dot v1 v2)))))
  (newline))

(define (test-flv3cross)
  (printf "Testing flv3cross~n")
  (for ([_  (in-range 5)])
    (time (for ([v1  (in-list speed-vs)]
                [v2  (in-list (rest speed-vs))])
            (set! bx (flv3cross v1 v2)))))
  (newline))

(define (test-flv3mag^2)
  (printf "Testing flv3mag^2~n")
  (for ([_  (in-range 5)])
    (time (for ([v1  (in-list speed-vs)])
            (unsafe-flvector-set! vbx 0 (flv3mag^2 v1)))))
  (newline))

(define (test-flv3mag)
  (printf "Testing flv3mag~n")
  (for ([_  (in-range 5)])
    (time (for ([v1  (in-list speed-vs)])
            (unsafe-flvector-set! vbx 0 (flv3mag v1)))))
  (newline))

(define (test-flv3normalize)
  (printf "Testing flv3normalize~n")
  (for ([_  (in-range 5)])
    (time (for ([v1  (in-list speed-vs)])
            (set! bx (flv3normalize v1)))))
  (newline))

(define (test-flv3dist^2)
  (printf "Testing flv3dist^2~n")
  (for ([_  (in-range 5)])
    (time (for ([v1  (in-list speed-vs)]
                [v2  (in-list (rest speed-vs))])
            (unsafe-flvector-set! vbx 0 (flv3dist^2 v1 v2)))))
  (newline))

(define (test-flv3dist)
  (printf "Testing flv3dist~n")
  (for ([_  (in-range 5)])
    (time (for ([v1  (in-list speed-vs)]
                [v2  (in-list (rest speed-vs))])
            (unsafe-flvector-set! vbx 0 (flv3dist v1 v2)))))
  (newline))

;; ===================================================================================================
;; FlPlane3 tests

(define (test-flplane3)
  (printf "Testing flplane3~n")
  (for ([_  (in-range 5)])
    (time (for ([v1  (in-list speed-vs)]
                [x1  (in-list speed-xs)])
            (set! bx (flplane3 v1 x1)))))
  (newline))

(define (test-flplane3-flip)
  (printf "Testing flplane3-flip~n")
  (for ([_  (in-range 5)])
    (time (for ([p  (in-list speed-ps)])
            (set! bx (flplane3-flip p)))))
  (newline))

(define (test-flplane3-point-dist)
  (printf "Testing flplane3-point-dist~n")
  (for ([_  (in-range 5)])
    (time (for ([p  (in-list speed-ps)]
                [v  (in-list speed-vs)])
            (define d (flplane3-point-dist p v))
            (when d (unsafe-flvector-set! vbx 0 d)))))
  (newline))

;; ===================================================================================================
;; FlTransform3 tests

(define (test-flt3inverse)
  (printf "Testing flt3inverse~n")
  (for ([_  (in-range 5)])
    (time (for ([t1  (in-list speed-ts)])
            (set! bx (flt3inverse t1)))))
  (newline))

(define (test-flt3compose)
  (printf "Testing flt3compose~n")
  (for ([_  (in-range 5)])
    (time (for ([t1  (in-list speed-ts)]
                [t2  (in-list (rest speed-ts))])
            (set! bx (flt3compose t1 t2)))))
  (newline))

;; ===================================================================================================

(begin
  (test-fast-flfma)
  )

(begin
  (test-fl2dot)
  )

(begin
  (test-call/fl3fma)
  (test-fl3dot)
  (test-call/fl3cross)
  (test-fl3mag^2)
  (test-fl3mag)
  (test-call/fl3normalize)
  (test-call/fl3plane-normalize)
  (test-fl3plane-point-dist)
  )

(begin
  (test-fl4dot)
  )

(begin
  (test-flv3+)
  (test-flv3-)
  (test-flv3neg)
  (test-flv3*)
  (test-flv3/)
  (test-flv3fma)
  (test-flv3dot)
  (test-flv3cross)
  (test-flv3mag^2)
  (test-flv3mag)
  (test-flv3normalize)
  (test-flv3dist^2)
  (test-flv3dist)
  )

(begin
  (test-flplane3)
  (test-flplane3-flip)
  (test-flplane3-point-dist)
  )

(begin
  (test-flt3inverse)
  (test-flt3compose)
  )
