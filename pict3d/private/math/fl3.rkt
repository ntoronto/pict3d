#lang typed/racket/base

(require math/flonum
         "fl.rkt"
         "fl2.rkt")

(provide call/fl3fma
         fl3dot
         call/fl3cross
         fl3mag^2
         fl3mag
         call/fl3normalize
         fl3plane?
         call/fl3plane-normalize
         fl3plane-point-dist)

;(: call/fl3fma
;   (All (A) (-> Flonum Flonum Flonum Flonum Flonum Flonum Flonum (-> Flonum Flonum Flonum A) A)))
(define-syntax-rule (call/fl3fma x1 y1 z1 s x2 y2 z2 k)
  (k (fast-flfma x1 s x2)
     (fast-flfma y1 s y2)
     (fast-flfma z1 s z2)))

;(: fl3fma (-> Flonum Flonum Flonum Flonum Flonum Flonum Flonum (Values Flonum Flonum Flonum)))
;; This one creates values objects on the heap - use call/fl3fma instead for now
(define-syntax-rule (fl3fma x1 y1 z1 s x2 y2 z2)
  (call/fl3fma x1 y1 z1 s x2 y2 z2 values))

;(: fl3dot (-> Flonum Flonum Flonum Flonum Flonum Flonum Flonum))
;; Observed error bounded by 3.75 ulp
(define-syntax-rule (fl3dot x1-stx y1-stx z1-stx x2-stx y2-stx z2-stx)
  (let ([x1 : Flonum  x1-stx]
        [y1 : Flonum  y1-stx]
        [z1 : Flonum  z1-stx]
        [x2 : Flonum  x2-stx]
        [y2 : Flonum  y2-stx]
        [z2 : Flonum  z2-stx])
    (let* ([x  (* x1 x2)]
           [y  (* y1 y2)]
           [w  (+ x y)]
           [z  (* z1 z2)])
      (if (or (< -2.0 (/ x y) -0.5)
              (< -2.0 (/ w z) -0.5))
          (let*-values ([(x.hi x.lo)  (fast-fl*/error x1 x2)]
                        [(y.hi y.lo)  (fast-fl*/error y1 y2)]
                        [(z.hi z.lo)  (fast-fl*/error z1 z2)]
                        [(e.hi e.lo)  (fast-fl+/error x.hi y.hi)]
                        [(w.hi w.lo)  (fast-mono-fl+/error e.hi (+ e.lo y.lo x.lo))])
            (+ w.hi z.hi w.lo z.lo))
          (+ w z)))))

;(: call/fl3cross
;   (All (A) (-> Flonum Flonum Flonum Flonum Flonum Flonum (-> Flonum Flonum Flonum A) A)))
(define-syntax-rule (call/fl3cross x1 y1 z1 x2-stx y2-stx z2-stx k)
  ;; fl2cross passes x1 y1 z1 on directly, so we don't need to name them
  (let ([x2 : Flonum  x2-stx]
        [y2 : Flonum  y2-stx]
        [z2 : Flonum  z2-stx])
    (k (fl2cross y1 z1 y2 z2)
       (fl2cross z1 x1 z2 x2)
       (fl2cross x1 y1 x2 y2))))

;(: fl3cross (-> Flonum Flonum Flonum Flonum Flonum Flonum (Values Flonum Flonum Flonum)))
;; This one creates values objects on the heap - use call/fl3cross instead for now
(define-syntax-rule (fl3cross x1 y1 z1 x2 y2 z2)
  (call/fl3cross x1 y1 z1 x2 y2 z2 values))

;(: fl3mag^2 (-> Flonum Flonum Flonum Nonnegative-Flonum))
;; Observed error bounded by 1.75 ulp
(define-syntax-rule (fl3mag^2 x-stx y-stx z-stx)
  (let ([x : Flonum  x-stx]
        [y : Flonum  y-stx]
        [z : Flonum  z-stx])
    (max 0.0 (+ (* x x) (* y y) (* z z)))))

;(: fl3mag (-> Flonum Flonum Flonum Nonnegative-Flonum))
;; Observed error bounded by 1.5 ulp
(define-syntax-rule (fl3mag x-stx y-stx z-stx)
  (let ([x : Flonum  x-stx]
        [y : Flonum  y-stx]
        [z : Flonum  z-stx])
    (let ([m  (flsqrt (fl3mag^2 x y z))])
      (if (<= m 2^-512)
          (* 2^-564 (flsqrt (fl3mag^2 (* 2^564 x) (* 2^564 y) (* 2^564 z))))
          m))))

;(: call/fl3normalize (All (A) (-> Flonum Flonum Flonum (-> Flonum Flonum Flonum A) A)))
;; Observed error bounded by 2.5 ulp
(define-syntax-rule (call/fl3normalize x-stx y-stx z-stx k-stx)
  (let ([x : Flonum  x-stx]
        [y : Flonum  y-stx]
        [z : Flonum  z-stx]
        [k k-stx])
    (let ([mx  (max (abs x) (abs y) (abs z))])
      (if (< mx 2^-512)
          (if (= mx 0.0)
              (k 0.0 0.0 0.0)
              (let ([x  (* 2^564 x)]
                    [y  (* 2^564 y)]
                    [z  (* 2^564 z)])
                (let ([m  (flsqrt (+ (* x x) (* y y) (* z z)))])
                  (k (/ x m) (/ y m) (/ z m)))))
          (let ([m  (flsqrt (+ (* x x) (* y y) (* z z)))])
            (k (/ x m) (/ y m) (/ z m)))))))

(: fl3plane? (-> Flonum Flonum Flonum Flonum Boolean))
(define (fl3plane? a b c d)
  (and (< 0.0 (max (abs (ann a Flonum)) (abs (ann b Flonum)) (abs (ann c Flonum))))
       (< -inf.0 (ann d Flonum) +inf.0)))

;(: call/fl3plane-normalize
;   (All (A) (-> Flonum Flonum Flonum Flonum (-> Flonum Flonum Flonum Flonum A) A)))
;; Observed error bounded by 2.5 ulp
(define-syntax-rule (call/fl3plane-normalize x-stx y-stx z-stx d-stx k-stx)
  (let ([x : Flonum  x-stx]
        [y : Flonum  y-stx]
        [z : Flonum  z-stx]
        [d : Flonum  d-stx]
        [k k-stx])
    (let ([mx  (max (abs x) (abs y) (abs z))])
      (if (< mx 2^-512)
          (if (= mx 0.0)
              (k 0.0 0.0 0.0 0.0)
              (let ([x  (* 2^564 x)]
                    [y  (* 2^564 y)]
                    [z  (* 2^564 z)])
                (let ([m  (flsqrt (+ (* x x) (* y y) (* z z)))])
                  (k (/ x m) (/ y m) (/ z m) (/ (* 2^256 d) (* 2^-308 m))))))
          (let ([m  (flsqrt (+ (* x x) (* y y) (* z z)))])
            (k (/ x m) (/ y m) (/ z m) (/ d m)))))))

;(: fl3plane-point-dist (-> Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum))
;; Observed error bounded by 3.5 ulp
(define-syntax-rule (fl3plane-point-dist x1-stx y1-stx z1-stx d1-stx x2-stx y2-stx z2-stx)
  (let ([x1 : Flonum  x1-stx]
        [y1 : Flonum  y1-stx]
        [z1 : Flonum  z1-stx]
        [d1 : Flonum  d1-stx]
        [x2 : Flonum  x2-stx]
        [y2 : Flonum  y2-stx]
        [z2 : Flonum  z2-stx])
    (let* ([x  (* x1 x2)]
           [y  (* y1 y2)]
           [w  (+ x y)]
           [z  (* z1 z2)]
           [v  (+ w z)])
      (if (or (< -2.0 (/ x y) -0.5)
              (< -2.0 (/ w z) -0.5)
              (< -2.0 (/ v d1) -0.5))
          (let*-values ([(x.hi x.lo)  (fast-fl*/error x1 x2)]
                        [(y.hi y.lo)  (fast-fl*/error y1 y2)]
                        [(z.hi z.lo)  (fast-fl*/error z1 z2)]
                        [(e.hi e.lo)  (fast-fl+/error x.hi y.hi)]
                        [(f.hi f.lo)  (fast-fl+/error z.hi d1)]
                        [(w.hi w.lo)  (fast-mono-fl+/error e.hi (+ e.lo y.lo x.lo))]
                        [(v.hi v.lo)  (fast-mono-fl+/error f.hi (+ f.lo z.lo))])
            (+ v.hi w.hi v.lo w.lo))
          (+ v d1)))))
