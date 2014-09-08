#lang typed/racket/base

(require math/flonum)

(provide fast-fl2+1
         fast-fl2/1
         fast-fl1/2
         fast-fl2+
         fast-fl2-
         fast-fl2*
         fast-fl2sqr
         fast-fl2/
         fast-fl2sqrt
         fast-flsqrt/error
         fast-flsqrt+)

(: fast-fl2+1 (-> Flonum Flonum Flonum (Values Flonum Flonum)))
(define (fast-fl2+1 e2 e1 b)
  (let*-values ([(q1 h1)  (fast-fl+/error e1 b)]
                [(q2 h2)  (fast-fl+/error e2 q1)])
    (values q2 (+ h1 h2))))

(: fast-fl2/1 (-> Flonum Flonum Flonum (Values Flonum Flonum)))
(define (fast-fl2/1 x-hi x-lo n)
  (let-values ([(x-hi-hi x-hi-lo)  (fast-fl//error x-hi n)])
    (fast-fl2+1 x-hi-hi x-hi-lo (/ x-lo n))))

(: fast-fl1/2 (-> Flonum Flonum Flonum (Values Flonum Flonum)))
(define (fast-fl1/2 x2 y2 y1)
  (let*-values ([(c2)  (fl/ x2 y2)]
                [(u2 u1)  (fast-fl*/error c2 y2)]
                [(c1)  (fl/ (fl- (fl- (fl- x2 u2) u1) (fl* c2 y1)) y2)]
                [(z2)  (fl+ c2 c1)])
    (values z2 (fl+ (fl- c2 z2) c1))))

;(: fast-fl2+ (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum)))
(define-syntax-rule (fast-fl2+ x2 x1 y2 y1)
  (let*-values ([(q1 e1)  (fast-mono-fl+/error y2 y1)]
                [(e3 e2)  (fast-fl+/error x2 q1)]
                [(q1 e1)  (fast-fl+/error e1 x1)]
                [(q2 e2)  (fast-mono-fl+/error e2 q1)]
                [(e4 e3)  (fast-mono-fl+/error e3 q2)])
    (values e4 (+ e1 e2 e3))))

;(: fast-fl2- (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum)))
(define-syntax-rule (fast-fl2- x2 x1 y2 y1)
  (fast-fl2+ x2 x1 (- y2) (- y1)))

;(: fast-fl2* (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum)))
(define-syntax-rule (fast-fl2* x2-stx x1-stx y2-stx y1-stx)
  (let ([x2 : Flonum  x2-stx]
        [x1 : Flonum  x1-stx]
        [y2 : Flonum  y2-stx]
        [y1 : Flonum  y1-stx])
    (let* ([up  (fl* x2 (fl+ 1.0 (flexpt 2.0 27.0)))]
           [vp  (fl* y2 (fl+ 1.0 (flexpt 2.0 27.0)))]
           [u1  (fl+ (fl- x2 up) up)]
           [v1  (fl+ (fl- y2 vp) vp)]
           [u2  (fl- x2 u1)]
           [v2  (fl- y2 v1)]
           [m2  (fl* x2 y2)]
           [m1  (fl+ (fl+ (fl+ (fl+ (fl+ (fl- (fl* u1 v1) m2)
                                         (fl* u1 v2))
                                    (fl* u2 v1))
                               (fl* u2 v2))
                          (fl* x2 y1))
                     (fl* x1 y2))]
           [z2  (fl+ m2 m1)])
      (values z2 (fl+ (fl- m2 z2) m1)))))

;(: fast-fl2sqr (Flonum Flonum -> (Values Flonum Flonum)))
(define-syntax-rule (fast-fl2sqr x2-stx x1-stx)
  (let* ([x2 : Flonum  x2-stx]
         [x1 : Flonum  x1-stx]
         [up  (fl* x2 (fl+ 1.0 (flexpt 2.0 27.0)))]
         [u1  (fl+ (fl- x2 up) up)]
         [u2  (fl- x2 u1)]
         [m2  (fl* x2 x2)]
         [m1  (fl+ (fl+ (fl+ (fl- (fl* u1 u1) m2)
                             (fl* 2.0 (fl* u1 u2)))
                        (fl* u2 u2))
                   (fl* 2.0 (fl* x2 x1)))]
         [z2  (fl+ m2 m1)])
    (values z2 (fl+ (fl- m2 z2) m1))))

;(: fast-fl2/ (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum)))
(define-syntax-rule (fast-fl2/ x2-stx x1-stx y2-stx y1-stx)
  (let ([x2 : Flonum  x2-stx]
        [x1 : Flonum  x1-stx]
        [y2 : Flonum  y2-stx]
        [y1 : Flonum  y1-stx])
    (let*-values ([(c2)  (fl/ x2 y2)]
                  [(u2 u1)  (fast-fl*/error c2 y2)]
                  [(c1)  (fl/ (fl- (fl+ (fl- (fl- x2 u2) u1) x1) (fl* c2 y1)) y2)]
                  [(z2)  (fl+ c2 c1)])
      (values z2 (fl+ (fl- c2 z2) c1)))))

;(: fast-fl2sqrt (-> Flonum Flonum (Values Flonum Flonum)))
(define-syntax-rule (fast-fl2sqrt x2-stx x1-stx)
  (let ([x2 : Flonum  x2-stx]
        [x1 : Flonum  x1-stx])
    (define y (flsqrt (fl+ x2 x1)))
    (cond [(= y 0.0)  (values 0.0 0.0)]
          [else  (define-values (z2 z1) (fast-flsqr/error y))
                 (fast-fl+/error (fl* 0.5 (fl/ (+ (fl- x2 z2) (fl- x1 z1)) y)) y)])))

;(: fast-flsqrt/error (-> Flonum (Values Flonum Flonum)))
(define-syntax-rule (fast-flsqrt/error x2-stx)
  (let ([x2 : Flonum  x2-stx])
    (define y (flsqrt x2))
    (cond [(= y 0.0)  (values 0.0 0.0)]
          [else  (define-values (z2 z1) (fast-flsqr/error y))
                 (fast-fl+/error (fl* 0.5 (fl/ (- (fl- x2 z2) z1) y)) y)])))

(: fast-flsqrt+ (-> Flonum Flonum Flonum))
(define (fast-flsqrt+ x2 x1)
  (define y (flsqrt (fl+ x2 x1)))
  (cond [(= y 0.0)  0.0]
        [else  (define-values (z2 z1) (fast-flsqr/error y))
               (+ (fl* 0.5 (fl/ (+ (fl- x2 z2) (fl- x1 z1)) y)) y)]))
