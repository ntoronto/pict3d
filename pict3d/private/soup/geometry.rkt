#lang typed/racket/base

(require math/base
         math/flonum
         "../math.rkt"
         "types.rkt")

(provide (all-defined-out))
#|
(: flv3vertex-area (-> FlV3 FlV3 FlV3 Flonum))
;; Computes the Voronoi area around v2 if the triangle is acute
;; If the triangle is obtuse, returns a fixed fraction of the triangle area
(define (flv3vertex-area v1 v2 v3)
  (define a3 (acos (flv3corner-cos v2 v3 v1)))
  (define a1 (acos (flv3corner-cos v3 v1 v2)))
  (define a2 (- pi a3 a1))
  (cond [(and (<= (max a1 a2 a3) (* pi 0.5)))
         ;; Voronoi area
         (* 0.125 (+ (/ (flv3mag^2 (flv3- v1 v2)) (fltan a3))
                     (/ (flv3mag^2 (flv3- v3 v2)) (fltan a1))))]
        [else
         (define s (if (> a2 (* pi 0.5)) 0.5 0.25))
         (* s (flv3polygon-area v1 v2 v3))]))

(: pseudo-voronoi-area (All (C A) (-> (vertex C) (Listof (face A)) (-> FlV3 FlV3) Flonum)))
(define (pseudo-voronoi-area v faces t)
  (let ([v  (t (vertex-flv3 v))])
    (for/fold ([a : Flonum  0.0]) ([f  (in-list faces)])
      (define-values (v1 v2 v3) (face-flv3s f))
      (let ([v1  (t v1)] [v2  (t v2)] [v3  (t v3)])
        (define area
          (cond [(equal? v1 v)  (flv3vertex-area v3 v1 v2)]
                [(equal? v2 v)  (flv3vertex-area v1 v2 v3)]
                [(equal? v3 v)  (flv3vertex-area v2 v3 v1)]
                [else  (error 'bad)]))
        (+ a area)))))

(: adjacent-angle-sum (All (C A) (-> (vertex C) (Listof (face A)) (-> FlV3 FlV3) Flonum)))
(define (adjacent-angle-sum v faces t)
  (let ([v  (t (vertex-flv3 v))])
    (for/fold ([a : Flonum  0.0]) ([f  (in-list faces)])
      (define-values (v1 v2 v3) (face-flv3s f))
      (let ([v1  (t v1)] [v2  (t v2)] [v3  (t v3)])
        (define angle
          (cond [(equal? v1 v)  (acos (flv3corner-cos v3 v1 v2))]
                [(equal? v2 v)  (acos (flv3corner-cos v1 v2 v3))]
                [(equal? v3 v)  (acos (flv3corner-cos v2 v3 v1))]
                [else  (error 'bad)]))
        (+ a angle)))))

(: gaussian-curvature (All (C A) (-> (vertex C) (Listof (face A)) (-> FlV3 FlV3) Flonum)))
(define (gaussian-curvature v faces t)
  (define angle (adjacent-angle-sum v faces t))
  (define area (pseudo-voronoi-area v faces t))
  (/ (- (* 2.0 pi) angle) area))

(: mean-curvature-dir (All (C A) (-> (vertex C) (Listof (face A)) (-> FlV3 FlV3) FlV3)))
(define (mean-curvature-dir v faces t)
  (let ([v  (t (vertex-flv3 v))])
    (for/fold ([n : FlV3  zero-flv3]) ([f  (in-list faces)])
      (define-values (v1 v2 v3) (face-flv3s f))
      (let ([v1  (t v1)] [v2  (t v2)] [v3  (t v3)])
        (let-values ([(v1 v2 v3)  (cond [(equal? v1 v)  (values v1 v2 v3)]
                                        [(equal? v2 v)  (values v2 v3 v1)]
                                        [(equal? v3 v)  (values v3 v1 v2)]
                                        [else  (error 'bad)])])
          (flv3+ n (flv3* (flv3+ (flv3- v2 v1) (flv3- v3 v1))
                          (+ (/ (fltan (flacos (flv3corner-cos v1 v2 v3))))
                             (/ (fltan (flacos (flv3corner-cos v2 v3 v1))))))))))))

(: mean-curvature-perp (All (C A) (-> (vertex C) (Listof (face A)) (-> FlV3 FlV3) FlV3)))
(define (mean-curvature-perp v faces t)
  (define n (mean-curvature-dir v faces t))
  (define area (pseudo-voronoi-area v faces t))
  (flv3* n (/ 0.25 area)))
|#
