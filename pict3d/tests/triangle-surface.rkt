#lang racket

(require racket/gui
         math/flonum
         pict3d
         pict3d/private/math/flv3
         profile
         )

(current-pict3d-width 512)
(current-pict3d-height 384)
(current-color (rgba "azure"))
(current-material (material #:ambient 0.01 #:diffuse 0.29 #:specular 0.7 #:roughness 0.2))

;(: xyz-fun (-> Flonum Flonum FlVector))
(define (xyz-fun x y)
  (let ([x  (+ x (* 0.1 (sin (* (+ x y) 10))))]
        [y  (+ y (* 0.1 (cos (* (+ x y) 10))))])
    (pos x y (* 1.0 (sin x) (cos y)))))

;(: norm-fun (-> Flonum Flonum FlVector))
(define (norm-fun x y)
  (let ([x  (+ x (* 0.1 (sin (* (+ x y) 10))))]
        [y  (+ y (* 0.1 (cos (* (+ x y) 10))))])
    (dir (* -1.0 (cos x) (cos y))
         (* +1.0 (sin x) (sin y))
         1.0)))

(define grid-size 64)

(define-values (vsss nsss)
  (time
   (for*/lists (vss nss) ([xi  (in-range 0 grid-size)]
                          [yi  (in-range 0 grid-size)])
     (define x0 (* 0.5 (- xi (* 0.5 grid-size))))
     (define y0 (* 0.5 (- yi (* 0.5 grid-size))))
     (define x1 (+ x0 0.5))
     (define y1 (+ y0 0.5))
     (define v0 (xyz-fun x0 y0))
     (define v1 (xyz-fun x1 y0))
     (define v2 (xyz-fun x1 y1))
     (define v3 (xyz-fun x0 y1))
     (define n0 (norm-fun x0 y0))
     (define n1 (norm-fun x1 y0))
     (define n2 (norm-fun x1 y1))
     (define n3 (norm-fun x0 y1))
     ;; Split on shortest diagonal
     (if (< (pos-dist v0 v2)
            (pos-dist v1 v3))
         (values (list (list v0 v1 v2)
                       (list v0 v2 v3))
                 (list (list n0 n1 n2)
                       (list n0 n2 n3)))
         (values (list (list v0 v1 v3)
                       (list v1 v2 v3))
                 (list (list n0 n1 n3)
                       (list n1 n2 n3)))))))

(define vss (append* vsss))
(define nss (append* nsss))

(define ts
  (time
   (append
    (for/list ([vs  (in-list vss)]
               [ns  (in-list nss)])
      (match-define (list v1 v2 v3) vs)
      (match-define (list n1 n2 n3) ns)
      (triangle (vertex v1 #:normal n1)
                (vertex v2 #:normal n2)
                (vertex v3 #:normal n3))))))

(define lights
  (time
   (append
    
    (list (light (pos 0 0 2) (emitted 1.0 1.0 0.95 2)))
    
    (for*/list ([xi  (in-range 0 grid-size 8)]
                [yi  (in-range 0 grid-size 8)])
      (define x0 (* 0.5 (- xi (* 0.5 grid-size))))
      (define y0 (* 0.5 (- yi (* 0.5 grid-size))))
      (light (pos x0 y0 2) (emitted 1 1 0.95 2))))))

(define rects
  (time
   (for*/list ([xi  (in-range 1 grid-size 2)]
               [yi  (in-range 1 grid-size 2)])
     (define x0 (* 0.5 (- xi (* 0.5 grid-size))))
     (define y0 (* 0.5 (- yi (* 0.5 grid-size))))
     (define x1 (+ x0 0.5))
     (define y1 (+ y0 0.5))
     (define z (pos-z (xyz-fun (* 0.5 (+ x0 x1)) (* 0.5 (+ y0 y1)))))
     (define transparent? (= 0 (modulo (+ xi yi) 4)))
     (with-color (if transparent? (rgba 0.2 0.3 1.0 0.5) (rgba 0.2 1.0 0.3 1.0))
       (with-material (if transparent?
                          (material #:ambient 0.1 #:diffuse 0.2 #:specular 0.7 #:roughness 0.1)
                          default-material)
         (rectangle (pos x0 y0 (* 0.5 z))
                    (pos x1 y1 (+ (* 0.5 z) 1.0))))))))

(define pict
  (combine lights ts rects
           (basis 'camera (point-at (pos 10 10 10) origin))))
#;
(profile
 (for ([_  (in-range 100)])
   (pict3d->bitmap pict 32 32)))

(freeze pict)
