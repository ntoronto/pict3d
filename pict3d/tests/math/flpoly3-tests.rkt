#lang typed/racket/base
#|
(require racket/list
         racket/match
         math/flonum
         math/base
         pict3d/private/shape/flpoly3
         pict3d/private/shape/tessellate
         pict3d/private/math/flv3)

(require (except-in plot/typed lines points)
         math/distributions)

(require/typed
 profile
 [profile-thunk  (All (A) (-> (-> A) A))])


(define p
  (flpoly3
   #f
   (vector
    (flvector 0.6226371106156744 -0.3325651917439213 0.6226371106158333)
    (flvector 0.4682256433133432 0.7325108141280566 0.4682256433139569)
    (flvector 0.13023162419625772 0.2365319062714402 0.1302316241964394)
    (flvector 0.37617367130887575 0.825675151018612 0.3761736713090313)
    (flvector 0.1390566972722074 0.3532516272606099 0.13905669727269548)
    (flvector -0.042603262368990494 0.2998817597344788 -0.04260326236863064)
    (flvector -0.08364856569538565 0.32940443593363167 -0.08364856569520697))
   #(0.0 0.0 0.0 0.0 0.0 0.0 0.0)))


(define angle-dist (normal-dist 0 1))

(: random-polygon (-> FlPoly3))
(define (random-polygon)
  (define n (random-integer 4 16))
  (define θs
    (let* ([dθs  (build-flvector (+ n 1) (λ (_) (abs (sample angle-dist))))]
           [θs   (flvector->list (flvector-sums dθs))])
      (define θmax (last θs))
      (drop-right (map (λ ([θ : Flonum]) (* 2.0 pi (/ θ θmax))) θs) 1)))
  (define vs
    (for/vector ([θ  (in-list θs)]) : FlVector
      (define r (+ 0.25 (* 0.75 (random))))
      (define x (* r (flcos θ)))
      (define y (* r (flsin θ)))
      (flv3* (flvector x y (+ x (* 16.0 (- 1.0 flpoly3-planarity-cutoff) (random)))) 1.0)))
  (flpoly3 #f vs (make-vector n 0.0)))

(: flpoly3-renderers (-> FlPoly3 (Listof renderer3d)))
(define (flpoly3-renderers p)
  (match-define (flpoly3 data vs cs plane) p)
  (define centroid (flpoly3-centroid p))
  (define normal (flpoly3-normal p))
  (cons (polygon3d (map flvector->vector (vector->list (flpoly3-vertices p)))
                   #:line-width 2)
        (if normal
            (list (lines3d (list (flvector->vector centroid)
                                 (flvector->vector (flv3+ centroid normal)))
                           #:alpha 0.6))
            empty)))

(define ps
  #;(list p)
  (time (build-list 50 (λ (_) (random-polygon)))))

(parameterize ([plot-x-ticks  no-ticks]
               [plot-y-ticks  no-ticks]
               [plot-z-ticks  no-ticks]
               [plot-x-label  #f]
               [plot-y-label  #f]
               [plot-width   300]
               [plot-height  300])
  (for ([i  (in-naturals)]
        [p  (in-list ps)])
    (printf "i = ~v~n" i)
    (displayln
     (list
      (plot3d (list (flpoly3-renderers p)
                    (for/list : (Listof renderer3d) ([v  (in-vector (flpoly3-vertices p))]
                                                     [i  (in-naturals)])
                      (define-values (x y z) (flv3-values v))
                      (point-label3d (list x y z) (number->string i))))
              #:x-min -1 #:x-max 1
              #:y-min -1 #:y-max 1
              #:z-min -1 #:z-max 1)
      (plot3d (for/list : (Listof (Listof renderer3d)) ([p  (in-list (flpoly3-tessellate p))])
                (flpoly3-renderers p))
              #:x-min -1 #:x-max 1
              #:y-min -1 #:y-max 1
              #:z-min -1 #:z-max 1)))))

(define: bx : Any  #f)

(profile-thunk
 (λ ()
   (for ([_  (in-range 5)])
     (define ps (build-list 10000 (λ (_) (random-polygon))))
     (for ([p  (in-list ps)])
       (set! bx (flpoly3-tessellate p))))))
(newline)

(for ([_  (in-range 5)])
  (printf "Build:~n")
  (define ps (time (build-list 10000 (λ (_) (random-polygon)))))
  (printf "Tessellate:~n")
  (time (for ([p  (in-list ps)])
          (set! bx (flpoly3-tessellate p)))))
(newline)
|#
