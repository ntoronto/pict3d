#lang typed/racket

(require typed/racket/gui
         typed/racket/class
         pict3d)

(require math/flonum
         math/base
         pict3d/private/math/flv3)

(: 3d-polar->cartesian (-> Flonum Flonum Flonum FlVector))
(define (3d-polar->cartesian θ ρ r)
  (let ([cos-ρ  (cos ρ)])
    (flvector (* r (* (cos θ) cos-ρ))
              (* r (* (sin θ) cos-ρ))
              (* r (sin ρ)))))

(: cartesian->3d-polar (-> FlVector (Values Flonum Flonum Flonum)))
(define (cartesian->3d-polar v)
  (define-values (x y z) (flv3-values v))
  (define r (flsqrt (+ (sqr x) (sqr y) (sqr z))))
  (values (atan y x) (asin (/ z r)) r))

(: retesselate (-> (-> FlVector FlVector)
                   (-> (List FlVector FlVector FlVector)
                       (Listof (List FlVector FlVector FlVector)))))
(define ((retesselate f) vs)
  (match-define (list v0 v1 v2) vs)
  (define v01 (f (flv3* (flv3+ v0 v1) 0.5)))
  (define v12 (f (flv3* (flv3+ v1 v2) 0.5)))
  (define v20 (f (flv3* (flv3+ v2 v0) 0.5)))
  (list (list v0 v01 v20)
        (list v1 v12 v01)
        (list v2 v20 v12)
        (list v01 v12 v20)))

(: absmax (-> Flonum Flonum Flonum))
(define (absmax x y)
  (if (> (abs x) (abs y)) x y))

(: make-planetoid-pict (-> Pict3D))
(define (make-planetoid-pict)
  (define n 200)
  (define as
    (build-list n (λ (_) (assert (flv3normalize (flvector (- (random) 0.5)
                                                          (- (random) 0.5)
                                                          (- (random) 0.5)))
                                 values))))
  (define ws (build-list n (λ (_) 4.0 #;(+ 0.5 (* (random) 10.0)))))
  (define ss (build-list n (λ (_) 0.025 #;(+ 0.05 (* (random) 0.05)))))
  
  (: f (-> FlVector FlVector))
  (define (f v)
    (let ([v  (assert (flv3normalize v) values)])
      (define r
        (for/fold ([r : Flonum  1.0]) ([a  (in-list as)]
                                       [w  (in-list ws)]
                                       [s  (in-list ss)])
          (+ r (* s (flexp (* (sqr w) (- (flv3dot v a) 1.0)))))))
      (flv3* v r)))
  
  (define vss
    (map
     (λ ([vs : (List FlVector FlVector FlVector)])
       (match-define (list v0 v1 v2) vs)
       (list (f v0) (f v1) (f v2)))
     (list (list (flvector 1.0 0.0 0.0)
                 (flvector 0.0 1.0 0.0)
                 (flvector 0.0 0.0 1.0))
           (list (flvector 0.0 1.0 0.0)
                 (flvector -1.0 0.0 0.0)
                 (flvector 0.0 0.0 1.0))
           (list (flvector -1.0 0.0 0.0)
                 (flvector 0.0 -1.0 0.0)
                 (flvector 0.0 0.0 1.0))
           (list (flvector 0.0 -1.0 0.0)
                 (flvector 1.0 0.0 0.0)
                 (flvector 0.0 0.0 1.0))
           (list (flvector 0.0 1.0 0.0)
                 (flvector 1.0 0.0 0.0)
                 (flvector 0.0 0.0 -1.0))
           (list (flvector -1.0 0.0 0.0)
                 (flvector 0.0 1.0 0.0)
                 (flvector 0.0 0.0 -1.0))
           (list (flvector 0.0 -1.0 0.0)
                 (flvector -1.0 0.0 0.0)
                 (flvector 0.0 0.0 -1.0))
           (list (flvector 1.0 0.0 0.0)
                 (flvector 0.0 -1.0 0.0)
                 (flvector 0.0 0.0 -1.0)))))
  (let* ([vss  (append* (map (retesselate f) vss))]
         [vss  (append* (map (retesselate f) vss))]
         [vss  (append* (map (retesselate f) vss))]
         ;[vss  (append* (map (retesselate f) vss))]
         ;[vss  (append* (map (retesselate f) vss))]
         )
    (combine*
     (map (λ ([vs : (List FlVector FlVector FlVector)])
            (apply triangle vs))
          vss))))

(define num-planetoid-picts 20)
(define num-planetoids 200)

(define planetoid-picts
  (build-vector
   num-planetoid-picts
   (λ ([n : Index])
     (printf "building planetoid ~v~n" (+ n 1))
     (with-color (list (- 0.8 (* (random) 0.16))
                       (- 0.9 (* (random) 0.18))
                       (- 1.0 (* (random) 0.20)))
       (freeze 
        (combine
         (with-material '(0.01 0.29 0.70 0.1)
           (make-planetoid-pict))
         (with-material '(0.01 0.19 0.80 0.3)
           (with-color '(1 0.25 0.5)
             (sphere '(0 0 0) 1.125)))
         (with-material '(0.1 0.8 0.1 0.5)
           (with-color '(1/4 1/2 1 0.075)
             (sphere '(0 0 0) 1.35)))))))))

(struct planetoid ([pict : Pict3D]
                   [position : User-Vector]
                   [axis : User-Vector]
                   [speed : Real])
  #:transparent)

(define planetoids
  (build-list
   num-planetoids
   (λ ([n : Index])
     (planetoid (vector-ref planetoid-picts (random (vector-length planetoid-picts)))
                (list (* (- (random) 0.5) 40.0)
                      (* (- (random) 0.5) 40.0)
                      (* (- (random) 0.5) 40.0))
                (list (- (random) 0.5) (- (random) 0.5) (- (random) 0.5))
                (+ 0.1 (random))))))

(define frame (new frame% [label "1"] [width 400] [height 400]))
(define canvas (new pict3d-canvas% [parent frame]))
(send frame show #t)

(define sun
  (combine
   (with-color "black"
     (with-emitted '(4.0 3.2 2.4)
       (sphere '(0 0 0) 1)))
   (light '(0 0 0) '(2.0 1.6 1.2) 50)))

(define pict
  (set-basis
   (combine
    (combine*
     (for/list : (Listof Pict3D) ([p  (in-list planetoids)])
       (match-define (planetoid pict pos axis speed) p)
       (move (rotate pict axis (* speed (* 2 pi (random)))) pos)))
    sun)
   "camera"
   (normal-basis '(0 -40 0) '(0 1 0))))

(define spheres
  (for/list : (Listof Pict3D) ([_  (in-range 1000)])
    (sphere (list (* (- (random) 0.5) 40.0)
                  (* (- (random) 0.5) 40.0)
                  (* (- (random) 0.5) 40.0))
            0.5)))

(define (run-anim)
  (for ([angle  (in-range 0 (* 5 360) 5)])
    (define pict
      (set-basis
       (combine
        (combine*
         (for/list : (Listof Pict3D) ([p  (in-list planetoids)])
           (match-define (planetoid pict pos axis speed) p)
           (rotate (move (rotate pict axis (* speed angle)) pos)
                   '(0 0 1)
                   (* angle 0.01))))
        sun)
       "camera"
       (normal-basis '(0 -40 0) '(0 1 0))))
    (send canvas set-pict3d pict)
    (sleep/yield (/ 16 1000))))


#|
(current-material '(0.05 0.75 0.25 0.1))

(define body
  (let* ([body  (combine
                 (rectangle '(-1/4 -1/8 -1) '(1/4 1/8 2))
                 (with-color '(1/4 1/2 1 1/2)
                   (with-emitted '(2 4 8)
                     (rectangle '(-1/8 -1/16 -1.125) '(1/8 1/16 2.25))))
                 (scale
                  (with-color '(1/2 1 1/2 1/2)
                    (sphere '(0 0 0) 1/4))
                  '(1 1 2)))]
         [body  (set-basis body "right-wing" (normal-basis '(3/16 0 0) '(1 1/3 0)))]
         [body  (set-basis body "left-wing" (scale (normal-basis '(-3/16 0 0) '(-1 1/3 0))
                                                   '(-1 1 1)))])
    body))

(: make-wing (-> String Pict3D))
(define (make-wing gun-name)
  (define wing
    (combine
     ;; Attachment thingies
     (combine
      (rectangle '(-2 -1/16 -1/8) '(-1 1/16 1/8))
      (with-color '(1 3/4 1/4 1/2)
        (with-emitted '(4 3 1)
          (rectangle (list (+ -2 1/32) (+ -1/16 1/32) (- -1/8 1/32))
                     (list (- -1 1/32) (-  1/16 1/32) (+  1/8 1/16)))))
      (combine*
       (for/list : (Listof Pict3D) ([x  (in-range 1/32 1 1/16)])
         (combine
          (light (list (- -1 x) 0 (- -1/8 1/64)) '(1 3/4 1/4) #i1/256)
          (light (list (- -1 x) 0 (+  1/8 1/32)) '(1 3/4 1/4) #i1/256)))))
     ;; Lasers
     (combine
      (rectangle '(-1/16 -1/16 -1/2) '(1/16 1/16 2))
      (with-color '(1 3/4 1/4 1/2)
        (with-emitted '(16 12 4)
          (rectangle '(-1/32 -1/32 2) '(1/32 1/32 2.5))))
      (light '(0 0 2.25) '(1 3/4 1/4) 1))
     (rotate-y (scale (rotate-y
                       (combine
                        (rectangle '(-1 -1/8 -1) '(0 1/8 1/2))
                        (with-color '(1/2 1 1/2 1/2)
                          (move (scale (sphere '(0 0 0) 1)
                                       '(1/2 1/4 3/4))
                                '(-1/2 0 -1/4)))
                        ;; Engines
                        (with-color '(1 1/2 1/2 1/2)
                          (with-emitted '(8 0.5 0.5)
                            (rectangle (list (+ -1 1/8) (+ -1/8 1/16) (- -1 1/16))
                                       (list (-  0 1/8) (-  1/8 1/16) (+ 1/2 1/16))))))
                       30)
                      '(1 1 2))
               -30)))
  (let* ([wing  (set-basis wing "wing-attach" (normal-basis '(-2 0 0) '(1 0 0)))]
         [wing  (set-basis wing gun-name (normal-basis '(0 0 2) '(0 0 1)))])
    wing))

(define ship
  (rotate-x
   (pin (pin body (make-wing "right-gun") "right-wing" "wing-attach")
        (make-wing "left-gun") "left-wing" "wing-attach")
   -90))

ship
|#
