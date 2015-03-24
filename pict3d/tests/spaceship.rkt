#lang typed/racket

(require typed/racket/gui
         pict3d
         math/flonum
         math/base)

(: retesselate (-> (-> Pos Pos)
                   (-> (List Pos Pos Pos)
                       (Listof (List Pos Pos Pos)))))
(define ((retesselate f) vs)
  (match-define (list v0 v1 v2) vs)
  (define v01 (f (pos-between v0 v1 0.5)))
  (define v12 (f (pos-between v1 v2 0.5)))
  (define v20 (f (pos-between v2 v0 0.5)))
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
    (build-list n (λ (_) (assert (dir-normalize (dir (- (random) 0.5)
                                                     (- (random) 0.5)
                                                     (- (random) 0.5)))
                                 values))))
  (define ws (build-list n (λ (_) 4.0 #;(+ 0.5 (* (random) 10.0)))))
  (define ss (build-list n (λ (_) 0.025 #;(+ 0.05 (* (random) 0.05)))))
  
  (: f (-> Pos Pos))
  (define (f v)
    (let ([v  (assert (dir-normalize (pos- v origin)) values)])
      (define r
        (for/fold ([r : Flonum  1.0]) ([a  (in-list as)]
                                       [w  (in-list ws)]
                                       [s  (in-list ss)])
          (+ r (* s (flexp (* (sqr w) (- (dir-dot v a) 1.0)))))))
      (pos+ origin (dir-scale v r))))
  
  (define vss
    (map
     (λ ([vs : (List Pos Pos Pos)])
       (match-define (list v0 v1 v2) vs)
       (list (f v0) (f v1) (f v2)))
     (list (list (pos 1.0 0.0 0.0)
                 (pos 0.0 1.0 0.0)
                 (pos 0.0 0.0 1.0))
           (list (pos 0.0 1.0 0.0)
                 (pos -1.0 0.0 0.0)
                 (pos 0.0 0.0 1.0))
           (list (pos -1.0 0.0 0.0)
                 (pos 0.0 -1.0 0.0)
                 (pos 0.0 0.0 1.0))
           (list (pos 0.0 -1.0 0.0)
                 (pos 1.0 0.0 0.0)
                 (pos 0.0 0.0 1.0))
           (list (pos 0.0 1.0 0.0)
                 (pos 1.0 0.0 0.0)
                 (pos 0.0 0.0 -1.0))
           (list (pos -1.0 0.0 0.0)
                 (pos 0.0 1.0 0.0)
                 (pos 0.0 0.0 -1.0))
           (list (pos 0.0 -1.0 0.0)
                 (pos -1.0 0.0 0.0)
                 (pos 0.0 0.0 -1.0))
           (list (pos 1.0 0.0 0.0)
                 (pos 0.0 -1.0 0.0)
                 (pos 0.0 0.0 -1.0)))))
  (let* ([vss  (append* (map (retesselate f) vss))]
         [vss  (append* (map (retesselate f) vss))]
         ;[vss  (append* (map (retesselate f) vss))]
         ;[vss  (append* (map (retesselate f) vss))]
         ;[vss  (append* (map (retesselate f) vss))]
         )
    (combine
     (map (λ ([vs : (List Pos Pos Pos)]) (apply triangle vs)) vss))))

(define num-planetoid-picts 20)
(define num-planetoids 500)

(define planetoid-picts
  (build-vector
   num-planetoid-picts
   (λ ([n : Index])
     (printf "building planetoid ~v~n" (+ n 1))
     (with-color (rgba (- 0.8 (* (random) 0.16))
                       (- 0.9 (* (random) 0.18))
                       (- 1.0 (* (random) 0.20)))
       (freeze
        (combine
         (with-material (material #:ambient 0.01 #:diffuse 0.29 #:specular 0.70 #:roughness 0.1)
           (make-planetoid-pict))
         (with-material (material #:ambient 0.01 #:diffuse 0.19 #:specular 0.80 #:roughness 0.3)
           (with-color (rgba 1 0.25 0.5)
             (sphere (pos 0 0 0) 1.125)))
         (with-material (material #:ambient 0.1 #:diffuse 0.8 #:specular 0.1 #:roughness 0.5)
           (with-color (rgba 1/4 1/2 1 0.075)
             (sphere (pos 0 0 0) 1.35)))))))))

(struct planetoid ([pict : Pict3D]
                   [position : Pos]
                   [axis : Dir]
                   [speed : Real])
  #:transparent)

(define planetoids
  (build-list
   num-planetoids
   (λ (n)
     (planetoid (vector-ref planetoid-picts (random (vector-length planetoid-picts)))
                (pos (* (- (random) 0.5) 40.0)
                     (* (- (random) 0.5) 40.0)
                     (* (- (random) 0.5) 40.0))
                (dir (- (random) 0.5) (- (random) 0.5) (- (random) 0.5))
                (+ 0.1 (random))))))

(define frame (new frame% [label "1"] [width 400] [height 400]))
(define canvas (new pict3d-canvas% [parent frame]))
(send frame show #t)

(define sun
  (combine
   (with-color (rgba "black")
     (with-emitted (emitted 1 0.8 0.6 4)
       (sphere (pos 0 0 0) 1)))
   (light (pos 0 0 0) (emitted 2.0 1.6 1.2 50))))

(define pict
  (combine
   sun
   (basis 'camera (point-at (pos 0 -40 0) (dir 0 1 0)))
   (combine
    (for/list : (Listof Pict3D) ([p  (in-list planetoids)])
      (match-define (planetoid pict pos axis speed) p)
      (move (rotate pict axis (* speed (* 2 pi (random))))
            (pos- pos origin))))))

(define spheres
  (for/list : (Listof Pict3D) ([_  (in-range 1000)])
    (sphere (pos (* (- (random) 0.5) 40.0)
                 (* (- (random) 0.5) 40.0)
                 (* (- (random) 0.5) 40.0))
            0.5)))

(require/typed
 profile
 [profile-thunk  (-> (-> Void) Void)])

(define (run-anim)
  (profile-thunk
   (λ ()
     (for ([angle  (in-range 0 (* 5 360) 5)])
       (time
        (define pict
          (combine
           sun
           (basis 'camera (point-at (pos 0 -40 0) (dir 0 1 0)))
           (combine
            (for/list : (Listof Pict3D) ([p  (in-list planetoids)])
              (match-define (planetoid pict pos axis speed) p)
              (rotate (move (rotate pict axis (* speed angle)) (pos- pos origin))
                      (dir 0 0 1)
                      (* angle 0.1))))))
        (send canvas set-pict3d pict)
        (sleep/yield #i1/1000))
       (yield)))))

(current-material (material #:ambient 0.05 #:diffuse 0.75 #:specular 0.25 #:roughness 0.1))

(define body
  (let* ([body  (combine
                 (rectangle (pos -1/4 -1/8 -1) (pos 1/4 1/8 2))
                 (with-color (rgba 1/4 1/2 1 1/2)
                   (with-emitted (emitted 1/4 1/2 1 8)
                     (rectangle (pos -1/8 -1/16 -1.125) (pos 1/8 1/16 2.25))))
                 (scale
                  (with-color (rgba 1/4 1/2 1 1/2)
                    (with-emitted (emitted 1/4 1/2 1 8)
                      (sphere (pos 0 0 0) 1/4)))
                  (dir 1 1 2)))]
         [body  (combine body (basis 'right-wing (point-at (pos 3/16 0 0) (dir 1 1/3 0))))]
         [body  (combine body (basis 'left-wing (point-at (pos -3/16 0 0) (dir -1 1/3 0))))])
    body))

(: make-wing (-> Symbol Pict3D))
(define (make-wing gun-name)
  (define wing
    (combine
     ;; Attachment thingies
     (combine
      (rectangle (pos -2 -1/16 -1/8) (pos -1 1/16 1/8))
      (with-color (rgba 1 3/4 1/4 1/2)
        (with-emitted (emitted 1 3/4 1/4 4)
          (rectangle (pos (+ -2 1/32) (+ -1/16 1/32) (- -1/8 1/32))
                     (pos (- -1 1/32) (-  1/16 1/32) (+  1/8 1/16)))))
      (combine
       (for/list : (Listof Pict3D) ([x  (in-range 1/32 1 1/16)])
         (combine
          (light (pos (- -1 x) 0 (- -1/8 1/64)) (emitted 1 3/4 1/4 #i1/256))
          (light (pos (- -1 x) 0 (+  1/8 1/32)) (emitted 1 3/4 1/4 #i1/256))))))
     ;; Lasers
     (combine
      (rectangle (pos -1/16 -1/16 -1/2) (pos 1/16 1/16 2))
      (with-color (rgba 1 3/4 1/4 1/2)
        (with-emitted (emitted 1 12/16 1/16 16)
          (rectangle (pos -1/32 -1/32 2) (pos 1/32 1/32 2.5))))
      (light (pos 0 0 2.25) (emitted 1 3/4 1/4 1)))
     (rotate-y (scale (rotate-y
                       (combine
                        (rectangle (pos -1 -1/8 -1) (pos 0 1/8 1/2))
                        (with-color (rgba 1/4 1/2 1 1/2)
                          (with-emitted (emitted 1/4 1/2 1 8)
                            (move (scale (sphere (pos 0 0 0) 1)
                                         (dir 1/2 1/4 3/4))
                                  (dir -1/2 0 -1/4))))
                        ;; Engines
                        (with-color (rgba 1 1/2 1/2 1/2)
                          (with-emitted (emitted 1 1/16 1/16 8)
                            (rectangle (pos (+ -1 1/8) (+ -1/8 1/16) (- -1 1/16))
                                       (pos (-  0 1/8) (-  1/8 1/16) (+ 1/2 1/16))))))
                       30)
                      (dir 1 1 2))
               -30)))
  (let* ([wing  (combine wing (basis 'wing-attach (point-at (pos -2 0 0) (dir 1 0 0))))]
         [wing  (combine wing (basis gun-name (point-at (pos 0 0 2) (dir 0 0 1))))])
    wing))

(define ship
  (rotate-x
   (weld (weld body '(right-wing) (make-wing 'right-gun) '(wing-attach))
         '(left-wing)
         (make-wing 'left-gun)
         '(wing-attach))
   -90))

ship
