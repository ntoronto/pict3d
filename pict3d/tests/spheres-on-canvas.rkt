#lang typed/racket/base

(require typed/racket/gui
         typed/racket/class
         pict3d
         racket/math
         math/flonum
         pict3d/private/math/flt3
         pict3d/private/engine/draw-passes)

(current-material '(0.05 0.75 0.25 0.1))

(define spheres
  (combine
   (combine* (for/list ([_  (in-range 50000)])
               (let* ([r  (+ (* (random) 0.5) 0.5)]
                      [g  (+ (* (random) 0.5) 0.5)]
                      [b  (+ (* (random) 0.5) 0.5)])
                 (with-color (list r g b (if (< (random) 0.5) 0.75 1.0))
                   (sphere (list (* (- (random) 0.5) 40)
                                 (* (- (random) 0.5) 40)
                                 (* (- (random) 0.5) 40))
                           (* 0.25 (+ (random) 0.1)))))))
   (combine* (for/list ([_  (in-range 500)])
               (let* ([r  (random)]
                      [g  (random)]
                      [b  (random)]
                      [mx  (max r g b)]
                      [r  (/ r mx)]
                      [g  (/ g mx)]
                      [b  (/ b mx)]
                      [x  (* (- (random) 0.5) 40)]
                      [y  (* (- (random) 0.5) 40)]
                      [z  (* (- (random) 0.5) 40)])
                 (combine
                  (with-color "black"
                    (with-emitted (list (* 16 r) (* 16 g) (* 16 b))
                      (sphere (list x y z) #i1/16)))
                  (light (list x y z)
                         (list r g b)
                         (* (random) 0.5))))))))

(define frozen-spheres (freeze spheres))

(define frame (new frame% [label "1"] [width 800] [height 800]))
(define canvas (new pict3d-canvas% [parent frame]))
(send frame show #t)

(: i Natural)
(define i 0)

(: thread-loop (-> Void))
(define (thread-loop)
  (unless (send canvas is-shown?)
    (thread-suspend th))
  (define start-time (fl (current-inexact-milliseconds)))
  (define cx (cos (degrees->radians i)))
  (define sx (sin (degrees->radians i)))
  (define cx2 (cos (* 2 (degrees->radians i))))
  (define sx2 (sin (* 2 (degrees->radians i))))
  
  (define camera-basis
    (normal-basis (list (* -6 cx) (* -6 sx) (* 0.5 sx))
                  (list (* 6 cx) (* 6 sx) (* -0.5 sx))))
  
  (define pict
    (combine
     (set-basis frozen-spheres "camera" camera-basis)
     (with-color "black"
       (with-emitted '(4 4 4)
         (sphere (list (* 2 cx2) (* 2 sx2) (* 2 sx2)) 0.5)))
     (light (list (* 2 cx2) (* 2 sx2) (* 2 sx2)) "silver" 20)))
  (send canvas set-pict3d pict)
  (set! i (+ i 1))
  (define end-time (fl (current-inexact-milliseconds)))
  (sleep/yield (* #i1/1000 (max 0.0 (- #i1000/60 (- end-time start-time)))))
  (thread-loop))

(define th
  (thread thread-loop))
