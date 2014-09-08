#lang typed/racket/base

(require typed/racket/gui
         typed/racket/class
         pict3d
         racket/math)

(current-material '(0.05 0.75 0.25 0.1))

(define spheres
  (combine
   (combine* (for/list ([_  (in-range 50000)])
               (let* ([r  (+ (* (random) 0.5) 0.5)]
                      [g  (+ (* (random) 0.5) 0.5)]
                      [b  (+ (* (random) 0.5) 0.5)])
                 (with-color (list r g b (if (< (random) 0.5) 0.75 1.0))
                   (sphere (list (- (* (random) 20) 10)
                                 (- (* (random) 20) 10)
                                 (- (* (random) 20) 10))
                          (* 0.125 (+ (random) 0.1)))))))
   (combine* (for/list ([_  (in-range 500)])
               (let* ([r  (random)]
                      [g  (random)]
                      [b  (random)])
                 (light (list (- (* (random) 20) 10)
                              (- (* (random) 20) 10)
                              (- (* (random) 20) 10))
                        (list r g b)
                        (* (random) 0.5)))))))
(define frozen-spheres (freeze spheres))

(define frame (new frame% [label "1"] [width 400] [height 400]))
(define canvas (new pict3d-canvas% [parent frame]))
(send frame show #t)

(for ([i  (in-range 0 (* 5 360) 1)])
  (define cx (cos (degrees->radians i)))
  (define sx (sin (degrees->radians i)))
  (define cx2 (cos (* 2 (degrees->radians i))))
  (define sx2 (sin (* 2 (degrees->radians i))))
  (define pict
    (combine
     (set-basis frozen-spheres "camera" (normal-basis (list (* -4 cx) (* -4 sx) (+ 1 sx))
                                                      (list (* 4 cx) (* 4 sx) (- (+ 1 sx)))))
     (with-emitted '(4 4 4)
       (sphere (list (* 2 cx2) (* 2 sx2) (* 2 sx2)) 0.5))
     (light (list (* 2 cx2) (* 2 sx2) (* 2 sx2)) "silver" 40)))
  (send canvas set-pict3d pict)
  (sleep/yield (/ 16 1000)))
