#lang racket

(require racket/gui
         racket/class
         pict3d
         math/flonum
         profile)

(current-material (material #:ambient 0.05 #:diffuse 0.70 #:specular 0.25 #:roughness 0.1))

(define (random-color)
  (list (+ (* (random) 0.5) 0.5)
        (+ (* (random) 0.5) 0.5)
        (+ (* (random) 0.5) 0.5)))

(define (normalize-color c)
  (match-define (list r g b) c)
  (define mx (max r g b))
  (list (/ r mx) (/ g mx) (/ b mx)))

(define (random-position)
  (pos (* (- (random) 0.5) 50)
       (* (- (random) 0.5) 50)
       (* (- (random) 0.5) 50)))

(define spheres
  (time
   (combine
    (for/list ([_  (in-range 70000)])
      (with-color (rgba (random-color) (if (< (random) 0.5) 0.75 1.0))
        (sphere (random-position)
                (* 0.25 (+ (random) 0.1)))))
    (for/list ([_  (in-range 500)])
      (let* ([rgb  (normalize-color (random-color))]
             [pos  (random-position)]
             [int  (+ 0.25 (* (random) 0.25))])
        (combine
         (with-color (rgba "black")
           (with-emitted (emitted rgb (* int 32))
             (sphere pos #i1/16)))
         (light pos (emitted rgb int))))))))

(define frozen-spheres (time (freeze spheres)))

(define frame (new frame% [label "Sphere Field"] [width 800] [height 600]))
(define canvas (new pict3d-canvas% [parent frame]))
(send frame show #t)

(require profile)

(profile
 (let loop ([i 0])
   (when (send frame is-shown?)
     (time
      (define start-time (fl (current-inexact-milliseconds)))
      (define cx (cos (degrees->radians i)))
      (define sx (sin (degrees->radians i)))
      (define cx2 (cos (* 2 (degrees->radians i))))
      (define sx2 (sin (* 2 (degrees->radians i))))
      
      (define camera
        (point-at (pos (* -6 cx) (* -6 sx) (* 0.5 sx)) origin))
      
      (define light-pos
        (pos (* 2 cx2) (* 2 sx2) (* 2 sx2)))
      
      (define pict
        (combine
         (basis 'camera camera)
         frozen-spheres
         (with-color (rgba "black")
           (with-emitted (emitted 1 1 1 4)
             (sphere light-pos 0.5)))
         (light light-pos (emitted "silver" 20))))
      
      (send canvas set-pict3d pict)
      ;; Rate-limit to 60 FPS
      (define end-time (fl (current-inexact-milliseconds)))
      (define delay (* #i1/1000 (max 1.0 (- #i1000/60 (- end-time start-time)))))
      (sleep/yield delay)
      )
     (loop (+ i 1)))))
