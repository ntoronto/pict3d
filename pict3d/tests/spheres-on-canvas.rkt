#lang racket

(require racket/gui
         racket/class
         pict3d
         math/flonum
         profile)

(current-material '(0.05 0.70 0.25 0.1))

(define (random-color)
  (build-list 3 (λ (_) (+ (* (random) 0.5) 0.5))))

(define (normalize-color rgb)
  (define mx (apply max rgb))
  (map (λ (r) (/ r mx)) rgb))

(define (random-position)
  (build-list 3 (λ (_) (* (- (random) 0.5) 50))))

(define spheres
  (time
   (combine
    (combine*
     (for/list ([_  (in-range 70000)])
       (with-color (append (random-color) (list (if (< (random) 0.5) 0.75 1.0)))
         (sphere (random-position)
                 (* 0.25 (+ (random) 0.1))))))
    (combine*
     (for/list ([_  (in-range 500)])
       (let* ([rgb  (normalize-color (random-color))]
              [pos  (random-position)]
              [int  (+ 0.25 (* (random) 0.25))])
         (combine
          (with-color "black"
            (with-emitted (append rgb (list (* int 32)))
              (sphere pos #i1/16)))
          (light pos rgb int))))))))

(define frozen-spheres (freeze spheres))

(define frame (new frame% [label "Sphere Field"] [width 800] [height 600]))
(define canvas (new pict3d-canvas% [parent frame]))
(send canvas set-async-updates? #f)
(send frame show #t)

(require profile)

(profile
 (let loop ([i 0])
   (when (and (< i 100) (send frame is-shown?))
     ;(define start-time (fl (current-inexact-milliseconds)))
     (define cx (cos (degrees->radians i)))
     (define sx (sin (degrees->radians i)))
     (define cx2 (cos (* 2 (degrees->radians i))))
     (define sx2 (sin (* 2 (degrees->radians i))))
     
     (define camera-basis
       (normal-basis (list (* -6 cx) (* -6 sx) (* 0.5 sx))
                     (list (* 6 cx) (* 6 sx) (* -0.5 sx))))
     
     (define pict
       ;(set-basis spheres 'camera camera-basis)
       
       (combine
        (set-basis spheres 'camera camera-basis)
        (with-color "black"
          (with-emitted '(1 1 1 4)
            (sphere (list (* 2 cx2) (* 2 sx2) (* 2 sx2)) 0.5)))
        (light (list (* 2 cx2) (* 2 sx2) (* 2 sx2)) "silver" 20)))
     
     ;(values
     (time
      (send canvas set-pict3d pict))
     ;(define end-time (fl (current-inexact-milliseconds)))
     ;(define delay (* #i1/1000 (max 0.0 (- #i1000/60 (- end-time start-time)))))
     ;(sleep/yield delay)
     (loop (+ i 1)))))
