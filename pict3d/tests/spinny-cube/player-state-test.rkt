#lang typed/racket

(require pict3d
         pict3d/universe
         math/flonum
         "player-state.rkt"
         "debug-picts.rkt")

(define world-geometry
  (combine
   (with-color (rgba "lightblue")
     (combine
      (rectangle (pos 0 -3 -9) (dir 1.5 1.5 0.5))
      (rectangle (pos 0 -3 -8.5) (dir 1 1 0.5))
      (rectangle (pos 0 -3 -8) (dir 0.5 0.5 0.5))))
   (with-color (rgba "chartreuse")
     (rectangle origin (dir 9 9 9) #:inside? #t))
   (with-color (rgba "pink" 0.9)
     (combine
      (rectangle (pos 9 -9 -9) (pos 1 -6 4))
      (rectangle (pos -9 -9 -9) (pos -1 -6 4))))
   (with-color (rgba "lavender")
     (rectangle (pos -2 0 -9) (pos 2 9 -4.5)))
   (with-emitted (emitted 1 2)
     (rectangle (pos -2 0 -4.5) (pos 2 9 -4.4)))
   (rectangle (pos -2 0 -4.4) (pos 2 9 -4))
   (with-color (rgba "lightblue" 0.9)
     (rectangle (pos -2 0 5) (pos 2 9 6)))))

(current-pict3d-ambient (emitted 0))

(: last-camera-pos (U #f Pos))
(define last-camera-pos #f)

(: last-look-at-pos (U #f Pos))
(define last-look-at-pos #f)

(: camera-pos (-> Pos Dir Pos))
(define (camera-pos v ddv)
  (let ([last-c  last-camera-pos])
    (cond [last-c  (pos-between (pos+ v (camera-offset ddv)) last-c 0.99)]
          [else  (pos+ v (dir 0.5 -6 6))])))

(: look-at-pos (-> Pos Pos))
(define (look-at-pos v)
  (let ([last-p  last-look-at-pos])
    (cond [last-p  (pos-between v last-p 0.95)]
          [else  v])))

((inst big-bang3d player-state)
 (player-state origin
               (dir-scale -z gravity-accel)
               (rotate-z 225) ;identity-affine
               init-timers
               init-maneuver)
 #:width 1024
 #:height 768
 #:frame-delay frame-delay #;(assert (* 10.0 frame-delay) positive?)
 #:on-frame
 (λ ([pstate : player-state] [n : Natural] [_ : Flonum])
   (player-state-advance pstate world-geometry (* frame-delay (fl n)) (/ frame-delay 1000.0)))
 #:on-draw
 (λ ([pstate : player-state] [n : Natural] [_ : Flonum])
   (define time (/ (* n frame-delay) 1000.0))
   (match-define (player-state v ddv t tm man) pstate)
   
   (define c (camera-pos v ddv))
   (define p (look-at-pos v))
   (set! last-camera-pos c)
   (set! last-look-at-pos p)
   (combine (basis 'camera (point-at c p))
            world-geometry
            (sunlight (dir -1 -2 -4) (emitted "azure" 0.1))
            (player-state->pict3d pstate)
            (process-debug-picts frame-delay)))
 #:on-key
 (λ ([pstate : player-state] [n : Natural] [_ : Flonum] [key : String])
   (player-state-on-key pstate (* frame-delay (fl n)) key))
 #:on-release
 (λ ([pstate : player-state] [n : Natural] [_ : Flonum] [key : String])
   (player-state-on-release pstate (* frame-delay (fl n)) key)))
