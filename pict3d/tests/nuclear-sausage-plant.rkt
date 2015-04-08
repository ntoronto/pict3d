#lang racket

(require racket/gui
         pict3d
         pict3d/universe
         plot/utils)

(current-material (material #:diffuse 0.1 #:specular 0.9 #:roughness 0.1))

(define (make-segment n)
  (define size (exact->inexact (/ 3 (+ 3 n))))
  (combine (scale (ellipsoid (pos -1/2 -1/2 0) (pos 1/2 1/2 1))
                  (dir (* 1/2 size) (* 1/2 size) size))
           (basis 'bottom (point-at (pos 0 0 0) (dir 0 0 1)))
           (if (zero? (modulo n 3))
               (combine
                (basis 'top (point-at (pos 0 0 size) (dir 1 0 1) #:angle 45))
                (basis 'top (point-at (pos 0 0 size) (dir -1 0 1) #:angle -45)))
               (basis 'top (point-at (pos 0 0 size)
                                     (dir (if (zero? (modulo n 2)) -1 1) 0 1/2)
                                     #:angle 0)))))

(define (segment-chain n)
  (cond [(= n 0)  (set-emitted (make-segment n) (emitted "green" 0.25))]
        [else  (pin* (segment-chain (- n 1)) (make-list n 'top)
                     (set-emitted (make-segment n) (emitted "green" 0.25))
                     '(bottom))]))

(define chain (segment-chain 18))

(define (wiggle p)
  (replace-in-group p '(top) (λ (p) (rotate-z (wiggle p)
                                              (* 0.5 (+ 0.9 (* 0.1 (random))))))))

(big-bang3d
 (list chain 0.0)
 #:name "Nuclear Sausage Plant"
 #:width 800
 #:height 600
 #:on-frame
 (λ (s n t)
   (match-define (list p t0) s)
   (printf "frame num: ~a time ~a~n" n (- t t0))
   (list (rotate-z (wiggle p) 0.5) t))
 #:on-draw
 (λ (s n t)
   (match-define (list p t) s)
   (combine (basis 'camera (point-at (pos 0 2 4) (dir 0 -1/2 -1)))
            (sunlight (dir -1 0 -1) (emitted "yellow" 2))
            p))
 )
