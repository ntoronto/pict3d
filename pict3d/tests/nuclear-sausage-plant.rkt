#lang racket

(require racket/gui
         pict3d
         plot/utils)

(current-color '(1 1 1 1))
(current-material (make-material 0.0 0.1 0.9 0.1))

(define (make-segment n)
  (define size (exact->inexact (/ 3 (+ 3 n))))
  (combine (scale (ellipsoid '(-1/2 -1/2 0) '(1/2 1/2 1))
                  (list (* 1/2 size) (* 1/2 size) size))
           (basis 'bottom (point-at #:from '(0 0 0) #:dir '(0 0 1)))
           (if (zero? (modulo n 3))
               (combine
                (basis 'top (point-at #:from (list 0 0 size) #:dir (list 1 0 1) #:angle 45))
                (basis 'top (point-at #:from (list 0 0 size) #:dir (list -1 0 1) #:angle -45)))
               (basis 'top (point-at #:from (list 0 0 size)
                                     #:dir (list (if (zero? (modulo n 2)) -1 1) 0 1/2)
                                     #:angle 0)))))

(define (segment-chain n)
  (cond [(= n 0)  (set-emitted (make-segment n) (list 0 1 0 0.25))]
        [else  (pin (segment-chain (- n 1)) (make-list n 'top)
                    (set-emitted (make-segment n) (list 0 1 0 0.25))
                    'bottom)]))

(define frame (new frame% [label "Wiggly Segments"] [width 800] [height 600]))
(define canvas (new pict3d-canvas% [parent frame]))
(send canvas set-async-updates? #f)
(send frame show #t)

(define chain (segment-chain 18))

(define (wiggle p)
  (replace-in-group p 'top (λ (p) (rotate-z (wiggle p)
                                            (* 0.5 (+ 0.9 (* 0.1 (random))))))))

(let loop ([chain chain])
  (when (send frame is-shown?)
    (let ([chain  (rotate-z (wiggle chain) 0.5)])
      (time
       (define start (current-inexact-milliseconds))
       (send canvas set-pict3d
             (combine (basis 'camera (point-at #:from '(0 2 4) #:dir '(0 -1/2 -1)))
                      (sunlight '(-1 0 -1) "yellow" 2)
                      chain))
       (define diff (max 0.0 (- (current-inexact-milliseconds) start)))
       (sleep/yield (/ (max 1.0 (- #i1000/60 diff)) 1000.0)))
      (loop chain))))
