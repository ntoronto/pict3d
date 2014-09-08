#lang typed/racket/base

(require pict3d)

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
