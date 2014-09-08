#lang typed/racket

(require pict3d)

(define leaf
  (set-basis
   (scale (rectangle '(-1 -1 -1) '(1 1 1))
          '(0.1 0.1 1))
   "base"
   (normal-basis '(0 0 -1))))

(define branch
  (set-basis
   (set-basis leaf "left" (scale (normal-basis '(0 0 1) '(2/3 0 1) 30)
                                 '(0.75 0.75 0.75)))
   "right"
   (scale (normal-basis '(0 0 1) '(-1/3 0 1) 15)
          '(0.75 0.75 0.75))))

(: build-tree (-> Natural Pict3D))
(define (build-tree n)
  (cond [(= n 0)  leaf]
        [else
         (define left (build-tree (- n 1)))
         (define right (build-tree (- n 1)))
         (pin (pin branch left "left" "base")
              right "right" "base")]))

(define t (freeze (build-tree 9)))
(define s
  (for*/fold ([s : Pict3D t]) ([x  (in-range -3 4)]
                               [y  (in-range -3 4)])
    (combine s (move t (list (* x 5) (* y 5) 0)))))

t
s
