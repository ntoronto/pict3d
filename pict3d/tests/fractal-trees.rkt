#lang racket

(require pict3d)

(define branch
  (combine (scale (rectangle (pos -1 -1 -1) (pos 1 1 1)) (dir 0.1 0.1 1))
           (basis 'base (point-at (pos 0 0 -1) (dir 0 0 1)))
           (scale (basis 'top (point-at (pos 0 0 1) (dir 2/3 0 1) #:angle 30)) 0.75)
           (scale (basis 'top (point-at (pos 0 0 1) (dir -1/3 0 1) #:angle 15)) 0.75)))

;(: build-tree (-> Natural Pict3D))
(define (build-tree n)
  (ungroup
   (for/fold ([p branch]) ([_  (in-range n)])
     (weld p 'top branch 'base))
   'top))

(define t (build-tree 9))
(define frozen-t (freeze t))
t
frozen-t

(define s
  (for*/fold ([s  empty-pict3d]) ([x  (in-range -3 4)]
                                  [y  (in-range -3 4)])
    (combine s (move frozen-t (dir (* x 5) (* y 5) 0)))))
s
