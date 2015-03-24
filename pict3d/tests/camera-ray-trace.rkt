#lang racket

(require pict3d)

(define pict
  (with-color (rgba "salmon" 0.5)
    (scale
     (combine
      (for/list ([_  (in-range 2000)])
        (cube (pos (- (random) 0.5) (- (random) 0.5) (- (random) 0.5)) 0.05)))
     5)))

(define t ((current-pict3d-auto-camera) pict))

(define pts
  (time
   (with-color (rgba "chartreuse")
     (combine
      (for*/list ([x  (in-range 0 256 2)]
                  [y  (in-range 0 256 2)])
        (define-values (v dv) (camera-ray t x y))
        (define p (trace pict v dv))
        (if p (sphere p 0.025) empty-pict3d))))))

(current-pict3d-width 512)
(current-pict3d-height 512)
(combine (freeze pict)
         (freeze pts))
