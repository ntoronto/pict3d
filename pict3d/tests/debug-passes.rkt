#lang racket

(require pict3d
         pict3d/engine)

(define material1 (material #:diffuse 0.25 #:specular 0.75 #:roughness 0.25))
(define material2 (material #:diffuse 0.75 #:specular 0.25 #:roughness 1.0))

(define pict (combine (with-material material2
                        (cube origin 1/2))
                      (with-material material1
                        (with-color (rgba 1 0.75)
                          (cube (pos 0 0 1) 1/2)))
                      (with-material material2
                        (with-color (rgba 1 0.75)
                          (sphere (pos 1 0 0) 1/2)))
                      (with-material material1
                        (sphere (pos 0 1 0) 1/2))
                      (light (pos 0 1 1) (emitted "pink" 1))
                      (light (pos 1 0 1) (emitted "lightgreen" 1))
                      (light (pos 1 1 0) (emitted "lightblue" 1))
                      (basis 'camera (point-at (pos 1.2 1.2 1.2)
                                               (pos 0 0 0.35)))))

(for ([dp  (in-list (cons #f (get-engine-debug-passes)))])
  (parameterize ([current-engine-debug-pass  dp]
                 [current-pict3d-z-near  0.75]
                 [current-pict3d-z-far  2.5])
    (printf "~a: ~v " dp (pict3d->bitmap pict 128 128))))
(newline)
