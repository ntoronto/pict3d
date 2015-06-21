#lang racket

(require pict3d
         pict3d/engine
         images/flomap)

(provide check-ray-trace-vs-opengl)

(define l (pos 1.0 1.5 2.0))

(define (dress-up p)
  (let* ([p  (set-material p (material #:ambient 0 #:diffuse 1 #:specular 0 #:roughness 1))]
         [p  (set-color p (rgba 1))]
         [p  (set-emitted p (emitted 0))]
         [p  (plane-vertex-normals p)])
    (combine p (light l (emitted 3) #:range 1000.0))))

(define (render/ray-trace p)
  (let ([p  (dress-up p)])
    (define t
      (let ([t  (camera-transform p)])
        (if t t ((current-pict3d-auto-camera) p))))
    (define v0 (affine-origin t))
    (define ray-dir (camera-ray-dir t))
    (flomap->bitmap
     (build-flomap
      1 (current-pict3d-width) (current-pict3d-height)
      (λ (_ x y)
        (define-values (v n) (trace/normal p v0 (ray-dir (+ x 0.5) (+ y 0.5))))
        (cond [(and v n)
               (define dl (pos- l v))
               (define m^2 (dir-dist^2 dl))
               (define a (/ 3.0 m^2))
               (define b (max 0.0 (/ (dir-dot n dl) (sqrt m^2))))
               (expt (* a b) (/ 1.0 2.2))]
              [else  0.0]))))))

(define (render/opengl p)
  (let ([p  (dress-up p)])
    (parameterize ([current-engine-debug-pass  'no-bloom])
      (pict3d->bitmap p))))

(define-syntax-rule (check-ray-trace-vs-opengl p-stx)
  (with-handlers ([exn:fail?  (λ (e) (eprintf "Error ~v on ~a~n" e 'p-stx))])
    (let ([p  p-stx])
      (define bm1 (render/ray-trace p))
      (define bm2 (render/opengl p))
      (define diff-fm ((flomap-lift abs) (fm- (flomap-ref-component (bitmap->flomap bm1) 1)
                                              (flomap-ref-component (bitmap->flomap bm2) 1))))
      (define-values (_ max-error) (flomap-extreme-values (flomap-blur diff-fm 10)))
      (cond [(< max-error 0.02)  #t]
            [else
             (eprintf "Ray trace vs. OpenGL test failed with max error ~v on ~a~n"
                      max-error
                      'p-stx)
             (eprintf "Ray trace~n~v~n" bm1)
             (eprintf "OpenGL~n~v~n" bm2)
             (eprintf "Absolute difference~n~v~n" (flomap->bitmap diff-fm))
             #f]))))

