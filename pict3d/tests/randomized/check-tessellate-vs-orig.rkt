#lang racket

(require pict3d
         images/flomap)

(provide check-tessellate-vs-orig)

(define l (light (pos 1.0 1.5 2.0) (emitted 3) #:range 1000.0))

(define (tess p)
  (tessellate p #:segments 16 #:max-angle 5))

(define-syntax-rule (check-tessellate-vs-orig p-stx)
  (with-handlers ([exn:fail?  (λ (e) (eprintf "Error ~v on ~a~n" e 'p-stx))])
    (let ([p  p-stx])
      (define t ((current-pict3d-auto-camera) p))
      (define bm1 (pict3d->bitmap (combine p l (basis 'camera t))))
      (define bm2 (pict3d->bitmap (combine (tess p) l (basis 'camera t))))
      (define diff-fm ((flomap-lift abs) (fm- (flomap-ref-component (bitmap->flomap bm1) 1)
                                              (flomap-ref-component (bitmap->flomap bm2) 1))))
      (define-values (_ max-error) (flomap-extreme-values (flomap-blur diff-fm 10)))
      (cond [(< max-error 0.04)  #t]
            [else
             (eprintf "Tessellate test failed with max error ~v on ~a~n"
                      max-error
                      'p-stx)
             (eprintf "Original~n~v~n" bm1)
             (eprintf "Tessellated~n~v~n" bm2)
             (eprintf "Absolute difference~n~v~n" (flomap->bitmap diff-fm))
             #f]))))
