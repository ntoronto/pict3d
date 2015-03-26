#lang typed/racket

(require pict3d)

(require/typed
 profile
 [profile-thunk  (All (A) (-> (-> A) A))])

(require/typed
 contract-profile
 [contract-profile-thunk  (All (A) (-> (-> A) A))])

(printf "starting...~n")

(current-pict3d-width 512)
(current-pict3d-height 512)

(define pict
  (with-color (rgba "salmon" 0.5)
    (scale
     (combine
      (for/list : (Listof Pict3D) ([_  (in-range 2000)])
        (cube (pos (- (random) 0.5) (- (random) 0.5) (- (random) 0.5)) 0.05)))
     5)))

(define t ((current-pict3d-auto-camera) pict))

(define vdvs
  (time
   (for*/list : (Listof (U #f (Pair Pos Dir))) ([x  (in-range 0 512 4)]
                                                [y  (in-range 0 512 4)])
     (define-values (v dv) (camera-ray t x y))
     (and v dv (cons v dv)))))

(define-values (vs dvs)
  (for/lists ([vs : (Listof Pos)] [dvs : (Listof Dir)]) ([vdv  (in-list vdvs)]
                                                         #:when vdv)
    (values (car vdv) (cdr vdv))))

(define ps
  (append*
   (time
    (for/list : (Listof (Listof Pos)) ([v   (in-list vs)]
                                       [dv  (in-list dvs)])
      (define p (trace pict v dv))
      (if p (list p) empty)))))

(define pts
  (with-color (rgba "chartreuse")
    (combine
     (for/list : (Listof Pict3D) ([p  (in-list ps)])
       (sphere p 0.025)))))

(combine (freeze pict)
         (freeze pts))
