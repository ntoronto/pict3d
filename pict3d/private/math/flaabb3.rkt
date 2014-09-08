#lang typed/racket/base

#|
TODO

Implement flaabb3-plane-side
 * Initially use fast point-distance test
|#

(require (for-syntax racket/base)
         racket/match
         racket/list
         math/flonum
         "flv3.rkt"
         "flt3.rkt"
         "../utils.rkt"
         "../types.rkt")

(provide FlAABB3 flaabb3? flaabb3-min flaabb3-max
         (rename-out [flaabb3* flaabb3])
         make-flaabb3
         flaabb3-values
         flv3aabb
         flaabb3-join
         flaabb3-meet
         flaabb3-separating-plane
         flaabb3-center
         flaabb3-volume
         flaabb3-plane-side
         flaabb3-contains-point?
         flaabb3-contains-aabb?
         flaabb3-longest-axis/center
         flaabb3-transform
         flaabb3-rational?
         )

(struct flaabb3 ([min : FlVector]
                 [max : FlVector])
  #:transparent)

(define-type FlAABB3 flaabb3)

(: make-flaabb3 (-> FlVector FlVector (U #f flaabb3)))
(define (make-flaabb3 mn mx)
  (define-values (xmin ymin zmin) (flv3-values mn))
  (define-values (xmax ymax zmax) (flv3-values mx))
  (and (<= xmin xmax) (<= ymin ymax) (<= zmin zmax) (flaabb3 mn mx)))

(define-match-expander flaabb3*
  (λ (stx)
    (syntax-case stx ()
      [(_ e1 e2)  (syntax/loc stx (flaabb3 e1 e2))]))
  (λ (stx)
    (syntax-case stx ()
      [(_ . args)  (syntax/loc stx (make-flaabb3 . args))]
      [_  (syntax/loc stx make-flaabb3)])))

(: flaabb3-values (-> flaabb3 (Values Flonum Flonum Flonum Flonum Flonum Flonum)))
(define (flaabb3-values bb)
  (define-values (xmin ymin zmin) (flv3-values (flaabb3-min bb)))
  (define-values (xmax ymax zmax) (flv3-values (flaabb3-max bb)))
  (values xmin ymin zmin xmax ymax zmax))

(: flv3aabb (-> (Vectorof FlVector) (U #f flaabb3)))
(define (flv3aabb vs)
  (cond [(= 0 (vector-length vs))  #f]
        [else  (define-values (xmin ymin zmin xmax ymax zmax) (flv3aabb-values vs))
               (flaabb3 (flvector xmin ymin zmin) (flvector xmax ymax zmax))]))

(: flaabb3-join (-> flaabb3 flaabb3 flaabb3))
(define (flaabb3-join bb1 bb2)
  (define-values (xmin1 ymin1 zmin1 xmax1 ymax1 zmax1) (flaabb3-values bb1))
  (define-values (xmin2 ymin2 zmin2 xmax2 ymax2 zmax2) (flaabb3-values bb2))
  (flaabb3 (flvector (min xmin1 xmin2) (min ymin1 ymin2) (min zmin1 zmin2))
           (flvector (max xmax1 xmax2) (max ymax1 ymax2) (max zmax1 zmax2))))

(: flaabb3-meet (-> flaabb3 flaabb3 (U #f flaabb3)))
(define (flaabb3-meet bb1 bb2)
  (define-values (xmin1 ymin1 zmin1 xmax1 ymax1 zmax1) (flaabb3-values bb1))
  (define-values (xmin2 ymin2 zmin2 xmax2 ymax2 zmax2) (flaabb3-values bb2))
  (make-flaabb3 (flvector (max xmin1 xmin2) (max ymin1 ymin2) (max zmin1 zmin2))
                (flvector (min xmax1 xmax2) (min ymax1 ymax2) (min zmax1 zmax2))))

(: flaabb3-separating-plane (-> flaabb3 flaabb3 (U #f FlPlane3)))
(define (flaabb3-separating-plane bb1 bb2)
  (define-values (xmin1 ymin1 zmin1 xmax1 ymax1 zmax1) (flaabb3-values bb1))
  (define-values (xmin2 ymin2 zmin2 xmax2 ymax2 zmax2) (flaabb3-values bb2))
  (cond [(<= xmax1 xmin2)  (flplane3 (flvector +1.0 0.0 0.0) (* -0.5 (+ xmax1 xmin2)))]
        [(<= xmax2 xmin1)  (flplane3 (flvector -1.0 0.0 0.0) (* +0.5 (+ xmax2 xmin1)))]
        [(<= ymax1 ymin2)  (flplane3 (flvector 0.0 +1.0 0.0) (* -0.5 (+ ymax1 ymin2)))]
        [(<= ymax2 ymin1)  (flplane3 (flvector 0.0 -1.0 0.0) (* +0.5 (+ ymax2 ymin1)))]
        [(<= zmax1 zmin2)  (flplane3 (flvector 0.0 0.0 +1.0) (* -0.5 (+ zmax1 zmin2)))]
        [(<= zmax2 zmin1)  (flplane3 (flvector 0.0 0.0 -1.0) (* +0.5 (+ zmax2 zmin1)))]
        [else  #f]))

(: flaabb3-center (-> flaabb3 FlVector))
(define (flaabb3-center bb)
  (define-values (xmin ymin zmin xmax ymax zmax) (flaabb3-values bb))
  (flvector (* 0.5 (+ xmin xmax))
            (* 0.5 (+ ymin ymax))
            (* 0.5 (+ zmin zmax))))

(: flaabb3-volume (-> FlAABB3 Flonum))
(define (flaabb3-volume bb)
  (define-values (xmin ymin zmin xmax ymax zmax) (flaabb3-values bb))
  (* (- xmax xmin)
     (- ymax ymin)
     (- zmax zmin)))

(: flaabb3-plane-side (-> FlAABB3 FlPlane3 Box-Plane-Sides))
(define (flaabb3-plane-side b p)
  'both)

(: flaabb3-contains-point? (-> FlAABB3 FlVector Boolean))
(define (flaabb3-contains-point? bb v)
  (define-values (xmin ymin zmin xmax ymax zmax) (flaabb3-values bb))
  (define-values (x y z) (flv3-values v))
  (and (<= xmin x xmax)
       (<= ymin y ymax)
       (<= zmin z zmax)))

(: flaabb3-contains-aabb? (-> FlAABB3 FlAABB3 Boolean))
(define (flaabb3-contains-aabb? bb1 bb2)
  (define-values (xmin1 ymin1 zmin1 xmax1 ymax1 zmax1) (flaabb3-values bb1))
  (define-values (xmin2 ymin2 zmin2 xmax2 ymax2 zmax2) (flaabb3-values bb2))
  (and (<= xmin1 xmin2) (<= xmax2 xmax1)
       (<= ymin1 ymin2) (<= ymax2 ymax1)
       (<= zmin1 zmin2) (<= zmax2 zmax1)))

(: flaabb3-longest-axis/center (-> FlAABB3 (Values Index Flonum)))
(define (flaabb3-longest-axis/center b)
  (define-values (xmin ymin zmin xmax ymax zmax) (flaabb3-values b))
  (define xsize (- xmax xmin))
  (define ysize (- ymax ymin))
  (define zsize (- zmax zmin))
  (cond [(>= xsize (max ysize zsize))  (values 0 (* 0.5 (+ xmin xmax)))]
        [(>= ysize (max xsize zsize))  (values 1 (* 0.5 (+ ymin ymax)))]
        [else                          (values 2 (* 0.5 (+ zmin zmax)))]))

(: flaabb3-transform (-> FlAABB3 FlTransform3 FlAABB3))
(define (flaabb3-transform b t)
  (define-values (xmin ymin zmin xmax ymax zmax) (flaabb3-values b))
  (assert
   (flv3aabb (vector (flv4->pos (flt3apply t (flvector xmin ymin zmin 1.0)))
                     (flv4->pos (flt3apply t (flvector xmin ymin zmax 1.0)))
                     (flv4->pos (flt3apply t (flvector xmin ymax zmin 1.0)))
                     (flv4->pos (flt3apply t (flvector xmin ymax zmax 1.0)))
                     (flv4->pos (flt3apply t (flvector xmax ymin zmin 1.0)))
                     (flv4->pos (flt3apply t (flvector xmax ymin zmax 1.0)))
                     (flv4->pos (flt3apply t (flvector xmax ymax zmin 1.0)))
                     (flv4->pos (flt3apply t (flvector xmax ymax zmax 1.0)))))
   values))

(: flaabb3-rational? (-> FlAABB3 Boolean))
(define (flaabb3-rational? b)
  (define-values (xmin ymin zmin xmax ymax zmax) (flaabb3-values b))
  (and (flrational? xmin) (flrational? ymin) (flrational? zmin)
       (flrational? xmax) (flrational? ymax) (flrational? zmax)))
