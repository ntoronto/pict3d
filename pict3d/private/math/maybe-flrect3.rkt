#lang typed/racket/base

(require "flv3.rkt"
         "flplane3.rkt"
         "flt3.rkt"
         "flrect3.rkt")

(provide (all-defined-out))

(: maybe-flrect3-join (-> (U #f FlRect3) (U #f FlRect3) (U #f FlRect3)))
(define (maybe-flrect3-join b1 b2)
  (if b1 (if b2 (flrect3-join b1 b2) b1) b2))

(: maybe-flrect3-transform (-> (U #f FlRect3) FlAffine3 (U #f FlRect3)))
(define (maybe-flrect3-transform b t)
  (and b (flrect3-transform b t)))

(: maybe-flrect3-contains-rect? (-> (U #f FlRect3) (U #f FlRect3) Boolean))
(define (maybe-flrect3-contains-rect? b1 b2)
  (cond [(not b2)  #t]
        [(not b1)  #f]
        [else  (flrect3-contains-rect? b1 b2)]))

(: maybe-flrect3-line-intersects (-> (U #f FlRect3) FlV3 FlV3 Flonum Flonum
                                     (Values (U #f Flonum) (U #f Flonum))))
(define (maybe-flrect3-line-intersects bb v dv min-time max-time)
  (if bb (flrect3-line-intersects bb v dv min-time max-time) (values #f #f)))

(: maybe-flrect3-classify/planes (-> (U #f FlRect3) (Listof FlPlane3) (U 'inside 'outside 'both)))
(define (maybe-flrect3-classify/planes bb planes)
  (if bb (flrect3-classify/planes bb planes) 'inside))

(: maybe-flrect3-plane-side (-> (U #f FlRect3) FlPlane3 (U #f Rect-Plane-Sides)))
(define (maybe-flrect3-plane-side bb p)
  (and bb (flrect3-plane-side bb p)))

(: maybe-flrect3-disjoint? (-> (U #f FlRect3) (U #f FlRect3) Boolean))
(define (maybe-flrect3-disjoint? bb1 bb2)
  (if (and bb1 bb2)
      (flrect3-disjoint? bb1 bb2)
      #t))
