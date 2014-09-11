#lang racket/base

#|
TODO

Line-triangle intersection
 * Fast line-inside-line against edges
 ? Autodetect special cases for axial planes

Sphere-triangle intersection?

Clipping
 ? Autodetect special cases for axial planes
|#

(module typed-defs typed/racket/base
  
  (require (for-syntax racket/base)
           racket/list
           racket/match
           math/flonum
           "../math/flv3.rkt"
           "../math/flrect3.rkt"
           "../utils.rkt"
           "../types.rkt")
  
  (provide (all-defined-out))
  
  ;; =================================================================================================
  ;; Triangle type, constructors and accessors
  
  (struct fltriangle3 ([vertices : (Vectorof FlVector)]
                       [lazy-plane : (Lazy-Box (U #f FlPlane3))])
    #:transparent)
  
  (define-type FlTriangle3 fltriangle3)
  
  (: make-fltriangle3 (->* [(Vectorof FlVector)] [(U FlPlane3 #f)] FlTriangle3))
  (define (make-fltriangle3 vs [plane 'lazy])
    (cond [(not (= 3 (vector-length vs)))
           (raise-argument-error 'fltriangle3 "vector of length 3" 0 vs)]
          [else
           (fltriangle3 vs (box plane))]))
  
  (: fltriangle3-plane (-> FlTriangle3 (U #f FlPlane3)))
  (define (fltriangle3-plane p)
    (lazy-box-ref! (fltriangle3-lazy-plane p)
                   (λ () (flv3polygon-plane (fltriangle3-vertices p)))))
  
  (: fltriangle3-normal (-> FlTriangle3 (U #f FlVector)))
  (define (fltriangle3-normal p)
    (define plane (fltriangle3-plane p))
    (and plane (flplane3-normal plane)))
  
  (: fltriangle3-regularity (-> FlTriangle3 Flonum))
  (define (fltriangle3-regularity p)
    (define s (flv3polygon-regularity (fltriangle3-vertices p)))
    (if s s 0.0))
  
  (: fltriangle3-rect (-> FlTriangle3 FlRect3))
  (define (fltriangle3-rect p)
    (assert (flv3rect (fltriangle3-vertices p)) flrect3?))
  
  (: fltriangle3-centroid (-> FlTriangle3 FlVector))
  (define (fltriangle3-centroid p)
    (flv3polygon-centroid (fltriangle3-vertices p)))
  
  ;; =================================================================================================
  ;; Containment (after projection onto plane)
  
  (: triangle-point-inside? (-> FlVector FlVector FlVector FlVector Boolean))
  (define (triangle-point-inside? v1 v2 v3 v)
    (let ([a  (flv3- v1 v)]
          [b  (flv3- v2 v)]
          [c  (flv3- v3 v)])
      (define ab (flv3dot a b))
      (define ac (flv3dot a c))
      (define bc (flv3dot b c))
      (define cc (flv3mag^2 c))
      (cond [(< (- (* bc ac) (* cc ab)) 0.0)  #f]
            [else
             (define bb (flv3mag^2 b))
             (>= (- (* ab bc) (* ac bb)) 0.0)])))
  
  #;; Alternative:
  (define (triangle-point-inside? v1 v2 v3 v)
    (let ([a  (flv3- v1 v)]
          [b  (flv3- v2 v)]
          [c  (flv3- v3 v)])
      (define u (flv3cross b c))
      (define v (flv3cross c a))
      (cond [(< (flv3dot u v) 0.0)  #f]
            [else
             (define w (flv3cross a b))
             (>= (flv3dot u w) 0.0)])))
  
  ;; =================================================================================================
  ;; Clipping
  
  (: fltriangle3-clip (-> FlTriangle3
                          FlPlane3
                          Plane-Sides
                          (Listof FlTriangle3)))
  ;; TODO: specialize this to requested side (after perfecting fltriangle3-split)
  (define (fltriangle3-clip t p side)
    (define-values (ts- ts0 ts+) (fltriangle3-split t p))
    (case side
      [(neg)  ts-]
      [(pos)  ts+]
      [(negzero)  (append ts0 ts-)]
      [(poszero)  (append ts0 ts+)]
      [(zero)  ts0]
      [(nonzero)  (append ts- ts+)]))
  
  ;; =================================================================================================
  ;; Splitting
  
  (define +max-dist.0 (* 16.0 epsilon.0))
  (define -max-dist.0 (- +max-dist.0))
  
  (: fltriangle3-split* (-> FlTriangle3
                            FlPlane3
                            (Values (Listof FlTriangle3)
                                    (Listof FlTriangle3)
                                    (Listof FlTriangle3))))
  (define (fltriangle3-split* t p)
    (define tp (fltriangle3-plane t))
    (cond
      ;; If (equal? p tp) then the t is definitely contained in p (we are most likely in a BSP
      ;; build for which t or one of its subtriangles contributed p as a splitting plane)
      [(equal? p tp)  (values empty (list t) empty)]
      [else
       (match-define (fltriangle3 vs _) t)
       (match-define (vector v1 v2 v3) vs)
       ;; Find all vertices' signed distances to the plane
       (define ds (flplane3-relative-dists p vs))
       (cond
         ;; Check for degenerate triangle at 0,0,0
         [(not ds)  (values empty empty empty)]
         [else
          ;; Split the triangle with vertices v1 v2 v3 into two triangles; v3 is assumed on the plane
          ;; If neg? = #t, then v1 is on the negative side of the plane
          (define split2
            (λ ([v1 : FlVector] [v2 : FlVector] [v3 : FlVector] [neg? : Boolean])
              ;; Find the point on the plane between v1 and v2
              (define v12 (flplane3-line-isect p v1 v2 #f))
              ;; It should exist, but make sure (the type system requires it, and it's a good idea)
              (cond
                [v12
                 ;; Build negative-side triangle (assuming neg? = #t)
                 (define ts1 (list (make-fltriangle3 (vector v12 v3 v1) tp)))
                 ;; Build positive-side triangle (assuming neg? = #t)
                 (define ts2 (list (make-fltriangle3 (vector v3 v12 v2) tp)))
                 ;; Return them on the correct side according to neg?
                 (if neg? (values ts1 empty ts2) (values ts2 empty ts1))]
                [else
                 ;; This shouldn't happen. The vertices on opposite sides must be more than a few
                 ;; epsilons away from the plane, so the line they define can't be parallel to the
                 ;; plane. flplane3-line-isect is correctly rounded, so it should find an
                 ;; intersection.
                 ;; Just silently fail (TODO: is this the right thing?)
                 (printf "failing in split2~n")
                 (values empty empty empty)])))
          
          ;; Split the triangle with vertices v1 v2 v3 into three triangles, where v1 is on one side
          ;; of the plane and v2 and v3 are on the other
          ;; If neg? = #t, then v1 is on the negative side of the plane
          (define split3
            (λ ([v1 : FlVector] [v2 : FlVector] [v3 : FlVector] [neg? : Boolean])
              ;; Find intersection points
              (define v12 (flplane3-line-isect p v1 v2 #f))
              (define v31 (flplane3-line-isect p v3 v1 #f))
              ;; They should exist, but make sure
              (cond
                [(and v12 v31)
                 ;; Build negative-side triangle (assuming neg? = #t)
                 (define ts1 (list (make-fltriangle3 (vector v1 v12 v31) tp)))
                 ;; Build positive-side triangles (assuming neg? = #t)
                 ;; TODO: do this in a way that minimizes the maximum nonregularity
                 (define ts2 (list (make-fltriangle3 (vector v3 v12 v2) tp)
                                   (make-fltriangle3 (vector v12 v3 v31) tp)))
                 ;; Return them on the correct side according to neg?
                 (if neg? (values ts1 empty ts2) (values ts2 empty ts1))]
                [else
                 ;; Just silently fail
                 (printf "failing in split3~n")
                 (values empty empty empty)])))
          
          ;; Get point-plane distances
          (define d1 (flvector-ref ds 0))
          (define d2 (flvector-ref ds 1))
          (define d3 (flvector-ref ds 2))
          
          (define all-nonpositive? (<= (max d1 d2 d3) +max-dist.0))
          (define all-nonnegative? (>= (min d1 d2 d3) -max-dist.0))
          
          ;; Determine how to split the triangle
          (cond
            ;; Easy cases: all zero, all nonpositive, all nonnegative
            [(and all-nonpositive? all-nonnegative?)  (values empty (list t) empty)]
            [all-nonpositive?                         (values (list t) empty empty)]
            [all-nonnegative?                         (values empty empty (list t))]
            ;; At this point, at most one vertex can be on the plane, and the triangle is bisected
            [(<= (abs d3) +max-dist.0)  (split2 v1 v2 v3 (< d1 0.0))]  ; v3 on the plane
            [(<= (abs d1) +max-dist.0)  (split2 v2 v3 v1 (< d2 0.0))]  ; v1 on the plane
            [(<= (abs d2) +max-dist.0)  (split2 v3 v1 v2 (< d3 0.0))]  ; v2 on the plane
            ;; At this point, no vertices can be on the plane, and the triangle is bisected
            [(> (* d2 d3) 0.0)  (split3 v1 v2 v3 (< d1 0.0))]  ; v2 and v3 on same side
            [(> (* d3 d1) 0.0)  (split3 v2 v3 v1 (< d2 0.0))]  ; v3 and v1 on same side
            [else               (split3 v3 v1 v2 (< d3 0.0))]  ; v1 and v2 on same side
            )])]))
  
  (: fltriangle3-plane-side (-> FlTriangle3 FlPlane3 (U 'negzero 'poszero 'zero 'both)))
  (define (fltriangle3-plane-side t p)
    (match-define (fltriangle3 vs _) t)
    (match-define (vector v1 v2 v3) vs)
    ;; Find all vertices' signed distances to the plane
    (define ds (flplane3-relative-dists p vs))
    (cond
      ;; Check for degenerate triangle at 0,0,0
      [(not ds)  'both]
      [else
       ;; Get point-plane distances
       (define d1 (flvector-ref ds 0))
       (define d2 (flvector-ref ds 1))
       (define d3 (flvector-ref ds 2))
       
       (define all-nonpositive? (<= (max d1 d2 d3) +max-dist.0))
       (define all-nonnegative? (>= (min d1 d2 d3) -max-dist.0))
       
       (cond [(and all-nonpositive? all-nonnegative?)  'zero]
             [all-nonpositive?  'negzero]
             [all-nonnegative?  'poszero]
             [else  'both])]))
  
  (: fltriangle3-split (-> FlTriangle3
                           FlPlane3
                           (Values (Listof FlTriangle3)
                                   (Listof FlTriangle3)
                                   (Listof FlTriangle3))))
  (define (fltriangle3-split t p)
    (define-values (neg-ts zero-ts pos-ts) (fltriangle3-split* t p))
    (for ([t  (in-list neg-ts)])
      (unless (eq? 'negzero (fltriangle3-plane-side t p))
        (printf "bad split: triangle ~v isn't on negative side of plane ~v" t p)))
    (for ([t  (in-list zero-ts)])
      (unless (eq? 'zero (fltriangle3-plane-side t p))
        (printf "bad split: triangle ~v isn't in plane ~v" t p)))
    (for ([t  (in-list pos-ts)])
      (unless (eq? 'poszero (fltriangle3-plane-side t p))
        (printf "bad split: triangle ~v isn't on positive side of plane ~v" t p)))
    (values neg-ts zero-ts pos-ts))
  
  ;; =================================================================================================
  ;; BSP build support
  
  (: fltriangle3-splitting-planes (-> FlTriangle3 (Listof FlPlane3)))
  (define (fltriangle3-splitting-planes t)
    (define p (fltriangle3-plane t))
    (if p (list p) empty))
  )

(require (for-syntax racket/base)
         racket/match
         'typed-defs)

(provide FlTriangle3 fltriangle3? fltriangle3-vertices
         (rename-out [fltriangle3* fltriangle3])
         fltriangle3-centroid
         fltriangle3-normal
         fltriangle3-plane
         fltriangle3-regularity
         fltriangle3-rect
         fltriangle3-clip
         fltriangle3-split
         fltriangle3-splitting-planes
         )

(define-match-expander fltriangle3*
  (λ (stx)
    (syntax-case stx ()
      [(_ e1 e2)  (syntax/loc stx (fltriangle3 e1 e2 _))]))
  (λ (stx)
    (syntax-case stx ()
      [(_ . args)  (syntax/loc stx (make-fltriangle3 . args))]
      [_  (syntax/loc stx make-fltriangle3)])))
