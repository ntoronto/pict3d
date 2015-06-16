#lang typed/racket/base

(require (for-syntax racket/base)
         racket/match
         racket/list
         math/flonum
         math/base
         "fl.rkt"
         "fl3.rkt"
         "flv3.rkt"
         "flv4.rkt"
         "flt3.rkt"
         "flplane3.rkt"
         "../utils.rkt")

(provide (all-defined-out))

(define-type Rect-Plane-Sides (U 'neg 'negzero 'zero 'poszero 'pos 'both))

(define-type Plane-Sides (U 'neg 'negzero 'zero 'poszero 'pos 'nonzero))

;; ===================================================================================================
;; Types

(define print-flrect3
  (make-constructor-style-printer
   (λ ([bb : FlRect3]) 'flrect3)
   (λ ([bb : FlRect3]) (list (flrect3-min bb) (flrect3-max bb)))))

(struct FlRect3 ([min : FlV3] [max : FlV3])
 #:property prop:custom-print-quotable 'never
 #:property prop:custom-write print-flrect3
 #:transparent)

(define-syntax flrect3? (make-rename-transformer #'FlRect3?))
(define-syntax flrect3-min (make-rename-transformer #'FlRect3-min))
(define-syntax flrect3-max (make-rename-transformer #'FlRect3-max))

;; ===================================================================================================

(define zero-flrect3 (FlRect3 zero-flv3 zero-flv3))
(define unit-flrect3 (FlRect3 -x-y-z-flv3 +x+y+z-flv3))
(define inf-flrect3 (FlRect3 (flv3 -inf.0 -inf.0 -inf.0)
                             (flv3 +inf.0 +inf.0 +inf.0)))

(: flrect3 (case-> (-> FlV3 FlRect3)
                   (-> FlV3 FlV3 FlRect3)
                   (-> FlV3 FlV3 FlV3 FlRect3)
                   (-> FlV3 FlV3 FlV3 FlV3 FlRect3)))
(define flrect3
  (case-lambda
    [(v)  (FlRect3 v v)]
    [(v1 v2)
     (call/flv3-values v1
       (λ (x1 y1 z1)
         (call/flv3-values v2
           (λ (x2 y2 z2)
             (if (and (<= x1 x2) (<= y1 y2) (<= z1 z2))
                 (FlRect3 v1 v2)
                 (FlRect3 (flv3 (min x1 x2) (min y1 y2) (min z1 z2))
                          (flv3 (max x1 x2) (max y1 y2) (max z1 z2))))))))]
    [(v1 v2 v3)
     (call/flv3-values v1
       (λ (x1 y1 z1)
         (call/flv3-values v2
           (λ (x2 y2 z2)
             (call/flv3-values v3
               (λ (x3 y3 z3)
                 (FlRect3 (flv3 (min x1 x2 x3) (min y1 y2 y3) (min z1 z2 z3))
                          (flv3 (max x1 x2 x3) (max y1 y2 y3) (max z1 z2 z3)))))))))]
    [(v1 v2 v3 v4)
     (call/flv3-values v1
       (λ (x1 y1 z1)
         (call/flv3-values v2
           (λ (x2 y2 z2)
             (call/flv3-values v3
               (λ (x3 y3 z3)
                 (call/flv3-values v4
                   (λ (x4 y4 z4)
                     (FlRect3 (flv3 (min x1 x2 x3 x4) (min y1 y2 y3 y4) (min z1 z2 z3 z4))
                              (flv3 (max x1 x2 x3 x4) (max y1 y2 y3 y4) (max z1 z2 z3 z4)))))))))))]
    ))

(: flrect3-update (-> FlRect3 FlV3 FlRect3))
(define (flrect3-update bb v)
  (flrect3 (flrect3-min bb) (flrect3-max bb) v))

(: transformed-sphere-flrect3 (-> FlAffine3 FlRect3))
(define (transformed-sphere-flrect3 t)
  (call/flaffine3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (define dx (fl3mag m00 m01 m02))
      (define dy (fl3mag m10 m11 m12))
      (define dz (fl3mag m20 m21 m22))
      (flrect3 (flv3 (- m03 dx) (- m13 dy) (- m23 dz))
               (flv3 (+ m03 dx) (+ m13 dy) (+ m23 dz))))))

(: transformed-disk-flrect3 (-> FlAffine3 FlRect3))
(define (transformed-disk-flrect3 t)
  (call/flaffine3-forward t
    (λ (m00 m01 _m02 m03 m10 m11 _m12 m13 m20 m21 _m22 m23)
      (define dx (fl3mag m00 m01 0.0))
      (define dy (fl3mag m10 m11 0.0))
      (define dz (fl3mag m20 m21 0.0))
      (flrect3 (flv3 (- m03 dx) (- m13 dy) (- m23 dz))
               (flv3 (+ m03 dx) (+ m13 dy) (+ m23 dz))))))

(define-syntax-rule (call/flrect3-values bb-stx f)
  (let ([bb : FlRect3  bb-stx])
    (call/flv3-values (flrect3-min bb)
      (λ (xmin ymin zmin)
        (call/flv3-values (flrect3-max bb)
          (λ (xmax ymax zmax)
            (f xmin ymin zmin xmax ymax zmax)))))))

;; ===================================================================================================
;; Operations

(: flrect3-join (-> FlRect3 FlRect3 FlRect3))
(define (flrect3-join bb1 bb2)
  (call/flrect3-values bb1
    (λ (xmin1 ymin1 zmin1 xmax1 ymax1 zmax1)
      (call/flrect3-values bb2
        (λ (xmin2 ymin2 zmin2 xmax2 ymax2 zmax2)
          (FlRect3 (flv3 (min xmin1 xmin2) (min ymin1 ymin2) (min zmin1 zmin2))
                   (flv3 (max xmax1 xmax2) (max ymax1 ymax2) (max zmax1 zmax2))))))))

(: flrect3-meet (-> FlRect3 FlRect3 (U #f FlRect3)))
(define (flrect3-meet bb1 bb2)
  (call/flrect3-values bb1
    (λ (xmin1 ymin1 zmin1 xmax1 ymax1 zmax1)
      (call/flrect3-values bb2
        (λ (xmin2 ymin2 zmin2 xmax2 ymax2 zmax2)
          (define xmin (max xmin1 xmin2))
          (define ymin (max ymin1 ymin2))
          (define zmin (max zmin1 zmin2))
          (define xmax (min xmax1 xmax2))
          (define ymax (min ymax1 ymax2))
          (define zmax (min zmax1 zmax2))
          (if (and (<= xmin xmax) (<= ymin ymax) (<= zmin zmax))
              (FlRect3 (flv3 xmin ymin zmin)
                       (flv3 xmax ymax zmax))
              #f))))))

(: flrect3-separating-plane (-> FlRect3 FlRect3 (U #f FlPlane3)))
(define (flrect3-separating-plane bb1 bb2)
  (call/flrect3-values bb1
    (λ (xmin1 ymin1 zmin1 xmax1 ymax1 zmax1)
      (call/flrect3-values bb2
        (λ (xmin2 ymin2 zmin2 xmax2 ymax2 zmax2)
          (cond [(<= xmax1 xmin2)  (unsafe-flplane3 +1.0 0.0 0.0 (* -0.5 (+ xmax1 xmin2)))]
                [(<= xmax2 xmin1)  (unsafe-flplane3 -1.0 0.0 0.0 (* +0.5 (+ xmin1 xmax2)))]
                [(<= ymax1 ymin2)  (unsafe-flplane3 0.0 +1.0 0.0 (* -0.5 (+ ymax1 ymin2)))]
                [(<= ymax2 ymin1)  (unsafe-flplane3 0.0 -1.0 0.0 (* +0.5 (+ ymax2 ymin1)))]
                [(<= zmax1 zmin2)  (unsafe-flplane3 0.0 0.0 +1.0 (* -0.5 (+ zmax1 zmin2)))]
                [(<= zmax2 zmin1)  (unsafe-flplane3 0.0 0.0 -1.0 (* +0.5 (+ zmax2 zmin1)))]
                [else  #f]))))))

(: flrect3-inside-planes (-> FlRect3 (Values FlPlane3 FlPlane3 FlPlane3 FlPlane3 FlPlane3 FlPlane3)))
(define (flrect3-inside-planes bb)
  (call/flrect3-values bb
    (λ (xmin ymin zmin xmax ymax zmax)
      (values (unsafe-flplane3 +1.0 0.0 0.0 (- xmin))
              (unsafe-flplane3 -1.0 0.0 0.0 xmax)
              (unsafe-flplane3 0.0 +1.0 0.0 (- ymin))
              (unsafe-flplane3 0.0 -1.0 0.0 ymax)
              (unsafe-flplane3 0.0 0.0 +1.0 (- zmin))
              (unsafe-flplane3 0.0 0.0 -1.0 zmax)))))

(: flrect3-center (-> FlRect3 FlV3))
(define (flrect3-center bb)
  (call/flrect3-values bb
    (λ (xmin ymin zmin xmax ymax zmax)
      (flv3 (* 0.5 (+ xmin xmax))
            (* 0.5 (+ ymin ymax))
            (* 0.5 (+ zmin zmax))))))

(: flrect3-volume (-> FlRect3 Flonum))
(define (flrect3-volume bb)
  (call/flrect3-values bb
    (λ (xmin ymin zmin xmax ymax zmax)
      (* (- xmax xmin)
         (- ymax ymin)
         (- zmax zmin)))))

(: flrect3-corners (-> FlRect3 (Values FlV3 FlV3 FlV3 FlV3 FlV3 FlV3 FlV3 FlV3)))
(define (flrect3-corners bb)
  (call/flrect3-values bb
    (λ (xmin ymin zmin xmax ymax zmax)
      (values (flv3 xmin ymin zmin)
              (flv3 xmin ymin zmax)
              (flv3 xmin ymax zmin)
              (flv3 xmin ymax zmax)
              (flv3 xmax ymin zmin)
              (flv3 xmax ymin zmax)
              (flv3 xmax ymax zmin)
              (flv3 xmax ymax zmax)))))

(: flrect3-plane-side (-> FlRect3 FlPlane3 Rect-Plane-Sides))
(define (flrect3-plane-side bb p)
  (call/flrect3-values bb
    (λ (x1 y1 z1 x2 y2 z2)
      (call/flplane3-values p
        (λ (nx ny nz dist)
          (let-values ([(x1 x2)  (if (>= nx 0.0) (values x1 x2) (values x2 x1))]
                       [(y1 y2)  (if (>= ny 0.0) (values y1 y2) (values y2 y1))]
                       [(z1 z2)  (if (>= nz 0.0) (values z1 z2) (values z2 z1))])
            (define d1 (+ (* nx x1) (* ny y1) (* nz z1) dist))
            (cond
              [(>= d1 0.0)  'pos]
              [else
               (define d2 (+ (* nx x2) (* ny y2) (* nz z2) dist))
               (cond
                 [(<= d2 0.0)  'neg]
                 [else  'both])])))))))

(: flrect3-classify/planes (-> FlRect3 (Listof FlPlane3) (U 'inside 'outside 'both)))
(define (flrect3-classify/planes b planes)
  (let loop ([planes planes])
    (cond [(empty? planes)  'inside]
          [else
           (define side (flrect3-plane-side b (first planes)))
           (cond [(eq? side 'neg)  'outside]
                 [(or (eq? 'poszero side) (eq? 'pos side) (eq? 'zero side))  (loop (rest planes))]
                 [else  'both])])))

(: flrect3-contains-point? (-> FlRect3 FlV3 Boolean))
(define (flrect3-contains-point? bb v)
  (call/flrect3-values bb
    (λ (xmin ymin zmin xmax ymax zmax)
      (call/flv3-values v
        (λ (x y z)
          (and (<= xmin x xmax)
               (<= ymin y ymax)
               (<= zmin z zmax)))))))

(: flrect3-contains-rect? (-> FlRect3 FlRect3 Boolean))
(define (flrect3-contains-rect? bb1 bb2)
  (call/flrect3-values bb1
    (λ (xmin1 ymin1 zmin1 xmax1 ymax1 zmax1)
      (call/flrect3-values bb2
        (λ (xmin2 ymin2 zmin2 xmax2 ymax2 zmax2)
          (and (<= xmin1 xmin2) (<= xmax2 xmax1)
               (<= ymin1 ymin2) (<= ymax2 ymax1)
               (<= zmin1 zmin2) (<= zmax2 zmax1)))))))

(: flrect3-disjoint? (-> FlRect3 FlRect3 Boolean))
(define (flrect3-disjoint? bb1 bb2)
  (call/flrect3-values bb1
    (λ (xmin1 ymin1 zmin1 xmax1 ymax1 zmax1)
      (call/flrect3-values bb2
        (λ (xmin2 ymin2 zmin2 xmax2 ymax2 zmax2)
          (or (< (min xmax1 xmax2) (max xmin1 xmin2))
              (< (min ymax1 ymax2) (max ymin1 ymin2))
              (< (min zmax1 zmax2) (max zmin1 zmin2))))))))

(: flrect3-longest-axis/center (-> FlRect3 (Values Index Flonum)))
(define (flrect3-longest-axis/center bb)
  (call/flrect3-values bb
    (λ (xmin ymin zmin xmax ymax zmax)
      (define xsize (- xmax xmin))
      (define ysize (- ymax ymin))
      (define zsize (- zmax zmin))
      (cond
        [(and (flrational? xsize) (>= xsize (max ysize zsize)))  (values 0 (* 0.5 (+ xmin xmax)))]
        [(and (flrational? ysize) (>= ysize (max xsize zsize)))  (values 1 (* 0.5 (+ ymin ymax)))]
        [(flrational? zsize)  (values 2 (* 0.5 (+ zmin zmax)))]
        [else  (values 0 0.0)]))))

(: flrect3-transform (-> FlRect3 FlAffine3 FlRect3))
;; About 7x faster than transforming the corners
(define (flrect3-transform bb t)
  (cond
    [(identity-flaffine3? t)  bb]
    [else
     (call/flrect3-values bb
       (λ (xmin ymin zmin xmax ymax zmax)
         (cond
           [(or (= -inf.0 (min xmin ymin zmin xmax ymax zmax))
                (= +inf.0 (max xmin ymin zmin xmax ymax zmax)))
            inf-flrect3]
           [else
            (call/flaffine3-forward t
              (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
                (define new-xmin
                  (+ (min (* m00 xmin) (* m00 xmax))
                     (min (* m01 ymin) (* m01 ymax))
                     (min (* m02 zmin) (* m02 zmax))
                     m03))
                (define new-xmax
                  (+ (max (* m00 xmin) (* m00 xmax))
                     (max (* m01 ymin) (* m01 ymax))
                     (max (* m02 zmin) (* m02 zmax))
                     m03))
                (define new-ymin
                  (+ (min (* m10 xmin) (* m10 xmax))
                     (min (* m11 ymin) (* m11 ymax))
                     (min (* m12 zmin) (* m12 zmax))
                     m13))
                (define new-ymax
                  (+ (max (* m10 xmin) (* m10 xmax))
                     (max (* m11 ymin) (* m11 ymax))
                     (max (* m12 zmin) (* m12 zmax))
                     m13))
                (define new-zmin
                  (+ (min (* m20 xmin) (* m20 xmax))
                     (min (* m21 ymin) (* m21 ymax))
                     (min (* m22 zmin) (* m22 zmax))
                     m23))
                (define new-zmax
                  (+ (max (* m20 xmin) (* m20 xmax))
                     (max (* m21 ymin) (* m21 ymax))
                     (max (* m22 zmin) (* m22 zmax))
                     m23))
                (FlRect3 (flv3 new-xmin new-ymin new-zmin)
                         (flv3 new-xmax new-ymax new-zmax))))])))]))

(: flrect3-transform/badness (-> FlRect3 FlAffine3 (Values FlRect3 Nonnegative-Flonum)))
(define (flrect3-transform/badness bb t)
  (cond
    [(identity-flaffine3? t)  (values bb 0.0)]
    [else
     (call/flrect3-values bb
       (λ (xmin ymin zmin xmax ymax zmax)
         (cond
           [(or (= -inf.0 (min xmin ymin zmin xmax ymax zmax))
                (= +inf.0 (max xmin ymin zmin xmax ymax zmax)))
            (values inf-flrect3 (if (equal? bb inf-flrect3) 0.0 +inf.0))]
           [else
            (call/flaffine3-forward t
              (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
                (define new-xmin
                  (+ (min (* m00 xmin) (* m00 xmax))
                     (min (* m01 ymin) (* m01 ymax))
                     (min (* m02 zmin) (* m02 zmax))
                     m03))
                (define new-xmax
                  (+ (max (* m00 xmin) (* m00 xmax))
                     (max (* m01 ymin) (* m01 ymax))
                     (max (* m02 zmin) (* m02 zmax))
                     m03))
                (define new-ymin
                  (+ (min (* m10 xmin) (* m10 xmax))
                     (min (* m11 ymin) (* m11 ymax))
                     (min (* m12 zmin) (* m12 zmax))
                     m13))
                (define new-ymax
                  (+ (max (* m10 xmin) (* m10 xmax))
                     (max (* m11 ymin) (* m11 ymax))
                     (max (* m12 zmin) (* m12 zmax))
                     m13))
                (define new-zmin
                  (+ (min (* m20 xmin) (* m20 xmax))
                     (min (* m21 ymin) (* m21 ymax))
                     (min (* m22 zmin) (* m22 zmax))
                     m23))
                (define new-zmax
                  (+ (max (* m20 xmin) (* m20 xmax))
                     (max (* m21 ymin) (* m21 ymax))
                     (max (* m22 zmin) (* m22 zmax))
                     m23))
                (define dx (- xmax xmin))
                (define dy (- ymax ymin))
                (define dz (- zmax zmin))
                (define oct-xmin (min (* dx m00) (* dy m01) (* dz m02)))
                (define oct-xmax (max (* dx m00) (* dy m01) (* dz m02)))
                (define oct-ymin (min (* dx m10) (* dy m11) (* dz m12)))
                (define oct-ymax (max (* dx m10) (* dy m11) (* dz m12)))
                (define oct-zmin (min (* dx m20) (* dy m21) (* dz m22)))
                (define oct-zmax (max (* dx m20) (* dy m21) (* dz m22)))
                (define oct-dx (* 0.5 (- (max oct-xmax (- oct-xmin)) (min oct-xmin (- oct-xmax)))))
                (define oct-dy (* 0.5 (- (max oct-ymax (- oct-ymin)) (min oct-ymin (- oct-ymax)))))
                (define oct-dz (* 0.5 (- (max oct-zmax (- oct-zmin)) (min oct-zmin (- oct-zmax)))))
                (values (FlRect3 (flv3 new-xmin new-ymin new-zmin)
                                 (flv3 new-xmax new-ymax new-zmax))
                        (max 0.0
                             (if (> oct-dx 0.0) (/ (- (- new-xmax new-xmin) oct-dx) oct-dx) 0.0)
                             (if (> oct-dy 0.0) (/ (- (- new-ymax new-ymin) oct-dy) oct-dy) 0.0)
                             (if (> oct-dz 0.0) (/ (- (- new-zmax new-zmin) oct-dz) oct-dz) 0.0)))
                ))])))]))

(: flrect3-line-intersects (-> FlRect3 FlV3 FlV3 Flonum Flonum (Values (U #f Flonum) (U #f Flonum))))
(define (flrect3-line-intersects bb v dv min-time max-time)
  (define mn (flrect3-min bb))
  (define mx (flrect3-max bb))
  ;; Compute [tmin,tmax], the interval of times for which a point on the ray is in the box
  ;; Main idea: Compute [tmin_i,tmax_i] for each pair of parallel bounding planes, and intersect them
  (if (> min-time max-time)
      ;; Intersection starts empty: fail
      (values #f #f)
      (let loop ([i : Index  0]                    ; Current coordinate index
                 [tmin min-time] [tmax max-time])  ; Accumulator starts at [min-time,max-time]
        (cond
          [(< i 3)
           (define x (unsafe-flv3-ref v i))
           (define dx (unsafe-flv3-ref dv i))
           (define x1 (unsafe-flv3-ref mn i))
           (define x2 (unsafe-flv3-ref mx i))
           (cond [(< (abs dx) +max-subnormal.0)
                  ;; The ray is approximately parallel to slab i
                  (if (not (<= x1 x x2))
                      ;; It's outside slab i, so it can't intersect
                      (values #f #f)
                      ;; It's inside slab i, so interval i is [-inf,+inf]
                      (loop (+ i 1) tmin tmax))]
                 [else
                  ;; Compute interval [tmin_i,tmax_i] for slab i
                  (define t1 (/ (- x1 x) dx))
                  (define t2 (/ (- x2 x) dx))
                  (define tmin_i (min t1 t2))
                  (define tmax_i (max t1 t2))
                  ;; Compute [tmin,tmax] ∩ [tmin_i,tmax_i]
                  (let ([tmin  (max tmin_i tmin)]
                        [tmax  (min tmax_i tmax)])
                    (if (> tmin tmax)
                        ;; Intersection is empty: fail
                        (values #f #f)
                        ;; Intersection is nonempty, so keep going with new accumulator
                        (loop (+ i 1) tmin tmax)))])]
          [else
           (values tmin tmax)]))))

(: flrect3-closest-point (-> FlRect3 FlV3 FlV3))
(define (flrect3-closest-point bb v)
  (call/flrect3-values bb
    (λ (xmin ymin zmin xmax ymax zmax)
      (call/flv3-values v
        (λ (x y z)
          (define dx0 (- x xmin))
          (define dx1 (- xmax x))
          (define dy0 (- y ymin))
          (define dy1 (- ymax y))
          (define dz0 (- z zmin))
          (define dz1 (- zmax z))
          (define d (min dx0 dx1 dy0 dy1 dz0 dz1))
          (if (<= d 0.0)
              (flv3 (flclamp x xmin xmax)
                    (flclamp y ymin ymax)
                    (flclamp z zmin zmax))
              (flv3 (if (= d dx0) xmin (if (= d dx1) xmax x))
                    (if (= d dy0) ymin (if (= d dy1) ymax y))
                    (if (= d dz0) zmin (if (= d dz1) zmax z)))))))))

(: flrect3-point-normals (-> FlRect3 FlV3 (Listof FlV3)))
(define (flrect3-point-normals bb v)
  (call/flrect3-values bb
    (λ (xmin ymin zmin xmax ymax zmax)
      (call/flv3-values v
        (λ (x y z)
          (let* ([ns  empty]
                 [ns  (if (= xmin x) (cons -x-flv3 ns) ns)]
                 [ns  (if (= xmax x) (cons +x-flv3 ns) ns)]
                 [ns  (if (= ymin y) (cons -y-flv3 ns) ns)]
                 [ns  (if (= ymax y) (cons +y-flv3 ns) ns)]
                 [ns  (if (= zmin z) (cons -z-flv3 ns) ns)]
                 [ns  (if (= zmax z) (cons +z-flv3 ns) ns)])
            ns))))))
