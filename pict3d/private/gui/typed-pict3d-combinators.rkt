#lang typed/racket/base

(require (except-in racket/list flatten)
         racket/match
         racket/promise
         math/flonum
         math/base
         "../math.rkt"
         "../engine.rkt"
         "../utils.rkt"
         "typed-user-types.rkt"
         "pict3d-struct.rkt"
         "parameters.rkt"
         )

(provide
 ;; Types
 Pict3Ds
 ;; Parameters
 default-color
 default-emitted
 default-material
 current-color
 current-emitted
 current-material
 set-color
 set-emitted
 set-material
 ;; Naming, mapping, finding
 group
 basis
 group-tag
 group-contents
 replace-group
 replace-in-group
 ungroup
 remove-group
 remove-in-group
 map-group
 map-group/transform
 ;; Basic shapes
 triangle
 quad
 rectangle
 cube
 ellipsoid
 sphere
 sunlight
 light
 freeze
 ;; Transformations
 transform
 scale-x
 scale-y
 scale-z
 scale
 rotate-x
 rotate-y
 rotate-z
 rotate
 move-x
 move-y
 move-z
 move
 relocate
 local-transform
 rotate-x/center
 rotate-y/center
 rotate-z/center
 rotate/center
 scale-x/center
 scale-y/center
 scale-z/center
 scale/center
 set-origin
 point-at
 ;; Information
 camera-transform
 bounding-rectangle
 center
 ;; Combining scenes
 combine
 pin
 weld
 ;; Testing
 plane-cull
 rect-cull
 frustum-cull
 ;; Other shapes
 arrow
 cylinder
 cone
 ;; Collision detection
 trace
 trace/normal
 trace/data
 surface
 surface/normal
 surface/data
 ;; Camera/view
 canvas-projection
 bitmap-projection
 camera->view
 camera-ray
 )

;; ===================================================================================================
;; Parameters

(define default-color (rgba 1.0 1.0 1.0 1.0))
(define default-emitted (emitted 0.0 0.0 0.0 0.0))
(define default-material (make-material 0.05 0.6 0.35 0.3))

(: current-color (Parameterof RGBA))
(define current-color (make-parameter default-color))

(: current-emitted (Parameterof Emitted))
(define current-emitted (make-parameter default-emitted))

(: current-material (Parameterof Material))
(define current-material (make-parameter default-material))

(: set-color (-> Pict3D RGBA Pict3D))
(define (set-color p c)
  (pict3d (scene-map-shapes (pict3d-scene p) (λ (a) (shape-set-color a c)))))

(: set-emitted (-> Pict3D Emitted Pict3D))
(define (set-emitted p e)
  (pict3d (scene-map-shapes (pict3d-scene p) (λ (a) (shape-set-emitted a e)))))

(: set-material (-> Pict3D Material Pict3D))
(define (set-material p m)
  (pict3d (scene-map-shapes (pict3d-scene p) (λ (a) (shape-set-material a m)))))

;; ===================================================================================================
;; Union, naming, mapping, finding

(define-type Pict3Ds (U Pict3D (Listof Pict3Ds)))

(require/typed
 racket/list
 [flatten  (-> (Listof Pict3Ds) (Listof Pict3D))])

(: combine (-> Pict3Ds * Pict3D))
(define (combine . ps)
  (pict3d (scene-union* (map pict3d-scene (flatten ps)))))

(: group (-> Pict3D (U #f Tag) Pict3D))
(define (group p n)
  (if n (pict3d (make-group-scene (pict3d-scene p) n)) p))

(: basis (-> Tag Affine Pict3D))
(define (basis n t)
  (pict3d (make-trans-scene (make-group-scene empty-scene n) t)))

(: group-tag (-> Pict3D (U #f Tag)))
(define (group-tag p)
  (define s (pict3d-scene p))
  (if (group-scene? s)
      (group-scene-tag s)
      #f))

(: group-contents (-> Pict3D Pict3D))
(define (group-contents p)
  (define s (pict3d-scene p))
  (if (group-scene? s)
      (pict3d (group-scene-scene s))
      p))

(: replace-group (-> Pict3D (Listof Tag) (-> Pict3D Pict3D) Pict3D))
(define (replace-group p n f)
  (if (empty? n)
      (f p)
      (pict3d (scene-replace-group (pict3d-scene p) n (λ (s) (pict3d-scene (f (pict3d s))))))))

(: replace-in-group (-> Pict3D (Listof Tag) (-> Pict3D Pict3D) Pict3D))
(define (replace-in-group p n f)
  (replace-group p n (λ (p) (group (f (group-contents p)) (group-tag p)))))

(: ungroup (-> Pict3D (Listof Tag) Pict3D))
(define (ungroup p n)
  (replace-group p n group-contents))

(: remove-group (-> Pict3D (Listof Tag) Pict3D))
(define (remove-group p n)
  (replace-group p n (λ (_) empty-pict3d)))

(: remove-in-group (-> Pict3D (Listof Tag) Pict3D))
(define (remove-in-group p n)
  (replace-in-group p n (λ (_) empty-pict3d)))

(: map-group (All (A) (-> Pict3D (Listof Tag) (-> Pict3D A) (Listof A))))
(define (map-group p n f)
  (if (empty? n)
      (list (f p))
      (scene-map-group (pict3d-scene p) n (λ ([s : group-scene]) (f (pict3d s))))))

(: map-group/transform (All (A) (-> Pict3D (Listof Tag) (-> Affine Pict3D A) (Listof A))))
(define (map-group/transform p n f)
  (if (empty? n)
      (list (f identity-affine p))
      (scene-map-group/transform (pict3d-scene p) n (λ ([t : FlAffine3] [s : group-scene])
                                                      (f (flaffine3->affine t) (pict3d s))))))

;; ===================================================================================================
;; Information

(: camera-transform (-> Pict3D (U #f Affine)))
(define (camera-transform p)
  (define ts (scene-map-group/transform (pict3d-scene p) '(camera) (λ ([t : FlAffine3] _) t)))
  (and (pair? ts) (flaffine3->affine (first ts))))

(: bounding-rectangle (-> Pict3D (Values (U #f Pos) (U #f Pos))))
(define (bounding-rectangle p)
  (define r (maybe-bbox-rect (scene-visible-bbox/badness (pict3d-scene p) tight-badness)))
  (if (flrect3? r)
      (values (call/flv3-values (flrect3-min r) pos)
              (call/flv3-values (flrect3-max r) pos))
      (values #f #f)))

(: center (-> Pict3D (U #f Pos)))
(define (center p)
  (define-values (v1 v2) (bounding-rectangle p))
  (if (and v1 v2) (pos-between v1 v2 0.5) #f))

;; ===================================================================================================
;; Basic shapes

(: interpret-vertex (-> (U Pos Vertex) FlV4 FlV4 Material
                        (Values FlV3 (U #f FlV3) FlV4 FlV4 Material)))
(define (interpret-vertex vert cc ce cm)
  (cond [(pos? vert)
         (values vert #f cc ce cm)]
        [else
         (match-define (Vertex v n c e m) vert)
         (values v n (if c c cc) (if e e ce) (if m m cm))]))

;; ---------------------------------------------------------------------------------------------------
;; Triangle

(: triangle (->* [(U Pos Vertex) (U Pos Vertex) (U Pos Vertex)] [#:back? Any] Pict3D))
(define (triangle vert1 vert2 vert3 #:back? [back? #f])
  (define cc (current-color))
  (define ce (current-emitted))
  (define cm (current-material))
  (define-values (v1 n1 c1 e1 m1) (interpret-vertex vert1 cc ce cm))
  (define-values (v2 n2 c2 e2 m2) (interpret-vertex vert2 cc ce cm))
  (define-values (v3 n3 c3 e3 m3) (interpret-vertex vert3 cc ce cm))
  (define norm (flv3triangle-normal v1 v2 v3))
  (cond [norm
         (pict3d
          (make-triangle-shape
           (vtx v1 (if n1 n1 norm) c1 e1 m1)
           (vtx v2 (if n2 n2 norm) c2 e2 m2)
           (vtx v3 (if n3 n3 norm) c3 e3 m3)
           (and back? #t)))]
        [else  empty-pict3d]))

;; ---------------------------------------------------------------------------------------------------
;; Quad

(: quad (->* [(U Pos Vertex) (U Pos Vertex) (U Pos Vertex) (U Pos Vertex)] [#:back? Any] Pict3D))
(define (quad vert1 vert2 vert3 vert4 #:back? [back? #f])
  (define cc (current-color))
  (define ce (current-emitted))
  (define cm (current-material))
  (define-values (v1 n1 c1 e1 m1) (interpret-vertex vert1 cc ce cm))
  (define-values (v2 n2 c2 e2 m2) (interpret-vertex vert2 cc ce cm))
  (define-values (v3 n3 c3 e3 m3) (interpret-vertex vert3 cc ce cm))
  (define-values (v4 n4 c4 e4 m4) (interpret-vertex vert4 cc ce cm))
  (define norm (flv3polygon-normal (vector v1 v2 v3 v3)))
  (cond [norm
         (define as
           (make-quad-shapes
            (vtx v1 (if n1 n1 norm) c1 e1 m1)
            (vtx v2 (if n2 n2 norm) c2 e2 m2)
            (vtx v3 (if n3 n3 norm) c3 e3 m3)
            (vtx v4 (if n4 n4 norm) c4 e4 m4)
            (and back? #t)))
         (pict3d (scene-union (first as) (second as)))]
        [else  empty-pict3d]))

;; ---------------------------------------------------------------------------------------------------
;; Rectangle

(: rectangle (->* [Pos (U Pos Dir Real)] [#:inside? Any] Pict3D))
(define (rectangle v1 v2 #:inside? [inside? #f])
  (define b
    (cond [(pos? v2)  (flrect3 v1 v2)]
          [else
           (define r
             (cond [(real? v2)  (define r (fl v2))
                                (flv3 r r r)]
                   [else  v2]))
           (let ([mn : FlV3  (flv3- v1 r)]
                 [mx : FlV3  (flv3+ v1 r)])
             (flrect3 mn mx))]))
  (let ([b : FlRect3  b])
    (pict3d
     (make-rectangle-shape b
                           (current-color)
                           (current-emitted)
                           (current-material)
                           (and inside? #t)))))

(: cube (->* [Pos Real] [#:inside? Any] Pict3D))
(define cube rectangle)

;; ---------------------------------------------------------------------------------------------------
;; Ellipsoid

(: standard-transform (-> Pos (U Pos Dir Real) FlAffine3))
(define (standard-transform v1 v2)
  (cond [(pos? v2)
         (let* ([dv : FlV3  (flv3- v2 v1)]
                [dv : FlV3  (flv3* dv 0.5)]
                [t1 : FlAffine3  (scale-flt3 dv)]
                [dv : FlV3  (flv3+ v1 v2)]
                [dv : FlV3  (flv3* dv 0.5)]
                [t2 : FlAffine3  (translate-flt3 dv)])
           (flt3compose t2 t1))]
        [else
         (define r
           (cond [(real? v2)  (define r (fl v2))
                              (flv3 r r r)]
                 [else  v2]))
         (let* ([t1 : FlAffine3  (scale-flt3 r)]
                [t2 : FlAffine3  (translate-flt3 v1)])
           (flt3compose t2 t1))]))

(: ellipsoid (->* [Pos (U Pos Dir Real)] [#:inside? Any] Pict3D))
(define (ellipsoid v1 v2 #:inside? [inside? #f])
  (let ([t : FlAffine3  (standard-transform v1 v2)])
    (pict3d
     (make-sphere-shape t
                        (current-color)
                        (current-emitted)
                        (current-material)
                        (and inside? #t)))))

(: sphere (->* [Pos Real] [#:inside? Any] Pict3D))
(define sphere ellipsoid)

;; ---------------------------------------------------------------------------------------------------
;; Directional light

(: sunlight (->* [Dir] [Emitted] Pict3D))
(define (sunlight dv [e  (emitted 1.0 1.0 1.0 1.0)])
  (let ([dv : (U #f FlV3)  (flv3normalize dv)])
    (if dv
        (pict3d (make-directional-light-shape e dv))
        empty-pict3d)))

;; ---------------------------------------------------------------------------------------------------
;; Point light

(: light (->* [Pos] [Emitted #:min-radius Real #:max-radius Real] Pict3D))
(define (light v
               [e  (emitted 1.0 1.0 1.0 1.0)]
               #:min-radius [r0 0.0]
               #:max-radius [r1 (flsqrt (* 20.0 (emitted-intensity e)))])
  (let ([r0  (abs (fl r0))]
        [r1  (abs (fl r1))])
    (if (< r0 r1)
        (let ([t : FlAffine3  (translate-flt3 v)])
          (pict3d (make-point-light-shape e t r0 r1)))
        empty-pict3d)))

;; ---------------------------------------------------------------------------------------------------
;; Efficiency

(: freeze (-> Pict3D Pict3D))
(define (freeze p)
  (define s (pict3d-scene p))
  (cond [(empty-scene? s)  p]
        [else
         (combine
          (pict3d (make-frozen-scene-shape s))
          (for/list : (Listof Pict3D) ([nt  (in-list (scene-group-transforms s 'empty))])
            (basis (car nt) (flaffine3->affine (cdr nt)))))]))

;; ===================================================================================================
;; Transformations

(: transform (-> Pict3D Affine Pict3D))
(define (transform s t)
  (pict3d (make-trans-scene (pict3d-scene s) t)))

(: make-transformer (All (A) (-> (-> A FlAffine3)
                                 (case-> (-> A Affine)
                                         (-> Pict3D A Pict3D)))))
(define (make-transformer f)
  (case-lambda
    [(v)  (flaffine3->affine (f v))]
    [(p v)  (transform p (flaffine3->affine (f v)))]))

(: set-origin (-> Pict3D (Listof Tag) Pict3D))
(define (set-origin p n)
  (: fail (-> Index Nothing))
  (define (fail m)
    (error 'set-origin "expected one group ~e; given a Pict3D with ~a groups tagged ~e" n m n))
  (define ts (map-group/transform p n (λ ([t : Affine] _) t)))
  (cond [(empty? ts)  (fail 0)]
        [(empty? (rest ts))  (transform p (affine-inverse (first ts)))]
        [else  (fail (length ts))]))

;; ---------------------------------------------------------------------------------------------------
;; Scale

(: check-scale (-> Symbol (U Flonum FlV3) FlV3))
(define (check-scale name v)
  (cond [(flonum? v)
         (cond [(= v 0.0)  (raise-argument-error name "nonzero scale" v)]
               [else  (flv3 v v v)])]
        [else
         (call/flv3-values v
           (λ (x y z)
             (cond [(or (= x 0.0) (= y 0.0) (= z 0.0))
                    (raise-argument-error name "nonzero scale" v)]
                   [else  v])))]))

(define scale-x
  (make-transformer (λ ([v : Real])
                      (let* ([dv : FlV3  (flv3 (fl v) 1.0 1.0)]
                             [dv : FlV3  (check-scale 'scale-x dv)])
                        (scale-flt3 dv)))))

(define scale-y
  (make-transformer (λ ([v : Real])
                      (let* ([dv : FlV3  (flv3 1.0 (fl v) 1.0)]
                             [dv : FlV3  (check-scale 'scale-y dv)])
                        (scale-flt3 dv)))))

(define scale-z
  (make-transformer (λ ([v : Real])
                      (let* ([dv : FlV3  (flv3 1.0 1.0 (fl v))]
                             [dv : FlV3  (check-scale 'scale-z dv)])
                        (scale-flt3 dv)))))

(define scale
  (make-transformer (λ ([v : (U Real Dir)])
                      (let ([v  (if (real? v) (fl v) v)])
                        (let ([dv : FlV3  (check-scale 'scale v)])
                          (scale-flt3 dv))))))

;; ---------------------------------------------------------------------------------------------------
;; Translate

(define move-x
  (make-transformer (λ ([v : Real])
                      (let ([v : FlV3  (flv3 (fl v) 0.0 0.0)])
                        (translate-flt3 v)))))

(define move-y
  (make-transformer (λ ([v : Real])
                      (let ([v : FlV3  (flv3 0.0 (fl v) 0.0)])
                        (translate-flt3 v)))))

(define move-z
  (make-transformer (λ ([v : Real])
                      (let ([v : FlV3  (flv3 0.0 0.0 (fl v))])
                        (translate-flt3 v)))))

(define move
  (make-transformer (λ ([v : Dir]) (translate-flt3 v))))

;; ---------------------------------------------------------------------------------------------------
;; Rotate

(: check-axis (-> Symbol Dir FlV3))
(define (check-axis name orig-v)
  (define v (flv3normalize orig-v))
  (if v v (raise-argument-error name "nonzero axis vector" v)))

(define rotate-x
  (make-transformer (λ ([a : Real])
                      (rotate-flt3 +x-flv3 (degrees->radians (fl a))))))

(define rotate-y
  (make-transformer (λ ([a : Real])
                      (rotate-flt3 +y-flv3 (degrees->radians (fl a))))))

(define rotate-z
  (make-transformer (λ ([a : Real])
                      (rotate-flt3 +z-flv3 (degrees->radians (fl a))))))

(: rotate (case-> (-> Dir Real Affine)
                  (-> Pict3D Dir Real Pict3D)))
(define rotate
  (case-lambda
    [(v a)
     (let ([v : FlV3  (check-axis 'rotate v)])
       (flaffine3->affine (rotate-flt3 v (degrees->radians (fl a)))))]
    [(p v a)
     (transform p (rotate v a))]))

;; ---------------------------------------------------------------------------------------------------
;; Change of basis

(: relocate (case-> (-> Affine Affine Affine)
                    (-> Pict3D Affine Affine Pict3D)))
(define relocate
  (case-lambda
    [(t1 t2)  (affine-compose t2 (affine-inverse t1))]
    [(pict t1 t2)  (transform pict (relocate t1 t2))]))

(: local-transform (case-> (-> Affine Affine Affine)
                           (-> Pict3D Affine Affine Pict3D)))
(define local-transform
  (case-lambda
    [(t local-t)  (affine-compose local-t (relocate local-t t))]
    [(pict t local-t)  (transform pict (local-transform t local-t))]))

;; ---------------------------------------------------------------------------------------------------
;; Local scaling

(: center-or-origin (-> Pict3D Pos))
(define (center-or-origin pict)
  (define v (center pict))
  (if v v origin))

(: scale/center (->* [Pict3D (U Real Dir)] [Pos] Pict3D))
(define (scale/center pict dv [v (center-or-origin pict)])
  (local-transform pict (scale dv) (move (pos- v origin))))

(: scale-x/center (->* [Pict3D Real] [Pos] Pict3D))
(define (scale-x/center pict dx [v (center-or-origin pict)]) (scale/center pict (dir dx 1 1) v))

(: scale-y/center (->* [Pict3D Real] [Pos] Pict3D))
(define (scale-y/center pict dy [v (center-or-origin pict)]) (scale/center pict (dir 1 dy 1) v))

(: scale-z/center (->* [Pict3D Real] [Pos] Pict3D))
(define (scale-z/center pict dz [v (center-or-origin pict)]) (scale/center pict (dir 1 1 dz) v))

;; ---------------------------------------------------------------------------------------------------
;; Local rotation

(: rotate/center (->* [Pict3D Dir Real] [Pos] Pict3D))
(define (rotate/center pict axis angle [v (center-or-origin pict)])
  (local-transform pict (rotate axis angle) (move (pos- v origin))))

(: rotate-x/center (->* [Pict3D Real] [Pos] Pict3D))
(define (rotate-x/center pict angle [v (center-or-origin pict)]) (rotate/center pict +x angle v))

(: rotate-y/center (->* [Pict3D Real] [Pos] Pict3D))
(define (rotate-y/center pict angle [v (center-or-origin pict)]) (rotate/center pict +y angle v))

(: rotate-z/center (-> Pict3D Real Pict3D))
(define (rotate-z/center pict angle [v (center-or-origin pict)]) (rotate/center pict +z angle v))

;; ---------------------------------------------------------------------------------------------------
;; Point at/to

(: point-at (->* [Pos (U Pos Dir)] [#:angle Real #:up Dir #:normalize? Any] Affine))
(define (point-at from to 
                  #:angle [angle 0.0]
                  #:up [up +z]
                  #:normalize? [normalize? #t])
  (define z-axis (if (dir? to) to (pos- to from)))
  (if (= (dir-dist z-axis) 0.0)
      (raise-argument-error 'point-at "nonzero look direction" 1 from to)
      (flaffine3->affine
       (point-at-flt3 from z-axis (degrees->radians (fl angle)) up (and normalize? #t)))))

;; ===================================================================================================
;; Combining scenes

(: pin (->* [Pict3D (Listof Tag) Pict3D] [(Listof Tag)] Pict3D))
(define (pin p1 n1 p2 [n2 empty])
  (let ([p2  (ungroup (set-origin p2 n2) n2)])
    (replace-in-group p1 n1 (λ ([p : Pict3D]) (combine p p2)))))

(: weld (->* [Pict3D (Listof Tag) Pict3D] [(Listof Tag)] Pict3D))
(define (weld p1 n1 p2 [n2 empty])
  (let ([p2  (ungroup (set-origin p2 n2) n2)])
    (replace-group p1 n1 (λ ([p : Pict3D]) (combine (group-contents p) p2)))))

;; ===================================================================================================
;; Testing combinators

(: plane-cull (-> Pict3D FlPlane3 Pict3D))
(define (plane-cull s p)
  (pict3d (scene-plane-cull (pict3d-scene s) p)))

(: rect-cull (-> Pict3D FlRect3 Pict3D))
(define (rect-cull s b)
  (pict3d (scene-rect-cull (pict3d-scene s) b)))

(: frustum-cull (-> Pict3D FlTransform3 Pict3D))
(define (frustum-cull s t)
  (pict3d (scene-frustum-cull (pict3d-scene s) t)))

;; ===================================================================================================
;; Other shapes

;; ---------------------------------------------------------------------------------------------------
;; Arrows

(define (up-arrow)
  (freeze
   (combine
    (rectangle (pos -1/64 -1/64 0)
               (pos 1/64 1/64 56/64))
    (let ([p  (triangle (pos 2/64 2/64 56/64)
                        (pos -2/64 2/64 56/64)
                        (pos 0 0 1))])
      (combine p (rotate-z p 90) (rotate-z p 180) (rotate-z p 270)))
    (quad (pos 2/64 2/64 56/64)
          (pos 2/64 -2/64 56/64)
          (pos -2/64 -2/64 56/64)
          (pos -2/64 2/64 56/64)))))

(: arrow (->* [Pos (U Pos Dir)] [#:normalize? Any] Pict3D))
(define (arrow from to #:normalize? [normalize? #f])
  (transform (up-arrow) (point-at from to #:normalize? normalize?)))

;; ---------------------------------------------------------------------------------------------------
;; Cylinder

(: standard-cylinder-scene (-> Boolean Natural Scene))
(define (standard-cylinder-scene inside? n)
  (define c (current-color))
  (define e (current-emitted))
  (define m (current-material))
  (scene-union*
   (for/list ([i  (in-range n)])
     (define t1 (* (* 2.0 pi) (/ (fl i) (fl n))))
     (define t2 (* (* 2.0 pi) (/ (+ (fl i) 1.0) (fl n))))
     (define x1 (flcos t1))
     (define y1 (flsin t1))
     (define x2 (flcos t2))
     (define y2 (flsin t2))
     (scene-union*
      (list
       ;; Top
       (make-triangle-shape
        (vtx (flv3 x1 y1 1.0) +z-flv3 c e m)
        (vtx (flv3 x2 y2 1.0) +z-flv3 c e m)
        (vtx +z-flv3 +z-flv3 c e m)
        inside?)
       ;; Sides
       (make-triangle-shape
        (vtx (flv3 x1 y1 -1.0) (flv3 x1 y1 0.0) c e m)
        (vtx (flv3 x2 y2 -1.0) (flv3 x2 y2 0.0) c e m)
        (vtx (flv3 x2 y2 +1.0) (flv3 x2 y2 0.0) c e m)
        inside?)
       (make-triangle-shape
        (vtx (flv3 x1 y1 -1.0) (flv3 x1 y1 0.0) c e m)
        (vtx (flv3 x2 y2 +1.0) (flv3 x2 y2 0.0) c e m)
        (vtx (flv3 x1 y1 +1.0) (flv3 x1 y1 0.0) c e m)
        inside?)
       ;; Bottom
       (make-triangle-shape
        (vtx (flv3 x2 y2 -1.0) -z-flv3 c e m)
        (vtx (flv3 x1 y1 -1.0) -z-flv3 c e m)
        (vtx -z-flv3 -z-flv3 c e m)
        inside?))))))

(: cylinder (->* [Pos (U Pos Dir Real)] [#:inside? Any #:segments Natural] Pict3D))
(define (cylinder v1 v2 #:inside? [inside? #f] #:segments [n 32])
  (freeze (pict3d (make-trans-scene (standard-cylinder-scene (and inside? #t) n)
                                    (standard-transform v1 v2)))))

;; ---------------------------------------------------------------------------------------------------
;; Cone

(: standard-cone-scene (-> Boolean Natural Boolean Scene))
(define (standard-cone-scene inside? n smooth?)
  (define nx (/ 1.0 (flsqrt (+ (sqr 1.0) (sqr 0.5)))))
  (define ny (/ 0.5 (flsqrt (+ (sqr 1.0) (sqr 0.5)))))
  (define c (current-color))
  (define e (current-emitted))
  (define m (current-material))
  (scene-union*
   (for/list ([i  (in-range n)])
     (define t0 (* (* 2.0 pi) (/ (+ (fl i) 0.5) (fl n))))
     (define t1 (* (* 2.0 pi) (/ (fl i) (fl n))))
     (define t2 (* (* 2.0 pi) (/ (+ (fl i) 1.0) (fl n))))
     (define x0 (flcos t0))
     (define y0 (flsin t0))
     (define x1 (flcos t1))
     (define y1 (flsin t1))
     (define x2 (flcos t2))
     (define y2 (flsin t2))
     (scene-union
      ;; Sides
      (make-triangle-shape
       (vtx +z-flv3 (if smooth? +z-flv3 (flv3 (* nx x0) (* nx y0) ny)) c e m)
       (vtx (flv3 x1 y1 -1.0) (flv3 (* nx x1) (* nx y1) ny) c e m)
       (vtx (flv3 x2 y2 -1.0) (flv3 (* nx x2) (* nx y2) ny) c e m)
       inside?)
      ;; Bottom
      (make-triangle-shape
       (vtx (flv3 x2 y2 -1.0) -z-flv3 c e m)
       (vtx (flv3 x1 y1 -1.0) -z-flv3 c e m)
       (vtx -z-flv3 -z-flv3 c e m)
       inside?)))))

(: cone (->* [Pos (U Pos Dir Real)] [#:inside? Any #:segments Natural #:smooth? Any] Pict3D))
(define (cone v1 v2 #:inside? [inside? #f] #:segments [n 32] #:smooth? [smooth? #f])
    (freeze (pict3d (make-trans-scene (standard-cone-scene (and inside? #t) n (and smooth? #t))
                                      (standard-transform v1 v2)))))

;; ===================================================================================================
;; Collision detection

(: trace (-> Pict3D Pos (U Pos Dir) (U #f Pos)))
(define (trace p v1 to)
  (when (= +inf.0 (flv3mag^2 v1))
    (raise-argument-error 'trace "Pos with finite squared magnitude" 1 p v1 to))
  (when (= +inf.0 (flv3mag^2 to))
    (raise-argument-error 'trace "Pos or Dir with finite squared magnitude" 2 p v1 to))
  (define-values (time data)
    (cond [(pos? to)  (scene-line-intersect (pict3d-scene p) v1 (flv3- to v1))]
          [else       (scene-ray-intersect  (pict3d-scene p) v1 to)]))
  (and time data (flv3->pos (trace-data-pos (force data)))))

(: trace/data (-> Pict3D Pos (U Pos Dir) (U #f Surface-Data)))
(define (trace/data p v1 to)
  (when (= +inf.0 (flv3mag^2 v1))
    (raise-argument-error 'trace/data "Pos with finite squared magnitude" 1 p v1 to))
  (when (= +inf.0 (flv3mag^2 to))
    (raise-argument-error 'trace/data "Pos or Dir with finite squared magnitude" 2 p v1 to))
  (define-values (time data)
    (cond [(pos? to)  (scene-line-intersect (pict3d-scene p) v1 (flv3- to v1))]
          [else       (scene-ray-intersect  (pict3d-scene p) v1 to)]))
  (and time data (trace-data->surface-data time (force data))))

(: trace/normal (-> Pict3D Pos (U Pos Dir) (Values (U #f Pos) (U #f Dir))))
(define (trace/normal p v1 to)
  (define data (trace/data p v1 to))
  (cond [data  (values (surface-data-pos data)
                       (surface-data-normal data))]
        [else  (values #f #f)]))

(: find-surface-endpoints (-> Pict3D Dir (Values (U #f Pos) (U #f Pos))))
(define (find-surface-endpoints p dv)
  (define-values (v1 v2) (bounding-rectangle p))
  (define m (dir-dist dv))
  (cond
    [(and v1 v2 (> m 0.0))
     (define v (pos-between v1 v2 0.5))
     (define r (dir-dist (pos- v2 v1)))
     (values v (pos+ v (dir-scale dv (/ r m))))]
    [else
     (values #f #f)]))

(: surface (->* [Pict3D Dir] [#:inside? Any] (U #f Pos)))
(define (surface p dv #:inside? [inside? #f])
  (define-values (vin vout) (find-surface-endpoints p dv))
  (and vin vout
       (if inside?
           (trace p vin vout)
           (trace p vout vin))))

(: surface/data (->* [Pict3D Dir] [#:inside? Any] (U #f Surface-Data)))
(define (surface/data p dv #:inside? [inside? #f])
  (define-values (vin vout) (find-surface-endpoints p dv))
  (if (and vin vout)
      (if inside?
          (trace/data p vin vout)
          (trace/data p vout vin))
      #f))

(: surface/normal (->* [Pict3D Dir] [#:inside? Any] (Values (U #f Pos) (U #f Dir))))
(define (surface/normal p dv #:inside? [inside? #f])
  (define data (surface/data  p dv #:inside? inside?))
  (cond [data  (values (surface-data-pos data)
                       (surface-data-normal data))]
        [else  (values #f #f)]))

;; ===================================================================================================
;; Camera/view

(: canvas-projection (->* [] [Integer Integer #:z-near Real #:z-far Real #:fov Real] FlTransform3))
(define (canvas-projection [width (current-pict3d-width)]
                           [height (current-pict3d-height)]
                           #:z-near [z-near (current-pict3d-z-near)]
                           #:z-far [z-far (current-pict3d-z-far)]
                           #:fov [fov (current-pict3d-fov)])
  (let ([width   (fl (max 1 width))]
        [height  (fl (max 1 height))]
        [z-near  (max default-pict3d-z-near (min default-pict3d-z-far (fl z-near)))]
        [z-far   (max default-pict3d-z-near (min default-pict3d-z-far (fl z-far)))]
        [fov     (max 1.0 (min 179.0 (fl fov)))])
    (perspective-flt3/viewport width height (degrees->radians fov) z-near z-far)))

(: bitmap-projection (->* [] [Integer Integer #:z-near Real #:z-far Real #:fov Real] FlTransform3))
;; Like canvas projection but upside-down because OpenGL origin is lower-left
(define (bitmap-projection [width (current-pict3d-width)]
                           [height (current-pict3d-height)]
                           #:z-near [z-near (current-pict3d-z-near)]
                           #:z-far [z-far (current-pict3d-z-far)]
                           #:fov [fov (current-pict3d-fov)])
  (flt3compose (scale-flt3 +x-y+z-flv3)
               (canvas-projection width height #:z-near z-near #:z-far z-far #:fov fov)))

(: camera->view (-> Affine Affine))
;; Inverts a camera basis to get a view transform; also negates the y and z axes
;; In OpenGL, +z is toward the viewer and +y is up
;; In Pict3D, +z is away from the viewer and +y is down (like typical bitmap coordinates)
(define (camera->view t)
  (flaffine3->affine
   (flt3compose (scale-flt3 +x-y-z-flv3)
                (flt3inverse t))))

(: camera-ray (->* [Affine Real Real]
                   [Integer Integer #:z-near Real #:z-far Real #:fov Real]
                   (Values (U #f Pos) (U #f Dir))))
(define (camera-ray t x y
                    [width (current-pict3d-width)]
                    [height (current-pict3d-height)]
                    #:z-near [z-near (current-pict3d-z-near)]
                    #:z-far [z-far (current-pict3d-z-far)]
                    #:fov [fov (current-pict3d-fov)])
  (define unview (flt3inverse (camera->view t)))
  (define unproj
    (flt3inverse (bitmap-projection width height #:z-near z-near #:z-far z-far #:fov fov)))
  ;; View position
  (define v (flt3apply/pos unview zero-flv3))
  ;; View direction
  (define clip-x (* (- (/ (fl x) (fl (max 1 width))) 0.5) 2.0))
  (define clip-y (* (- (/ (fl y) (fl (max 1 height))) 0.5) 2.0))
  (define dv
    (let ([dv  (flt3apply/pos unproj (flv3 clip-x clip-y 0.0))])
      (flv3normalize (flt3apply/dir unview dv))))
  (if dv
      (values (call/flv3-values v pos)
              (call/flv3-values dv dir))
      (values #f #f)))
