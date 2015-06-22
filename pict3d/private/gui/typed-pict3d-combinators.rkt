#lang typed/racket/base

(require (except-in racket/list flatten)
         (only-in racket/unsafe/ops unsafe-fx+)
         racket/fixnum
         racket/match
         racket/promise
         math/flonum
         math/base
         "../math.rkt"
         "../engine.rkt"
         "../soup.rkt"
         "../shape.rkt"
         "../utils.rkt"
         "user-types.rkt"
         (only-in "typed-user-types.rkt"
                  trace-data->surface-data
                  flv3->pos
                  flv3->dir
                  face->vertices
                  fllinear3->linear
                  flaffine3->affine
                  flafflin3->affine
                  make-material)
         "pict3d-struct.rkt"
         "parameters.rkt"
         "shape/light-grid.rkt"
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
 current-tessellate-segments
 current-tessellate-max-edge
 current-tessellate-max-angle
 current-adaptive-segments
 current-adaptive-max-edge
 current-adaptive-max-angle
 current-adaptive-max-iters
 ;; Attributes
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
 find-group-transforms
 find-group-transform
 ;; Basic shapes
 triangle
 ;triangle-mesh
 quad
 rectangle
 cube
 ellipsoid
 sphere
 ring
 pipe
 cylinder
 cone
 freeze
 freeze-in-groups
 ;; Lights
 default-light-range
 current-light-range
 light
 sunlight
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
 find-group-transforms
 find-group-transform
 point-at
 ;; Tessellation and deformation
 tessellate
 deform
 adaptive-tessellate
 adaptive-deform
 displace
 twist
 extend
 bend
 bend-smooth
 bend-pict3d
 local-deform
 ;; Other face operations
 ;replace-vertices
 ;replace-vertices/adjacent
 ;merge-normals
 ;plane-normals
 merge-vertex-normals
 plane-vertex-normals
 ;; Information
 camera-transform
 bounding-rectangle
 center
 ;; Combining scenes
 combine
 pin
 weld
 join
 glue
 pin*
 weld*
 join*
 glue*
 ;; Testing
 plane-cull
 rect-cull
 frustum-cull
 ;; Other shapes
 arrow
 ;; Collision detection
 trace
 trace/normal
 trace/data
 surface
 surface/normal
 surface/data
 ;; Camera/view
 canvas-projective
 bitmap-projective
 camera->view
 camera-ray-dir
 ;;
 light-grid
 wireframe
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

(: current-tessellate-segments (Parameterof Integer Natural))
(define current-tessellate-segments
  (make-parameter
   12
   (λ ([n : Integer])
     (if (>= n 0)
         n
         (raise-argument-error 'current-tessellate-segments "Natural" n)))))

(: current-tessellate-max-edge (Parameterof (U #f Real) (U #f Positive-Flonum)))
(define current-tessellate-max-edge
  (make-parameter
   #f
   (λ ([edge : (U #f Real)])
     (let ([edge  (and edge (fl edge))])
       (if (and edge (> edge 0.0))
           edge
           (raise-argument-error 'current-tessellate-max-edge "(U #f Positive-Real)" edge))))))

(: current-tessellate-max-angle (Parameterof Real Positive-Flonum))
(define current-tessellate-max-angle
  (make-parameter
   15.0
   (λ ([angle : Real])
     (let ([angle  (fl angle)])
       (if (> angle 0.0)
           angle
           (raise-argument-error 'current-tessellate-max-angle "Positive-Real" angle))))))

(: current-adaptive-segments (Parameterof Integer Natural))
(define current-adaptive-segments
  (make-parameter
   0
   (λ ([n : Integer])
     (if (>= n 0)
         n
         (raise-argument-error 'current-adaptive-segments "Natural" n)))))

(: current-adaptive-max-edge (Parameterof (U #f Real) (U #f Positive-Flonum)))
(define current-adaptive-max-edge
  (make-parameter
   #f
   (λ ([edge : (U #f Real)])
     (let ([edge  (and edge (fl edge))])
       (if (and edge (> edge 0.0))
           edge
           (raise-argument-error 'current-adaptive-max-edge "(U #f Positive-Real)" edge))))))

(: current-adaptive-max-angle (Parameterof Real Positive-Flonum))
(define current-adaptive-max-angle
  (make-parameter
   15.0
   (λ ([angle : Real])
     (let ([angle  (fl angle)])
       (if (> angle 0.0)
           angle
           (raise-argument-error 'current-adaptive-max-angle "Positive-Real" angle))))))

(: current-adaptive-max-iters (Parameter Integer Natural))
(define current-adaptive-max-iters
  (make-parameter
   5
   (λ ([n : Integer])
     (if (>= n 0)
         n
         (raise-argument-error 'current-adaptive-max-iters "Natural" n)))))

;; ===================================================================================================
;; Set attributes
                            
(: set-color (-> Pict3D RGBA Pict3D))
(define (set-color p c)
  (pict3d (scene-map-shapes (pict3d-scene p) (λ (a) (set-shape-color a c)))))

(: set-emitted (-> Pict3D Emitted Pict3D))
(define (set-emitted p e)
  (pict3d (scene-map-shapes (pict3d-scene p) (λ (a) (set-shape-emitted a e)))))

(: set-material (-> Pict3D Material Pict3D))
(define (set-material p m)
  (pict3d (scene-map-shapes (pict3d-scene p) (λ (a) (set-shape-material a m)))))

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
  (pict3d (make-trans-scene (make-group-scene empty-scene n)
                            (->flaffine3 t))))

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

(: find-group-transforms (-> Pict3D (Listof Tag) (Listof Affine)))
(define (find-group-transforms p n)
  (map-group/transform p n (λ ([t : Affine] _) t)))

(: find-group-transform (-> Pict3D (Listof Tag) Affine))
(define (find-group-transform p n)
  (: fail (-> Index Nothing))
  (define (fail m)
    (error 'find-group-transform "expected one group with path ~e; given a Pict3D with ~a such groups"
           n m))
  (define ts (find-group-transforms p n))
  (cond [(empty? ts)  (fail 0)]
        [(empty? (rest ts))  (first ts)]
        [else  (fail (length ts))]))

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
;; Canonicalizing arguments

(: interpret-vertex (-> (U Pos Vertex) FlV4 FlV4 Material
                        (Values FlV3 (U #f FlV3) FlV4 FlV4 Material)))
(define (interpret-vertex vert cc ce cm)
  (cond [(pos? vert)
         (values vert #f cc ce cm)]
        [else
         (match-define (Vertex v n c e m) vert)
         (values v n (if c c cc) (if e e ce) (if m m cm))]))

(: interpret-vtx (-> Vertex (Promise (U #f FlV3)) RGBA Emitted Material (U #f vtx)))
(define (interpret-vtx vert lazy-n cc ce cm)
  (define-values (p n c e m) (interpret-vertex vert cc ce cm))
  (let* ([n  (if n n (force lazy-n))]
         [n  (and n (flv3normalize n))])
    (and n (vtx p n c e m))))

(: interpret-scale (-> (U Dir Real) (Values Flonum Flonum Flonum)))
(define (interpret-scale d)
  (if (dir? d)
      (call/flv3-values d values)
      (let ([d  (fl d)])
        (values d d d))))

(: interpret-corners (-> Pos (U Pos Dir Real) (Values Pos Pos)))
(define (interpret-corners v1 v2)
  (if (pos? v2)
      (values v1 v2)
      (call/flv3-values v1
        (λ (x1 y1 z1)
          (let-values ([(dx dy dz)  (interpret-scale v2)])
            (values (pos (- x1 dx) (- y1 dy) (- z1 dz))
                    (pos (+ x1 dx) (+ y1 dy) (+ z1 dz))))))))

(: interpret-center-scale (-> Pos (U Pos Dir Real) (Values Pos Dir)))
(define (interpret-center-scale v1 v2)
  (cond
    [(pos? v2)  (call/flv3-values v1
                  (λ (x1 y1 z1)
                    (call/flv3-values v2
                      (λ (x2 y2 z2)
                        (values (pos (* 0.5 (+ x1 x2)) (* 0.5 (+ y1 y2)) (* 0.5 (+ z1 z2)))
                                (dir (* 0.5 (- x2 x1)) (* 0.5 (- y2 y1)) (* 0.5 (- z2 z1))))))))]
    [(dir? v2)  (values v1 v2)]
    [else       (let ([d  (fl v2)])
                  (values v1 (dir d d d)))]))

(: flclamp/snap (-> Flonum Flonum Flonum Flonum Flonum Flonum))
(define (flclamp/snap x mn mx mn* mx*)
  (cond [(<= x mn*)  mn]
        [(>= x mx*)  mx]
        [else  x]))

(: interpret-arc (->* [Arc] [Flonum] (Values Flonum Flonum)))
(define (interpret-arc a [eps 1e-8])
  (match-define (arc (app degrees->radians a1) (app degrees->radians a2)) a)
  (values a1 (flclamp/snap (- a2 a1) 0.0 (* 2.0 pi) eps (- (* 2.0 pi) eps))))

(: interpret-interval (->* [Interval Flonum Flonum] [Flonum] (Values Flonum Flonum)))
(define (interpret-interval x mn mx [eps 1e-8])
  (match-define (interval x1 x2) x)
  (values (flclamp/snap x1 mn mx (+ mn eps) (- mx eps))
          (flclamp/snap x2 mn mx (+ mn eps) (- mx eps))))

(: standard-transform (-> Pos (U Pos Dir Real) FlAffine3))
(define (standard-transform v1 v2)
  (if (pos? v2)
      (flt3compose (move-flt3 (flv3* (flv3+ v1 v2) 0.5))
                   (scale-flt3 (flv3* (flv3- v2 v1) 0.5)))
      (flt3compose (move-flt3 v1)
                   (scale-flt3 (if (real? v2) (fl v2) v2)))))

;; ===================================================================================================
;; Basic shapes

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
  (define norm (flv3polygon-normal v1 v2 v3))
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
  (define norm (flv3polygon-normal v1 v2 v3 v3))
  (cond [norm
         (pict3d
          (make-quad-triangle-mesh-shape  ;make-quad-shape  ;; not ready yet - splitter b0rken
           (vector (vtx v1 (if n1 n1 norm) c1 e1 m1)
                   (vtx v2 (if n2 n2 norm) c2 e2 m2)
                   (vtx v3 (if n3 n3 norm) c3 e3 m3)
                   (vtx v4 (if n4 n4 norm) c4 e4 m4))
           (and back? #t)))]
        [else  empty-pict3d]))

;; ---------------------------------------------------------------------------------------------------
;; Rectangle

(: rectangle (->* [Pos (U Pos Dir Real)] [#:inside? Any] Pict3D))
(define (rectangle v1 v2 #:inside? [inside? #f])
  (define t (standard-transform v1 v2))
  (define cc (current-color))
  (define ce (current-emitted))
  (define cm (current-material))
  (pict3d (make-rectangle-shape t cc ce cm (and inside? #t))))

(: cube (->* [Pos Real] [#:inside? Any] Pict3D))
(define cube rectangle)

;; ---------------------------------------------------------------------------------------------------
;; Ellipsoid

(: ellipsoid (->* [Pos (U Pos Dir Real)] [#:inside? Any] Pict3D))
(define (ellipsoid v1 v2 #:inside? [inside? #f])
  (define t (standard-transform v1 v2))
  (define cc (current-color))
  (define ce (current-emitted))
  (define cm (current-material))
  (pict3d (make-sphere-shape t cc ce cm (and inside? #t))))

(: sphere (->* [Pos Real] [#:inside? Any] Pict3D))
(define sphere ellipsoid)

;; ---------------------------------------------------------------------------------------------------
;; Annulus

(: ring (->* [Pos (U Pos Dir Real)] [#:radii Interval #:arc Arc #:back? Any] Pict3D))
(define (ring v1 v2 #:radii [radii unit-interval] #:arc [arc circle-arc] #:back? [back? #f])
  (let-values ([(r1 r2)  (interpret-interval radii 0.0 +inf.0)]
               [(rot a)  (interpret-arc arc)]
               [(back?)  (and back? #t)])
    (cond
      [(and (> a 0.0) (< r1 r2))
       (define t (flt3compose (standard-transform v1 v2)
                              (flt3compose (rotate-z-flt3 rot)
                                           (scale-flt3 (flv3 r2 r2 +1.0)))))
       (define cc (current-color))
       (define ce (current-emitted))
       (define cm (current-material))
       (pict3d (make-disk-shape t (abs (/ r1 r2)) 0.0 a cc ce cm back?))]
      [else
       empty-pict3d])))

;; ---------------------------------------------------------------------------------------------------
;; Pipes, cylinders and cones

(: pipe (->* [Pos (U Pos Dir Real)]
             [#:inside? Any
              #:arc Arc
              #:bottom-radii Interval
              #:top-radii Interval
              #:bottom-cap? Any
              #:top-cap? Any
              #:start-cap? Any
              #:end-cap? Any
              #:inner-wall? Any
              #:outer-wall? Any]
             Pict3D))
(define (pipe v1 v2
              #:inside? [inside? #f]
              #:arc [arc circle-arc]
              #:bottom-radii [bottom-radii (interval 0.5 1.0)]
              #:top-radii [top-radii bottom-radii]
              #:bottom-cap? [bottom-cap? #t]
              #:top-cap? [top-cap? #t]
              #:start-cap? [start-cap? #t]
              #:end-cap? [end-cap? #t]
              #:outer-wall? [outer-wall? #t]
              #:inner-wall? [inner-wall? #t])
  (let-values ([(bot1 bot2)  (interpret-interval bottom-radii 0.0 +inf.0)]
               [(top1 top2)  (interpret-interval top-radii 0.0 +inf.0)]
               [(rot a)      (interpret-arc arc)]
               [(inside?)    (and inside? #t)])
    (define t0 (flt3compose (standard-transform v1 v2)
                            (rotate-z-flt3 rot)))
    (define cc (current-color))
    (define ce (current-emitted))
    (define cm (current-material))
    
    ;; Every (abs (/ x y)) below is to keep TR satisfied the quotient is nonnegative
    
    (: maybe-add-wall (-> (Listof shape) Flonum Flonum Any Boolean (Listof shape)))
    (define (maybe-add-wall ss bot top add? inside?)
      (cond [(and add? (> a 0.0) (or (> bot 0.0) (> top 0.0)))
             (define-values (t h)
               (if (>= bot top)
                   (values (flt3compose t0 (scale-flt3 (flv3 bot bot +1.0)))
                           (abs (/ top bot)))
                   (values (flt3compose t0 (scale-flt3 (flv3 top top -1.0)))
                           (abs (/ bot top)))))
             (cons (make-cylinder-shape t h a cc ce cm inside?) ss)]
            [else  ss]))
    
    (: maybe-add-z-cap (-> (Listof shape) Flonum Flonum Flonum Any Boolean (Listof shape)))
    (define (maybe-add-z-cap ss z r1 r2 add? inside?)
      (if (and add? (> a 0.0) (< r1 r2))
          (cons (make-disk-shape (flt3compose t0 (scale-flt3 (flv3 r2 r2 1.0)))
                                 (abs (/ r1 r2))
                                 z a cc ce cm inside?) ss)
          ss))
    
    (: maybe-add-angle-cap (-> (Listof shape) Flonum Any Boolean (Listof shape)))
    (define (maybe-add-angle-cap ss angle add? inside?)
      (if (and add?
               (< a (* 2.0 pi))
               (>= bot2 0.0)
               (>= top2 0.0)
               (>= bot1 0.0)
               (>= top1 0.0)
               (or (< bot1 bot2) (< top1 top2)))
          (cons (make-cylinder-wall-shape t0 top1 top2 bot1 bot2 angle cc ce cm inside?) ss)
          ss))
    
    (let* ([ss  empty]
           [ss  (maybe-add-wall ss bot2 top2 outer-wall? inside?)]
           [ss  (maybe-add-wall ss bot1 top1 inner-wall? (not inside?))]
           [ss  (maybe-add-z-cap ss +1.0 top1 top2 top-cap? inside?)]
           [ss  (maybe-add-z-cap ss -1.0 bot1 bot2 bottom-cap? (not inside?))]
           [ss  (maybe-add-angle-cap ss 0.0 start-cap? inside?)]
           [ss  (maybe-add-angle-cap ss a end-cap? (not inside?))])
      (pict3d (scene-union* ss)))))

(: cylinder (->* [Pos (U Pos Dir Real)]
                 [#:inside? Any
                  #:arc Arc
                  #:top-cap? Any
                  #:bottom-cap? Any
                  #:start-cap? Any
                  #:end-cap? Any
                  #:outer-wall? Any]
                 Pict3D))
(define (cylinder v1 v2
                  #:inside? [inside? #f]
                  #:arc [arc circle-arc]
                  #:outer-wall? [outer-wall? #t]
                  #:top-cap? [top-cap? #t]
                  #:bottom-cap? [bottom-cap? #t]
                  #:start-cap? [start-cap? #t]
                  #:end-cap? [end-cap? #t])
  (pipe v1 v2
        #:inside? inside?
        #:bottom-radii unit-interval
        #:top-radii unit-interval
        #:arc arc
        #:top-cap? top-cap?
        #:bottom-cap? bottom-cap?
        #:start-cap? start-cap?
        #:end-cap? end-cap?
        #:outer-wall? outer-wall?))

(: cone (->* [Pos (U Pos Dir Real)]
             [#:inside? Any
              #:arc Arc
              #:bottom-cap? Any
              #:start-cap? Any
              #:end-cap? Any
              #:outer-wall? Any]
             Pict3D))
(define (cone v1 v2
              #:inside? [inside? #f]
              #:arc [arc circle-arc]
              #:outer-wall? [outer-wall? #t]
              #:bottom-cap? [bottom-cap? #t]
              #:start-cap? [start-cap? #t]
              #:end-cap? [end-cap? #t])
  (pipe v1 v2
        #:inside? inside?
        #:bottom-radii unit-interval
        #:top-radii zero-interval
        #:arc arc
        #:bottom-cap? bottom-cap?
        #:start-cap? start-cap?
        #:end-cap? end-cap?
        #:outer-wall? outer-wall?))

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

(: default-light-range (-> Emitted Real))
(define (default-light-range e)
  (flsqrt (/ (emitted-intensity e) #i1/40)))

(: current-light-range (Parameterof (-> Emitted Real)))
(define current-light-range (make-parameter default-light-range))

(: light (->* [Pos] [Emitted #:range Real #:radii Interval] Pict3D))
(define (light v [e  (emitted 1.0 1.0 1.0 1.0)]
               #:range [range  ((current-light-range) e)]
               #:radii [radii  unit-interval])
  ;; At max range, light intensity is 32-bit floating-point epsilon
  (define max-range (flsqrt (/ (emitted-intensity e) (flexpt 2.0 -23.0))))
  ;; It would take 419431 lights with this max range to create a discrepancy of at least 0.05
  ;; in the worst case
  (let*-values ([(range)  (flclamp (fl range) 0.0 max-range)]
                ;; Convert fractions of max distance to distances
                [(r1 r2)  (interpret-interval radii 0.0 1.0)]
                [(r1 r2)  (values (* r1 range) (* r2 range))])
    (if (and (> r2 0.0) (< r1 r2))
        (pict3d (make-point-light-shape (move-flt3 v) e range r1 r2))
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

(: freeze-in-groups (-> Pict3D Pict3D))
(define (freeze-in-groups p)
  (pict3d
   (scene-map-in-leaf-groups
    (pict3d-scene p)
    (λ (s) (if (empty-scene? s) s (make-frozen-scene-shape s))))))

;; ===================================================================================================
;; Transformations

(: transform (-> Pict3D Affine Pict3D))
(define (transform p t)
  (cond
    [(eq? t identity-linear)  p]
    [else  (pict3d (make-trans-scene (pict3d-scene p) (->flaffine3 t)))]))

(: make-transformer
   (All (A) (case-> (-> (-> A FlLinear3) (case-> (-> A Linear) (-> Pict3D A Pict3D)))
                    (-> (-> A FlAffLin3) (case-> (-> A Affine) (-> Pict3D A Pict3D))))))
(define (make-transformer f)
  (case-lambda
    [(v)  (define t (f v))
          (if (fllinear3? t)
              (fllinear3->linear t)
              (flaffine3->affine t))]
    [(p v)  (transform p (flafflin3->affine (f v)))]))

;; ---------------------------------------------------------------------------------------------------
;; Scale

(: check-real-scale (-> Symbol Real Flonum))
(define (check-real-scale name v)
  (let ([v  (fl v)])
    (if (= v 0.0)
        (raise-argument-error name "nonzero scale factor" v)
        v)))

(: check-dir-scale (-> Symbol Dir FlV3))
(define (check-dir-scale name v)
  (call/flv3-values v
    (λ (x y z)
      (if (or (= x 0.0) (= y 0.0) (= z 0.0))
          (raise-argument-error name "scale direction with nonzero components" v)
          v))))

(: check-scale (case-> (-> Symbol Real Flonum)
                       (-> Symbol Dir FlV3)
                       (-> Symbol (U Real Dir) (U Flonum FlV3))))
(define (check-scale name v)
  (if (real? v)
      (check-real-scale name v)
      (check-dir-scale name v)))

(define scale-x (make-transformer (λ ([v : Real]) (scale-x-flt3 (check-real-scale 'scale-x v)))))
(define scale-y (make-transformer (λ ([v : Real]) (scale-y-flt3 (check-real-scale 'scale-y v)))))
(define scale-z (make-transformer (λ ([v : Real]) (scale-z-flt3 (check-real-scale 'scale-z v)))))
(define scale (make-transformer (λ ([v : (U Real Dir)]) (scale-flt3 (check-scale 'scale v)))))

;; ---------------------------------------------------------------------------------------------------
;; Translate

(define move-x
  (make-transformer (λ ([v : Real])
                      (let ([v  (fl v)])
                        (if (= v 0.0) identity-affine (move-x-flt3 v))))))

(define move-y
  (make-transformer (λ ([v : Real])
                      (let ([v  (fl v)])
                        (if (= v 0.0) identity-affine (move-y-flt3 v))))))

(define move-z
  (make-transformer (λ ([v : Real])
                      (let ([v  (fl v)])
                        (if (= v 0.0) identity-affine (move-z-flt3 v))))))

(define move ((inst make-transformer Dir) move-flt3))

;; ---------------------------------------------------------------------------------------------------
;; Rotate

(: check-axis (-> Symbol Dir FlV3))
(define (check-axis name orig-v)
  (define v (flv3normalize orig-v))
  (if v v (raise-argument-error name "nonzero axis vector" v)))

(define rotate-x
  (make-transformer (λ ([a : Real])
                      (let ([a  (fl a)])
                        (cond [(= a 0.0)  identity-linear]
                              [else  (rotate-x-flt3 (degrees->radians a))])))))

(define rotate-y
  (make-transformer (λ ([a : Real])
                      (let ([a  (fl a)])
                        (cond [(= a 0.0)  identity-linear]
                              [else  (rotate-y-flt3 (degrees->radians a))])))))

(define rotate-z
  (make-transformer (λ ([a : Real])
                      (let ([a  (fl a)])
                        (cond [(= a 0.0)  identity-linear]
                              [else  (rotate-z-flt3 (degrees->radians a))])))))

(: rotate (case-> (-> Dir Real Linear)
                  (-> Pict3D Dir Real Pict3D)))
(define rotate
  (case-lambda
    [(v a)
     (let ([a  (fl a)]
           [v : FlV3  (check-axis 'rotate v)])
       (cond [(= a 0.0)  identity-linear]
             [else  (fllinear3->linear (rotate-flt3 v (degrees->radians a)))]))]
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
(define (scale-x/center pict dx [v (center-or-origin pict)])
  (scale/center pict (dir dx 1 1) v))

(: scale-y/center (->* [Pict3D Real] [Pos] Pict3D))
(define (scale-y/center pict dy [v (center-or-origin pict)])
  (scale/center pict (dir 1 dy 1) v))

(: scale-z/center (->* [Pict3D Real] [Pos] Pict3D))
(define (scale-z/center pict dz [v (center-or-origin pict)])
  (scale/center pict (dir 1 1 dz) v))

;; ---------------------------------------------------------------------------------------------------
;; Local rotation

(: rotate/center (->* [Pict3D Dir Real] [Pos] Pict3D))
(define (rotate/center pict axis angle [v (center-or-origin pict)])
  (local-transform pict (rotate axis angle) (move (pos- v origin))))

(: rotate-x/center (->* [Pict3D Real] [Pos] Pict3D))
(define (rotate-x/center pict angle [v (center-or-origin pict)])
  (rotate/center pict +x angle v))

(: rotate-y/center (->* [Pict3D Real] [Pos] Pict3D))
(define (rotate-y/center pict angle [v (center-or-origin pict)])
  (rotate/center pict +y angle v))

(: rotate-z/center (-> Pict3D Real Pict3D))
(define (rotate-z/center pict angle [v (center-or-origin pict)])
  (rotate/center pict +z angle v))

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
;; Triangle conversion and deformations

;; ---------------------------------------------------------------------------------------------------
;; Nonadaptive tessellation and deformation

(: tessellate-max-edge (-> Pict3D Integer Positive-Flonum))
(define (tessellate-max-edge p n)
  (define-values (v1 v2) (bounding-rectangle p))
  (if (and v1 v2)
      (call/flv3-values v1
        (λ (x1 y1 z1)
          (call/flv3-values v2
            (λ (x2 y2 z2)
              (define max-edge (/ (max (abs (- x1 x2)) (abs (- y1 y2)) (abs (- z1 z2))) (fl n)))
              (if (> max-edge 0.0) max-edge +inf.0)))))
      +inf.0))

(: tessellate (->* [Pict3D] [#:segments Integer #:max-edge (U #f Real) #:max-angle Real] Pict3D))
(define (tessellate p
                    #:segments [n (current-tessellate-segments)]
                    #:max-edge [max-edge (current-tessellate-max-edge)]
                    #:max-angle [max-angle (current-tessellate-max-angle)])
  (let* ([max-edge  (if (not max-edge)
                        (tessellate-max-edge p n)
                        (abs (fl max-edge)))]
         [max-edge  (abs (fl max-edge))]
         [max-edge  (if (> max-edge 0.0) max-edge +inf.0)]
         [max-angle  (degrees->radians (abs (fl max-angle)))])
    (pict3d
     (scene-map-in-leaf-groups/transform
      (pict3d-scene p)
      (λ (s t)
        (define-values (ss fs)
          (if (shape? s)
              (shape-tessellate s t max-edge max-angle)
              (scene-tessellate s t max-edge max-angle)))
        (define new-ss (faces->triangle-mesh-shapes fs))
        (scene-union (scene-union* ss) (scene-union* new-ss)))))))

(: deform (-> Pict3D Smooth Pict3D))
(define (deform p t0)
  (pict3d
   (let loop ([s : Scene  (pict3d-scene p)]
              [t : FlAffine3  identity-flaffine3]
              [inv-t : FlAffine3  identity-flaffine3]
              [deform-t : (Promise FlSmooth3)  (delay t0)])
     (cond
       [(empty-scene? s)  empty-scene]
       [(shape? s)
        (scene-union* (shape-deform s (force deform-t)))]
       [(node-scene? s)
        (make-node-scene (loop (node-scene-neg s) t inv-t deform-t)
                         (loop (node-scene-pos s) t inv-t deform-t))]
       [(trans-scene? s)
        (let ([t  (flt3compose t (trans-scene-affine s))])
          (define-values (new-t inv-new-t)
            (let* ([new-t  (fls3apply/affine t0 t)]
                   [inv-new-t  (flt3inverse new-t)])
              ;; Don't like this hack...
              (if inv-new-t
                  (values new-t inv-new-t)
                  (values identity-flaffine3 identity-flaffine3))))
          (define deform-t (delay (fls3compose inv-new-t (fls3compose t0 t))))
          (make-trans-scene
           (loop (trans-scene-scene s) t inv-new-t deform-t)
           (flt3compose inv-t new-t)))]
       [(group-scene? s)
        (group-scene (loop (group-scene-scene s) t inv-t deform-t)
                     (group-scene-tag s))]))))

;; ---------------------------------------------------------------------------------------------------
;; Adaptive tessellation and deformation

(: adaptive-tessellate
   (->* [Pict3D]
        [Smooth #:segments Integer #:max-edge (U #f Real) #:max-angle Real #:max-iters Integer]
        Pict3D))
(define (adaptive-tessellate p [t0 identity-affine]
                             #:segments [n (current-adaptive-segments)]
                             #:max-edge [max-edge (current-adaptive-max-edge)]
                             #:max-angle [max-angle (current-adaptive-max-angle)]
                             #:max-iters [max-iters (current-adaptive-max-iters)])
  (let* ([max-edge  (if (not max-edge)
                        (tessellate-max-edge (deform p t0) n)
                        (abs (fl max-edge)))]
         [max-edge  (if (> max-edge 0.0) max-edge +inf.0)]
         [max-angle  (degrees->radians (abs (fl max-angle)))]
         [max-iters  (max 0 max-iters)])
    (pict3d
     (scene-map-in-leaf-groups/transform
      (pict3d-scene p)
      (λ (s t)
        (define-values (ss fs) (scene-tessellate s t +inf.0 (* 0.5 pi)))
        (define new-fs
          (let* ([soup  (make-face-soup (ann fs (Listof (face deform-data Boolean))))]
                 [soup  (subdivide/deform soup (fls3compose t0 t) max-edge max-angle max-iters)])
            (face-soup-faces soup)))
        (define new-ss (faces->triangle-mesh-shapes new-fs))
        (scene-union (scene-union* ss) (scene-union* new-ss)))))))

(: adaptive-deform
   (->* [Pict3D Smooth]
        [#:segments Integer #:max-edge (U #f Real) #:max-angle Real #:max-iters Integer]
        Pict3D))
(define (adaptive-deform p t
                         #:segments [n (current-adaptive-segments)]
                         #:max-edge [max-edge (current-adaptive-max-edge)]
                         #:max-angle [max-angle (current-adaptive-max-angle)]
                         #:max-iters [max-iters (current-adaptive-max-iters)])
  (define tess-p
    (adaptive-tessellate p t
                         #:segments n
                         #:max-edge max-edge
                         #:max-angle max-angle
                         #:max-iters max-iters))
  (deform tess-p t))

;; ---------------------------------------------------------------------------------------------------
;; Deformation functions

(: displace (case-> (-> (-> Flonum Flonum Real) Smooth)
                    (-> Pict3D (-> Flonum Flonum Real) Pict3D)))
(define displace
  (case-lambda
    [(f)
     (smooth
      (λ (v)
        (call/flv3-values v
          (λ (x y z)
            (pos x y (+ z (fl (f x y))))))))]
    [(p f)
     (deform p (displace f))]))

(: twist (case-> (-> Real Smooth)
                 (-> Pict3D Real Pict3D)))
(define twist
  (case-lambda
    [(speed)
     (let ([speed  (degrees->radians (fl speed))])
       (smooth
        (λ (v)
          (call/flv3-values v
            (λ (x y z)
              (define angle (+ (* speed z) (atan y x)))
              (define r (flsqrt (+ (* x x) (* y y))))
              (pos (* r (cos angle))
                   (* r (sin angle))
                   z))))))]
    [(p speed)
     (deform p (twist speed))]))

(: extend-coord (-> Flonum Flonum Flonum))
(define (extend-coord x dx)
  (cond [(< x 0.0)  (- x dx)]
        [(> x 0.0)  (+ x dx)]
        [else  x]))

(: collapse-coord (-> Flonum Flonum Flonum))
(define (collapse-coord x dx)
  (cond [(< x dx)      (- x dx)]
        [(> x (- dx))  (+ x dx)]
        [else  0.0]))

(: extend (case-> (-> (U Real Dir) Smooth)
                  (-> Pict3D (U Real Dir) Pict3D)))
(define extend
  (case-lambda
    [(d)
     (define-values (dx dy dz) (interpret-scale d))
     (smooth
      (λ (v)
        (call/flv3-values v
          (λ (x y z)
            (pos (if (>= dx 0.0) (extend-coord x dx) (collapse-coord x dx))
                 (if (>= dy 0.0) (extend-coord y dy) (collapse-coord y dy))
                 (if (>= dz 0.0) (extend-coord z dz) (collapse-coord z dz)))))))]
    [(p d)
     (deform p (extend d))]))

(: bend-smooth (-> Real Interval Smooth))
(define (bend-smooth angle zivl)
  (match-define (interval zmin zmax) zivl)
  (let ([angle  (degrees->radians (fl angle))])
    (cond
      ;; If angle too small, or interval too large or irrational, return identity
      [(or (not (>= (abs angle) 1e-8)) (not (< (- zmax zmin) 1e16)))
       identity-affine]
      [else
       (define zsize (- zmax zmin))
       (define zofs (cond [(> zmin 0.0)  zmin]
                          [(< zmax 0.0)  zmax]
                          [else  0.0]))
       (smooth
        (λ (v)
          (call/flv3-values v
            (λ (x y z)
              (define α (/ (- (max zmin (min zmax z)) zofs) zsize))
              (define c (cos (* angle α)))
              (define s (sin (* angle α)))
              (define o (/ zsize angle))
              (pos (+ (- o (* (+ o x) c))
                      (* s (cond [(>= z zmax)  (- z zmax)]
                                 [(<= z zmin)  (- z zmin)]
                                 [else  0.0])))
                   y
                   (+ (* (+ o x) s)
                      (* c (cond [(>= z zmax)  (- z zmax)]
                                 [(<= z zmin)  (- z zmin)]
                                 [else  0.0]))
                      zofs))))))])))

(: bend-pict3d (->* [Pict3D Real] [Interval] Pict3D))
(define bend-pict3d
  (case-lambda
    [(p angle)
     (define-values (v1 v2) (bounding-rectangle p))
     (if (and v1 v2)
         (bend-pict3d p angle (interval (pos-z v1) (pos-z v2)))
         p)]
    [(p angle zivl)
     (deform p (bend-smooth angle zivl))]))

(: bend (case-> (-> Pict3D Real Pict3D)
                (-> Real Interval Smooth)
                (-> Pict3D Real Interval Pict3D)))
(define bend
  (case-lambda
    [(arg1 arg2)
     (cond [(and (pict3d? arg1) (real? arg2))    (bend-pict3d arg1 arg2)]
           [(and (real? arg1) (interval? arg2))  (bend-smooth arg1 arg2)])]
    [(p angle zivl)
     (bend-pict3d p angle zivl)]))

(: local-deform (case-> (-> Smooth Affine Smooth)
                        (-> Pict3D Smooth Affine Pict3D)))
(define local-deform
  (case-lambda
    [(t local-t)  (smooth-compose local-t t (affine-inverse local-t))]
    [(pict t local-t)  (deform pict (local-deform t local-t))]))

;; ===================================================================================================
;; Other face soup operations

(: transform-faces (All (A B) (-> (Listof (face A B)) FlAffine3 (Listof (face A B)))))
(define (transform-faces fs t)
  (map (λ ([f : (face A B)]) (flt3apply/face t f)) fs))

(: face-vertex-fun (All (A B) (-> (-> (Listof Vertex) Vertex) RGBA Emitted Material
                                  (-> (face A B) (U #f vtx)))))
(define (face-vertex-fun g cc ce cm)
  (λ (f)
    (define vert (g (face->vertices f)))
    (define lazy-n (delay (define-values (v1 v2 v3) (face-flv3s f))
                          (flv3polygon-perp v1 v2 v3)))
    (interpret-vtx vert lazy-n cc ce cm)))

(: face-vertex/adjacent-fun (All (A B) (-> (-> (Listof Vertex) (Listof (Listof Vertex)) Vertex)
                                           RGBA Emitted Material
                                           (-> (face A B) (Listof (face A B)) (U #f vtx)))))
(define (face-vertex/adjacent-fun g cc ce cm)
  (λ (f fs)
    (define h (λ ([f : (face A B)]) (face->vertices f)))
    (define vert (g (h f) (map h fs)))
    (define lazy-n (delay (define-values (v1 v2 v3) (face-flv3s f))
                          (flv3polygon-perp v1 v2 v3)))
    (interpret-vtx vert lazy-n cc ce cm)))

(: map-face-vertices (All (A B) (-> (Listof (face A B)) (-> (face A B) (U #f vtx))
                                    (Listof (face A B)))))
(define (map-face-vertices fs g)
  (define fsoup (make-face-soup fs))
  (for/fold ([new-fs : (Listof (face A B))  empty])
            ([f  (in-list (face-soup-faces fsoup))])
    (match-define (face vtx1 vtx2 vtx3 d d12 d23 d31) f)
    (match-define (list new-vtx1 new-vtx2 new-vtx3)
      (for/list : (Listof (U #f vtx)) ([i  (in-list (ann (list 1 2 3) (Listof (U 1 2 3))))])
        (g (face-set-first-vertex f i))))
    (if (and new-vtx1 new-vtx2 new-vtx3)
        (cons (face new-vtx1 new-vtx2 new-vtx3 d d12 d23 d31) new-fs)
        new-fs)))

(: map-face-vertices/adjacent (All (A B) (-> (Listof (face A B))
                                             (-> (face A B) (Listof (face A B)) (U #f vtx))
                                             (Listof (face A B)))))
(define (map-face-vertices/adjacent fs g)
  (define fsoup (make-face-soup fs))
  (for/fold ([new-fs : (Listof (face A B))  empty])
            ([f  (in-list (face-soup-faces fsoup))])
    (match-define (face vtx1 vtx2 vtx3 d d12 d23 d31) f)
    (match-define (list new-vtx1 new-vtx2 new-vtx3)
      (for/list : (Listof (U #f vtx)) ([i  (in-list (ann (list 1 2 3) (Listof (U 1 2 3))))])
        (define this-f (face-set-first-vertex f i))
        (define v1 (vtx-position (face-vtx1 this-f)))
        (define other-fs
          (for/list : (Listof (face A B)) ([o  (in-list (face-soup-corner-faces fsoup v1))])
            (face-set-first-vertex (cdr o) (car o))))
        (g this-f other-fs)))
    (if (and new-vtx1 new-vtx2 new-vtx3)
        (cons (face new-vtx1 new-vtx2 new-vtx3 d d12 d23 d31) new-fs)
        new-fs)))

(: replace-vertices (-> Pict3D (-> (Listof Vertex) Vertex) Pict3D))
(define (replace-vertices p g)
  (define cc (current-color))
  (define ce (current-emitted))
  (define cm (current-material))
  (pict3d
   (scene-map-in-leaf-groups/transform
    (pict3d-scene p)
    (λ (s t)
      (define tinv (flt3inverse t))
      (cond
        [tinv
         (define h (face-vertex-fun g cc ce cm))
         (define-values (ss fs) (scene-extract-faces s))
         ;; Group them because we don't want things touching at just a vertex to get smoothed
         (define fss (face-soup-group (make-face-soup fs)))
         (define sss (map (λ ([fs : (Listof (face deform-data #f))])
                            (let* ([fs  (transform-faces fs t)]
                                   [fs  (map-face-vertices fs h)]
                                   [fs  (transform-faces fs tinv)])
                              (faces->triangle-mesh-shapes fs)))
                          fss))
         (scene-union* (cons (scene-union* ss) (map scene-union* sss)))]
        [else
         empty-scene])))))

(: replace-vertices/adjacent (-> Pict3D (-> (Listof Vertex) (Listof (Listof Vertex)) Vertex) Pict3D))
(define (replace-vertices/adjacent p g)
  (define cc (current-color))
  (define ce (current-emitted))
  (define cm (current-material))
  (pict3d
   (scene-map-in-leaf-groups/transform
    (pict3d-scene p)
    (λ (s t)
      (define tinv (flt3inverse t))
      (cond
        [tinv
         (define h (face-vertex/adjacent-fun g cc ce cm))
         (define-values (ss fs) (scene-extract-faces s))
         ;; Group them because we don't want things touching at just a vertex to get smoothed
         (define fss (face-soup-group (make-face-soup fs)))
         (define sss (map (λ ([fs : (Listof (face deform-data #f))])
                            (let* ([fs  (transform-faces fs t)]
                                   [fs  (map-face-vertices/adjacent fs h)]
                                   [fs  (transform-faces fs tinv)])
                              (faces->triangle-mesh-shapes fs)))
                          fss))
         (scene-union* (cons (scene-union* ss) (map scene-union* sss)))]
        [else
         empty-scene])))))

(: vertex-normal-or-zero (-> Vertex FlV3))
(define (vertex-normal-or-zero vert)
  (let* ([n1  (vertex-normal vert)]
         [n1  (and n1 (flv3normalize n1))])
    (if n1 n1 zero-flv3)))

(: merge-normals (->* [] [#:blend Real] (-> (Listof Vertex) (Listof (Listof Vertex)) Vertex)))
(define (merge-normals #:blend [α 1.0])
  (let ([α  (flclamp (fl α) 0.0 1.0)])
    (λ (vs vss)
      (define v0 (first vs))
      (define n0 (vertex-normal-or-zero v0))
      ;; Loop over first vertices' normals and sum
      (define new-n0
        (for/fold ([n : FlV3  zero-flv3]) ([vs  (in-list vss)])
          (match-define (list v1 v2 v3) (map vertex-pos vs))
          (define n1 (vertex-normal-or-zero (first vs)))
          (define angle (acos (flv3corner-cos v3 v1 v2)))
          (flv3+ n (flv3+ (flv3* n0 (* (- 1.0 α) angle))
                          (flv3* n1 (* α angle))))))
      ;; Set first vertex's normal
      (let* ([new-n0  (and new-n0 (flv3normalize new-n0))]
             [new-n0  (and new-n0 (flv3->dir new-n0))])
        (set-vertex-normal v0 new-n0)))))

(: plane-normals (->* [] [#:blend Real] (-> (Listof Vertex) Vertex)))
(define (plane-normals #:blend [α 1.0])
  (let ([α  (flclamp (fl α) 0.0 1.0)])
    (λ (vs)
      (define v0 (first vs))
      (define n0 (vertex-normal-or-zero v0))
      (match-define (list v1 v2 v3) (map vertex-pos vs))
      (let* ([new-n0  (flv3polygon-normal v1 v2 v3)]
             [new-n0  (if new-n0 new-n0 zero-flv3)]
             [new-n0  (flv3->dir (flv3blend n0 new-n0 α))])
        (set-vertex-normal v0 new-n0)))))

(: merge-vertex-normals (->* [Pict3D] [#:blend Real] Pict3D))
(define (merge-vertex-normals p #:blend [α 1.0])
  (replace-vertices/adjacent p (merge-normals #:blend α)))

(: plane-vertex-normals (->* [Pict3D] [#:blend Real] Pict3D))
(define (plane-vertex-normals p #:blend [α 1.0])
  (replace-vertices p (plane-normals #:blend α)))

;; ===================================================================================================
;; Combining scenes

(: ungroup-or-affine (-> Pict3D (U (Listof Tag) Affine) Pict3D))
(define (ungroup-or-affine p n)
  (if (affine? n) p (ungroup p n)))

(: find-group-transforms-or-affine (-> Pict3D (U (Listof Tag) Affine) (Listof Affine)))
(define (find-group-transforms-or-affine p n)
  (if (affine? n) (list n) (find-group-transforms-or-affine p n)))

(: find-group-transform-or-affine (-> Pict3D (U (Listof Tag) Affine) Affine))
(define (find-group-transform-or-affine p n)
  (if (affine? n) n (find-group-transform p n)))

(: set-origin (-> Pict3D (U (Listof Tag) Affine) Pict3D))
(define (set-origin p n)
  (transform p (affine-inverse (find-group-transform-or-affine p n))))

(: pin* (->* [Pict3D (Listof Tag) Pict3D] [(U (Listof Tag) Affine)] Pict3D))
(define (pin* p1 n1 p2 [n2 empty])
  (let ([p2  (ungroup-or-affine (set-origin p2 n2) n2)])
    (replace-in-group p1 n1 (λ ([p : Pict3D]) (combine p p2)))))

(: weld* (->* [Pict3D (Listof Tag) Pict3D] [(U (Listof Tag) Affine)] Pict3D))
(define (weld* p1 n1 p2 [n2 empty])
  (let ([p2  (ungroup-or-affine (set-origin p2 n2) n2)])
    (replace-group p1 n1 (λ ([p : Pict3D]) (combine (group-contents p) p2)))))

(: pin (->* [Pict3D (Listof Tag) Pict3D] [(U (Listof Tag) Affine)] Pict3D))
(define (pin p1 n1 p2 [n2 empty])
  (check-pin-multi 'pin p1 n1)
  (pin* p1 n1 p2 n2))

(: weld (->* [Pict3D (Listof Tag) Pict3D] [(U (Listof Tag) Affine)] Pict3D))
(define (weld p1 n1 p2 [n2 empty])
  (check-pin-multi 'weld p1 n1)
  (weld* p1 n1 p2 n2))

(: check-pin-multi : Symbol Pict3D (Listof Tag) -> Void)
(define (check-pin-multi pin p1 n1)
  (define ps (map-group p1 n1 (λ ([p : Pict3D]) p)))
  (define n (length ps))
  (unless (= n 1)
    (error pin "~v groups with path ~v exist \
(if this is intentional, use ~a* instead)"
           n n1 pin)))

(: join (->* [Pict3D (U (Listof Tag) Affine) Pict3D] [(U (Listof Tag) Affine)] Pict3D))
(define (join p1 n1 p2 [n2 empty])
  (combine
   p1
   (relocate p2
             (find-group-transform-or-affine p2 n2)
             (find-group-transform-or-affine p1 n1))))

(: glue (->* [Pict3D (U (Listof Tag) Affine) Pict3D] [(U (Listof Tag) Affine)] Pict3D))
(define (glue p1 n1 p2 [n2 empty])
  (combine
   (ungroup-or-affine p1 n1)
   (relocate (ungroup-or-affine p2 n2)
             (find-group-transform-or-affine p2 n2)
             (find-group-transform-or-affine p1 n1))))

(: join* (->* [Pict3D (U (Listof Tag) Affine) Pict3D] [(U (Listof Tag) Affine)] Pict3D))
(define (join* p1 n1 p2 [n2 empty])
  (let ([p2 (set-origin p2 n2)])
    (combine
     p1
     (for/list ([t1 (in-list (find-group-transforms-or-affine p1 n1))]) : (Listof Pict3D)
       (transform p2 t1)))))

(: glue* (->* [Pict3D (U (Listof Tag) Affine) Pict3D] [(U (Listof Tag) Affine)] Pict3D))
(define (glue* p1 n1 p2 [n2 empty])
  (let ([p2 (ungroup-or-affine (set-origin p2 n2) n2)])
    (combine
     (ungroup-or-affine p1 n1)
     (for/list ([t1 (in-list (find-group-transforms-or-affine p1 n1))]) : (Listof Pict3D)
       (transform p2 t1)))))

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
  (define t (flt3compose (move-z-flt3 0.5) (scale-flt3 (flv3 #i2/64 #i2/64 0.5))))
  (define cc (current-color))
  (define ce (current-emitted))
  (define cm (current-material))
  (pict3d (make-arrow-scene t 0.5 #i1/8 0.125 cc ce cm #f)))

(: arrow (->* [Pos (U Pos Dir)] [#:normalize? Any] Pict3D))
(define (arrow from to #:normalize? [normalize? #f])
  (transform (up-arrow) (point-at from to #:normalize? normalize?)))

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

(: canvas-projective
   (->* []
        [#:width Integer #:height Integer #:z-near Real #:z-far Real #:fov Real]
        FlTransform3))
(define (canvas-projective #:width [width (current-pict3d-width)]
                           #:height [height (current-pict3d-height)]
                           #:z-near [z-near (current-pict3d-z-near)]
                           #:z-far [z-far (current-pict3d-z-far)]
                           #:fov [fov (current-pict3d-fov)])
  (let ([width   (fl (max 1 width))]
        [height  (fl (max 1 height))]
        [z-near  (flclamp (fl z-near) default-pict3d-z-near default-pict3d-z-far)]
        [z-far   (flclamp (fl z-far)  default-pict3d-z-near default-pict3d-z-far)]
        [fov     (flclamp (fl fov) 1.0 179.0)])
    (perspective-flt3/viewport width height (degrees->radians fov) z-near z-far)))

(: bitmap-projective
   (->* []
        [#:width Integer #:height Integer #:z-near Real #:z-far Real #:fov Real]
        FlTransform3))
;; Like canvas projection but upside-down because OpenGL origin is lower-left
(define (bitmap-projective #:width [width (current-pict3d-width)]
                           #:height [height (current-pict3d-height)]
                           #:z-near [z-near (current-pict3d-z-near)]
                           #:z-far [z-far (current-pict3d-z-far)]
                           #:fov [fov (current-pict3d-fov)])
  (flt3compose
   (scale-flt3 +x-y+z-flv3)
   (canvas-projective #:width width #:height height #:z-near z-near #:z-far z-far #:fov fov)))

(: camera->view (-> Affine Affine))
;; Inverts a camera basis to get a view transform; also negates the y and z axes
;; In OpenGL, +z is toward the viewer and +y is up
;; In Pict3D, +z is away from the viewer and +y is down (like typical bitmap coordinates)
(define (camera->view t)
  (define tinv (flt3inverse t))
  (cond [tinv  (flafflin3->affine (flt3compose (scale-flt3 +x-y-z-flv3) tinv))]
        [else  (raise-argument-error 'camera->view "invertible Affine" t)]))

(: camera-ray-dir
   (->* [Affine]
        [#:width Integer #:height Integer #:z-near Real #:z-far Real #:fov Real]
        (-> Real Real Dir)))
(define (camera-ray-dir t
                        #:width [width (current-pict3d-width)]
                        #:height [height (current-pict3d-height)]
                        #:z-near [z-near (current-pict3d-z-near)]
                        #:z-far [z-far (current-pict3d-z-far)]
                        #:fov [fov (current-pict3d-fov)])
  (define unview (assert (flt3inverse (camera->view t)) values))
  (define unproj
    (assert
     (flt3inverse
      (bitmap-projective #:width width #:height height #:z-near z-near #:z-far z-far #:fov fov))
     values))
  (define w (fl (max 1 width)))
  (define h (fl (max 1 height)))
  (λ (x y)
    ;; View direction
    (define clip-x (* (- (/ (fl x) w) 0.5) 2.0))
    (define clip-y (* (- (/ (fl y) h) 0.5) 2.0))
    (define dv
      (let ([dv  (flt3apply/pos unproj (flv3 clip-x clip-y -1.0))])
        (flt3apply/dir unview dv)))
    (define n (flv3normalize dv))
    (if n
        (call/flv3-values dv dir)
        (error 'camera-ray-dir "view transform inverse ~e returned zero direction ~e" unview dv))))

;; ===================================================================================================
;; Debugging and visualization

(: light-grid (->* [Emitted Emitted Emitted] [Real] Pict3D))
(define (light-grid ex ey ez [s 1.0])
  (pict3d (make-light-grid-shape ex ey ez (real->double-flonum s))))

(: outline-trues (Vectorof Boolean))
(define outline-trues (vector #t #t #t))

(: wireframe (->* [Pict3D] [#:width Real] Pict3D))
(define (wireframe p #:width [width 1.5])
  (let* ([width  (abs (fl width))]
         [width  (if (= width 0.0) 1.0 width)])
    (define cc (current-color))
    (define ce (current-emitted))
    (define cm (current-material))
    (pict3d
     (let loop ([s  (pict3d-scene p)])
       (scene-map-shapes
        s
        (λ (s)
          (cond
            [(triangle-mesh-shape? s)
             (match-define (triangle-mesh-shape _ _ vtxs idxs back?) s)
             (scene-union*
              (for/fold ([ss : (Listof Scene)  empty])
                        ([i  (in-range 0 (vector-length idxs) 3)])
                (match-define (vtx v1 n1 _ _ _) (vector-ref vtxs (vector-ref idxs i)))
                (match-define (vtx v2 n2 _ _ _) (vector-ref vtxs (vector-ref idxs (+ i 1))))
                (match-define (vtx v3 n3 _ _ _) (vector-ref vtxs (vector-ref idxs (+ i 2))))
                (define vtx1 (vtx v1 n1 cc ce cm))
                (define vtx2 (vtx v2 n2 cc ce cm))
                (define vtx3 (vtx v3 n3 cc ce cm))
                (cons (make-triangle-outline-shape (vector vtx1 vtx2 vtx3)
                                                   outline-trues outline-trues 1.5 back?)
                      ss)))]
            [(frozen-scene-shape? s)
             (loop (frozen-scene-shape-scene s))]
            [else
             empty-scene])))))))
