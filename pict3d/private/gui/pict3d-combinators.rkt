#lang typed/racket/base

(require racket/list
         racket/set
         racket/match
         typed/racket/class
         typed/racket/gui
         math/flonum
         math/base
         "../math/flv3.rkt"
         "../math/flt3.rkt"
         "../math/flrect3.rkt"
         "../engine/scene.rkt"
         "../engine/utils.rkt"
         (only-in "../engine/types.rkt" affine)
         "../utils.rkt"
         "user-types.rkt"
         "pict3d-struct.rkt"
         "axes-scene.rkt"
         )

(provide
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
 group?
 group-name
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
 sphere
 ellipsoid
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
 set-origin
 point-at
 pict3d-view-transform
 ;; Combining scenes
 combine
 combine*
 pin
 weld
 ;; Testing
 plane-cull
 rect-cull
 frustum-cull
 ;; Other shapes
 up-arrow
 arrow
 cylinder
 cone
 ;; Collision detection
 trace
 )

;; ===================================================================================================
;; Parameters

(define default-color (flvector 1.0 1.0 1.0 1.0))
(define default-emitted (flvector 0.0 0.0 0.0 0.0))
(define default-material (make-material 0.05 0.6 0.35 0.3))

(: current-color (Parameterof Pict3D-Color FlVector))
(define current-color
  (make-parameter default-color (λ ([col : Pict3D-Color]) (->flcolor4 'current-color col))))

(: current-emitted (Parameterof Pict3D-Color FlVector))
(define current-emitted
  (make-parameter default-emitted (λ ([col : Pict3D-Color]) (->flcolor4 'current-emitted col))))

(: current-material (Parameterof Material))
(define current-material (make-parameter default-material))

(: set-color (-> Pict3D Pict3D-Color Pict3D))
(define (set-color p c)
  (let ([c  (->flcolor4 'set-color c)])
    (pict3d (scene-map-shapes (pict3d-scene p) (λ (a) (shape-set-color a c))))))

(: set-emitted (-> Pict3D Pict3D-Color Pict3D))
(define (set-emitted p e)
  (let ([e  (->flcolor4 'set-emitted e)])
    (pict3d (scene-map-shapes (pict3d-scene p) (λ (a) (shape-set-emitted a e))))))

(: set-material (-> Pict3D Material Pict3D))
(define (set-material p m)
  (pict3d (scene-map-shapes (pict3d-scene p) (λ (a) (shape-set-material a m)))))

;; ===================================================================================================
;; Naming, mapping, finding

(: group (-> Pict3D Tag Pict3D))
(define (group p n)
  (pict3d (make-group-scene n (pict3d-scene p))))

(: basis (-> Tag Affine Pict3D))
(define (basis n t)
  (pict3d (make-trans-scene t (make-group-scene n empty-scene))))

(: group? (-> Pict3D Boolean))
(define (group? p)
  (group-scene? (pict3d-scene p)))

(: group-name (-> Pict3D Tag))
(define (group-name p)
  (define s (pict3d-scene p))
  (if (group-scene? s)
      (group-scene-tag s)
      (raise-argument-error 'group-name "a group" p)))

(: group-contents (-> Pict3D Pict3D))
(define (group-contents p)
  (define s (pict3d-scene p))
  (if (group-scene? s)
      (pict3d (group-scene-scene s))
      (raise-argument-error 'group-contents "a group" p)))

(: make-replace (-> (-> Scene Tag (-> Scene Scene) Scene)
                    (-> Pict3D (U Tag (Listof+1 Tag)) (-> Pict3D Pict3D) Pict3D)))
(define ((make-replace g) p n f)
  (if (pair? n)
      (if (empty? (cdr n))
          ((make-replace g) p (car n) f)
          ((make-replace g) p (car n) (λ (p) ((make-replace g) p (cdr n) f))))
      (pict3d (g (pict3d-scene p) n (λ (s) (pict3d-scene (f (pict3d s))))))))

(define replace-group (make-replace scene-replace-group))
(define replace-in-group (make-replace scene-replace-in-group))

(: ungroup (-> Pict3D (U Tag (Listof+1 Tag)) Pict3D))
(define (ungroup p n)
  (replace-group p n group-contents))

(: remove-group (-> Pict3D (U Tag (Listof+1 Tag)) Pict3D))
(define (remove-group p n)
  (replace-group p n (λ (_) empty-pict3d)))

(: remove-in-group (-> Pict3D (U Tag (Listof+1 Tag)) Pict3D))
(define (remove-in-group p n)
  (replace-in-group p n (λ (_) empty-pict3d)))

(: map-group* (All (A) (-> Pict3D Tag (-> Pict3D A) (Listof A))))
(define (map-group* p n f)
  (scene-map-group (pict3d-scene p) n (λ ([s : group-scene]) (f (pict3d s)))))

(: map-group (All (A) (-> Pict3D (U Tag (Listof+1 Tag)) (-> Pict3D A) (Listof A))))
(define (map-group p n f)
  (if (pair? n)
      (if (empty? (cdr n))
          (map-group* p (car n) f)
          (append* (map-group* p (car n) (λ ([p : Pict3D]) (map-group p (cdr n) f)))))
      (map-group* p n f)))

(: map-group/transform* (All (A) (-> Pict3D Tag (-> Affine Pict3D A) (Listof A))))
(define (map-group/transform* p n f)
  (scene-map-group/transform (pict3d-scene p) n
                             (λ ([t : Affine] [s : group-scene]) (f t (pict3d s)))))

(: map-group/transform (All (A) (-> Pict3D
                                    (U Tag (Listof+1 Tag))
                                    (-> Affine Pict3D A)
                                    (Listof A))))
(define (map-group/transform p n f)
  (if (pair? n)
      (if (empty? (cdr n))
          (map-group/transform* p (car n) f)
          (append* (map-group/transform*
                    p (car n)
                    (λ ([t : Affine] [p : Pict3D])
                      (map-group/transform
                       p (cdr n)
                       (λ ([t0 : Affine] [p : Pict3D])
                         (f (affine-compose t t0) p)))))))
      (map-group/transform* p n f)))

;; ===================================================================================================
;; Basic shapes

;; ---------------------------------------------------------------------------------------------------
;; Triangle

(: triangle (->* [Pos Pos Pos] [#:back? Any] Pict3D))
(define (triangle v1 v2 v3 #:back? [back? #f])
  (define vs (vector (pos->flvector v1) (pos->flvector v2) (pos->flvector v3)))
  (define norm (flv3polygon-normal vs))
  (cond [norm
         (pict3d
          (shape->scene
           (make-triangle-shape vs norm
                                (current-color)
                                (current-emitted)
                                (current-material)
                                (and back? #t))))]
        [else  empty-pict3d]))

;; ---------------------------------------------------------------------------------------------------
;; Quad

(: quad (->* [Pos Pos Pos Pos] [#:back? Any] Pict3D))
(define (quad v1 v2 v3 v4 #:back? [back? #f])
  (define vs (vector (pos->flvector v1) (pos->flvector v2) (pos->flvector v3) (pos->flvector v4)))
  (define norm (flv3polygon-normal vs))
  (cond [norm  (pict3d
                (scene-union*
                 (map
                  shape->scene
                  (make-quad-shapes vs norm
                                    (current-color)
                                    (current-emitted)
                                    (current-material)
                                    (and back? #t)))))]
        [else  empty-pict3d]))

;; ---------------------------------------------------------------------------------------------------
;; Rectangle

(: rectangle (->* [Pos Pos] [#:inside? Any] Pict3D))
(define (rectangle v1 v2 #:inside? [inside? #f])
  (pict3d
   (shape->scene
    (make-rectangle-shape (assert (flv3rect (vector (pos->flvector v1) (pos->flvector v2)))
                                  nonempty-flrect3?)
                          (current-color)
                          (current-emitted)
                          (current-material)
                          (and inside? #t)))))

;; ---------------------------------------------------------------------------------------------------
;; Ellipsoid

(: sphere (->* [Pos Real] [#:inside? Any] Pict3D))
(define (sphere center radius #:inside? [inside? #f])
  (define r (fl radius))
  (define t (flt3compose (translate-flt3 (pos->flvector center))
                         (scale-flt3 (flvector r r r))))
  (pict3d
   (shape->scene
    (make-sphere-shape (affine t)
                       (current-color)
                       (current-emitted)
                       (current-material)
                       (and inside? #t)))))

(: ellipsoid (->* [Pos Pos] [#:inside? Any] Pict3D))
(define (ellipsoid v1 v2 #:inside? [inside? #f])
  (let ([v1  (pos->flvector v1)]
        [v2  (pos->flvector v2)])
    (define t (flt3compose (translate-flt3 (flv3* (flv3+ v1 v2) 0.5))
                           (scale-flt3 (flv3* (flv3- v2 v1) 0.5))))
    (pict3d
     (shape->scene
      (make-sphere-shape (affine t)
                         (current-color)
                         (current-emitted)
                         (current-material)
                         (and inside? #t))))))

;; ---------------------------------------------------------------------------------------------------
;; Directional light

(: sunlight (-> Dir Pict3D-Color Real Pict3D))
(define (sunlight direction color intensity)
  (let ([direction  (flv3normalize (dir->flvector direction))]
        [color      (->flcolor3 'sunlight color)]
        [intensity  (fl intensity)])
    (if direction
        (pict3d (shape->scene (make-directional-light-shape color intensity direction)))
        empty-pict3d)))

;; ---------------------------------------------------------------------------------------------------
;; Point light

(: default-light-radius (-> Flonum Flonum))
(define (default-light-radius intensity)
  (flsqrt (* 20.0 intensity)))

(: light (->* [Pos] [Pict3D-Color Real Real] Pict3D))
(define (light position
               [color  (flvector 1.0 1.0 1.0)]
               [intensity  1.0]
               [radius  #f])
  (let* ([position  (pos->flvector position)]
         [color  (->flcolor3 'light color)]
         [intensity  (fl intensity)]
         [radius  (if radius (fl radius) (default-light-radius intensity))])
    (pict3d
     (shape->scene
      (make-point-light-shape color intensity position radius)))))

;; ---------------------------------------------------------------------------------------------------
;; Efficiency

(: freeze (-> Pict3D Pict3D))
(define (freeze p)
  (define scene (pict3d-scene p))
  (if (empty-scene? scene)
      p
      (pict3d (shape->scene (make-frozen-scene-shape scene)))))

;; ===================================================================================================
;; Transformations

(: transform (-> Pict3D Affine Pict3D))
(define (transform s t)
  (pict3d (make-trans-scene t (pict3d-scene s))))

(: make-transformer (All (A) (-> (-> A FlAffine3-)
                                 (case-> (-> A Affine)
                                         (-> Pict3D A Pict3D)))))
(define (make-transformer f)
  (case-lambda
    [(v)  (affine (f v))]
    [(p v)  (transform p (affine (f v)))]))


(: set-origin (-> Pict3D (U Tag (Listof+1 Tag)) Pict3D))
(define (set-origin p n)
  (: fail (-> Index Nothing))
  (define (fail m)
    (error 'set-origin "epected one group named ~e; given a Pict3D with ~a groups named ~e" n m n))
  (define ps (map-group/transform p n (λ ([t : Affine] _) (transform p (affine-inverse t)))))
  (cond [(empty? ps)  (fail 0)]
        [(empty? (rest ps))  (first ps)]
        [else  (fail (length ps))]))

;; ---------------------------------------------------------------------------------------------------
;; Scale

(: check-scale (-> Symbol (U Flonum FlVector) FlVector))
(define (check-scale name v)
  (cond [(flonum? v)
         (cond [(= v 0.0)  (raise-argument-error name "nonzero scale" v)]
               [else  (flvector v v v)])]
        [else
         (define-values (x y z) (flv3-values v))
         (cond [(or (= x 0.0) (= y 0.0) (= z 0.0))
                (raise-argument-error name "nonzero scale" v)]
               [else  v])]))

(define scale-x
  (make-transformer (λ ([v : Real]) (scale-flt3 (check-scale 'scale-x (flvector (fl v) 1.0 1.0))))))

(define scale-y
  (make-transformer (λ ([v : Real]) (scale-flt3 (check-scale 'scale-y (flvector 1.0 (fl v) 1.0))))))

(define scale-z
  (make-transformer (λ ([v : Real]) (scale-flt3 (check-scale 'scale-z (flvector 1.0 1.0 (fl v)))))))

(define scale
  (make-transformer (λ ([v : (U Real Dir)])
                      (scale-flt3 (check-scale 'scale (if (real? v) (fl v) (dir->flvector v)))))))

;; ---------------------------------------------------------------------------------------------------
;; Translate

(define move-x (make-transformer (λ ([v : Real]) (translate-flt3 (flvector (fl v) 0.0 0.0)))))
(define move-y (make-transformer (λ ([v : Real]) (translate-flt3 (flvector 0.0 (fl v) 0.0)))))
(define move-z (make-transformer (λ ([v : Real]) (translate-flt3 (flvector 0.0 0.0 (fl v))))))
(define move (make-transformer (λ ([v : Dir]) (translate-flt3 (dir->flvector v)))))

;; ---------------------------------------------------------------------------------------------------
;; Rotate

(: check-axis (-> Symbol Dir FlVector))
(define (check-axis name orig-v)
  (define v (flv3normalize (dir->flvector orig-v)))
  (if v v (raise-argument-error name "nonzero axis vector" v)))

(define rotate-x (make-transformer (λ ([a : Real])
                                     (rotate-flt3 (dir->flvector x+) (degrees->radians (fl a))))))
(define rotate-y (make-transformer (λ ([a : Real])
                                     (rotate-flt3 (dir->flvector y+) (degrees->radians (fl a))))))
(define rotate-z (make-transformer (λ ([a : Real])
                                     (rotate-flt3 (dir->flvector z+) (degrees->radians (fl a))))))

(: rotate (case-> (-> Dir Real Affine)
                  (-> Pict3D Dir Real Pict3D)))
(define rotate
  (case-lambda
    [(v a)  (affine (rotate-flt3 (check-axis 'rotate v) (degrees->radians (fl a))))]
    [(p v a)  (transform p (rotate v a))]))

;; ---------------------------------------------------------------------------------------------------
;; Point at/to

(: point-at (->* [Pos (U Pos Dir)] [#:angle Real #:up Dir #:normalize? Any] Affine))
(define (point-at from to 
                  #:angle [angle 0.0]
                  #:up [up z+]
                  #:normalize? [normalize? #t])
  (define z-axis (if (dir? to) to (pos- to from)))
  (let* ([z-axis  (if normalize? (dir-normalize z-axis) z-axis)]
         [z-axis : Dir  (if z-axis z-axis z+)]
         [angle  (degrees->radians (fl angle))]
         [up  (dir-normalize up)]
         [up  (if up up z+)])
    (define x-axis (dir-normalize (dir-cross z-axis up)))
    (define t
      (cond
        [x-axis
         (define y-axis (assert (dir-normalize (dir-cross z-axis x-axis)) values))
         (cols->affine x-axis y-axis z-axis from)]
        [(>= (flvector-ref (dir->flvector z-axis) 2) 0.0)
         (move (pos- from origin))]
        [else
         (affine-compose (move (pos- from origin))
                         (scale (dir -1.0 1.0 -1.0)))]))
    (affine-compose t (affine (rotate-z-flt3 angle)))))

;; ===================================================================================================
;; View transform and auto camera

(: pict3d-camera (-> Pict3D (U #f Affine)))
(define (pict3d-camera p)
  (define ts (scene-map-group/transform (pict3d-scene p) 'camera (λ ([t : Affine] _) t)))
  (if (pair? ts) (first ts) #f))

(: pict3d-auto-camera (-> Pict3D Affine))
(define (pict3d-auto-camera p)
  (let* ([s  (pict3d-scene p)]
         [s  (scene-filter-shapes s (λ (a) (or (solid-shape? a) (frozen-scene-shape? a))))]
         [b  (scene-rect s)]
         [c  (if (empty-flrect3? b) origin (flvector->pos (flrect3-center b)))]
         [d  (if (= 0.0 (flrect3-volume b))
                 1.0
                 (dir-dist (pos- (flvector->pos (flrect3-max b)) c)))]
         [d  (/ (* d 1.25) (flsqrt 3.0))])
    (point-at (pos+ c (dir d d d)) c)))

(: pict3d-view-transform (-> Pict3D Affine))
(define (pict3d-view-transform p)
  (let* ([t  (pict3d-camera p)]
         [t  (if t t (pict3d-auto-camera p))])
    (affine-compose (scale (dir 1.0 -1.0 -1.0))
                    (affine-inverse t))))

;; ===================================================================================================
;; Combining scenes (i.e. union)

(: combine* (-> (Listof Pict3D) Pict3D))
(define (combine* ps)
  (cond [(empty? ps)  empty-pict3d]
        [else  (pict3d (scene-union* (map pict3d-scene ps)))]))

(: combine (-> Pict3D * Pict3D))
(define (combine . ps) (combine* ps))

(: pin (->* [Pict3D (U Tag (Listof+1 Tag)) Pict3D] [(U Tag (Listof+1 Tag))]
            Pict3D))
(define (pin p1 n1 p2 [n2 #f])
  (let ([p2  (if n2 (ungroup (set-origin p2 n2) n2) p2)])
    (replace-in-group p1 n1 (λ ([p : Pict3D]) (combine p p2)))))

(: weld (->* [Pict3D Tag Pict3D] [Tag] Pict3D))
(define (weld p1 n1 p2 [n2 #f])
  (let ([p2  (if n2 (ungroup (set-origin p2 n2) n2) p2)])
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
       (shape->scene
        (make-triangle-shape
         (vector (flvector x1 y1 1.0)
                 (flvector x2 y2 1.0)
                 (dir->flvector z+))
         (dir->flvector z+)
         c e m inside?))
       ;; Sides
       (shape->scene
        (make-triangle-shape
         (vector (flvector x1 y1 -1.0)
                 (flvector x2 y2 -1.0)
                 (flvector x2 y2 1.0))
         (vector (flvector x1 y1 0.0)
                 (flvector x2 y2 0.0)
                 (flvector x2 y2 0.0))
         c e m inside?))
       (shape->scene
        (make-triangle-shape
         (vector (flvector x1 y1 -1.0)
                 (flvector x2 y2 1.0)
                 (flvector x1 y1 1.0))
         (vector (flvector x1 y1 0.0)
                 (flvector x2 y2 0.0)
                 (flvector x1 y1 0.0))
         c e m inside?))
       ;; Bottom
       (shape->scene
        (make-triangle-shape
         (vector (flvector x2 y2 -1.0)
                 (flvector x1 y1 -1.0)
                 (dir->flvector z-))
         (dir->flvector z-)
         c e m inside?)))))))

(: cylinder (->* [Pos Pos] [#:inside? Any #:segments Natural] Pict3D))
(define (cylinder v1 v2 #:inside? [inside? #f] #:segments [n 32])
  (let ([v1  (pos->flvector v1)]
        [v2  (pos->flvector v2)])
    (define t (flt3compose (translate-flt3 (flv3* (flv3+ v1 v2) 0.5))
                           (scale-flt3 (flv3* (flv3- v2 v1) 0.5))))
    (freeze
     (pict3d (make-trans-scene (affine t) (standard-cylinder-scene (and inside? #t) n))))))

;; ---------------------------------------------------------------------------------------------------
;; Cone

(: standard-cone-scene (-> Boolean Natural Boolean Scene))
(define (standard-cone-scene inside? n smooth?)
  (define c (current-color))
  (define e (current-emitted))
  (define m (current-material))
  (define nx (/ 1.0 (flsqrt (+ (sqr 1.0) (sqr 0.5)))))
  (define ny (/ 0.5 (flsqrt (+ (sqr 1.0) (sqr 0.5)))))
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
     (scene-union*
      (list
       ;; Sides
       (shape->scene
        (make-triangle-shape
         (vector (flvector 0.0 0.0 1.0)
                 (flvector x1 y1 -1.0)
                 (flvector x2 y2 -1.0))
         (vector (if smooth?
                     (flvector 0.0 0.0 1.0)
                     (flvector (* nx x0) (* nx y0) ny))
                 (flvector (* nx x1) (* nx y1) ny)
                 (flvector (* nx x2) (* nx y2) ny))
         c e m inside?))
       ;; Bottom
       (shape->scene
        (make-triangle-shape
         (vector (flvector x2 y2 -1.0)
                 (flvector x1 y1 -1.0)
                 (dir->flvector z-))
         (dir->flvector z-)
         c e m inside?)))))))

(: cone (->* [Pos Pos] [#:inside? Any #:segments Natural #:smooth? Any] Pict3D))
(define (cone v1 v2 #:inside? [inside? #f] #:segments [n 32] #:smooth? [smooth? #f])
  (let ([v1  (pos->flvector v1)]
        [v2  (pos->flvector v2)])
    (define t (flt3compose (translate-flt3 (flv3* (flv3+ v1 v2) 0.5))
                           (scale-flt3 (flv3* (flv3- v2 v1) 0.5))))
    (define s (standard-cone-scene (and inside? #t) n (and smooth? #t)))
    (freeze (pict3d (make-trans-scene (affine t) s)))))

;; ===================================================================================================
;; Collision detection

(: trace (-> Pict3D Pos (U Pos Dir) (U #f Pos)))
(define (trace p v1 to)
  (cond
    [(pos? to)
     (let ([v1  (pos->flvector v1)]
           [v2  (pos->flvector to)])
       (define dv (flv3- v2 v1))
       (define tmin (scene-ray-intersect (pict3d-scene p) v1 dv))
       (cond [(and tmin (<= tmin 1.0))  (flvector->pos (flv3+ v1 (flv3* dv tmin)))]
             [else  #f]))]
    [else
     (let ([v1  (pos->flvector v1)]
           [dv  (dir->flvector to)])
       (define tmin (scene-ray-intersect (pict3d-scene p) v1 dv))
       (if tmin (flvector->pos (flv3+ v1 (flv3* dv tmin))) #f))]))
