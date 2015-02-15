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
         "../engine/types.rkt"
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
 set-origin
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
 point-at
 point-to
 columns
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
 arrow
 )

;; ===================================================================================================
;; Parameters

(define default-color "white")
(define default-emitted '(0 0 0 0))
(define default-material '(0.05 0.6 0.35 0.3))

(: current-color (Parameterof User-Color FlVector))
(define current-color
  (make-parameter (->flcolor4 default-color) ->flcolor4))

(: current-emitted (Parameterof User-Color FlVector))
(define current-emitted
  (make-parameter (->flcolor4 default-emitted) ->flcolor4))

(: current-material (Parameterof User-Material material))
(define current-material 
  (make-parameter (->material default-material) ->material))

(: set-color (-> Pict3D User-Color Pict3D))
(define (set-color p c)
  (let ([c  (->flcolor4 c)])
    (pict3d (scene-map-shapes (pict3d-scene p) (λ (a) (shape-set-color a c))))))

(: set-emitted (-> Pict3D User-Color Pict3D))
(define (set-emitted p e)
  (let ([e  (->flcolor4 e)])
    (pict3d (scene-map-shapes (pict3d-scene p) (λ (a) (shape-set-emitted a e))))))

(: set-material (-> Pict3D User-Material Pict3D))
(define (set-material p m)
  (let ([m  (->material m)])
    (pict3d (scene-map-shapes (pict3d-scene p) (λ (a) (shape-set-material a m))))))

;; ===================================================================================================
;; Naming, mapping, finding

(: group (-> Pict3D User-Name Pict3D))
(define (group p n)
  (pict3d (make-group-scene n (pict3d-scene p))))

(: basis (-> User-Name FlAffine3- Pict3D))
(define (basis n t)
  (pict3d (make-trans-scene t (make-group-scene n empty-scene))))

(: group? (-> Pict3D Boolean))
(define (group? p)
  (group-scene? (pict3d-scene p)))

(: group-name (-> Pict3D User-Name))
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

(: make-replace (-> (-> Scene User-Name (-> Scene Scene) Scene)
                    (-> Pict3D (U User-Name (Listof+1 User-Name)) (-> Pict3D Pict3D) Pict3D)))
(define ((make-replace g) p n f)
  (if (pair? n)
      (if (empty? (cdr n))
          ((make-replace g) p (car n) f)
          ((make-replace g) p (car n) (λ (p) ((make-replace g) p (cdr n) f))))
      (pict3d (g (pict3d-scene p) n (λ (s) (pict3d-scene (f (pict3d s))))))))

(define replace-group (make-replace scene-replace-group))
(define replace-in-group (make-replace scene-replace-in-group))

(: ungroup (-> Pict3D (U User-Name (Listof+1 User-Name)) Pict3D))
(define (ungroup p n)
  (replace-group p n group-contents))

(: remove-group (-> Pict3D (U User-Name (Listof+1 User-Name)) Pict3D))
(define (remove-group p n)
  (replace-group p n (λ (_) empty-pict3d)))

(: remove-in-group (-> Pict3D (U User-Name (Listof+1 User-Name)) Pict3D))
(define (remove-in-group p n)
  (replace-in-group p n (λ (_) empty-pict3d)))

(: map-group* (All (A) (-> Pict3D User-Name (-> Pict3D A) (Listof A))))
(define (map-group* p n f)
  (scene-map-group (pict3d-scene p) n (λ ([s : group-scene]) (f (pict3d s)))))

(: map-group (All (A) (-> Pict3D (U User-Name (Listof+1 User-Name)) (-> Pict3D A) (Listof A))))
(define (map-group p n f)
  (if (pair? n)
      (if (empty? (cdr n))
          (map-group* p (car n) f)
          (append* (map-group* p (car n) (λ ([p : Pict3D]) (map-group p (cdr n) f)))))
      (map-group* p n f)))

(: map-group/transform* (All (A) (-> Pict3D User-Name (-> FlAffine3- Pict3D A) (Listof A))))
(define (map-group/transform* p n f)
  (scene-map-group/transform (pict3d-scene p) n
                             (λ ([t : FlAffine3-] [s : group-scene]) (f t (pict3d s)))))

(: map-group/transform (All (A) (-> Pict3D
                                    (U User-Name (Listof+1 User-Name))
                                    (-> FlAffine3- Pict3D A)
                                    (Listof A))))
(define (map-group/transform p n f)
  (if (pair? n)
      (if (empty? (cdr n))
          (map-group/transform* p (car n) f)
          (append* (map-group/transform*
                    p (car n)
                    (λ ([t : FlAffine3-] [p : Pict3D])
                      (map-group/transform
                       p (cdr n)
                       (λ ([t0 : FlAffine3-] [p : Pict3D])
                         (f (flt3compose t t0) p)))))))
      (map-group/transform* p n f)))

(: set-origin (-> Pict3D (U User-Name (Listof+1 User-Name)) Pict3D))
(define (set-origin p n)
  (: fail (-> Index Nothing))
  (define (fail m)
    (error 'set-origin "epected one group named ~e; given a Pict3D with ~a groups named ~e" n m n))
  (define ps (map-group/transform p n (λ ([t : FlAffine3-] _) (transform p (flt3inverse t)))))
  (cond [(empty? ps)  (fail 0)]
        [(empty? (rest ps))  (first ps)]
        [else  (fail (length ps))]))

;; ===================================================================================================
;; Basic shapes

;; ---------------------------------------------------------------------------------------------------
;; Triangle

(: triangle (->* [User-Vector User-Vector User-Vector] [Any] Pict3D))
(define (triangle v1 v2 v3 [back? #f])
  (define vs (vector (->flv3 v1) (->flv3 v2) (->flv3 v3)))
  (define norm (flv3polygon-normal vs))
  (pict3d
   (shape->scene
    (make-triangle-shape vs
                         (if norm norm (flvector 0.0 0.0 0.0))
                         (current-color)
                         (current-emitted)
                         (current-material)
                         (and back? #t)))))

;; ---------------------------------------------------------------------------------------------------
;; Quad

(: quad (->* [User-Vector User-Vector User-Vector User-Vector] [Any] Pict3D))
(define (quad v1 v2 v3 v4 [back? #f])
  (define vs (vector (->flv3 v1) (->flv3 v2) (->flv3 v3) (->flv3 v4)))
  (define norm (flv3polygon-normal vs))
  (pict3d
   (scene-union*
    (map
     shape->scene
     (make-quad-shapes vs
                       (if norm norm (flvector 0.0 0.0 0.0))
                       (current-color)
                       (current-emitted)
                       (current-material)
                       (and back? #t))))))

;; ---------------------------------------------------------------------------------------------------
;; Rectangle

(: rectangle (->* [User-Vector User-Vector] [Any] Pict3D))
(define (rectangle v1 v2 [inside? #f])
  (pict3d
   (shape->scene
    (make-rectangle-shape (assert (flv3rect (vector (->flv3 v1) (->flv3 v2))) nonempty-flrect3?)
                          (current-color)
                          (current-emitted)
                          (current-material)
                          (and inside? #t)))))

;; ---------------------------------------------------------------------------------------------------
;; Ellipsoid

(: sphere (->* [User-Vector Real] [Any] Pict3D))
(define (sphere center radius [inside? #f])
  (define r (fl radius))
  (define t (flt3compose (translate-flt3 (->flv3 center))
                         (scale-flt3 (flvector r r r))))
  (pict3d
   (shape->scene
    (make-sphere-shape t
                       (current-color)
                       (current-emitted)
                       (current-material)
                       (and inside? #t)))))

(: ellipsoid (->* [User-Vector User-Vector] [Any] Pict3D))
(define (ellipsoid v1 v2 [inside? #f])
  (let ([v1  (->flv3 v1)]
        [v2  (->flv3 v2)])
    (define t (flt3compose (translate-flt3 (flv3* (flv3+ v1 v2) 0.5))
                           (scale-flt3 (flv3* (flv3- v2 v1) 0.5))))
    (pict3d
     (shape->scene
      (make-sphere-shape t
                         (current-color)
                         (current-emitted)
                         (current-material)
                         (and inside? #t))))))

;; ---------------------------------------------------------------------------------------------------
;; Directional light

(: sunlight (-> User-Vector User-Color Real Pict3D))
(define (sunlight direction color intensity)
  (define dir (flv3normalize (->flv3 direction)))
  (pict3d
   (shape->scene
    (make-directional-light-shape (->flcolor3 color)
                                  (fl intensity)
                                  (if dir dir (flvector 0.0 0.0 0.0))))))

;; ---------------------------------------------------------------------------------------------------
;; Point light

(: default-light-radius (-> Flonum Flonum))
(define (default-light-radius intensity)
  (flsqrt (* 20.0 intensity)))

(: light (->* [User-Vector] [User-Color Real Real] Pict3D))
(define (light position
               [color  (flvector 1.0 1.0 1.0)]
               [intensity  1.0]
               [radius  #f])
  (let* ([position  (->flv3 position)]
         [color  (->flcolor3 color)]
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

(: transform (-> Pict3D FlAffine3- Pict3D))
(define (transform s t)
  (pict3d (make-trans-scene t (pict3d-scene s))))

(: make-transformer (All (A) (-> (-> A FlAffine3-)
                                 (case-> (-> A FlAffine3-)
                                         (-> Pict3D A Pict3D)))))
(define (make-transformer f)
  (case-lambda
    [(v)  (f v)]
    [(p v)  (transform p (f v))]))

;; ---------------------------------------------------------------------------------------------------
;; Scale

(: check-scale (-> Symbol (U Real User-Vector) FlVector))
(define (check-scale name v)
  (cond [(real? v)
         (let ([v  (fl v)])
           (cond [(= v 0.0)  (raise-argument-error name "nonzero scale" v)]
                 [else  (flvector v v v)]))]
        [else
         (let ([v  (->flv3 v)])
           (define-values (x y z) (flv3-values v))
           (cond [(or (= x 0.0) (= y 0.0) (= z 0.0))
                  (raise-argument-error name "nonzero scale" v)]
                 [else  v]))]))

(define scale-x
  (make-transformer (λ ([v : Real]) (scale-flt3 (check-scale 'scale-x (flvector (fl v) 1.0 1.0))))))

(define scale-y
  (make-transformer (λ ([v : Real]) (scale-flt3 (check-scale 'scale-y (flvector 1.0 (fl v) 1.0))))))

(define scale-z
  (make-transformer (λ ([v : Real]) (scale-flt3 (check-scale 'scale-z (flvector 1.0 1.0 (fl v)))))))

(define scale
  (make-transformer (λ ([v : (U Real User-Vector)]) (scale-flt3 (check-scale 'scale v)))))

;; ---------------------------------------------------------------------------------------------------
;; Translate

(define move-x (make-transformer (λ ([v : Real]) (translate-flt3 (flvector (fl v) 0.0 0.0)))))
(define move-y (make-transformer (λ ([v : Real]) (translate-flt3 (flvector 0.0 (fl v) 0.0)))))
(define move-z (make-transformer (λ ([v : Real]) (translate-flt3 (flvector 0.0 0.0 (fl v))))))
(define move (make-transformer (λ ([v : User-Vector]) (translate-flt3 (->flv3 v)))))

;; ---------------------------------------------------------------------------------------------------
;; Rotate

(: check-axis (-> Symbol User-Vector FlVector))
(define (check-axis name v)
  (let ([v  (flv3normalize (->flv3 v))])
    (if v v (raise-argument-error name "nonzero axis vector" v))))

(define rotate-x
  (make-transformer (λ ([a : Real]) (rotate-flt3 (flvector 1.0 0.0 0.0) (degrees->radians (fl a))))))

(define rotate-y
  (make-transformer (λ ([a : Real]) (rotate-flt3 (flvector 0.0 1.0 0.0) (degrees->radians (fl a))))))

(define rotate-z
  (make-transformer (λ ([a : Real]) (rotate-flt3 (flvector 0.0 0.0 1.0) (degrees->radians (fl a))))))

(: rotate (case-> (-> User-Vector Real FlAffine3-)
                  (-> Pict3D User-Vector Real Pict3D)))
(define rotate
  (case-lambda
    [(v a)  (rotate-flt3 (check-axis 'rotate v) (degrees->radians (fl a)))]
    [(p v a)  (transform p (rotate v a))]))

;; ---------------------------------------------------------------------------------------------------

(: point-at (->* [] [User-Vector User-Vector Real User-Vector] FlAffine3-))
(define (point-at [origin (flvector 0.0 0.0 0.0)]
                  [z-axis (flvector 0.0 0.0 1.0)]
                  [angle 0.0]
                  [up (flvector 0.0 0.0 1.0)])
  (let* ([origin  (->flv3 origin)]
         [z-axis  (flv3normalize (->flv3 z-axis))]
         [z-axis  (if z-axis z-axis (flvector 0.0 0.0 1.0))]
         [angle  (degrees->radians (fl angle))]
         [up  (flv3normalize (->flv3 up))]
         [up  (if up up (flvector 0.0 0.0 1.0))])
    (define x-axis (flv3normalize (flv3cross z-axis up)))
    (define t
      (cond
        [x-axis
         (define y-axis (flv3cross z-axis x-axis))
         (basis->flaffine3 x-axis y-axis z-axis origin)]
        [(>= (flvector-ref z-axis 2) 0.0)
         (translate-flt3 origin)]
        [else
         (flt3compose (translate-flt3 origin)
                      (scale-flt3 (flvector -1.0 1.0 -1.0)))]))
    (flt3compose t (rotate-z-flt3 angle))))

;; ---------------------------------------------------------------------------------------------------

(: point-to (->* [] [User-Vector User-Vector Real User-Vector] FlAffine3-))
(define (point-to [origin (flvector 0.0 0.0 0.0)]
                  [z-axis (flvector 0.0 0.0 1.0)]
                  [angle 0.0]
                  [up (flvector 0.0 0.0 1.0)])
  (let ([z-axis  (->flv3 z-axis)])
    (define t (point-at origin z-axis angle up))
    (flt3compose t (scale-flt3 (flvector 1.0 1.0 (flv3mag z-axis))))))

;; ---------------------------------------------------------------------------------------------------

(: columns (-> User-Vector User-Vector User-Vector User-Vector FlAffine3-))
(define (columns x y z p)
  (basis->flaffine3 (->flv3 x) (->flv3 y) (->flv3 z) (->flv3 p)))

;; ===================================================================================================
;; Combining scenes (i.e. union)

(: combine* (-> (Listof Pict3D) Pict3D))
(define (combine* ps)
  (cond [(empty? ps)  empty-pict3d]
        [else  (pict3d (scene-union* (map pict3d-scene ps)))]))

(: combine (-> Pict3D * Pict3D))
(define (combine . ps) (combine* ps))

(: pin (->* [Pict3D (U User-Name (Listof+1 User-Name)) Pict3D] [(U User-Name (Listof+1 User-Name))]
            Pict3D))
(define (pin p1 n1 p2 [n2 #f])
  (let ([p2  (if n2 (ungroup (set-origin p2 n2) n2) p2)])
    (replace-in-group p1 n1 (λ ([p : Pict3D]) (combine p p2)))))

(: weld (->* [Pict3D User-Name Pict3D] [User-Name] Pict3D))
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

(define (make-up-arrow)
  (freeze
   (combine
    (rectangle '(-1/64 -1/64 0)
               '(1/64 1/64 56/64))
    (let ([p  (triangle '(2/64 2/64 56/64)
                        '(-2/64 2/64 56/64)
                        '(0 0 1))])
      (combine p (rotate-z p 90) (rotate-z p 180) (rotate-z p 270)))
    (quad '(2/64 2/64 56/64)
          '(2/64 -2/64 56/64)
          '(-2/64 -2/64 56/64)
          '(-2/64 2/64 56/64)))))

(: arrow (case-> (-> User-Vector Pict3D)
                 (-> User-Vector User-Vector Pict3D)))
(define arrow
  (case-lambda
    [(end)  (arrow '(0 0 0) end)]
    [(v0 v1)  (transform (make-up-arrow) (point-to v0 v1))]))
