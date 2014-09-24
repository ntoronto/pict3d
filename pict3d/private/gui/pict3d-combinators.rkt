#lang racket/base

(require racket/list
         racket/match
         typed/racket/base
         math/flonum
         math/base
         "../math/flv3.rkt"
         "../math/flt3.rkt"
         "../math/flrect3.rkt"
         "../engine/scene.rkt"
         "../engine/utils.rkt"
         "../engine/types.rkt"
         "../utils.rkt"
         "pict3d-snip.rkt"
         "user-types.rkt"
         )

(provide
 ;; Parameters
 default-color
 default-emitted
 default-material
 current-color
 current-emitted
 current-material
 with-color
 with-emitted
 with-material
 ;; Bases
 get-basis
 set-basis
 remove-basis
 normal-basis
 columns->basis
 ;; Combinators
 triangle
 quad
 rectangle
 sphere
 ellipsoid
 sunlight
 light
 freeze
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
 combine
 combine*
 pin
 plane-cull
 rect-cull
 frustum-cull
 arrow
 )

;; ===================================================================================================
;; Parameters

(define default-color "white")
(define default-emitted '(0 0 0 0))
(define default-material '(0.25 0.5 0.25 0.4))

(: current-color (Parameterof User-Color FlVector))
(define current-color
  (make-parameter (->flcolor4 default-color) ->flcolor4))

(: current-emitted (Parameterof User-Color FlVector))
(define current-emitted
  (make-parameter (->flcolor4 default-emitted) ->flcolor4))

(: current-material (Parameterof (U material (List Real Real Real Real)) material))
(define current-material 
  (make-parameter (->material default-material) ->material))

(define-syntax-rule (with-color col body ...)
  (parameterize ([current-color col]) body ...))

(define-syntax-rule (with-emitted col body ...)
  (parameterize ([current-emitted col]) body ...))

(define-syntax-rule (with-material mat body ...)
  (parameterize ([current-material mat]) body ...))

;; ===================================================================================================
;; Affine bases

(: normal-basis (->* [] [User-Vector User-Vector Real User-Vector] Basis))
(define (normal-basis [origin (flvector 0.0 0.0 0.0)]
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
    (basis (flt3compose t (rotate-z-flt3 angle)))))

(: columns->basis (-> User-Vector User-Vector User-Vector User-Vector Basis))
(define (columns->basis x y z p)
  (define-values (x0 x1 x2) (flv3-values (->flv3 x)))
  (define-values (y0 y1 y2) (flv3-values (->flv3 y)))
  (define-values (z0 z1 z2) (flv3-values (->flv3 z)))
  (define-values (p0 p1 p2) (flv3-values (->flv3 p)))
  (basis (flaffine3
          (flvector x0 y0 z0 p0
                    x1 y1 z1 p1
                    x2 y2 z2 p2))))

(: basis-pre-transform (-> Basis FlAffine3- FlAffine3- Basis))
(define (basis-pre-transform p t tinv)
  (basis (flt3compose (basis-forward p) t)
         (flt3compose tinv (basis-inverse p))))

(: basis-post-transform (-> Basis FlAffine3- FlAffine3- Basis))
(define (basis-post-transform p t tinv)
  (basis (flt3compose t (basis-forward p))
         (flt3compose (basis-inverse p) tinv)))

(: bases-post-transform (-> Bases FlAffine3- FlAffine3- Bases))
(define (bases-post-transform ps t tinv)
  (for/hash : Bases ([(label p)  (in-hash ps)])
    (values label (basis-post-transform p t tinv))))

(: get-basis (->* [Pict3D] [Basis-Label] Basis))
(define (get-basis s [label null])
  (define h (pict3d-bases s))
  (if (hash-empty?* h)
      (error 'get-basis "scene has no bases")
      (hash-ref h label (λ () (error 'get-basis "scene has no basis labeled ~e" label)))))

(: set-basis (case-> (-> Pict3D Basis Pict3D)
                     (-> Pict3D Basis-Label Basis Pict3D)))
(define set-basis 
  (case-lambda
    [(s p)  (set-basis s null p)]
    [(s label p)
     (define h (pict3d-bases s))
     (pict3d (pict3d-scene s) (hash-set h label p))]))

(: remove-basis (->* [Pict3D] [Basis-Label] Pict3D))
(define (remove-basis s [label null])
  (define h (pict3d-bases s))
  (pict3d (pict3d-scene s) (hash-remove h label)))

;; ===================================================================================================
;; Constructors

(: scene->pict3d (-> Scene Pict3D))
(define (scene->pict3d scene)
  (pict3d scene (make-immutable-hash)))

(: shape->pict3d (-> Shape Pict3D))
(define (shape->pict3d s)
  (scene->pict3d (shape->scene s)))

;; ---------------------------------------------------------------------------------------------------
;; Triangle

(: triangle (->* [User-Vector User-Vector User-Vector] [(U 'front 'back 'both)] Pict3D))
(define (triangle v1 v2 v3 [face 'front])
  (define vs (vector (->flv3 v1) (->flv3 v2) (->flv3 v3)))
  (define norm (flv3polygon-normal vs))
  (shape->pict3d
   (make-triangle-shape vs
                        (if norm norm (flvector 0.0 0.0 0.0))
                        (current-color)
                        (current-emitted)
                        (current-material)
                        face)))

;; ---------------------------------------------------------------------------------------------------
;; Quad

(: quad (->* [User-Vector User-Vector User-Vector User-Vector] [(U 'front 'back 'both)] Pict3D))
(define (quad v1 v2 v3 v4 [face 'front])
  (define vs (vector (->flv3 v1) (->flv3 v2) (->flv3 v3) (->flv3 v4)))
  (define norm (flv3polygon-normal vs))
  (shape->pict3d
   (make-quad-shape vs
                    (if norm norm (flvector 0.0 0.0 0.0))
                    (current-color)
                    (current-emitted)
                    (current-material)
                    face)))

;; ---------------------------------------------------------------------------------------------------
;; Rectangle

(: rectangle (->* [User-Vector User-Vector] [(U 'front 'back 'both)] Pict3D))
(define (rectangle v1 v2 [face 'front])
  (shape->pict3d
   (make-rectangle-shape (assert (flv3rect (vector (->flv3 v1) (->flv3 v2))) nonempty-flrect3?)
                         (current-color)
                         (current-emitted)
                         (current-material)
                         face)))

;; ---------------------------------------------------------------------------------------------------
;; Ellipsoid

(: sphere (->* [User-Vector Real] [Any] Pict3D))
(define (sphere center radius [inside? #f])
  (define r (fl radius))
  (define t (flt3compose (translate-flt3 (->flv3 center))
                         (scale-flt3 (flvector r r r))))
  (shape->pict3d
   (make-sphere-shape t
                      (current-color)
                      (current-emitted)
                      (current-material)
                      (and inside? #t))))
  
(: ellipsoid (->* [User-Vector User-Vector] [Any] Pict3D))
(define (ellipsoid v1 v2 [inside? #f])
  (let ([v1  (->flv3 v1)]
        [v2  (->flv3 v2)])
    (define t (flt3compose (translate-flt3 (flv3* (flv3+ v1 v2) 0.5))
                           (scale-flt3 (flv3* (flv3- v2 v1) 0.5))))
    (shape->pict3d
     (make-sphere-shape t
                        (current-color)
                        (current-emitted)
                        (current-material)
                        (and inside? #t)))))

;; ---------------------------------------------------------------------------------------------------
;; Directional light

(: sunlight (-> User-Vector User-Color Real Pict3D))
(define (sunlight direction color intensity)
  (define dir (flv3normalize (->flv3 direction)))
  (shape->pict3d
   (make-directional-light-shape (->flcolor3 color)
                                 (fl intensity)
                                 (if dir dir (flvector 0.0 0.0 0.0)))))

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
    (shape->pict3d
     (make-point-light-shape color intensity position radius))))

;; ---------------------------------------------------------------------------------------------------
;; Efficiency

(: freeze (-> Pict3D Pict3D))
(define (freeze p)
  (define scene (pict3d-scene p))
  (if (empty-scene? scene)
      p
      (pict3d (shape->scene (make-frozen-scene-shape scene))
              (pict3d-bases p))))

;; ===================================================================================================
;; Transformations

(: pict3d-post-transform (-> Pict3D FlAffine3- FlAffine3- Pict3D))
(define (pict3d-post-transform s t tinv)
  (define scene (pict3d-scene s))
  (define ps (pict3d-bases s))
  (pict3d (scene-transform scene t tinv)
          (bases-post-transform ps t tinv)))

(: transform (case-> (-> Pict3D FlAffine3- Pict3D)
                     (-> Pict3D FlAffine3- FlAffine3- Pict3D)
                     (-> Basis FlAffine3- Basis)
                     (-> Basis FlAffine3- FlAffine3- Basis)))
(define (transform s t [tinv (flt3inverse t)])
  (if (basis? s)
      (basis-pre-transform s t tinv)
      (pict3d-post-transform s t tinv)))

;; ---------------------------------------------------------------------------------------------------
;; Scale

(: scale-x (case-> (-> Pict3D Real Pict3D)
                   (-> Basis Real Basis)))
(define (scale-x s v) (scale s (flvector (fl v) 1.0 1.0)))

(: scale-y (case-> (-> Pict3D Real Pict3D)
                   (-> Basis Real Basis)))
(define (scale-y s v) (scale s (flvector 1.0 (fl v) 1.0)))

(: scale-z (case-> (-> Pict3D Real Pict3D)
                   (-> Basis Real Basis)))
(define (scale-z s v) (scale s (flvector 1.0 1.0 (fl v))))

(: scale (case-> (-> Pict3D (U Real User-Vector) Pict3D)
                 (-> Basis (U Real User-Vector) Basis)))
(define (scale s v)
  (let ([v  (cond [(real? v)  (let ([v  (fl v)])
                                (flvector v v v))]
                  [else  (->flv3 v)])])
    (transform s (scale-flt3 v))))

;; ---------------------------------------------------------------------------------------------------
;; Translate

(: move-x (case-> (-> Pict3D Real Pict3D)
                  (-> Basis Real Basis)))
(define (move-x s v) (move s (flvector (fl v) 0.0 0.0)))

(: move-y (case-> (-> Pict3D Real Pict3D)
                  (-> Basis Real Basis)))
(define (move-y s v) (move s (flvector 0.0 (fl v) 0.0)))

(: move-z (case-> (-> Pict3D Real Pict3D)
                  (-> Basis Real Basis)))
(define (move-z s v) (move s (flvector 0.0 0.0 (fl v))))

(: move (case-> (-> Pict3D User-Vector Pict3D)
                (-> Basis User-Vector Basis)))
(define (move s v) (transform s (translate-flt3 (->flv3 v))))

;; ---------------------------------------------------------------------------------------------------
;; Rotate

(: rotate-x (case-> (-> Pict3D Real Pict3D)
                    (-> Basis Real Basis)))
(define (rotate-x s a) (rotate s (flvector 1.0 0.0 0.0) a))

(: rotate-y (case-> (-> Pict3D Real Pict3D)
                    (-> Basis Real Basis)))
(define (rotate-y s a) (rotate s (flvector 0.0 1.0 0.0) a))

(: rotate-z (case-> (-> Pict3D Real Pict3D)
                    (-> Basis Real Basis)))
(define (rotate-z s a) (rotate s (flvector 0.0 0.0 1.0) a))

(: rotate (case-> (-> Pict3D User-Vector Real Pict3D)
                  (-> Basis User-Vector Real Basis)))
(define (rotate s v a)
  (let ([v  (flv3normalize (->flv3 v))])
    (cond [v
           (transform s (rotate-flt3 (->flv3 v) (degrees->radians (fl a))))]
          [else
           (raise-argument-error 'rotate "nonzero direction vector" 1 s v a)])))

;; ===================================================================================================
;; Combining scenes (i.e. union)

(: combine* (-> (Listof Pict3D) Pict3D))
(define (combine* ss)
  (pict3d (scene-union* (map pict3d-scene ss))
          (for/fold ([ps : Bases  (make-immutable-hash)]) ([s  (in-list ss)])
            (hash-merge ps (pict3d-bases s)))))

(: combine (-> Pict3D * Pict3D))
(define (combine . ss)
  (combine* ss))

(: pin (->* [Pict3D Pict3D] [Basis-Label Basis-Label] Pict3D))
(define (pin s1 s2 [label1 null] [label2 null])
  (define p1 (get-basis s1 label1))
  (define p2 (get-basis s2 label2))
  (define scene1 (pict3d-scene s1))
  (define scene2 (pict3d-scene s2))
  (define t1 (basis-forward p1))
  (define t2 (basis-forward p2))
  (define t (flt3compose t1 (flt3inverse t2)))
  (define tinv (flt3compose t2 (flt3inverse t1)))
  (pict3d
   (scene-union scene1 (scene-transform scene2 t tinv))
   (hash-merge (hash-remove (pict3d-bases s1) label1)
               (bases-post-transform (hash-remove (pict3d-bases s2) label2) t tinv))))

;; ===================================================================================================
;; Testing combinators

(: plane-cull (-> Pict3D FlPlane3 Pict3D))
(define (plane-cull s p)
  (pict3d (scene-plane-cull (pict3d-scene s) p)
          (pict3d-bases s)))

(: rect-cull (-> Pict3D FlRect3 Pict3D))
(define (rect-cull s b)
  (pict3d (scene-rect-cull (pict3d-scene s) b)
          (pict3d-bases s)))

(: frustum-cull (-> Pict3D FlTransform3 Pict3D))
(define (frustum-cull s t)
  (pict3d (scene-frustum-cull (pict3d-scene s) t)
          (pict3d-bases s)))

;; ===================================================================================================
;; Arrows

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

(: direction-basis (-> User-Vector User-Vector Basis))
(define (direction-basis origin z-axis)
  (let* ([origin  (->flv3 origin)]
         [z-axis  (->flv3 z-axis)])
    (define x-axis (flv3normalize (flv3cross z-axis (flvector 0.0 0.0 1.0))))
    (cond
      [x-axis
       (define y-axis (assert (flv3normalize (flv3cross z-axis x-axis)) values))
       (columns->basis x-axis y-axis z-axis origin)]
      [(>= (flvector-ref z-axis 2) 0.0)
       (columns->basis (flvector 1.0 0.0 0.0)
                       (flvector 0.0 1.0 0.0)
                       (flvector 0.0 0.0 1.0)
                       origin)]
      [else
       (columns->basis (flvector -1.0 0.0  0.0)
                       (flvector  0.0 1.0  0.0)
                       (flvector  0.0 0.0 -1.0)
                       origin)])))

(: arrow (case-> (-> User-Vector Pict3D)
                 (-> User-Vector User-Vector Pict3D)))
(define arrow
  (case-lambda
    [(end)  (arrow '(0 0 0) end)]
    [(v0 v1)  (transform (make-up-arrow) (basis-forward (direction-basis v0 v1)))]))
