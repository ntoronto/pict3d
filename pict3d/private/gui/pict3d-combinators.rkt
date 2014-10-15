#lang typed/racket/base

(require racket/list
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
         "basis.rkt"
         "pict3d-struct.rkt"
         )

(provide
 ;; Parameters
 default-color
 default-emitted
 default-material
 current-color
 current-emitted
 current-material
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
 scale-basis-x
 scale-basis-y
 scale-basis-z
 scale-basis
 rotate-x
 rotate-y
 rotate-z
 rotate
 rotate-basis-x
 rotate-basis-y
 rotate-basis-z
 rotate-basis
 move-x
 move-y
 move-z
 move
 move-basis-x
 move-basis-y
 move-basis-z
 move-basis
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
  (basis (basis->flaffine3 (->flv3 x) (->flv3 y) (->flv3 z) (->flv3 p))))

(: basis-pre-transform (-> Basis FlAffine3- Basis))
(define (basis-pre-transform p t)
  (basis (flt3compose (basis-transform p) t)))

(: basis-post-transform (-> Basis FlAffine3- Basis))
(define (basis-post-transform p t)
  (basis (flt3compose t (basis-transform p))))

(: bases-post-transform (-> Bases FlAffine3- Bases))
(define (bases-post-transform bases t)
  (for/list : Bases ([kv  (in-list bases)])
    (match-define (cons label p) kv)
    (cons label (basis-post-transform p t))))

(: get-basis (->* [Pict3D] [Symbol] Basis))
(define (get-basis s [label 'default])
  (define h (pict3d-bases s))
  (if (empty? h)
      (error 'get-basis "scene has no bases")
      (list-hasheq-ref h label (λ () (error 'get-basis "scene has no basis labeled ~e" label)))))

(: set-basis (case-> (-> Pict3D Basis Pict3D)
                     (-> Pict3D Symbol Basis Pict3D)))
(define set-basis
  (case-lambda
    [(s p)  (set-basis s 'default p)]
    [(s label p)
     (define h (pict3d-bases s))
     (pict3d (pict3d-scene s) (list-hasheq-set h label p))]))

(: remove-basis (->* [Pict3D] [Symbol] Pict3D))
(define (remove-basis s [label 'default])
  (define h (pict3d-bases s))
  (pict3d (pict3d-scene s) (list-hasheq-remove h label)))

;; ===================================================================================================
;; Constructors

(: scene->pict3d (-> Scene Pict3D))
(define (scene->pict3d scene)
  (pict3d scene empty))

(: shape->pict3d (-> Shape Pict3D))
(define (shape->pict3d s)
  (scene->pict3d (shape->scene s)))

;; ---------------------------------------------------------------------------------------------------
;; Triangle

(: triangle (->* [User-Vector User-Vector User-Vector] [Any] Pict3D))
(define (triangle v1 v2 v3 [back? #f])
  (define vs (vector (->flv3 v1) (->flv3 v2) (->flv3 v3)))
  (define norm (flv3polygon-normal vs))
  (shape->pict3d
   (make-triangle-shape vs
                        (if norm norm (flvector 0.0 0.0 0.0))
                        (current-color)
                        (current-emitted)
                        (current-material)
                        (and back? #t))))

;; ---------------------------------------------------------------------------------------------------
;; Quad

(: quad (->* [User-Vector User-Vector User-Vector User-Vector] [Any] Pict3D))
(define (quad v1 v2 v3 v4 [back? #f])
  (define vs (vector (->flv3 v1) (->flv3 v2) (->flv3 v3) (->flv3 v4)))
  (define norm (flv3polygon-normal vs))
  (scene->pict3d
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
  (shape->pict3d
   (make-rectangle-shape (assert (flv3rect (vector (->flv3 v1) (->flv3 v2))) nonempty-flrect3?)
                         (current-color)
                         (current-emitted)
                         (current-material)
                         (and inside? #t))))

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

(: pict3d-post-transform (-> Pict3D FlAffine3- Pict3D))
(define (pict3d-post-transform s t)
  (define scene (pict3d-scene s))
  (define h (pict3d-bases s))
  (pict3d (scene-transform scene t)
          (and h (bases-post-transform h t))))

(: transform (-> Pict3D FlAffine3- Pict3D))
(define (transform s t)
  (pict3d-post-transform s t))

(: transform-basis (-> Basis FlAffine3- Basis))
(define (transform-basis s t)
  (basis-pre-transform s t))

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

(: scale-x (-> Pict3D Real Pict3D))
(: scale-y (-> Pict3D Real Pict3D))
(: scale-z (-> Pict3D Real Pict3D))
(: scale (-> Pict3D (U Real User-Vector) Pict3D))

(define (scale-x s v) (scale s (flvector (fl v) 1.0 1.0)))
(define (scale-y s v) (scale s (flvector 1.0 (fl v) 1.0)))
(define (scale-z s v) (scale s (flvector 1.0 1.0 (fl v))))
(define (scale s v) (transform s (scale-flt3 (check-scale 'scale v))))

(: scale-basis-x (-> Basis Real Basis))
(: scale-basis-y (-> Basis Real Basis))
(: scale-basis-z (-> Basis Real Basis))
(: scale-basis (-> Basis (U Real User-Vector) Basis))

(define (scale-basis-x s v) (scale-basis s (flvector (fl v) 1.0 1.0)))
(define (scale-basis-y s v) (scale-basis s (flvector 1.0 (fl v) 1.0)))
(define (scale-basis-z s v) (scale-basis s (flvector 1.0 1.0 (fl v))))
(define (scale-basis s v) (transform-basis s (scale-flt3 (check-scale 'scale-basis v))))

;; ---------------------------------------------------------------------------------------------------
;; Translate

(: move-x (-> Pict3D Real Pict3D))
(: move-y (-> Pict3D Real Pict3D))
(: move-z (-> Pict3D Real Pict3D))
(: move (-> Pict3D User-Vector Pict3D))

(define (move-x s v) (move s (flvector (fl v) 0.0 0.0)))
(define (move-y s v) (move s (flvector 0.0 (fl v) 0.0)))
(define (move-z s v) (move s (flvector 0.0 0.0 (fl v))))
(define (move s v) (transform s (translate-flt3 (->flv3 v))))

(: move-basis-x (-> Basis Real Basis))
(: move-basis-y (-> Basis Real Basis))
(: move-basis-z (-> Basis Real Basis))
(: move-basis (-> Basis User-Vector Basis))

(define (move-basis-x s v) (move-basis s (flvector (fl v) 0.0 0.0)))
(define (move-basis-y s v) (move-basis s (flvector 0.0 (fl v) 0.0)))
(define (move-basis-z s v) (move-basis s (flvector 0.0 0.0 (fl v))))
(define (move-basis s v) (transform-basis s (translate-flt3 (->flv3 v))))

;; ---------------------------------------------------------------------------------------------------
;; Rotate

(: check-axis (-> Symbol User-Vector FlVector))
(define (check-axis name v)
  (let ([v  (flv3normalize (->flv3 v))])
    (if v v (raise-argument-error name "nonzero direction vector" v))))

(: rotate-x (-> Pict3D Real Pict3D))
(: rotate-y (-> Pict3D Real Pict3D))
(: rotate-z (-> Pict3D Real Pict3D))
(: rotate (-> Pict3D User-Vector Real Pict3D))

(define (rotate-x s a) (rotate s (flvector 1.0 0.0 0.0) a))
(define (rotate-y s a) (rotate s (flvector 0.0 1.0 0.0) a))
(define (rotate-z s a) (rotate s (flvector 0.0 0.0 1.0) a))
(define (rotate s v a)
  (transform s (rotate-flt3 (check-axis 'rotate v) (degrees->radians (fl a)))))

(: rotate-basis-x (-> Basis Real Basis))
(: rotate-basis-y (-> Basis Real Basis))
(: rotate-basis-z (-> Basis Real Basis))
(: rotate-basis (-> Basis User-Vector Real Basis))

(define (rotate-basis-x s a) (rotate-basis s (flvector 1.0 0.0 0.0) a))
(define (rotate-basis-y s a) (rotate-basis s (flvector 0.0 1.0 0.0) a))
(define (rotate-basis-z s a) (rotate-basis s (flvector 0.0 0.0 1.0) a))
(define (rotate-basis s v a)
  (transform-basis s (rotate-flt3 (check-axis 'rotate-basis v) (degrees->radians (fl a)))))

;; ===================================================================================================
;; Combining scenes (i.e. union)

(: combine* (-> (Listof Pict3D) Pict3D))
(define (combine* ss)
  (cond [(empty? ss)  empty-pict3d]
        [else
         (pict3d (scene-union* (map pict3d-scene ss))
                 ((inst remove-duplicates (Pair Symbol Basis) Symbol)
                  (append* (reverse (map pict3d-bases ss)))
                  eq?
                  #:key car))]))

(: combine (-> Pict3D * Pict3D))
(define (combine . ss)
  (combine* ss))

(: pin (->* [Pict3D Pict3D] [Symbol Symbol] Pict3D))
(define (pin s1 s2 [label1 'default] [label2 'default])
  (define p1 (get-basis s1 label1))
  (define p2 (get-basis s2 label2))
  (define t1 (basis-transform p1))
  (define t2 (basis-transform p2))
  (define t (flt3compose t1 (flt3inverse t2)))
  (define scene1 (pict3d-scene s1))
  (define scene2 (pict3d-scene s2))
  (define h1 (assert (pict3d-bases s1) values))
  (define h2 (assert (pict3d-bases s2) values))
  (pict3d
   (scene-union scene1 (scene-transform scene2 t))
   (list-hasheq-merge (list-hasheq-remove h1 label1)
                      (bases-post-transform (list-hasheq-remove h2 label2) t))))

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
    [(v0 v1)  (transform (make-up-arrow) (basis-transform (direction-basis v0 v1)))]))
