#lang typed/racket/base

(require racket/list
         racket/match
         math/flonum
         math/base
         "../math.rkt"
         "../engine.rkt"
         "../soup.rkt"
         "../shape.rkt"
         "shape/point-light-shell.rkt"
         "parameters.rkt"
         "user-types.rkt"
         (only-in "typed-user-types.rkt"
                  flv3->pos
                  make-vertex
                  make-material
                  flaffine3->affine)
         "typed-pict3d-combinators.rkt"
         "pict3d-struct.rkt")

(provide standard-over-light
         standard-under-light
         scene-light-indicators
         scene-origin-indicator
         scene-basis-indicators
         group-box
         group-boxes)

(define standard-over-light
  (sunlight (dir -0.25 -0.5 -1.0) (emitted 1.0)))

(define standard-under-light
  (sunlight (dir +0.25 +0.5 +1.0) (emitted 0.5)))

(: unit-octahedron-vertices (Listof (Listof Dir)))
(define unit-octahedron-vertices
  (list (list +x +y +z)
        (list +y -x +z)
        (list -x -y +z)
        (list -y +x +z)
        (list +y +x -z)
        (list -x +y -z)
        (list -y -x -z)
        (list +x -y -z)))

(: unit-octahedron (-> RGBA Emitted Material Boolean Pict3D))
(define (unit-octahedron c e m inside?)
  (combine
   (map (λ ([dvs : (Listof Dir)])
          (match-define (list dv1 dv2 dv3) dvs)
          (triangle
           (make-vertex (pos+ origin dv1) #f c e m)
           (make-vertex (pos+ origin dv2) #f c e m)
           (make-vertex (pos+ origin dv3) #f c e m)
           #:back? inside?))
        unit-octahedron-vertices)))

(: scene-all-point-lights (-> Scene (Listof point-light-shape)))
(define (scene-all-point-lights s)
  (let loop ([s s] [t  identity-flaffine3])
    (: lights (Listof point-light-shape))
    (define lights empty)
    
    (scene-for-each!
     s
     empty  ; no culling planes
     (λ ([s : shape] [t0 : FlAffine3] [c : Nonnegative-Fixnum])
       (cond [(point-light-shape? s)
              (let ([s  (assert (shape-fast-transform s (flt3compose t t0)) point-light-shape?)])
                (set! lights (cons s lights)))]
             [(frozen-scene-shape? s)
              (let ([ss  (loop (frozen-scene-shape-scene s) (flt3compose t t0))])
                (set! lights (append ss lights)))]))
     0)  ; don't care about starting index
    lights))

(define black-color (rgba 0.0 0.0 0.0 1.0))
(define ambient-material (make-material 1.0 0.0 0.0 1.0))

(: point-light-indicator (-> point-light-shape Pict3D))
(define (point-light-indicator a)
  (match-define (point-light-shape _ _ t0 e0 qmax rmin rmax) a)
  ;; i = intensity
  (define i (flv4-ref e0 3))
  ;; e = emitted color of octahedron
  (define e (call/flv4-values e0 (λ (r g b _) (emitted r g b 2.0))))  ; 2.0 looks nice and glowy
  ;; s = radius of a ball that would give off that much light...
  (define s (* (flsqrt (/ i 2.0)) #i1/8))  ; ... but 1/8 size - it would look too big otherwise
  ;; t = transform to put octahedron in place
  (define t (affine-compose (flaffine3->affine t0) (scale s)))
  (freeze
   (combine
    (pict3d (make-point-light-shell-shape t0 e0 qmax rmin rmax))
    (transform (unit-octahedron black-color e ambient-material #f) t))))

(: scene-light-indicators (-> Scene (Listof Pict3D)))
(define (scene-light-indicators s)
  (map point-light-indicator (scene-all-point-lights s)))

(define axis-material (make-material 0.1 0.2 0.7 0.3))

(define x-axis
  (parameterize ([current-color     black-color]
                 [current-emitted   (emitted 1.0 0.05 0.05 2.0)]
                 [current-material  axis-material])
    (arrow origin +x)))

(define y-axis
  (parameterize ([current-color     black-color]
                 [current-emitted   (emitted 0.0 1.0 0.0 1.75)]
                 [current-material  axis-material])
    (arrow origin +y)))

(define z-axis
  (parameterize ([current-color     black-color]
                 [current-emitted   (emitted 0.1 0.1 1.0 2.5)]
                 [current-material  axis-material])
    (arrow origin +z)))

(define axes
  (freeze
   (combine
    (parameterize ([current-color     black-color]
                   [current-emitted   (emitted 1 1 1 2)]
                   [current-material  axis-material])
      (sphere origin 0.03))
    x-axis
    y-axis
    z-axis)))

(define basis-dim 0.5)

(define x-basis-axis
  (parameterize ([current-color     black-color]
                 [current-emitted   (emitted 0.5 0.025 0.025 1.0)]
                 [current-material  axis-material])
    (scale (arrow origin +x) (dir 1.0 0.5 0.5))))

(define y-basis-axis
  (parameterize ([current-color     black-color]
                 [current-emitted   (emitted 0.0 0.5 0.0 1.0)]
                 [current-material  axis-material])
    (scale (arrow origin +y) (dir 0.5 1.0 0.5))))

(define z-basis-axis
  (parameterize ([current-color     black-color]
                 [current-emitted   (emitted 0.05 0.05 0.5 1.0)]
                 [current-material  axis-material])
    (scale (arrow origin +z) (dir 0.5 0.5 1.0))))

(define basis-axes
  (freeze
   (combine
    (parameterize ([current-color     black-color]
                   [current-emitted   (emitted 1 1 1 1)]
                   [current-material  axis-material])
      (sphere origin 0.015))
    x-basis-axis
    y-basis-axis
    z-basis-axis)))

(: scene-origin-indicator (-> Flonum Pict3D))
(define (scene-origin-indicator s)
  (transform axes (scale s)))

(: scene-basis-indicators (-> Scene Flonum (Listof (Pair Pos Pict3D))))
(define (scene-basis-indicators scene s)
  (define t (scale s))
  (map (λ ([nt : (Pair Tag FlAffine3)])
         (define t0 (flaffine3->affine (cdr nt)))
         (cons (flv3->pos (affine-origin t0))
               (transform basis-axes (affine-compose t0 t))))
       (scene-group-transforms scene 'all)))

(define (make-group-box)
  (: make-face (-> RGBA Emitted Material Positive-Flonum Boolean Pict3D))
  (define (make-face cc ce cm width back?)
    (define vtx1 (vtx (flv3 1.0 1.0 1.0) +z cc ce cm))
    (define vtx2 (vtx (flv3 -1.0 1.0 1.0) +z cc ce cm))
    (define vtx3 (vtx (flv3 1.0 -1.0 1.0) +z cc ce cm))
    (define vtx4 (vtx (flv3 -1.0 -1.0 1.0) +z cc ce cm))
    (define ts
      (list identity-affine
            (rotate-z 90)
            (rotate-z 180)
            (rotate-z 270)))
    (pict3d
     (scene-union*
      (list*
       (make-triangle-outline-shape
        (vector vtx3 vtx2 vtx4)
        (vector #t #t #t)
        (vector #f #t #t)
        width
        back?)
       (make-triangle-outline-shape
        (vector vtx1 vtx2 vtx3)
        (vector #t #t #t)
        (vector #t #f #t)
        width
        back?)
       (for/list : (Listof triangle-outline-shape) ([t  (in-list ts)])
         (make-triangle-outline-shape
          (vector (vtx (flt3apply/pos t (flv3 1.0 1.0 1.0)) -y cc ce cm)
                  (vtx (flt3apply/pos t (flv3 0.0 1.25 1.0)) -y cc ce cm)
                  (vtx (flt3apply/pos t (flv3 -1.0 1.0 1.0)) -y cc ce cm))
          (vector #t #f #t)
          (vector #f #f #t)
          width
          back?))))))
  
  (define m (material #:ambient 1.0 #:diffuse 0.0 #:specular 0.0 #:roughness 1.0))
  
  (define front-face
    (combine (make-face (rgba 0 0.5) (emitted 0) m 3.5 #f)
             (make-face (rgba 1 0.5) (emitted 1 1 0.5 2) m 1.5 #f)))
  
  (define back-face
    (combine (make-face (rgba 0 0.25) (emitted 0) m 3.5 #t)
             (make-face (rgba 0 0.25) (emitted 0.5 1 1 2) m 1.5 #t)))
  
  (define ts
    (list identity-affine
          (scale-z -1)
          (rotate-x 90)
          (rotate-x -90)
          (rotate-y -90)
          (rotate-y 90)))
  
  (freeze
   (combine
    (for*/list : (Listof Pict3D) ([t  (in-list ts)])
      (combine (transform front-face t)
               (transform back-face t))))))

(define group-box (make-group-box))


(: group-boxes (-> Pict3D (Listof Tag) (Listof Pict3D)))
(define (group-boxes pict path)
  (define trs
    (map-group/transform
     pict path
     (λ ([t : Affine] [p : Pict3D])
       (define-values (v1 v2) (bounding-rectangle p))
       (list t v1 v2))))
  
  (for/fold ([picts : (Listof Pict3D)  empty]) ([tr  (in-list trs)])
    (match-define (list t v1 v2) tr)
    (define s (and v1 v2 (dir-scale (pos- v2 v1) 0.5)))
    (cond [(and v1 v2 s (> (min (abs (dir-dx s)) (abs (dir-dy s)) (abs (dir-dz s))) epsilon.0))
           (define t1 (scale s))
           (define t2 (move (pos- (pos-between v1 v2 0.5) origin)))
           (define t0 (affine-compose t2 t1))
           (cons (transform group-box (affine-compose t t0)) picts)]
          [else
           picts])))
