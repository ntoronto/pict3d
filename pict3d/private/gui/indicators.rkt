#lang typed/racket/base

(require racket/list
         racket/match
         math/flonum
         math/base
         "../math.rkt"
         "../engine.rkt"
         "typed-user-types.rkt")

(provide standard-over-light-scene
         standard-under-light-scene
         scene-light-indicators
         scene-origin-indicator
         scene-basis-indicators)

(define-values (standard-over-light-scene standard-under-light-scene)
  (let* ([dv : FlV3  (flv3 -0.25 -0.5 -1.0)]
         [-dv : FlV3  (flv3neg dv)]
         [e1 : FlV4  (flv4 1.0 1.0 1.0 1.0)]
         [e2 : FlV4  (flv4 1.0 1.0 1.0 0.5)])
    (values
     (make-leaf-scene (make-directional-light-shape e1 dv))
     (make-leaf-scene (make-directional-light-shape e2 -dv)))))

(: unit-octahedron-vertices (Listof (Vectorof FlV3)))
(define unit-octahedron-vertices
  (list (vector +x-flv3 +y-flv3 +z-flv3)
        (vector +y-flv3 -x-flv3 +z-flv3)
        (vector -x-flv3 -y-flv3 +z-flv3)
        (vector -y-flv3 +x-flv3 +z-flv3)
        (vector +y-flv3 +x-flv3 -z-flv3)
        (vector -x-flv3 +y-flv3 -z-flv3)
        (vector -y-flv3 -x-flv3 -z-flv3)
        (vector +x-flv3 -y-flv3 -z-flv3)))

(: unit-octahedron-normals (Listof FlV3))
(define unit-octahedron-normals
  (map (λ ([vs : (Vectorof FlV3)])
         (match-define (vector v1 v2 v3) vs)
         (assert (flv3triangle-normal v1 v2 v3) values))
       unit-octahedron-vertices))

(: unit-octahedron-scene (-> FlV4 FlV4 FlV4 Boolean Scene))
(define (unit-octahedron-scene c e m inside?)
  (scene-union*
   (map (λ ([vs : (Vectorof FlV3)]
            [n : FlV3])
          (make-leaf-scene
           (make-triangle-shape
            (vtx (vector-ref vs 0) n c e m)
            (vtx (vector-ref vs 1) n c e m)
            (vtx (vector-ref vs 2) n c e m)
            #f)))
        unit-octahedron-vertices
        unit-octahedron-normals)))

(: scene-all-point-lights (-> Scene (Listof point-light-shape)))
(define (scene-all-point-lights s)
  (let loop ([s s] [t  identity-flaffine3])
    (: lights (Listof point-light-shape))
    (define lights empty)
    
    (scene-for-each!
     s
     empty  ; no planes
     (λ ([a : Shape] [b : (U #f FlRect3)] [t0 : FlAffine3] [c : Nonnegative-Fixnum])
       (cond [(point-light-shape? a)
              (let ([t : FlAffine3  (flt3compose t t0)])
                (let ([a  (point-light-shape-easy-transform a t)])
                  (set! lights (cons a lights))))]
             [(frozen-scene-shape? a)
              (let ([t : FlAffine3  (flt3compose t t0)])
                (let ([frozen-lights (loop (frozen-scene-shape-scene a) t)])
                  (set! lights (append frozen-lights lights))))]))
     0)  ; don't care about starting index
    lights))

(define black-color (flv4 0.0 0.0 0.0 1.0))
(define ambient-material (flv4 1.0 0.0 0.0 1.0))

(: flaffine3-position (-> FlAffine3 FlV3))
(define (flaffine3-position t)
  (cond [(identity-flaffine3? t)  zero-flv3]
        [else  (call/flaffine3-forward t
                 (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
                   (flv3 m03 m13 m23)))]))

(: point-light-indicator (-> point-light-shape Scene))
(define (point-light-indicator a)
  (match-define (point-light-shape _ fs e t r0 r1) a)
  ;; i = intensity
  (define i (unsafe-flv4-ref e 3))
  ;; s = radius of a ball that would give off that much light...
  (define s (* (flsqrt (/ i 2.0)) #i1/8))  ; but reduced in size - it would look too big
  ;; e1 = emitted color of octahedron
  ;; ts = transform to put octahedron in place
  (let* ([e1 : FlV4  (call/flv4-values e
                       (λ (r g b i) (flv4 r g b 2.0)))]  ; 2.0 looks nice and glowy
         [dv : FlV3  (flv3 s s s)]
         [t0 : FlAffine3  (scale-flt3 dv)]
         [ts : FlAffine3  (flt3compose t t0)])
    (make-leaf-scene
     (make-frozen-scene-shape
      (assert
       (scene-union
        (make-leaf-scene (make-point-light-shell-shape e t (* 0.99 r0) (* 1.01 r1)))
        (make-trans-scene ts (unit-octahedron-scene black-color e1 ambient-material #f)))
       nonempty-scene?)))))

(: scene-light-indicators (-> Scene (Listof Scene)))
(define (scene-light-indicators s)
  (map point-light-indicator (scene-all-point-lights s)))

(: make-unit-pyramid-scene (-> FlV4 FlV4 FlV4 Scene))
(define (make-unit-pyramid-scene c e m)
  (scene-union*
   (for/list : (Listof Scene) ([i  (in-range 4)])
     (define n (assert (flv3triangle-normal +x-y-flv3 +x+y-flv3 +z-flv3) values))
     (define s
       (make-leaf-scene
        (make-triangle-shape
         (vtx +x-y-flv3 n c e m)
         (vtx +x+y-flv3 n c e m)
         (vtx +z-flv3 n c e m)
         #f)))
     (scene-transform-shapes s (rotate-z-flt3 (degrees->radians (* (fl i) +90.0)))))))

(: make-unit-arrow-scene (-> FlV4 FlV4 FlV4 Scene))
(define (make-unit-arrow-scene c e m)
  (scene-union
   (make-leaf-scene
    (let* ([mn : FlV3  (flv3 #i-1/64 #i-1/64 0.0)]
           [mx : FlV3  (flv3 #i1/64 #i1/64 #i60/64)]
           [b : FlRect3  (flrect3 mn mx)])
      (make-rectangle-shape b c e m #f)))
   (let* ([dv : FlV3  (flv3 #i2/64 #i2/64 #i8/64)]
          [v  : FlV3  (flv3 0.0 0.0 #i56/64)]
          [t1 : FlAffine3  (scale-flt3 dv)]
          [t2 : FlAffine3  (translate-flt3 v)]
          [t  : FlAffine3  (flt3compose t2 t1)])
     (scene-transform-shapes (make-unit-pyramid-scene c e m) t))))

(define axis-material (flv4 0.1 0.2 0.7 0.3))

(define x-axis-scene
  (let ([e : FlV4  (flv4 1.0 0.05 0.05 2.0)])
    (scene-transform-shapes (make-unit-arrow-scene black-color e axis-material)
                            (rotate-y-flt3 (degrees->radians +90.0)))))

(define y-axis-scene
  (let ([e : FlV4  (flv4 0.0 1.0 0.0 1.75)])
    (scene-transform-shapes (make-unit-arrow-scene black-color e axis-material)
                            (rotate-x-flt3 (degrees->radians -90.0)))))

(define z-axis-scene
  (let ([e : FlV4  (flv4 0.1 0.1 1.0 2.5)])
    (make-unit-arrow-scene black-color e axis-material)))

(define axes-scene
  (let* ([s : FlV3  (flv3 0.03 0.03 0.03)]
         [t : FlAffine3  (scale-flt3 s)]
         [e : FlV4  (flv4 1.0 1.0 1.0 2.0)])
    (make-leaf-scene
     (make-frozen-scene-shape
      (assert
       (scene-union*
        (list (make-leaf-scene (make-sphere-shape t black-color e axis-material #f))
              x-axis-scene
              y-axis-scene
              z-axis-scene))
       nonempty-scene?)))))

(define basis-dim 0.5)
(define basis-scale-t (scale-flt3 (flv3 0.5 0.5 1.0)))

(define x-basis-scene
  (let ([e : FlV4  (flv4 basis-dim (* basis-dim 0.05) (* basis-dim 0.05) 1.0)])
    (scene-transform-shapes (make-unit-arrow-scene black-color e axis-material)
                            (flt3compose (rotate-y-flt3 (degrees->radians +90.0))
                                         basis-scale-t))))

(define y-basis-scene
  (let ([e : FlV4  (flv4 0.0 basis-dim 0.0 1.0)])
    (scene-transform-shapes (make-unit-arrow-scene black-color e axis-material)
                            (flt3compose (rotate-x-flt3 (degrees->radians -90.0))
                                         basis-scale-t))))

(define z-basis-scene
  (let ([e : FlV4  (flv4 (* basis-dim 0.1) (* basis-dim 0.1) basis-dim 1.0)])
    (scene-transform-shapes (make-unit-arrow-scene black-color e axis-material)
                            basis-scale-t)))

(define basis-scene
  (let ([t : FlAffine3  (scale-flt3 (flv3 0.015 0.015 0.015))]
        [e : FlV4  (flv4 1.0 1.0 1.0 1.0)])
    (make-leaf-scene
     (make-frozen-scene-shape
      (assert
       (scene-union*
        (list (make-leaf-scene (make-sphere-shape t black-color e axis-material #f))
              x-basis-scene
              y-basis-scene
              z-basis-scene))
       nonempty-scene?)))))

(: scene-origin-indicator (-> Flonum Scene))
(define (scene-origin-indicator scale)
  (make-trans-scene (scale-flt3 (flv3 scale scale scale))
                    axes-scene))

(: scene-basis-indicators (-> Scene Flonum (Listof (Pair Pos Scene))))
(define (scene-basis-indicators s scale)
  (define t (scale-flt3 (flv3 scale scale scale)))
  (map (λ ([nt : (Pair Tag FlAffine3)])
         (define t0 (cdr nt))
         (cons (flv3->pos (flaffine3-position t0))
               (make-trans-scene (flt3compose t0 t) basis-scene)))
       (scene-group-transforms s 'all)))
