#lang typed/racket/base

(require racket/list
         racket/match
         math/flonum
         math/base
         "../math/flv3.rkt"
         "../math/flt3.rkt"
         "../math/flrect3.rkt"
         "../engine/types.rkt"
         "../engine/scene.rkt"
         "../engine/scene/tags.rkt")

(provide standard-over-light-scene
         standard-under-light-scene
         scene-light-indicators
         scene-origin-indicator
         scene-basis-indicators)

(define-values (standard-over-light-scene standard-under-light-scene)
  (let ([dv  (flvector -0.25 -0.5 -1.0)]
        [e1  (flvector 1.0 1.0 1.0 1.0)]
        [e2  (flvector 1.0 1.0 1.0 0.5)])
    (values
     (shape->scene (make-directional-light-shape e1 dv))
     (shape->scene (make-directional-light-shape e2 (flv3neg dv))))))

(: unit-octahedron-vertices (Listof (Vectorof FlVector)))
(define unit-octahedron-vertices
  (list (vector +x-flv3 +y-flv3 +z-flv3)
        (vector +y-flv3 -x-flv3 +z-flv3)
        (vector -x-flv3 -y-flv3 +z-flv3)
        (vector -y-flv3 +x-flv3 +z-flv3)
        (vector +y-flv3 +x-flv3 -z-flv3)
        (vector -x-flv3 +y-flv3 -z-flv3)
        (vector -y-flv3 -x-flv3 -z-flv3)
        (vector +x-flv3 -y-flv3 -z-flv3)))

(: unit-octahedron-normals (Listof FlVector))
(define unit-octahedron-normals
  (map (λ ([vs : (Vectorof FlVector)])
         (assert (flv3polygon-normal vs) values))
       unit-octahedron-vertices))

(: unit-octahedron-scene (-> FlVector FlVector material Boolean Scene))
(define (unit-octahedron-scene c e m inside?)
  (scene-union*
   (map (λ ([vs : (Vectorof FlVector)] [n : FlVector])
          (shape->scene (make-triangle-shape vs n c e m #f)))
        unit-octahedron-vertices
        unit-octahedron-normals)))

(: scene-all-point-lights (-> Scene (Listof point-light-shape)))
(define (scene-all-point-lights s)
  (let loop ([s s] [t identity-affine])
    (: lights (Listof point-light-shape))
    (define lights empty)
    (scene-for-each!
     s
     empty  ; no planes
     (λ ([a : Shape] [b : FlRect3] [t0 : Affine] [c : Nonnegative-Fixnum])
       (cond [(point-light-shape? a)
              (let ([a  (point-light-shape-easy-transform a (affine-compose2 t t0))])
                (set! lights (cons a lights)))]
             [(frozen-scene-shape? a)
              (let ([frozen-lights (loop (frozen-scene-shape-scene a)
                                         (affine-compose2 t t0))])
                (set! lights (append frozen-lights lights)))]))
     0)  ; don't care about starting index
    lights))

(define flv4-black (flvector 0.0 0.0 0.0 1.0))
(define ambient-material (material 1.0 0.0 0.0 1.0))

(: affine-position (-> Affine FlVector))
(define (affine-position t)
  (let ([t  (affine-transform t)])
    (cond [(flidentity3? t)  zero-flv3]
          [(fllinear3? t)    zero-flv3]
          [else  (define ms (fltransform3-forward t))
                 (flvector (flvector-ref ms 3)
                           (flvector-ref ms 7)
                           (flvector-ref ms 11))])))

(: point-light-indicator (-> point-light-shape Scene))
(define (point-light-indicator a)
  (match-define (point-light-shape _ fs e t r0 r1) a)
  ;; i = intensity
  (define i (flvector-ref e 3))
  ;; e1 = emitted color of octahedron
  (define e1 (flvector-copy e))
  (flvector-set! e1 3 2.0)  ; 2.0 looks nice and glowy
  ;; s = radius of a ball that would give off that much light...
  (define s (* (flsqrt (/ i 2.0)) #i1/8))  ; but reduced in size - it would look too big
  ;; ts = transform to put octahedron in place
  (define ts (affine (flt3compose (affine-transform t) (scale-flt3 (flvector s s s)))))
  (shape->scene
   (make-frozen-scene-shape
    (assert
     (scene-union
       (shape->scene (make-point-light-shell-shape e t (* 0.99 r0) (* 1.01 r1)))
       (make-trans-scene ts (unit-octahedron-scene flv4-black e1 ambient-material #f)))
     nonempty-scene?))))

(: scene-light-indicators (-> Scene (Listof Scene)))
(define (scene-light-indicators s)
  (map point-light-indicator (scene-all-point-lights s)))

(: make-unit-pyramid-scene (-> FlVector FlVector material Scene))
(define (make-unit-pyramid-scene c e m)
  (scene-union
   (scene-union*
    (for/list : (Listof Scene) ([i  (in-range 4)])
      (define vs (vector +x-y-flv3 +x+y-flv3 +z-flv3))
      (define norm (assert (flv3polygon-normal vs) values))
      (scene-transform-shapes
       (shape->scene (make-triangle-shape vs norm c e m #f))
       (affine (rotate-z-flt3 (degrees->radians (* (fl i) +90.0)))))))
   empty-scene
   #;
   (scene-union*
    (map
     shape->scene
     (make-quad-shapes
      (vector +x+y-flv3 +x-y-flv3 -x-y-flv3 -x+y-flv3)
      +z-flv3
      c e m #f)))))

(: make-unit-arrow-scene (-> FlVector FlVector material Scene))
(define (make-unit-arrow-scene c e m)
  (scene-union
   (shape->scene
    (make-rectangle-shape
     (assert (flrect3 (flvector #i-1/64 #i-1/64 0.0)
                      (flvector #i1/64 #i1/64 #i60/64))
             nonempty-flrect3?)
     c e m #f))
   (let ([t  (flt3compose
              (translate-flt3 (flvector 0.0 0.0 #i56/64))
              (scale-flt3 (flvector #i2/64 #i2/64 #i8/64)))])
     (scene-transform-shapes
      (make-unit-pyramid-scene c e m)
      (affine t)))))

(define axis-material
  (material 0.1 0.2 0.7 0.3))

(define x-axis-scene
  (scene-transform-shapes
   (make-unit-arrow-scene
    (flvector 0.0 0.0 0.0 1.0)
    (flvector 1.0 0.05 0.05 2.0)
    axis-material)
   (affine (rotate-y-flt3 (degrees->radians +90.0)))))

(define y-axis-scene
  (scene-transform-shapes
   (make-unit-arrow-scene
    (flvector 0.0 0.0 0.0 1.0)
    (flvector 0.0 1.0 0.0 1.75)
    axis-material)
   (affine (rotate-x-flt3 (degrees->radians -90.0)))))

(define z-axis-scene
  (make-unit-arrow-scene
   (flvector 0.0 0.0 0.0 1.0)
   (flvector 0.1 0.1 1.0 2.5)
   axis-material))

(define axes-scene
  (shape->scene
   (make-frozen-scene-shape
    (assert
     (scene-union*
      (list (shape->scene
             (make-sphere-shape
              (affine (scale-flt3 (flvector 0.03 0.03 0.03)))
              (flvector 0.0 0.0 0.0 1.0)
              (flvector 1.0 1.0 1.0 2.0)
              axis-material
              #f))
            x-axis-scene
            y-axis-scene
            z-axis-scene))
     nonempty-scene?))))

(define basis-dim 0.5)

(define x-basis-scene
  (scene-transform-shapes
   (make-unit-arrow-scene
    (flvector 0.0 0.0 0.0 1.0)
    (flvector basis-dim (* basis-dim 0.05) (* basis-dim 0.05) 1.0)
    axis-material)
   (affine (flt3compose (rotate-y-flt3 (degrees->radians +90.0))
                        (scale-flt3 (flvector 0.5 0.5 1.0))))))

(define y-basis-scene
  (scene-transform-shapes
   (make-unit-arrow-scene
    (flvector 0.0 0.0 0.0 1.0)
    (flvector 0.0 basis-dim 0.0 1.0)
    axis-material)
   (affine (flt3compose (rotate-x-flt3 (degrees->radians -90.0))
                        (scale-flt3 (flvector 0.5 0.5 1.0))))))

(define z-basis-scene
  (scene-transform-shapes
   (make-unit-arrow-scene
    (flvector 0.0 0.0 0.0 1.0)
    (flvector (* basis-dim 0.1) (* basis-dim 0.1) basis-dim 1.0)
    axis-material)
   (affine (scale-flt3 (flvector 0.5 0.5 1.0)))))

(define basis-scene
  (shape->scene
   (make-frozen-scene-shape
    (assert
     (scene-union*
      (list (shape->scene
             (make-sphere-shape
              (affine (scale-flt3 (flvector 0.015 0.015 0.015)))
              (flvector 0.0 0.0 0.0 1.0)
              (flvector 1.0 1.0 1.0 1.0)
              axis-material
              #f))
            x-basis-scene
            y-basis-scene
            z-basis-scene))
     nonempty-scene?))))

(: scene-origin-indicator (-> Flonum Scene))
(define (scene-origin-indicator scale)
  (make-trans-scene (affine (scale-flt3 (flvector scale scale scale)))
                    axes-scene))

(: scene-basis-indicators (-> Scene Flonum (Listof Scene)))
(define (scene-basis-indicators s scale)
  (define scale-t (affine (scale-flt3 (flvector scale scale scale))))
  (map (λ ([nt : (Pair Tag Affine)])
         (make-trans-scene (affine-compose2 (cdr nt) scale-t) basis-scene))
       (scene-group-transforms s 'all)))
