#lang typed/racket/base

(require racket/list
         racket/match
         racket/promise
         math/base
         "../math.rkt"
         "../engine.rkt"
         "../soup.rkt"
         "../utils.rkt")

(provide (all-defined-out))

(define min-angle (degrees->radians 1/60))

(struct attrib-shape-functions shape-functions
  ([set-color : (-> shape FlV4 shape)]
   [set-emitted : (-> shape FlV4 shape)]
   [set-material : (-> shape FlV4 shape)])
  #:transparent)

(: default-set-color (-> shape FlV4 shape))
(define (default-set-color s c) s)

(: default-set-emitted (-> shape FlV4 shape))
(define (default-set-emitted s e) s)

(: default-set-material (-> shape FlV4 shape))
(define (default-set-material s m) s)

(struct deform-shape-functions attrib-shape-functions
  ([extract-faces : (-> shape (Values (Listof shape) (Listof (face deform-data #f))))]
   [tessellate : (-> shape FlAffine3 Positive-Flonum Nonnegative-Flonum
                     (Values (Listof shape) (Listof (face deform-data #f))))]
   [deform : (-> shape FlSmooth3 (Listof shape))])
  #:transparent)

(: default-extract-faces (-> shape (Values (Listof shape) (Listof (face deform-data #f)))))
(define (default-extract-faces s) (values (list s) empty))

(: default-tessellate (-> shape FlAffine3 Positive-Flonum Nonnegative-Flonum
                          (Values (List shape) Null)))
(define (default-tessellate s t max-edge max-angle) (values (list s) empty))

(: default-deform (-> shape FlSmooth3 (List shape)))
(define (default-deform s t) (list s))

(: set-shape-color (-> shape FlV4 shape))
(define (set-shape-color s c)
  (define vt (shape-vtable s))
  (if (attrib-shape-functions? vt)
      ((attrib-shape-functions-set-color vt) s c)
      (default-set-color s c)))

(: set-shape-emitted (-> shape FlV4 shape))
(define (set-shape-emitted s e)
  (define vt (shape-vtable s))
  (if (attrib-shape-functions? vt)
      ((attrib-shape-functions-set-emitted vt) s e)
      (default-set-emitted s e)))

(: set-shape-material (-> shape FlV4 shape))
(define (set-shape-material s m)
  (define vt (shape-vtable s))
  (if (attrib-shape-functions? vt)
      ((attrib-shape-functions-set-material vt) s m)
      (default-set-material s m)))

(: shape-extract-faces (-> shape (Values (Listof shape) (Listof (face deform-data #f)))))
(define (shape-extract-faces s)
  (define vt (shape-vtable s))
  (if (deform-shape-functions? vt)
      ((deform-shape-functions-extract-faces vt) s)
      (default-extract-faces s)))

(: shape-tessellate (-> shape FlAffine3 Positive-Flonum Nonnegative-Flonum
                        (Values (Listof shape) (Listof (face deform-data #f)))))
(define (shape-tessellate s t max-edge max-angle)
  (define vt (shape-vtable s))
  (if (deform-shape-functions? vt)
      ((deform-shape-functions-tessellate vt) s t max-edge max-angle)
      (default-tessellate s t max-edge max-angle)))

(: shape-deform (-> shape FlSmooth3 (Listof shape)))
(define (shape-deform s t)
  (define vt (shape-vtable s))
  (if (deform-shape-functions? vt)
      ((deform-shape-functions-deform vt) s t)
      (default-deform s t)))

(: scene-extract-faces* (All (A B) (-> Scene
                                       (-> shape (Values (Listof shape) (Listof (face A B))))
                                       (Values (Listof shape) (Listof (face A B))))))
(define (scene-extract-faces* s extract)
  (: rs (Listof (Pairof (Listof shape) (Listof (face A B)))))
  (define rs
    (scene-extract
     s
     empty
     (λ ([s : shape] [t : FlAffine3])
       (define-values (ss fs)
         (for/fold ([ss : (Listof shape)  empty]
                    [fs : (Listof (face A B))  empty])
                   ([s  (in-list (shape-deep-transform s t))])
           (define-values (shape-ss shape-fs) (extract s))
           (values (append shape-ss ss)
                   (append shape-fs fs))))
       ((ann cons (All (A B) (-> A B (Pair A B)))) ss fs))))
  (values (append* (map (inst car (Listof shape) (Listof (face A B))) rs))
          (append* (map (inst cdr (Listof shape) (Listof (face A B))) rs))))

(: scene-extract-faces (-> Scene (Values (Listof shape) (Listof (face deform-data #f)))))
(define (scene-extract-faces s)
  (scene-extract-faces* s shape-extract-faces))

(: scene-tessellate (-> Scene FlAffine3 Positive-Flonum Nonnegative-Flonum
                        (Values (Listof shape) (Listof (face deform-data #f)))))
(define (scene-tessellate s t0 max-edge max-angle)
  (scene-extract-faces* s (λ ([s : shape]) (shape-tessellate s t0 max-edge max-angle))))

(: scene-deform (-> Scene FlSmooth3 Scene))
(define (scene-deform s t)
  (cond
    [(empty-scene? s)  s]
    [(shape? s)
     (scene-union* (shape-deform s t))]
    [(node-scene? s)
     (make-node-scene (scene-deform (node-scene-neg s) t)
                      (scene-deform (node-scene-pos s) t))]
    [(trans-scene? s)
     (scene-deform (trans-scene-scene s)
                   (fls3compose t (trans-scene-affine s)))]
    [(group-scene? s)
     (scene-deform (group-scene-scene s) t)]))

(: transformed-shape-intersect
   (-> FlAffine3 Boolean FlV3 FlV3 Nonnegative-Flonum
       (-> FlV3 FlV3 (Values (U #f Flonum) (U #f Flonum)))
       (-> FlV3 FlV3 Nonnegative-Flonum (U #f FlV3))
       (Values (U #f Nonnegative-Flonum) (U #f (Promise trace-data)))))
(define (transformed-shape-intersect t inside? v dv max-time shape-intersects intersect-normal)
  ;; Convert ray to local coordinates
  (define tinv (flt3inverse t))
  (cond [tinv
         (define sv (flt3apply/pos tinv v))
         (define sdv (flt3apply/dir tinv dv))
         ;; Compute intersection
         (define-values (tmin tmax) (shape-intersects sv sdv))
         (define time (if inside? tmax tmin))
         (cond [(and time (>= time 0.0) (<= time max-time))
                (define data
                  (delay (define p (flv3fma dv time v))
                         (define n (let ([n  (intersect-normal sv sdv time)])
                                     (and n (flt3apply/norm t (if inside? (flv3neg n) n)))))
                         (trace-data p n empty)))
                (values time data)]
               [else
                (values #f #f)])]
        [else
         (values #f #f)]))
