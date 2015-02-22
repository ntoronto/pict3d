#lang typed/racket/base

(require racket/match
         racket/sequence
         typed/racket/class
         typed/racket/gui
         math/flonum
         math/base
         "../engine/scene/tags.rkt"
         (except-in "../engine/types.rkt"
                    material)
         (prefix-in
          engine:
          (only-in "../engine/types.rkt"
                   material))
         "../math/flt3.rkt"
         "../math/flv3.rkt"
         "../utils.rkt")

(provide
 Tag
 ;; Materials
 Material
 material?
 material
 make-material
 ;; Vectors
 Vec
 ->flv3
 ->flv4
 ;; Colors
 Pict3D-Color
 origin x+ x- y+ y- z+ z-
 ->flcolor4
 ->flcolor3
 transparent
 ;; Affine transforms
 Affine
 affine?
 identity-affine
 values->affine
 rows->affine
 cols->affine
 affine-values
 affine-rows
 affine-cols
 affine-compose
 affine-inverse
 affine-consistent?
 point-at
 )

;; ===================================================================================================
;; Materials

(define-type Material engine:material)

(: make-material (-> Real Real Real Real Material))
(define (make-material a d s r)
  (engine:material (fl a) (fl d) (fl s) (fl r)))

(: material (->* [] [#:ambient Real #:diffuse Real #:specular Real #:roughness Real] Material))
(define (material #:ambient [a 0.05] #:diffuse [d 0.6] #:specular [s 0.35] #:roughness [r 0.3])
  (make-material a d s r))

;; ===================================================================================================
;; Vectors

(define-type Vec (U (Listof Real) (Vectorof Real) FlVector))

(define origin (flvector 0.0 0.0 0.0))
(define x+ (flvector +1.0 0.0 0.0))
(define x- (flvector -1.0 0.0 0.0))
(define y+ (flvector 0.0 +1.0 0.0))
(define y- (flvector 0.0 -1.0 0.0))
(define z+ (flvector 0.0 0.0 +1.0))
(define z- (flvector 0.0 0.0 -1.0))

(: vec-length (-> Vec Index))
(define (vec-length xs)
  (cond [(flvector? xs)  (flvector-length xs)]
        [(list? xs)  (length xs)]
        [else  (vector-length xs)]))

(: ->flv (-> Vec Index (U #f FlVector)))
(define (->flv xs n)
  (cond [(flvector? xs)  (if (= n (flvector-length xs)) xs #f)]
        [(list? xs)  (if (= n (length xs)) (list->flvector xs) #f)]
        [else  (if (= n (vector-length xs)) (vector->flvector xs) #f)]))

(: ->flv3 (-> Symbol Vec FlVector))
(define (->flv3 name xs)
  (define v (->flv xs 3))
  (if v v (raise-argument-error name "length-3 vector" xs)))

(: ->flv4 (-> Symbol Vec FlVector))
(define (->flv4 name xs)
  (define v (->flv xs 4))
  (if v v (raise-argument-error name "length-4 vector" xs)))

;; ===================================================================================================
;; Colors

(define-type Pict3D-Color (U String Symbol Real Vec (Instance Color%)))

(: color->rgb (-> Symbol (U String Symbol (Instance Color%)) (Vector Byte Byte Byte)))
(define (color->rgb name orig-col)
  (let* ([col  orig-col]
         [col  (if (symbol? col) (symbol->string col) col)]
         [col  (if (string? col) (send the-color-database find-color col) col)])
    (if col
        (vector (send col red) (send col green) (send col blue))
        (raise-argument-error name "known color" orig-col))))

(: ->flcolor4 (-> Symbol Pict3D-Color FlVector))
(define (->flcolor4 name col)
  (cond
    [(real? col)
     (define r (fl col))
     (flvector r r r 1.0)]
    [(or (list? col) (vector? col) (flvector? col))
     (define n (vec-length col))
     (cond
       [(= n 4)  (->flv4 name col)]
       [(= n 3)
        (define c (make-flvector 4 1.0))
        (for ([r  (ann col (Sequenceof Real))]
              [i  (in-range 4)])
          (flvector-set! c i (fl r)))
        c]
       [else
        (raise-argument-error name "string, symbol or length-3 or length-4 vector" col)])]
    [else
     (match-define (vector r g b) (color->rgb name col))
     (flvector (/ (fl r) 255.0)
               (/ (fl g) 255.0)
               (/ (fl b) 255.0)
               1.0)]))

(: ->flcolor3 (-> Symbol Pict3D-Color FlVector))
(define (->flcolor3 name col)
  (cond [(real? col)
         (define r (fl col))
         (flvector r r r)]
        [(or (list? col) (vector? col) (flvector? col))
         (->flv3 name col)]
        [else
         (match-define (vector r g b) (color->rgb name col))
         (flvector (/ (fl r) 255.0)
                   (/ (fl g) 255.0)
                   (/ (fl b) 255.0))]))

(: transparent (-> Pict3D-Color Real FlVector))
(define (transparent col a)
  (define v (flvector-copy (->flcolor4 'transparent col)))
  (flvector-set! v 3 (* (fl a) (flvector-ref v 3)))
  v)

;; ===================================================================================================
;; Affine transforms

(: values->affine (-> FlVector Affine))
(define (values->affine m)
  (affine (flvector->flaffine3 m)))

(: rows->affine (-> Vec Vec Vec Affine))
(define (rows->affine r0 r1 r2)
  (affine (rows->flaffine3 (->flv4 'rows->affine r0)
                           (->flv4 'rows->affine r1)
                           (->flv4 'rows->affine r2))))

(: cols->affine (-> Vec Vec Vec Vec Affine))
(define (cols->affine x y z p)
  (affine (cols->flaffine3 (->flv3 'cols->affine x)
                           (->flv3 'cols->affine y)
                           (->flv3 'cols->affine z)
                           (->flv3 'cols->affine p))))

(: affine-values (-> Affine FlVector))
(define (affine-values t)
  (fltransform3-forward (->flaffine3 (affine-transform t))))

(: affine-rows (-> Affine (Values FlVector FlVector FlVector)))
(define (affine-rows t)
  (define-values (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
    (flvector-values (affine-values t) 12))
  (values (flvector m00 m01 m02 m03)
          (flvector m10 m11 m12 m13)
          (flvector m20 m21 m22 m23)))

(: affine-cols (-> Affine (Values FlVector FlVector FlVector FlVector)))
(define (affine-cols t)
  (define-values (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
    (flvector-values (affine-values t) 12))
  (values (flvector m00 m10 m20)
          (flvector m01 m11 m21)
          (flvector m02 m12 m22)
          (flvector m03 m13 m23)))

(: point-at
   (->* [] [#:from Vec #:to (U Vec #f) #:dir (U Vec #f) #:angle Real #:up Vec #:normalize? Any]
        Affine))
(define (point-at #:from [origin origin]
                  #:to [dest #f]
                  #:dir [z-axis #f]
                  #:angle [angle 0.0]
                  #:up [up z+]
                  #:normalize? [normalize? #t])
  (cond
    [(and dest z-axis)
     (error 'point-at "expected exactly one of #:to or #:dir; got #:to ~e and #:dir ~e" dest z-axis)]
    [(not (or dest z-axis))
     (error 'point-at "expected exactly one of #:to or #:dir; got neither")]
    [else
     (let* ([origin  (->flv3 'point-at origin)]
            [dest  (if dest (->flv3 'point-at dest) #f)]
            [z-axis  (cond [dest  (flv3- dest origin)]
                           [else  (->flv3 'point-at (assert z-axis values))])]
            [z-axis  (if normalize? (flv3normalize z-axis) z-axis)]
            [z-axis : FlVector  (if z-axis z-axis z+)]
            [angle  (degrees->radians (fl angle))]
            [up  (flv3normalize (->flv3 'point-at up))]
            [up  (if up up z+)])
       (define x-axis (flv3normalize (flv3cross z-axis up)))
       (define t
         (cond
           [x-axis
            (define y-axis (assert (flv3normalize (flv3cross z-axis x-axis)) values))
            (cols->flaffine3 x-axis y-axis z-axis origin)]
           [(>= (flvector-ref z-axis 2) 0.0)
            (translate-flt3 origin)]
           [else
            (flt3compose (translate-flt3 origin)
                         (scale-flt3 (flvector -1.0 1.0 -1.0)))]))
       (affine (flt3compose t (rotate-z-flt3 angle))))]))
