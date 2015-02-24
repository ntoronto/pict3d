#lang typed/racket/base

(require (only-in racket/unsafe/ops unsafe-flvector-ref)
         racket/match
         typed/racket/class
         typed/racket/gui
         math/flonum
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
 tag?
 ;; Materials
 Material
 material?
 material
 make-material
 ;; Colors
 Pict3D-Color
 ->flcolor4
 ->flcolor3
 set-opacity
 set-intensity
 ;; Vectors
 (rename-out [-Pos Pos]
             [-Dir Dir])
 pos?
 dir?
 origin x+ y+ z+ x- y- z-
 pos pos->flvector flvector->pos pos-coordinates
 dir dir->flvector flvector->dir dir-components
 dir+
 dir-
 dir-negate
 dir-scale
 dir-dist^2
 dir-dist
 dir-normalize
 dir-dot
 dir-cross
 pos+
 pos-
 pos-between
 pos-dist
 pos-dist^2
 ;; Affine transforms
 Affine
 affine?
 identity-affine
 cols->affine
 affine->cols
 affine-compose
 affine-inverse
 affine-consistent?
 )

;; ===================================================================================================
;; Materials

(define-type Material engine:material)

(: make-material (-> Real Real Real Real Material))
(define (make-material a d s r)
  (engine:material (fl a) (fl d) (fl s) (fl r)))

(: material (->* [] [#:ambient Real #:diffuse Real #:specular Real #:roughness Real] Material))
(define (material #:ambient [a 0.0] #:diffuse [d 0.0] #:specular [s 0.0] #:roughness [r 0.1])
  (make-material a d s r))

;; ===================================================================================================
;; Colors

(define-type Color-Vector (U (Listof Real) (Vectorof Real) FlVector))
(define-type Pict3D-Color (U String Symbol Real Color-Vector (Instance Color%)))

(: color-vec-length (-> Color-Vector Index))
(define (color-vec-length xs)
  (cond [(flvector? xs)  (flvector-length xs)]
        [(list? xs)  (length xs)]
        [else  (vector-length xs)]))

(: color-vec->flv (-> Color-Vector Index (U #f FlVector)))
(define (color-vec->flv xs n)
  (cond [(flvector? xs)  (if (= n (flvector-length xs)) xs #f)]
        [(list? xs)  (if (= n (length xs)) (list->flvector xs) #f)]
        [else  (if (= n (vector-length xs)) (vector->flvector xs) #f)]))

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
     (define n (color-vec-length col))
     (cond
       [(= n 4)
        (define v (color-vec->flv col 4))
        (if v v (raise-argument-error name "length-4 vector" col))]
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
         (define v (color-vec->flv col 3))
         (if v v (raise-argument-error name "length-3 vector" col))]
        [else
         (match-define (vector r g b) (color->rgb name col))
         (flvector (/ (fl r) 255.0)
                   (/ (fl g) 255.0)
                   (/ (fl b) 255.0))]))

(: set-opacity (-> Pict3D-Color Real FlVector))
(define (set-opacity col a)
  (define v (flvector-copy (->flcolor4 'set-opacity col)))
  (flvector-set! v 3 (* (fl a) (flvector-ref v 3)))
  v)

(: set-intensity (-> Pict3D-Color Real FlVector))
(define (set-intensity col a)
  (define v (flvector-copy (->flcolor4 'set-intensity col)))
  (flvector-set! v 3 (* (fl a) (flvector-ref v 3)))
  v)

;; ===================================================================================================
;; Vectors

(define print-vec
  (λ ([name : Symbol])
    (make-constructor-style-printer
     (λ ([v : Vec]) name)
     (λ ([v : Vec])
       (define-values (x y z) (vec-values v))
       (list x y z)))))

(struct Vec ([vector : FlVector]) #:transparent
  #:property prop:custom-print-quotable 'never)
  
(struct Pos Vec () #:transparent
  #:property prop:custom-write (print-vec 'pos))
  
(struct Dir Vec () #:transparent
  #:property prop:custom-write (print-vec 'dir))

(define-type -Pos Pos)
(define-type -Dir Dir)
(define pos? Pos?)
(define dir? Dir?)

(define origin (Pos (flvector 0.0 0.0 0.0)))
(define x+ (Dir (flvector +1.0 0.0 0.0)))
(define y+ (Dir (flvector 0.0 +1.0 0.0)))
(define z+ (Dir (flvector 0.0 0.0 +1.0)))
(define x- (Dir (flvector -1.0 0.0 0.0)))
(define y- (Dir (flvector 0.0 -1.0 0.0)))
(define z- (Dir (flvector 0.0 0.0 -1.0)))

(: pos (-> Real Real Real Pos))
(define (pos x y z)
  (Pos (flvector (fl x) (fl y) (fl z))))

(: pos->flvector (-> Pos FlVector))
(define pos->flvector Vec-vector)

(: flvector->pos (-> FlVector Pos))
(define (flvector->pos v)
  (unless (= 3 (flvector-length v))
    (raise-argument-error 'flvector->pos "length-3 flvector" v))
  (Pos v))

(: dir (-> Real Real Real Dir))
(define (dir dx dy dz)
  (Dir (flvector (fl dx) (fl dy) (fl dz))))

(: dir->flvector (-> Dir FlVector))
(define dir->flvector Vec-vector)

(: flvector->dir (-> FlVector Dir))
(define (flvector->dir v)
  (unless (= 3 (flvector-length v))
    (raise-argument-error 'flvector->dir "length-3 flvector" v))
  (Dir v))

(define-syntax-rule (vec-values stx)
  (let ([v  (Vec-vector stx)])
    (values (unsafe-flvector-ref v 0)
            (unsafe-flvector-ref v 1)
            (unsafe-flvector-ref v 2))))

(: pos-coordinates (-> Pos (Values Flonum Flonum Flonum)))
(define (pos-coordinates v) (vec-values v))

(: dir-components (-> Dir (Values Flonum Flonum Flonum)))
(define (dir-components v) (vec-values v))

(: dir+ (-> Dir Dir Dir))
(define (dir+ dv1 dv2)
  (define-values (dx1 dy1 dz1) (vec-values dv1))
  (define-values (dx2 dy2 dz2) (vec-values dv2))
  (Dir (flvector (+ dx1 dx2) (+ dy1 dy2) (+ dz1 dz2))))

(: dir- (-> Dir Dir Dir))
(define (dir- dv1 dv2)
  (define-values (dx1 dy1 dz1) (vec-values dv1))
  (define-values (dx2 dy2 dz2) (vec-values dv2))
  (Dir (flvector (- dx1 dx2) (- dy1 dy2) (- dz1 dz2))))

(: dir-negate (-> Dir Dir))
(define (dir-negate dv)
  (define-values (dx dy dz) (vec-values dv))
  (Dir (flvector (- dx) (- dy) (- dz))))

(: dir-scale (-> Dir Real Dir))
(define (dir-scale dv s)
  (define-values (dx dy dz) (vec-values dv))
  (let ([s  (fl s)])
    (Dir (flvector (* dx s) (* dy s) (* dz s)))))

(: dir-dist^2 (-> Dir Flonum))
(define (dir-dist^2 dv)
  (define-values (dx dy dz) (vec-values dv))
  (+ (* dx dx) (* dy dy) (* dz dz)))

(: dir-dist (-> Dir Flonum))
(define (dir-dist dv)
  (flsqrt (dir-dist^2 dv)))

(: dir-normalize (-> Dir (U #f Dir)))
(define (dir-normalize dv)
  (define-values (dx dy dz) (vec-values dv))
  (define m (flsqrt (+ (* dx dx) (* dy dy) (* dz dz))))
  (let ([dx  (/ dx m)]
        [dy  (/ dy m)]
        [dz  (/ dz m)])
    (if (and (< -inf.0 dx +inf.0)
             (< -inf.0 dy +inf.0)
             (< -inf.0 dz +inf.0))
        (Dir (flvector dx dy dz))
        #f)))

(: dir-dot (-> Dir Dir Flonum))
(define (dir-dot dv1 dv2)
  (flv3dot (dir->flvector dv1) (dir->flvector dv2)))

(: dir-cross (-> Dir Dir Dir))
(define (dir-cross dv1 dv2)
  (Dir (flv3cross (dir->flvector dv1) (dir->flvector dv2))))

(: pos+ (-> Pos Dir Pos))
(define (pos+ v dv)
  (define-values (x y z) (vec-values v))
  (define-values (dx dy dz) (vec-values dv))
  (Pos (flvector (+ x dx) (+ y dy) (+ z dz))))

(: pos- (-> Pos Pos Dir))
(define (pos- v1 v2)
  (define-values (x1 y1 z1) (vec-values v1))
  (define-values (x2 y2 z2) (vec-values v2))
  (Dir (flvector (- x1 x2) (- y1 y2) (- z1 z2))))

(: pos-between (-> Pos Pos Real Pos))
(define (pos-between v1 v2 a)
  (let* ([a  (fl a)]
         [1-a  (- 1.0 a)])
    (define-values (x1 y1 z1) (vec-values v1))
    (define-values (x2 y2 z2) (vec-values v2))
    (Pos (flvector (+ (* x1 1-a) (* x2 a))
                   (+ (* y1 1-a) (* y2 a))
                   (+ (* z1 1-a) (* z2 a))))))

(: pos-dist^2 (-> Pos Pos Flonum))
(define (pos-dist^2 v1 v2)
  (dir-dist^2 (pos- v2 v1)))

(: pos-dist (-> Pos Pos Flonum))
(define (pos-dist v1 v2)
  (dir-dist (pos- v2 v1)))

;; ===================================================================================================
;; Affine transforms

(define print-affine
  (make-constructor-style-printer
   (λ ([t : Affine]) 'cols->affine)
   (λ ([t : Affine])
     (define-values (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
       (flvector-values (fltransform3-forward (->flaffine3 (affine-transform t))) 12))
     (list (dir m00 m10 m20)
           (dir m01 m11 m21)
           (dir m02 m12 m22)
           (pos m03 m13 m23)))))

(current-affine-custom-write print-affine)

(: cols->affine (-> Dir Dir Dir Pos Affine))
(define (cols->affine x y z p)
  (affine (cols->flaffine3 (dir->flvector x)
                           (dir->flvector y)
                           (dir->flvector z)
                           (pos->flvector p))))

(: affine->cols (-> Affine (Values Dir Dir Dir Pos)))
(define (affine->cols t)
  (define-values (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
    (flvector-values (fltransform3-forward (->flaffine3 (affine-transform t))) 12))
  (values (flvector->dir (flvector m00 m10 m20))
          (flvector->dir (flvector m01 m11 m21))
          (flvector->dir (flvector m02 m12 m22))
          (flvector->pos (flvector m03 m13 m23))))
