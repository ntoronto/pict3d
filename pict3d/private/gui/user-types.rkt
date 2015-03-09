#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax)
         (only-in racket/unsafe/ops unsafe-flvector-ref)
         racket/match
         typed/racket/class
         typed/racket/draw
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
 tag?
 ;; Materials
 Material
 material?
 material
 make-material
 ;; Colors
 (rename-out [-RGBA RGBA]
             [-Emitted Emitted])
 col-flvector
 rgba?
 emitted?
 rgba
 rgba-red
 rgba-green
 rgba-blue
 rgba-alpha
 emitted
 emitted-red
 emitted-green
 emitted-blue
 emitted-intensity
 ;; Vectors
 (rename-out [-Pos Pos]
             [-Dir Dir])
 vec-flvector
 pos?
 dir?
 origin
 pos
 dir
 pos-x
 pos-y
 pos-z
 dir-dx
 dir-dy
 dir-dz
 dir+
 dir-
 dir-negate
 dir-scale
 dir-dist^2
 dir-dist
 dir-normalize
 dir-dot
 dir-proj
 dir-cross
 angles->dir
 dir->angles
 pos+
 pos-
 pos-between
 pos-dist
 pos-dist^2
 ;; Vertices
 Vertex
 vertex
 make-vertex
 vertex?
 vertex-pos
 vertex-normal
 vertex-color
 vertex-emitted
 vertex-material
 ;; Affine transforms
 Affine
 affine?
 identity-affine
 cols->affine
 affine->cols
 affine-compose
 affine-inverse
 affine-consistent?
 transform-pos
 transform-dir
 transform-norm
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

(: color-vec-length (-> Color-Vector Index))
(define (color-vec-length xs)
  (cond [(flvector? xs)  (flvector-length xs)]
        [(list? xs)  (length xs)]
        [else  (vector-length xs)]))

(: color-vec->flv (-> Color-Vector Index (U #f FlVector)))
(define (color-vec->flv xs n)
  (cond [(flvector? xs)  (if (= n (flvector-length xs)) (flvector-copy xs) #f)]
        [(list? xs)  (if (= n (length xs)) (list->flvector xs) #f)]
        [else  (if (= n (vector-length xs)) (vector->flvector xs) #f)]))

(: color->rgb (-> Symbol (U String (Instance Color%)) (Vector Byte Byte Byte)))
(define (color->rgb name orig-col)
  (let* ([col  orig-col]
         [col  (if (string? col) (send the-color-database find-color col) col)])
    (if col
        (vector (send col red) (send col green) (send col blue))
        (raise-argument-error name "known color" orig-col))))

;; ---------------------------------------------------------------------------------------------------

(define print-col
  (λ ([name : Symbol])
    (make-constructor-style-printer
     (λ ([c : col]) name)
     (λ ([c : col])
       (define v (col-flvector c))
       (list (flvector-ref v 0)
             (flvector-ref v 1)
             (flvector-ref v 2)
             (flvector-ref v 3))))))

(struct col ([flvector : FlVector]) #:transparent
  #:property prop:custom-print-quotable 'never)

(struct RGBA col () #:transparent
  #:property prop:custom-write (print-col 'rgba))
  
(struct Emitted col () #:transparent
  #:property prop:custom-write (print-col 'emitted))

(define-type -RGBA RGBA)
(define-type -Emitted Emitted)
(define rgba? RGBA?)
(define emitted? Emitted?)

(define-type User-Color (U String Real Color-Vector (Instance Color%)))

(: ->flcolor4 (->* [Symbol User-Color] [Real] (Values Flonum Flonum Flonum Flonum)))
(define (->flcolor4 name col [w 1.0])
  (cond
    [(real? col)
     (define r (fl col))
     (values r r r (fl w))]
    [(or (list? col) (vector? col) (flvector? col))
     (define n (color-vec-length col))
     (cond
       [(= n 4)
        (define v (assert (color-vec->flv col 4) values))
        (values (flvector-ref v 0)
                (flvector-ref v 1)
                (flvector-ref v 2)
                (* (fl w) (flvector-ref v 3)))]
       [(= n 3)
        (define c (make-flvector 4 (fl w)))
        (for ([r  (ann col (Sequenceof Real))]
              [i  (in-range 3)])
          (flvector-set! c i (fl r)))
        (values (flvector-ref c 0)
                (flvector-ref c 1)
                (flvector-ref c 2)
                (fl w))]
       [else
        (raise-argument-error 'name "length-3 or length-4 vector" col)])]
    [else
     (match-define (vector r g b) (color->rgb 'name col))
     (values (/ (fl r) 255.0)
             (/ (fl g) 255.0)
             (/ (fl b) 255.0)
             (fl w))]))

(: make-rgba (-> Flonum Flonum Flonum Flonum RGBA))
(define (make-rgba r g b a)
  (let ([r  (min 1.0 (max 0.0 r))]
        [g  (min 1.0 (max 0.0 g))]
        [b  (min 1.0 (max 0.0 b))]
        [a  (min 1.0 (max 0.0 a))])
    (RGBA (flvector r g b a))))

(: rgba (case-> (-> (U RGBA User-Color) RGBA)
                (-> (U RGBA User-Color) Real RGBA)
                (-> Real Real Real RGBA)
                (-> Real Real Real Real RGBA)))
(define rgba
  (case-lambda
    [(col)  (rgba col 1.0)]
    [(col alpha)  (let ([col  (if (rgba? col) (col-flvector col) col)])
                    (let-values ([(r g b a)  (->flcolor4 'rgba col alpha)])
                      (make-rgba r g b a)))]
    [(r g b)  (make-rgba (fl r) (fl g) (fl b) 1.0)]
    [(r g b a)  (make-rgba (fl r) (fl g) (fl b) (fl a))]))

(define black-flvector (flvector 0.0 0.0 0.0 0.0))

(: make-emitted (-> Flonum Flonum Flonum Flonum Emitted))
(define (make-emitted r g b i)
  (let ([r  (max 0.0 r)]
        [g  (max 0.0 g)]
        [b  (max 0.0 b)]
        [i  (max 0.0 i)])
    (define mx (max r g b))
    (cond [(or (= mx 0.0) (= i 0.0))  (Emitted black-flvector)]
          [else  (Emitted (flvector (/ r mx) (/ g mx) (/ b mx) (* i mx)))])))

(: emitted (case-> (-> (U Emitted User-Color) Emitted)
                   (-> (U Emitted User-Color) Real Emitted)
                   (-> Real Real Real Emitted)
                   (-> Real Real Real Real Emitted)))
(define emitted
  (case-lambda
    [(col)  (emitted col 1.0)]
    [(col intensity)  (let ([col  (if (emitted? col) (col-flvector col) col)])
                        (let-values ([(r g b i)  (->flcolor4 'emitted col intensity)])
                          (make-emitted r g b i)))]
    [(r g b)  (make-emitted (fl r) (fl g) (fl b) 1.0)]
    [(r g b i)  (make-emitted (fl r) (fl g) (fl b) (fl i))]))

(define rgba-red   (λ ([c : RGBA]) (flvector-ref (col-flvector c) 0)))
(define rgba-green (λ ([c : RGBA]) (flvector-ref (col-flvector c) 1)))
(define rgba-blue  (λ ([c : RGBA]) (flvector-ref (col-flvector c) 2)))
(define rgba-alpha (λ ([c : RGBA]) (flvector-ref (col-flvector c) 3)))

(define emitted-red       (λ ([e : Emitted]) (flvector-ref (col-flvector e) 0)))
(define emitted-green     (λ ([e : Emitted]) (flvector-ref (col-flvector e) 1)))
(define emitted-blue      (λ ([e : Emitted]) (flvector-ref (col-flvector e) 2)))
(define emitted-intensity (λ ([e : Emitted]) (flvector-ref (col-flvector e) 3)))

;; ===================================================================================================
;; Vectors

(define print-vec
  (λ ([name : Symbol])
    (make-constructor-style-printer
     (λ ([v : vec]) name)
     (λ ([v : vec])
       (define-values (x y z) (vec-values v))
       (list x y z)))))

(struct vec ([flvector : FlVector]) #:transparent
  #:property prop:custom-print-quotable 'never)

(struct Pos vec () #:transparent
  #:property prop:custom-write (print-vec 'pos))

(struct Dir vec () #:transparent
  #:property prop:custom-write (print-vec 'dir))

(define-type -Pos Pos)
(define-type -Dir Dir)
(define pos? Pos?)
(define dir? Dir?)

(define origin (Pos zero-flv3))

(define-syntax (define/provide-unit-vectors stx)
  (define/with-syntax ([name val] ...)
    (for*/list ([nx  (in-list '("-x" "" "+x"))]
                [ny  (in-list '("-y" "" "+y"))]
                [nz  (in-list '("-z" "" "+z"))]
                #:unless (and (equal? nx "") (equal? ny "") (equal? nz "")))
      (define str (string-append nx ny nz))
      (list (format-id stx "~a" str)
            (format-id stx "~a-flv3" str))))
  #'(begin
      (define name (Dir val)) ...
      (provide name ...)))

(define/provide-unit-vectors)

(define-syntax-rule (vec-values stx)
  (let ([v  (vec-flvector stx)])
    (values (unsafe-flvector-ref v 0)
            (unsafe-flvector-ref v 1)
            (unsafe-flvector-ref v 2))))

(: ->flv3 (-> Symbol (U FlVector (Listof Real) (Vectorof Real)) FlVector))
(define (->flv3 name v)
  (cond [(flvector? v)
         (if (= 3 (flvector-length v))
             v
             (raise-argument-error name "length-3 flvector" v))]
        [(vector? v)
         (if (= 3 (vector-length v))
             (vector->flvector v)
             (raise-argument-error name "length-3 vector" v))]
        [else
         (if (= 3 (length v))
             (list->flvector v)
             (raise-argument-error 'name "length-3 list" v))]))

(: pos (case-> (-> (U FlVector (Listof Real) (Vectorof Real)) Pos)
               (-> Real Real Real Pos)))
(define pos
  (case-lambda
    [(v)  (Pos (->flv3 'pos v))]
    [(x y z)  (Pos (flvector (fl x) (fl y) (fl z)))]))

(: dir (case-> (-> (U FlVector (Listof Real) (Vectorof Real)) Dir)
               (-> Real Real Real Dir)))
(define dir
  (case-lambda
    [(dv)  (Dir (->flv3 'dir dv))]
    [(dx dy dz)  (Dir (flvector (fl dx) (fl dy) (fl dz)))]))

(define pos-x (λ ([v : Pos]) (flvector-ref (vec-flvector v) 0)))
(define pos-y (λ ([v : Pos]) (flvector-ref (vec-flvector v) 1)))
(define pos-z (λ ([v : Pos]) (flvector-ref (vec-flvector v) 2)))

(define dir-dx (λ ([dv : Pos]) (flvector-ref (vec-flvector dv) 0)))
(define dir-dy (λ ([dv : Pos]) (flvector-ref (vec-flvector dv) 1)))
(define dir-dz (λ ([dv : Pos]) (flvector-ref (vec-flvector dv) 2)))

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
  (define v (flv3normalize (vec-flvector dv)))
  (and v (let-values ([(dx dy dz)  (flv3-values v)])
           (if (and (< -inf.0 dx +inf.0)
                    (< -inf.0 dy +inf.0)
                    (< -inf.0 dz +inf.0))
               (Dir v)
               #f))))

(: dir-dot (-> Dir Dir Flonum))
(define (dir-dot dv1 dv2)
  (flv3dot (vec-flvector dv1) (vec-flvector dv2)))

(: dir-proj (-> Dir Dir (U #f Dir)))
(define (dir-proj A B)
  (define d (dir-dist^2 B))
  (cond [(= d 0.0)  #f]
        [else  (dir-scale B (/ (dir-dot A B) d))]))

(: dir-cross (-> Dir Dir Dir))
(define (dir-cross dv1 dv2)
  (Dir (flv3cross (vec-flvector dv1) (vec-flvector dv2))))

(: angles->dir (-> Real Real Dir))
(define (angles->dir ang alt)
  (let ([ang  (fl (degrees->radians ang))]
        [alt  (fl (degrees->radians alt))])
    (define c0 (flcos ang))
    (define s0 (flsin ang))
    (define c1 (flcos alt))
    (define s1 (flsin alt))
    (dir (* c0 c1) (* s0 c1) s1)))

(: dir->angles (-> Dir (Values Flonum Flonum)))
(define (dir->angles dv)
  (define-values (x y z) (vec-values dv))
  (define r (flsqrt (+ (sqr x) (sqr y) (sqr z))))
  (values (radians->degrees (atan y x))
          (radians->degrees (asin (/ z r)))))

(: pos+ (->* [Pos Dir] [Real] Pos))
(define pos+
  (case-lambda
    [(v dv)
     (cond [(eq? v origin)  (Pos (vec-flvector dv))]
           [else  (define-values (x y z) (vec-values v))
                  (define-values (dx dy dz) (vec-values dv))
                  (Pos (flvector (+ x dx) (+ y dy) (+ z dz)))])]
    [(v dv s)
     (let ([s  (fl s)])
       (cond [(eq? v origin)
              (define-values (dx dy dz) (vec-values dv))
              (Pos (flvector (* s dx) (* s dy) (* s dz)))]
             [else
              (Pos (flv3fma (vec-flvector dv) (fl s) (vec-flvector v)))]))]))

(: pos- (-> Pos Pos Dir))
(define (pos- v1 v2)
  (cond [(eq? v2 origin)
         (Dir (vec-flvector v1))]
        [else
         (define-values (x1 y1 z1) (vec-values v1))
         (define-values (x2 y2 z2) (vec-values v2))
         (Dir (flvector (- x1 x2) (- y1 y2) (- z1 z2)))]))

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
  (flv3dist^2 (vec-flvector v1) (vec-flvector v2)))

(: pos-dist (-> Pos Pos Flonum))
(define (pos-dist v1 v2)
  (flv3dist (vec-flvector v1) (vec-flvector v2)))

;; ===================================================================================================
;; Vertex data

(: print-vertex (-> Vertex Output-Port (U #t #f 0 1) Void))
(define (print-vertex vert port mode)
  (match-define (Vertex v n c e m) vert)
  (write-string "(vertex" port)
  (write-string (format " ~v" v) port)
  (when n (write-string (format " #:normal ~v" n) port))
  (when c (write-string (format " #:color ~v" c) port))
  (when e (write-string (format " #:emitted ~v" e) port))
  (when m (write-string (format " #:material ~v" m) port))
  (write-string ")" port)
  (void))

(struct Vertex ([pos : Pos]
                [normal : (U #f Dir)]
                [color : (U #f RGBA)]
                [emitted : (U #f Emitted)]
                [material : (U #f Material)])
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-vertex)

(define vertex? Vertex?)
(define make-vertex Vertex)
(define vertex-pos Vertex-pos)
(define vertex-normal Vertex-normal)
(define vertex-color Vertex-color)
(define vertex-emitted Vertex-emitted)
(define vertex-material Vertex-material)

(: vertex
   (->* [Pos]
        [#:normal (U #f Dir) #:color (U #f RGBA) #:emitted (U #f Emitted) #:material (U #f Material)]
        Vertex))
(define (vertex v
                #:normal [n #f]
                #:color [c #f]
                #:emitted [e #f]
                #:material [m #f])
  (let ([n  (if n (dir-normalize n) #f)])
    (make-vertex v n c e m)))

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
  (affine (cols->flaffine3 (vec-flvector x)
                           (vec-flvector y)
                           (vec-flvector z)
                           (vec-flvector p))))

(: affine->cols (-> Affine (Values Dir Dir Dir Pos)))
(define (affine->cols t)
  (define-values (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
    (flvector-values (fltransform3-forward (->flaffine3 (affine-transform t))) 12))
  (values (dir (flvector m00 m10 m20))
          (dir (flvector m01 m11 m21))
          (dir (flvector m02 m12 m22))
          (pos (flvector m03 m13 m23))))

(: transform-pos (-> Pos Affine Pos))
(define (transform-pos v t)
  (Pos (flt3apply/pos (affine-transform t) (vec-flvector v))))

(: transform-dir (-> Dir Affine Dir))
(define (transform-dir v t)
  (Dir (flt3apply/dir (affine-transform t) (vec-flvector v))))

(: transform-norm (-> Dir Affine Dir))
(define (transform-norm v t)
  (Dir (flt3apply/nrm (affine-transform t) (vec-flvector v))))
