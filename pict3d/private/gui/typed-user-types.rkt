#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax)
         (only-in racket/unsafe/ops
                  unsafe-flvector-ref)
         racket/match
         racket/vector
         racket/list
         typed/racket/class
         typed/racket/draw
         math/flonum
         math/base
         "../math.rkt"
         "../engine.rkt"
         "../soup.rkt"
         "../utils.rkt")

(provide
 Tag
 tag?
 (rename-out [-Interval Interval])
 interval?
 interval
 interval-min
 interval-max
 interval-values
 zero-interval
 unit-interval
 (rename-out [-Arc Arc])
 arc?
 arc
 arc-min
 arc-max
 arc-values
 zero-arc
 circle-arc
 ;; Materials
 (rename-out [-Material Material])
 flv4->material
 material?
 material
 make-material
 material-ambient
 material-diffuse
 material-specular
 material-roughness
 ;; Colors
 (rename-out [-RGBA RGBA]
             [-Emitted Emitted])
 flv4-values
 flv4->rgba
 flv4->emitted
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
 flv3-values
 flv3->pos
 flv3->dir
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
 zero-dir
 dir+
 dir-
 dir-negate
 dir-scale
 dir-dist^2
 dir-dist
 dir-norm
 dir-normalize
 dir-dot
 dir-cross
 dir-project
 dir-reject
 dir-reflect
 angles->dir
 dir->angles
 pos+
 pos-
 pos-between
 pos-dist
 pos-dist^2
 ;; Vertices
 Vertex
 vtx->vertex
 face->vertices
 vertex
 make-vertex
 vertex?
 vertex-pos
 vertex-normal
 vertex-color
 vertex-emitted
 vertex-material
 set-vertex-pos
 set-vertex-normal
 set-vertex-color
 set-vertex-emitted
 set-vertex-material
 ;; Affine transforms
 (rename-out [-Affine Affine])
 flaffine3->affine
 affine?
 identity-affine
 affine
 affine->cols*
 cols->affine
 affine->cols
 affine-x-axis
 affine-y-axis
 affine-z-axis
 affine-origin
 affine-compose
 affine-inverse
 affine-consistent?
 transform-pos
 transform-dir
 transform-norm
 ;; Smooth functions
 (rename-out [-Differentiable Differentiable])
 fldiff3->differentiable
 differentiable?
 differentiable
 Smooth
 smooth?
 smooth-function
 smooth-jacobian
 flsmooth3->smooth
 smooth-compose
 smooth-consistent?
 smooth-approximate
 smooth-between
 deform-pos
 deform-dir
 deform-norm
 deform-affine
 ;; Surface data
 (rename-out [-Surface-Data Surface-Data])
 trace-data->surface-data
 surface-data
 make-surface-data
 surface-data?
 surface-data-dist
 surface-data-pos
 surface-data-normal
 surface-data-path
 )

;; ===================================================================================================
;; Intervals

(define print-interval
  (make-constructor-style-printer
   (λ ([a : Interval]) 'interval)
   (λ ([a : Interval]) (list (Interval-min a) (Interval-max a)))))

(struct Interval ([min : Flonum] [max : Flonum])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-interval)

(define-type -Interval Interval)
(define interval? Interval?)
(define interval-min Interval-min)
(define interval-max Interval-max)

(: interval (-> Real Real Interval))
(define (interval mn mx)
  (let ([mn  (fl mn)]
        [mx  (fl mx)])
    (Interval (min mn mx) (max mn mx))))

(: interval-values (-> Interval (Values Flonum Flonum)))
(define (interval-values i)
  (values (interval-min i) (interval-max i)))

(define zero-interval (Interval 0.0 0.0))
(define unit-interval (Interval 0.0 1.0))

;; ===================================================================================================
;; Arcs

(define print-arc
  (make-constructor-style-printer
   (λ ([a : Arc]) 'arc)
   (λ ([a : Arc]) (list (Arc-min a) (Arc-max a)))))

(struct Arc ([min : Flonum] [max : Flonum])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-arc)

(define-type -Arc Arc)
(define arc? Arc?)
(define arc-min Arc-min)
(define arc-max Arc-max)

(: arc (-> Real Real Arc))
(define (arc mn mx)
  (Arc (fl mn) (fl mx)))

(: arc-values (-> Arc (Values Flonum Flonum)))
(define (arc-values a)
  (values (arc-min a) (arc-max a)))

(define zero-arc (Arc 0.0 0.0))
(define circle-arc (Arc 0.0 360.0))

;; ===================================================================================================
;; Materials

(: print-material (-> Material Output-Port (U #t #f 0 1) Void))
(define (print-material mat port mode)
  (call/flv4-values mat
    (λ (a d s r)
      (write-string "(material" port)
      (when (> a 0.0) (write-string (format " #:ambient ~v" a) port))
      (when (> d 0.0) (write-string (format " #:diffuse ~v" d) port))
      (when (> s 0.0) (write-string (format " #:specular ~v" s) port))
      (when (> r 0.0) (write-string (format " #:roughness ~v" r) port))
      (write-string ")" port)
      (void))))

(struct Material FlV4 ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-material)

(define-type -Material Material)
(define material? Material?)

(: flv4->material (-> FlV4 Material))
(define (flv4->material v)
  (if (material? v) v (Material (FlV4-flvector v))))

(: make-material (-> Flonum Flonum Flonum Flonum Material))
(define (make-material a d s r)
  (let ([a  (flclamp a 0.0 1.0)]
        [d  (flclamp d 0.0 1.0)]
        [s  (flclamp s 0.0 1.0)]
        [r  (flclamp r 0.0 1.0)])
    (Material (flvector a d s r))))

(: material (->* [] [#:ambient Real #:diffuse Real #:specular Real #:roughness Real] Material))
(define (material #:ambient [a 0.0] #:diffuse [d 0.0] #:specular [s 0.0] #:roughness [r 0.1])
  (make-material (fl a) (fl d) (fl s) (fl r)))

(define material-ambient   (λ ([m : Material]) (flv4-ref m 0)))
(define material-diffuse   (λ ([m : Material]) (flv4-ref m 1)))
(define material-specular  (λ ([m : Material]) (flv4-ref m 2)))
(define material-roughness (λ ([m : Material]) (flv4-ref m 3)))

;; ===================================================================================================
;; Colors

(define-type Color-Vector (U (Listof Real) (Vectorof Real) FlVector))

(: color-vec->list (-> Color-Vector (Listof Flonum)))
(define (color-vec->list xs)
  (cond [(flvector? xs)  (flvector->list xs)]
        [(list? xs)  (map fl xs)]
        [else  (vector->list (vector-map fl xs))]))

(: color->rgb (-> Symbol (U String (Instance Color%)) (Vector Byte Byte Byte)))
(define (color->rgb name orig-col)
  (let* ([col  orig-col]
         [col  (if (string? col) (send the-color-database find-color col) col)])
    (if col
        (vector (send col red) (send col green) (send col blue))
        (raise-argument-error name "known color" orig-col))))

;; ---------------------------------------------------------------------------------------------------

(: flv4-values (-> FlV4 (Values Flonum Flonum Flonum Flonum)))
(define (flv4-values v)
  (call/flv4-values v values))

(define print-color
  (λ ([name : Symbol])
    (make-constructor-style-printer
     (λ ([c : FlV4]) name)
     (λ ([c : FlV4]) (call/flv4-values c list)))))

(struct RGBA FlV4 ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (print-color 'rgba))

(struct Emitted FlV4 ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (print-color 'emitted))

(define-type -RGBA RGBA)
(define-type -Emitted Emitted)
(define rgba? RGBA?)
(define emitted? Emitted?)

(: flv4->rgba (-> FlV4 RGBA))
(define (flv4->rgba c)
  (if (rgba? c) c (RGBA (FlV4-flvector c))))

(: flv4->emitted (-> FlV4 Emitted))
(define (flv4->emitted e)
  (if (emitted? e) e (Emitted (FlV4-flvector e))))

(define-type User-Color (U String Real Color-Vector (Instance Color%)))

(: ->flcolor4 (->* [Symbol User-Color] [Real] (Values Flonum Flonum Flonum Flonum)))
(define (->flcolor4 name col [w 1.0])
  (cond
    [(real? col)
     (define r (fl col))
     (values r r r (fl w))]
    [(or (list? col) (vector? col) (flvector? col))
     (match (color-vec->list col)
       [(list r g b new-w)
        (values (fl r) (fl g) (fl b) (* (fl w) (fl new-w)))]
       [(list r g b)
        (values (fl r) (fl g) (fl b) (fl w))]
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
    [(col alpha)
     (define-values (r g b a)
       (if (rgba? col)
           (call/flv4-values col
             (λ (r g b a) (values r g b (* a (fl alpha)))))
           (->flcolor4 'rgba col alpha)))
     (make-rgba r g b a)]
    [(r g b)  (make-rgba (fl r) (fl g) (fl b) 1.0)]
    [(r g b a)  (make-rgba (fl r) (fl g) (fl b) (fl a))]))

(: make-emitted (-> Flonum Flonum Flonum Flonum Emitted))
(define (make-emitted r g b i)
  (let ([r  (max 0.0 r)]
        [g  (max 0.0 g)]
        [b  (max 0.0 b)]
        [i  (max 0.0 i)])
    (define mx (max r g b))
    (cond [(or (= mx 0.0) (= i 0.0))  (flv4->emitted zero-flv4)]
          [else  (Emitted (flvector (/ r mx) (/ g mx) (/ b mx) (* i mx)))])))

(: emitted (case-> (-> (U Emitted User-Color) Emitted)
                   (-> (U Emitted User-Color) Real Emitted)
                   (-> Real Real Real Emitted)
                   (-> Real Real Real Real Emitted)))
(define emitted
  (case-lambda
    [(col)  (emitted col 1.0)]
    [(col intensity)
     (define-values (r g b i)
       (if (emitted? col)
           (call/flv4-values col
             (λ (r g b i) (values r g b (* i (fl intensity)))))
           (->flcolor4 'emitted col intensity)))
     (make-emitted r g b i)]
    [(r g b)  (make-emitted (fl r) (fl g) (fl b) 1.0)]
    [(r g b i)  (make-emitted (fl r) (fl g) (fl b) (fl i))]))

(define rgba-red   (λ ([c : RGBA]) (flv4-ref c 0)))
(define rgba-green (λ ([c : RGBA]) (flv4-ref c 1)))
(define rgba-blue  (λ ([c : RGBA]) (flv4-ref c 2)))
(define rgba-alpha (λ ([c : RGBA]) (flv4-ref c 3)))

(define emitted-red       (λ ([e : Emitted]) (flv4-ref e 0)))
(define emitted-green     (λ ([e : Emitted]) (flv4-ref e 1)))
(define emitted-blue      (λ ([e : Emitted]) (flv4-ref e 2)))
(define emitted-intensity (λ ([e : Emitted]) (flv4-ref e 3)))

;; ===================================================================================================
;; Vectors

(: flv3-values (-> FlV3 (Values Flonum Flonum Flonum)))
(define (flv3-values v)
  (call/flv3-values v values))

(define print-flv3-value
  (λ ([name : Symbol])
    (make-constructor-style-printer
     (λ ([v : FlV3]) name)
     (λ ([v : FlV3]) (call/flv3-values v list)))))

(: print-pos (-> Pos Output-Port (U Boolean Zero One) Void))
(define (print-pos v port mode)
  (cond [(equal? v origin)  (write-string "origin" port)
                            (void)]
        [else  ((print-flv3-value 'pos) v port mode)]))

(: print-dir (-> Dir Output-Port (U Boolean Zero One) Void))
(define (print-dir dv port mode)
  (define name (hash-ref dir-names dv #f))
  (cond [name  (write-string name port)
               (void)]
        [else  ((print-flv3-value 'dir) dv port mode)]))

(struct Pos FlV3 ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-pos)

(struct Dir FlV3 ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-dir)

(define-type -Pos Pos)
(define-type -Dir Dir)
(define pos? Pos?)
(define dir? Dir?)

(: flv3->pos (-> FlV3 Pos))
(define (flv3->pos v)
  (if (pos? v) v (Pos (FlV3-flvector v))))

(: flv3->dir (-> FlV3 Dir))
(define (flv3->dir v)
  (if (dir? v) v (Dir (FlV3-flvector v))))

(define origin (flv3->pos zero-flv3))

(: dir-names (HashTable Dir String))
(define dir-names (make-hash))

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
      (define name (flv3->dir val)) ...
      (hash-set! dir-names name (symbol->string 'name)) ...
      (provide name ...)))

(define/provide-unit-vectors)

(: rational-flvector3 (-> Symbol Flonum Flonum Flonum FlVector))
(define (rational-flvector3 name x y z)
  (if (and (< -inf.0 (min x y z))
           (< (max x y z) +inf.0))
      (flvector x y z)
      (error name "expected rational coordinates; given ~e ~e ~e" x y z)))

(: ->flvector3 (-> Symbol (U FlVector (Listof Real) (Vectorof Real)) FlVector))
(define (->flvector3 name v)
  (cond [(flvector? v)
         (if (= 3 (flvector-length v))
             (rational-flvector3 name
                                 (unsafe-flvector-ref v 0)
                                 (unsafe-flvector-ref v 1)
                                 (unsafe-flvector-ref v 2))
             (raise-argument-error name "length-3 flvector, list or vector" v))]
        [else
         (match v
           [(vector x y z)
            (rational-flvector3 name (fl x) (fl y) (fl z))]
           [(list x y z)
            (rational-flvector3 name (fl x) (fl y) (fl z))]
           [_
            (raise-argument-error name "length-3 flvector, list or vector" v)])]))

(: pos (case-> (-> (U FlVector (Listof Real) (Vectorof Real)) Pos)
               (-> Real Real Real Pos)))
(define pos
  (case-lambda
    [(v)  (Pos (->flvector3 'pos v))]
    [(x y z)  (Pos (rational-flvector3 'pos (fl x) (fl y) (fl z)))]))

(: dir (case-> (-> (U FlVector (Listof Real) (Vectorof Real)) Dir)
               (-> Real Real Real Dir)))
(define dir
  (case-lambda
    [(dv)  (Dir (->flvector3 'dir dv))]
    [(dx dy dz)  (Dir (rational-flvector3 'dir (fl dx) (fl dy) (fl dz)))]))

(define pos-x (λ ([v : Pos]) (flv3-ref v 0)))
(define pos-y (λ ([v : Pos]) (flv3-ref v 1)))
(define pos-z (λ ([v : Pos]) (flv3-ref v 2)))

(define dir-dx (λ ([dv : Dir]) (flv3-ref dv 0)))
(define dir-dy (λ ([dv : Dir]) (flv3-ref dv 1)))
(define dir-dz (λ ([dv : Dir]) (flv3-ref dv 2)))

(define zero-dir (dir 0 0 0))

(: dir+ (-> Dir Dir Dir))
(define (dir+ dv1 dv2)
  (flv3->dir (flv3+ dv1 dv2)))

(: dir- (-> Dir Dir Dir))
(define (dir- dv1 dv2)
  (flv3->dir (flv3- dv1 dv2)))

(: dir-negate (-> Dir Dir))
(define (dir-negate dv)
  (flv3->dir (flv3neg dv)))

(: dir-scale (-> Dir Real Dir))
(define (dir-scale dv s)
  (flv3->dir (flv3* dv (fl s))))

(: dir-dist^2 (-> Dir Flonum))
(define (dir-dist^2 dv)
  (flv3mag^2 dv))

(: dir-dist (-> Dir Flonum))
(define (dir-dist dv)
  (flv3mag dv))

(: dir-norm (-> Dir Nonnegative-Real Flonum))
(define (dir-norm dv p)
  (let ([p  (fl p)])
    (call/flv3-values dv
      (λ (dx dy dz)
        (let ([dx  (abs dx)]
              [dy  (abs dy)]
              [dz  (abs dz)])
          (cond
            [(= p 2.0)     (flsqrt (+ (* dx dx) (* dy dy) (* dz dz)))]
            [(> p 1e16)    (max dx dy dz)]
            [(= p 1.0)     (+ dx dy dz)]
            [(= p 0.0)     (if (> (max dx dy dz) 0.0) +inf.0 0.0)]
            [else  (flexpt (+ (flexpt dx p) (flexpt dy p) (flexpt dz p)) (/ p))]))))))

(: dir-normalize (-> Dir (U #f Dir)))
(define (dir-normalize dv)
  (define v (flv3normalize dv))
  (and v (flv3->dir v)))

(: dir-dot (-> Dir Dir Flonum))
(define (dir-dot dv1 dv2)
  (flv3dot dv1 dv2))

(: dir-cross (-> Dir Dir Dir))
(define (dir-cross dv1 dv2)
  (flv3->dir (flv3cross dv1 dv2)))

(: dir-project (-> Dir Dir (U #f Dir)))
(define (dir-project dv1 dv2)
  (define dv (flv3proj dv1 dv2))
  (and dv (flv3->dir dv)))

(: dir-reject (->* [Dir Dir] [Real] Dir))
(define (dir-reject dv n [s 1.0])
  (flv3->dir (flv3rej dv n (fl s))))

(: dir-reflect (-> Dir Dir Dir))
(define (dir-reflect dv n)
  (flv3->dir (flv3refl dv n)))

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
  (call/flv3-values dv
    (λ (x y z)
      (define r (flsqrt (+ (sqr x) (sqr y) (sqr z))))
      (values (radians->degrees (atan y x))
              (radians->degrees (asin (/ z r)))))))

(: pos+ (->* [Pos Dir] [Real] Pos))
(define pos+
  (case-lambda
    [(v dv)
     (if (eq? v origin)
         (flv3->pos dv)
         (flv3->pos (flv3+ v dv)))]
    [(v dv s)
     (if (eq? v origin)
         (flv3->pos (flv3* dv (fl s)))
         (flv3->pos (flv3fma dv (fl s) v)))]))

(: pos- (-> Pos Pos Dir))
(define (pos- v1 v2)
  (if (eq? v2 origin)
      (flv3->dir v1)
      (flv3->dir (flv3- v1 v2))))

(: pos-between (-> Pos Pos Real Pos))
(define (pos-between v1 v2 a)
  (let* ([a  (fl a)]
         [1-a  (- 1.0 a)])
    (call/flv3-values v1
      (λ (x1 y1 z1)
        (call/flv3-values v2
          (λ (x2 y2 z2)
            (Pos (flvector (+ (* x1 1-a) (* x2 a))
                           (+ (* y1 1-a) (* y2 a))
                           (+ (* z1 1-a) (* z2 a))))))))))

(: pos-dist^2 (-> Pos Pos Flonum))
(define (pos-dist^2 v1 v2)
  (flv3dist^2 v1 v2))

(: pos-dist (-> Pos Pos Flonum))
(define (pos-dist v1 v2)
  (flv3dist v1 v2))

;; ===================================================================================================
;; Homogeneous coordinates
#|
Don't know if I want these for anything other than specifying projective matrices

(define print-hom
  (make-constructor-style-printer
   (λ ([v : FlV4]) 'hom)
   (λ ([v : FlV4]) (call/flv4-values v list))))

(struct Hom FlV4 ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-hom)

(define-type -Hom Hom)
(define hom? Hom?)

(: flv4->hom (-> FlV4 Hom))
(define (flv4->hom v)
  (if (hom? v) v (Hom (FlV4-flvector v))))

(: rational-flvector4 (-> Symbol Flonum Flonum Flonum Flonum FlVector))
(define (rational-flvector4 name x y z w)
  (if (and (< -inf.0 (min x y z w))
           (< (max x y z w) +inf.0))
      (flvector x y z w)
      (error name "expected rational coordinates; given ~e ~e ~e ~e" x y z w)))

(: ->flvector4 (-> Symbol (U FlVector (Listof Real) (Vectorof Real)) FlVector))
(define (->flvector4 name v)
  (cond [(flvector? v)
         (if (= 4 (flvector-length v))
             (rational-flvector4 name
                                 (unsafe-flvector-ref v 0)
                                 (unsafe-flvector-ref v 1)
                                 (unsafe-flvector-ref v 2)
                                 (unsafe-flvector-ref v 3))
             (raise-argument-error name "length-4 flvector, list or vector" v))]
        [else
         (match v
           [(vector x y z w)
            (rational-flvector4 name (fl x) (fl y) (fl z) (fl w))]
           [(list x y z w)
            (rational-flvector4 name (fl x) (fl y) (fl z) (fl w))]
           [_
            (raise-argument-error name "length-4 flvector, list or vector" v)])]))

(: hom (case-> (-> (U FlVector (Listof Real) (Vectorof Real)) Hom)
               (-> Real Real Real Real Hom)))
(define hom
  (case-lambda
    [(v)  (Hom (->flvector4 'hom v))]
    [(x y z w)  (Hom (rational-flvector4 'hom (fl x) (fl y) (fl z) (fl w)))]))

(define hom-x (λ ([v : Hom]) (flv4-ref v 0)))
(define hom-y (λ ([v : Hom]) (flv4-ref v 1)))
(define hom-z (λ ([v : Hom]) (flv4-ref v 2)))
(define hom-w (λ ([v : Hom]) (flv4-ref v 3)))

(: pos->hom (-> Pos Hom))
(define (pos->hom v)
  (call/flv3-values v
    (λ (x y z)
      (Hom (flvector x y z 1.0)))))

(: hom->pos (-> Hom (U #f Pos)))
(define (hom->pos v)
  (call/flv4-values v
    (λ (x y z w)
      (pos (/ x w) (/ y w) (/ z w)))))

(: dir->hom (-> Dir Hom))
(define (dir->hom v)
  (call/flv3-values v
    (λ (dx dy dz)
      (Hom (flvector dx dy dz 0.0)))))

(: hom->dir (-> Hom Dir))
(define (hom->dir v)
  (call/flv4-values v
    (λ (x y z w)
      (dir x y z))))
|#
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

(: set-vertex-pos (-> Vertex Pos Vertex))
(define (set-vertex-pos vert v)
  (match-define (Vertex _ n c e m) vert)
  (Vertex v n c e m))

(: set-vertex-normal (-> Vertex (U #f Dir) Vertex))
(define (set-vertex-normal vert n)
  (match-define (Vertex v _ c e m) vert)
  (Vertex v (and n (dir-normalize n)) c e m))

(: set-vertex-color (-> Vertex (U #f RGBA) Vertex))
(define (set-vertex-color vert c)
  (match-define (Vertex v n _ e m) vert)
  (Vertex v n c e m))

(: set-vertex-emitted (-> Vertex (U #f Emitted) Vertex))
(define (set-vertex-emitted vert e)
  (match-define (Vertex v n c _ m) vert)
  (Vertex v n c e m))

(: set-vertex-material (-> Vertex (U #f Material) Vertex))
(define (set-vertex-material vert m)
  (match-define (Vertex v n c e _) vert)
  (Vertex v n c e m))

(: vtx->vertex (-> vtx Vertex))
(define (vtx->vertex v)
  (match-define (vtx p n c e m) v)
  (make-vertex (flv3->pos p) (flv3->dir n) (flv4->rgba c) (flv4->emitted e) (flv4->material m)))

(: face->vertices (All (A B) (-> (face A B) (Listof Vertex))))
(define (face->vertices f)
  (define-values (vtx1 vtx2 vtx3) (face-vtxs f))
  (list (vtx->vertex vtx1) (vtx->vertex vtx2) (vtx->vertex vtx3)))

;; ===================================================================================================
;; Affine transforms

(define print-affine
  (make-constructor-style-printer
   (λ ([t : Affine]) 'affine)
   (λ ([t : Affine])
     (list (affine-x-axis t)
           (affine-y-axis t)
           (affine-z-axis t)
           (affine-origin t)))))

(: affine-equal? (-> Affine Affine (-> Any Any Boolean) Boolean))
(define (affine-equal? t1 t2 _)
  (equal? (FlAffine3-forward t1)
          (FlAffine3-forward t2)))

(: affine-hash (-> Affine (-> Any Integer) Integer))
(define (affine-hash t hash)
  (hash (FlAffine3-forward t)))

(struct Affine FlAffine3 ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-affine
  #:property prop:equal+hash (list affine-equal? affine-hash affine-hash))

(define-type -Affine Affine)
(define affine? Affine?)

(: flaffine3->affine (-> FlAffine3 Affine))
(define (flaffine3->affine t)
  (if (affine? t)
      t
      (Affine (FlAffine3-forward t)
              (FlAffine3-inverse t)
              (FlAffine3-determinant t)
              (FlAffine3-1/determinant t)
              (FlAffine3-forward-data-vec t)
              (FlAffine3-inverse-data-vec t))))

(define identity-affine (flaffine3->affine identity-flaffine3))

(: affine-compose2 (-> Affine Affine Affine))
(define (affine-compose2 t1 t2)
  (flaffine3->affine (flt3compose t1 t2)))

(: affine-compose (-> Affine * Affine))
(define (affine-compose . ts)
  (if (empty? ts)
      identity-affine
      (let ([t1  (first ts)]
            [ts  (rest ts)])
        (if (empty? ts)
            t1
            (let loop ([t1 t1] [t2  (first ts)] [ts  (rest ts)])
              (if (empty? ts)
                  (affine-compose2 t1 t2)
                  (loop (affine-compose2 t1 t2) (first ts) (rest ts))))))))

(: affine-inverse (-> Affine Affine))
(define (affine-inverse t)
  (flaffine3->affine (flt3inverse t)))

(: affine-consistent? (-> Affine Boolean))
(define (affine-consistent? t)
  (flt3consistent? t))

(: affine (-> Dir Dir Dir Pos (U #f Affine)))
(define (affine x y z p)
  (define t (cols->flaffine3 x y z p))
  (and t (flaffine3->affine t)))

(: affine->cols* (-> Affine (Values Dir Dir Dir Pos)))
(define (affine->cols* t)
  (call/flaffine3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (values (dir m00 m10 m20)
              (dir m01 m11 m21)
              (dir m02 m12 m22)
              (pos m03 m13 m23)))))

(: warning-hash (HashTable Symbol Void))
(define warning-hash (make-hasheq))

(: deprecation-warning! (-> Symbol String Void))
(define (deprecation-warning! old new)
  (hash-ref! warning-hash old
             (λ () (eprintf "~a is deprecated and will be removed soon
  please use ~a instead\n"
                            old new))))

(: cols->affine (-> Dir Dir Dir Pos (U #f Affine)))
(define (cols->affine dx dy dz p)
  (deprecation-warning! 'cols->affine "affine")
  (affine dx dy dz p))

(: affine->cols (-> Affine (Values Dir Dir Dir Pos)))
(define (affine->cols t)
  (deprecation-warning! 'affine->cols "(match (affine ...) ...), affine-x-axis, \
affine-y-axis, affine-z-axis or affine-origin")
  (affine->cols* t))

(: affine-x-axis (-> Affine Dir))
(define (affine-x-axis t)
  (call/flaffine3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (dir m00 m10 m20))))

(: affine-y-axis (-> Affine Dir))
(define (affine-y-axis t)
  (call/flaffine3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (dir m01 m11 m21))))

(: affine-z-axis (-> Affine Dir))
(define (affine-z-axis t)
  (call/flaffine3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (dir m02 m12 m22))))

(: affine-origin (-> Affine Pos))
(define (affine-origin t)
  (call/flaffine3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (pos m03 m13 m23))))

(: transform-pos (-> Pos Affine Pos))
(define (transform-pos v t)
  (flv3->pos (flt3apply/pos t v)))

(: transform-dir (-> Dir Affine Dir))
(define (transform-dir v t)
  (flv3->dir (flt3apply/dir t v)))

(: transform-norm (-> Dir Affine (U #f Dir)))
(define (transform-norm v t)
  (let ([n  (flt3apply/norm t v)])
    (and n (flv3->dir n))))

;; ===================================================================================================
;; Smooth functions

(: print-differentiable (-> Differentiable Output-Port (U #t #f 0 1) Void))
(define (print-differentiable t out mode)
  (write-string "#<differentiable>" out)
  (void))

(struct Differentiable FlDiff3 ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-differentiable)

(define-type -Differentiable Differentiable)
(define differentiable? Differentiable?)

(: fldiff3->differentiable (-> FlDiff3 Differentiable))
(define (fldiff3->differentiable t)
  (if (differentiable? t)
      t
      (match-let ([(FlDiff3 f j j-given?)  t])
        (Differentiable f j j-given?))))

(: pos-pos->values-values (-> (-> Pos Pos) FlFunction3))
(define ((pos-pos->values-values f) x y z)
  (call/flv3-values (f (pos x y z)) values))

(: values-values->pos-pos (-> FlFunction3 (-> Pos Pos)))
(define ((values-values->pos-pos f) v)
  (define-values (x y z) (call/flv3-values v f))
  (pos x y z))

(: pos-cols->values-values (-> (-> Pos (Values Dir Dir Dir)) FlJacobian3))
(define ((pos-cols->values-values f) x y z)
  (define-values (dx dy dz) (f (pos x y z)))
  (call/flv3-values dx
    (λ (m00 m10 m20)
      (call/flv3-values dy
        (λ (m01 m11 m21)
          (call/flv3-values dz
            (λ (m02 m12 m22)
              (values m00 m01 m02
                      m10 m11 m12
                      m20 m21 m22))))))))

(: values-values->pos-cols (-> FlJacobian3 (-> Pos (Values Dir Dir Dir))))
(define ((values-values->pos-cols f) v)
  (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22) (call/flv3-values v f))
  (values (dir m00 m10 m20)
          (dir m01 m11 m21)
          (dir m02 m12 m22)))

(: differentiable (->* [(-> Pos Pos)]
                       [(U #f (-> Pos (Values Dir Dir Dir)))]
                       Differentiable))
(define (differentiable f [j #f])
  (if j
      (Differentiable (pos-pos->values-values f)
                      (pos-cols->values-values j)
                      #t)
      (let ([f  (pos-pos->values-values f)])
        (Differentiable f (make-jacobian f) #f))))

(define-type Smooth (U Affine Differentiable))

(: smooth? (-> Any Boolean : Smooth))
(define (smooth? t) (or (affine? t) (differentiable? t)))

(: smooth-function (-> Smooth (-> Pos Pos)))
(define (smooth-function t)
  (if (differentiable? t)
      (values-values->pos-pos (FlDiff3-function t))
      (λ (v) (transform-pos v t))))

(: smooth-jacobian (-> Smooth (-> Pos (Values Dir Dir Dir))))
(define (smooth-jacobian t)
  (if (differentiable? t)
      (values-values->pos-cols (FlDiff3-jacobian t))
      (let-values ([(dx dy dz _)  (affine->cols t)])
        (λ (_) (values dx dy dz)))))

(: flsmooth3->smooth (-> FlSmooth3 Smooth))
(define (flsmooth3->smooth t)
  (if (flaffine3? t)
      (flaffine3->affine t)
      (fldiff3->differentiable t)))

(: smooth-compose2 (-> Smooth Smooth Smooth))
(define (smooth-compose2 t1 t2)
  (flsmooth3->smooth (fls3compose t1 t2)))

(: smooth-compose (-> Smooth * Smooth))
(define (smooth-compose . ts)
  (if (empty? ts)
      identity-affine
      (let ([t1  (first ts)]
            [ts  (rest ts)])
        (if (empty? ts)
            t1
            (let loop ([t1 t1] [t2  (first ts)] [ts  (rest ts)])
              (if (empty? ts)
                  (smooth-compose2 t1 t2)
                  (loop (smooth-compose2 t1 t2) (first ts) (rest ts))))))))

(: smooth-consistent? (-> Smooth Pos Boolean))
(define (smooth-consistent? t v)
  (fls3consistent? t v))

(: smooth-approximate (-> Smooth Pos (U #f Affine)))
(define (smooth-approximate t v)
  (let ([t  (fls3approximate t v)])
    (and t (flaffine3->affine t))))

(: smooth-between (-> Smooth Smooth (U Real (-> Pos Real)) Smooth))
(define (smooth-between t0 t1 fα)
  (let ([t0  (if (flaffine3? t0) (flaffine3->fldiff3 t0) t0)]
        [t1  (if (flaffine3? t1) (flaffine3->fldiff3 t1) t1)]
        [fα  (cond [(real? fα)  (λ ([_ : Pos]) fα)]
                   [else  fα])])
    
    (match-define (FlDiff3 f0 j0 given0?) t0)
    (match-define (FlDiff3 f1 j1 given1?) t1)
    
    (: f FlFunction3)
    (define (f x y z)
      (define-values (x0 y0 z0) (f0 x y z))
      (define-values (x1 y1 z1) (f1 x y z))
      (define α (fl (fα (pos x y z))))
      (values (flblend x0 x1 α)
              (flblend y0 y1 α)
              (flblend z0 z1 α)))
    
    (: j FlJacobian3)
    (define (j x y z)
      (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22) (j0 x y z))
      (define-values (n00 n01 n02 n10 n11 n12 n20 n21 n22) (j1 x y z))
      (define α (fl (fα (pos x y z))))
       (values (flblend m00 n00 α) (flblend m01 n01 α) (flblend m02 n02 α)
               (flblend m10 n10 α) (flblend m11 n11 α) (flblend m12 n12 α)
               (flblend m20 n20 α) (flblend m21 n21 α) (flblend m22 n22 α)))
    
    (Differentiable f j (or given0? given1?))))

(: deform-pos (-> Pos Smooth Pos))
(define (deform-pos v t)
  (if (affine? t)
      (transform-pos v t)
      (flv3->pos (fls3apply/pos t v))))

(: deform-dir (-> Pos Dir Smooth Dir))
(define (deform-dir v dv t)
  (if (affine? t)
      (transform-dir dv t)
      (flv3->dir (fls3apply/dir t v dv))))

(: deform-norm (-> Pos Dir Smooth (U #f Dir)))
(define (deform-norm v dv t)
  (if (affine? t)
      (transform-norm dv t)
      (let ([v  (fls3apply/norm t v dv)])
        (and v (flv3->dir v)))))

(: deform-affine (-> Affine Smooth (U #f Affine)))
(define (deform-affine t1 t2)
  (define t (fls3apply/affine t2 t1))
  (and t (flaffine3->affine t))
  #;; Equivalent to this when t2 is Affine:
  (affine-compose t2 t1)
  #;; Equivalent to this when t2 is Differentiable:
  (let-values ([(dx dy dz v)  (affine->cols* t1)])
    (affine (deform-dir v dx t2)
            (deform-dir v dy t2)
            (deform-dir v dz t2)
            (deform-pos v t2))))

;; ===================================================================================================
;; Surface data

(: print-surface-data (-> Surface-Data Output-Port (U #t #f 0 1) Void))
(define (print-surface-data surf port mode)
  (match-define (Surface-Data t v n path) surf)
  (write-string (format "(surface-data ~v ~v" t v) port)
  (when n (write-string (format " #:normal ~v" n) port))
  (when (not (empty? path)) (write-string (format " #:path ~v" path) port))
  (write-string ")" port)
  (void))

(struct Surface-Data ([dist : Nonnegative-Flonum]
                      [pos : Pos]
                      [normal : (U #f Dir)]
                      [path : (Listof Tag)])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-surface-data)

(define-type -Surface-Data Surface-Data)
(define make-surface-data Surface-Data)
(define surface-data? Surface-Data?)
(define surface-data-dist Surface-Data-dist)
(define surface-data-pos Surface-Data-pos)
(define surface-data-normal Surface-Data-normal)
(define surface-data-path Surface-Data-path)

(: trace-data->surface-data (-> Nonnegative-Flonum trace-data Surface-Data))
(define (trace-data->surface-data time data)
  (match-define (trace-data v n path) data)
  (make-surface-data time (flv3->pos v) (and n (flv3->dir n)) path))

(: surface-data (->* [Real Pos] [#:normal (U #f Dir) #:path (Listof Tag)] Surface-Data))
(define (surface-data time v #:normal [n #f] #:path [path empty])
  (let ([n  (and n (dir-normalize n))])
    (make-surface-data (max 0.0 (fl time)) v n path)))
