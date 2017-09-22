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
         "../utils.rkt"
         "../pos/pos-dir.rkt"
         "../pos/linear-affine-smooth.rkt")

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
 arc-start
 arc-end
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
 (all-from-out "../pos/pos-dir.rkt")
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
 ;; Linear transformations, Affine transformations, and Smooth functions
 (all-from-out "../pos/linear-affine-smooth.rkt")
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
   (λ ([a : Arc]) (list (Arc-start a) (Arc-end a)))))

(struct Arc ([start : Flonum] [end : Flonum])
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-arc)

(define-type -Arc Arc)
(define arc? Arc?)
(define arc-start Arc-start)
(define arc-end Arc-end)

(: arc (-> Real Real Arc))
(define (arc a1 a2)
  (let ([a1  (fl a1)]
        [a2  (fl a2)])
    (if (= a1 a2)
        (let ([a1  (- a1 (* 360.0 (floor (/ a1 360.0))))])
          (Arc a1 a1))
        (let*-values ([(a1 a2)  (let ([d  (* 360.0 (floor (/ a1 360.0)))])
                                  (values (- a1 d) (- a2 d)))]
                      [(a2)     (let ([a2  (- a2 (* 360.0 (floor (/ a2 360.0))))])
                                  (if (<= a2 a1) (+ a2 360.0) a2))])
          (Arc a1 a2)))))

(: arc-values (-> Arc (Values Flonum Flonum)))
(define (arc-values a)
  (values (arc-start a) (arc-end a)))

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
