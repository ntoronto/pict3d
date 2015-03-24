#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/match
         racket/vector
         racket/list
         typed/racket/class
         typed/racket/draw
         math/flonum
         math/base
         "../math.rkt"
         "../engine.rkt"
         "../utils.rkt")

(provide
 Tag
 tag?
 ;; Materials
 (rename-out [-Material Material])
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
 (rename-out [-Affine Affine])
 flaffine3->affine
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

(: make-material (-> Flonum Flonum Flonum Flonum Material))
(define (make-material a d s r)
  (let ([a  (min 1.0 (max 0.0 a))]
        [d  (min 1.0 (max 0.0 d))]
        [s  (min 1.0 (max 0.0 s))]
        [r  (min 1.0 (max 0.0 r))])
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
  (RGBA (FlV4-flvector c)))

(: flv4->emitted (-> FlV4 Emitted))
(define (flv4->emitted e)
  (Emitted (FlV4-flvector e)))

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

(define print-vector
  (λ ([name : Symbol])
    (make-constructor-style-printer
     (λ ([v : FlV3]) name)
     (λ ([v : FlV3]) (call/flv3-values v list)))))

(struct Pos FlV3 ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (print-vector 'pos))

(struct Dir FlV3 ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (print-vector 'dir))

(define-type -Pos Pos)
(define-type -Dir Dir)
(define pos? Pos?)
(define dir? Dir?)

(: flv3->pos (-> FlV3 Pos))
(define (flv3->pos v)
  (Pos (FlV3-flvector v)))

(: flv3->dir (-> FlV3 Dir))
(define (flv3->dir v)
  (Dir (FlV3-flvector v)))

(define origin (flv3->pos zero-flv3))

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
      (provide name ...)))

(define/provide-unit-vectors)

(: ->flv3 (-> Symbol (U FlVector (Listof Real) (Vectorof Real)) FlV3))
(define (->flv3 name v)
  (cond [(flvector? v)
         (if (= 3 (flvector-length v))
             (flv3 (flvector-ref v 0)
                   (flvector-ref v 1)
                   (flvector-ref v 2))
             (raise-argument-error name "length-3 flvector, list or vector" v))]
        [else
         (match v
           [(vector x y z)
            (flv3 (fl x) (fl y) (fl z))]
           [(list x y z)
            (flv3 (fl x) (fl y) (fl z))]
           [_
            (raise-argument-error name "length-3 flvector, list or vector" v)])]))

(: pos (case-> (-> (U FlVector (Listof Real) (Vectorof Real)) Pos)
               (-> Real Real Real Pos)))
(define pos
  (case-lambda
    [(v)  (flv3->pos (->flv3 'pos v))]
    [(x y z)  (flv3->pos (flv3 (fl x) (fl y) (fl z)))]))

(: dir (case-> (-> (U FlVector (Listof Real) (Vectorof Real)) Dir)
               (-> Real Real Real Dir)))
(define dir
  (case-lambda
    [(dv)  (flv3->dir (->flv3 'dir dv))]
    [(dx dy dz)  (flv3->dir (flv3 (fl dx) (fl dy) (fl dz)))]))

(define pos-x (λ ([v : Pos]) (flv3-ref v 0)))
(define pos-y (λ ([v : Pos]) (flv3-ref v 1)))
(define pos-z (λ ([v : Pos]) (flv3-ref v 2)))

(define dir-dx (λ ([dv : Dir]) (flv3-ref dv 0)))
(define dir-dy (λ ([dv : Dir]) (flv3-ref dv 1)))
(define dir-dz (λ ([dv : Dir]) (flv3-ref dv 2)))

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

(: dir-normalize (-> Dir (U #f Dir)))
(define (dir-normalize dv)
  (define v (flv3normalize dv))
  (and v (flv3->dir v)))

(: dir-dot (-> Dir Dir Flonum))
(define (dir-dot dv1 dv2)
  (flv3dot dv1 dv2))

(: dir-proj (-> Dir Dir (U #f Dir)))
(define (dir-proj A B)
  (define d (dir-dist^2 B))
  (cond [(= d 0.0)  #f]
        [else  (dir-scale B (/ (dir-dot A B) d))]))

(: dir-cross (-> Dir Dir Dir))
(define (dir-cross dv1 dv2)
  (flv3->dir (flv3cross dv1 dv2)))

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
     (call/flaffine3-forward t
       (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
         (list (Dir (flvector m00 m10 m20))
               (Dir (flvector m01 m11 m21))
               (Dir (flvector m02 m12 m22))
               (Pos (flvector m03 m13 m23))))))))

(: affine-equal? (-> Affine Affine (-> Any Any Boolean) Boolean))
(define (affine-equal? t1 t2 _)
  (equal? (call/flaffine3-forward t1 flvector)
          (call/flaffine3-forward t2 flvector)))

(: affine-hash (-> Affine (-> Any Integer) Integer))
(define (affine-hash t hash)
  (hash (call/flaffine3-forward t flvector)))

(struct Affine FlAffine3 ()
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-affine
  #:property prop:equal+hash (list affine-equal? affine-hash affine-hash))

(define-type -Affine Affine)
(define affine? Affine?)

(: flaffine3->affine (-> FlAffine3 Affine))
(define (flaffine3->affine t)
  (Affine (FlAffine3-forward t)
          (FlAffine3-inverse t)
          (FlAffine3-determinant t)
          (FlAffine3-1/determinant t)
          (FlAffine3-forward-data-ptr t)
          (FlAffine3-inverse-data-ptr t)))

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

(: cols->affine (-> Dir Dir Dir Pos Affine))
(define (cols->affine x y z p)
  (flaffine3->affine (cols->flaffine3 x y z p)))

(: affine->cols (-> Affine (Values Dir Dir Dir Pos)))
(define (affine->cols t)
  (call/flaffine3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (values (dir m00 m10 m20)
              (dir m01 m11 m21)
              (dir m02 m12 m22)
              (pos m03 m13 m23)))))

(: transform-pos (-> Pos Affine Pos))
(define (transform-pos v t)
  (flv3->pos (flt3apply/pos t v)))

(: transform-dir (-> Dir Affine Dir))
(define (transform-dir v t)
  (flv3->dir (flt3apply/dir t v)))

(: transform-norm (-> Dir Affine (U #f Dir)))
(define (transform-norm v t)
  (define n (flt3apply/norm t v))
  (and n (flv3->dir n)))
