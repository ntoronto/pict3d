#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax)
         (only-in racket/unsafe/ops
                  unsafe-flvector-ref)
         racket/match
         math/flonum
         math/base
         "../math.rkt"
         "../utils.rkt")

(provide
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
 )

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

