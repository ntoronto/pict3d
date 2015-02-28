#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/match
         racket/vector
         racket/flonum
         racket/unsafe/ops
         racket/promise)

(provide
 ;; Types
 Marshalled-Value
 Marshalled-Type
 (rename-out [-Marshaller Marshaller])
 ;; Do the thing
 marshal marshal*
 unmarshal unmarshal*
 ;; Primitives
 null/m
 boolean/m
 byte/m
 fixnum/m
 nonnegative-fixnum/m
 index/m
 natural/m
 integer/m
 real/m
 flonum/m
 number/m
 symbol/m
 string/m
 char/m
 bytes/m
 flvector/m
 ;; Combinators
 primitive/m
 union/m
 pair/m
 struct/m
 singleton/m
 opaque/m
 list/m
 vector/m
 listof/m
 vectorof/m
 delay/m
 )

;; These need to be usable by `serialize` and be able to be sent through place channels
(define-type Marshalled-Value
  (U Null
     Boolean
     Number
     Symbol
     String
     Char
     Bytes
     FlVector
     (Vectorof Marshalled-Value)
     (Pair Symbol (Listof Marshalled-Value))))

(define-type Marshalled-Type
  (U Symbol (Listof Marshalled-Type)))

;; Do not provide this!
(struct bad ())

(: fail (-> Marshalled-Type Marshalled-Value Nothing))
(define (fail type v)
  (error 'unmarshal "cannot unmarshal ~e as an instance of type ~a" v type))

(define-type (Marshal-Proc T) (-> T Marshalled-Value))
(define-type (Unmarshal-Proc T) (All (N) (-> Marshalled-Value (-> N) (U N T))))

(struct (T) Marshaller ([type : Marshalled-Type]
                        [marshal : (Marshal-Proc T)]
                        [unmarshal : (Unmarshal-Proc T)]))

(define-type -Marshaller Marshaller)

(: marshal (All (T) (-> (Marshaller T) T Marshalled-Value)))
(define (marshal m t)
  (((inst Marshaller-marshal T) m) t))

(: unmarshal (All (T) (-> (Marshaller T) Marshalled-Value T)))
(define (unmarshal m v)
  (((inst Marshaller-unmarshal T) m)
   v
   (λ () (fail ((inst Marshaller-type T) m) v))))

(define-syntax-rule (marshal* m t)
  ((unsafe-struct-ref (assert m Marshaller?) 1) t))

(define-syntax-rule (unmarshal* m v)
  ((unsafe-struct-ref (assert m Marshaller?) 2)
   v
   (λ () (fail (unsafe-struct-ref m 0) v))))

(define-syntax-rule (primitive/m [T pred?])
  ((inst Marshaller T)
   'T
   (λ (t) t)
   (λ (v next) (if (pred? v) v (next)))))

(define-syntax (define-primitive-marshallers stx)
  (syntax-case stx ()
    [(_ [name T pred?] ...)
     (with-syntax ([(name/m ...)  (map (λ (n) (format-id stx "~a/m" n))
                                       (syntax->list #'(name ...)))])
       (syntax/loc stx
         (begin
           (define name/m (primitive/m [T pred?]))
           ...)))]))

(define nonnegative-fixnum?
  (λ (v) (and (fixnum? v) (exact-nonnegative-integer? v))))

(define-primitive-marshallers
  [null Null null?]
  [boolean Boolean boolean?]
  [byte Byte byte?]
  [fixnum Fixnum fixnum?]
  [nonnegative-fixnum Nonnegative-Fixnum nonnegative-fixnum?]
  [index Index index?]
  [natural Natural exact-nonnegative-integer?]
  [integer Integer exact-integer?]
  [real Real real?]
  [flonum Flonum flonum?]
  [number Number number?]
  [symbol Symbol symbol?]
  [string String string?]
  [char Char char?]
  [bytes Bytes bytes?]
  [flvector FlVector flvector?])

(define-syntax (union-chain stx)
  (syntax-case stx ()
    [(_ v next)  (syntax/loc stx (next))]
    [(_ g0 g ... v next)
     (syntax/loc stx (g0 v (λ () (union-chain g ... v next))))]))

(define-syntax (union/m stx)
  (syntax-case stx ()
    [(_ ([m T pred?] ...))
     (with-syntax ([(type ...)  (generate-temporaries #'(T ...))]
                   [(f ...)     (generate-temporaries #'(m ...))]
                   [(g ...)     (generate-temporaries #'(m ...))])
       (quasisyntax/loc stx
         (match-let ([(Marshaller type f g)  (ann m (Marshaller T))] ...)
           ((inst Marshaller (U T ...))
            `(U ,type ...)
            (λ (t) (cond [(pred? t)  (f t)] ... [else  (typecheck-fail #,stx)]))
            (λ (v next) (union-chain g ... v next))))))]))

(define-syntax (pair/m stx)
  (syntax-case stx ()
    [(_ [m1 T1 pred1?] [m2 T2 pred2?])
     (syntax/loc stx
       (match-let ([(Marshaller type1 f1 g1)  (ann m1 (Marshaller T1))]
                   [(Marshaller type2 f2 g2)  (ann m2 (Marshaller T2))])
         ((inst Marshaller (Pair T1 T2))
          `(Pair ,type1 ,type2)
          (λ (t) (list 'cons (f1 (car t)) (f2 (cdr t))))
          (λ (v next)
            (match v
              [(list 'cons v1 v2)
               (let ([t1  (g1 v1 bad)])
                 (if (or (bad? t1) (not (pred1? t1)))
                     (next)
                     (let ([t2  (g2 v2 bad)])
                       (if (or (bad? t2) (not (pred2? t2)))
                           (next)
                           (cons t1 t2)))))]
              [_  (next)])))))]))

(define-syntax (struct/m stx)
  (syntax-case stx ()
    [(_ name ([m T pred?] ...))
     (syntax/loc stx (struct/m name name ([m T pred?] ...)))]
    [(_ name Type ([m T pred?] ...))
     (with-syntax ([(type ...)  (generate-temporaries #'(T ...))]
                   [(f ...)     (generate-temporaries #'(m ...))]
                   [(g ...)     (generate-temporaries #'(m ...))]
                   [(field ...)  (generate-temporaries #'(m ...))])
       (syntax/loc stx
         (match-let ([(Marshaller type f g)  (ann m (Marshaller T))] ...)
           ((inst Marshaller Type)
            'Type
            (λ (t)
              (match-define (name field ...) t)
              (list 'name (f field) ...))
            (λ (v next)
              (match v
                [(list 'name field ...)
                 (let ([field  (g field bad)] ...)
                   (if (or (bad? field) ...)
                       (next)
                       (if (or (not (pred? field)) ...)
                           (next)
                           (name field ...))))]
                [_  (next)]))))))]))

(define-syntax (singleton/m stx)
  (syntax-case stx ()
    [(_ Type value)
     (syntax/loc stx
       ((inst Marshaller Type)
        'Type
        (λ (t) (list 'Type))
        (λ (v next)
          (match v
            [(list 'Type)  value]
            [_  (next)]))))]))

(define-syntax (opaque/m stx)
  (syntax-case stx ()
    [(_ Type make ([m T proj] ...))
     (with-syntax ([(type ...)  (generate-temporaries #'(T ...))]
                   [(f ...)     (generate-temporaries #'(m ...))]
                   [(g ...)     (generate-temporaries #'(m ...))]
                   [(field ...)  (generate-temporaries #'(m ...))])
       (syntax/loc stx
         (match-let ([(Marshaller type f g)  (ann m (Marshaller T))] ...)
           ((inst Marshaller Type)
            'Type
            (λ (t)
              (let ([field  ((ann proj (-> Type T)) t)] ...)
                (list 'Type (f field) ...)))
            (λ (v next)
              (match v
                [(list 'Type field ...)
                 (let ([field  ((inst g bad) field bad)] ...)
                   (if (or (bad? field) ...)
                       (next)
                       ((ann make (-> T ... Type)) field ...)))]
                [_  (next)]))))))]))

(define-syntax (list/m stx)
  (syntax-case stx ()
    [(_ ([m T pred?] ...))
     (syntax/loc stx (struct/m list (List T ...) ([m T pred?] ...)))]))

(define-syntax (vector/m stx)
  (syntax-case stx ()
    [(_ ([m T pred?] ...))
     (with-syntax ([(type ...)  (generate-temporaries #'(T ...))]
                   [(f ...)     (generate-temporaries #'(m ...))]
                   [(g ...)     (generate-temporaries #'(m ...))]
                   [(field ...)  (generate-temporaries #'(m ...))])
       (syntax/loc stx
         (match-let ([(Marshaller type f g)  (ann m (Marshaller T))] ...)
           ((inst Marshaller (Vector T ...))
            '(Vector T ...)
            (λ (t)
              (match-define (vector field ...) t)
              (vector (f field) ...))
            (λ (v next)
              (match v
                [(vector field ...)
                 (let ([field  (g field bad)] ...)
                   (if (or (bad? field) ...)
                       (next)
                       (if (or (not (pred? field)) ...)
                           (next)
                           (vector field ...))))]
                [_  (next)]))))))]))

(define-syntax (listof/m stx)
  (syntax-case stx ()
    [(_ [m T pred?])
     (syntax/loc stx
       (match-let ([(Marshaller type f g)  m])
         ((inst Marshaller (Listof T))
          `(Listof ,type)
          (λ ([ts : (Listof T)]) (cons 'list (map f ts)))
          (λ (vs next)
            (match vs
              [(cons 'list (? list? vs))
               (let loop ([vs vs] [res : (Listof T)  null])
                 (cond [(null? vs)  (reverse res)]
                       [else  (let ([v  (g (car vs) bad)])
                                (if (bad? v)
                                    (next)
                                    (loop (cdr vs) (cons v res))))]))]
              [_  (next)])))))]))

(define-syntax (vectorof/m stx)
  (syntax-case stx ()
    [(_ [m T pred?])
     (syntax/loc stx
       (match-let ([(Marshaller type f g)  m])
         ((inst Marshaller (Vectorof T))
          `(Vectorof ,type)
          (λ ([ts : (Vectorof T)]) (vector-map f ts))
          (λ (vs next)
            (cond [(vector? vs)
                   (define n (vector-length vs))
                   (cond
                     [(= n 0)  (vector)]
                     [else
                      (define new-v (g (vector-ref vs 0) bad))
                      (cond
                        [(bad? new-v)  (next)]
                        [else
                         (define new-vs ((inst make-vector T) n new-v))
                         (let loop ([i : Nonnegative-Fixnum  1])
                           (when (< i n)
                             (define new-v (g (vector-ref vs i) bad))
                             (cond [(bad? new-v)  (next)]
                                   [else  (vector-set! new-vs i new-v)
                                          (loop (+ i 1))])))
                         new-vs])])]
                  [else  (next)])))))]))

(: lazy/m (All (T) (-> (Promise (Marshaller T)) Marshalled-Type (Marshaller T))))
(define (lazy/m m type)
  ((inst Marshaller T)
   type
   (λ (t) (((inst Marshaller-marshal T) (force m)) t))
   (λ (v next)
     (((inst Marshaller-unmarshal T) (force m)) v next))))

(define-syntax-rule (delay/m [m T])
  ((inst lazy/m T) (delay (ann m (Marshaller T))) 'T))

#|
(define pair-boolean-flonum/m
  (pair/m [boolean/m Boolean (λ (_) #t)]
          [flonum/m Flonum values]))

(define union-boolean-flonum/m
  (union/m ([boolean/m Boolean boolean?]
            [flonum/m Flonum values])))

(unmarshal* pair-boolean-flonum/m
            (marshal* pair-boolean-flonum/m '(#t . 3.2)))

(unmarshal* pair-boolean-flonum/m
            (marshal* pair-boolean-flonum/m '(#f . 3.2)))

(unmarshal* union-boolean-flonum/m
            (marshal* union-boolean-flonum/m #f))

(unmarshal* union-boolean-flonum/m
            (marshal* union-boolean-flonum/m #t))
 
(unmarshal* union-boolean-flonum/m
            (marshal* union-boolean-flonum/m 3.2))

(define list-test/m
  (list/m ([boolean/m Boolean values]
           [flonum/m Flonum values]
           [integer/m Integer values])))

(define vector-test/m
  (vector/m ([boolean/m Boolean values]
             [flonum/m Flonum values]
             [integer/m Integer values])))

(define listof-test/m
  (listof/m [flonum/m Flonum values]))

(define vectorof-test/m
  (vectorof/m [flonum/m Flonum values]))

(define test/m
  (union/m ([listof-test/m    (Listof Flonum)    list?]
            [vectorof-test/m  (Vectorof Flonum)  vector?])))
|#
