#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/fixnum
         racket/flonum)

(provide (all-defined-out))

(define-type Flags Nonnegative-Fixnum)

(define-syntax (define-flags stx)
  (syntax-case stx ()
    [(_ sym ...)
     (let ([syms  (syntax->list #'(sym ...))])
       (with-syntax ([Flag-Name  (format-id stx "Flag-Name")]
                     [flag-name-hash  (format-id stx "flag-name-hash")]
                     [name-flag-hash  (format-id stx "name-flag-hash")]
                     [all-flags   (format-id stx "all-flags")]
                     [(name ...)  (map (λ (sym) (format-id sym "~a-flag" sym)) syms)]
                     [(bit ...)   (build-list (length syms) values)]
                     [top-bit  (length syms)])
         (syntax/loc stx
           (begin
             (define-type Flag-Name (U 'sym ...))
             
             (: flag-name-hash (Vectorof (U #f Flag-Name)))
             (define flag-name-hash (make-vector 30 #f))
             
             (: name-flag-hash (HashTable Symbol Flags))
             (define name-flag-hash (make-hasheq))
             
             (define name (assert (arithmetic-shift 1 bit) fixnum?)) ...
             (vector-set! flag-name-hash bit 'sym) ...
             (hash-set! name-flag-hash 'sym name) ...
             
             (: all-flags Flags)
             (define all-flags
               (assert (assert (- (arithmetic-shift 1 top-bit) 1) exact-nonnegative-integer?)
                       fixnum?))
             ))))]))

(define-flags
  visible invisible
  opaque transparent
  emitting nonemitting)

(: empty-flags Flags)
(define empty-flags 0)

(: flags? (-> Any Boolean : #:+ Flags))
(define (flags? v)
  (and (exact-nonnegative-integer? v)
       (<= v all-flags)))

(: names->flags (-> (Listof Flag-Name) Flags))
(define (names->flags as)
  (for/fold ([bits : Flags  0]) ([a  (in-list as)])
    (bitwise-ior bits (hash-ref name-flag-hash a))))

(: flags->names (-> Flags (Listof Flag-Name)))
(define (flags->names bs)
  (reverse
   (for/fold ([as : (Listof Flag-Name)  null]) ([b  (in-range 0 30)])
     (if (bitwise-bit-set? bs b)
         (cons (assert (vector-ref flag-name-hash b) values) as)
         as))))

(: flags-join (case-> (-> Flags)
                      (-> Flags Flags)
                      (-> Flags Flags Flags)
                      (-> Flags * Flags)))
(define flags-join
  (case-lambda
    [()  0]
    [(b1)  b1]
    [(b1 b2)  (bitwise-ior b1 b2)]
    [bs  (for/fold ([res : Flags  0]) ([b  (in-list bs)])
           (bitwise-ior res b))]))

(: flags-meet (case-> (-> Flags)
                      (-> Flags Flags)
                      (-> Flags Flags Flags)
                      (-> Flags * Flags)))
(define flags-meet
  (case-lambda
    [()  all-flags]
    [(b1)  b1]
    [(b1 b2)  (bitwise-and b1 b2)]
    [bs  (for/fold ([res : Flags  all-flags]) ([b  (in-list bs)])
           (bitwise-and res b))]))

(: flags-subtract (-> Flags Flags Flags))
(define (flags-subtract b1 b2)
  (assert (- (bitwise-ior b1 b2) b2) exact-nonnegative-integer?))

(: flags-subset? (-> Flags Flags Boolean))
(define (flags-subset? b1 b2)
  (= (flags-meet b1 b2) b1))

(define visibility-flags
  (flags-join visible-flag invisible-flag))

(define opacity-flags
  (flags-join opaque-flag transparent-flag))

(define emitting-flags
  (flags-join emitting-flag nonemitting-flag))

(: color-opaque? (-> FlVector Boolean))
(define (color-opaque? cs)
  (>= (flvector-ref cs 3) 1.0))

(: colors-opaque? (-> (U FlVector (Vectorof FlVector)) Boolean))
(define (colors-opaque? cs)
  (if (vector? cs)
      (for/and ([c  (in-vector cs)])
        (color-opaque? c))
      (color-opaque? cs)))

(: color-emitting? (-> FlVector Boolean))
(define (color-emitting? es)
  (and (or (> (flvector-ref es 0) 0.0)
           (> (flvector-ref es 1) 0.0)
           (> (flvector-ref es 2) 0.0))
       (> (flvector-ref es 3) 0.0)))

(: colors-emitting? (-> (U FlVector (Vectorof FlVector)) Boolean))
(define (colors-emitting? es)
  (if (vector? es)
      (for/or ([e  (in-vector es)])
        (color-emitting? e))
      (color-emitting? es)))

(: color-opacity-flag (-> FlVector Flags))
(define (color-opacity-flag cs)
  (if (color-opaque? cs) opaque-flag transparent-flag))

(: color-emitting-flag (-> FlVector Flags))
(define (color-emitting-flag es)
  (if (color-emitting? es) emitting-flag nonemitting-flag))

(: colors-opacity-flag (-> (U FlVector (Vectorof FlVector)) Flags))
(define (colors-opacity-flag cs)
  (if (colors-opaque? cs) opaque-flag transparent-flag))

(: colors-emitting-flag (-> (U FlVector (Vectorof FlVector)) Flags))
(define (colors-emitting-flag es)
  (if (colors-emitting? es) emitting-flag nonemitting-flag))
