#lang typed/racket/base

(require (for-syntax racket/base)
         racket/list
         racket/unsafe/ops
         math/flonum
         math/base
         (except-in typed/opengl/ffi cast ->)
         "../gl.rkt"
         "../utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Utils for packing and unpacking vertex data

(: byte->flonum (-> Byte Flonum))
(define (byte->flonum b)
  (/ (fl b) 255.0))

(: flonum->byte (-> Flonum Natural))
(define (flonum->byte x)
  (if (< -inf.0 x +inf.0)
      (min 255 (max 0 (exact-floor (* x 256.0))))
      0))
#|
;; ===================================================================================================
;; Shader analogues

(: flfract (-> Flonum Flonum))
(define (flfract x) (- x (floor x)))

(: flclamp (-> Flonum Flonum Flonum Flonum))
(define (flclamp x mn mx)
  (max mn (min mx x)))

(: flmix (-> Flonum Flonum Flonum Flonum))
(define (flmix x y α)
  (+ (* x (- 1.0 α)) (* y α)))

(: rgb->hsv (-> FlVector FlVector))
;; Translated from the branchless GLSL code in "shader-code.rkt"
(define (rgb->hsv c)
  (define-values (c.r c.g c.b) (flv3-values c))
  (define-values (p.x p.y p.z p.w)
    (if (< c.g c.b)
        (values c.b c.g -1.0 #i2/3)
        (values c.g c.b 0.0 #i-1/3)))
  (define-values (q.x q.y q.z q.w)
    (if (< c.r p.x)
        (values p.x p.y p.w c.r)
        (values c.r p.y p.z p.x)))
  (define d (- q.x (min q.w q.y)))
  (define e epsilon.0)
  (flvector (abs (+ q.z (/ (- q.w q.y) (+ (* 6.0 d) e))))
            (/ d (+ q.x e))
            q.x))

(: hsv->rgb (-> FlVector FlVector))
;; Translated from the branchless GLSL code in "shader-code.rkt"
(define (hsv->rgb c)
  (define-values (c.x c.y c.z) (flv3-values c))
  (define-values (p.x p.y p.z)
    (values (abs (- (* (flfract (+ c.x   1.0)) 6.0) 3.0))
            (abs (- (* (flfract (+ c.x #i2/3)) 6.0) 3.0))
            (abs (- (* (flfract (+ c.x #i1/3)) 6.0) 3.0))))
  (flvector (* c.z (flmix 1.0 (flclamp (- p.x 1.0) 0.0 1.0) c.y))
            (* c.z (flmix 1.0 (flclamp (- p.y 1.0) 0.0 1.0) c.y))
            (* c.z (flmix 1.0 (flclamp (- p.z 1.0) 0.0 1.0) c.y))))
|#
;; ===================================================================================================
;; Repeated memcpy (could also be called memset* I guess...)

(: memcpy* (-> CPointer Nonnegative-Fixnum CPointer Nonnegative-Fixnum Nonnegative-Fixnum Void))
(define (memcpy* dst-ptr dst-offset src-ptr src-size count)
  (cond [(unsafe-fx< count 4)
         (for ([j  (in-range count)])
           (memcpy dst-ptr (unsafe-fx+ dst-offset (unsafe-fx* src-size j)) src-ptr src-size _byte))]
        [else
         (define count/2 (unsafe-fxrshift (unsafe-fx+ count 1) 1))
         (memcpy* dst-ptr dst-offset src-ptr src-size count/2)
         (memcpy dst-ptr (unsafe-fx+ dst-offset (unsafe-fx* count/2 src-size))
                 dst-ptr dst-offset (unsafe-fx* (unsafe-fx- count count/2) src-size) _byte)]))

;; ===================================================================================================
;; Single-value memoization

(: cache-singleton (All (B A ...) (-> (-> A ... B)
                                      (-> A ... B))))
(define (cache-singleton f)
  (: cache (U #f (Pair (List A ...) B)))
  (define cache #f)
  
  (λ args
    (: apply-f (-> (List A ...) B))
    (define (apply-f args)
      (define value (apply f args))
      (set! cache (cons args value))
      value)
    
    (define entry cache)
    (cond [(not entry)  (apply-f args)]
          [else
           (define res (equal? args (car entry)))
           (cond [(eq? res #t)  (cdr entry)]
                 [else  (apply-f (if res res args))])])))

(: cache-thunk (All (B) (-> (-> B) (-> B))))
(define (cache-thunk f)
  (: cache (U #f B))
  (define cache #f)
  (λ ()
    (define entry cache)
    (cond [entry  entry]
          [else
           (define value (f))
           (set! cache value)
           value])))

(define-syntax (define-singleton stx)
  (syntax-case stx (:)
    [(_ (name) body ...)
     (syntax/loc stx
       (define name
         (cache-thunk (λ () body ...))))]
    [(_ (name arg ...) body ...)
     (syntax/loc stx
       (define name
         (cache-singleton (λ (arg ...) body ...))))]))

;; ===================================================================================================
;; OpenGL-context-sensitive, single-value memoization

(: cache-singleton/context (All (B A ...) (-> (-> A ... B)
                                              (-> A ... B))))
(define (cache-singleton/context f)
  (: cache (HashTable GL-Context (U #f (Pair (List A ...) B))))
  (define cache (make-weak-hasheq))
  
  (λ args
    (define ctxt (get-current-managed-gl-context 'cache-singleton/context))
    
    (: apply-f (-> (List A ...) B))
    (define (apply-f args)
      (define value (apply f args))
      (hash-set! cache ctxt (cons args value))
      value)
    
    (define entry (hash-ref! cache ctxt (λ () #f)))
    (cond [(not entry)  (apply-f args)]
          [else
           (define res (equal? args (car entry)))
           (cond [(eq? res #t)  (cdr entry)]
                 [else  (apply-f (if res res args))])])))

(: cache-thunk/context (All (B) (-> (-> B) (-> B))))
(define (cache-thunk/context f)
  (: cache (HashTable GL-Context (U #f B)))
  (define cache (make-weak-hasheq))
  (λ ()
    (define ctxt (get-current-managed-gl-context 'cache-thunk/context))
    (define entry (hash-ref! cache ctxt (λ () #f)))
    (cond [entry  entry]
          [else
           (define value (f))
           (hash-set! cache ctxt value)
           value])))

(define-syntax (define-singleton/context stx)
  (syntax-case stx (:)
    [(_ (name) body ...)
     (syntax/loc stx
       (define name
         (cache-thunk/context (λ () body ...))))]
    [(_ (name arg ...) body ...)
     (syntax/loc stx
       (define name
         (cache-singleton/context (λ (arg ...) body ...))))]))

;; ===================================================================================================
;; Fast grouping for state sorting

(struct span ([start : Nonnegative-Fixnum]
              [end : Nonnegative-Fixnum]
              [current : Nonnegative-Fixnum])
  #:transparent
  #:mutable)

(define get-span-vector
  (make-gl-cached-vector
   'get-span-vector
   (λ ([n : Integer])
     (log-pict3d-info "<engine> creating span vector of length ~v" n)
     ((inst make-vector span) n (span 0 0 0)))
   vector-length))

(define no-key (gensym 'no-key))

(: group-by-key! (All (A K) (-> (Vectorof A)
                                Nonnegative-Fixnum
                                Nonnegative-Fixnum
                                (-> A K)
                                (Listof (Pair K span)))))
(define (group-by-key! xs start end key)
  (: spans (HashTable Any span))
  (define spans (make-hasheq))
  
  (define span-vec (get-span-vector (vector-length xs)))
  
  (: kss (Listof (Pair K span)))
  (define-values (kss _last-key _last-span)
    (for/fold ([kss : (Listof (Pair K span))  empty]
               [last-key : Any  no-key]
               [last-span : span  (span 0 0 0)]
               ) ([i  (in-range start end)])
      (define x (unsafe-vector-ref xs i))
      (define k (key x))
      (define s (if (eq? k last-key)
                    last-span
                    (hash-ref spans k #f)))
      (cond [s  (set-span-end! s (unsafe-fx+ 1 (span-end s)))
                (unsafe-vector-set! span-vec i s)
                (values kss k s)]
            [else  (define s (span 0 1 0))
                   (hash-set! spans k s)
                   (unsafe-vector-set! span-vec i s)
                   (values (cons (cons k s) kss) k s)])))
  
  (cond
    [(empty? kss)  empty]
    [(empty? (rest kss))
     (define s (cdr (first kss)))
     (set-span-start! s start)
     (set-span-end! s end)
     kss]
    [else
     (set! kss (reverse kss))
     (for/fold ([n : Nonnegative-Fixnum  start]) ([ks  (in-list kss)])
       (define s (cdr ks))
       (define len (span-end s))
       (define next-n (unsafe-fx+ n len))
       (set-span-start! s n)
       (set-span-current! s n)
       (set-span-end! s next-n)
       next-n)
     
     (let loop ([i start])
       (when (< i end)
         (define s (unsafe-vector-ref span-vec i))
         (cond [(and (<= (span-start s) i) (< i (span-end s)))
                (loop (unsafe-fx+ i 1))]
               [else
                (define x (unsafe-vector-ref xs i))
                (define n (span-current s))
                (define xtmp (unsafe-vector-ref xs n))
                (unsafe-vector-set! xs n x)
                (unsafe-vector-set! xs i xtmp)
                (define stmp (unsafe-vector-ref span-vec n))
                (unsafe-vector-set! span-vec n s)
                (unsafe-vector-set! span-vec i stmp)
                (set-span-current! s (unsafe-fx+ n 1))
                (loop i)])))
     kss]))
