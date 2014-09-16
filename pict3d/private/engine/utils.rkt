#lang typed/racket/base

(require (for-syntax racket/base)
         racket/list
         racket/unsafe/ops
         typed/racket/gui
         typed/racket/class
         math/flonum
         math/base
         (except-in typed/opengl/ffi cast ->)
         "gl.rkt"
         "../math/flv3.rkt"
         "../math/flt3.rkt")

(provide (all-defined-out))

(: next-pow2 (-> Natural Natural))
(define (next-pow2 size)
  (arithmetic-shift 1 (integer-length (- size 1))))

;; ===================================================================================================
;; Utils for packing and unpacking vertex data

(: byte->flonum (-> Byte Flonum))
(define (byte->flonum b)
  (/ (fl b) 255.0))

(: flonum->byte (-> Flonum Byte))
(define (flonum->byte x)
  (assert (max 0 (min 255 (exact-floor (* x 256.0)))) byte?))

(: pack-color (-> FlVector Bytes))
(define (pack-color v)
  (define n (flvector-length v))
  (define bs (make-bytes n))
  (for ([i  (in-range n)])
    (unsafe-bytes-set! bs i (flonum->byte (unsafe-flvector-ref v i))))
  bs)

(: unpack-color (-> Bytes FlVector))
(define (unpack-color bs)
  (define n (bytes-length bs))
  (define v (make-flvector n))
  (for ([i  (in-range n)])
    (unsafe-flvector-set! v i (byte->flonum (unsafe-bytes-ref bs i))))
  v)

(: pack-emitted (-> FlVector (Values Bytes Byte)))
(define (pack-emitted e)
  (define r (flvector-ref e 0))
  (define g (flvector-ref e 1))
  (define b (flvector-ref e 2))
  (define i (flvector-ref e 3))
  (define i.hi (exact-floor i))
  (define i.lo (- i i.hi))
  (values (bytes (flonum->byte r) (flonum->byte g) (flonum->byte b) (max 0 (min 255 i.hi)))
          (flonum->byte i.lo)))

(: normal->rgb-bytes (-> FlVector Bytes))
(define (normal->rgb-bytes v)
  (define-values (x y z) (flv3-values v))
  (: flonum->byte (-> Flonum Byte))
  (define (flonum->byte x)
    (assert (max 0 (min 255 (+ 127 (exact-ceiling (* x 127.0))))) byte?))
  (bytes (flonum->byte x)
         (flonum->byte y)
         (flonum->byte z)))

(: decode-normal (-> FlVector FlVector))
(define (decode-normal v)
  (define zero #i127/255)
  (let ([v  (flv3normalize (flv3- v (flvector zero zero zero)))])
    (if v v (flvector 0.0 0.0 0.0))))

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
;; Translated from the branchless GLSL code in "shader-lib.rkt"
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
;; Translated from the branchless GLSL code in "shader-lib.rkt"
(define (hsv->rgb c)
  (define-values (c.x c.y c.z) (flv3-values c))
  (define-values (p.x p.y p.z)
    (values (abs (- (* (flfract (+ c.x   1.0)) 6.0) 3.0))
            (abs (- (* (flfract (+ c.x #i2/3)) 6.0) 3.0))
            (abs (- (* (flfract (+ c.x #i1/3)) 6.0) 3.0))))
  (flvector (* c.z (flmix 1.0 (flclamp (- p.x 1.0) 0.0 1.0) c.y))
            (* c.z (flmix 1.0 (flclamp (- p.y 1.0) 0.0 1.0) c.y))
            (* c.z (flmix 1.0 (flclamp (- p.z 1.0) 0.0 1.0) c.y))))

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
;; Vector and matrix stuff

(: flvector->f32vector (-> FlVector F32Vector))
(define (flvector->f32vector v)
  (define n (flvector-length v))
  (define f32s (make-f32vector n))
  (for ([i  (in-range n)])
    (f32vector-set! f32s i (unsafe-flvector-ref v i)))
  f32s)

(: rect-triangle-strip (All (A) (-> A A A A A A A A (Listof A))))
(define (rect-triangle-strip v1 v2 v3 v4 v5 v6 v7 v8)
  (list v3 v4 v7 v8 v5 v4 v1 v3 v2 v7 v6 v5 v2 v1))

(define-type GL-Data (U Bytes
                        F32Vector
                        (Pair GL-Data GL-Data)
                        Null))

(: gl-data-size (-> GL-Data Index))
(define (gl-data-size data)
  (assert
   (let loop : Nonnegative-Fixnum ([data : GL-Data  data])
     (cond [(bytes? data)      (bytes-length data)]
           [(f32vector? data)  (unsafe-fx* 4 (f32vector-length data))]
           [(pair? data)       (unsafe-fx+ (loop (car data)) (loop (cdr data)))]
           [else               0]))
   index?))

(: gl-data->bytes (->* [GL-Data] [Index] Bytes))
(define (gl-data->bytes data [size (gl-data-size data)])
  (define bs (make-bytes size))
  (define ptr (u8vector->cpointer bs))
  (define i
    (let loop : Nonnegative-Fixnum ([data : GL-Data  data]
                                    [i : Nonnegative-Fixnum  0])
      (cond [(bytes? data)
             (bytes-copy! bs i data)
             (unsafe-fx+ i (bytes-length data))]
            [(f32vector? data)
             (define n (unsafe-fx* 4 (f32vector-length data)))
             (memcpy ptr i (f32vector->cpointer data) n _byte)
             (unsafe-fx+ i n)]
            [(pair? data)
             (let ([i  (loop (car data) i)])
               (loop (cdr data) i))]
            [else  i])))
  (unless (= i size)
    (error 'gl-data->bytes "count mismatch: should convert ~a bytes; converted ~a" size i))
  bs)

;; ===================================================================================================
;; Single-value memoization

(: cache-singleton (All (B A ...) (-> (-> A ... B) (-> A ... B))))
(define (cache-singleton f)
  (: entry (U #f (Pair (List A ...) B)))
  (define entry #f)
  (λ args
    (define e entry)
    (cond [(and e (equal? args (car e)))  (cdr e)]
          [else
           (define value (apply f args))
           (set! entry (cons args value))
           value])))

(: cache-thunk (All (B) (-> (-> B) (-> B))))
(define (cache-thunk f)
  (: entry (U #f B))
  (define entry #f)
  (λ ()
    (let ([e  entry])
      (if e e (let ([e  (f)])
                (set! entry e)
                e)))))

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
;; Context-sensitive, single-value memoization

(: cache-singleton/context (All (B A ...) (-> (-> (List A ...) (List A ...)
                                                  (U (List A ...) Boolean))
                                              (-> A ... B)
                                              (-> A ... B))))
(define (cache-singleton/context lte f)
  (: cache (HashTable (U #f gl-context) (U #f (Pair (List A ...) B))))
  (define cache (make-weak-hasheq))
  
  (λ args
    (define ctxt (current-gl-context))
    
    (: apply-f (-> (List A ...) B))
    (define (apply-f args)
      (define value (apply f args))
      (hash-set! cache ctxt (cons args value))
      value)
    
    (define entry (hash-ref! cache ctxt (λ () #f)))
    (cond [(not entry)  (apply-f args)]
          [else
           (define res (lte args (car entry)))
           (cond [(eq? res #t)  (cdr entry)]
                 [else  (apply-f (if res res args))])])))

(: cache-thunk/context (All (B) (-> (-> B) (-> B))))
(define (cache-thunk/context f)
  (: cache (HashTable (U #f gl-context) (U #f B)))
  (define cache (make-weak-hasheq))
  (λ ()
    (define ctxt (current-gl-context))
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
    [(_ (name arg ...) #:lte lte body ...)
     (syntax/loc stx
       (define name
         (cache-singleton/context lte (λ (arg ...) body ...))))]
    [(_ (name arg ...) body ...)
     (syntax/loc stx
       (define-singleton/context (name arg ...) #:lte equal? body ...))]))

;; ===================================================================================================

(: make-cached-vector (All (A) (-> Symbol (-> Integer A) (-> A Index) (-> Integer A))))
(define (make-cached-vector name make-vec vec-length)
  (: the-vec (Thread-Cellof (U #f A)))
  (define the-vec (make-thread-cell #f #f))  ; do not want preservation!
  
  (: get-vec (-> Integer A))
  (define (get-vec size)
    (cond [(index? size)
           (define vec (thread-cell-ref the-vec))
           (cond [(and vec (<= size (vec-length vec)))  vec]
                 [else
                  (define vec (make-vec (next-pow2 size)))
                  (thread-cell-set! the-vec vec)
                  vec])]
          [else
           (raise-argument-error name "Index" size)]))
  
  get-vec)

;; ===================================================================================================

(define-type Timeout-Timer%
  (Class (init-field [notify-callback (U #f (-> Any))]
                     [timeout-callback (U #f (-> Any))]
                     [notify-interval Integer]
                     [timeout-interval Integer])
         [notify (-> Void)]
         [timeout (-> Void)]
         [keep-alive (-> Void)]))

(: timeout-timer% Timeout-Timer%)
(define timeout-timer%
  (class object%
    (init-field notify-callback
                timeout-callback
                notify-interval
                timeout-interval)
    
    (super-new)
    
    (: notify-timer (U #f (Instance Timer%)))
    (define notify-timer
      (make-object timer% (λ () (send this notify)) notify-interval #f))
    
    (: timeout-timer (U #f (Instance Timer%)))
    (define timeout-timer
      (make-object timer% (λ () (send this timeout)) timeout-interval #t))
    
    (define/public (notify)
      (define notify-callback-val notify-callback)
      (when notify-callback-val
        (notify-callback-val)
        (void)))
    
    (define/public (timeout)
      ;; Stop the notify timer, and remove reference to it and its callback
      (define notify-timer-val notify-timer)
      (when notify-timer-val
        (send notify-timer-val stop)
        (set! notify-timer #f)
        (set! notify-callback #f))
      ;; Stop the timeout timer, and remove reference to it
      (define timeout-timer-val timeout-timer)
      (when timeout-timer-val
        (send timeout-timer-val stop)
        (set! timeout-timer #f))
      ;; Call the timeout callback if it exists and remove reference to it
      (define timeout-callback-val timeout-callback)
      (when timeout-callback-val
        (timeout-callback-val)
        (set! timeout-callback #f)))
    
    (define/public (keep-alive)
      (define timeout-timer-val timeout-timer)
      (when timeout-timer-val
        (send timeout-timer-val stop)
        (send timeout-timer-val start timeout-interval #t)))
    ))

;; ===================================================================================================

(define-type Camera%
  (Class (init-field [position  FlVector]
                     [velocity  FlVector]
                     [yaw    Flonum]
                     [pitch  Flonum])
         [get-position  (-> FlVector)]
         [set-position  (-> FlVector Void)]
         [get-velocity  (-> FlVector)]
         [set-velocity  (-> FlVector Void)]
         [get-view-matrix  (-> FlAffine3)]
         [change-angles  (-> Flonum Flonum Void)]
         [accelerate  (-> FlVector Flonum Void)]
         [rotate-direction  (-> FlVector FlVector)]
         [unrotate-direction  (-> FlVector FlVector)]))

(define camera%
  (class object%
    (init)
    (init-field [position : FlVector]
                [velocity : FlVector]
                [yaw : Flonum]
                [pitch : Flonum])
    
    (super-new)
    
    (: get-position (-> FlVector))
    (define/public (get-position) position)
    
    (: set-position (-> FlVector Void))
    (define/public (set-position v) (set! position v))
    
    (: get-velocity (-> FlVector))
    (define/public (get-velocity) velocity)
    
    (: set-velocity (-> FlVector Void))
    (define/public (set-velocity v) (set! velocity v))
    
    (: get-translation-matrix (-> FlAffine3))
    (define/private (get-translation-matrix)
      (translate-flt3 (flv3neg position)))
    
    (: get-rotation-matrix (-> FlLinear3))
    (define/private (get-rotation-matrix)
      (flt3compose
       (flt3compose (rotate-x-flt3 (- pitch))
                    (rotate-y-flt3 (- yaw)))
       (rotate-x-flt3 (/ pi -2.0))))
    
    (: get-view-matrix (-> FlAffine3))
    (define/public (get-view-matrix)
      (flt3compose (get-rotation-matrix) (get-translation-matrix)))
    
    (: accelerate (-> FlVector Flonum Void))
    (define/public (accelerate acc dt)
      (set! position (flv3+ (flv3+ position (flv3* velocity dt))
                            (flv3* acc (* 0.5 dt dt))))
      (set! velocity (flv3+ velocity (flv3* acc dt)))
      (define speed (flv3mag velocity))
      (when (< speed (flexpt 2.0 -10.0))
        (set! velocity (flvector 0.0 0.0 0.0))))
    
    (: rotate-direction (-> FlVector FlVector))
    (define/public (rotate-direction v)
      (flv4->norm (flt3tapply (get-rotation-matrix) (norm->flv4 v))))
    
    (: unrotate-direction (-> FlVector FlVector))
    (define/public (unrotate-direction v)
      (flv4->norm (flt3apply (flt3inverse (get-rotation-matrix)) (norm->flv4 v))))
    
    (: change-angles (-> Flonum Flonum Void))
    (define/public (change-angles dy dp)
      (let* ([y  (- yaw dy)]
             [p  (- pitch dp)]
             ;; Keep yaw between -pi and pi by floating-point modulo
             [y  (- y (* (* 2.0 pi) (round (/ y (* 2.0 pi)))))]
             ;; Keep pitch between -pi/2 and pi/2 by clamping (gimball lock)
             [p  (min (* 0.5 pi) (max (* -0.5 pi) p))])
      (set! yaw y)
      (set! pitch p)))
    ))

;; ===================================================================================================

(: snip-center-pointer (-> (Instance Snip%) (Values (U #f Integer) (U #f Integer))))
(define (snip-center-pointer snip)
  (define admin (send snip get-admin))
  (define editor (and admin (send admin get-editor)))
  (define canvas (and editor (send editor get-active-canvas)))
  (cond [(and editor canvas)
         (define loc-x0 ((inst box Real) 0))
         (define loc-y0 ((inst box Real) 0))
         (define loc-x1 ((inst box Real) 0))
         (define loc-y1 ((inst box Real) 0))
         (send editor get-snip-location snip loc-x0 loc-y0 #f)
         (send editor get-snip-location snip loc-x1 loc-y1 #t)
         (define-values (x0 y0)
           (let-values ([(x y)  (send editor editor-location-to-dc-location
                                      (unbox loc-x0) (unbox loc-y0))])
             (values (exact-floor x) (exact-floor y))))
         (define-values (x1 y1)
           (let-values ([(x y)  (send editor editor-location-to-dc-location
                                      (unbox loc-x1) (unbox loc-y1))])
             (values (exact-ceiling x) (exact-ceiling y))))
         (cond [(and (>= x0 0) (<= x1 (send canvas get-width))
                     (>= y0 0) (<= y1 (send canvas get-height)))
                (define x (quotient (+ x0 x1) 2))
                (define y (quotient (+ y0 y1) 2))
                (send canvas warp-pointer x y)
                (values x y)]
               [else
                (values #f #f)])]
        [else
         (values #f #f)]))

;; ===================================================================================================
;; Fast grouping for state sorting

(struct (K) span ([key : K]
                  [start : Nonnegative-Fixnum]
                  [end : Nonnegative-Fixnum]
                  [current : Nonnegative-Fixnum])
  #:transparent #:mutable)

(: group-by-key! (All (A K) (-> (Vectorof A)
                                (-> Integer (Vectorof A))
                                Nonnegative-Fixnum
                                Nonnegative-Fixnum
                                (-> A K)
                                (Listof (span K)))))
(define (group-by-key! xs get-swap-xs start end key)
  (: spans (HashTable K (span K)))
  (define spans (make-hasheq))
  
  (: ss (Listof (span K)))
  (define ss
    (for/fold ([ss : (Listof (span K))  empty]) ([i  (in-range start end)])
      (define k (key (vector-ref xs i)))
      (define s (hash-ref spans k #f))
      (cond [s  (set-span-end! s (unsafe-fx+ 1 (span-end s)))
                ss]
            [else  (define s (span k 0 1 0))
                   (hash-set! spans k s)
                   (cons s ss)])))
  
  (define len (- end start))
  (define swap-xs (get-swap-xs len))
  (vector-copy! swap-xs 0 xs start end)
  
  (for/fold ([n : Nonnegative-Fixnum  start]) ([s  (in-list ss)])
    (define len (span-end s))
    (define next-n (unsafe-fx+ n len))
    (set-span-start! s n)
    (set-span-current! s n)
    (set-span-end! s next-n)
    next-n)
  
  (for ([i  (in-range 0 len)])
    (define x (unsafe-vector-ref swap-xs i))
    (define k (key x))
    (define s (hash-ref spans k))
    (define n (span-current s))
    (vector-set! xs n x)
    (set-span-current! s (unsafe-fx+ n 1)))
  
  ss)

;; ===================================================================================================
;; Hash functions

(: hash-clear!* (All (A B) (-> (HashTable A B) Void)))
(define (hash-clear!* h)
  (for ([k  (in-list (hash-keys h))])
    (hash-remove! h k)))

(: hash-empty?* (All (A B) (-> (HashTable A B) Boolean)))
(define (hash-empty?* h)
  (let/ec return : Boolean
    (for/and : Boolean ([(k v)  (in-hash h)])
      (return #f))))

(: hash-merge (All (A B) (-> (HashTable A B) (HashTable A B) (HashTable A B))))
(define (hash-merge h1 h2)
  (for/fold ([h : (HashTable A B)  h1]) ([(k v)  (in-hash h2)])
    (hash-set h k v)))
