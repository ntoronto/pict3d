#lang typed/racket/base

(require racket/list
         racket/unsafe/ops
         math/base
         math/flonum
         (except-in typed/opengl/ffi cast ->)
         "../gl.rkt"
         "../utils.rkt")

(provide (all-defined-out))

(: find-tail (All (A) (-> A (Listof A) (Listof A))))
(define (find-tail x xs)
  (let ([xs  (member x xs)])
    (if xs xs (error 'find-tail "expected list containing ~e; given ~e" x xs))))

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

(: flfract (-> Flonum Flonum))
(define (flfract x)
  (- x (floor x)))

(: unorm8x3_to_snorm12x2 (-> Integer Integer Integer (Values Flonum Flonum)))
(define (unorm8x3_to_snorm12x2 ux uy uz)
  (let ([uy  (* (fl uy) #i1/16)])
    (define sx (+ (* ux 16.0) (floor uy)))
    (define sy (+ (* (flfract uy) (* 16.0 256.0)) uz))
    (values (max -1.0 (min 1.0 (- (* sx #i1/2047) 1.0)))
            (max -1.0 (min 1.0 (- (* sy #i1/2047) 1.0))))))

(: snorm12x2_to_unorm8x3 (-> Flonum Flonum (Values Integer Integer Integer)))
(define (snorm12x2_to_unorm8x3 fx fy)
  (cond
    [(and (< -inf.0 fx +inf.0)
          (< -inf.0 fy +inf.0))
     (define ux (round (+ (* (max -1.0 (min 1.0 fx)) 2047.0) 2047.0)))
     (define uy (round (+ (* (max -1.0 (min 1.0 fy)) 2047.0) 2047.0)))
     (define t (floor (* uy #i1/256)))
     (values (exact-floor (* ux #i1/16))
             (exact-floor (+ (* (flfract (* ux #i1/16)) 256.0) t))
             (exact-floor (- uy (* t 256.0))))]
    [else
     (values 0 0 1)]))

(: sign-not-zero (-> Flonum Flonum))
(define (sign-not-zero x)
  (if (>= x 0.0) +1.0 -1.0))

(: pack-normal (-> Flonum Flonum Flonum (Values Integer Integer Integer)))
(define (pack-normal vx vy vz)
  (define m (/ 1.0 (+ (abs vx) (abs vy) (abs vz))))
  (define px (* vx m))
  (define py (* vy m))
  (snorm12x2_to_unorm8x3
   (if (> vz 0.0) px (* (- 1.0 (abs py)) (sign-not-zero px)))
   (if (> vz 0.0) py (* (- 1.0 (abs px)) (sign-not-zero py)))))

(: unpack-normal (-> Integer Integer Integer (Values Flonum Flonum Flonum)))
(define (unpack-normal bx by bz)
  (define-values (ex ey) (unorm8x3_to_snorm12x2 bx by bz))
  (define vz (- 1.0 (abs ex) (abs ey)))
  (define vx (if (>= vz 0.0) ex (* (- 1.0 (abs ey)) (sign-not-zero ex))))
  (define vy (if (>= vz 0.0) ey (* (- 1.0 (abs ex)) (sign-not-zero ey))))
  (define m (sqrt (+ (sqr vx) (sqr vy) (sqr vz))))
  (values (/ vx m) (/ vy m) (/ vz m)))

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
