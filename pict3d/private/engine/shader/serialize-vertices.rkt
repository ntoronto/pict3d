#lang typed/racket/base

#|
Many of these functions put single flonums into a bytes object using something like

    (define bs-ptr (u8vector->cpointer bs))
    (ptr-set! bs-ptr _float 'abs i x)
    (ptr-set! bs-ptr _float 'abs (unsafe-fx+ i 4) y)
             
instead of 

    (define bs-ptr (ptr-add (u8vector->cpointer bs) i))
    (ptr-set! bs-ptr _float 0 x)
    (ptr-set! bs-ptr _float 1 y)

Turns out the former is about 10% faster.
|#

(require racket/unsafe/ops
         racket/flonum
         racket/math
         (except-in typed/opengl/ffi -> cast)
         "../../ffi.rkt"
         "../../math.rkt"
         "../utils.rkt")

(provide (all-defined-out))

(: serialize-byte (-> Bytes Nonnegative-Fixnum Integer Nonnegative-Fixnum))
(define (serialize-byte bs i b)
  (bytes-set! bs i b)
  (unsafe-fx+ i 1))

(: serialize-bytes (-> Bytes Nonnegative-Fixnum Bytes Index Nonnegative-Fixnum))
(define (serialize-bytes bs i b k)
  (bytes-copy! bs i b 0 k)
  (unsafe-fx+ i k))

(: serialize-float (-> Bytes Nonnegative-Fixnum Flonum Nonnegative-Fixnum))
(define (serialize-float bs i x)
  (cond [(< (bytes-length bs) (unsafe-fx+ i 4))
         (error 'serialize-float
                "expected buffer with at least 4 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (ptr-set! (u8vector->cpointer bs) _float 'abs i x)
         (unsafe-fx+ i 4)]))

(: serialize-vec3 (-> Bytes Nonnegative-Fixnum FlV3 Nonnegative-Fixnum))
(define (serialize-vec3 bs i v)
  (cond [(< (bytes-length bs) (unsafe-fx+ i 12))
         (error 'serialize-vec3
                "expected buffer with at least 12 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (call/flv3-values v
           (λ (x y z)
             (define bs-ptr (u8vector->cpointer bs))
             (ptr-set! bs-ptr _float 'abs i x)
             (ptr-set! bs-ptr _float 'abs (unsafe-fx+ i 4) y)
             (ptr-set! bs-ptr _float 'abs (unsafe-fx+ i 8) z)
             (unsafe-fx+ i 12)))]))

(: serialize-vec4 (-> Bytes Nonnegative-Fixnum FlV4 Nonnegative-Fixnum))
(define (serialize-vec4 bs i v)
  (cond [(< (bytes-length bs) (unsafe-fx+ i 16))
         (error 'serialize-vec4
                "expected buffer with at least 16 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (call/flv4-values v
           (λ (x y z w)
             (define bs-ptr (u8vector->cpointer bs))
             (ptr-set! bs-ptr _float 'abs i x)
             (ptr-set! bs-ptr _float 'abs (unsafe-fx+ i 4) y)
             (ptr-set! bs-ptr _float 'abs (unsafe-fx+ i 8) z)
             (ptr-set! bs-ptr _float 'abs (unsafe-fx+ i 12) w)
             (unsafe-fx+ i 16)))]))

(: serialize-floats (-> Bytes Nonnegative-Fixnum FlVector Nonnegative-Fixnum Nonnegative-Fixnum))
(define (serialize-floats bs i xs n)
  (define m (unsafe-fx* 4 n))
  (cond
    [(< (flvector-length xs) n)
     (error 'serialize-floats
            "expected FlVector with at least ~a elements; given length-~a FlVector"
            n (flvector-length xs))]
    [(< (bytes-length bs) (unsafe-fx+ i m))
     (error 'serialize-floats
            "expected buffer with at least ~a bytes left; given length-~a bytes at ~a"
            m (bytes-length bs) i)]
    [else
     (define bs-ptr (u8vector->cpointer bs))
     (let loop ([j : Nonnegative-Fixnum  0])
       (when (< j n)
         (define x (unsafe-flvector-ref xs j))
         (ptr-set! bs-ptr _float 'abs (unsafe-fx+ i (unsafe-fx* j 4)) x)
         (loop (unsafe-fx+ j 1))))
     (unsafe-fx+ i m)]))

(: serialize-float/byte (-> Bytes Nonnegative-Fixnum Flonum Nonnegative-Fixnum))
(define (serialize-float/byte bs i x)
  (bytes-set! bs i (flonum->byte x))
  (unsafe-fx+ i 1))

(: serialize-vec3/bytes (-> Bytes Nonnegative-Fixnum FlV3 Nonnegative-Fixnum))
(define (serialize-vec3/bytes bs i v)
  (cond [(< (bytes-length bs) (unsafe-fx+ i 3))
         (error 'serialize-vec3/bytes
                "expected buffer with at least 3 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (call/flv3-values v
           (λ (x y z)
             (unsafe-bytes-set! bs i (flonum->byte x))
             (unsafe-bytes-set! bs (unsafe-fx+ i 1) (flonum->byte y))
             (unsafe-bytes-set! bs (unsafe-fx+ i 2) (flonum->byte z))
             (unsafe-fx+ i 3)))]))

(: serialize-vec4/bytes (-> Bytes Nonnegative-Fixnum FlV4 Nonnegative-Fixnum))
(define (serialize-vec4/bytes bs i v)
  (cond [(< (bytes-length bs) (unsafe-fx+ i 4))
         (error 'serialize-vec4/bytes
                "expected buffer with at least 4 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (call/flv4-values v
           (λ (x y z w)
             (unsafe-bytes-set! bs i (flonum->byte x))
             (unsafe-bytes-set! bs (unsafe-fx+ i 1) (flonum->byte y))
             (unsafe-bytes-set! bs (unsafe-fx+ i 2) (flonum->byte z))
             (unsafe-bytes-set! bs (unsafe-fx+ i 3) (flonum->byte w))
             (unsafe-fx+ i 4)))]))

(: serialize-normal/bytes (-> Bytes Nonnegative-Fixnum FlV3 Boolean Nonnegative-Fixnum))
(define (serialize-normal/bytes bs i v back?)
  (cond [(< (bytes-length bs) (unsafe-fx+ i 3))
         (error 'serialize-normal/bytes
                "expected buffer with at least 3 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (call/flv3-values v
           (λ (x y z)
             (call/fl3normalize x y z
               (λ ([x : Flonum] [y : Flonum] [z : Flonum])
                 (define-values (b0 b1 b2)
                   (cond [back?  (pack-normal (- x) (- y) (- z))]
                         [else   (pack-normal x y z)]))
                 (unsafe-bytes-set! bs i b0)
                 (unsafe-bytes-set! bs (unsafe-fx+ i 1) b1)
                 (unsafe-bytes-set! bs (unsafe-fx+ i 2) b2)
                 (unsafe-fx+ i 3)))))]))

(: serialize-material-reflectances/bytes (-> Bytes Nonnegative-Fixnum FlV4 Nonnegative-Fixnum))
(define (serialize-material-reflectances/bytes bs i m)
  (cond [(< (bytes-length bs) (unsafe-fx+ i 3))
         (error 'serialize-material-reflectances/bytes
                "expected buffer with at least 3 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (call/flv4-values m
           (λ (a d s _)
             (unsafe-bytes-set! bs i (flonum->byte a))
             (unsafe-bytes-set! bs (unsafe-fx+ i 1) (flonum->byte d))
             (unsafe-bytes-set! bs (unsafe-fx+ i 2) (flonum->byte s))
             (unsafe-fx+ i 3)))]))

(: serialize-emitted/bytes (-> Bytes Nonnegative-Fixnum FlV4 Nonnegative-Fixnum))
(define (serialize-emitted/bytes bs i v)
  (cond [(< (bytes-length bs) (unsafe-fx+ i 4))
         (error 'serialize-emitted/bytes
                "expected buffer with at least 4 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (call/flv4-values v
           (λ (r g b int)
             (define s (+ r g b))
             (define intensity (* s int))
             (define intensity.hi (exact-floor intensity))
             (define intensity.lo (- intensity intensity.hi))
             (unsafe-bytes-set! bs i (flonum->byte (/ r s)))
             (unsafe-bytes-set! bs (unsafe-fx+ i 1) (flonum->byte (/ g s)))
             (unsafe-bytes-set! bs (unsafe-fx+ i 2) (flonum->byte intensity.lo))
             (unsafe-bytes-set! bs (unsafe-fx+ i 3) (max 0 (min 255 intensity.hi)))
             (unsafe-fx+ i 4)))]))

(: serialize-affine (-> Bytes Nonnegative-Fixnum FlAffine3 Nonnegative-Fixnum))
(define (serialize-affine bs i t)
  (cond [(< (bytes-length bs) (unsafe-fx+ i 48))
         (error 'serialize-affine
                "expected buffer with at least 48 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (memcpy (u8vector->cpointer bs) i (f32vector->cpointer (flaffine3-forward-data t)) 48)
         (unsafe-fx+ i 48)]))
