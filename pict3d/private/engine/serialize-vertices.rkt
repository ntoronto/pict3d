#lang typed/racket/base

(require racket/unsafe/ops
         racket/flonum
         racket/math
         (except-in typed/opengl/ffi -> cast)
         "../ffi.rkt"
         "types.rkt"
         "utils.rkt")

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

(: serialize-vec2 (-> Bytes Nonnegative-Fixnum FlVector Nonnegative-Fixnum))
(define (serialize-vec2 bs i x)
  (cond [(< (flvector-length x) 2)
         (raise-argument-error 'serialize-vec2 "length-2 flvector" x)]
        [(< (bytes-length bs) (unsafe-fx+ i 8))
         (error 'serialize-vec2
                "expected buffer with at least 8 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (define bs-ptr (u8vector->cpointer bs))
         (ptr-set! bs-ptr _float 'abs i (unsafe-flvector-ref x 0))
         (ptr-set! bs-ptr _float 'abs (unsafe-fx+ i 4) (unsafe-flvector-ref x 1))
         (unsafe-fx+ i 8)]))

(: serialize-vec3 (-> Bytes Nonnegative-Fixnum FlVector Nonnegative-Fixnum))
(define (serialize-vec3 bs i x)
  (cond [(< (flvector-length x) 3)
         (raise-argument-error 'serialize-vec3 "length-3 flvector" x)]
        [(< (bytes-length bs) (unsafe-fx+ i 12))
         (error 'serialize-vec3
                "expected buffer with at least 12 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (define bs-ptr (u8vector->cpointer bs))
         (ptr-set! bs-ptr _float 'abs i (unsafe-flvector-ref x 0))
         (ptr-set! bs-ptr _float 'abs (unsafe-fx+ i 4) (unsafe-flvector-ref x 1))
         (ptr-set! bs-ptr _float 'abs (unsafe-fx+ i 8) (unsafe-flvector-ref x 2))
         (unsafe-fx+ i 12)]))

(: serialize-vec4 (-> Bytes Nonnegative-Fixnum FlVector Nonnegative-Fixnum))
(define (serialize-vec4 bs i x)
  (cond [(< (flvector-length x) 4)
         (raise-argument-error 'serialize-vec4 "length-4 flvector" x)]
        [(< (bytes-length bs) (unsafe-fx+ i 16))
         (error 'serialize-vec4
                "expected buffer with at least 16 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (define bs-ptr (u8vector->cpointer bs))
         (ptr-set! bs-ptr _float 'abs i (unsafe-flvector-ref x 0))
         (ptr-set! bs-ptr _float 'abs (unsafe-fx+ i 4) (unsafe-flvector-ref x 1))
         (ptr-set! bs-ptr _float 'abs (unsafe-fx+ i 8) (unsafe-flvector-ref x 2))
         (ptr-set! bs-ptr _float 'abs (unsafe-fx+ i 12) (unsafe-flvector-ref x 3))
         (unsafe-fx+ i 16)]))

(: serialize-float/byte (-> Bytes Nonnegative-Fixnum Flonum Nonnegative-Fixnum))
(define (serialize-float/byte bs i x)
  (bytes-set! bs i (flonum->byte x))
  (unsafe-fx+ i 1))

(: serialize-vec2/bytes (-> Bytes Nonnegative-Fixnum FlVector Nonnegative-Fixnum))
(define (serialize-vec2/bytes bs i x)
  (cond [(< (flvector-length x) 2)
         (raise-argument-error 'serialize-vec2/bytes "length-2 flvector" x)]
        [(< (bytes-length bs) (unsafe-fx+ i 2))
         (error 'serialize-vec2/bytes
                "expected buffer with at least 2 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (unsafe-bytes-set! bs i (flonum->byte (unsafe-flvector-ref x 0)))
         (unsafe-bytes-set! bs (unsafe-fx+ i 1) (flonum->byte (unsafe-flvector-ref x 1)))
         (unsafe-fx+ i 2)]))

(: serialize-vec3/bytes (-> Bytes Nonnegative-Fixnum FlVector Nonnegative-Fixnum))
(define (serialize-vec3/bytes bs i x)
  (cond [(< (flvector-length x) 3)
         (raise-argument-error 'serialize-vec3/bytes "length-3 flvector" x)]
        [(< (bytes-length bs) (unsafe-fx+ i 3))
         (error 'serialize-vec3/bytes
                "expected buffer with at least 3 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (unsafe-bytes-set! bs i (flonum->byte (unsafe-flvector-ref x 0)))
         (unsafe-bytes-set! bs (unsafe-fx+ i 1) (flonum->byte (unsafe-flvector-ref x 1)))
         (unsafe-bytes-set! bs (unsafe-fx+ i 2) (flonum->byte (unsafe-flvector-ref x 2)))
         (unsafe-fx+ i 3)]))

(: serialize-vec4/bytes (-> Bytes Nonnegative-Fixnum FlVector Nonnegative-Fixnum))
(define (serialize-vec4/bytes bs i x)
  (cond [(< (flvector-length x) 4)
         (raise-argument-error 'serialize-vec4/bytes "length-4 flvector" x)]
        [(< (bytes-length bs) (unsafe-fx+ i 4))
         (error 'serialize-vec4/bytes
                "expected buffer with at least 4 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (unsafe-bytes-set! bs i (flonum->byte (unsafe-flvector-ref x 0)))
         (unsafe-bytes-set! bs (unsafe-fx+ i 1) (flonum->byte (unsafe-flvector-ref x 1)))
         (unsafe-bytes-set! bs (unsafe-fx+ i 2) (flonum->byte (unsafe-flvector-ref x 2)))
         (unsafe-bytes-set! bs (unsafe-fx+ i 3) (flonum->byte (unsafe-flvector-ref x 3)))
         (unsafe-fx+ i 4)]))

(: serialize-normal/bytes (-> Bytes Nonnegative-Fixnum FlVector Boolean Nonnegative-Fixnum))
(define (serialize-normal/bytes bs i x back?)
  (cond [(< (flvector-length x) 3)
         (raise-argument-error 'serialize-normal/bytes "length-3 flvector" x)]
        [(< (bytes-length bs) (unsafe-fx+ i 3))
         (error 'serialize-normal/bytes
                "expected buffer with at least 3 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (define s (if back? -1.0 1.0))
         (define flonum->byte
           (λ ([x : Flonum]) (max 0 (min 255 (+ 127 (exact-ceiling (* s x 127.0)))))))
         (unsafe-bytes-set! bs i (flonum->byte (unsafe-flvector-ref x 0)))
         (unsafe-bytes-set! bs (unsafe-fx+ i 1) (flonum->byte (unsafe-flvector-ref x 1)))
         (unsafe-bytes-set! bs (unsafe-fx+ i 2) (flonum->byte (unsafe-flvector-ref x 2)))
         (unsafe-fx+ i 3)]))

(: serialize-material-reflectances/bytes (-> Bytes Nonnegative-Fixnum material Nonnegative-Fixnum))
(define (serialize-material-reflectances/bytes bs i m)
  (cond [(< (bytes-length bs) (unsafe-fx+ i 3))
         (error 'serialize-material-reflectances/bytes
                "expected buffer with at least 3 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (unsafe-bytes-set! bs i (flonum->byte (material-ambient m)))
         (unsafe-bytes-set! bs (unsafe-fx+ i 1) (flonum->byte (material-diffuse m)))
         (unsafe-bytes-set! bs (unsafe-fx+ i 2) (flonum->byte (material-specular m)))
         (unsafe-fx+ i 3)]))

(: serialize-emitted/bytes (-> Bytes Nonnegative-Fixnum FlVector Nonnegative-Fixnum))
(define (serialize-emitted/bytes bs i x)
  (cond [(< (flvector-length x) 4)
         (raise-argument-error 'serialize-emitted/bytes "length-4 flvector" x)]
        [(< (bytes-length bs) (unsafe-fx+ i 4))
         (error 'serialize-emitted/bytes
                "expected buffer with at least 4 bytes left; given length-~a bytes at ~a"
                (bytes-length bs) i)]
        [else
         (define r (unsafe-flvector-ref x 0))
         (define g (unsafe-flvector-ref x 1))
         (define b (unsafe-flvector-ref x 2))
         (define s (+ r g b))
         (define intensity (* s (unsafe-flvector-ref x 3)))
         (define intensity.hi (exact-floor intensity))
         (define intensity.lo (- intensity intensity.hi))
         (unsafe-bytes-set! bs i (flonum->byte (/ r s)))
         (unsafe-bytes-set! bs (unsafe-fx+ i 1) (flonum->byte (/ g s)))
         (unsafe-bytes-set! bs (unsafe-fx+ i 2) (flonum->byte intensity.lo))
         (unsafe-bytes-set! bs (unsafe-fx+ i 3) (max 0 (min 255 intensity.hi)))
         (unsafe-fx+ i 4)]))

(: serialize-affine (-> Bytes Nonnegative-Fixnum Affine Nonnegative-Fixnum))
(define (serialize-affine bs i x)
  (cond [(< (bytes-length bs) (unsafe-fx+ i affine-data-size))
         (error 'serialize-affine
                "expected buffer with at least ~a bytes left; given length-~a bytes at ~a"
                affine-data-size (bytes-length bs) i)]
        [else
         (define bs-ptr (u8vector->cpointer bs))
         (define affine-ptr (f32vector->cpointer (affine-data x)))
         (memcpy bs-ptr affine-ptr affine-data-size)
         (unsafe-fx+ i affine-data-size)]))
