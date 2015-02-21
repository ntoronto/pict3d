#lang typed/racket/base

(require racket/unsafe/ops
         racket/list
         racket/vector
         (except-in typed/opengl/ffi -> cast)
         "../gl.rkt"
         "../utils.rkt"
         "draw-pass.rkt"
         "utils.rkt")

(provide merge-passes)

(: get-vertex-count (-> Boolean
                        (Vectorof shape-params)
                        Nonnegative-Fixnum
                        Nonnegative-Fixnum
                        Nonnegative-Fixnum))
(define (get-vertex-count indexed? ps start end)
  (for/fold ([vertex-count : Nonnegative-Fixnum  0]) ([i  (in-range start end)])
    (define v (shape-params-vertices (unsafe-vector-ref ps i)))
    (if (eq? indexed? (and (vertices-indexes v) #t))
        (unsafe-fx+ vertex-count (vertices-vertex-count v))
        vertex-count)))

(: get-index-count (-> (Vectorof shape-params)
                       Nonnegative-Fixnum
                       Nonnegative-Fixnum
                       Nonnegative-Fixnum))
(define (get-index-count ps start end)
  (for/fold ([index-count : Nonnegative-Fixnum  0]) ([i  (in-range start end)])
    (define v (shape-params-vertices (unsafe-vector-ref ps i)))
    (define indexes (vertices-indexes v))
    (if indexes
        (unsafe-fx+ index-count (vector-length indexes))
        index-count)))

(: merge-vertex-data (-> gl-program
                         Boolean
                         (Vectorof shape-params)
                         Nonnegative-Fixnum
                         Nonnegative-Fixnum
                         Nonnegative-Fixnum
                         Bytes))
(define (merge-vertex-data program indexed? ps start end vertex-count)
  (define struct-size (vao-struct-size (gl-program-struct program)))
  (define buffer-size (unsafe-fx* vertex-count struct-size))
  (define all-vertex-data (make-bytes buffer-size))
  (define all-vertex-data-ptr (u8vector->cpointer all-vertex-data))
  ;; Copy the vertex data into the buffer
  (for/fold ([vertex-num : Nonnegative-Fixnum  0]) ([i  (in-range start end)])
    (define v (shape-params-vertices (unsafe-vector-ref ps i)))
    (cond
      [(eq? indexed? (and (vertices-indexes v) #t))
       (define vertex-count (vertices-vertex-count v))
       (define vertex-data (vertices-vertex-data v))
       (memcpy all-vertex-data-ptr
               (unsafe-fx* vertex-num struct-size)
               (u8vector->cpointer vertex-data)
               (unsafe-fx* vertex-count struct-size)
               _byte)
       (unsafe-fx+ vertex-num vertex-count)]
      [else
       vertex-num]))
  all-vertex-data)

(: merge-indexes (-> (Vectorof shape-params)
                     Nonnegative-Fixnum
                     Nonnegative-Fixnum
                     (Vectorof Index)))
(define (merge-indexes ps start end)
  (define index-count (get-index-count ps start end))
  (define all-indexes ((inst make-vector Index) index-count))
  
  ;; Copy the index data into the buffer, shifted
  (for/fold ([vertex-num : Nonnegative-Fixnum  0]
             [index-num : Nonnegative-Fixnum 0]
             ) ([i  (in-range start end)])
    (define v (shape-params-vertices (unsafe-vector-ref ps i)))
    (define indexes (vertices-indexes v))
    (cond
      [indexes
       (define vertex-count (vertices-vertex-count v))
       (define index-count (vector-length indexes))
       (for ([j  (in-range index-count)])
         (define idx (unsafe-vector-ref indexes j))
         (vector-set! all-indexes
                      (unsafe-fx+ index-num j)
                      (assert (unsafe-fx+ vertex-num idx) index?)))
       (values (unsafe-fx+ vertex-num vertex-count)
               (unsafe-fx+ index-num index-count))]
      [else
       (values vertex-num
               index-num)]))
  all-indexes)

(: merge-vertices (-> gl-program
                      Boolean
                      (List-Hash String (U Symbol Uniform))
                      Boolean
                      Integer
                      (Vectorof shape-params)
                      Nonnegative-Fixnum
                      Nonnegative-Fixnum
                      (Listof shape-params)))
(define (merge-vertices program indexed? uniforms two-sided? mode ps start end)
  (define vertex-count (get-vertex-count indexed? ps start end))
  (cond
    [(> vertex-count max-shape-vertex-count)
     (define mid (unsafe-fxquotient (unsafe-fx+ start end) 2))
     (when (or (= start mid) (= end mid))
       (error 'merge-vertices
              "cannot merge a single shape with more than ~a vertices; given ~a vertices"
              max-shape-vertex-count
              vertex-count))
     (append
      (merge-vertices program indexed? uniforms two-sided? mode ps start mid)
      (merge-vertices program indexed? uniforms two-sided? mode ps mid end))]
    [(> vertex-count 0)
     ;; Allocate enough space for all the vertex data
     (define all-vertex-data (merge-vertex-data program indexed? ps start end vertex-count))
     (define all-indexes (if indexed? (merge-indexes ps start end) #f))
     
     (define verts (vertices (assert vertex-count index?) all-vertex-data all-indexes))
     (list (shape-params (λ () program) uniforms two-sided? mode verts))]
    [else
     empty]))

(: merge-shape-params (-> (Vectorof shape-params) (Vectorof shape-params)))
(define (merge-shape-params ps)
  (list->vector
   (append*
    (for*/list : (Listof (Listof shape-params))
      ([ks  (in-list (group-by-key! ps 0 (vector-length ps) shape-params-program))]
       [program  (in-value ((car ks)))]
       [s   (in-value (cdr ks))]
       [ks  (in-list (group-by-key! ps (span-start s) (span-end s) shape-params-uniforms))]
       [uniforms  (in-value (car ks))]
       [s  (in-value (cdr ks))]
       [ks  (in-list (group-by-key! ps (span-start s) (span-end s) shape-params-two-sided?))]
       [face  (in-value (car ks))]
       [s  (in-value (cdr ks))]
       [ks  (in-list (group-by-key! ps (span-start s) (span-end s) shape-params-mode))]
       [mode  (in-value (car ks))]
       [s  (in-value (cdr ks))])
      (append
       (merge-vertices program #f uniforms face mode ps (span-start s) (span-end s))
       (merge-vertices program #t uniforms face mode ps (span-start s) (span-end s)))))))

(: merge-passes (-> (Listof passes) passes))
(define (merge-passes ps)
  (passes
   (merge-shape-params (apply vector-append (map passes-light ps)))
   (merge-shape-params (apply vector-append (map passes-opaque-material ps)))
   (merge-shape-params (apply vector-append (map passes-opaque-color ps)))
   (merge-shape-params (apply vector-append (map passes-transparent-material ps)))
   (merge-shape-params (apply vector-append (map passes-transparent-color ps)))))
