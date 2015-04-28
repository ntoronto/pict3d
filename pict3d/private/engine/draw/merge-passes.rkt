#lang typed/racket/base

(require racket/unsafe/ops
         racket/list
         racket/vector
         (except-in typed/opengl/ffi -> cast)
         "../../gl.rkt"
         "../../utils.rkt"
         "../utils.rkt"
         "types.rkt")

(provide merge-passes)

(: get-counts (-> (Vectorof shape-params)
                  Nonnegative-Fixnum
                  Nonnegative-Fixnum
                  (Values Nonnegative-Fixnum Nonnegative-Fixnum)))
(define (get-counts ps start end)
  (for/fold ([vertex-count : Nonnegative-Fixnum  0]
             [index-count : Nonnegative-Fixnum  0])
            ([i  (in-range start end)])
    (define v (shape-params-vertices (unsafe-vector-ref ps i)))
    (values (unsafe-fx+ vertex-count (vertices-vertex-count v))
            (unsafe-fx+ index-count (vector-length (vertices-indexes v))))))

(: merge-data (-> gl-program
                  (Vectorof shape-params)
                  Nonnegative-Fixnum
                  Nonnegative-Fixnum
                  Nonnegative-Fixnum
                  Nonnegative-Fixnum
                  vertices))
(define (merge-data program ps start end vertex-count index-count)
  (define struct-size (vao-struct-size (gl-program-struct program)))
  (define buffer-size (unsafe-fx* vertex-count struct-size))
  (define all-vertex-data (make-bytes buffer-size))
  (define all-vertex-data-ptr (u8vector->cpointer all-vertex-data))
  
  (define all-indexes ((inst make-vector Index) index-count))
  
  (: vertex-hash (HashTable Bytes Index))
  (define vertex-hash (make-hash))
  
  (define-values (vertex-num index-num)
    (for/fold ([vertex-num : Nonnegative-Fixnum  0]
               [index-num : Nonnegative-Fixnum 0])
              ([i  (in-range start end)])
      (define v (shape-params-vertices (unsafe-vector-ref ps i)))
      (define old-vertex-count (vertices-vertex-count v))
      (define vertex-data (vertices-vertex-data v))
      (define indexes (vertices-indexes v))
      (define index-count (vector-length indexes))
      
      ;; Mapping from old index to new index
      (define vertex-indexes ((inst make-vector Index) old-vertex-count 0))
      ;; Copy the vertex data while merging vertexes
      (define vertex-count
        (for/fold ([vertex-count : Nonnegative-Fixnum  0])
                  ([j  (in-range old-vertex-count)])
          (define bs-start (unsafe-fx* j struct-size))
          (define bs (subbytes vertex-data bs-start (unsafe-fx+ bs-start struct-size)))
          (define new-j (hash-ref vertex-hash bs #f))
          (cond
            [(not new-j)
             (define new-j (assert (unsafe-fx+ vertex-num vertex-count) index?))
             (hash-set! vertex-hash bs new-j)
             (vector-set! vertex-indexes j new-j)
             (memcpy all-vertex-data-ptr
                     (unsafe-fx* new-j struct-size)
                     (u8vector->cpointer vertex-data)
                     (unsafe-fx* j struct-size)
                     struct-size
                     _byte)
             (unsafe-fx+ vertex-count 1)]
            [else
             (vector-set! vertex-indexes j new-j)
             vertex-count])))
      
      ;; Copy the indexes
      (for ([k  (in-range index-count)])
        (define j (unsafe-vector-ref indexes k))
        (vector-set! all-indexes
                     (unsafe-fx+ index-num k)
                     (vector-ref vertex-indexes j)))
      
      (values (unsafe-fx+ vertex-num vertex-count)
              (unsafe-fx+ index-num index-count))))
  
  (vertices (assert vertex-num index?)
            (subbytes all-vertex-data 0 (* vertex-num struct-size))
            all-indexes))

(: merge-vertices (-> gl-program
                      (List-Hash String (U Symbol Uniform))
                      Boolean
                      Integer
                      (Vectorof shape-params)
                      Nonnegative-Fixnum
                      Nonnegative-Fixnum
                      (Listof shape-params)))
(define (merge-vertices program uniforms two-sided? mode ps start end)
  (define-values (vertex-count index-count) (get-counts ps start end))
  (cond
    [(> vertex-count max-shape-vertex-count)
     (define mid (unsafe-fxquotient (unsafe-fx+ start end) 2))
     (when (or (= start mid) (= end mid))
       (error 'merge-vertices
              "cannot merge a single shape with more than ~a vertices; given ~a vertices"
              max-shape-vertex-count
              vertex-count))
     (append
      (merge-vertices program uniforms two-sided? mode ps start mid)
      (merge-vertices program uniforms two-sided? mode ps mid end))]
    [(> vertex-count 0)
     (define verts (merge-data program ps start end vertex-count index-count))
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
      (merge-vertices program uniforms face mode ps (span-start s) (span-end s))))))

(: merge-passes (-> (Listof passes) passes))
(define (merge-passes ps)
  (passes
   (merge-shape-params (apply vector-append (map passes-light ps)))
   (merge-shape-params (apply vector-append (map passes-opaque-material ps)))
   (merge-shape-params (apply vector-append (map passes-opaque-color ps)))
   (merge-shape-params (apply vector-append (map passes-transparent-material ps)))
   (merge-shape-params (apply vector-append (map passes-transparent-color ps)))))
