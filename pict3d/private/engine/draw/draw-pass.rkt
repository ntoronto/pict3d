#lang typed/racket/base

(require racket/match
         racket/unsafe/ops
         typed/opengl
         (except-in typed/opengl/ffi cast ->)
         "../../math.rkt"
         "../../utils.rkt"
         "../../gl.rkt"
         "../../ffi.rkt"
         "../../utils.rkt"
         "../utils.rkt"
         "../shader.rkt"
         "types.rkt")

(provide draw-light-pass
         draw-opaque-material-pass
         draw-opaque-color-pass
         draw-transparent-material-pass
         draw-transparent-color-pass)

;; ===================================================================================================

(struct vertex-buffer ([vao : gl-vertex-array]
                       [vertex-buf : (U #f gl-array-buffer)]
                       [transform-buf : (U #f gl-array-buffer)]
                       [index-buf : (U #f gl-index-buffer)]
                       [vertex-count : Index]
                       [index-count : Index]))

(: make-vertex-buffer (-> Index Index Index Boolean vertex-buffer))
(define (make-vertex-buffer struct-size vertex-count index-count transform?)
  (define vao (make-gl-vertex-array))
  
  (define vertex-buf-size (* struct-size vertex-count))
  (define vertex-buf
    (cond [(> vertex-buf-size 0)
           (define vertex-buf (make-gl-array-buffer))
           (with-gl-array-buffer vertex-buf
             (glBufferData GL_ARRAY_BUFFER vertex-buf-size 0 GL_STREAM_DRAW))
           vertex-buf]
          [else  #f]))
  
  (define transform-buf
    (cond [transform?
           (define transform-buf (make-gl-array-buffer))
           (with-gl-array-buffer transform-buf
             (glBufferData GL_ARRAY_BUFFER (* 48 vertex-count) 0 GL_STREAM_DRAW))
           transform-buf]
          [else  #f]))
  
  (define index-buf-size (* 2 index-count))
  (define index-buf
    (cond [(> index-buf-size 0)
           (define index-buf (make-gl-index-buffer))
           (with-gl-index-buffer index-buf
             (glBufferData GL_ELEMENT_ARRAY_BUFFER index-buf-size 0 GL_STREAM_DRAW))
           index-buf]
          [else  #f]))
  
  (vertex-buffer vao vertex-buf transform-buf index-buf vertex-count index-count))

(: vertex-buffers (HashTable GL-Context (HashTable vao-struct (Vector (U vertex-buffer #f)
                                                                      (U vertex-buffer #f)))))
(define vertex-buffers (make-weak-hasheq))

(: get-vertex-buffer (-> gl-program Nonnegative-Fixnum Nonnegative-Fixnum vertex-buffer))
(define (get-vertex-buffer program vertex-count index-count)
  (define hash (hash-ref! vertex-buffers (get-current-managed-gl-context 'get-vertex-buffer)
                          (λ () ((inst make-weak-hasheq
                                       vao-struct
                                       (Vector (U vertex-buffer #f) (U vertex-buffer #f)))))))
  (define struct (gl-program-struct program))
  (define vbuffers ((inst hash-ref! vao-struct (Vector (U vertex-buffer #f) (U vertex-buffer #f)))
                    hash
                    struct
                    (λ () (vector #f #f))))
  (define vbuffers-idx (if (= index-count 0) 0 1))
  (define vbuffer (vector-ref vbuffers vbuffers-idx))
  (cond [(and vbuffer
              (<= vertex-count (vertex-buffer-vertex-count vbuffer))
              (<= index-count (vertex-buffer-index-count vbuffer)))
         vbuffer]
        [else
         (define _model0 (glGetAttribLocation (gl-object-handle program) "_model0"))
         (define _model1 (glGetAttribLocation (gl-object-handle program) "_model1"))
         (define _model2 (glGetAttribLocation (gl-object-handle program) "_model2"))
         (define transform? (and (>= _model0 0) (>= _model1 0) (>= _model2 0)))
         
         (define vertex-count2 (assert (next-pow2 vertex-count) index?))
         (define index-count2 (if (= index-count 0) 0 (assert (next-pow2 index-count) index?)))
         (log-pict3d-info "<engine> creating vao for ~v (~v) vertices, ~v (~v) indexes" 
                           vertex-count2 vertex-count
                           index-count2 index-count)
         (define vbuffer
           (make-vertex-buffer (vao-struct-size struct) vertex-count2 index-count2 transform?))
         
         (with-gl-vertex-array (vertex-buffer-vao vbuffer)
           (define vbuf (vertex-buffer-vertex-buf vbuffer))
           (when vbuf
             (with-gl-array-buffer vbuf
               (vao-struct-bind-attributes struct)))
           
           (define tbuf (vertex-buffer-transform-buf vbuffer))
           (when (and tbuf (>= _model0 0) (>= _model1 0) (>= _model2 0))
             (with-gl-array-buffer tbuf
               (glEnableVertexAttribArray _model0)
               (glEnableVertexAttribArray _model1)
               (glEnableVertexAttribArray _model2)
               (glVertexAttribPointer _model0 4 GL_FLOAT #f 48 0)
               (glVertexAttribPointer _model1 4 GL_FLOAT #f 48 (* 4 4))
               (glVertexAttribPointer _model2 4 GL_FLOAT #f 48 (* 8 4)))))
         
         (vector-set! vbuffers vbuffers-idx vbuffer)
         vbuffer]))

(: gl-map-buffer-range/stream (-> Integer Integer CPointer))
(define (gl-map-buffer-range/stream target size)
  (assert
   (glMapBufferRange
    target
    0
    size
    (bitwise-ior
     ;; Write-only
     GL_MAP_WRITE_BIT
     ;; Don't care if the entire buffer is invalidated; we're overwriting everything
     GL_MAP_INVALIDATE_RANGE_BIT
     GL_MAP_INVALIDATE_BUFFER_BIT
     ;; Can't use this with multiple calls to this function per frame
     ;; Can help a bit
     ;GL_MAP_UNSYNCHRONIZED_BIT
     ))
   cpointer?))

;; ===================================================================================================
;; Vertex drawing of fixed-length primitives (e.g. points, lines, triangles, quads)

(define get-tmp-transform-data
  (make-gl-cached-vector
   'get-tmp-transform-data
   (λ ([n : Integer])
     (log-pict3d-info "<engine> creating temp transform data of length ~v" n)
     (make-bytes n))
   bytes-length))

(define get-tmp-index-data
  (make-gl-cached-vector
   'get-tmp-index-data
   (λ ([n : Integer])
     (log-pict3d-info "<engine> creating temp index data of length ~v" n)
     (make-u16vector n))
   u16vector-length))

(: get-vertex-count (-> (Vectorof draw-params)
                        Nonnegative-Fixnum
                        Nonnegative-Fixnum 
                        Nonnegative-Fixnum))
(define (get-vertex-count ps start end)
  (for/fold ([vertex-count : Nonnegative-Fixnum  0])
            ([i  (in-range start end)])
    (define v (shape-params-vertices (draw-params-shape-params (unsafe-vector-ref ps i))))
    (unsafe-fx+ vertex-count (vertices-vertex-count v))))

(: get-index-count (-> (Vectorof draw-params)
                       Nonnegative-Fixnum
                       Nonnegative-Fixnum 
                       Nonnegative-Fixnum))
(define (get-index-count ps start end)
  (for/fold ([index-count : Nonnegative-Fixnum  0])
            ([i  (in-range start end)])
    (define v (shape-params-vertices (draw-params-shape-params (unsafe-vector-ref ps i))))
    (unsafe-fx+ index-count (vector-length (vertices-indexes v)))))

(: send-vertex-data (-> gl-array-buffer
                        gl-program
                        (Vectorof draw-params)
                        Nonnegative-Fixnum
                        Nonnegative-Fixnum
                        Nonnegative-Fixnum
                        Any))
(define (send-vertex-data vbuf program ps start end vertex-count)
  (gl-bind-array-buffer vbuf)
  ;; Map the vertex buffer into memory
  (define struct-size (vao-struct-size (gl-program-struct program)))
  (define buffer-size (unsafe-fx* struct-size vertex-count))
  (define all-vertex-data-ptr (gl-map-buffer-range/stream GL_ARRAY_BUFFER buffer-size))
  ;; Copy the vertex data into the buffer
  (for/fold ([vertex-num : Nonnegative-Fixnum  0])
            ([i  (in-range start end)])
    (define v (shape-params-vertices (draw-params-shape-params (unsafe-vector-ref ps i))))
    (define vertex-count (vertices-vertex-count v))
    (define vertex-data (vertices-vertex-data v))
    (memcpy all-vertex-data-ptr
            (unsafe-fx* vertex-num struct-size)
            (u8vector->cpointer vertex-data)
            (unsafe-fx* vertex-count struct-size)
            _byte)
    (unsafe-fx+ vertex-num vertex-count))
  ;; Unmap the vertex buffer (i.e. send the vertex data)
  (glUnmapBuffer GL_ARRAY_BUFFER))

(define transform-data (make-bytes 48))
(define transform-data-ptr (u8vector->cpointer transform-data))

(: send-transform-data (-> gl-array-buffer
                           (Vectorof draw-params)
                           Nonnegative-Fixnum
                           Nonnegative-Fixnum
                           Nonnegative-Fixnum
                           Any))
(define (send-transform-data tbuf ps start end vertex-count)
  ;; Copy the transform data into a temporary buffer - it must be done here because `memcpy*` also
  ;; reads from the buffer, which can be very slow from memory-mapped IO intended for writes
  (define buffer-size (unsafe-fx* 48 vertex-count))
  (define tmp-transform-data (get-tmp-transform-data buffer-size))
  (define tmp-transform-data-ptr (u8vector->cpointer tmp-transform-data))
  (for/fold ([vertex-num : Nonnegative-Fixnum  0])
            ([i  (in-range start end)])
    (define p (unsafe-vector-ref ps i))
    (define v (shape-params-vertices (draw-params-shape-params p)))
    (define vertex-count (vertices-vertex-count v))
    (serialize-affine transform-data 0 (draw-params-affine p))
    (memcpy* tmp-transform-data-ptr
             (unsafe-fx* vertex-num 48)
             transform-data-ptr
             48
             vertex-count)
    (unsafe-fx+ vertex-num vertex-count))
  ;; Map the transform buffer into memory and copy into it
  (gl-bind-array-buffer tbuf)
  (define all-transform-data-ptr (gl-map-buffer-range/stream GL_ARRAY_BUFFER buffer-size))
  (memcpy all-transform-data-ptr 0 tmp-transform-data-ptr buffer-size)
  ;; Unmap the transform buffer (i.e. send the transform data)
  (glUnmapBuffer GL_ARRAY_BUFFER))

(: send-index-data (-> gl-index-buffer
                       (Vectorof draw-params)
                       Nonnegative-Fixnum
                       Nonnegative-Fixnum
                       Nonnegative-Fixnum
                       Any))
(define (send-index-data ibuf ps start end index-count)
  ;; Copy the index data into the temporary buffer, shifted
  (define tmp-index-data (get-tmp-index-data index-count))
  (define tmp-index-data-ptr (u16vector->cpointer tmp-index-data))
  (for/fold ([vertex-num : Nonnegative-Fixnum  0]
             [index-num : Nonnegative-Fixnum 0])
            ([i  (in-range start end)])
    (define v (shape-params-vertices (draw-params-shape-params (unsafe-vector-ref ps i))))
    (define indexes (vertices-indexes v))
    (define vertex-count (vertices-vertex-count v))
    (define index-count (vector-length indexes))
    (for ([j  (in-range index-count)])
      (define idx (unsafe-vector-ref indexes j))
      (unsafe-u16vector-set! tmp-index-data
                             (unsafe-fx+ index-num j)
                             (unsafe-fx+ vertex-num idx)))
    (values (unsafe-fx+ vertex-num vertex-count)
            (unsafe-fx+ index-num index-count)))
  ;; Map the index buffer into memory and copy into it
  (gl-bind-index-buffer ibuf)
  (define buffer-size (unsafe-fx* 2 index-count))
  (define all-index-data-ptr (gl-map-buffer-range/stream GL_ELEMENT_ARRAY_BUFFER buffer-size))
  (memcpy all-index-data-ptr 0 tmp-index-data-ptr buffer-size)
  ;; Unmap the index buffer (i.e. send the index data)
  (glUnmapBuffer GL_ELEMENT_ARRAY_BUFFER))

(: send-vertices (-> gl-program
                     Integer
                     (Vectorof draw-params)
                     Nonnegative-Fixnum
                     Nonnegative-Fixnum
                     Void))
(define (send-vertices program mode ps start end)
  (define vertex-count (get-vertex-count ps start end))
  (cond
    [(> vertex-count max-shape-vertex-count)
     (define mid (unsafe-fxquotient (unsafe-fx+ start end) 2))
     (when (or (= start mid) (= end mid))
       (error 'send-single-vertices
              "cannot draw a single shape with more than ~a vertices; given ~a vertices"
              max-shape-vertex-count
              vertex-count))
     (send-vertices program mode ps start mid)
     (send-vertices program mode ps mid end)]
    [(> vertex-count 0)
     (define index-count (get-index-count ps start end))
     ;; Get (and cache) a VAO with a big enough buffer
     (match-define (vertex-buffer vao vbuf tbuf ibuf _ _)
       (get-vertex-buffer program vertex-count index-count))
     ;; With the vertex array object (not using with-gl-vertex-array for performance reasons)...
     (gl-bind-vertex-array vao)
     ;; Send all the vertex data and indexes
     (when vbuf (send-vertex-data vbuf program ps start end vertex-count))
     (when tbuf (send-transform-data tbuf ps start end vertex-count))
     (when ibuf (send-index-data ibuf ps start end index-count))
     ;; Draw!
     (glDrawElements mode index-count GL_UNSIGNED_SHORT 0)]))

;; ===================================================================================================
;; Drawing pass loop

(: send-draw-params (-> (Vectorof draw-params)
                        Nonnegative-Fixnum
                        Nonnegative-Fixnum
                        (HashTable Symbol Uniform)
                        Face
                        Void))
(define (send-draw-params ps start end standard-uniforms face)
  ;; For each program...
  (for ([ks  (in-list (group-by-key! ps start end
                                     (λ ([ts : draw-params])
                                       (shape-params-program
                                        (draw-params-shape-params ts)))))])
    (match-define (cons k s) ks)
    (define program (k))
    (define program-uniforms (gl-program-standard-uniforms program))
    (with-gl-program program
      (gl-program-send-uniforms program program-uniforms standard-uniforms)
      ;; For each set of shape uniforms...
      (for ([ks  (in-list (group-by-key! ps (span-start s) (span-end s)
                                         (λ ([ts : draw-params])
                                           (shape-params-uniforms (draw-params-shape-params ts)))))])
        (match-define (cons shape-uniforms s) ks)
        (gl-program-send-uniforms program shape-uniforms standard-uniforms)
        ;; For each kind of face...
        (for ([ks  (in-list
                    ((inst group-by-key! draw-params Face)
                     ps (span-start s) (span-end s)
                     (λ ([ts : draw-params])
                       (cond [(shape-params-two-sided? (draw-params-shape-params ts))  'both]
                             [(flt3consistent? (draw-params-affine ts))  face]
                             [else  (opposite-gl-face face)]))))])
          (match-define (cons face s) ks)
          (gl-draw-face face)
          ;; For each drawing mode...
          (for ([ks  (in-list (group-by-key! ps (span-start s) (span-end s)
                                             (λ ([ts : draw-params])
                                               (shape-params-mode (draw-params-shape-params ts)))))])
            (match-define (cons mode s) ks)
            (define start (span-start s))
            (define end (span-end s))
            (send-vertices program mode ps start end))))))
  (gl-bind-index-buffer null-gl-index-buffer)
  (gl-bind-array-buffer null-gl-array-buffer)
  (gl-bind-vertex-array null-gl-vertex-array)
  (gl-draw-face 'front)
  )

;; ===================================================================================================
;; Pass extraction

(define get-all-params
  (make-gl-cached-vector
   'get-all-params
   (λ ([n : Integer])
     (log-pict3d-info "<engine> creating draw-params vector of length ~v" n)
     ((inst build-vector draw-params) n (λ (_) (empty-draw-params))))
   vector-length))

(define-syntax-rule (make-draw-pass get-params-stx)
  (let ([get-params : (-> passes (Vectorof shape-params))  get-params-stx])
    (λ ([passess : (Vectorof draw-passes)]
        [num : Natural]
        [standard-uniforms : (HashTable Symbol Uniform)]
        [face : Face])
      ;(log-pict3d-debug "<engine> drawing pass ~v" pass)
      (define len
        (for/fold ([len : Nonnegative-Fixnum  0]) ([j  (in-range (min num (vector-length passess)))])
          (define passes (unsafe-vector-ref passess j))
          (unsafe-fx+ len (vector-length (get-params (draw-passes-passes passes))))))
      
      (define params (get-all-params len))
      
      (define n
        (for/fold ([n : Nonnegative-Fixnum  0]) ([j  (in-range (min num (vector-length passess)))])
          (define passes (unsafe-vector-ref passess j))
          (let ([t  (draw-passes-affine passes)]
                [passes  (draw-passes-passes passes)])
            (define ps (get-params passes))
            (for ([i  (in-range (vector-length ps))])
              (define pi (unsafe-vector-ref params (+ n i)))
              (set-draw-params-shape-params! pi (unsafe-vector-ref ps i))
              (set-draw-params-affine! pi t))
            (unsafe-fx+ n (vector-length ps)))))
      
      (unless (= n len)
        (error 'draw-pass "copy error: wrote ~a but len = ~a" n len))
      
      (send-draw-params params 0 len standard-uniforms face)
      
      (for ([i  (in-range n)])
        (define pi (unsafe-vector-ref params i))
        (set-draw-params-shape-params! pi empty-shape-params)
        (set-draw-params-affine! pi identity-flaffine3)))))

(define draw-light-pass (make-draw-pass passes-light))
(define draw-opaque-material-pass (make-draw-pass passes-opaque-material))
(define draw-opaque-color-pass (make-draw-pass passes-opaque-color))
(define draw-transparent-material-pass (make-draw-pass passes-transparent-material))
(define draw-transparent-color-pass (make-draw-pass passes-transparent-color))
