#lang typed/racket/base

(require racket/match
         racket/list
         racket/unsafe/ops
         racket/vector
         typed/opengl
         (except-in typed/opengl/ffi cast ->)
         "../math/flt3.rkt"
         "../utils.rkt"
         "gl.rkt"
         "types.rkt"
         "utils.rkt")

(provide (struct-out vertices)
         (struct-out single-vertices)
         (struct-out multi-vertices)
         Vertices
         (struct-out shape-params)
         (struct-out affine)
         affine-data-size
         identity-affine
         make-affine
         ->affine
         affine-compose
         affine-data
         (struct-out draw-passes)
         empty-shape-params
         Passes
         (struct-out span)
         group-by-key!
         draw-pass)

(struct vertices
  ([vertex-count : Index]
   [vertex-data : Bytes])  ; data to send, already converted to bytes
  #:transparent)
;; The length of `vertex-data` must be no less than `vertex-count` times the struct size!

(struct single-vertices vertices ()
  #:transparent)

(struct multi-vertices vertices
  ([starts : (Vectorof Nonnegative-Fixnum)]   ; primitive start indexes
   [counts : S32Vector])  ; primitive vertex counts
  #:transparent)

(define-type Vertices (U single-vertices
                         multi-vertices))

(struct shape-params ([program-spec : (-> program-spec)]
                      [uniforms : (List-Hash String (U Symbol Uniform))]
                      [face : Face]
                      [mode : Integer]
                      [vertices : Vertices])
  #:transparent)

(struct affine ([transform : FlAffine3-]
                [lazy-data : (Lazy-Box F32Vector)]
                [lazy-consistent? : (Lazy-Box Boolean)])
  #:transparent)

(struct draw-params ([shape-params : shape-params]
                     [transform : affine])
  #:transparent)

(define-type Passes (Vectorof (Vectorof shape-params)))

(struct draw-passes ([passes : Passes]
                     [transform : affine])
  #:transparent)

(define empty-shape-params
  (shape-params (λ () (error 'empty-shape-params)) empty 'both 0 (single-vertices 0 #"")))

(define identity-transform (f32vector 1.0 0.0 0.0 0.0
                                      0.0 1.0 0.0 0.0
                                      0.0 0.0 1.0 0.0))

(define identity-affine
  (affine identity-flt3
          (lazy-box F32Vector identity-transform)
          (lazy-box Boolean #t)))

(: make-affine (-> FlAffine3- affine))
(define (make-affine t)
  (affine t (box 'lazy) (box 'lazy)))

(: ->affine (-> (U affine FlAffine3-) affine))
(define (->affine t)
  (if (affine? t) t (make-affine t)))

(: affine-compose (-> (U affine FlAffine3-) (U affine FlAffine3-) affine))
(define (affine-compose t1 t2)
  (let ([t1  (if (affine? t1) (affine-transform t1) t1)]
        [t2  (if (affine? t2) (affine-transform t2) t2)])
    (make-affine (flt3compose t1 t2))))

(: empty-draw-params draw-params)
(define empty-draw-params (draw-params empty-shape-params identity-affine))

;; ===================================================================================================

(require "../ffi.rkt")

(: get-transform-data (-> FlAffine3- F32Vector))
(define (get-transform-data t)
  (cond
    [(flidentity3? t)  identity-transform]
    [else
     (define-values (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
       (cond [(flidentity3? t)
              (values 1.0 0.0 0.0 0.0
                      0.0 1.0 0.0 0.0
                      0.0 0.0 1.0 0.0)]
             [(fllinear3? t)
              (define-values (m00 m01 m02 m10 m11 m12 m20 m21 m22) (fllinear3-values t))
              (values m00 m01 m02 0.0
                      m10 m11 m12 0.0
                      m20 m21 m22 0.0)]
             [(flaffine3? t)
              (flaffine3-values t)]))
     (define vec (make-f32vector 12))
     (define p (f32vector->cpointer vec))
     (ptr-set! p _float 0 m00)
     (ptr-set! p _float 1 m01)
     (ptr-set! p _float 2 m02)
     (ptr-set! p _float 3 m03)
     (ptr-set! p _float 4 m10)
     (ptr-set! p _float 5 m11)
     (ptr-set! p _float 6 m12)
     (ptr-set! p _float 7 m13)
     (ptr-set! p _float 8 m20)
     (ptr-set! p _float 9 m21)
     (ptr-set! p _float 10 m22)
     (ptr-set! p _float 11 m23)
     vec]))

(: affine-data (-> affine F32Vector))
(define (affine-data m)
  (lazy-box-ref!
   (affine-lazy-data m)
   (λ () (get-transform-data (affine-transform m)))))

(: affine-consistent? (-> affine Boolean))
(define (affine-consistent? m)
  (lazy-box-ref!
   (affine-lazy-consistent? m)
   (λ () (flt3consistent? (affine-transform m)))))

;; ===================================================================================================

(struct vertex-buffer ([vao : gl-object]
                       [vertex-buf : (U #f gl-object)]
                       [transform-buf : (U #f gl-object)]
                       [vertex-count : Index]))

(define affine-data-size (* 12 4))

(: make-vertex-buffer (-> Index Index Boolean vertex-buffer))
(define (make-vertex-buffer struct-size vertex-count transform?)
  (define vao (make-gl-vertex-array))
  
  (define vertex-buf-size (* struct-size vertex-count))
  (define vertex-buf
    (cond [(> vertex-buf-size 0)
           (define vertex-buf (make-gl-buffer))
           (with-gl-array-buffer vertex-buf
             (glBufferData GL_ARRAY_BUFFER vertex-buf-size 0 GL_STREAM_DRAW))
           vertex-buf]
          [else  #f]))
  
  (define transform-buf
    (cond [transform?
           (define transform-buf (make-gl-buffer))
           (with-gl-array-buffer transform-buf
             (glBufferData GL_ARRAY_BUFFER (* affine-data-size vertex-count) 0 GL_STREAM_DRAW))
           transform-buf]
          [else  #f]))
  
  (vertex-buffer vao vertex-buf transform-buf vertex-count))

(: vertex-buffers (HashTable (U #f gl-context) (HashTable vao-struct vertex-buffer)))
(define vertex-buffers (make-weak-hasheq))

(: get-vertex-buffer (-> gl-object vao-struct Nonnegative-Fixnum vertex-buffer))
(define (get-vertex-buffer program struct vertex-count)
  (define hash (hash-ref! vertex-buffers (current-gl-context)
                          (λ () ((inst make-weak-hasheq vao-struct vertex-buffer)))))
  (define vbuffer (hash-ref hash struct #f))
  (cond [(and vbuffer (<= vertex-count (vertex-buffer-vertex-count vbuffer)))  vbuffer]
        [else
         (define _model0 (glGetAttribLocation (gl-object-handle program) "_model0"))
         (define _model1 (glGetAttribLocation (gl-object-handle program) "_model1"))
         (define _model2 (glGetAttribLocation (gl-object-handle program) "_model2"))
         (define transform? (and (>= _model0 0) (>= _model1 0) (>= _model2 0)))
         
         (define buffer-count (assert (next-pow2 vertex-count) index?))
         (printf "creating vao for ~v (~v) vertices~n" buffer-count vertex-count)
         (define vbuffer (make-vertex-buffer (vao-struct-size struct) buffer-count transform?))
         
         (with-gl-vertex-array (vertex-buffer-vao vbuffer)
           (define vbuf (vertex-buffer-vertex-buf vbuffer))
           (when vbuf
             (with-gl-array-buffer vbuf
               (vao-struct-bind-attributes struct)))
           
           (define tbuf (vertex-buffer-transform-buf vbuffer))
           (when (and tbuf (>= _model0 0) (>= _model1 0) (>= _model2 0))
             (with-gl-array-buffer tbuf
               (glEnableVertexAttribArray _model0)
               (glVertexAttribPointer _model0 4 GL_FLOAT #f affine-data-size 0)
               (glEnableVertexAttribArray _model1)
               (glVertexAttribPointer _model1 4 GL_FLOAT #f affine-data-size (* 4 4))
               (glEnableVertexAttribArray _model2)
               (glVertexAttribPointer _model2 4 GL_FLOAT #f affine-data-size (* 8 4)))))
         
         (hash-set! hash struct vbuffer)
         vbuffer]))

(define get-all-starts (make-cached-vector 'get-all-starts make-s32vector s32vector-length))
(define get-all-counts (make-cached-vector 'get-all-counts make-s32vector s32vector-length))

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
;; Vertex drawing of fixed-length primitives (e.g. points, lines, triangles)

(define get-tmp-transform-data
  (make-cached-vector 'get-tmp-transform-data make-bytes bytes-length))

(: send-single-vertices (-> gl-object
                            vao-struct
                            Integer
                            (Vectorof draw-params)
                            Nonnegative-Fixnum
                            Nonnegative-Fixnum
                            Void))
(define (send-single-vertices program struct mode ps start end)
  ;; Compute the total number of single vertices to send to the GPU
  (: vertex-count Nonnegative-Fixnum)
  (define-values (vertex-count vertex-count-max)
    (for/fold ([vertex-count : Nonnegative-Fixnum  0]
               [vertex-count-max : Nonnegative-Fixnum  0]
               ) ([i  (in-range start end)])
      (define v (shape-params-vertices (draw-params-shape-params (unsafe-vector-ref ps i))))
      (define count (vertices-vertex-count v))
      (if (single-vertices? v)
          (values (unsafe-fx+ vertex-count count)
                  (unsafe-fxmax vertex-count-max count))
          (values vertex-count
                  vertex-count-max))))
  
  (cond
    [(> vertex-count (* 1024 1024))
     (define mid (unsafe-fxquotient (unsafe-fx+ start end) 2))
     (when (or (= start mid) (= end mid))
       (error 'send-single-vertices
              "cannot draw a single shape with more than ~a vertices; given ~a vertices"
              (* 1024 1024)
              vertex-count))
     (send-single-vertices program struct mode ps start mid)
     (send-single-vertices program struct mode ps mid end)]
    [(> vertex-count 0)
     (define tmp-transform-data
       (get-tmp-transform-data (unsafe-fx* vertex-count-max affine-data-size)))
     (define tmp-transform-data-ptr (u8vector->cpointer tmp-transform-data))
     
     ;; Get (and cache) a VAO with a big enough buffer
     (match-define (vertex-buffer vao vbuf tbuf _) (get-vertex-buffer program struct vertex-count))
     
     ;; With the vertex array object (not using with-gl-vertex-array for performance reasons)...
     (gl-bind-vertex-array vao)
     
     ;; Write the vertex data
     (when vbuf
       (gl-bind-array-buffer vbuf)
       ;; Map the vertex buffer into memory
       (define buffer-size (unsafe-fx* (vao-struct-size struct) vertex-count))
       (define all-vertex-data-ptr (gl-map-buffer-range/stream GL_ARRAY_BUFFER buffer-size))
       ;; Copy the vertex data into the buffer
       (define struct-size (vao-struct-size struct))
       (for/fold ([vertex-num : Nonnegative-Fixnum  0]) ([i  (in-range start end)])
         (define p (unsafe-vector-ref ps i))
         (define v (shape-params-vertices (draw-params-shape-params p)))
         (cond
           [(single-vertices? v)
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
       ;; Unamp the vertex buffer (i.e. send the vertex data)
       (glUnmapBuffer GL_ARRAY_BUFFER))
     
     ;; Write the transform data
     (when tbuf
       (gl-bind-array-buffer tbuf)
       ;; Map the transform buffer into memroy
       (define buffer-size (unsafe-fx* affine-data-size vertex-count))
       (define all-transform-data-ptr (gl-map-buffer-range/stream GL_ARRAY_BUFFER buffer-size))
       ;; Copy the transform data into the buffer
       (for/fold ([vertex-num : Nonnegative-Fixnum  0]) ([i  (in-range start end)])
         (define p (unsafe-vector-ref ps i))
         (define v (shape-params-vertices (draw-params-shape-params p)))
         (cond
           [(single-vertices? v)
            (define vertex-count (vertices-vertex-count v))
            (define transform-data (affine-data (draw-params-transform p)))
            (memcpy* tmp-transform-data-ptr 0
                     (f32vector->cpointer transform-data) affine-data-size
                     vertex-count)
            (memcpy all-transform-data-ptr (unsafe-fx* vertex-num affine-data-size)
                    tmp-transform-data-ptr (unsafe-fx* vertex-count affine-data-size))
            (unsafe-fx+ vertex-num vertex-count)]
           [else
            vertex-num]))
       ;; Unamp the transform buffer (i.e. send the transform data)
       (glUnmapBuffer GL_ARRAY_BUFFER))
     
     (glDrawArrays mode 0 vertex-count)]))

;; ===================================================================================================
;; Vertex drawing of variable-length primitives (e.g. line strips, triangle strips)

(: send-multi-vertices (-> gl-object 
                           vao-struct
                           Integer
                           (Vectorof draw-params)
                           Nonnegative-Fixnum
                           Nonnegative-Fixnum
                           Void))
(define (send-multi-vertices program struct mode ps start end)
  ;; Compute the total number of vertexes and primitives to send to the GPU
  (define-values (vertex-count prim-count)
    (for/fold ([vertex-count : Nonnegative-Fixnum  0]
               [prim-count : Nonnegative-Fixnum  0]
               ) ([i  (in-range start end)])
      (define v (shape-params-vertices (draw-params-shape-params (unsafe-vector-ref ps i))))
      (if (multi-vertices? v)
          (values
           (unsafe-fx+ vertex-count (vertices-vertex-count v))
           (unsafe-fx+ prim-count (min (vector-length (multi-vertices-starts v))
                                       (s32vector-length (multi-vertices-counts v)))))
          (values vertex-count prim-count))))
  
  (when (and (> vertex-count 0) (> prim-count 0))
    ;; Allocate space for starts and counts
    (define all-starts (get-all-starts prim-count))
    (define all-counts (get-all-counts prim-count))
    (define all-counts-ptr (s32vector->cpointer all-counts))
    ;; Copy starts (shifted) and counts
    (for/fold ([vertex-num : Nonnegative-Fixnum  0]
               [prim-num : Nonnegative-Fixnum  0]
               ) ([i  (in-range start end)])
      (define p (unsafe-vector-ref ps i))
      (define v (shape-params-vertices (draw-params-shape-params p)))
      (cond
        [(multi-vertices? v)
         (define vertex-count (vertices-vertex-count v))
         (define starts (multi-vertices-starts v))
         (define counts (multi-vertices-counts v))
         (define prim-count (min (vector-length starts) (s32vector-length counts)))
         ;; Copy the starts, shifted by the number of vertices
         (for ([j  (in-range prim-count)])
           (define start (unsafe-vector-ref starts j))
           (define shifted-j (unsafe-fx+ prim-num j))
           (define shifted-start (unsafe-fx+ vertex-num start))
           (s32vector-set! all-starts shifted-j shifted-start))
         ;; Copy the counts directly
         (memcpy all-counts-ptr
                 (unsafe-fx* prim-num 4)
                 (s32vector->cpointer counts)
                 (unsafe-fx* prim-count 4)
                 _byte)
         ;; Increment the counters
         (values (unsafe-fx+ vertex-num vertex-count)
                 (unsafe-fx+ prim-num prim-count))]
        [else
         (values vertex-num prim-num)]))
    
    ;; Get (and cache) a VAO with a big enough buffer
    (match-define (vertex-buffer vao vbuf tbuf _) (get-vertex-buffer program struct vertex-count))
    
    ;; With the vertex array object (not using with-gl-vertex-array for performance reasons)...
    (gl-bind-vertex-array vao)
    
    ;; Write the vertex data
    (when vbuf
      (gl-bind-array-buffer vbuf)
      ;; Map the vertex array into memory
      (define buffer-size (unsafe-fx* (vao-struct-size struct) vertex-count))
      (define all-vertex-data-ptr (gl-map-buffer-range/stream GL_ARRAY_BUFFER buffer-size))
      ;; Copy the vertex data into the buffer
      (define struct-size (vao-struct-size struct))
      (for/fold ([vertex-num : Nonnegative-Fixnum  0]) ([i  (in-range start end)])
        (define p (unsafe-vector-ref ps i))
        (define v (shape-params-vertices (draw-params-shape-params p)))
        (cond
          [(multi-vertices? v)
           (define vertex-count (vertices-vertex-count v))
           (define vertex-data (vertices-vertex-data v))
           ;; Copy the vertex data directly
           (memcpy all-vertex-data-ptr
                   (unsafe-fx* vertex-num struct-size)
                   (u8vector->cpointer vertex-data)
                   (unsafe-fx* vertex-count struct-size)
                   _byte)
           (unsafe-fx+ vertex-num vertex-count)]
          [else
           vertex-num]))
      ;; Unmap the vertex buffer (i.e. send the vertex data) and draw
      (glUnmapBuffer GL_ARRAY_BUFFER))
    
    (glMultiDrawArrays mode all-starts all-counts prim-count)))

;; ===================================================================================================
;; Sending uniforms

(: send-uniforms (-> gl-object (List-Hash String (U Symbol Uniform)) (HashTable Symbol Uniform) Void))
(define (send-uniforms program uniforms standard-uniforms)
  (for ([nu  (in-list uniforms)])
    (define name (car nu))
    (define uniform (cdr nu))
    (let ([uniform  (if (symbol? uniform) (hash-ref standard-uniforms uniform #f) uniform)])
      (when uniform
        (gl-program-uniform program name uniform)))))

;; ===================================================================================================
;; Drawing pass loop

(define get-swap-params
  (make-cached-vector
   'get-swap-params
   (λ ([n : Integer])
     ((inst make-vector draw-params) n empty-draw-params))
   vector-length))

(: send-draw-params (-> (Vectorof draw-params)
                        Nonnegative-Fixnum
                        Nonnegative-Fixnum
                        (HashTable Symbol Uniform)
                        Void))
(define (send-draw-params ps start end standard-uniforms)
  ;; For each program...
  (for ([s  (in-list (group-by-key! ps get-swap-params start end
                                    (λ ([ts : draw-params])
                                      (shape-params-program-spec
                                       (draw-params-shape-params ts)))))])
    (define pd ((span-key s)))
    (define program (program-spec-program pd))
    (define uniforms (program-spec-uniforms pd))
    (define struct (program-spec-struct pd))
    (with-gl-program program
      (send-uniforms program uniforms standard-uniforms)
      ;; For each set of shape uniforms...
      (for ([s  (in-list (group-by-key! ps get-swap-params (span-start s) (span-end s)
                                        (λ ([ts : draw-params])
                                          (shape-params-uniforms (draw-params-shape-params ts)))))])
        (send-uniforms program (span-key s) standard-uniforms)
        ;; For each kind of face...
        (for ([s  (in-list ((inst group-by-key! draw-params Face)
                            ps get-swap-params (span-start s) (span-end s)
                            (λ ([ts : draw-params])
                              (define consistent? (affine-consistent? (draw-params-transform ts)))
                              (define face (shape-params-face (draw-params-shape-params ts)))
                              (if consistent? face (opposite-gl-face face)))))])
          (gl-draw-face (span-key s))
          ;; For each drawing mode...
          (for ([s  (in-list (group-by-key! ps get-swap-params (span-start s) (span-end s)
                                            (λ ([ts : draw-params])
                                              (shape-params-mode (draw-params-shape-params ts)))))])
            (define mode (span-key s))
            (define start (span-start s))
            (define end (span-end s))
            (send-single-vertices program struct mode ps start end)
            (send-multi-vertices  program struct mode ps start end))))))
  (gl-bind-array-buffer null-gl-object)
  (gl-bind-vertex-array null-gl-object)
  (gl-draw-face 'front))

;; ===================================================================================================
;; Pass extraction

(define get-all-params
  (make-cached-vector
   'get-all-params
   (λ ([n : Integer])
     ((inst make-vector draw-params) n empty-draw-params))
   vector-length))

(: draw-pass (-> Index (Vectorof draw-passes) (HashTable Symbol Uniform) Void))
(define (draw-pass pass passess standard-uniforms)
  ;(printf "drawing pass ~v~n" pass)
  (define len
    (for/fold ([len : Nonnegative-Fixnum  0]) ([passes  (in-vector passess)])
      (let ([passes  (draw-passes-passes passes)])
        (cond
          [(< pass (vector-length passes))
           (unsafe-fx+ len (vector-length (unsafe-vector-ref passes pass)))]
          [else  len]))))
  
  (define params (get-all-params len))
  
  (define n
    (for/fold ([n : Nonnegative-Fixnum  0]) ([passes  (in-vector passess)])
      (let ([t  (draw-passes-transform passes)]
            [passes  (draw-passes-passes passes)])
        (cond
          [(< pass (vector-length passes))
           (define ps (unsafe-vector-ref passes pass))
           (for ([i  (in-range (vector-length ps))])
             (unsafe-vector-set! params
                                 (+ n i)
                                 (draw-params (unsafe-vector-ref ps i) t)))
           (unsafe-fx+ n (vector-length ps))]
          [else  n]))))
  
  (unless (= n len)
    (error 'draw-pass "copy error: wrote ~a but len = ~a" n len))
  
  (send-draw-params params 0 len standard-uniforms))
