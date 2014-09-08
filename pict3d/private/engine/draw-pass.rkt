#lang typed/racket/base

(require racket/match
         racket/list
         racket/unsafe/ops
         racket/vector
         typed/opengl
         (except-in typed/opengl/ffi cast ->)
         "../math/flt3.rkt"
         "gl.rkt"
         "types.rkt"
         "utils.rkt")

(provide (struct-out vertices)
         (struct-out single-vertices)
         (struct-out multi-vertices)
         Vertices
         (struct-out shape-params)
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

(struct draw-params ([transform : FlAffine3-]
                     [inverse : FlAffine3-]
                     [shape-params : shape-params])
  #:transparent)

(define-type Passes (Vectorof (Vectorof shape-params)))

(struct draw-passes ([transform : FlAffine3-]
                     [inverse : FlAffine3-]
                     [passes : Passes])
  #:transparent)

(define empty-shape-params
  (shape-params (λ () (error 'empty-shape-params)) empty 'both 0 (single-vertices 0 #"")))

(: empty-draw-params draw-params)
(define empty-draw-params (draw-params identity-flt3 identity-flt3 empty-shape-params))

;; ===================================================================================================

(struct vertex-buffer ([vao : gl-object] [buf : gl-object] [length : Index]))

(: make-vertex-buffer (-> Integer vertex-buffer))
(define (make-vertex-buffer size)
  (cond
    [(index? size)
     (define vao (make-gl-vertex-array))
     (define buf (make-gl-buffer))
     (with-gl-array-buffer buf
       (glBufferData GL_ARRAY_BUFFER size 0 GL_STREAM_DRAW))
     (vertex-buffer vao buf size)]
    [else
     (raise-argument-error 'make-vertex-buffer "Index" size)]))

(define get-all-vertex-buffer
  (make-cached-vector 'get-all-vertex-buffer make-vertex-buffer vertex-buffer-length))

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
     ;; Doesn't seem to help much anyway
     ;GL_MAP_UNSYNCHRONIZED_BIT
     ))
   cpointer?))

;; ===================================================================================================
;; Vertex drawing of fixed-length primitives (e.g. points, lines, triangles)

(: send-single-vertices (-> vao-struct
                            Integer
                            (Vectorof draw-params)
                            Nonnegative-Fixnum
                            Nonnegative-Fixnum
                            Void))
(define (send-single-vertices struct mode ps start end)
  ;; Compute the total number of single vertices to send to the GPU
  (define vertex-count
    (for/fold ([vertex-count : Nonnegative-Fixnum  0]) ([i  (in-range start end)])
      (define v (shape-params-vertices (draw-params-shape-params (unsafe-vector-ref ps i))))
      (if (single-vertices? v)
          (unsafe-fx+ vertex-count (vertices-vertex-count v))
          vertex-count)))
  
  (when (> vertex-count 0)
    (define struct-size (vao-struct-size struct))
    (define buffer-size (unsafe-fx* vertex-count struct-size))
    ;; Get a big enough VAO and vertex buffer
    (match-define (vertex-buffer vao buf _) (get-all-vertex-buffer buffer-size))
    ;; With both...
    (with-gl-vertex-array vao
      (with-gl-array-buffer buf
        ;; Bind the attributes
        (vao-struct-bind-attributes struct)
        ;; Map the vertex buffer into memory
        (define all-vertex-data-ptr (gl-map-buffer-range/stream GL_ARRAY_BUFFER buffer-size))
        ;; Copy the vertex data into the buffer
        (for/fold ([vertex-num : Nonnegative-Fixnum  0]) ([i  (in-range start end)])
          (define v (shape-params-vertices (draw-params-shape-params (unsafe-vector-ref ps i))))
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
        ;; Unamp the vertex buffer (i.e. send the vertex data) and draw
        (glUnmapBuffer GL_ARRAY_BUFFER)
        (glDrawArrays mode 0 vertex-count)))))

;; ===================================================================================================
;; Vertex drawing of variable-length primitives (e.g. line strips, triangle strips)

(: send-multi-vertices (-> vao-struct
                           Integer
                           (Vectorof draw-params)
                           Nonnegative-Fixnum
                           Nonnegative-Fixnum
                           Void))
(define (send-multi-vertices struct mode ps start end)
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
    (define struct-size (vao-struct-size struct))
    (define buffer-size (unsafe-fx* vertex-count struct-size))
    ;; Get a big enough VAO and vertex buffer
    (match-define (vertex-buffer vao buf _) (get-all-vertex-buffer buffer-size))
    ;; With both...
    (with-gl-vertex-array vao
      (with-gl-array-buffer buf
        ;; Bind the attributes
        (vao-struct-bind-attributes struct)
        ;; Map the vertex array into memory
        (define all-vertex-data-ptr (gl-map-buffer-range/stream GL_ARRAY_BUFFER buffer-size))
        ;; Allocate space for starts and counts
        (define all-starts (get-all-starts prim-count))
        (define all-counts (get-all-counts prim-count))
        (define all-counts-ptr (s32vector->cpointer all-counts))
        ;; Copy the vertex data into the buffer; copy starts (shifted) and counts
        (for/fold ([vertex-num : Nonnegative-Fixnum  0]
                   [prim-num : Nonnegative-Fixnum  0]
                   ) ([i  (in-range start end)])
          (define v (shape-params-vertices (draw-params-shape-params (unsafe-vector-ref ps i))))
          (cond
            [(multi-vertices? v)
             (define vertex-count (vertices-vertex-count v))
             (define vertex-data (vertices-vertex-data v))
             (define starts (multi-vertices-starts v))
             (define counts (multi-vertices-counts v))
             (define prim-count (min (vector-length starts) (s32vector-length counts)))
             ;; Copy the vertex data directly
             (memcpy all-vertex-data-ptr
                     (unsafe-fx* vertex-num struct-size)
                     (u8vector->cpointer vertex-data)
                     (unsafe-fx* vertex-count struct-size)
                     _byte)
             ;; Copy the starts, shifted by the number of vertices already copied
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
        ;; Unmap the vertex buffer (i.e. send the vertex data) and draw
        (glUnmapBuffer GL_ARRAY_BUFFER)
        (glMultiDrawArrays mode all-starts all-counts prim-count)))))

;; ===================================================================================================
;; Drawing pass loop

(: do-uniforms (-> gl-object (List-Hash String (U Symbol Uniform)) (HashTable Symbol Uniform)
                   (List-Hash String (U Symbol Uniform))))
(define (do-uniforms program uniforms standard-uniforms)
  (for/fold ([leftovers : (List-Hash String (U Symbol Uniform))  empty]) ([nu  (in-list uniforms)])
    (define name (car nu))
    (define uniform (cdr nu))
    (let ([uniform  (if (symbol? uniform) (hash-ref standard-uniforms uniform #f) uniform)])
      (cond [uniform  (gl-program-uniform program name uniform)
                      leftovers]
            [else  (cons nu leftovers)]))))

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
  ;; For each program and shape params that use it...
  (for ([s  (in-list (group-by-key! ps get-swap-params start end
                                    (λ ([ts : draw-params])
                                      (shape-params-program-spec
                                       (draw-params-shape-params ts)))))])
    (define pd ((span-key s)))
    (define program (program-spec-program pd))
    (define struct (program-spec-struct pd))
    (with-gl-program program
      (define leftovers (do-uniforms program (program-spec-uniforms pd) standard-uniforms))
      (for ([s  (in-list (group-by-key! ps get-swap-params (span-start s) (span-end s)
                                        (λ ([ts : draw-params]) (draw-params-transform ts))))])
        ;; Determine whether we need to draw the opposite face
        (define consistent? (flt3consistent? (span-key s)))
        ;; Set the model matrix in the standard uniforms
        (define t (->flprojective3 (span-key s)))
        (define tinv (->flprojective3 (draw-params-inverse (vector-ref ps (span-start s)))))
        (define world->model (uniform-mat (flprojective3-entries t) 4))
        (define model->world (uniform-mat (flprojective3-entries tinv) 4))
        (let* ([standard-uniforms  (hash-set standard-uniforms 'model world->model)]
               [standard-uniforms  (hash-set standard-uniforms 'unmodel model->world)])
          (do-uniforms program leftovers standard-uniforms)
          ;; For each kind of face and shape params that use it...
          (for ([s  (in-list ((inst group-by-key! draw-params Face)
                              ps get-swap-params (span-start s) (span-end s)
                              (λ ([ts : draw-params])
                                (shape-params-face
                                 (draw-params-shape-params ts)))))])
            (define face (span-key s))
            (gl-draw-face (if consistent? face (opposite-gl-face face)))
            ;; For each set of shape uniforms and shape params that use it...
            (for ([s  (in-list (group-by-key! ps get-swap-params (span-start s) (span-end s)
                                              (λ ([ts : draw-params])
                                                (shape-params-uniforms
                                                 (draw-params-shape-params ts)))))])
              (do-uniforms program (span-key s) standard-uniforms)
              ;; For each mode and shape params that use it...
              (for ([s  (in-list (group-by-key! ps get-swap-params (span-start s) (span-end s)
                                                (λ ([ts : draw-params])
                                                  (shape-params-mode
                                                   (draw-params-shape-params ts)))))])
                (define mode (span-key s))
                (define start (span-start s))
                (define end (span-end s))
                (send-single-vertices struct mode ps start end)
                (send-multi-vertices  struct mode ps start end))))))))
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
      (match-let ([(draw-passes t tinv passes)  passes])
        (cond
          [(< pass (vector-length passes))
           (define ps (unsafe-vector-ref passes pass))
           (for ([i  (in-range (vector-length ps))])
             (vector-set! params (+ n i) (draw-params t tinv (vector-ref ps i))))
           (unsafe-fx+ n (vector-length ps))]
          [else  n]))))
  
  (unless (= n len)
    (error 'draw-pass "copy error: wrote ~a but len = ~a" n len))
  
  (send-draw-params params 0 len standard-uniforms))
