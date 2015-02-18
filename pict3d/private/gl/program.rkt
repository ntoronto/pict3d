#lang typed/racket/base

(require racket/match
         racket/list
         racket/sequence
         racket/unsafe/ops
         math/flonum
         typed/opengl
         (except-in typed/opengl/ffi cast ->)
         "../untyped-utils.rkt"
         "context.rkt"
         "object.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; GLSL version crap

(: glsl-version-string (-> String))
(define (glsl-version-string)
  (format "#version ~a" (gl-shading-language-version)))

;; ===================================================================================================
;; Vertex array object structs

(: gl-type-size-hash (HashTable Integer Index))
(define gl-type-size-hash
  (make-hasheq (list (cons GL_FLOAT 4)
                     (cons GL_UNSIGNED_BYTE 1)
                     ;; Doesn't work (no f16vector in Racket)
                     #;(cons GL_HALF_FLOAT 2))))

(: gl-type->size (-> Integer Index))
(define (gl-type->size t)
  (hash-ref gl-type-size-hash t (λ () (error 'gl-type->size "unknown GL type ~e" t))))

(struct vao-field ([name : String]
                   [count : Index]
                   [type : Integer]
                   [size : Index])
  #:transparent)

(struct vao-struct ([fields : (Listof vao-field)]
                    [size : Index])
  #:transparent)

(: make-vao-field (-> String Index Integer vao-field))
(define (make-vao-field name count type)
  (vao-field name count type (assert (* count (gl-type->size type)) index?)))

(: make-vao-struct (-> vao-field * vao-struct))
(define (make-vao-struct . fs)
  (vao-struct fs (assert (apply + (map vao-field-size fs)) index?)))

(: vao-struct-bind-attributes (-> vao-struct Void))
(define (vao-struct-bind-attributes struct)
  (match-define (vao-struct fields struct-size) struct)
  (for/fold ([start : Nonnegative-Fixnum  0]) ([field  (in-list fields)]
                                               [index : Natural  (in-naturals 0)])
    (match-define (vao-field name field-count type field-size) field)
    (glEnableVertexAttribArray index)
    (glVertexAttribPointer index field-count type #f struct-size start)
    (unsafe-fx+ start field-size))
  (void))

;; ===================================================================================================
;; Managed shaders and programs

(struct gl-shader gl-object ([type : Integer] [code : String]) #:transparent)
(struct gl-program gl-object ([name : String]
                              [standard-uniforms : (Listof (Pair String Symbol))]
                              [struct : vao-struct]
                              [output-names : (Listof String)]
                              [shaders : (Listof gl-shader)]
                              [uniform-locs : (HashTable String Integer)])
  #:transparent)

(: make-gl-shader (-> Integer String gl-shader))
(define (make-gl-shader type orig-code)
  (define code (string-append (glsl-version-string) "\n\n" orig-code))
  
  (get-current-managed-gl-context 'make-gl-shader)
  
  (define handle (glCreateShader type))
  (define shader (gl-shader handle type code))
  (manage-gl-object shader (λ ([handle : Natural]) (glDeleteShader handle)))
  
  (glShaderSource handle 1 (vector code) (s32vector -1))
  
  (glCompileShader handle)
  (define status (glGetShaderiv handle GL_COMPILE_STATUS))
  (when (= status GL_FALSE)
    (define len (glGetShaderiv handle GL_INFO_LOG_LENGTH))
    (define-values (_ bs) (glGetShaderInfoLog handle len))
    (error 'make-gl-shader "~a in\n~a" bs code))
  
  shader)

(: make-gl-program (-> String
                       (Listof (Pair String Symbol)) vao-struct (Listof String) (Listof gl-shader)
                       gl-program))
(define (make-gl-program name standard-uniforms struct output-names shaders)
  (get-current-managed-gl-context 'make-gl-program)
  
  (define handle (glCreateProgram))
  (define program (gl-program handle name standard-uniforms struct output-names shaders (make-hash)))
  (manage-gl-object program (λ ([handle : Natural]) (glDeleteProgram handle)))
  
  (for ([shader  (in-list shaders)])
    (glAttachShader handle (gl-object-handle shader)))
  
  (for ([field  (in-list (vao-struct-fields struct))]
        [index : Natural  (in-naturals 0)])
    (glBindAttribLocation handle index (vao-field-name field)))
  
  (for ([name  (in-list output-names)]
        [index : Natural  (in-naturals 0)])
    (glBindFragDataLocation handle index name))
  
  (glLinkProgram handle)
  (define status (glGetProgramiv handle GL_LINK_STATUS))
  (when (= status GL_FALSE)
    (define len (glGetProgramiv handle GL_INFO_LOG_LENGTH))
    (define-values (_ bs) (glGetProgramInfoLog handle len))
    (error 'gl-create-program (bytes->string/utf-8 bs)))
  
  (for ([shader  (in-list shaders)])
    (glDetachShader handle (gl-object-handle shader)))
  
  program)

(define null-gl-program (gl-program 0 "null" empty (make-vao-struct) empty empty (make-hash)))

(: current-gl-program (Parameterof gl-program))
(define current-gl-program (make-parameter null-gl-program))

(define-syntax-rule (with-gl-program obj-stx body ...)
  (call-with-gl-object (λ () body ...)
                       current-gl-program
                       obj-stx
                       glUseProgram))

;; ===================================================================================================
;; Uniforms

(struct uniform () #:transparent)
(struct uniform-floats uniform ([values : (Sequenceof (U Flonum FlVector))]
                                [sizes : Index])
  #:transparent)
(struct uniform-ints uniform ([values : (Sequenceof (U Integer (Listof Integer)))]
                              [sizes : Index])
  #:transparent)

(struct uniform-uints uniform ([values : (Sequenceof (U Natural (Listof Natural)))]
                               [sizes : Index])
  #:transparent)

(struct uniform-mats uniform ([values : F32Vector]
                              [sizes : (U Index (Pair Index Index))])
  #:transparent)


(define-type Uniform (U uniform-floats uniform-ints uniform-uints uniform-mats))

(: check-uniform-size (-> Symbol Integer Index))
(define (check-uniform-size name n)
  (cond [(index? n)  n]
        [else  (error name "expected an Index; given ~e" n)]))

(: uniform-float (->* [(U Flonum FlVector)] [Integer] uniform-floats))
(define (uniform-float x [n #f])
  (uniform-floats (list x) (cond [n  (check-uniform-size 'uniform-float n)]
                                 [(flonum? x)  1]
                                 [else  (flvector-length x)])))

(: uniform-int (->* [(U Integer (Listof Integer))] [Integer] uniform-ints))
(define (uniform-int x [n #f])
  (uniform-ints (list x) (cond [n  (check-uniform-size 'uniform-int n)]
                               [(integer? x)  1]
                               [else  (length x)])))

(: uniform-uint (->* [(U Integer (Listof Integer))] [Integer] uniform-uints))
(define (uniform-uint x [n #f])
  (uniform-uints (list (cast x (U Natural (Listof Natural))))
                 (cond [n  (check-uniform-size 'uniform-uint n)]
                       [(integer? x)  1]
                       [else  (length x)])))

(: mat-size->size (-> (U Index (Pair Index Index)) Natural))
(define (mat-size->size s)
  (cond [(pair? s)  (* (car s) (cdr s))]
        [else  (* s s)]))

(: uniform-mat (-> (U Flonum FlVector) (U Integer (Pair Integer Integer)) uniform-mats))
(define (uniform-mat m s)
  (cond [(index? s)
         (define n (assert (mat-size->size s) index?))
         (uniform-mats (take-mat m n) s)]
        [(and (pair? s) (index? (car s)) (index? (cdr s)))
         (define n (assert (mat-size->size s) index?))
         (uniform-mats (take-mat m n) s)]
        [else
         (error 'uniform-mat "expected (U Index (Pair Index Index)); given ~e" s)]))

(: uniform-affine (-> F32Vector uniform-mats))
(define (uniform-affine m)
  (cond [(= 12 (f32vector-length m))
         (uniform-mats m '(3 . 4))]
        [else
         (raise-argument-error 'uniform-affine "length-12 f32vector" m)]))

(: take-floats (-> (U Flonum FlVector) Index (Listof Flonum)))
(define (take-floats v n)
  (cond [(flonum? v)  (make-list n v)]
        [else
         (define m (flvector-length v))
         (cond [(<= n m)  (take (flvector->list v) n)]
               [else  (raise-argument-error 'take-floats
                                            (format "FlVector of length at least ~a" n)
                                            0 v n)])]))

(: take-ints (case-> (-> (U Integer (Listof Integer)) Index (Listof Integer))
                     (-> (U Natural (Listof Natural)) Index (Listof Natural))))
(define (take-ints v n)
  (cond [(integer? v)  (make-list n v)]
        [else
         (define m (length v))
         (cond [(<= n m)  (take v n)]
               [else  (raise-argument-error 'take-ints
                                            (format "List of length at least ~a" n)
                                            0 v n)])]))

(: take-mat (-> (U Flonum FlVector) Index F32Vector))
(define (take-mat v n)
  (define vs (make-f32vector n))
  (cond [(flonum? v)
         (for ([i  (in-range n)])
           (f32vector-set! vs i v))]
        [(<= n (flvector-length v))
         (for ([i  (in-range n)])
           (f32vector-set! vs i (flvector-ref v i)))]
        [else
         (raise-argument-error 'take-mat
                               (format "FlVector of length at least ~a" n)
                               0 v n)])
  vs)

(: gl-uniform-floats (-> Integer uniform-floats Void))
(define (gl-uniform-floats loc u)
  (match-define (uniform-floats (app sequence->list vs) n) u)
  (define xs (append* (map (λ ([v : (U Flonum FlVector)]) (take-floats v n)) vs)))
  (define glUniform
    (case n
      [(1)  glUniform1fv]
      [(2)  glUniform2fv]
      [(3)  glUniform3fv]
      [(4)  glUniform4fv]
      [else  (error 'gl-uniform-floats "expected size 1, 2, 3 or 4; given ~e" n)]))
  (glUniform loc (length vs) (list->f32vector xs)))

(: gl-uniform-ints (-> Integer uniform-ints Void))
(define (gl-uniform-ints loc u)
  (match-define (uniform-ints (app sequence->list vs) n) u)
  (define xs (append* (map (λ ([v : (U Integer (Listof Integer))]) (take-ints v n)) vs)))
  (define glUniform
    (case n
      [(1)  glUniform1iv]
      [(2)  glUniform2iv]
      [(3)  glUniform3iv]
      [(4)  glUniform4iv]
      [else  (error 'gl-uniform-ints "expected size 1, 2, 3 or 4; given ~e" n)]))
  (glUniform loc (length vs) (list->s32vector xs)))

(: gl-uniform-uints (-> Integer uniform-uints Void))
(define (gl-uniform-uints loc u)
  (match-define (uniform-uints (app sequence->list vs) n) u)
  (define xs (append* (map (λ ([v : (U Natural (Listof Natural))]) (take-ints v n)) vs)))
  (define glUniform
    (case n
      [(1)  glUniform1uiv]
      [(2)  glUniform2uiv]
      [(3)  glUniform3uiv]
      [(4)  glUniform4uiv]
      [else  (error 'gl-uniform-uints "expected size 1, 2, 3 or 4; given ~e" n)]))
  (glUniform loc (length vs) (list->u32vector xs)))

(: gl-uniform-mats (-> Integer uniform-mats Void))
(define (gl-uniform-mats loc u)
  (match-define (uniform-mats xs s) u)
  (define glUniform
    (case s
      [(2)  glUniformMatrix2fv]
      [(3)  glUniformMatrix3fv]
      [(4)  glUniformMatrix4fv]
      [((2 . 2))  glUniformMatrix2fv]
      [((2 . 3))  glUniformMatrix3x2fv]
      [((2 . 4))  glUniformMatrix4x2fv]
      [((3 . 2))  glUniformMatrix2x3fv]
      [((3 . 3))  glUniformMatrix3fv]
      [((3 . 4))  glUniformMatrix4x3fv]
      [((4 . 2))  glUniformMatrix2x4fv]
      [((4 . 3))  glUniformMatrix3x4fv]
      [((4 . 4))  glUniformMatrix4fv]
      [else  (error 'gl-uniform-mats "expected legal matrix size; given ~e" s)]))
  (glUniform loc 1 #t xs))

(: gl-uniform (-> Integer Uniform Void))
(define (gl-uniform loc u)
  (cond [(uniform-floats? u)  (gl-uniform-floats loc u)]
        [(uniform-ints? u)    (gl-uniform-ints loc u)]
        [(uniform-uints? u)   (gl-uniform-uints loc u)]
        [(uniform-mats? u)    (gl-uniform-mats loc u)]
        [else  (raise-argument-error 'gl-uniform "known uniform" 1 loc u)]))

(: gl-program-uniform-loc (-> gl-program String Integer))
(define (gl-program-uniform-loc prog name)
  (hash-ref! (gl-program-uniform-locs prog) name
             (λ ()
               (define loc (glGetUniformLocation (gl-object-handle prog) name))
               (log-pict3d-debug "<gl> ~a binds uniform ~v at location ~v"
                                 (gl-program-name prog) name loc)
               loc)))

(: gl-program-send-uniform (-> gl-program String Uniform Void))
(define (gl-program-send-uniform prog name u)
  (define loc (gl-program-uniform-loc prog name))
  (unless (negative? loc)
    (gl-uniform loc u)))

(: gl-program-send-uniforms (-> gl-program
                           (Listof (Pair String (U Symbol Uniform)))
                           (HashTable Symbol Uniform)
                           Void))
(define (gl-program-send-uniforms program uniforms standard-uniforms)
  (for ([nu  (in-list uniforms)])
    (define name (car nu))
    (define uniform (cdr nu))
    (let ([uniform  (if (symbol? uniform) (hash-ref standard-uniforms uniform #f) uniform)])
      (when uniform
        (gl-program-send-uniform program name uniform)))))
