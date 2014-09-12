#lang typed/racket/base

(require racket/match
         racket/list
         racket/sequence
         racket/unsafe/ops
         typed/racket/gui
         typed/racket/class
         math/flonum
         typed/opengl
         (except-in typed/opengl/ffi register-finalizer cast ->))

(require/typed
 racket/base
 [call-with-semaphore  (All (A) (-> Semaphore (-> A) A))])

(provide (all-defined-out))

;; ===================================================================================================
;; Managed contexts

(struct gl-context ([number : Natural]) #:transparent)

(: gl-contexts (HashTable gl-context (Instance GL-Context<%>)))
(define gl-contexts (make-weak-hasheq))

(: managed-gl-context (-> (Instance GL-Context<%>) gl-context))
(define managed-gl-context
  (let ([next-number : Natural  0])
    (λ (ctxt)
      (define number next-number)
      (set! next-number (+ 1 number))
      (define key (gl-context number))
      (hash-set! gl-contexts key ctxt)
      key)))

(: get-gl-context (-> gl-context (Instance GL-Context<%>)))
(define (get-gl-context key)
  (hash-ref gl-contexts key (λ () (raise-argument-error 'get-gl-context "valid gl-context" key))))

(: current-gl-context (Parameterof (U #f gl-context)))
(define current-gl-context (make-parameter #f))

(: call-with-gl-context (-> gl-context (-> Any) Any))
(define (call-with-gl-context new-ctxt thunk)
  (define ctxt (current-gl-context))
  (cond [(not ctxt)
         (send (get-gl-context new-ctxt) call-as-current
               (λ () (parameterize ([current-gl-context  new-ctxt]) (thunk))))]
        [(eq? new-ctxt ctxt)
         (send (get-gl-context new-ctxt) call-as-current thunk)]
        [else
         (error 'call-with-gl-context "already in another managed OpenGL context")]))

(define-syntax-rule (with-gl-context new-ctxt-stx body ...)
  (call-with-gl-context new-ctxt-stx (λ () body ...)))

(: check-current-gl-context (-> Symbol gl-context))
(define (check-current-gl-context name)
  (define ctxt (current-gl-context))
  (if ctxt ctxt (error name "not in a managed OpenGL context (use with-gl-context)")))

(: gl-swap-buffers (-> Void))
(define (gl-swap-buffers)
  (define ctxt (check-current-gl-context 'gl-swap-buffers))
  (send (get-gl-context ctxt) swap-buffers))

(define-syntax-rule (call-with-gl-state body-thunk param obj set-state!)
  (let ()
    (check-current-gl-context 'call-with-gl-state)
    (define old (param))
    (cond [(eq? old obj)  (body-thunk)]
          [else  (set-state! obj)
                 (begin0
                   (parameterize ([param obj]) (body-thunk))
                   (set-state! old))])))

;; ===================================================================================================
;; Master GL context

(define master-gl-context-mutex (make-semaphore 1))

(: master-frame (U #f (Instance Frame%)))
(define master-frame #f)

(: master-context (U #f gl-context))
(define master-context #f)

(: get-master-gl-context (-> gl-context))
(define (get-master-gl-context)
  (call-with-semaphore
   master-gl-context-mutex
   (λ ()
     (define ctxt master-context)
     (cond [ctxt  ctxt]
           [else
            (define config (new gl-config%))
            (send config set-legacy? #f)
            (define frame (new frame% [label "Master GL context frame"] [width 512] [height 512]))
            (define canvas (new canvas% [parent frame] [style '(gl no-autoclear)] [gl-config config]))
            (send frame show #t)
            (send frame show #f)
            (sleep/yield 1)
            (define ctxt (send (send canvas get-dc) get-gl-context))
            (cond [(and ctxt (send ctxt ok?))
                   (set! master-frame frame)
                   (let ([ctxt  (managed-gl-context ctxt)])
                     (set! master-context ctxt)
                     ctxt)]
                  [else
                   (error 'get-master-gl-context "can't get a GL context")])]))))

;; ===================================================================================================
;; Managed objects

;(struct gl-object ([tag : Symbol] [handle : Natural]) #:transparent)  ; won't work!
(define-type gl-object (Pair Symbol Natural))
;; We have to abuse pairs (can't use a struct) because we need to guarantee that the object won't be
;; wrapped by an impersonator when going through the typed-untyped contract boundary
;; If an object is wrapped going into `register-finalizer`, the finalizer is put on the *wrapper*,
;; whose short life causes the OS to run the finalizer almost immediately

(define null-gl-object (cons 'null 0))

(: gl-object-contexts (HashTable gl-object gl-context))
(define gl-object-contexts (make-weak-hasheq))

(: gl-object-context (-> gl-object gl-context))
(define (gl-object-context obj)
  (hash-ref gl-object-contexts obj
            (λ () (raise-argument-error 'get-gl-object-context "valid gl-object" obj))))

(define gl-object-tag (λ ([obj : gl-object]) (car obj)))
(define gl-object-handle (λ ([obj : gl-object]) (cdr obj)))

(require/typed
 ffi/unsafe
 [register-finalizer  (-> gl-object (-> gl-object Any) Any)])

(: num-gl-objects-collected Natural)
(define num-gl-objects-collected 0)

(define get-num-gl-objects-collected (λ () num-gl-objects-collected))

(: make-gl-object (-> Symbol Natural (-> Natural Any) gl-object))
(define (make-gl-object tag handle delete)
  (define obj (cons tag handle))
  (define ctxt (check-current-gl-context 'make-gl-object))
  (hash-set! gl-object-contexts obj ctxt)
  (register-finalizer
   obj
   (λ ([obj : gl-object])
     (set! num-gl-objects-collected (+ 1 num-gl-objects-collected))
     (define ctxt (with-handlers ([exn?  (λ (e) #f)])
                    (gl-object-context obj)))
     (when (and ctxt (send (get-gl-context ctxt) ok?))
       (with-gl-context ctxt
         (delete (gl-object-handle obj))))))
  obj)

(define-syntax-rule (call-with-gl-object body-thunk param obj bind)
  (call-with-gl-state body-thunk param obj (λ (v) (bind (gl-object-handle v)))))

;; ===================================================================================================
;; Managed buffers

(define (make-gl-buffer)
  (make-gl-object 'buffer
                  (u32vector-ref (glGenBuffers 1) 0)
                  (λ ([handle : Natural]) (glDeleteBuffers 1 (u32vector handle)))))

(: current-gl-array-buffer (Parameterof gl-object))
(define current-gl-array-buffer (make-parameter null-gl-object))

(define-syntax-rule (with-gl-array-buffer obj-stx body ...)
  (call-with-gl-object (λ () body ...)
                       current-gl-array-buffer
                       obj-stx
                       (λ ([handle : Natural]) (glBindBuffer GL_ARRAY_BUFFER handle))))

(: gl-bind-array-buffer (-> gl-object Void))
(define (gl-bind-array-buffer buf)
  (unless (eq? buf (current-gl-array-buffer))
    (glBindBuffer GL_ARRAY_BUFFER (gl-object-handle buf))
    (current-gl-array-buffer buf)))

(: current-gl-element-array-buffer (Parameterof gl-object))
(define current-gl-element-array-buffer (make-parameter null-gl-object))

(define-syntax-rule (with-gl-element-array-buffer obj-stx body ...)
  (call-with-gl-object (λ () body ...)
                       current-gl-element-array-buffer
                       obj-stx
                       (λ ([handle : Natural]) (glBindBuffer GL_ELEMENT_ARRAY_BUFFER handle))))

;; ===================================================================================================
;; Managed vertex arrays

(define (make-gl-vertex-array)
  (make-gl-object 'vertex-array
                  (u32vector-ref (glGenVertexArrays 1) 0)
                  (λ (vao) (glDeleteVertexArrays 1 (u32vector vao)))))

(: current-gl-vertex-array (Parameterof gl-object))
(define current-gl-vertex-array (make-parameter null-gl-object))

(define-syntax-rule (with-gl-vertex-array obj-stx body ...)
  (call-with-gl-object (λ () body ...)
                       current-gl-vertex-array
                       obj-stx
                       glBindVertexArray))

(: gl-bind-vertex-array (-> gl-object Void))
(define (gl-bind-vertex-array vao)
  (unless (eq? vao (current-gl-vertex-array))
    (glBindVertexArray (gl-object-handle vao))
    (current-gl-vertex-array vao)))

;; ===================================================================================================
;; Managed textures

(define (make-gl-texture-object)
  (make-gl-object 'texture
                  (u32vector-ref (glGenTextures 1) 0)
                  (λ ([handle : Natural]) (glDeleteTextures 1 (u32vector handle)))))

(: current-gl-texture-object (Parameterof gl-object))
(define current-gl-texture-object (make-parameter null-gl-object))

(define-syntax-rule (with-gl-texture-object kind obj-stx body ...)
  (call-with-gl-object (λ () body ...)
                       current-gl-texture-object
                       obj-stx
                       (λ ([handle : Natural]) (glBindTexture kind handle))))

(struct gl-texture-2d ([object : gl-object]
                       [width : Natural]
                       [height : Natural]
                       [internal-format : Integer]
                       [format : Integer]
                       [type : Integer]
                       [params : (HashTable Integer Integer)])
  #:transparent)

(: make-gl-texture-2d (-> Integer Integer Integer Integer Integer (Listof (Pair Integer Integer))
                          gl-texture-2d))
(define (make-gl-texture-2d width height internal-format format type params)
  (cond [(negative? width)
         (raise-argument-error 'make-gl-texture-2d "Natural" 0
                               width height internal-format format type params)]
        [(negative? height)
         (raise-argument-error 'make-gl-texture-2d "Natural" 1
                               width height internal-format format type params)]
        [else
         (define obj (make-gl-texture-object))
         
         (with-gl-texture-object GL_TEXTURE_2D obj
           (for ([param  (in-list params)])
             (match-define (cons key value) param)
             (glTexParameteri GL_TEXTURE_2D key value))
           (glTexImage2D GL_TEXTURE_2D 0 internal-format width height 0 format type 0))
         
         (gl-texture-2d obj width height internal-format format type
                        (make-hasheqv params))]))

(: current-gl-texture-2d (Parameterof (U #f gl-texture-2d)))
(define current-gl-texture-2d (make-parameter #f))

(define-syntax-rule (with-gl-texture-2d obj-stx body ...)
  (let ([obj obj-stx])
    (parameterize ([current-gl-texture-2d  obj])
      (with-gl-texture-object GL_TEXTURE_2D (gl-texture-2d-object obj)
        body ...))))

;; ===================================================================================================
;; Managed render buffers

(define (make-gl-renderbuffer-object)
  (make-gl-object 'renderbuffer
                  (u32vector-ref (glGenRenderbuffers 1) 0)
                  (λ ([handle : Natural]) (glDeleteRenderbuffers 1 (u32vector handle)))))

(: current-gl-renderbuffer-object (Parameterof gl-object))
(define current-gl-renderbuffer-object (make-parameter null-gl-object))

(define-syntax-rule (with-gl-renderbuffer-object obj-stx body ...)
  (call-with-gl-object (λ () body ...)
                       current-gl-renderbuffer-object
                       obj-stx
                       (λ ([handle : Natural]) (glBindRenderbuffer GL_RENDERBUFFER handle))))

(struct gl-renderbuffer ([object : gl-object]
                         [width : Natural]
                         [height : Natural]
                         [internal-format : Integer])
  #:transparent)

(: make-gl-renderbuffer (-> Integer Integer Integer gl-renderbuffer))
(define (make-gl-renderbuffer width height internal-format)
  (cond [(negative? width)
         (raise-argument-error 'make-gl-renderbuffer "Natural" 0 width height internal-format)]
        [(negative? height)
         (raise-argument-error 'make-gl-renderbuffer "Natural" 1 width height internal-format)]
        [else
         (define obj (make-gl-renderbuffer-object))
         (with-gl-renderbuffer-object obj
           (glRenderbufferStorage GL_RENDERBUFFER internal-format width height))
         (gl-renderbuffer obj width height internal-format)]))

;; ===================================================================================================
;; Managed framebuffer objects

(define (make-gl-framebuffer-object)
  (make-gl-object 'framebuffer
                  (u32vector-ref (glGenFramebuffers 1) 0)
                  (λ ([handle : Natural]) (glDeleteFramebuffers 1 (u32vector handle)))))

(: current-gl-framebuffer-object (Parameterof gl-object))
(define current-gl-framebuffer-object (make-parameter null-gl-object))

(define-syntax-rule (with-gl-framebuffer-object obj-stx body ...)
  (call-with-gl-object (λ () body ...)
                       current-gl-framebuffer-object
                       obj-stx
                       (λ ([handle : Natural]) (glBindFramebuffer GL_FRAMEBUFFER handle))))

(: gl-framebuffer-status-message (-> Integer String))
(define (gl-framebuffer-status-message code)
  (cond
    [(eq? code GL_FRAMEBUFFER_COMPLETE)
     "The framebuffer is complete."]
    [(eq? code GL_FRAMEBUFFER_UNDEFINED)
     "The target is the default framebuffer, but the default framebuffer does not exist."]
    [(eq? code GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT)
     "One of the framebuffer attachment points is incomplete."]
    [(eq? code GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT)
     "The framebuffer does not have at least one image attached to it."]
    [(eq? code GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER)
     "The value of GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is GL_NONE for a color attachment point \
named by GL_DRAW_BUFFERi."]
    [(eq? code GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER)
     "GL_READ_BUFFER is not GL_NONE and the value of GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is \
GL_NONE for the color attachment point named by GL_READ_BUFFER."]
    [(eq? code GL_FRAMEBUFFER_UNSUPPORTED)
     "The combination of internal formats of the attached images violates an implementation-\
dependent set of restrictions."]
    [(eq? code GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE)
     "One of the following occurred:
 * The value of GL_RENDERBUFFER_SAMPLES is not the same for all attached renderbuffers.
 * The value of GL_TEXTURE_SAMPLES is the not same for all attached textures.
 * The value of GL_TEXTURE_FIXED_SAMPLE_LOCATIONS is not the same for all attached textures.
 * The attached images are a mix of renderbuffers and textures, and the value of \
GL_RENDERBUFFER_SAMPLES does not match the value of GL_TEXTURE_SAMPLES.
 * The attached images are a mix of renderbuffers and textures, and the value of \
GL_TEXTURE_FIXED_SAMPLE_LOCATIONS is not GL_TRUE for all attached textures."]
    [(eq? code GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS)
     "One of the following occurred:
 * A framebuffer attachment is layered and a populated attachment is not layered.
 * All populated color attachments are not from textures of the same target."]
    [else
     "Unknown error."]))

(: check-gl-framebuffer-status (-> Void))
(define (check-gl-framebuffer-status)
  (define code (glCheckFramebufferStatus GL_FRAMEBUFFER))
  (unless (equal? code GL_FRAMEBUFFER_COMPLETE)
    (error 'check-gl-framebuffer-status
           (gl-framebuffer-status-message code))))

(struct gl-framebuffer ([object : gl-object]
                        [width : Natural]
                        [height : Natural]
                        [attachments : (HashTable Integer (U gl-texture-2d gl-renderbuffer))])
  #:transparent)

(: make-gl-framebuffer (-> Integer Integer (Listof (Pair Integer (U gl-texture-2d gl-renderbuffer)))
                           gl-framebuffer))
(define (make-gl-framebuffer width height attachments)
  (cond [(negative? width)
         (raise-argument-error 'make-gl-framebuffer "Natural" 0 width height attachments)]
        [(negative? height)
         (raise-argument-error 'make-gl-framebuffer "Natural" 1 width height attachments)]
        [else
         (define fbo (make-gl-framebuffer-object))
         
         (with-gl-framebuffer-object fbo
           (for ([att  (in-list attachments)])
             (match-define (cons attachment obj) att)
             (cond [(gl-texture-2d? obj)
                    (glFramebufferTexture2D GL_FRAMEBUFFER
                                            attachment
                                            GL_TEXTURE_2D
                                            (gl-object-handle (gl-texture-2d-object obj))
                                            0)]
                   [(gl-renderbuffer? obj)
                    (glFramebufferRenderbuffer GL_FRAMEBUFFER
                                               attachment
                                               GL_RENDERBUFFER
                                               (gl-object-handle (gl-renderbuffer-object obj)))]
                   [else
                    (error 'make-gl-framebuffer "unknown attachment target ~e" att)]))
           (check-gl-framebuffer-status))
         
         (gl-framebuffer fbo width height (make-hasheqv attachments))]))

(: current-gl-framebuffer (Parameterof (U #f gl-framebuffer)))
(define current-gl-framebuffer (make-parameter #f))

(define-syntax-rule (with-gl-framebuffer obj-stx body ...)
  (let ([obj obj-stx])
    (parameterize ([current-gl-framebuffer  obj])
      (with-gl-framebuffer-object (gl-framebuffer-object obj)
        body ...))))

(: gl-framebuffer-attachment (-> gl-framebuffer Integer (U gl-texture-2d gl-renderbuffer)))
(define (gl-framebuffer-attachment obj attachment)
  (hash-ref (gl-framebuffer-attachments obj) attachment
            (λ () (error 'gl-framebuffer-attachment "for framebuffer ~e, no attachment ~e"
                         obj attachment))))

(: gl-framebuffer-texture-2d (-> gl-framebuffer Integer gl-texture-2d))
(define (gl-framebuffer-texture-2d obj attachment)
  (define a (gl-framebuffer-attachment obj attachment))
  (cond [(gl-texture-2d? a)  a]
        [else  (raise-result-error 'gl-framebuffer-texture-2d "gl-texture-2d" a)]))

(: gl-framebuffer-renderbuffer (-> gl-framebuffer Integer gl-renderbuffer))
(define (gl-framebuffer-renderbuffer obj attachment)
  (define a (gl-framebuffer-attachment obj attachment))
  (cond [(gl-renderbuffer? a)  a]
        [else  (raise-result-error 'gl-framebuffer-renderbuffer "gl-renderbuffer" a)]))

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
                   [normalized? : Boolean]
                   [size : Index])
  #:transparent)

(struct vao-struct ([fields : (Listof vao-field)]
                    [size : Index])
  #:transparent)

(: make-vao-field (->* [String Index Integer] [Boolean] vao-field))
(define (make-vao-field name count type [normalized? #t])
  (vao-field name count type normalized? (assert (* count (gl-type->size type)) index?)))

(: make-vao-struct (-> vao-field * vao-struct))
(define (make-vao-struct . fs)
  (vao-struct fs (assert (apply + (map vao-field-size fs)) index?)))

(: vao-struct-bind-attributes (-> vao-struct Void))
(define (vao-struct-bind-attributes struct)
  (match-define (vao-struct fields struct-size) struct)
  (for/fold ([start : Nonnegative-Fixnum  0]) ([field  (in-list fields)]
                                               [index : Natural  (in-naturals 0)])
    (match-define (vao-field name field-count type normalized? field-size) field)
    (glEnableVertexAttribArray index)
    (glVertexAttribPointer index field-count type normalized? struct-size start)
    (unsafe-fx+ start field-size))
  (void))

;; ===================================================================================================
;; Managed shaders and programs

(: make-gl-shader (-> Integer String gl-object))
(define (make-gl-shader type code)
  (check-current-gl-context 'make-gl-shader)
  (define handle (glCreateShader type))
  (glShaderSource handle 1 (vector code) (s32vector -1))
  
  (glCompileShader handle)
  (define status (glGetShaderiv handle GL_COMPILE_STATUS))
  (when (= status GL_FALSE)
    (define len (glGetShaderiv handle GL_INFO_LOG_LENGTH))
    (define-values (_ bs) (glGetShaderInfoLog handle len))
    (error 'make-gl-shader "~a in\n~a" bs code))
  
  (make-gl-object 'shader
                  handle
                  (λ ([handle : Natural]) (glDeleteShader handle))))

(: make-gl-program (-> vao-struct (Listof gl-object) gl-object))
(define (make-gl-program struct shaders)
  (check-current-gl-context 'make-gl-program)
  
  (define handle (glCreateProgram))
  (for ([shader  (in-list shaders)])
    (glAttachShader handle (gl-object-handle shader)))
  
  (for ([field  (in-list (vao-struct-fields struct))]
        [index : Natural  (in-naturals 0)])
    (glBindAttribLocation handle index (vao-field-name field)))
  
  (glLinkProgram handle)
  (define status (glGetProgramiv handle GL_LINK_STATUS))
  (when (= status GL_FALSE)
    (define len (glGetProgramiv handle GL_INFO_LOG_LENGTH))
    (define-values (_ bs) (glGetProgramInfoLog handle len))
    (error 'gl-create-program (bytes->string/utf-8 bs)))
  
  (for ([shader  (in-list shaders)])
    (glDetachShader handle (gl-object-handle shader)))
  
  (make-gl-object 'program
                  handle
                  (λ ([handle : Natural]) (glDeleteProgram handle))))

(: current-gl-program (Parameterof gl-object))
(define current-gl-program (make-parameter null-gl-object))

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

(: gl-program-uniform (-> gl-object String Uniform Void))
(define (gl-program-uniform prog name u)
  (gl-uniform (glGetUniformLocation (gl-object-handle prog) name) u))

;; ===================================================================================================
;; Misc. convenience

(: gl-fullscreen-quad (->* [] [Flonum Flonum] Void))
(define (gl-fullscreen-quad [w 1.0] [h 1.0])
  (glBegin GL_TRIANGLE_STRIP)
  (glTexCoord2f 0.0 0.0) (glVertex2f -1.0 -1.0)
  (glTexCoord2f  w  0.0) (glVertex2f +1.0 -1.0)
  (glTexCoord2f 0.0  h ) (glVertex2f -1.0 +1.0)
  (glTexCoord2f  w   h ) (glVertex2f +1.0 +1.0)
  (glEnd))

;; ===================================================================================================

(define-type Face (U 'neither 'front 'back 'both))

(: opposite-gl-face (-> Face Face))
(define (opposite-gl-face f)
  (case f
    [(neither)  'neither]
    [(front)  'back]
    [(back)  'front]
    [else  'both]))

(: gl-set-draw-face (-> Face Void))
(define (gl-set-draw-face f)
  (case f
    [(neither)  (glEnable GL_CULL_FACE)
                (glCullFace GL_FRONT_AND_BACK)]
    [(front)  (glEnable GL_CULL_FACE)
              (glCullFace GL_BACK)]
    [(back)  (glEnable GL_CULL_FACE)
             (glCullFace GL_FRONT)]
    [else  (glDisable GL_CULL_FACE)]))

(: current-gl-draw-face (Parameterof Face))
(define current-gl-draw-face (make-parameter 'both))

(define-syntax-rule (with-gl-draw-face face-stx body ...)
  (let ([body-thunk  (λ () body ...)]
        [face : Face  face-stx])
    (call-with-gl-state body-thunk current-gl-draw-face face gl-set-draw-face)))

(: gl-draw-face (-> Face Void))
(define (gl-draw-face face)
  (unless (eq? face (current-gl-draw-face))
    (gl-set-draw-face face)
    (current-gl-draw-face face)))
