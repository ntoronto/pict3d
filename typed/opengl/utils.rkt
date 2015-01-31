#lang typed/racket/base

(require racket/class
         racket/set
         racket/promise
         racket/list
         racket/string
         typed/racket/draw
         "typed.rkt"
         (except-in "ffi.rkt" -> cast))

(provide gl-get-string
         gl-version
         gl-shading-language-version
         gl-extensions
         gl-has-extension?
         gl-version-at-least?
         gl-core-profile?
         gl-type?
         gl-vector->type
         gl-vector->length
         gl-vector->type/cpointer
         gl-vector->type/cpointer/length
         gl-type->ctype
         gl-type-sizeof
         gl-type-alignof
         gl-vector-sizeof
         gl-vector-alignof
         bitmap->texture
         load-texture
         load-shader
         create-program
         )

(: split-spaces (-> String (Pairof String (Listof String))))
(define (split-spaces str)
  (regexp-split #px"\\s+" str))

(: gl-get-string (->* [Integer] [Natural] String))
(define (gl-get-string i [j #f])
  ;; When there's no GL context, glGetString returns NULL, and the FFI returns #f to Racket,
  ;; which causes glGetString to break its own return contract
  ;; This is a stupid way to handle this error case, but there aren't many options
  (with-handlers ([exn:fail:contract?  (λ (e) (error 'gl-get-string "not in a GL context"))])
    (cond [j     (glGetStringi i j)]
          [else  (glGetString i)])))

(: gl-version-hash (HashTable GL-Context<%> Natural))
(define gl-version-hash (make-weak-hasheq))

(: parse-version-string (-> String Natural))
(define (parse-version-string str)
  (define ns
    (map (λ ([s : String])
           (define n (string->number s))
           (cond [(exact-nonnegative-integer? n)  n]
                 [else  (error 'gl-version "bad version string: ~v~n" str)]))
         (regexp-split #px"\\." (car (split-spaces str)))))
  (+ (* (first ns) 10) (second ns)))

(: gl-version (-> Natural))
(define (gl-version)
  (define ctxt (get-current-gl-context))
  (if ctxt
      (hash-ref!
       gl-version-hash
       ctxt
       (λ ()
         (parse-version-string (gl-get-string GL_VERSION))))
      (error 'gl-version "not in a GL context")))

(: gl-shading-language-version-hash (HashTable GL-Context<%> Natural))
(define gl-shading-language-version-hash (make-weak-hasheq))

(: parse-shading-language-version-string (-> String Natural))
(define (parse-shading-language-version-string str)
  (define ns
  (map (λ ([s : String])
         (define n (string->number s))
         (cond [(exact-nonnegative-integer? n)  n]
               [else  (error 'gl-version "bad version string: ~v~n" str)]))
       (regexp-split #px"\\." (car (split-spaces str)))))
  (+ (* (first ns) 100) (second ns)))

(: gl-shading-language-version (-> Natural))
(define (gl-shading-language-version)
  (define ctxt (get-current-gl-context))
  (if ctxt
      (hash-ref!
       gl-shading-language-version-hash
       ctxt
       (λ ()
         (parse-shading-language-version-string (gl-get-string GL_SHADING_LANGUAGE_VERSION))))
      (error 'gl-shading-language-version "not in a GL context")))

(: gl-extensions-hash (HashTable GL-Context<%> (Setof Symbol)))
(define gl-extensions-hash (make-weak-hasheq))

(: gl-extensions (-> (Setof Symbol)))
(define (gl-extensions)
  (define ctxt (get-current-gl-context))
  (if ctxt
      (hash-ref!
       gl-extensions-hash
       ctxt
       (λ ()
         (define num (s32vector-ref (glGetIntegerv GL_NUM_EXTENSIONS) 0))
         (list->seteq
          (for/list : (Listof Symbol) ([j  (in-range num)])
            (string->symbol (gl-get-string GL_EXTENSIONS (assert j index?)))))))
      (error 'gl-extensions "not in a GL context")))

(: gl-has-extension? (-> Symbol Boolean))
(define (gl-has-extension? ext)
  (set-member? (gl-extensions) ext))

(: gl-version-at-least? (-> Natural Boolean))
(define (gl-version-at-least? version)
  (>= (gl-version) version))

(: gl-core-hash (HashTable GL-Context<%> Boolean))
(define gl-core-hash (make-weak-hasheq))

(: gl-core-profile? (-> Boolean))
(define (gl-core-profile?)
  (define ctxt (get-current-gl-context))
  (if ctxt
      (hash-ref!
       gl-core-hash
       ctxt
       (λ ()
         (cond [(gl-version-at-least? 32)
                (define profile-mask (s32vector-ref (glGetIntegerv GL_CONTEXT_PROFILE_MASK) 0))
                (= 1 (bitwise-and profile-mask 1))]
               [(gl-version-at-least? 31)
                (not (gl-has-extension? 'GL_ARB_compatibility))]
               [else  #f])))
      (error 'gl-core-profile? "not in a GL context")))

(define gl-types
  (make-hasheqv 
   (list (cons GL_UNSIGNED_BYTE _uint8)
         (cons GL_BYTE _sint8)
         (cons GL_UNSIGNED_SHORT _uint16)
         (cons GL_SHORT _sint16)
         (cons GL_UNSIGNED_INT _uint32)
         (cons GL_INT _sint32)
         (cons GL_FLOAT _float)
         (cons GL_DOUBLE _double))))

(: gl-type? (-> Integer Boolean))
(define (gl-type? obj)
  (hash-has-key? gl-types obj))

;; Get the appropriate type enum for a Racket vector.
;; Useful for glVertexPointer and friends.
;; Also get length and cpointer in one operation.

(: gl-vector->type (-> GLVector Integer))
(define (gl-vector->type vec)
  (let-values (((type ->cpointer length) (gl-vector->info vec)))
    type))

(: gl-vector->length (-> GLVector Index))
(define (gl-vector->length vec)
  (let-values (((type ->cpointer length) (gl-vector->info vec)))
    (length vec)))

(: gl-vector->type/cpointer (-> GLVector (Values Integer CPointer)))
(define (gl-vector->type/cpointer vec)
  (let-values (((type ->cpointer length) (gl-vector->info vec)))
    (values type (->cpointer vec))))

(: gl-vector->type/cpointer/length (-> GLVector (Values Integer CPointer Index)))
(define (gl-vector->type/cpointer/length vec)
  (let-values (((type ->cpointer length) (gl-vector->info vec)))
    (values type (->cpointer vec) (length vec))))

(: gl-type->ctype (-> Integer CType))
(define (gl-type->ctype type)
  (hash-ref gl-types type))

(: gl-type-sizeof (-> Integer Index))
(define (gl-type-sizeof type)
  (ctype-sizeof (gl-type->ctype type)))

(: gl-type-alignof (-> Integer Index))
(define (gl-type-alignof type)
  (ctype-alignof (gl-type->ctype type)))

(: gl-vector-sizeof (-> GLVector Index))
(define (gl-vector-sizeof vec)
  (let-values (((type ->cpointer length) (gl-vector->info vec)))
    (assert (* (length vec) (gl-type-sizeof type)) index?)))

(: gl-vector-alignof (-> GLVector Index))
(define (gl-vector-alignof vec)
  (gl-type-alignof (gl-vector->type vec)))

;;; Utility functions for dealing with textures

;; Convert argb -> rgba, and convert to pre-multiplied alpha.
;; (Non-premultiplied alpha gives blending artifacts and is evil.)
;; Modern wisdom is not to convert to rgba but rather use 
;; GL_BGRA with GL_UNSIGNED_INT_8_8_8_8_REV. But that turns out not
;; to work on some implementations, even ones which advertise
;; OpenGL 1.2 support. Great.
(: argb->rgba! (-> Bytes Void))
(define (argb->rgba! pixels)
  (for ((i (in-range (/ (bytes-length pixels) 4))))
    (let* ((offset (* 4 i))
           (alpha (bytes-ref pixels offset))
           (red (bytes-ref pixels (+ 1 offset)))
           (green (bytes-ref pixels (+ 2 offset)))
           (blue (bytes-ref pixels (+ 3 offset))))
      (bytes-set! pixels offset (quotient (* alpha red) 255))
      (bytes-set! pixels (+ 1 offset) (quotient (* alpha green) 255))
      (bytes-set! pixels (+ 2 offset) (quotient (* alpha blue) 255))
      (bytes-set! pixels (+ 3 offset) alpha))))


(: bitmap->texture (-> (Instance Bitmap%) [#:mipmap Any] [#:repeat (U 'none 'x 'y 'both)] Natural))
;; Convert a Racket bitmap into an OpenGL texture (with lots of default settings)
(define (bitmap->texture bm #:mipmap (mipmap #t) #:repeat (repeat-mode 'none))
  (let* ((w (send bm get-width))
         (h (send bm get-height))
         (pixels (make-bytes (* w h 4)))
         (texture (u32vector-ref (glGenTextures 1) 0)))
    
    (define (load-texture-data)
      (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 w h 0 GL_RGBA GL_UNSIGNED_BYTE pixels))
    
    (send bm get-argb-pixels 0 0 w h pixels)
    ;; massage data.
    (argb->rgba! pixels)
    
    (glBindTexture GL_TEXTURE_2D texture)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S 
                     (case repeat-mode ((x both) GL_REPEAT) (else GL_CLAMP)))
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T 
                     (case repeat-mode ((y both) GL_REPEAT) (else GL_CLAMP)))
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (cond 
      ;; modern mipmap generation method
      ((and mipmap (gl-version-at-least? 30))
       (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
       (load-texture-data)
       (glGenerateMipmap GL_TEXTURE_2D))
      
      ;; old mipmap generation method
      ((and mipmap (gl-version-at-least? 14))
       (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
       (glTexParameteri GL_TEXTURE_2D GL_GENERATE_MIPMAP GL_TRUE)
       (load-texture-data))
      
      (else
       ; fallback to not using mipmaps
       ; this seems more useful than erroring
       (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
       (load-texture-data)))
    
    texture))

(: load-texture (-> Path-String [#:mipmap Any] [#:repeat (U 'none 'x 'y 'both)] Natural))
;; Directly load a file from disk as texture.
(define (load-texture filename #:mipmap (mipmap #t) #:repeat (repeat-mode 'none))
  (error 'load-texture "currently not working due to an error in Typed Racket")
  #;
  (bitmap->texture (read-bitmap filename) #:mipmap mipmap #:repeat repeat-mode))

;;; Utility functions for dealing with shaders

(: get-shader-parameter (-> Natural Integer Integer))
(define (get-shader-parameter shader pname)
  (glGetShaderiv shader pname))

(: get-shader-info-log (-> Natural String))
(define (get-shader-info-log shader)
  (let ((log-length (get-shader-parameter shader GL_INFO_LOG_LENGTH)))
    (let-values (((actual-length info-log) (glGetShaderInfoLog shader log-length)))
      (bytes->string/utf-8 info-log #\? 0 actual-length))))

(: get-program-parameter (-> Natural Integer Integer))
(define (get-program-parameter program pname)
  (glGetProgramiv program pname))

(: get-program-info-log (-> Natural String))
(define (get-program-info-log program)
  (let ((log-length (get-program-parameter program GL_INFO_LOG_LENGTH)))
    (let-values (((actual-length info-log) (glGetProgramInfoLog program log-length)))
      (bytes->string/utf-8 info-log #\? 0 actual-length))))

(: load-shader-source (-> Natural Input-Port Void))
(define (load-shader-source shader port)
  (let* ((lines (for/list : (Listof String) ((line (in-lines port)))
                  (string-append line "\n")))
         (sizes (for/list : (Listof Index) ((line (in-list lines)))
                  (string-length line)))
         (sizes (list->s32vector sizes)))
   (glShaderSource shader (length lines) (list->vector lines) sizes)))

(: load-shader (-> (U Path-String Input-Port) Natural Natural))
(define (load-shader port-or-path shader-type)
  (let ((shader (glCreateShader shader-type)))
    (if (input-port? port-or-path) 
      (load-shader-source shader port-or-path)
      (call-with-input-file port-or-path (λ ([p : Input-Port])
                                           (load-shader-source shader p))
        #:mode 'text))
    (glCompileShader shader)
    (unless (= (get-shader-parameter shader GL_COMPILE_STATUS) GL_TRUE)
      (error 'load-shader "Error compiling shader ~a: ~a" port-or-path (get-shader-info-log shader)))
    shader))

(: create-program (-> Natural * Natural))
(define (create-program . shaders)
  (let ((program (glCreateProgram)))
    (for ([sh  (in-list shaders)])
      (glAttachShader program sh))
    (glLinkProgram program)
    (unless (= (get-program-parameter program GL_LINK_STATUS) GL_TRUE)
      (error 'create-program "Error linking program: ~a" (get-program-info-log program)))
    program))
