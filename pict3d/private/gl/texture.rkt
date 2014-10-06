#lang typed/racket/base

(require racket/match
         racket/list
         typed/opengl
         (except-in typed/opengl/ffi cast ->)
         "context.rkt"
         "object.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Managed textures

(: current-gl-active-texture (Parameterof Integer))
(define current-gl-active-texture (make-parameter GL_TEXTURE0))

(define-syntax-rule (with-gl-active-texture texnum-stx body ...)
  (let ([texnum : Integer  texnum-stx])
    (call-with-gl-state (λ () body ...) current-gl-active-texture texnum glActiveTexture)))

(struct gl-texture gl-object ([target : Integer]) #:transparent)

(struct gl-texture-2d gl-texture
  ([width : Natural]
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
         (define tex (gl-texture-2d (u32vector-ref (glGenTextures 1) 0)
                                    GL_TEXTURE_2D width height internal-format format type
                                    (make-immutable-hasheqv params)))
         (manage-gl-object tex (λ ([handle : Natural]) (glDeleteTextures 1 (u32vector handle))))
         
         (with-gl-texture tex
           (for ([param  (in-list params)])
             (match-define (cons key value) param)
             (glTexParameteri GL_TEXTURE_2D key value))
           (glTexImage2D GL_TEXTURE_2D 0 internal-format width height 0 format type 0))
         
         tex]))

(define null-gl-texture (gl-texture 0 GL_TEXTURE_1D))

(: current-gl-textures (Parameterof (HashTable Integer gl-texture)))
(define current-gl-textures
  (make-parameter ((inst make-immutable-hasheqv Integer gl-texture) empty)))

(: call-with-gl-texture (All (A) (-> (-> A) gl-texture A)))
(define (call-with-gl-texture body-thunk tex)
  (get-current-managed-gl-context 'with-gl-texture)
  (define old (hash-ref (current-gl-textures)
                        (current-gl-active-texture)
                        (λ () null-gl-texture)))
  (cond [(eq? old tex)  (body-thunk)]
        [else  (glBindTexture (gl-texture-target tex) (gl-object-handle tex))
               (begin0
                 (parameterize ([current-gl-textures  (hash-set (current-gl-textures)
                                                                (current-gl-active-texture)
                                                                tex)])
                   (body-thunk))
                 (glBindTexture (gl-texture-target old) (gl-object-handle old)))]))

(define-syntax-rule (with-gl-texture tex body ...)
  (call-with-gl-texture (λ () body ...) tex))
