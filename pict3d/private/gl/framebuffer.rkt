#lang typed/racket/base

(require racket/match
         typed/opengl
         (except-in typed/opengl/ffi cast ->)
         "object.rkt"
         "texture.rkt"
         "renderbuffer.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Managed framebuffer objects

(struct gl-framebuffer gl-object
  ([width : Natural]
   [height : Natural]
   [attachments : (HashTable Integer (U gl-texture-2d gl-renderbuffer))])
  #:transparent)

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

(define null-gl-framebuffer (gl-framebuffer 0 0 0 #hasheqv()))

(: current-gl-framebuffer (Parameterof gl-framebuffer))
(define current-gl-framebuffer (make-parameter null-gl-framebuffer))

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

(: make-gl-framebuffer (-> Integer Integer (Listof (Pair Integer (U gl-texture-2d gl-renderbuffer)))
                           gl-framebuffer))
(define (make-gl-framebuffer width height attachments)
  (cond [(negative? width)
         (raise-argument-error 'make-gl-framebuffer "Natural" 0 width height attachments)]
        [(negative? height)
         (raise-argument-error 'make-gl-framebuffer "Natural" 1 width height attachments)]
        [else
         (define fbo (gl-framebuffer (u32vector-ref (glGenFramebuffers 1) 0)
                                     width height (make-hasheqv attachments)))
         (manage-gl-object fbo (λ ([handle : Natural])
                                 (glDeleteFramebuffers 1 (u32vector handle))))
         
         (with-gl-framebuffer fbo
           (for ([att  (in-list attachments)])
             (match-define (cons attachment obj) att)
             (cond [(gl-texture-2d? obj)
                    (glFramebufferTexture2D GL_FRAMEBUFFER
                                            attachment
                                            GL_TEXTURE_2D
                                            (gl-object-handle obj)
                                            0)]
                   [(gl-renderbuffer? obj)
                    (glFramebufferRenderbuffer GL_FRAMEBUFFER
                                               attachment
                                               GL_RENDERBUFFER
                                               (gl-object-handle obj))]))
           (check-gl-framebuffer-status))
         
         fbo]))

(define-syntax-rule (with-gl-framebuffer obj-stx body ...)
  (call-with-gl-object (λ () body ...)
                       current-gl-framebuffer
                       obj-stx
                       (λ ([handle : Natural])
                         (glBindFramebuffer GL_FRAMEBUFFER handle))))
