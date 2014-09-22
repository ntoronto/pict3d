#lang typed/racket/base

(require typed/opengl
         (except-in typed/opengl/ffi cast ->)
         "object.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Managed render buffers

(struct gl-renderbuffer gl-object
  ([width : Natural]
   [height : Natural]
   [internal-format : Integer])
  #:transparent)

(define null-gl-renderbuffer (gl-renderbuffer 0 0 0 0))

(: current-gl-renderbuffer (Parameterof gl-renderbuffer))
(define current-gl-renderbuffer (make-parameter null-gl-renderbuffer))

(: make-gl-renderbuffer (-> Integer Integer Integer gl-renderbuffer))
(define (make-gl-renderbuffer width height internal-format)
  (cond [(negative? width)
         (raise-argument-error 'make-gl-renderbuffer "Natural" 0 width height internal-format)]
        [(negative? height)
         (raise-argument-error 'make-gl-renderbuffer "Natural" 1 width height internal-format)]
        [else
         (define buf (gl-renderbuffer (u32vector-ref (glGenRenderbuffers 1) 0)
                                      width height internal-format))
         (manage-gl-object buf (λ ([handle : Natural]) (glDeleteRenderbuffers 1 (u32vector handle))))
         (with-gl-renderbuffer buf
           (glRenderbufferStorage GL_RENDERBUFFER internal-format width height))
         buf]))

(define-syntax-rule (with-gl-renderbuffer obj-stx body ...)
  (call-with-gl-object (λ () body ...)
                       current-gl-renderbuffer
                       obj-stx
                       (λ ([handle : Natural]) (glBindRenderbuffer GL_RENDERBUFFER handle))))
