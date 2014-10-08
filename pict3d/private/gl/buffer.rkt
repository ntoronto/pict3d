#lang typed/racket/base

(require typed/opengl
         (except-in typed/opengl/ffi cast ->)
         "object.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Managed buffers

(struct gl-array-buffer gl-object () #:transparent)

(define null-gl-array-buffer (gl-array-buffer 0))

(define (make-gl-array-buffer)
  (define buf (gl-array-buffer (u32vector-ref (glGenBuffers 1) 0)))
  (manage-gl-object buf (λ ([handle : Natural]) (glDeleteBuffers 1 (u32vector handle))))
  buf)

(: current-gl-array-buffer (Parameterof gl-array-buffer))
(define current-gl-array-buffer (make-parameter null-gl-array-buffer))

(define-syntax-rule (with-gl-array-buffer obj-stx body ...)
  (call-with-gl-object (λ () body ...)
                       current-gl-array-buffer
                       obj-stx
                       (λ ([handle : Natural]) (glBindBuffer GL_ARRAY_BUFFER handle))))

(: gl-bind-array-buffer (-> gl-array-buffer Void))
(define (gl-bind-array-buffer buf)
  (unless (eq? buf (current-gl-array-buffer))
    (glBindBuffer GL_ARRAY_BUFFER (gl-object-handle buf))
    (current-gl-array-buffer buf)))

;; ===================================================================================================

(struct gl-index-buffer gl-object () #:transparent)

(define null-gl-index-buffer (gl-index-buffer 0))

(define (make-gl-index-buffer)
  (define buf (gl-index-buffer (u32vector-ref (glGenBuffers 1) 0)))
  (manage-gl-object buf (λ ([handle : Natural]) (glDeleteBuffers 1 (u32vector handle))))
  buf)

(: current-gl-index-buffer (Parameterof gl-index-buffer))
(define current-gl-index-buffer (make-parameter null-gl-index-buffer))

(define-syntax-rule (with-gl-index-buffer obj-stx body ...)
  (call-with-gl-object (λ () body ...)
                       current-gl-index-buffer
                       obj-stx
                       (λ ([handle : Natural]) (glBindBuffer GL_ELEMENT_ARRAY_BUFFER handle))))

(: gl-bind-index-buffer (-> gl-index-buffer Void))
(define (gl-bind-index-buffer buf)
  (unless (eq? buf (current-gl-index-buffer))
    (glBindBuffer GL_ELEMENT_ARRAY_BUFFER (gl-object-handle buf))
    (current-gl-index-buffer buf)))
