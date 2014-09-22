#lang typed/racket/base

(require typed/opengl
         (except-in typed/opengl/ffi cast ->)
         "object.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Managed vertex arrays

(struct gl-vertex-array gl-object () #:transparent)

(define null-gl-vertex-array (gl-vertex-array 0))

(define (make-gl-vertex-array)
  (define vao (gl-vertex-array (u32vector-ref (glGenVertexArrays 1) 0)))
  (manage-gl-object vao (λ (handle) (glDeleteVertexArrays 1 (u32vector handle))))
  vao)

(: current-gl-vertex-array (Parameterof gl-vertex-array))
(define current-gl-vertex-array (make-parameter null-gl-vertex-array))

(define-syntax-rule (with-gl-vertex-array obj-stx body ...)
  (call-with-gl-object (λ () body ...)
                       current-gl-vertex-array
                       obj-stx
                       glBindVertexArray))

(: gl-bind-vertex-array (-> gl-vertex-array Void))
(define (gl-bind-vertex-array vao)
  (unless (eq? vao (current-gl-vertex-array))
    (glBindVertexArray (gl-object-handle vao))
    (current-gl-vertex-array vao)))
