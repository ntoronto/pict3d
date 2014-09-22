#lang typed/racket/base

(require typed/opengl
         (except-in typed/opengl/ffi cast ->)
         "context.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Faces

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
