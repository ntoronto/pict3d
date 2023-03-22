#lang typed/racket/base
(require typed/opengl
         "../gl.rkt"
         "../utils.rkt"
         "parameters.rkt"
         "master-context.rkt"
         "pict3d-struct.rkt"
         "pict3d-draw.rkt")

(: pict3d-set-bytes! (-> Pict3D Nonnegative-Integer Nonnegative-Integer Bytes Void))
(define (pict3d-set-bytes! pict width height bs)
  (define rbo
    (make-gl-renderbuffer width height GL_RGBA8))
  (with-gl-renderbuffer rbo
    (define fbo
      (make-gl-framebuffer width height
                           (list (cons GL_COLOR_ATTACHMENT0 rbo))))
    (with-gl-framebuffer fbo
      (draw-pict3ds (list pict) #:width width #:height height #:bitmap? #t)

      (glReadPixels 0 0 width height GL_RGBA GL_UNSIGNED_BYTE bs)))
  (void))

(provide pict3d-set-bytes!)
