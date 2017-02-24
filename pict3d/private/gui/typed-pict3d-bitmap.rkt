#lang typed/racket/base
(require typed/opengl
         "../gl.rkt"
         "../utils.rkt"
         "parameters.rkt"
         "master-context.rkt"
         "pict3d-struct.rkt"
         "pict3d-draw.rkt"
         (submod "../engine/draw/draw-passes.rkt" gl-helpers))

(: pict3d-set-bytes! (-> Pict3D Nonnegative-Integer Nonnegative-Integer Bytes Void))
(define (pict3d-set-bytes! pict width height bs)
  (define fbo (get-draw-fbo width height))
  (with-gl-framebuffer fbo
    (draw-pict3ds (list pict) #:width width #:height height #:bitmap? #t))
  (define fbo-tex (gl-framebuffer-texture-2d fbo GL_COLOR_ATTACHMENT0))
  ;; Get the resulting pixels and set them into the bitmap
  (with-gl-active-texture GL_TEXTURE0
    (with-gl-texture fbo-tex
      (glGetTexImage GL_TEXTURE_2D 0 GL_RGBA GL_UNSIGNED_BYTE bs)))
  (void))

(provide pict3d-set-bytes!)
