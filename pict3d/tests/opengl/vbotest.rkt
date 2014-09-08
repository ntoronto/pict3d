;; Extremely simply OpenGL demo.
;; Draw a shaded rectangle on the screen, using VBOs.
;; Also query some information on the OpenGL implementation.

#lang racket/gui

(require typed/opengl)
(require typed/opengl/ffi)
(require "viewer.rkt")


(define texture #f)
(define vbo #f)

(define vertex-texcoord-array
   (f64vector -0.5 -0.5
    0.5 -0.5
    0.5 0.5
    -0.5 0.5
    0 2
    2 2
    2 0
    0 0))

(define sizeof-double (gl-type-sizeof GL_DOUBLE))

(define (setup)
  ;; Note that we can only load textures once we have an OpenGL context!
  (set! texture (load-texture "plt-logo-red-gradient.png" #:repeat 'both))
  (set! vbo (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ARRAY_BUFFER vbo)
  (glBufferData GL_ARRAY_BUFFER
   (gl-vector-sizeof vertex-texcoord-array)
   vertex-texcoord-array
   GL_STATIC_DRAW)
  (printf "VBO ~a loaded~%" vbo)
  (glBindBuffer GL_ARRAY_BUFFER 0))

(define (draw)
  ; the coordinates
  (glBindTexture GL_TEXTURE_2D texture)
  (glBindBuffer GL_ARRAY_BUFFER vbo)
  (glEnable GL_TEXTURE_2D)
  (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA) ; This is the correct setting for pre-multiplied alpha.
  (glEnable GL_BLEND)

   
  (glVertexPointer 2 GL_DOUBLE 0 0)
  (glTexCoordPointer 2 GL_DOUBLE 0 (* sizeof-double 2 4))

  (glEnableClientState GL_VERTEX_ARRAY)
  (glEnableClientState GL_TEXTURE_COORD_ARRAY)

  (glDrawArrays GL_QUADS 0 4)

  ; Clean up state.
  (glDisableClientState GL_TEXTURE_COORD_ARRAY)
  (glDisableClientState GL_VERTEX_ARRAY)
  (glBindBuffer GL_ARRAY_BUFFER 0))



(view draw setup)
