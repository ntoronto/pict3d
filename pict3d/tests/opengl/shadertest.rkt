;; Extremely simply OpenGL demo.
;; Draw a shaded rectangle on the screen.
;; Also query some information on the OpenGL implementation.

#lang racket/gui

(require typed/opengl)
(require typed/opengl/ffi)
(require "viewer.rkt")

(define program #f)

(define (setup)
  (if (or (gl-version-at-least? 2)
          (gl-has-extension? 'GL_ARB_shader_objects))
    (set! program (create-program (load-shader "test.glsl" GL_FRAGMENT_SHADER)))
    (printf "This OpenGL does not support shaders, you'll get a plain white rectangle.~%")))

(define (draw)
  ; the coordinates
  (define vertex-array
    (f64vector -0.5 -0.5
               0.5 -0.5
               0.5 0.5
               -0.5 0.5))

  (define texcoord-array
    (f64vector 0 0
               0.5 0
               0.5 0.5
               0 0.5))


  (when program
    (glUseProgram program))

  ; Let's be "modern" and use the array functions (introduced in OpenGL 1.1).
  ; Note that you need to ask GL everything 3 times:
  ; 1. Here is an array I'd like you to draw...
  (let-values (((type cptr) (gl-vector->type/cpointer vertex-array)))
    (glVertexPointer 2 type 0 cptr))
  (let-values (((type cptr) (gl-vector->type/cpointer texcoord-array)))
    (glTexCoordPointer 2 type 0 cptr))
  ; 2. Yes, I really want you to use it, I was not simply fooling around.
  (glEnableClientState GL_VERTEX_ARRAY)
  (glEnableClientState GL_TEXTURE_COORD_ARRAY)
  ; 3. Allright, now draw the silly thing already!
  (glDrawArrays GL_QUADS 0 4)

  ; Clean up state.
  (glDisableClientState GL_TEXTURE_COORD_ARRAY)
  (glDisableClientState GL_VERTEX_ARRAY)
  (when program
    (glUseProgram 0)))



(view draw setup)
