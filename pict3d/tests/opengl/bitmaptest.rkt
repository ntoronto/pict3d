;; Extremely simply OpenGL demo.
;; Draw a shaded rectangle on the screen.
;; Also query some information on the OpenGL implementation.

#lang racket/gui

(require typed/opengl)
(require typed/opengl/ffi)
(require "viewer.rkt")


(define texture #f)

(define (setup)
  ;; Note that we can only load textures once we have an OpenGL context!
  (set! texture (load-texture "plt-logo-red-gradient.png" #:repeat 'both)))

(define (draw)
  ; the coordinates
  (define vertex-array
    (f64vector -0.5 -0.5
               0.5 -0.5
               0.5 0.5
               -0.5 0.5))

  (define texcoord-array
    (s16vector 0 2
               2 2
               2 0
               0 0))

  (glBindTexture GL_TEXTURE_2D texture)
  (glEnable GL_TEXTURE_2D)
  (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA) ; This is the correct setting for pre-multiplied alpha.
  (glEnable GL_BLEND)

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
  (glDisableClientState GL_VERTEX_ARRAY))


;; Example how to instrument loading of functions
(set-gl-procedure-loader!
  (λ (name)
    (printf "Loading ~a~%" name)
    (default-gl-procedure-loader name)))

(view draw setup)
