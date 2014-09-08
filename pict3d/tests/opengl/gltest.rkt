;; Extremely simply OpenGL demo.
;; Draw a white rectangle on the screen.
;; Also query some information on the OpenGL implementation.

#lang racket/gui

(require typed/opengl)
(require typed/opengl/ffi)
(require "viewer.rkt")

(define (print-info)
  ; print available extensions
  (for ((ext (in-set (gl-extensions))))
       (printf "Extension: ~a~%" ext))

  ; print version, as a nice parseable Racket list
  (printf "~s~%" (gl-version))

  ; demo to check for a certain extension
  (if (gl-has-extension? 'GL_EXT_texture_object)
    (printf "Yep, we have GL_EXT_texture_object~%")
    (printf "Sorry no GL_EXT_texture_object~%"))

  ; demo to query some array-based state
  (let ((v (glGetIntegerv GL_VERTEX_ARRAY)))
    (printf "glGet on GL_VERTEX_ARRAY = ~s~%" (s32vector-ref v 0))))
 

(define (draw)
  ; the coordinates
  (define vertex-array
    (f64vector -0.5 -0.5
               0.5 -0.5
               0.5 0.5
               -0.5 0.5))

  ; Let's be "modern" and use the array functions (introduced in OpenGL 1.1).
  ; Note that you need to ask GL everything 3 times:
  ; 1. Here is an array I'd like you to draw...
  (let-values (((type cptr) (gl-vector->type/cpointer vertex-array)))
    (glVertexPointer 2 type 0 cptr))
  ; 2. Yes, I really want you to use it, I was not simply fooling around.
  (glEnableClientState GL_VERTEX_ARRAY)
  ; 3. Allright, now draw the silly thing already!
  (glDrawArrays GL_QUADS 0 4)
  ; Clean up state.
  (glDisableClientState GL_VERTEX_ARRAY))



(view draw print-info)
