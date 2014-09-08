;; Extremely simply OpenGL demo.
;; Draw a "planet" (OK, a textured sphere).

#lang racket/gui

(require typed/opengl)
(require typed/opengl/ffi)
(require "viewer.rkt")

(define texture #f)

(define (make-circle first last step)
  (let ((factor (/ pi 180.0)))
    (for/list ((i (in-range first (+ last step) step)))
      (let ((phi (* factor (modulo i 360))))
        (cons (cos phi) (sin phi))))))

(define (make-sphere step)
  (let ((xy-list (make-circle 0 360 step))
        (rz-list (make-circle -90 90 step)))
    (for/list ((rz (in-list rz-list)))
      (let ((r (car rz)) (z (cdr rz)))
      (for/list ((xy (in-list xy-list)))
        (let ((x (car xy)) (y (cdr xy)))
          (vector (* r x) (* r y) z)))))))

(define (flatten-grid grid)
  (list->f32vector
    (for*/list ((row (in-list grid))
                (xyz (in-list row))
                (item (in-vector xyz)))
               (exact->inexact item))))

(define (make-grid-indices nrows ncols)
  (define (pos->index row col)
    (+ col (* row ncols)))
  (list->u32vector
  (for*/list ((row (in-range (- nrows 1)))
              (col (in-range (- ncols 1)))
              (i (in-list
                   (list
                     (pos->index row col)
                     (pos->index row (+ col 1))
                     (pos->index (+ row 1) (+ col 1))
                     (pos->index (+ row 1) col)))))
             i)))

(define (make-texcoords nrows ncols)
  (list->f32vector
    (for*/list ((row (in-range nrows))
                (col (in-range ncols))
                (item (list 
                        (/ (- ncols col) ncols)
                        (/ row nrows))))
               (exact->inexact item))))



(define sphere (make-sphere 15))
(define vertex-array (flatten-grid sphere))
(define nrows (length sphere))
(define ncols (length (car sphere)))
(define indices (make-grid-indices nrows ncols))
(define texcoord-array (make-texcoords nrows ncols))

(define display-list #f)

(define (init)
  (set! texture (load-texture "earth.png"))
  (set! display-list (glGenLists 1))
  (glNewList display-list GL_COMPILE)
  ; Let's be "modern" and use the array functions (introduced in OpenGL 1.1).
  ; Note that you need to ask GL everything 3 times:
  ; 1. Here is an array I'd like you to draw...
  (let-values (((type cptr) (gl-vector->type/cpointer vertex-array)))
    (glVertexPointer 3 type 0 cptr)
    (glNormalPointer type 0 cptr))
  (let-values (((type cptr) (gl-vector->type/cpointer texcoord-array)))
    (glTexCoordPointer 2 type 0 cptr))
  ; 2. Yes, I really want you to use it, I was not simply fooling around.
  (glEnableClientState GL_VERTEX_ARRAY)
  (glEnableClientState GL_NORMAL_ARRAY)
  (glEnableClientState GL_TEXTURE_COORD_ARRAY)
  ; 3. Allright, now draw the silly thing already!
  (let-values (((type cptr len) (gl-vector->type/cpointer/length indices)))
    (glDrawElements GL_QUADS len type cptr))

  ; Clean up state.
  (glDisableClientState GL_TEXTURE_COORD_ARRAY)
  (glDisableClientState GL_VERTEX_ARRAY)
  (glDisableClientState GL_NORMAL_ARRAY)

  (glEndList))

(define (draw)
  (glEnable GL_LIGHTING)
  (glEnable GL_LIGHT0)
  (glEnable GL_DEPTH_TEST)
  (glEnable GL_NORMALIZE)
  (glEnable GL_CULL_FACE)

  (glBindTexture GL_TEXTURE_2D texture)
  (glEnable GL_TEXTURE_2D)
 
  (glCallList display-list))


(view draw init)
