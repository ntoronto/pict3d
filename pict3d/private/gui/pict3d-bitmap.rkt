#lang typed/racket/base

(require racket/fixnum
         racket/math
         math/flonum
         typed/racket/gui
         typed/racket/class
         typed/opengl
         "../math/flt3.rkt"
         "../engine/scene.rkt"
         "../engine/gl.rkt"
         "../engine/utils.rkt"
         "pict3d-snip.rkt"
         )

(provide pict3d->bitmap)

(define get-the-bytes (make-cached-vector 'get-the-bytes make-bytes bytes-length))
(define get-tmp-bytes (make-cached-vector 'get-tmp-bytes make-bytes bytes-length))

(: pict3d->bitmap (-> Pict3D Integer Integer (Instance Bitmap%)))
(define (pict3d->bitmap pict width height)
  (cond
    [(or (not (index? width)) (= width 0))
     (error 'pict3d->bitmap "expected Positive-Index width; given" width)]
    [(or (not (index? height)) (= height 0))
     (error 'pict3d->bitmap "expected Positive-Index height; given" height)]
    [else
     (define view (pict3d-view-transform pict))
     ;; Compute a projection matrix
     (define znear (current-z-near))
     (define zfar (current-z-far))
     (define fov-radians (degrees->radians (fl (current-fov-degrees))))
     (define proj (perspective-flt3/viewport (fl width) (fl height) fov-radians znear zfar))
     (define bm (make-bitmap width height))
     ;; Lock everything up for drawing
     (with-gl-context (get-master-gl-context)
       ;; Draw the scene
       (draw-scene (send pict get-scene) width height view proj (current-ambient))
       
       ;; Get the resulting pixels, upside-down (OpenGL origin is lower-left; we use upper-left)
       (define row-size (* width 4))
       (define bs (get-the-bytes (assert (* row-size height) index?)))
       (glReadPixels 0 0 width height GL_BGRA GL_UNSIGNED_INT_8_8_8_8 bs)
       
       ;; Flip right-side-up
       (define tmp (get-tmp-bytes row-size))
       (for ([row  (in-range (fxquotient height 2))])
         (define i0 (* row row-size))
         (define i1 (* (- (- height row) 1) row-size))
         (bytes-copy! tmp 0 bs i0 (+ i0 row-size))
         (bytes-copy! bs i0 bs i1 (+ i1 row-size))
         (bytes-copy! bs i1 tmp 0 row-size))
       
       (send bm set-argb-pixels 0 0 width height bs #f #t))
     bm]))
