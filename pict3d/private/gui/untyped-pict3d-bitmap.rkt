#lang racket/base

(require racket/fixnum
         racket/gui
         racket/class
         (only-in typed/racket/base assert index?)
         math/flonum
         typed/opengl
         "../math/flt3.rkt"
         "../engine/scene.rkt"
         "../engine/utils.rkt"
         "../gl.rkt"
         "parameters.rkt"
         "pict3d-struct.rkt"
         )

(provide (contract-out
          [pict3d->bitmap  (-> pict3d? (and/c index? (>/c 0)) (and/c index? (>/c 0))
                               (is-a?/c bitmap%))]))

(define get-the-bytes
  (make-cached-vector 'get-the-bytes
                      (λ (n)
                        (printf "creating temp ARGB bytes of length ~v~n" n)
                        (make-bytes n))
                      bytes-length))

;(: pict3d->bitmap (-> Pict3D Integer Integer (Instance Bitmap%)))
(define (pict3d->bitmap pict width height)
  (define view (pict3d-view-transform pict))
  ;; Compute a projection matrix
  (define znear (current-pict3d-z-near))
  (define zfar (current-pict3d-z-far))
  (define fov-radians (degrees->radians (fl (current-pict3d-fov-degrees))))
  (define proj (flt3compose
                (scale-flt3 (flvector 1.0 -1.0 1.0))  ; upside-down: OpenGL origin is lower-left
                (perspective-flt3/viewport (fl width) (fl height) fov-radians znear zfar)))
  (define bm (make-bitmap width height))
  ;; Lock everything up for drawing
  (with-gl-context (get-master-gl-context)
    ;; Draw the scene
    (draw-scene (pict3d-scene pict) width height
                view proj
                (current-pict3d-background)
                (current-pict3d-ambient-color)
                (current-pict3d-ambient-intensity))
    ;; Get the resulting pixels and set them into the bitmap
    (define bs (get-the-bytes (* 4 width height)))
    (glReadPixels 0 0 width height GL_BGRA GL_UNSIGNED_INT_8_8_8_8 bs)
    (send bm set-argb-pixels 0 0 width height bs #f #t))
  bm)
