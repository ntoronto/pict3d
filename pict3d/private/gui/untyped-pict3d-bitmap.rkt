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
         "../utils.rkt"
         "parameters.rkt"
         "pict3d-struct.rkt"
         )

(provide (contract-out
          [pict3d->bitmap  (-> pict3d? (and/c index? (>/c 0)) (and/c index? (>/c 0))
                               (is-a?/c bitmap%))]))

(define get-the-bytes
  ;; Only one thread at a time can have an active OpenGL context, so this should be safe
  (make-unsafe-cached-vector
   'get-the-bytes
   (λ (n)
     (log-pict3d-debug "<bitmap> creating temp ARGB bytes of length ~v" n)
     (make-bytes n))
   bytes-length))

;(: pict3d->bitmap (-> Pict3D Integer Integer (Instance Bitmap%)))
(define (pict3d->bitmap pict width height)
  (define-values (bms cpu real gc)
    (time-apply
     (λ ()
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
       (with-gl-context (get-master-gl-context (pict3d-legacy-contexts?))
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
     empty))
  
  (log-pict3d-debug "<bitmap> heap size: ~a cpu time: ~a real time: ~a gc time: ~a"
                    (real->decimal-string (/ (current-memory-use) (* 1024 1024)) 2)
                    cpu real gc)
  
  (first bms))
