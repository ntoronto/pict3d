#lang racket/base

(require racket/draw
         racket/class
         racket/list
         racket/contract
         math/base
         math/flonum
         typed/opengl
         "../math/flt3.rkt"
         "../engine/scene.rkt"
         "../engine/utils.rkt"
         (only-in "../engine/types.rkt" affine-transform)
         "../gl.rkt"
         "../utils.rkt"
         "parameters.rkt"
         "master-context.rkt"
         "pict3d-struct.rkt"
         "pict3d-combinators.rkt"
         "user-types.rkt"
         )

(provide (contract-out
          [pict3d->bitmap  (->* [pict3d?] [(and/c fixnum? (>/c 0)) (and/c fixnum? (>/c 0))]
                                (is-a?/c bitmap%))]))

(define get-the-bytes
  (make-gl-cached-vector
   'get-the-bytes
   (λ (n)
     (log-pict3d-debug "<bitmap> creating temp ARGB bytes of length ~v" n)
     (make-bytes n))
   bytes-length))

;(: pict3d->bitmap (-> Pict3D Integer Integer (Instance Bitmap%)))
(define (pict3d->bitmap pict [width (current-pict3d-width)] [height (current-pict3d-height)])
  (define-values (bms cpu real gc)
    (time-apply
     (λ ()
       (define view (affine-transform (pict3d-view-transform pict)))
       ;; Compute a projection matrix
       (define znear (current-pict3d-z-near))
       (define zfar (current-pict3d-z-far))
       (define fov-radians (degrees->radians (fl (current-pict3d-fov-degrees))))
       (define proj (flt3compose
                     (scale-flt3 (flvector 1.0 -1.0 1.0))  ; upside-down: OpenGL origin is lower-left
                     (perspective-flt3/viewport (fl width) (fl height) fov-radians znear zfar)))
       (define bm (make-bitmap width height))
       ;; Lock everything up for drawing
       (with-gl-context (get-master-gl-context (current-pict3d-legacy?))
         ;; Draw the scene
         (draw-scene (pict3d-scene pict) width height
                     view proj
                     (rgba->flvector (current-pict3d-background))
                     (emitted->flvector (current-pict3d-ambient)))
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
