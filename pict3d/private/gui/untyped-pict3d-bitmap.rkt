#lang racket/base

(require racket/draw
         racket/class
         racket/list
         racket/contract
         typed/opengl
         "../gl.rkt"
         "../utils.rkt"
         "parameters.rkt"
         "master-context.rkt"
         "pict3d-struct.rkt"
         "pict3d-draw.rkt"
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
       ;; Lock everything up for drawing
       (with-gl-context (get-master-gl-context (current-pict3d-legacy?)
                                               (current-pict3d-check-version?))
         (draw-pict3ds (list pict) #:width width #:height height #:bitmap? #t)
         ;; Get the resulting pixels and set them into the bitmap
         (define bs (get-the-bytes (* 4 width height)))
         (glReadPixels 0 0 width height GL_BGRA GL_UNSIGNED_INT_8_8_8_8 bs)
         (define bm (make-bitmap width height))
         (send bm set-argb-pixels 0 0 width height bs #f #t)
         bm))
     empty))
  
  (log-pict3d-debug "<bitmap> heap size: ~a cpu time: ~a real time: ~a gc time: ~a"
                    (real->decimal-string (/ (current-memory-use) (* 1024 1024)) 2)
                    cpu real gc)
  
  (first bms))
