#lang racket

(require racket/runtime-path
         racket/contract
         racket/draw
         racket/fasl
         "lazy-gui.rkt"
         (only-in "gui/user-types.rkt" col-flvector)
         "math/flt3.rkt"
         (only-in "engine/types.rkt" affine-transform)
         "engine/scene/marshal-scene.rkt")

(provide (contract-out
          [start-render-server (-> void?)]
          [stop-render-server (-> void?)]
          [request-render (->* [pict3d?]
                               [exact-positive-integer?
                                exact-positive-integer?
                                #:as-snip? boolean?]
                               (is-a?/c bitmap%))]))

(define-runtime-path render-server "render-server.rkt")
(define gracket (find-system-path 'exec-file))

(define proc-mutex (make-semaphore 1))

(define in #f)
(define out #f)
(define pid #f)
(define err #f)
(define jack #f)


(define (start-render-server)
  (call-with-semaphore
   proc-mutex
   (λ ()
     (unless pid
       (match-define (list new-in new-out new-pid new-err new-jack)
         (process*/ports #f #f #f gracket render-server))
       (set! in new-in)
       (set! out new-out)
       (set! pid new-pid)
       (set! err new-err)
       (set! jack new-jack)))))

(define (stop-render-server)
  (call-with-semaphore
   proc-mutex
   (λ ()
     (when pid
       (s-exp->fasl '(stop) out)
       (flush-output out)
       (close-input-port in)
       (close-output-port out)
       (close-input-port err)
       (set! in #f)
       (set! out #f)
       (set! pid #f)
       (set! err #f)
       (set! jack #f)))))

(define (request-render p
                        [width (current-pict3d-width)]
                        [height (current-pict3d-height)]
                        #:as-snip? [as-snip? #f])
  (call-with-semaphore
   proc-mutex
   (λ ()
     (cond
       [pid
        (define auto-camera-transform
          (fltransform3-forward (affine-transform ((current-pict3d-auto-camera) p))))
        (define sexp
          (list 'render
                as-snip?
                width
                height
                (current-pict3d-z-near)
                (current-pict3d-z-far)
                (current-pict3d-fov)
                (col-flvector (current-pict3d-background))
                (col-flvector (current-pict3d-ambient))
                (current-pict3d-add-sunlight?)
                (current-pict3d-add-indicators?)
                auto-camera-transform
                (marshal-scene (pict3d-scene p))))
        (s-exp->fasl sexp out)
        (flush-output out)
        (match (fasl->s-exp in)
          [(list 'bitmap width height bs)
           (define bm (make-bitmap width height))
           (send bm set-argb-pixels 0 0 width height bs)
           bm]
          [(list 'exception msg)
           (error 'request-render (format "render-server: ~a" msg))]
          [sexp
           (error 'request-render "unexpected server message ~e" sexp)])]
       [else
        (error 'request-render "render server is not started~n")]))))
