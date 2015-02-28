#lang racket

(require racket/runtime-path
         racket/contract
         racket/draw
         racket/fasl
         "lazy-gui.rkt"
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
       (s-exp->fasl #f out)
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
        (define sexp
          (list as-snip?
                width
                height
                (current-pict3d-z-near)
                (current-pict3d-z-far)
                (current-pict3d-fov-degrees)
                (rgba->flvector (current-pict3d-background))
                (emitted->flvector (current-pict3d-ambient))
                (current-pict3d-add-sunlight?)
                (current-pict3d-add-indicators?)
                (marshal-scene (pict3d-scene p))))
        (s-exp->fasl sexp out)
        (flush-output out)
        (define bs (fasl->s-exp in))
        (define bm (make-bitmap width height))
        (send bm set-argb-pixels 0 0 width height bs)
        bm]
       [else
        (error 'request-render "render server is not started~n")]))))
