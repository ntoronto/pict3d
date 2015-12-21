#lang racket/base
(require pict3d
         pict3d/private/engine/shader/shader-code
         pict3d/private/gl/context
         racket/class
         racket/gui)

(define-syntax-rule (get-shaders m)
  (let ()
    (local-require (only-in (submod m shaders) shaders))
    shaders))
(define-syntax-rule (get-all-shaders m ...)
  (list (cons 'm (get-shaders m)) ...))

(module+ main
  (define frame (new frame% [label "Test"] [width 800] [height 600]))
  (define canvas
    (new
     canvas%
     [parent frame]
     [gl-config (pict3d-default-gl-config)]
     [style '(gl)]
     [paint-callback
      (λ (c dc)
        (define glctx (send dc get-gl-context))
        (send
         glctx call-as-current
         (λ ()
           (with-gl-context (managed-gl-context glctx)
             (for-each
              (λ (m*shaders)
                (match-define (cons m shaders) m*shaders)
                (printf "Compiling shaders from ~a\n" m)
                (for ([s (in-list shaders)])
                  (printf "\tCompiling ~e\n" s)
                  (program-code->gl-program s)))
              (get-all-shaders pict3d/private/engine/draw/draw-passes
                               pict3d/private/gui/shape/light-grid
                               pict3d/private/gui/shape/point-light-shell
                               pict3d/private/shape/directional-light
                               pict3d/private/shape/cylinder
                               pict3d/private/shape/disk
                               pict3d/private/shape/point-light
                               pict3d/private/shape/sphere/ge_30
                               pict3d/private/shape/sphere/ge_32
                               pict3d/private/shape/triangle-mesh
                               pict3d/private/shape/triangle-outline)))
           (send glctx swap-buffers)
           (exit 0))))]))
  (send frame show #t)
  (send canvas refresh-now))
