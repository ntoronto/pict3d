#lang racket/base

(require racket/gui/base
         racket/match
         racket/list
         racket/draw
         racket/class
         racket/async-channel
         racket/math
         math/flonum
         typed/opengl
         "../math/flt3.rkt"
         "../engine/scene.rkt"
         (only-in "../engine/types.rkt" affine-transform)
         "../gl.rkt"
         "../utils.rkt"
         "parameters.rkt"
         "pict3d-struct.rkt"
         "pict3d-combinators.rkt"
         "user-types.rkt"
         )

(provide pict3d-canvas%)

;; ===================================================================================================
;; Rendering threads

(struct render-command
  (pict3d
   width
   height
   z-near
   z-far
   fov-degrees
   background
   ambient
   ack-channel)
  #:transparent)

#;
(struct render-command ([pict3d : Pict3D]
                        [width : Index]
                        [height : Index]
                        [z-near : Positive-Flonum]
                        [z-far : Positive-Flonum]
                        [fov-degrees : Positive-Flonum]
                        [background : FlVector]
                        [ambient : FlVector]
                        [ack-channel : (U #f (Channelof Boolean))]
                        ) #:transparent)

(define (render cmd canvas)
  (define-values (_ cpu real gc)
    (time-apply
     (λ ()
       (match-define (render-command pict width height
                                     znear zfar fov-degrees
                                     background ambient
                                     ack-channel)
         cmd)
       ;; Get the view matrix
       (define view (affine-transform (pict3d-view-transform pict)))
       ;; Compute the projection matrix
       (define fov-radians (degrees->radians fov-degrees))
       (define proj (perspective-flt3/viewport (fl width) (fl height) fov-radians znear zfar))
       
       ;; Lock everything up for drawing
       (call-with-gl-context
        (λ ()
          ;; Draw the scene and swap buffers
          (draw-scene (pict3d-scene pict) width height view proj
                      (rgba->flvector background)
                      (emitted->flvector ambient))
          (gl-swap-buffers))
        (send canvas get-managed-gl-context))
       
       ;; Send an ACK if the other side is waiting for one
       (when ack-channel
         (channel-put ack-channel #t)))
     empty))
  
  (log-pict3d-debug "<canvas> heap size: ~a cpu time: ~a real time: ~a gc time: ~a"
                    (real->decimal-string (/ (current-memory-use) (* 1024 1024)) 2)
                    cpu real gc))

;(: make-canvas-render-thread (-> (Instance Pict3D-Canvas%) (Async-Channelof render-command) Thread))
(define (make-canvas-render-thread canvas ch)
  ;(: render-thread-loop (-> Void))
  (define (render-thread-loop)
    ;; Wait for a scene and view matrix
    ;(: cmd render-command)
    (define cmd
      (let ([cmd  (async-channel-get ch)])
        ;; Empty the queue looking for the lastest one
        (let loop ([cmd  cmd])
          (define new-cmd (async-channel-try-get ch))
          (if new-cmd (loop new-cmd) cmd))))
    
    (render cmd canvas)
    (render-thread-loop))
  
  (thread render-thread-loop))

;; ===================================================================================================
;; Scene canvas

;(: pict3d-canvas% Pict3D-Canvas%)
(define pict3d-canvas%
  (class canvas%
    (init parent
          [style  '()]
          [label  #f]
          [enabled  #t]
          [vert-margin   0]
          [horiz-margin  0]
          [min-width   #f]
          [min-height  #f]
          [stretchable-width   #t]
          [stretchable-height  #t])
    (init-field [pict  empty-pict3d])
    
    (define legacy? (current-pict3d-legacy?))
    
    (define config (new gl-config%))
    (send config set-legacy? legacy?)
    
    (super-new [parent parent]
               [style  (list* 'gl 'no-autoclear style)]
               [paint-callback  void]
               [label  label]
               [gl-config  config]
               [enabled  enabled]
               [vert-margin   vert-margin]
               [horiz-margin  horiz-margin]
               [min-width   min-width]
               [min-height  min-height]
               [stretchable-width   stretchable-width]
               [stretchable-height  stretchable-height])
    
    (define async-updates? #f)
    
    (define/public (set-async-updates? async?)
      (set! async-updates? async?))
    
    ;(: render-queue (Async-Channel render-command))
    (define render-queue (make-async-channel))
    
    ;(: render-thread Thread)
    (define render-thread (make-canvas-render-thread this render-queue))
    
    ;(: get-gl-window-size (-> (Values Index Index)))
    (define (get-gl-window-size)
      (define-values (w h) (send (send this get-dc) get-size))
      (values (exact-floor w)
              (exact-floor h)))
    
    ;(: z-near Positive-Flonum)
    ;(: z-far Positive-Flonum)
    ;(: fov-degrees Positive-Flonum)
    ;(: background FlVector)
    ;(: ambient-color FlVector)
    ;(: ambient-intensity Flonum)
    (define z-near (current-pict3d-z-near))
    (define z-far (current-pict3d-z-far))
    (define fov-degrees (current-pict3d-fov-degrees))
    (define background (current-pict3d-background))
    (define ambient (current-pict3d-ambient))
    
    (define (do-render new-pict width height
                       z-near z-far fov-degrees
                       background ambient
                       async?)
      (define ack-channel (if async? #f (make-channel)))
      (async-channel-put
       render-queue
       (render-command new-pict width height
                       z-near z-far fov-degrees
                       background ambient
                       ack-channel))
      (when ack-channel
        (channel-get ack-channel)))
    
    (define/public (set-pict3d new-pict)
      (set! pict new-pict)
      (define-values (width height) (get-gl-window-size))
      (set! z-near (current-pict3d-z-near))
      (set! z-far (current-pict3d-z-far))
      (set! fov-degrees (current-pict3d-fov-degrees))
      (set! background (current-pict3d-background))
      (set! ambient (current-pict3d-ambient))
      (do-render new-pict width height
                 z-near z-far fov-degrees
                 background ambient
                 async-updates?))
    
    (define/public (get-pict3d) pict)
    
    ;(: managed-ctxt (U #f GL-Context))
    (define managed-ctxt #f)
    
    ;(: get-managed-gl-context (-> GL-Context))
    (define/public (get-managed-gl-context)
      (define mctxt managed-ctxt)
      (cond
        [mctxt  mctxt]
        [else
         (define ctxt (send (send this get-dc) get-gl-context))
         (cond
           [(or (not ctxt) (not (send ctxt ok?)))
            (log-pict3d-warning "<canvas> could not get canvas OpenGL context (legacy? = ~a)" legacy?)
            (error 'pict3d-canvas% "could not get canvas OpenGL context (legacy? = ~a)" legacy?)]
           [(send ctxt call-as-current (λ () (gl-version-at-least? 30)))
            (define version (send ctxt call-as-current gl-version))
            (log-pict3d-info "<canvas> got canvas OpenGL ~a context (legacy? = ~a)" version legacy?)
            (let ([mctxt  (managed-gl-context ctxt)])
              (set! managed-ctxt mctxt)
              mctxt)]
           [else
            (define version (send ctxt call-as-current gl-version))
            (log-pict3d-warning "<canvas> got canvas OpenGL ~a context (legacy? = ~a)"
                                version legacy?)
            (error 'pict3d-canvas% "could not get at least an OpenGL 30 context (legacy? = ~a)"
                   legacy?)])]))
    
    (define/override (on-paint)
      (define-values (width height) (get-gl-window-size))
      (do-render pict width height
                 z-near z-far fov-degrees
                 background ambient
                 #t)
      (super on-paint))
    ))
