#lang racket/base

(require racket/gui/base
         racket/match
         racket/list
         racket/draw
         racket/class
         racket/async-channel
         racket/math
         typed/opengl
         "../gl.rkt"
         "../utils.rkt"
         "parameters.rkt"
         "pict3d-struct.rkt"
         "pict3d-draw.rkt"
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
   fov
   background
   ambient
   auto-camera
   ack-channel)
  #:transparent)

(define gui-provides-gl-scale-support?
  ;; Version 1.16 of gui-lib adds `get-gl-client-size`
  (method-in-interface? 'get-gl-client-size (class->interface canvas%)))

(define (render cmd canvas)
  (define-values (_ cpu real gc)
    (time-apply
     (λ ()
       (match-define (render-command pict width height
                                     z-near z-far fov
                                     background ambient auto-camera
                                     ack-channel)
         cmd)
       
       (with-gl-context (send canvas get-managed-gl-context)
         (draw-pict3ds (list pict)
                       #:width width
                       #:height height
                       #:camera auto-camera
                       #:z-near z-near
                       #:z-far z-far
                       #:fov fov
                       #:background background
                       #:ambient ambient
                       #:bitmap? #f)
         (gl-swap-buffers))
       
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
    (init-field [pict3d  empty-pict3d])
    
    (define legacy? (current-pict3d-legacy?))
    (define check-version? (current-pict3d-check-version?))
    
    (define config (new gl-config%))
    (send config set-legacy? legacy?)
    (when gui-provides-gl-scale-support?
      (send config set-hires-mode #t))
    
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
      ;; Version 1.16 of gui-lib adds `get-gl-client-size`
      (cond
       [gui-provides-gl-scale-support?
        (send this get-gl-client-size)]
       [else
        (define-values (w h) (send (send this get-dc) get-size))
        (values (exact-ceiling w)
                (exact-ceiling h))]))
    
    (define z-near (current-pict3d-z-near))
    (define z-far (current-pict3d-z-far))
    (define fov (current-pict3d-fov))
    (define background (current-pict3d-background))
    (define ambient (current-pict3d-ambient))
    (define auto-camera (current-pict3d-auto-camera))
    
    (define (do-render new-pict width height
                       z-near z-far fov
                       background ambient auto-camera
                       async?)
      (define ack-channel (if async? #f (make-channel)))
      (async-channel-put
       render-queue
       (render-command new-pict width height
                       z-near z-far fov
                       background ambient auto-camera
                       ack-channel))
      (when ack-channel
        (channel-get ack-channel)))
    
    (define/public (set-pict3d new-pict)
      (set! pict3d new-pict)
      (define-values (width height) (get-gl-window-size))
      (set! z-near (current-pict3d-z-near))
      (set! z-far (current-pict3d-z-far))
      (set! fov (current-pict3d-fov))
      (set! background (current-pict3d-background))
      (set! ambient (current-pict3d-ambient))
      (set! auto-camera (current-pict3d-auto-camera))
      (do-render new-pict width height
                 z-near z-far fov
                 background ambient auto-camera
                 async-updates?))
    
    (define/public (get-pict3d) pict3d)
    
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
           [(or (not check-version?) (send ctxt call-as-current (λ () (gl-version-at-least? 30))))
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
      (do-render pict3d width height
                 z-near z-far fov
                 background ambient auto-camera
                 #t)
      (super on-paint))
    ))
