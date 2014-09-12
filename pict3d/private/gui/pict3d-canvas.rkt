#lang typed/racket/base

(require racket/match
         racket/math
         math/flonum
         typed/racket/gui
         typed/racket/class
         typed/racket/async-channel
         "../math/flv3.rkt"
         "../math/flt3.rkt"
         "../engine/scene.rkt"
         "../engine/gl.rkt"
         "../engine/draw-pass.rkt"
         "../engine/draw-passes.rkt"
         "../utils.rkt"
         "pict3d-snip.rkt"
         )

(provide Pict3D-Canvas%
         pict3d-canvas%)

(define-type Pict3D-Canvas%
  (Class #:implements Canvas%
         (init [parent  (Instance Area-Container<%>)]
               [style   (Listof
                         (U 'transparent
                            'border
                            'vscroll
                            'hscroll
                            'deleted
                            'control-border
                            'combo
                            'resize-corner
                            'no-focus))
                        #:optional]
               [label    (U #f String) #:optional]
               [enabled  Any #:optional]
               [vert-margin   Natural #:optional]
               [horiz-margin  Natural #:optional]
               [min-width   (U #f Natural) #:optional]
               [min-height  (U #f Natural) #:optional]
               [stretchable-width   Any #:optional]
               [stretchable-height  Any #:optional])
         (init-field [pict  Pict3D #:optional])
         [set-pict3d  (-> Pict3D Void)]
         [get-pict3d  (-> Pict3D)]
         [get-managed-gl-context  (-> gl-context)]
         ))

;; ===================================================================================================
;; Rendering threads

(struct render-command ([pict3d : Pict3D]
                        [width : Index]
                        [height : Index]
                        [z-near : Positive-Flonum]
                        [z-far : Positive-Flonum]
                        [fov-degrees : Positive-Flonum]
                        [ambient : FlVector]
                        ) #:transparent)

(: make-canvas-render-thread (-> (Instance Pict3D-Canvas%) (Async-Channelof render-command) Thread))
(define (make-canvas-render-thread canvas ch)
  (: render-thread-loop (-> Void))
  (define (render-thread-loop)
    ;; Wait for a scene and view matrix
    (: cmd render-command)
    (define cmd
      (let ([cmd  (async-channel-get ch)])
        ;; Empty the queue looking for the lastest one
        (let loop ([cmd  cmd])
          (define new-cmd (async-channel-try-get ch))
          (if new-cmd (loop new-cmd) cmd))))
    
    ;(values
    ;(profile
    (time
     (match-define (render-command pict width height znear zfar fov-degrees ambient) cmd)
     ;; Get the view matrix
     (define view (pict3d-view-transform pict))
     ;; Compute the projection matrix
     (define fov-radians (degrees->radians fov-degrees))
     (define proj (perspective-flt3/viewport (fl width) (fl height) fov-radians znear zfar))
     
     ;; Lock everything up for drawing
     (with-gl-context (send canvas get-managed-gl-context)
       ;; Draw the scene and swap buffers
       (draw-scene (send pict get-scene) width height view proj ambient)
       (gl-swap-buffers))
     )
    (render-thread-loop))
  
  (thread render-thread-loop))

;; ===================================================================================================
;; Scene canvas

(: pict3d-canvas% Pict3D-Canvas%)
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
    
    (define config (new gl-config%))
    (send config set-legacy? #f)
    (send config set-share-context (get-gl-context (get-master-gl-context)))
    
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
    
    (define render-queue ((inst make-async-channel render-command)))
    
    (: render-thread Thread)
    (define render-thread (make-canvas-render-thread this render-queue))
    
    (: last-width (U #f Index))
    (: last-height (U #f Index))
    (define last-width #f)
    (define last-height #f)
    
    (: get-gl-window-size (-> (Values Index Index)))
    (define (get-gl-window-size)
      (define-values (w h) (send (send this get-dc) get-size))
      (values (assert (exact-floor w) index?)
              (assert (exact-floor h) index?)))
    
    (: z-near Positive-Flonum)
    (: z-far Positive-Flonum)
    (: fov-degrees Positive-Flonum)
    (: ambient FlVector)
    (define z-near (current-z-near))
    (define z-far (current-z-far))
    (define fov-degrees (current-fov-degrees))
    (define ambient (current-ambient))
    
    (define/public (set-pict3d new-pict)
      (set! pict new-pict)
      (define-values (width height) (get-gl-window-size))
      (set! last-width width)
      (set! last-height height)
      (set! z-near (current-z-near))
      (set! z-far (current-z-far))
      (set! fov-degrees (current-fov-degrees))
      (set! ambient (current-ambient))
      (async-channel-put
       render-queue
       (render-command new-pict width height z-near z-far fov-degrees ambient)))
    
    (define/public (get-pict3d) pict)
    
    (: managed-ctxt (U #f gl-context))
    (define managed-ctxt #f)
    
    (: get-managed-gl-context (-> gl-context))
    (define/public (get-managed-gl-context)
      (define ctxt (send (send this get-dc) get-gl-context))
      (define mctxt managed-ctxt)
      (cond [(or (not ctxt) (not (send ctxt ok?)))
             (error 'get-managed-context "no GL context is available")]
            [(or (not mctxt)
                 (not (eq? ctxt (get-gl-context mctxt))))
             (let ([mctxt  (managed-gl-context ctxt)])
               (set! managed-ctxt mctxt)
               mctxt)]
            [else  mctxt]))
    
    (define/override (on-paint)
      (define-values (width height) (get-gl-window-size))
      (when (not (and (equal? width last-width)
                      (equal? height last-height)))
        (set! last-width width)
        (set! last-height height)
        (async-channel-put
         render-queue
         (render-command pict width height z-near z-far fov-degrees ambient))))

    ))
