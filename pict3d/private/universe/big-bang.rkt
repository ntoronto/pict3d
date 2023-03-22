#lang typed/racket/base

#|
TODO

Pad overlay
Separate state window
Universe/networking

|#

(require racket/match
         typed/racket/gui
         typed/racket/draw
         typed/racket/class
         typed/racket/async-channel
         (for-syntax racket/base
                     version/utils)
         "../lazy-gui.rkt"
         "../gui/pict3d-canvas.rkt")

(provide Pict3D-World-Canvas%
         pict3d-world-canvas%
         big-bang3d)

(define-type Pict3D-World-Canvas%
  (Class #:implements Pict3D-Canvas%
         (init [parent  (Instance Area-Container<%>)]
               [gl-config (Instance GL-Config%)])
         (init-field [on-key (-> Boolean String Void)]
                     [on-mouse (-> Integer Integer String Void)]
                     [on-start (-> Void)]
                     [pict3d Pict3D #:optional])))

(: pict3d-world-canvas% Pict3D-World-Canvas%)
(define pict3d-world-canvas%
  (class pict3d-canvas%
    (init parent gl-config)
    (init-field on-key on-mouse on-start)
    
    (super-new [parent parent]
               [gl-config gl-config]
               [style '()]
               [label #f]
               [enabled #t]
               [vert-margin 0]
               [horiz-margin 0]
               [min-width #f]
               [min-height #f]
               [stretchable-width #t]
               [stretchable-height #t])
    
    (define (code->string k)
      (cond [(symbol? k)  (symbol->string k)]
            [(char? k)    (make-string 1 k)]
            [else  (error 'on-key "unknown key code type ~e" k)]))
    
    (define/override (on-char evt)
      (define code (send evt get-key-code))
      (when (eq? code 'f12)
        (displayln (send this get-pict3d)))
      (cond [(eq? code 'release)
             (on-key #t (code->string (send evt get-key-release-code)))]
            [else
             (on-key #f (code->string code))]))
    
    (define/override (on-event evt)
      (define type (send evt get-event-type))
      (define str
        (if (eq? type 'motion)
            (if (send evt dragging?) "drag" "move")
            (symbol->string type)))
      (on-mouse (send evt get-x) (send evt get-y) str))
    
    (: painted? Boolean)
    (define painted? #f)
    
    (define/override (on-paint)
      (super on-paint)
      (when (not painted?)
        (set! painted? #t)
        (on-start)))
    ))

(struct key ([release? : Boolean] [code : String]) #:transparent)
(struct mouse ([x : Integer] [y : Integer] [type : String]) #:transparent)
(define-type Input (U key mouse))

(: big-bang3d
   (All (S) (-> S
                [#:valid-state? (-> S Natural Flonum Boolean)]
                [#:pause-state? (-> S Natural Flonum Boolean)]
                [#:stop-state? (-> S Natural Flonum Boolean)]
                [#:name String]
                [#:width Positive-Integer]
                [#:height Positive-Integer]
                [#:x (U Integer False)]
                [#:y (U Integer False)]
                [#:display-mode (U 'normal 'fullscreen 'hide-menu-bar)]
                [#:gl-config (Instance GL-Config%)]
                [#:cursor (U (Instance Cursor%) False)]
                [#:frame-delay Positive-Real]
                [#:on-frame (-> S Natural Flonum S)]
                [#:on-draw (-> S Natural Flonum Pict3D)]
                [#:on-key (-> S Natural Flonum String S)]
                [#:on-release (-> S Natural Flonum String S)]
                [#:on-mouse (-> S Natural Flonum Integer Integer String S)]
                S)))
(define (big-bang3d
         init-state
         #:valid-state? [valid-state? (λ ([s : S] [n : Natural] [t : Flonum]) #t)]
         #:pause-state? [pause-state? (λ ([s : S] [n : Natural] [t : Flonum]) #f)]
         #:stop-state? [stop-state? (λ ([s : S] [n : Natural] [t : Flonum]) #f)]
         #:name [name "World3D"]
         #:width [width 512]
         #:height [height 512]
         #:x [frame-x #f]
         #:y [frame-y #f]
         #:display-mode [display-mode 'normal]
         #:gl-config [gl-config (pict3d-default-gl-config)]
         #:cursor [cursor #f]
         #:frame-delay [orig-frame-delay #i1000/30]
         #:on-frame [on-frame (λ ([s : S] [n : Natural] [t : Flonum]) s)]
         #:on-draw [on-draw (λ ([s : S] [n : Natural] [t : Flonum]) empty-pict3d)]
         #:on-key [on-key (λ ([s : S] [n : Natural] [t : Flonum] [k : String]) s)]
         #:on-release [on-release (λ ([s : S] [n : Natural] [t : Flonum] [k : String]) s)]
         #:on-mouse [on-mouse (λ ([s : S] [n : Natural] [t : Flonum]
                                          [x : Integer] [y : Integer] [e : String]) s)]
         ;; For networked worlds
         ;#:on-receive [on-receive #f]
         ;#:register [register #f]
         ;#:port [port #f]
         )
  (define frame-delay (real->double-flonum orig-frame-delay))
  
  (define first-frame-time (current-inexact-milliseconds))
  
  (unless (valid-state? init-state 0 0.0)
    (error 'valid-state?
           "the initial state is ~e at 0 0.0, for which valid-state? returns #f"
           init-state))
  
  (: running? Boolean)
  (define running? (not (stop-state? init-state 0 0.0)))
  
  ;; Posted when the canvas first paints something
  (define start-sema (make-semaphore))
  
  ;; Input events are sent over this channel and handled in the main loop below, which is
  ;; in the user's thread and eventspace
  (: event-channel (Async-Channelof Input))
  (define event-channel (make-async-channel))
  
  ;; Used for 'hide-menu-bar style:
  (: get-frame-position (-> (Values Integer Integer (U Integer False) (U Integer False))))
  (define (get-frame-position)
    (define-values (dx dy) (get-display-left-top-inset))
    (define-values (w h) (get-display-size #t))
    (values (or w width) (or h height) (and dx (- dx)) (and dy (- dy))))
  
  (: mode-width Integer)
  (: mode-height Integer)
  (: mode-frame-x (U Integer False))
  (: mode-frame-y (U Integer False))
  (define-values (mode-width mode-height mode-frame-x mode-frame-y)
    (case display-mode
      [(normal fullscreen) (values width height frame-x frame-y)]
      [(hide-menu-bar) (get-frame-position)]))
  
  (define window (new (class frame%
                        ;; Subclass to handle resize for 'hide-menu-bar mode:
                        (super-new)
                        (inherit move resize)
                        (define/augment (display-changed)
                          (case display-mode
                            [(hide-menu-bar)
                             (let-values ([(w h x y) (get-frame-position)])
                               (when (and x y)
                                 (move x y))
                               (resize w h))]
                            [else (void)])))
                      [label name]
                      [width mode-width]
                      [height mode-height]
                      [x mode-frame-x]
                      [y mode-frame-y]
                      [style (if (eq? display-mode 'hide-menu-bar)
                                 '(no-resize-border no-caption hide-menu-bar)
                                 '(fullscreen-button))]))
  (send window fullscreen (eq? display-mode 'fullscreen))
  
  (define canvas
    (new pict3d-world-canvas%
         [parent window]
         [gl-config gl-config]
         ;; Key handler: throw everything into event-channel (if running)
         [on-key
          (λ ([r? : Boolean] [k : String])
            (when running? (async-channel-put event-channel (key r? k))))]
         ;; Mouse handler: throw everything into event-channel (if running)
         [on-mouse
          (λ ([x : Integer] [y : Integer] [e : String])
            (when running? (async-channel-put event-channel (mouse x y e))))]
         ;; Start handler: post to the semaphore so the main loop can start
         [on-start  (λ () (semaphore-post start-sema))]
         ;; Initial pict
         [pict3d  (on-draw init-state 0 0.0)]))
  
  (when cursor
    (send canvas set-cursor cursor))
  
  (send window show #t)
  (send canvas focus)
  
  ;; Give the GUI thread a chance to work through its event queue and show the window,
  ;; and wait until after the first paint
  (yield start-sema)
  
  (: current-state S)
  (define current-state init-state)
  
  (: frame Natural)
  (define frame 0)
  
  (: frame-time Flonum)
  (define frame-time 0.0)
  
  (: paused? Boolean)
  (define paused? #f)
  (: paused-time Flonum)
  (define paused-time 0.0)
  (: start-pause-time Flonum)
  (define start-pause-time 0.0)

  (: set-current-state! (-> Symbol S Void))
  ;; Sets the current state, checks valid-state?, pause-state?, and stop-state?
  (define (set-current-state! setter new-state)
    ;; valid?
    (unless (valid-state? new-state frame frame-time)
      (set! running? #f)
      (error 'valid-state?
             "~a handler returned ~e at ~a ~a, for which valid-state? returns #f"
             setter
             new-state
             frame
             frame-time))
    ;; pause?
    (if (pause-state? new-state frame frame-time)
        (unless paused?
          (set! paused? #t)
          (set! start-pause-time (current-inexact-milliseconds)))
        (when paused?
          (set! paused? #f)
          (set! paused-time (+ paused-time
                               (- (current-inexact-milliseconds) start-pause-time)))))
    ;; stop?
    (set! current-state new-state)
    (when (stop-state? new-state frame frame-time)
      (set! running? #f)))
  
  ;; Main loop
  (let loop ()
    (when (and running? (send window is-shown?))
      (define-syntax (version-must-be-after-6.3 stx)
        (syntax-case stx ()
          [(_ form)
           (if (version<? "6.3" (version))
               #'form
               (syntax/loc stx (void)))]))
      
      (version-must-be-after-6.3
       (collect-garbage 'incremental))
      ;; Mark the start of the frame
      (define start (real->double-flonum (current-inexact-milliseconds)))
      (set! frame (+ frame 1))
      (set! frame-time (- start first-frame-time paused-time))
      ;; Call the user's on-frame handler
      (set-current-state! 'on-frame (on-frame current-state frame frame-time))
      ;; Work through all the input events
      (let event-loop ()
        (match (if paused?
                   (yield event-channel)
                   (async-channel-try-get event-channel))
          ;; Key events
          [(key release? code)
           (cond
             ;; Release key codes
             [release?
              (set-current-state! 'on-release (on-release current-state frame frame-time code))]
             ;; Regular key codes
             [else
              (set-current-state! 'on-key (on-key current-state frame frame-time code))])
           (event-loop)]
          ;; Mouse events
          [(mouse x y e)
           (set-current-state! 'on-mouse (on-mouse current-state frame frame-time x y e))
           (event-loop)]
          [_
           (void)]))
      ;; Call the user's on-draw and set the result in the canvas
      (send canvas set-pict3d (on-draw current-state frame frame-time))
      ;; Determine how much time is left in the frame
      (define duration (max 0.0 (- (real->double-flonum (current-inexact-milliseconds)) start)))
      (define delay (/ (max 1.0 (- frame-delay duration)) 1000.0))
      ;; Sleep for the remainder of the frame and let GUI events run
      (sleep/yield delay)
      (loop)))
  
  (set! running? #f)
  ;; Clean out the event queue
  (let event-loop ()
    (when (async-channel-try-get event-channel)
      (event-loop)))
  ;; Return the final state
  current-state)
