#lang racket/base

(require racket/gui
         racket/class
         racket/async-channel
         racket/contract
         racket/list
         racket/match
         racket/fixnum
         racket/promise
         racket/math
         math/flonum
         typed/opengl
         "../math/flv3.rkt"
         "../math/flt3.rkt"
         "../math/flrect3.rkt"
         "../engine/scene.rkt"
         "../engine/utils.rkt"
         "../engine/draw-pass.rkt"
         "../engine/draw-passes.rkt"
         "../gl.rkt"
         "../utils.rkt"
         "pict3d-struct.rkt"
         "parameters.rkt"
         "utils.rkt"
         "axes-scene.rkt"
         )

;(provide snip-class)

;; ===================================================================================================
;; Parameters

(define physics-delay 16)
(define physics-timeout 500)

(define move-accel 25.0)
(define friction-accel -10.0)

;; ===================================================================================================
;; Types
#;
(define-type Pict3D%
  (Class #:implements Snip%
         (init-field [scene  Scene]
                     [legacy?  Boolean]
                     [width   Positive-Index]
                     [height  Positive-Index]
                     [z-near  Flonum]
                     [z-far   Flonum]
                     [fov-degrees  Flonum]
                     [background  FlVector]
                     [ambient-color  FlVector]
                     [ambient-intensity  Flonum])
         [get-init-params  (-> (Values Positive-Index Positive-Index Flonum Flonum Flonum
                                       FlVector FlVector Flonum))]
         [get-scene  (-> Scene)]
         [set-argb-pixels  (-> Bytes Void)]
         ))
#;
(define-type Pict3D-GUI%
  (Class (init-field [pict (Instance Pict3D%)])
         [is-capturing?  (-> Boolean)]
         [own-caret  (-> Any Void)]
         [on-event  (-> (Instance Mouse-Event%) Void)]
         [on-char   (-> (Instance Key-Event%) Void)]))

;; ===================================================================================================
;; Rendering threads

(define get-the-bytes
  (make-gl-cached-vector
   'get-the-bytes
   (λ (n)
     (log-pict3d-info "<snip> creating temp ARGB bytes of length ~v" n)
     (make-bytes n))
   bytes-length))

(define-values (standard-over-light standard-under-light)
  (let ([direction  (flvector -0.25 -0.5 -1.0)]
        [color  (flvector 1.0 1.0 1.0)]
        [intensity  1.0])
    (values
     (shape->scene (make-directional-light-shape color intensity direction))
     (shape->scene (make-directional-light-shape color (* intensity 0.5) (flv3neg direction))))))

;(: make-snip-render-thread (-> (Instance Pict3D%) (Async-Channelof FlAffine3-) Thread))
(define (make-snip-render-thread snip ch)
  (define (render-thread-loop)
    ;; Wait for a view matrix
    (define view
      (let ([view  (async-channel-get ch)])
        ;; Empty the queue looking for the lastest one
        (let loop ([view view])
          (define new-view (async-channel-try-get ch))
          (cond [new-view  (loop new-view)]
                [else  view]))))
    
    (define-values (_ cpu real gc)
      (time-apply
       (λ ()
         (define-values
           (legacy? width height znear zfar fov-degrees background ambient-color ambient-intensity)
           (send snip get-init-params))
         ;; Compute a projection matrix
         (define fov-radians (degrees->radians (fl fov-degrees)))
         (define proj
           (flt3compose
            (scale-flt3 (flvector 1.0 -1.0 1.0))  ; upside-down: OpenGL origin is lower-left
            (perspective-flt3/viewport (fl width) (fl height) fov-radians znear zfar)))
         
         ;; Lock everything up for drawing
         (with-gl-context (get-master-gl-context legacy?)
           ;; Draw the scene, an origin, and a couple of lights
           (define s (send snip get-scene))
           (define scenes
             (list* s
                    axes-scene
                    standard-over-light
                    standard-under-light
                    (map (λ (t) (make-trans-scene t basis-scene))
                         (scene-all-group-transforms s))))
           (draw-scenes scenes width height view proj background ambient-color ambient-intensity)
           ;; Get the resulting pixels and set them into the snip's bitmap
           (define bs (get-the-bytes (* 4 width height)))
           (glReadPixels 0 0 width height GL_BGRA GL_UNSIGNED_INT_8_8_8_8 bs)
           (send snip set-argb-pixels bs)))
       empty))
    
    (log-pict3d-debug "<snip> heap size: ~a cpu time: ~a real time: ~a gc time: ~a"
                      (real->decimal-string (/ (current-memory-use) (* 1024 1024)) 2)
                      cpu real gc)
    
    (render-thread-loop))
  
  (thread render-thread-loop))

;; ===================================================================================================
;; The snip's GUI

;(: pict3d-gui% Pict3D-GUI%)
(define pict3d-gui%
  (class object%
    (init-field pict)
    
    (super-new)
    
    ;(: render-queue (Async-Channel FlAffine3-))
    (define render-queue (make-async-channel))
    
    ;(: render-thread Thread)
    (define render-thread (make-snip-render-thread pict render-queue))
    
    ;(: camera (Instance Camera%))
    (define camera
      (let ([t  (pict3d-view-transform (pict3d (send pict get-scene)))])
        (match-define (list m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
          (flvector->list (fltransform3-inverse t)))
        (define position (flvector m03 m13 m23))
        (define yaw (+ (atan m12 m02) (/ pi 2)))
        (define pitch (- (asin (/ m22 (flsqrt (+ (sqr m02) (sqr m12) (sqr m22)))))))
        (new camera%
             [position  position]
             [velocity  (flvector 0.0 0.0 0.0)]
             [yaw  yaw]
             [pitch  pitch])))
    
    ;(: last-view-matrix (U #f FlAffine3-))
    (define last-view-matrix #f)
    
    ;(: maybe-redraw (-> Void))
    (define (maybe-redraw)
      (define view-matrix (send camera get-view-matrix))
      (unless (equal? view-matrix last-view-matrix)
        (set! last-view-matrix view-matrix)
        (async-channel-put render-queue view-matrix)))
    
    (maybe-redraw)
    
    ;(: capturing? Boolean)
    (define capturing? #f)
    
    (define/public (is-capturing?) capturing?)
    
    ;(: frame-timer (U #f (Instance Timeout-Timer%)))
    (define frame-timer #f)
    
    ;(: stop-frame-timer (-> Void))
    (define (stop-frame-timer)
      (define frame-timer-val frame-timer)
      (when frame-timer-val
        (send frame-timer-val timeout)
        (set! frame-timer #f)))
    
    ;(: start-frame-timer (-> Void))
    (define (start-frame-timer)
      (define frame-timer-val frame-timer)
      (if frame-timer-val
          (send frame-timer-val keep-alive)
          (set! frame-timer (make-object timeout-timer%
                              timer-tick
                              (λ () (set! frame-timer #f))
                              physics-delay
                              physics-timeout))))
    
    (define/public (own-caret own-it?)
      (unless own-it?
        (set! capturing? #f)
        (hash-clear!* key-hash)
        (stop-frame-timer)))
    
    ;(: home-mouse-x Integer)
    ;(: home-mouse-y Integer)
    (define home-mouse-x 0)
    (define home-mouse-y 0)
    
    ;(: yaw-vel Flonum)
    ;(: pitch-vel Flonum)
    (define yaw-vel 0.0)
    (define pitch-vel 0.0)
    
    (define/public (on-event e)
      (define admin (send pict get-admin))
      (define editor (and admin (send admin get-editor)))
      (case (send e get-event-type)
        [(left-down)
         (define-values (x y) (snip-center-pointer pict))
         (cond [capturing?
                (cond [editor  (send editor set-caret-owner #f)]
                      [else    (set! capturing? #f)
                               (stop-frame-timer)])]
               [(and x y)
                (set! home-mouse-x x)
                (set! home-mouse-y y)
                (set! capturing? #t)
                (start-frame-timer)])]
        [(right-down middle-down)  (when editor
                                     (send editor set-caret-owner #f)
                                     (send editor on-local-event e))]
        ;; Just in case (we don't want these events anyway):
        [(right-up middle-up)  (when editor
                                 (send editor on-local-event e))]
        [(motion)
         (when capturing?
           (define x (send e get-x))
           (define y (send e get-y))
           (define dx (max -50 (min 50 (- x home-mouse-x))))
           (define dy (max -50 (min 50 (- y home-mouse-y))))
           (unless (and (= dx 0) (= dy 0))
             (set! yaw-vel (+ yaw-vel (fl (* 0.002 dx))))
             (set! pitch-vel (+ pitch-vel (fl (* 0.002 dy))))
             (define-values (x y) (snip-center-pointer pict))
             (cond [(and x y)  (set! home-mouse-x x)
                               (set! home-mouse-y y)
                               (start-frame-timer)]
                   [editor  (send editor set-caret-owner #f)]
                   [else    (set! capturing? #f)
                            (stop-frame-timer)])))]))
    
    ;(: key-hash (HashTable (U Char Symbol) #t))
    (define key-hash (make-hasheq empty))
    
    ;(: key-pressed? (-> (U Char Symbol) Boolean))
    (define/private (key-pressed? code)
      (hash-ref key-hash code #f))
    
    ;(: keys-pressed? (-> (Listof (U Char Symbol)) Boolean))
    (define/private (keys-pressed? codes)
      (ormap (λ (code) (key-pressed? code)) codes))
    
    (define/public (on-char e)
      (define code (send e get-key-code))
      (case code
        [(release)  (hash-remove! key-hash (send e get-key-release-code))]
        [else  (hash-set! key-hash code #t)])
      (when (not frame-timer)
        (set! last-frame-time (- (fl (current-inexact-milliseconds))
                                 (fl physics-delay)))
        (send camera set-velocity (flvector 0.0 0.0 0.0))
        (update-camera)
        (maybe-redraw)
        (start-frame-timer)))
    
    ;(: last-frame-time Flonum)
    (define last-frame-time (fl (current-inexact-milliseconds)))
    
    ;(: update-camera (-> Void))
    (define (update-camera)
      (define frame-time (fl (current-inexact-milliseconds)))
      (define frame-delay (max 1.0 (min 100.0 (- frame-time last-frame-time))))
      (set! last-frame-time frame-time)
      (define dt (/ frame-delay 1000.0))
      
      (let* ([acc  (flvector 0.0 0.0 0.0)]
             [acc  (if (keys-pressed? '(#\a left))
                       (flv3+ acc (flv3* (flvector -1.0 0.0 0.0) move-accel))
                       acc)]
             [acc  (if (keys-pressed? '(#\d right))
                       (flv3+ acc (flv3* (flvector +1.0 0.0 0.0) move-accel))
                       acc)]
             [acc  (if (keys-pressed? '(#\f next))
                       (flv3+ acc (flv3* (flvector 0.0 -1.0 0.0) move-accel))
                       acc)]
             [acc  (if (keys-pressed? '(#\r prior))
                       (flv3+ acc (flv3* (flvector 0.0 +1.0 0.0) move-accel))
                       acc)]
             [acc  (if (keys-pressed? '(#\w up))
                       (flv3+ acc (flv3* (flvector 0.0 0.0 -1.0) move-accel))
                       acc)]
             [acc  (if (keys-pressed? '(#\s down))
                       (flv3+ acc (flv3* (flvector 0.0 0.0 +1.0) move-accel))
                       acc)]
             [_    (unless (equal? acc (flvector 0.0 0.0 0.0))
                     (start-frame-timer))]
             [acc  (send camera rotate-direction acc)]
             [acc  (flv3+ acc (flv3* (send camera get-velocity) friction-accel))])
        (send camera accelerate acc dt))
      
      (send camera change-angles yaw-vel pitch-vel)
      (set! yaw-vel (* yaw-vel #i1/3))
      (set! pitch-vel (* pitch-vel #i1/3))
      )
    
    ;(: timer-tick (-> Void))
    (define (timer-tick)
      (update-camera)
      (maybe-redraw))
    ))

;; ===================================================================================================
;; The snip

(define blank-cursor (make-object cursor% 'blank))

(define black-pen (make-object pen% "black" 1 'solid))
(define white-pen (make-object pen% "white" 1 'solid))
(define trans-brush (make-object brush% "black" 'transparent))

#|
(define pict3d-snip-class%
  (class snip-class%
    (super-make-object)
    
    (send this set-classname (format "~s" '(lib "pict3d-snip.rkt" "pict3d" "private" "gui")))
    (send this set-version 0)
    
    (define/override (read f)
      (error 'read "pict3d does not support cut, copy and paste yet"))
    ))

(define snip-class (make-object pict3d-snip-class%))
(send (get-the-snip-class-list) add snip-class)
|#

;(: pict3d% Pict3D%)
(define pict3d%
  (class image-snip%
    (init-field scene
                legacy?
                width
                height
                z-near
                z-far
                fov-degrees
                background
                ambient-color
                ambient-intensity)
    
    (super-make-object)
    
    ;(send this set-snipclass snip-class)
    ;(send (get-the-snip-class-list) add snip-class)
    
    (define/public (get-scene) scene)
    
    ;(: copy (-> (Instance Pict3D%)))
    (define/override (copy)
      (make-object pict3d% scene legacy?
        width height z-near z-far fov-degrees background ambient-color ambient-intensity))
    
    (define/override (write f)
      (error 'write "pict3d does not support cut, copy and paste yet"))
    
    ;(: the-bitmap (U #f (Instance Bitmap%)))
    (define the-bitmap #f)
    
    ;(: get-the-bitmap (-> (Instance Bitmap%)))
    (define (get-the-bitmap)
      (define the-bitmap-val the-bitmap)
      (if the-bitmap-val
          the-bitmap-val
          (let ([the-bitmap-val  (make-bitmap width height)])
            (set! the-bitmap the-bitmap-val)
            the-bitmap-val)))
    
    (define/public (get-init-params)
      (values legacy?
              width height z-near z-far fov-degrees background ambient-color ambient-intensity))
    
    (define/public (set-argb-pixels bs)
      (define len (* width height 4))
      (when (not (<= len (bytes-length bs)))
        (raise-argument-error 'set-argb-pixels
                              (format "bytes of length at least ~a" len)
                              (bytes-length bs)))
      (send (get-the-bitmap) set-argb-pixels 0 0 width height bs #f #t)
      (define admin (send this get-admin))
      (when admin
        (send admin needs-update this 0 0 (+ width 4) (+ height 4))))
    
    ;(: gui (U #f (Instance Pict3D-GUI%)))
    (define gui #f)
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (unless gui
        (send this set-flags (list* 'handles-events 'handles-all-mouse-events (send this get-flags)))
        (set! gui (make-object pict3d-gui% this)))
      (send dc set-brush trans-brush)
      (send dc set-pen black-pen)
      (send dc draw-rectangle (+ x 0.5) (+ y 0.5) (+ width 4) (+ height 4))
      (send dc set-pen white-pen)
      (send dc draw-rectangle (+ x 1.5) (+ y 1.5) (+ width 2) (+ height 2))
      (send dc draw-bitmap (get-the-bitmap) (+ x 2) (+ y 2))
      #;
      (super draw dc x y left top right bottom dx dy draw-caret))
    
    ;; Can't use this because of an error in TR
    (define/override (get-extent dc x y [w #f] [h #f] [descent #f] [space #f] [lspace #f] [rspace #f])
      (when (box? w) (set-box! w (+ width 4)))
      (when (box? h) (set-box! h (+ height 4)))
      (when (box? descent) (set-box! descent 0))
      (when (box? space) (set-box! space 0))
      (when (box? lspace) (set-box! lspace 0))
      (when (box? rspace) (set-box! rspace 0)))
    
    #;; This works around it
    (define/override (get-extent dc x y . #{args : (Listof (U #f (Boxof Nonnegative-Real)))})
      (match-define (list w h descent space lspace rspace) args)
      (when (box? w) (set-box! w (+ width 4)))
      (when (box? h) (set-box! h (+ height 4)))
      (when (box? descent) (set-box! descent 0))
      (when (box? space) (set-box! space 0))
      (when (box? lspace) (set-box! lspace 0))
      (when (box? rspace) (set-box! rspace 0)))
    
    (define/override (own-caret own-it?)
      (let ([gui-val  gui])
        (and gui-val (send gui-val own-caret own-it?)))
      (super own-caret own-it?))
    
    (define/override (on-event dc x y editorx editory e)
      (let ([gui-val  gui])
        (and gui-val (send gui-val on-event e)))
      (super on-event dc x y editorx editory e))
    
    (define/override (on-char dc x y editorx editory e)
      (let ([gui-val  gui])
        (and gui-val (send gui-val on-char e)))
      (super on-char dc x y editorx editory e))
    
    (define/override (adjust-cursor dc x y editorx editory evt)
      (if (let ([gui-val gui])
            (and gui-val (send gui-val is-capturing?)))
          blank-cursor
          #f))
    ))

(define (pict3d->pict3d% p)
  (define scene (pict3d-scene p))
  (make-object pict3d%
    scene
    (pict3d-legacy-contexts?)
    (current-pict3d-width)
    (current-pict3d-height)
    (current-pict3d-z-near)
    (current-pict3d-z-far)
    (current-pict3d-fov-degrees)
    (current-pict3d-background)
    (current-pict3d-ambient-color)
    (current-pict3d-ambient-intensity)))

(define (pict3d-custom-write p out mode)
  (define print-it
    (cond [(eq? mode #t)  write]
          [(eq? mode #f)  display]
          [else  print]))
  (print-it (pict3d->pict3d% p) out))

(define (pict3d-print-converter p recur)
  (pict3d->pict3d% p))

;; Set the custom printer so Pict3D instances will print nicely in Racket
(current-pict3d-custom-write pict3d-custom-write)
;; Set the print converter so Pict3D instances will print nicely in HTDP languages
(current-pict3d-print-converter pict3d-print-converter)
