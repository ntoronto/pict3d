#lang typed/racket/base

(require racket/list
         racket/match
         racket/fixnum
         racket/promise
         racket/math
         math/flonum
         typed/racket/gui
         typed/racket/class
         typed/racket/async-channel
         typed/opengl
         "../math/flv3.rkt"
         "../math/flt3.rkt"
         "../math/flaabb3.rkt"
         "../utils.rkt"
         "../engine/flscene3.rkt"
         "../engine/gl.rkt"
         "../engine/utils.rkt"
         "../engine/shape.rkt"
         "../engine/draw-pass.rkt"
         "../engine/draw-passes.rkt"
         "axes-scene.rkt"
         )

(provide (rename-out [-Basis Basis]
                     [Basis? basis?]
                     [Basis-forward basis-forward]
                     [Basis-inverse basis-inverse])
         basis
         Basis-Label
         Bases
         Pict3D
         current-pict3d-projection-width
         current-pict3d-projection-height
         pict3d
         empty-pict3d
         pict3d-scene
         pict3d-bases
         )

;; ===================================================================================================
;; Parameters

(: current-pict3d-projection-width (Parameterof Positive-Integer))
(define current-pict3d-projection-width (make-parameter 256))

(: current-pict3d-projection-height (Parameterof Positive-Integer))
(define current-pict3d-projection-height (make-parameter 256))

(define physics-delay 16)
(define physics-timeout 500)

(define move-accel 25.0)
(define friction-accel -10.0)

;; ===================================================================================================
;; Types

(struct Basis ([forward : FlAffine3-]
               [inverse : FlAffine3-]
               [scene : (Promise Scene)]))

(define-type -Basis Basis)

(define smaller-flt3 (scale-flt3 (flvector 0.5 0.5 0.5)))
(define bigger-flt3 (scale-flt3 (flvector 2.0 2.0 2.0)))

(: basis (->* [FlAffine3-] [FlAffine3-] Basis))
(define (basis t [tinv (flt3inverse t)])
  (Basis t
         tinv
         (delay (flscene3-transform
                 axes
                 (flt3compose t smaller-flt3)
                 (flt3compose bigger-flt3 tinv)))))

(define-type Basis-Label (U String Symbol Number (Pair Basis-Label Basis-Label) Null))

(define-type Bases (HashTable Basis-Label Basis))

(define-type Pict3D%
  (Class #:implements Snip%
         (init-field [scene  Scene]
                     [bases  Bases]
                     [width   Positive-Index]
                     [height  Positive-Index])
         [get-size  (-> (Values Positive-Index Positive-Index))]
         [get-scene  (-> Scene)]
         [get-bases  (-> Bases)]
         [get-draw-passes (-> (Vectorof draw-passes))]
         [set-argb-pixels  (-> Bytes Void)]
         ))

(define-type Pict3D (Instance Pict3D%))

(define-type Scene-GUI%
  (Class (init-field [scene Pict3D])
         [is-capturing?  (-> Boolean)]
         [own-caret  (-> Any Void)]
         [on-event  (-> (Instance Mouse-Event%) Void)]
         [on-char   (-> (Instance Key-Event%) Void)]))

;; ===================================================================================================
;; Rendering threads

(require/typed
 racket/base
 [call-with-semaphore  (All (A) (-> Semaphore (-> A) A))])

(: gl-snip-context (U #f gl-context))
(define gl-snip-context #f)

(define gl-snip-context-mutex (make-semaphore 1))

(: get-gl-snip-context (-> gl-context))
(define (get-gl-snip-context)
  (call-with-semaphore
   gl-snip-context-mutex
   (λ ()
     (define ctxt gl-snip-context)
     (if ctxt ctxt (let ([ctxt  (managed-gl-context (get-master-gl-context))])
                     (set! gl-snip-context ctxt)
                     ctxt)))))

(define get-the-bytes (make-cached-vector 'get-the-bytes make-bytes bytes-length))
(define get-tmp-bytes (make-cached-vector 'get-tmp-bytes make-bytes bytes-length))

(: make-snip-render-thread (-> Pict3D (Async-Channelof FlTransform3) Thread))
(define (make-snip-render-thread snip ch)
  (: render-thread-loop (-> Void))
  (define (render-thread-loop)
    ;; Wait for a view matrix
    (: view FlTransform3)
    (define view
      (let ([view  (async-channel-get ch)])
        ;; Empty the queue looking for the lastest one
        (let loop ([view view])
          (define new-view (async-channel-try-get ch))
          (cond [new-view  (loop new-view)]
                [else  view]))))
    
    ;(values
    ;(profile
    (time
     ;; Lock everything up for drawing
     (with-gl-context (get-gl-snip-context)
       (define-values (width height) (send snip get-size))
       (draw-draw-passes (send snip get-draw-passes) width height view)
       
       (define row-size (* width 4))
       (define bs (get-the-bytes (assert (* row-size height) index?)))
       (glReadPixels 0 0 width height GL_BGRA GL_UNSIGNED_INT_8_8_8_8 bs)
       
       ;; Flip right-side-up (OpenGL origin is lower-left; everything else is upper-left)
       (define tmp (get-tmp-bytes row-size))
       (for ([row  (in-range (fxquotient height 2))])
         (define i0 (* row row-size))
         (define i1 (* (- (- height row) 1) row-size))
         (bytes-copy! tmp 0 bs i0 (+ i0 row-size))
         (bytes-copy! bs i0 bs i1 (+ i1 row-size))
         (bytes-copy! bs i1 tmp 0 row-size))
       
       (send snip set-argb-pixels bs))
     )
    (render-thread-loop))
  
  (thread render-thread-loop))

;; ===================================================================================================
;; The snip's GUI

(: scene-gui% Scene-GUI%)
(define scene-gui%
  (class object%
    (init-field scene)
    
    (super-new)
    
    (define render-queue ((inst make-async-channel FlTransform3)))
    
    (: render-thread Thread)
    (define render-thread (make-snip-render-thread scene render-queue))
    
    (: camera (Instance Camera%))
    (define camera
      (let* ([s  (send scene get-scene)]
             [s  (flscene3-filter s (λ (a) (or (solid-shape? a) (frozen-scene-shape? a))))]
             [b  (and (not (empty-flscene3? s)) (flscene3-aabb s))]
             [c  (if b (flaabb3-center b) (flvector 0.0 0.0 0.0))]
             [d  (if b (flv3mag (flv3- (flaabb3-max b) c)) 0.0)])
        (new camera%
             [position  (flv3+ c (make-flvector 3 (/ (* d 1.25) (flsqrt 3.0))))]
             [velocity  (flvector 0.0 0.0 0.0)]
             [yaw  (degrees->radians 135.0)]
             [pitch  (degrees->radians -35.264389682754654)])))
    
    (: last-view-matrix (U #f FlTransform3))
    (define last-view-matrix #f)
    
    (: maybe-redraw (-> Void))
    (define (maybe-redraw)
      (define view-matrix (send camera get-view-matrix))
      (unless (equal? view-matrix last-view-matrix)
        (set! last-view-matrix view-matrix)
        (async-channel-put render-queue view-matrix)))
    
    (maybe-redraw)
    
    (: capturing? Boolean)
    (define capturing? #f)
    
    (define/public (is-capturing?) capturing?)
    
    (: frame-timer (U #f (Instance Timeout-Timer%)))
    (define frame-timer #f)
    
    (: stop-frame-timer (-> Void))
    (define (stop-frame-timer)
      (define frame-timer-val frame-timer)
      (when frame-timer-val
        (send frame-timer-val timeout)
        (set! frame-timer #f)))
    
    (: start-frame-timer (-> Void))
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
    
    (: home-mouse-x Integer)
    (: home-mouse-y Integer)
    (define home-mouse-x 0)
    (define home-mouse-y 0)
    
    (: yaw-vel Flonum)
    (: pitch-vel Flonum)
    (define yaw-vel 0.0)
    (define pitch-vel 0.0)
    
    (define/public (on-event e)
      (define admin (send scene get-admin))
      (define editor (and admin (send admin get-editor)))
      (case (send e get-event-type)
        [(left-down)
         (define-values (x y) (snip-center-pointer scene))
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
             (define-values (x y) (snip-center-pointer scene))
             (cond [(and x y)  (set! home-mouse-x x)
                               (set! home-mouse-y y)
                               (start-frame-timer)]
                   [editor  (send editor set-caret-owner #f)]
                   [else    (set! capturing? #f)
                            (stop-frame-timer)])))]))
    
    (: key-hash (HashTable (U Char Symbol) #t))
    (define key-hash ((inst make-hasheq (U Char Symbol) #t) empty))
    
    (: key-pressed? (-> (U Char Symbol) Boolean))
    (define/private (key-pressed? code)
      (hash-ref key-hash code #f))
    
    (: keys-pressed? (-> (Listof (U Char Symbol)) Boolean))
    (define/private (keys-pressed? codes)
      (ormap (λ ([code : (U Char Symbol)]) (key-pressed? code)) codes))
    
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
    
    (: last-frame-time Flonum)
    (define last-frame-time (fl (current-inexact-milliseconds)))
    
    (: update-camera (-> Void))
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
    
    (: timer-tick (-> Void))
    (define (timer-tick)
      (update-camera)
      (maybe-redraw))
    ))

;; ===================================================================================================
;; The snip

(define-values (standard-over-light standard-under-light)
  (let ([direction  (flvector -0.25 -0.5 -1.0)]
        [intensity  (flvector 1.0 1.0 1.0)])
    (values
     (make-directional-light-shape intensity direction)
     (make-directional-light-shape (flv3* intensity 0.5) (flv3neg direction)))))

(define blank-cursor (make-object cursor% 'blank))

(: pict3d% Pict3D%)
(define pict3d%
  (class snip%
    (init-field scene
                bases
                width
                height)
    
    (super-make-object)
    
    (: the-bitmap (U #f (Instance Bitmap%)))
    (define the-bitmap #f)
    
    (: get-the-bitmap (-> (Instance Bitmap%)))
    (define (get-the-bitmap)
      (define the-bitmap-val the-bitmap)
      (if the-bitmap-val
          the-bitmap-val
          (let ([the-bitmap-val  (make-bitmap width height)])
            (set! the-bitmap the-bitmap-val)
            the-bitmap-val)))
    
    (define/public (get-size) (values width height))
    
    (define/public (set-argb-pixels bs)
      (define len (* width height 4))
      (when (not (<= len (bytes-length bs)))
        (raise-argument-error 'set-argb-pixels
                              (format "bytes of length at least ~a" len)
                              (bytes-length bs)))
      (send (get-the-bitmap) set-argb-pixels 0 0 width height bs #f #f)
      (define admin (send this get-admin))
      (when admin
        (send admin needs-update this 0 0 width height)))
    
    (define/public (get-scene) scene)
    (define/public (get-bases) bases)
    
    (: passes (Lazy-Box (Vectorof draw-passes)))
    (define passes (lazy-box (Vectorof draw-passes)))
    
    (define/public (get-draw-passes)
      (lazy-box-ref!
       passes
       (λ ()
         (define scene-val scene)
         (if scene-val
             (list->vector
              (append (flscene3-draw-passes scene-val)
                      (flscene3-draw-passes axes)
                      (list (draw-passes (shape-passes standard-over-light) identity-affine)
                            (draw-passes (shape-passes standard-under-light) identity-affine))
                      (append*
                       (for/list : (Listof (Listof draw-passes)) ([(name p)  (in-hash bases)])
                         (flscene3-draw-passes (force (Basis-scene p)))))))
             (vector))
         ))
      )
    
    (: copy (-> Pict3D))
    (define/override (copy)
      (make-object pict3d% scene bases width height))
    
    (: gui (U #f (Instance Scene-GUI%)))
    (define gui #f)
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (unless gui
        (send this set-flags (list* 'handles-events 'handles-all-mouse-events (send this get-flags)))
        (set! gui (make-object scene-gui% this)))
      (send dc draw-bitmap (get-the-bitmap) x y)
      (super draw dc x y left top right bottom dx dy draw-caret))
    
    #;; Can't use this because of an error in TR
    (define/override (get-extent dc x y [w #f] [h #f] [descent #f] [space #f] [lspace #f] [rspace #f])
      (define-values (width height) (get-size))
      (when (box? w) (set-box! w width))
      (when (box? h) (set-box! h height))
      (when (box? descent) (set-box! descent 0))
      (when (box? space) (set-box! space 0))
      (when (box? lspace) (set-box! lspace 0))
      (when (box? rspace) (set-box! rspace 0)))
    
    ;; This works around it
    (define/override (get-extent dc x y . #{args : (Listof (U #f (Boxof Nonnegative-Real)))})
      (match-define (list w h descent space lspace rspace) args)
      (define-values (width height) (get-size))
      (when (box? w) (set-box! w width))
      (when (box? h) (set-box! h height))
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

(: pict3d (-> Scene Bases Pict3D))
(define (pict3d s ps)
  (make-object pict3d%
    s
    ps
    (assert (current-pict3d-projection-width) index?)
    (assert (current-pict3d-projection-height) index?)))

(define empty-pict3d (pict3d empty-flscene3 (make-immutable-hash)))

(: pict3d-scene (-> Pict3D Scene))
(define (pict3d-scene s) (send s get-scene))

(: pict3d-bases (-> Pict3D Bases))
(define (pict3d-bases s) (send s get-bases))
