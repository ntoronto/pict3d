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
         (only-in "../engine/types.rkt" affine affine-transform)
         "../gl.rkt"
         "../utils.rkt"
         "pict3d-struct.rkt"
         "pict3d-combinators.rkt"
         "parameters.rkt"
         "utils.rkt"
         "indicators.rkt"
         "user-types.rkt"
         )

(provide snip-class)

;; ===================================================================================================
;; Parameters

(define frame-delay 16)
(define frame-timeout 500)

(define hud-delay 66)
(define hud-timeout 500)

(define move-accel 25.0)
(define friction-accel -10.0)

(define icon-timeout 2000)

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
  (let ([dv  (flvector -0.25 -0.5 -1.0)]
        [e1  (flvector 1.0 1.0 1.0 1.0)]
        [e2  (flvector 1.0 1.0 1.0 0.5)])
    (values
     (shape->scene (make-directional-light-shape e1 dv))
     (shape->scene (make-directional-light-shape e2 (flv3neg dv))))))

(define (snip-proj-matrix width height fov-degrees znear zfar)
  (define fov-radians (degrees->radians (fl fov-degrees)))
  (flt3compose
   (scale-flt3 (flvector 1.0 -1.0 1.0))  ; upside-down: OpenGL origin is lower-left
   (perspective-flt3/viewport (fl width) (fl height) fov-radians znear zfar)))

(define light-indicator-hash (make-weak-hasheq))
(define axes-indicator-hash (make-weak-hasheq))

;(: make-snip-render-thread (-> (Instance Pict3D%) (Async-Channelof FlAffine3-) Thread))
(define (make-snip-render-thread gui ch)
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
           (legacy? width height znear zfar fov-degrees background ambient
                  add-sunlight? add-indicators?)
           (send gui get-render-params))
         ;; Compute a projection matrix
         (define proj (snip-proj-matrix width height fov-degrees znear zfar))
         
         ;; Get scaling factor for indicator objects like axes
         (define scale (fl (send gui get-scale)))
         (define scale-t (affine (scale-flt3 (flvector scale scale scale))))
         
         ;; Lock everything up for drawing
         (with-gl-context (get-master-gl-context legacy?)
           ;; Draw the scene, a couple of lights, the origin basis, group bases
           (define s (send gui get-scene))
           
           (define light-scenes
             (if add-indicators?
                 (hash-ref! light-indicator-hash s (λ () (scene-light-indicators s)))
                 empty))
           
           (define axes-scenes
             (if add-indicators?
                 (let ([h  (hash-ref! axes-indicator-hash s make-hash)])
                   (hash-ref!
                    h scale
                    (λ () (cons (make-trans-scene scale-t axes-scene)
                                (map (λ (t) (make-trans-scene (affine-compose t scale-t)
                                                              basis-scene))
                                     (scene-all-group-transforms s))))))
                 empty))
           
           (define scenes
             (append (list s)
                     (if add-sunlight?
                         (list standard-over-light
                               standard-under-light)
                         empty)
                     light-scenes
                     axes-scenes))
           (draw-scenes scenes width height view proj
                        (rgba->flvector background)
                        (emitted->flvector ambient))
           ;; Get the resulting pixels and set them into the snip's bitmap
           (define bs (get-the-bytes (* 4 width height)))
           (glReadPixels 0 0 width height GL_BGRA GL_UNSIGNED_INT_8_8_8_8 bs)
           (send gui set-argb-pixels bs)))
       empty))
    
    (log-pict3d-debug "<snip> heap size: ~a cpu time: ~a real time: ~a gc time: ~a"
                      (real->decimal-string (/ (current-memory-use) (* 1024 1024)) 2)
                      cpu real gc)
    
    (render-thread-loop))
  
  (thread render-thread-loop))

;; ===================================================================================================
;; The snip's GUI

(define (draw-sunlight-icon dc x y color)
  (define-values (orig-x orig-y) (send dc get-origin))
  (send dc set-origin (+ orig-x x) (+ orig-y y))
  (send dc set-pen "black" 0.5 'solid)
  (send dc set-brush color 'solid)
  (send dc draw-ellipse 5.5 5.5 13 13)
  (for ([ang  (in-range -180 180 (/ 360 10))])
    (define c (cos (degrees->radians ang)))
    (define s (sin (degrees->radians ang)))
    (send dc set-pen "black" 3 'solid)
    (send dc draw-line (+ 12 (* c 8)) (+ 12 (* s 8)) (+ 12 (* c 10)) (+ 12 (* s 10)))
    (send dc set-pen color 2 'solid)
    (send dc draw-line (+ 12 (* c 8)) (+ 12 (* s 8)) (+ 12 (* c 10)) (+ 12 (* s 10))))
  (send dc set-origin orig-x orig-y))

(define (draw-axes-icon dc x y white red green blue)
  (define-values (orig-x orig-y) (send dc get-origin))
  (send dc set-origin (+ orig-x x 2) (+ orig-y y 2))
  (send dc set-pen "black" 0.5 'solid)
  (send dc set-brush white 'solid)
  (send dc draw-ellipse 9 9 6 6)
  (for ([ang  (in-list '(150 30 -90))]
        [color  (in-list (list red green blue))])
    (define c (cos (degrees->radians ang)))
    (define s (sin (degrees->radians ang)))
    (send dc set-pen "black" 5 'solid)
    (send dc draw-line (+ 12 (* c 5)) (+ 12 (* s 5)) (+ 12 (* c 10)) (+ 12 (* s 10)))
    (send dc set-pen color 4 'solid)
    (send dc draw-line (+ 12 (* c 5)) (+ 12 (* s 5)) (+ 12 (* c 10)) (+ 12 (* s 10))))
  (send dc set-origin orig-x orig-y))

(define (draw-outlined-text dc str x y)
  (define path (new dc-path%))
  (send path text-outline (send dc get-font) str 0 0)
  (send path close)
  (send dc set-pen "black" 1 'solid)
  (send dc set-brush "black" 'solid)
  (send dc draw-path path x y)
  (send dc set-text-foreground "white")
  (send dc draw-text str x y))

(define gray (make-object color% 128 128 128))
(define red (make-object color% 255 128 128))
(define green (make-object color% 128 255 128))
(define blue (make-object color% 128 128 255))

(define snip-font
  (send the-font-list find-or-create-font
        12 'modern 'normal 'bold #f 'default #t 'aligned))

(define scales
  (list->vector
   (drop-right
    (rest
     (sort
      (append*
       (for/list ([j  (in-range -5 6)])
         (list (expt 10 j)
               (/ (expt 10 j) 2)
               (* (expt 10 j) 2))))
      <))
    1)))

(define max-scale-index (- (vector-length scales) 1))

(define (remove-trailing-zeros str)
  (define m (regexp-match #rx"(-|)([0-9]*)(\\.0*$)" str))
  (if m (third m) str))

(define (format/prec x digits)
  (cond [(rational? x)
         (define e (expt 10 digits))
         (remove-trailing-zeros
          (real->decimal-string (* (exact-round (/ (inexact->exact x) e)) e)
                                (max 1 (- digits))))]
        [else
         (number->string x)]))

;(: pict3d-gui% Pict3D-GUI%)
(define pict3d-gui%
  (class object%
    (init-field pict)
    
    (super-new)
    
    (define/public (get-render-params)
      (send pict get-init-params))
    
    (define/public (get-scene) (send pict get-scene))
    (define/public (set-argb-pixels bs) (send pict set-argb-pixels bs))
    
    ;(: render-queue (Async-Channel FlAffine3-))
    (define render-queue (make-async-channel))
    
    ;(: render-thread Thread)
    (define render-thread (make-snip-render-thread this render-queue))
    
    ;(: camera (Instance Camera%))
    (define camera (new camera%))
    
    (define (reset-camera)
      (send camera set-view-matrix 
            (affine-transform (pict3d-view-transform (pict3d (send pict get-scene))))))
    
    (reset-camera)
    
    ;(: last-view-matrix (U #f FlAffine3-))
    (define last-view-matrix #f)
    
    ;(: maybe-redraw (-> Void))
    (define (maybe-redraw)
      (define view-matrix (send camera get-view-matrix))
      (unless (equal? view-matrix last-view-matrix)
        (set! last-view-matrix view-matrix)
        (async-channel-put render-queue view-matrix)))
    
    (define (force-redraw)
      (set! last-view-matrix #f)
      (maybe-redraw))
    
    (maybe-redraw)
    
    ;(: capturing? Boolean)
    (define capturing? #f)
    (define/public (is-capturing?) capturing?)
    
    (define start-scale-index
      (let ([b  (scene-visible-rect (send pict get-scene))])
        (cond [b  (define-values (x1 y1 z1) (flv3-values (flrect3-min b)))
                  (define-values (x2 y2 z2) (flv3-values (flrect3-max b)))
                  (define r (expt (* (- x2 x1) (- y2 y1) (- z2 z1)) #i1/3))
                  (define scale (vector-argmin (λ (s) (abs (- r s))) scales))
                  (vector-member scale scales)]
              [else  (vector-member 1 scales)])))
    
    (define scale-index start-scale-index)
    (define/public (get-scale) (vector-ref scales scale-index))
    
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
                              frame-timer-tick
                              (λ () (set! frame-timer #f))
                              frame-delay
                              frame-timeout))))
    
    ;(: hud-timer (U #f (Instance Timeout-Timer%)))
    (define hud-timer #f)
    
    ;(: stop-hud-timer (-> Void))
    (define (stop-hud-timer)
      (define hud-timer-val hud-timer)
      (when hud-timer-val
        (send hud-timer-val timeout)
        (set! hud-timer #f)))
    
    ;(: start-hud-timer (-> Void))
    (define (start-hud-timer)
      (define hud-timer-val hud-timer)
      (if hud-timer-val
          (send hud-timer-val keep-alive)
          (set! hud-timer (make-object timeout-timer%
                              hud-timer-tick
                              (λ () (set! hud-timer #f))
                              hud-delay
                              hud-timeout))))
    
    (define display-icons? #f)
    (define icon-timer #f)
    
    ;(: start-icon-timer (-> Void))
    (define (start-display-icons)
      (when (not display-icons?)
        (set! display-icons? #t)
        (send pict refresh))
      (define icon-timer-val icon-timer)
      (if icon-timer-val
          (send icon-timer-val start icon-timeout #t)
          (set! icon-timer (make-object timer%
                             (λ ()
                               (set! icon-timer #f)
                               (set! display-icons? #f)
                               (send pict refresh))
                             icon-timeout
                             #t))))
    
    (define hud-items empty)
    
    (define (snip->world-dir sx sy width height proj view)
      (define clip-x (* (- (/ (- sx 2.0) width) 0.5) 2.0))
      (define clip-y (* (- (/ (- sy 2.0) height) 0.5) 2.0))
      (define view-v (flv3normalize (flt3apply/pos (flt3inverse proj) (flvector clip-x clip-y 0.0))))
      (flv3normalize (flt3apply/dir (flt3inverse view) view-v)))
    
    (define surface-changed? #f)
    (define last-sx -1)
    (define last-sy -1)
    
    (define (hud-timer-tick)
      (define view last-view-matrix)
      (when (and view surface-changed?)
        (set! surface-changed? #f)
        (define sx last-sx)
        (define sy last-sy)
        
        (define-values
          (legacy? width height znear zfar fov-degrees background ambient
                   add-sunlight? add-indicators?)
          (send pict get-init-params))
        
        (define proj (snip-proj-matrix width height fov-degrees znear zfar))
        (define v0 (flt3apply/pos (flt3inverse view) (flvector 0.0 0.0 0.0)))
        (define dv (snip->world-dir sx sy width height proj view))
        (define t (scene-ray-intersect (send pict get-scene) v0 dv))
        
        (define new-hud-items
          (cond
            [(not t)  empty]
            [else
             (define v (flv3fma dv t v0))
             (define dx1 (snip->world-dir (- sx 0.5) sy width height proj view))
             (define dx2 (snip->world-dir (+ sx 0.5) sy width height proj view))
             (define dy1 (snip->world-dir sx (- sy 0.5) width height proj view))
             (define dy2 (snip->world-dir sx (+ sy 0.5) width height proj view))
             (define d (max (flv3dist (flv3fma dx1 t v0) (flv3fma dx2 t v0))
                            (flv3dist (flv3fma dy1 t v0) (flv3fma dy2 t v0))))
             (cond [(and (rational? d) (> d 0))
                    (list (list 'trace-pos v (exact-floor (/ (log d) (log 10.0))) width height))]
                   [else
                    empty])]))
        
        (when (not (equal? hud-items new-hud-items))
          (set! hud-items new-hud-items)
          (send pict refresh))))
    
    (define/public (own-caret own-it?)
      (unless own-it?
        (set! capturing? #f)
        (hash-clear!* key-hash)
        (stop-frame-timer))
      (send pict refresh))
    
    (define (draw-hud-items dc)
      (for ([item  (in-list hud-items)])
        (match item
          [(list 'trace-pos v digits width height)
           (define-values (x y z) (flv3-values v))
           ;; Have to break it up like this and draw each coordinate in reverse order
           ;; because of an apparent limit on the lengths of paths
           (for/fold ([width  (- width 2)]) ([x  (in-list (list z y x))])
             (define str (string-append " " (format/prec x digits)))
             (define-values (w h _1 _2) (send dc get-text-extent str))
             (draw-outlined-text dc str (- width w) (- height h))
             (- width w))
           (void)])))
    
    (define scale-x-min -1)
    (define scale-x-mid -1)
    (define scale-x-max -1)
    (define scale-y-min -1)
    (define scale-y-max -1)
    
    (define template-str "-          +")
    
    (define (draw-scale dc)
      (define-values (width height) (send pict get-size))
      (define str (format "~a×" (/ (get-scale))))
      (define-values (w h b e) (send dc get-text-extent str))
      (define-values (tw th tb te) (send dc get-text-extent template-str))
      (define-values (bw bh bb be) (send dc get-text-extent "+"))
      (define ofs (exact-round (/ (- 24 (- th tb)) 2)))
      (draw-outlined-text dc "-" (- width ofs tw) ofs)
      (draw-outlined-text dc "+" (- width ofs bw) ofs)
      (draw-outlined-text dc str (- width ofs (exact-round (/ (+ tw w) 2))) ofs)
      (set! scale-x-min (- width ofs tw))
      (set! scale-x-mid (- width ofs (/ tw 2)))
      (set! scale-x-max (- width ofs))
      (set! scale-y-min 0)
      (set! scale-y-max 24))
    
    (define/public (draw dc x y)
      (with-handlers ([exn?  (λ (e) (displayln e))])
        (define-values (ofs-x ofs-y) (send dc get-origin))
        (define pen (send dc get-pen))
        (define brush (send dc get-brush))
        (define font (send dc get-font))
        (define text-foreground (send dc get-text-foreground))
        (define smoothing (send dc get-smoothing))
        
        (dynamic-wind
         (λ () (void))
         (λ ()
           (send dc set-origin (+ ofs-x x 2) (+ ofs-y y 2))
           (send dc set-text-foreground "white")
           (send dc set-font snip-font)
           (send dc set-smoothing 'smoothed)
           
           (when display-icons?
             (draw-scale dc)
             (if (send pict get-add-sunlight?)
                 (draw-sunlight-icon dc 0 0 "white")
                 (draw-sunlight-icon dc 0 0 gray))
             (if (send pict get-add-indicators?)
                 (draw-axes-icon dc 24 0 "white" red green blue)
                 (draw-axes-icon dc 24 0 gray gray gray gray)))
           
           (unless capturing?
             (draw-hud-items dc)))
         (λ ()
           (send dc set-smoothing smoothing)
           (send dc set-text-foreground text-foreground)
           (send dc set-font font)
           (send dc set-brush brush)
           (send dc set-pen pen)
           (send dc set-origin ofs-x ofs-y)))))
    
    ;(: last-mouse-x Integer)
    ;(: last-mouse-y Integer)
    (define last-mouse-x 0)
    (define last-mouse-y 0)
    
    ;(: center-mouse-x Integer)
    ;(: center-mouse-y Integer)
    (define center-mouse-x 0)
    (define center-mouse-y 0)
    
    ;(: yaw-vel Flonum)
    ;(: pitch-vel Flonum)
    (define yaw-vel 0.0)
    (define pitch-vel 0.0)
    
    (define (copy-hud-data time)
      (define str
        (string-append*
         (for/list ([item  (in-list hud-items)])
           (match item
             [(list 'trace-pos v digits width height)
              (define-values (x y z) (flv3-values v))
              (format "(pos ~a ~a ~a)~n" x y z)]
             [_  ""]))))
      (when (not (equal? str ""))
        (send the-clipboard set-clipboard-string str time)))
    
    (define right-click-menu (new popup-menu%))
    (new menu-item%
         [label "Copy Surface Data"]
         [parent right-click-menu]
         [callback  (λ (i e) (copy-hud-data (send e get-time-stamp)))])
    
    (define (mouse-moved sx sy)
      (when (not (and (equal? sx last-sx) (equal? sy last-sy)))
        (set! surface-changed? #t)
        (set! last-sx sx)
        (set! last-sy sy)
        (start-hud-timer))
      (start-display-icons))
    
    (define/public (on-event dc x y editorx editory e)
      (with-handlers ([exn?  (λ (e) (displayln e))])
        (define sx (- (send e get-x) x 2))
        (define sy (- (send e get-y) y 2))
        (define admin (send pict get-admin))
        (define editor (and admin (send admin get-editor)))
        (case (send e get-event-type)
          [(left-down)
           (cond
             [(and (<= 0 sx 24) (<= 0 sy 24))
              (send pict toggle-add-sunlight?)
              (force-redraw)
              (start-display-icons)]
             [(and (<= 24 sx (+ 24 24)) (<= 0 sy 24))
              (send pict toggle-add-indicators?)
              (set! last-view-matrix #f)
              (force-redraw)
              (start-display-icons)]
             [(and (<= scale-x-min sx scale-x-max) (<= scale-y-min sy scale-y-max))
              (cond [(< sx scale-x-mid)
                     (set! scale-index (min (+ scale-index 1) max-scale-index))]
                    [else
                     (set! scale-index (max (- scale-index 1) 0))])
              (force-redraw)
              (start-display-icons)]
             [else
              ;; Center the mouse pointer (generates another mouse event)
              (define-values (x y) (snip-center-pointer pict))
              (cond [capturing?
                     ;; If capturing, stop capturing
                     (cond [editor  (send editor set-caret-owner #f)]
                           [else    (set! capturing? #f)
                                    (stop-frame-timer)])]
                    [(and x y)
                     ;; If not capturing and it worked, start capturing
                     (set! center-mouse-x x)
                     (set! center-mouse-y y)
                     (set! last-mouse-x x)
                     (set! last-mouse-y y)
                     (set! capturing? #t)
                     (start-frame-timer)])])]
          [(right-down middle-down)
           (void)]
          [(right-up middle-up)
           (when (not capturing?)
             (define editor-admin (and editor (send editor get-admin)))
             (send editor-admin popup-menu right-click-menu (+ editorx sx 2) (+ editory sy 2)))]
          [(motion)
           (cond
             [capturing?
              (define x (send e get-x))
              (define y (send e get-y))
              (cond
                [(and (= x center-mouse-x) (= y center-mouse-y))
                 ;; This event is almost certainly generated by centering the pointer, so don't
                 ;; update the angle velocities
                 (set! last-mouse-x x)
                 (set! last-mouse-y y)]
                [else
                 ;; Update the angle velocities using mouse position differences
                 (define dx (- x last-mouse-x))
                 (define dy (- y last-mouse-y))
                 (set! last-mouse-x x)
                 (set! last-mouse-y y)
                 (unless (and (= dx 0) (= dy 0))
                   (set! yaw-vel (+ yaw-vel (fl (* 0.002 dx))))
                   (set! pitch-vel (+ pitch-vel (fl (* 0.002 dy))))
                   ;; Center the mouse pointer (generates another mouse event)
                   (define-values (x y) (snip-center-pointer pict))
                   (cond [(and x y)
                          ;; Keep track of these so we can tell the next time through whether the
                          ;; mouse event was generated by centering
                          (set! center-mouse-x x)
                          (set! center-mouse-y y)
                          (start-frame-timer)]
                         ;; Both of these cases below stop mouse capture
                         [editor  (send editor set-caret-owner #f)]
                         [else    (set! capturing? #f)
                                  (stop-frame-timer)]))])]
             [else
              (mouse-moved sx sy)])])))
    
    (define/public (adjust-cursor dc sx sy e)
      (cond
        [capturing?  blank-cursor]
        [else
         (mouse-moved sx sy)
         (if (or (and (<= 0 sx (+ 24 24)) (<= 0 sy 24))
                 (and (<= scale-x-min sx scale-x-max) (<= scale-y-min sy scale-y-max)))
             arrow-cursor
             cross-cursor)]))
    
    ;(: key-hash (HashTable (U Char Symbol) #t))
    (define key-hash (make-hasheq empty))
    
    ;(: key-pressed? (-> (U Char Symbol) Boolean))
    (define/private (key-pressed? code)
      (hash-ref key-hash code #f))
    
    ;(: keys-pressed? (-> (Listof (U Char Symbol)) Boolean))
    (define/private (keys-pressed? codes)
      (ormap (λ (code) (key-pressed? code)) codes))
    
    (define/public (on-char e)
      (with-handlers ([exn?  (λ (e) (displayln e))])
        (define code (send e get-key-code))
        (case code
          [(#\_ #\-)
           (set! scale-index (min (+ scale-index 1) max-scale-index))
           (force-redraw)
           (start-display-icons)]
          [(#\+ #\=)
           (set! scale-index (max (- scale-index 1) 0))
           (force-redraw)
           (start-display-icons)]
          [(escape)
           (reset-camera)
           (set! scale-index start-scale-index)
           (force-redraw)
           (start-display-icons)]
          [(release)
           (hash-remove! key-hash (send e get-key-release-code))]
          [else
           (hash-set! key-hash code #t)])
        (when (not frame-timer)
          (set! last-frame-time (- (fl (current-inexact-milliseconds))
                                   (fl frame-delay)))
          (send camera set-velocity (flvector 0.0 0.0 0.0))
          (frame-timer-tick)
          (start-frame-timer))))
    
    ;(: last-frame-time Flonum)
    (define last-frame-time (fl (current-inexact-milliseconds)))
    
    ;(: frame-timer-tick (-> Void))
    (define (frame-timer-tick)
      (define frame-time (fl (current-inexact-milliseconds)))
      (define frame-delay (max 1.0 (min 100.0 (- frame-time last-frame-time))))
      (set! last-frame-time frame-time)
      (define dt (/ frame-delay 1000.0))
      
      (let* ([move-accel  (* (fl (get-scale)) move-accel)]
             [acc  (flvector 0.0 0.0 0.0)]
             [acc  (if (keys-pressed? '(#\a #\A left))
                       (flv3+ acc (flv3* (flvector -1.0 0.0 0.0) move-accel))
                       acc)]
             [acc  (if (keys-pressed? '(#\d #\D right))
                       (flv3+ acc (flv3* (flvector +1.0 0.0 0.0) move-accel))
                       acc)]
             [acc  (if (keys-pressed? '(#\f #\F next))
                       (flv3+ acc (flv3* (flvector 0.0 -1.0 0.0) move-accel))
                       acc)]
             [acc  (if (keys-pressed? '(#\r #\R prior))
                       (flv3+ acc (flv3* (flvector 0.0 +1.0 0.0) move-accel))
                       acc)]
             [acc  (if (keys-pressed? '(#\w #\W up))
                       (flv3+ acc (flv3* (flvector 0.0 0.0 -1.0) move-accel))
                       acc)]
             [acc  (if (keys-pressed? '(#\s #\S down))
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
      
      (maybe-redraw))
    ))

;; ===================================================================================================
;; The snip

(define blank-cursor (make-object cursor% 'blank))
(define cross-cursor (make-object cursor% 'cross))
(define arrow-cursor (make-object cursor% 'arrow))

(define black-pen (make-object pen% "black" 1 'solid))
(define white-pen (make-object pen% "white" 1 'solid))
(define trans-brush (make-object brush% "black" 'transparent))

(define pict3d-snip-class%
  (class snip-class%
    ;(send this set-classname (format "~s" '(lib "pict3d-snip.rkt" "pict3d" "private" "gui")))
    ;(send this set-version 0)
    
    (define/override (read f)
      (error 'read "reading pict3d% instances is not supported"))
    
    (super-make-object)
    ))

(define snip-class (make-object pict3d-snip-class%))
(send snip-class set-classname (format "~s" '(lib "pict3d-snip.rkt" "pict3d" "private" "gui")))
(send snip-class set-version 0)

(send (get-the-snip-class-list) add snip-class)

;(: pict3d% Pict3D%)
(define pict3d%
  (class snip%
    (init-field scene
                legacy?
                width
                height
                z-near
                z-far
                fov-degrees
                background
                ambient
                add-sunlight?
                add-indicators?)
    
    (super-make-object)
    
    (send this set-snipclass snip-class)
    
    (define/public (get-add-sunlight?) add-sunlight?)
    (define/public (get-add-indicators?) add-indicators?)
    (define/public (set-add-sunlight? b) (set! add-sunlight? b))
    (define/public (set-add-indicators? b) (set! add-indicators? b))
    (define/public (toggle-add-sunlight?) (set! add-sunlight? (not add-sunlight?)))
    (define/public (toggle-add-indicators?) (set! add-indicators? (not add-indicators?)))
    
    (define/public (get-scene) scene)
    
    (define/public (get-size) (values width height))
    
    ;(: copy (-> (Instance Pict3D%)))
    (define/override (copy)
      (make-object pict3d% scene legacy?
        width height z-near z-far fov-degrees background ambient add-sunlight? add-indicators?))
    
    (define scroll-step #f)
    (define (calc-scroll-step)
      (unless scroll-step
        ;; try to set scroll step by font size of the standard style
        (let ([admin (send this get-admin)])
          (when admin
            (let* ([ed (send admin get-editor)]
                   [sl (send ed get-style-list)]
                   [standard (send sl find-named-style "Standard")])
              (when standard
                (let ([dc (make-object bitmap-dc% (make-object bitmap% 1 1))])
                  (let-values ([(w h d a) (send dc get-text-extent "X" (send standard get-font))])
                    (set! scroll-step (+ h (send admin get-line-spacing)))))))))
        ;; if that didn't happen, set it to 12.
        (unless scroll-step (set! scroll-step 12))))
    
    (define/override (get-num-scroll-steps)
      (calc-scroll-step)
      (exact-ceiling (/ height scroll-step)))
    
    (define/override (get-scroll-step-offset offset)
      (calc-scroll-step)
      (min height (exact-ceiling (* offset scroll-step))))
    
    (define/override (find-scroll-step y)
      (calc-scroll-step)
      (exact-ceiling (/ y scroll-step)))
    
    (define/override (write f)
      (error 'write "copying pict3d% instances is not supported"))
    
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
      (values legacy? width height z-near z-far fov-degrees background ambient
              add-sunlight? add-indicators?))
    
    (define/public (refresh)
      (define admin (send this get-admin))
      (when admin
        (send admin needs-update this 0 0 (+ width 4) (+ height 4))))
    
    (define/public (set-argb-pixels bs)
      (define len (* width height 4))
      (when (not (<= len (bytes-length bs)))
        (raise-argument-error 'set-argb-pixels
                              (format "bytes of length at least ~a" len)
                              (bytes-length bs)))
      (send (get-the-bitmap) set-argb-pixels 0 0 width height bs #f #t)
      (refresh))
    
    ;(: gui (U #f (Instance Pict3D-GUI%)))
    (define gui #f)
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define smoothing (send dc get-smoothing))
      (send dc set-smoothing 'unsmoothed)
      (send dc set-brush trans-brush)
      (send dc set-pen black-pen)
      (send dc draw-rectangle (+ x 0.5) (+ y 0.5) (+ width 4) (+ height 4))
      (send dc set-pen white-pen)
      (send dc draw-rectangle (+ x 1.5) (+ y 1.5) (+ width 2) (+ height 2))
      (send dc draw-bitmap (get-the-bitmap) (+ x 2) (+ y 2))
      (send dc set-smoothing smoothing)
      (when gui (send gui draw dc x y)))
    
    (define/override (get-extent dc x y [w #f] [h #f] [descent #f] [space #f] [lspace #f] [rspace #f])
      (send (get-the-snip-class-list) add snip-class)
      (unless gui
        (send this set-flags (list* 'handles-events 'handles-all-mouse-events (send this get-flags)))
        (set! gui (make-object pict3d-gui% this)))
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
        (and gui-val (send gui-val on-event dc x y editorx editory e)))
      (super on-event dc x y editorx editory e))
    
    (define/override (on-char dc x y editorx editory e)
      (let ([gui-val  gui])
        (and gui-val (send gui-val on-char e)))
      (super on-char dc x y editorx editory e))
    
    (define/override (adjust-cursor dc x y editorx editory e)
      (let ([gui-val  gui])
        (cond
          [gui-val
           (define sx (- (send e get-x) x 2))
           (define sy (- (send e get-y) y 2))
           (cond [(and (<= 0 sx width) (<= 0 sy height))
                  (send gui-val adjust-cursor dc sx sy e)]
                 [else  #f])]
          [else  #f])))
    ))

(define (scene->pict3d scene)
  (make-object pict3d%
    scene
    (pict3d-legacy-contexts?)
    (current-pict3d-width)
    (current-pict3d-height)
    (current-pict3d-z-near)
    (current-pict3d-z-far)
    (current-pict3d-fov-degrees)
    (current-pict3d-background)
    (current-pict3d-ambient)
    (current-pict3d-add-sunlight?)
    (current-pict3d-add-indicators?)))

(define (pict3d-custom-write scene out mode)
  (define print-it
    (cond [(eq? mode #t)  write]
          [(eq? mode #f)  display]
          [else  print]))
  (print-it (scene->pict3d scene) out))

(define (pict3d-print-converter scene recur)
  (scene->pict3d scene))

;; Set the custom printer so Pict3D instances will print nicely in Racket
(current-pict3d-custom-write pict3d-custom-write)
;; Set the print converter so Pict3D instances will print nicely in HTDP languages
(current-pict3d-print-converter pict3d-print-converter)
