#lang racket/base

(require racket/gui
         racket/class
         racket/async-channel
         racket/list
         racket/match
         racket/math
         math/flonum
         typed/opengl
         "../gl.rkt"
         "../utils.rkt"
         "../engine.rkt"
         "pict3d-struct.rkt"
         "pict3d-combinators.rkt"
         "pict3d-draw.rkt"
         "master-context.rkt"
         "parameters.rkt"
         "indicators.rkt"
         "user-types.rkt"
         "utils/camera.rkt"
         "utils/timeout-timer.rkt"
         "utils/center-pointer.rkt"
         "utils/scales.rkt"
         "utils/format.rkt"
         )

(provide snip-class
         scene->pict3d%
         pict3d%->scene)

(define sema (make-semaphore 1))

#;
(define-syntax-rule (with-reentry-lock name . body)
  (begin
    (define (thunk) . body)
    (define can? (semaphore-try-wait? sema))
    (cond [can?  (dynamic-wind void thunk (λ () (semaphore-post sema)))]
          [else  (log-pict3d-warning "<snip> tried to reenter callback code at ~a; waiting~n" 'name)
                 (call-with-semaphore sema thunk)])))

(define-syntax-rule (with-reentry-lock name . body)
  (begin
    (define (thunk) . body)
    (define can? (semaphore-try-wait? sema))
    (cond [can?  (dynamic-wind void thunk (λ () (semaphore-post sema)))]
          [else  (log-pict3d-error "<snip> reentered callback code at ~a~n" 'name)
                 (thunk)])))

;; ===================================================================================================
;; Parameters

(define frame-delay 16)
(define frame-timeout 500)

(define hud-delay 66)
(define hud-timeout 500)

(define move-accel 25.0)
(define friction-accel -10.0)

(define icon-timeout 2000)

(define allow-capture?
  (case (system-type 'os)
    [(macosx)  #f]
    [else      #t]))

(define debug-pass-names
  #hasheq((opaque-material . "Surface Normals + Roughness")
          (opaque-depth . "Surface Depth")
          (opaque-diffuse . "Diffuse Light")
          (opaque-specular . "Specular Light")
          (opaque-rgba . "Surface RGBA")
          (transparent-material . "Surface Normals + Roughness")
          (transparent-depth . "Surface Depth")
          (transparent-diffuse . "Diffuse Light")
          (transparent-specular . "Specular Light")
          (transparent-rgbv . "Surface RGB + Weight")
          (transparent-alpha . "Surface Alpha")
          (composite-rgba . "Opaque + Transparent RGBA")
          (bloom . "Bloom Lighting")
          (no-bloom . "Gamma-Corrected RGBA (no bloom)")
          (#f . "Gamma-Corrected RGBA (final)")))

;; ===================================================================================================
;; Rendering threads

(define get-the-bytes
  (make-gl-cached-vector
   'get-the-bytes
   (λ (n)
     (log-pict3d-info "<snip> creating temp ARGB bytes of length ~v" n)
     (make-bytes n))
   bytes-length))

(define light-indicator-hash (make-weak-hasheq))
(define axes-indicator-hash (make-weak-hasheq))
(define group-box-hash (make-weak-hasheq))
(define wireframe-hash (make-weak-hasheq))

(define rgba-black (rgba "black"))
(define rgba-white (rgba "white"))
(define emitted-black (emitted "black" 0))
(define emitted-magenta (emitted 1 0 1 1))
(define wireframe-material
  (material #:ambient 1.0 #:diffuse 0.0 #:specular 0.0 #:roughness 1.0))

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
         (match-define (list debug-pass path
                             legacy? check-version?
                             width height z-near z-far fov background ambient
                             add-sunlight? add-indicators? add-grid? add-wireframe
                             auto-camera
                             debug-shapes)
           (send gui get-render-params))
         ;; Lock everything up for drawing
         (with-gl-context (get-master-gl-context legacy? check-version?)
           ;; Get scaling factor for indicator objects like axes
           (define scale (fl (send gui get-scale)))
           
           ;; Draw the scene, a couple of lights, the origin basis, group bases
           (define scene (send gui get-scene))
           
           (define sunlight-pict3ds
             (if add-sunlight?
                 (list standard-over-light
                       standard-under-light)
                 empty))
           
           (define light-pict3ds
             (if add-indicators?
                 (hash-ref! light-indicator-hash scene
                            (λ () (scene-light-indicators scene)))
                 empty))
           
           (define axes-pos+picts
             (if add-indicators?
                 (let ([h  (hash-ref! axes-indicator-hash scene make-hash)])
                   (hash-ref! h scale
                              (λ () (cons (cons origin (scene-origin-indicator scale))
                                          (scene-basis-indicators scene scale)))))
                 empty))
           
           (define grid-pict3ds
             (if add-grid?
                 (list (light-grid (emitted 1.0 0.6 0.6 2.0)
                                   (emitted 0.5 1.5 0.5 2.0)
                                   (emitted 0.7 0.7 1.0 2.0)
                                   scale))
                 empty))
           
           (define (get-wireframes)
             (hash-ref!
              wireframe-hash scene
              (λ ()
                (cons (delay (parameterize ([current-color  rgba-black]
                                            [current-emitted  emitted-black]
                                            [current-material  wireframe-material])
                               (freeze-in-groups (wireframe (pict3d scene)))))
                      (delay (parameterize ([current-color  rgba-white]
                                            [current-emitted  emitted-magenta]
                                            [current-material  wireframe-material])
                               (freeze-in-groups (wireframe (pict3d scene) #:width 1.0))))))))
           
           (define wireframe-pict3ds
             (case add-wireframe
               [(color)    (list (force (car (get-wireframes))))]
               [(emitted)  (list (force (cdr (get-wireframes))))]
               [else       empty]))
           
           (define v0 (affine-origin view))
           (define axes-pict3ds
             (for/fold ([picts empty]) ([pos+pict  (in-list axes-pos+picts)])
               (match-define (cons v p) pos+pict)
               (if (< (pos-dist v v0) (* scale 0.015))
                   picts
                   (cons p picts))))
           
           (define boxes
             (if (empty? path)
                 empty
                 (let ([h  (hash-ref! group-box-hash scene make-hash)])
                   (hash-ref! h path (λ () (group-boxes (pict3d scene) path))))))
           
           (parameterize ([current-engine-debug-pass    debug-pass]
                          [current-engine-debug-shapes  debug-shapes])
             (draw-pict3ds (append (list (pict3d scene))
                                   sunlight-pict3ds
                                   light-pict3ds
                                   axes-pict3ds
                                   grid-pict3ds
                                   wireframe-pict3ds
                                   boxes)
                           #:width width
                           #:height height
                           #:camera view
                           #:z-near z-near
                           #:z-far z-far
                           #:fov fov
                           #:background background
                           #:ambient ambient
                           #:bitmap? #t))
           
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
  (send dc draw-ellipse 7 9 6 6)
  (for ([ang  (in-list '(150 30 -90))]
        [color  (in-list (list red green blue))])
    (define c (cos (degrees->radians ang)))
    (define s (sin (degrees->radians ang)))
    (send dc set-pen "black" 5 'solid)
    (send dc draw-line (+ 10 (* c 5)) (+ 12 (* s 5)) (+ 10 (* c 10)) (+ 12 (* s 10)))
    (send dc set-pen color 4 'solid)
    (send dc draw-line (+ 10 (* c 5)) (+ 12 (* s 5)) (+ 10 (* c 10)) (+ 12 (* s 10))))
  (send dc set-origin orig-x orig-y))

(define (draw-grid-icon-lines dc x y color width style)
  (define-values (orig-x orig-y) (send dc get-origin))
  (send dc set-origin (+ orig-x x) (+ orig-y y))
  (send dc set-pen color width style)
  (for ([ang  (in-range -180 180 90)])
    (define c1 (cos (degrees->radians ang)))
    (define s1 (* 0.5 (sin (degrees->radians ang))))
    (define c2 (cos (degrees->radians (+ ang 90))))
    (define s2 (* 0.5 (sin (degrees->radians (+ ang 90)))))
    (send dc draw-line (+ 12 (* c1 8)) (+ 8 (* s1 8)) (+ 12 (* c2 8)) (+ 8 (* s2 8))))
  (for ([ang  (in-range -0 270 90)])
    (define c1 (cos (degrees->radians ang)))
    (define s1 (* 0.5 (sin (degrees->radians ang))))
    (send dc draw-line (+ 12 (* c1 8)) (+ 8 (* s1 8)) (+ 12 (* c1 8)) (+ 16 (* s1 8))))
  (for ([ang  (in-range 0 180 90)])
    (define c1 (cos (degrees->radians ang)))
    (define s1 (* 0.5 (sin (degrees->radians ang))))
    (define c2 (cos (degrees->radians (+ ang 90))))
    (define s2 (* 0.5 (sin (degrees->radians (+ ang 90)))))
    (send dc draw-line (+ 12 (* c1 8)) (+ 16 (* s1 8)) (+ 12 (* c2 8)) (+ 16 (* s2 8))))
  (send dc set-origin orig-x orig-y))

(define (draw-grid-icon dc x y color)
  (draw-grid-icon-lines dc x y "black" 3 'solid)
  (draw-grid-icon-lines dc x y color 2 'short-dash))

(define (draw-outlined-text dc str x y)
  (define path (new dc-path%))
  (send path text-outline (send dc get-font) str 0 0)
  (send path close)
  (send dc set-pen "black" 1 'solid)
  (send dc set-brush "black" 'solid)
  (send dc draw-path path x y)
  (send dc set-text-foreground "white")
  (send dc draw-text str x y))

(define (draw-wireframe-icon-lines dc x y color width style)
  (define-values (orig-x orig-y) (send dc get-origin))
  (send dc set-origin (+ orig-x x) (+ orig-y y))
  (send dc set-pen color width style)
  (send dc set-brush "black" 'transparent)
  (send dc draw-rectangle 5 5 14 14)
  (send dc draw-line 7.25 16.75 16.75 7.25)
  (send dc set-origin orig-x orig-y))

(define (draw-wireframe-icon dc x y color)
  (draw-wireframe-icon-lines dc x y "black" 3 'solid)
  (draw-wireframe-icon-lines dc x y color 2 'solid))

(define gray (make-object color% 128 128 128))
(define red (make-object color% 255 128 128))
(define green (make-object color% 128 255 128))
(define blue (make-object color% 128 128 255))
(define magenta (make-object color% 255 128 255))

(define snip-font
  (send the-font-list find-or-create-font
        12 'modern 'normal 'bold #f 'default #t 'aligned))

;(: pict3d-gui% Pict3D-GUI%)
(define pict3d-gui%
  (class object%
    (init-field pict)
    
    (super-new)
    
    (define debug-pass #f)
    
    (define/public (get-render-params)
      (list* debug-pass last-trace-path (call-with-values (λ () (send pict get-init-params)) list)))
    
    (define/public (get-scene) (send pict get-scene))
    (define/public (set-argb-pixels bs) (send pict set-argb-pixels bs))
    
    ;(: yaw-vel Flonum)
    ;(: pitch-vel Flonum)
    (define yaw-vel 0.0)
    (define pitch-vel 0.0)
    
    ;(: camera (Instance Camera%))
    (define camera (new camera%))
    
    (define (reset-camera)
      (set! yaw-vel 0)
      (set! pitch-vel 0)
      (send camera set-velocity (dir 0 0 0))
      (send camera set-basis
            (let* ([p  (pict3d (send pict get-scene))]
                   [t  (camera-transform p)])
              (if t t ((send pict get-auto-camera) p)))))
    
    (reset-camera)
    
    ;(: mouse-mode Boolean)
    (define mouse-mode 'none)
    
    (define start-scale-index (scale->scale-index 1))
    (define scale-index start-scale-index)
    (define/public (get-scale) (scale-index->scale scale-index))
    
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
    
    (define surface-changed? #f)
    (define last-snip-x -1)
    (define last-snip-y -1)
    
    (define last-trace-pos #f)
    (define last-trace-path empty)
    
    (define (hud-timer-tick)
      (define view last-view-matrix)
      (when (and view surface-changed?)
        (set! surface-changed? #f)
        (define snip-x last-snip-x)
        (define snip-y last-snip-y)
        
        (define-values
          (legacy? check-version?
                   width height z-near z-far fov background ambient
                   add-sunlight? add-indicators? add-grid? add-wireframe
                   auto-camera
                   debug-shapes)
          (send pict get-init-params))
        
        (parameterize ([current-pict3d-width   width]
                       [current-pict3d-height  height]
                       [current-pict3d-z-near  z-near]
                       [current-pict3d-z-far   z-far]
                       [current-pict3d-fov     fov])
          ;; "Window" coordinates
          (define x (fl snip-x))
          (define y (fl snip-y))
          
          (define v0 (affine-origin view))
          (define ray-dir (camera-ray-dir view))
          (define dv (ray-dir x y))
          (define data
            (and dv (trace/data (pict3d (send pict get-scene)) v0 dv)))
          
          (define new-hud-items
            (cond
              [(not data)
               (when (not (empty? last-trace-path))
                 (force-redraw))
               (set! last-trace-path empty)
               empty]
              [else
               (define v (surface-data-pos data))
               (define n (surface-data-normal data))
               (define path (surface-data-path data))
               (define t (pos-dist v v0))
               ;; Compute an approximation of the change in position values per window coordinate at
               ;; the intersection point
               (define dv1 (dir-normalize (ray-dir (- x 0.5) y)))
               (define dv2 (dir-normalize (ray-dir (+ x 0.5) y)))
               (define dv3 (dir-normalize (ray-dir x (- y 0.5))))
               (define dv4 (dir-normalize (ray-dir x (+ y 0.5))))
               (define d (max (pos-dist (pos+ v0 dv1 t) (pos+ v0 dv2 t))
                              (pos-dist (pos+ v0 dv3 t) (pos+ v0 dv4 t))))
               
               (when (not (equal? path last-trace-path))
                 (force-redraw))
               (set! last-trace-path path)
               
               (append
                (cond [(and (rational? d) (> d 0))
                       (set! last-trace-pos v)
                       (list (list 'trace-pos v (exact-floor (/ (log d) (log 10.0)))))]
                      [else  empty])
                (cond [(not n)  empty]
                      [else  (list (list 'trace-norm n))])
                (cond [(empty? path)  empty]
                      [else  (list (list 'trace-path path))]))]))
          
          (when (not (equal? hud-items new-hud-items))
            (set! hud-items new-hud-items)
            (send pict refresh)))))
    
    (define/public (own-caret own-it?)
      (unless own-it?
        (set! mouse-mode 'none)
        (hash-clear!* key-hash)
        (stop-frame-timer))
      (send pict refresh))
    
    (define (draw-hud-vector dc v digits width height cstr line)
      (define-values (x y z)
        (if (pos? v)
            (values (pos-x v) (pos-y v) (pos-z v))
            (values (dir-dx v) (dir-dy v) (dir-dz v))))
      ;; Have to break it up like this and draw each coordinate in reverse order
      ;; because of an apparent limit on the lengths of paths
      (define strs
        (list (format "(~a " cstr)
              (string-append (format/prec x digits) " ")
              (string-append (format/prec y digits) " ")
              (string-append (format/prec z digits) ")")))
      (for/fold ([width  (- width 2)]) ([str  (in-list (reverse strs))])
        (define-values (w h _1 _2) (send dc get-text-extent str))
        (draw-outlined-text dc str (- width w) (- height (* h line)))
        (- width w))
      (void))
    
    (define (draw-hud-items dc)
      (define-values
        (legacy? check-version?
                 width height z-near z-far fov background ambient
                 add-sunlight? add-indicators? add-grid? add-wireframe
                 auto-camera
                 debug-shapes)
        (send pict get-init-params))
      
      (for ([item  (in-list hud-items)])
        (match item
          [(list 'trace-pos v digits)
           (draw-hud-vector dc v digits width height "pos" 2)]
          [(list 'trace-norm n)
           (draw-hud-vector dc n -3 width height "dir" 1)]
          [(list 'trace-path path)
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
        (define alpha (send dc get-alpha))
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
                 (draw-axes-icon dc 24 0 gray gray gray gray))
             (if (send pict get-add-grid?)
                 (draw-grid-icon dc 48 0 "white")
                 (draw-grid-icon dc 48 0 gray))
             (case (send pict get-add-wireframe)
               [(color)    (draw-wireframe-icon dc 72 0 "white")]
               [(emitted)  (draw-wireframe-icon dc 72 0 magenta)]
               [else       (draw-wireframe-icon dc 72 0 gray)]))
           
           (unless (eq? mouse-mode 'capturing)
             (draw-hud-items dc))
           
           (when (eq? mouse-mode 'dragging)
             (define dx (fl (- last-snip-x drag-down-x)))
             (define dy (fl (- last-snip-y drag-down-y)))
             (define d (flsqrt (+ (sqr dx) (sqr dy))))
             (when (> d 1.0)
               (send dc set-brush "black" 'transparent)
               (define nx (/ dx d))
               (define ny (/ dy d))
               (define r (min 12 (/ (* 12 (sqrt 12)) (sqrt d))))
               (define l (- (* 2 pi 12) (* 2 pi r)))
               
               (define (draw-source)
                 (send dc draw-ellipse (- drag-down-x r) (- drag-down-y r) (* r 2) (* r 2)))
               
               (define (draw-dest)
                 (send dc draw-line last-snip-x last-snip-y
                       (- last-snip-x (* 0.5 l (/ (+ nx (- ny)) (sqrt 2))))
                       (- last-snip-y (* 0.5 l (/ (+ ny nx) (sqrt 2)))))
                 (send dc draw-line last-snip-x last-snip-y
                       (- last-snip-x (* 0.5 l (/ (+ ny nx) (sqrt 2))))
                       (- last-snip-y (* 0.5 l (/ (- (+ nx (- ny))) (sqrt 2))))))
               
               (define (draw-line)
                 (send dc draw-line
                       (+ drag-down-x (* nx r))
                       (+ drag-down-y (* ny r))
                       last-snip-x last-snip-y))
               
               (send dc set-pen "black" 3 'solid)
               (draw-source)
               (draw-dest)
               (when (> d 12.0) (draw-line))
               
               (send dc set-pen "white" 2 'short-dash)
               (draw-source)
               (draw-dest)
               (when (> d 12.0) (draw-line))))
           )
         (λ ()
           (send dc set-smoothing smoothing)
           (send dc set-text-foreground text-foreground)
           (send dc set-font font)
           (send dc set-brush brush)
           (send dc set-pen pen)
           (send dc set-alpha alpha)
           (send dc set-origin ofs-x ofs-y)))))
    
    ;(: last-mouse-x Integer)
    ;(: last-mouse-y Integer)
    (define last-mouse-x 0)
    (define last-mouse-y 0)
    
    (define dragged? #f)
    (define drag-down-x 0)
    (define drag-down-y 0)
    
    ;(: center-mouse-x Integer)
    ;(: center-mouse-y Integer)
    (define center-mouse-x 0)
    (define center-mouse-y 0)
    
    (define (copy-hud-data time)
      (with-handlers ([exn?  (λ (e) (printf "exception: ~e" e))])
        (define pos-item (assq 'trace-pos hud-items))
        (define str
          (cond
            [pos-item
             (define norm-item (assq 'trace-norm hud-items))
             (define path-item (assq 'trace-path hud-items))
             (define v (second pos-item))
             (define n (and norm-item (second norm-item)))
             (define path (if path-item (second path-item) empty))
             (define strs
               (append (list (format "~v" v))
                       (if n (list (format "~v" n)) empty)
                       (if (and path (not (empty? path)))
                           (list (format "~v" path))
                           empty)))
             (string-join strs "\n")]
            [else
             ""]))
        (when (not (equal? str ""))
          (send the-clipboard set-clipboard-string str time))))
    
    (define (copy-camera-data time)
      (define t (affine-inverse (camera->view (send camera get-basis))))
      (match-define (affine dx dy dz p) t)
      (define str (format "~v~n~v" p (dir-negate dz)))
      (send the-clipboard set-clipboard-string str time))
    
    (define right-click-menu (new popup-menu%))
    (new menu-item%
         [label "Copy Surface Data"]
         [parent right-click-menu]
         [callback  (λ (i e) (copy-hud-data (send e get-time-stamp)))])
    (new menu-item%
         [label "Copy Camera Data"]
         [parent right-click-menu]
         [callback  (λ (i e) (copy-camera-data (send e get-time-stamp)))])
    
    (new separator-menu-item%
         [parent right-click-menu])
    
    (define debug-pass-submenu
      (new menu%
           [label "Show Rendering Pass Result"]
           [parent right-click-menu]))
    
    (define (add-debug-header str separator-before?)
      (when separator-before?
        (new separator-menu-item% [parent debug-pass-submenu]))
      (define item
        (new menu-item%
             [label str]
             [parent debug-pass-submenu]
             [callback void]))
      (send item enable #f)
      (new separator-menu-item% [parent debug-pass-submenu]))
    
    (define debug-passes-items
      (for/list ([pass  (in-list (append (get-engine-debug-passes) (list #f)))])
        (case pass
          [(opaque-material)  (add-debug-header "Opaque Object Passes" #f)]
          [(transparent-material)  (add-debug-header "Transparent Object Passes" #t)]
          [(composite-rgba)  (add-debug-header "Post-Processing Passes" #t)])
        
        (new checkable-menu-item%
             [label  (hash-ref debug-pass-names pass)]
             [parent  debug-pass-submenu]
             [help-string  (if pass (symbol->string pass) "#f")]
             [checked  (eq? pass #f)]
             [callback  (λ (i e)
                          (define str (send i get-help-string))
                          (define pass
                            (if (equal? str "#f")
                                #f
                                (string->symbol str)))
                          (set! debug-pass pass)
                          (force-redraw)
                          
                          (for ([item  (in-list debug-passes-items)])
                            (unless (eq? item i)
                              (send item check #f))))])))
    
    (define (mouse-moved snip-x snip-y)
      (unless (and (equal? snip-x last-snip-x)
                   (equal? snip-y last-snip-y))
        (set! surface-changed? #t)
        (set! last-snip-x snip-x)
        (set! last-snip-y snip-y)
        (start-hud-timer))
      (start-display-icons))
    
    (define/public (on-event dc orig-x orig-y editorx editory e)
      (with-handlers ([exn?  (λ (e) (displayln e))])
        (define x (send e get-x))
        (define y (send e get-y))
        (define dx (- x last-mouse-x))
        (define dy (- y last-mouse-y))
        (set! last-mouse-x x)
        (set! last-mouse-y y)
        (define type (send e get-event-type))
        (define snip-x (- x orig-x 2))
        (define snip-y (- y orig-y 2))
        (define admin (send pict get-admin))
        (define editor (and admin (send admin get-editor)))
        (case type
          [(left-down)
           (when (eq? mouse-mode 'none)
             (set! drag-down-x snip-x)
             (set! drag-down-y snip-y)
             (set! mouse-mode 'dragging)
             (set! dragged? #f)
             (start-frame-timer))]
          [(left-up)
           (when (eq? mouse-mode 'dragging)
             (set! mouse-mode 'none))
           (force-redraw)
           (start-display-icons)
           (cond
             [(and (<= 0 snip-x 24) (<= 0 snip-y 24))
              (send pict toggle-add-sunlight?)]
             [(and (<= 24 snip-x (+ 24 24)) (<= 0 snip-y 24))
              (send pict toggle-add-indicators?)]
             [(and (<= 48 snip-x (+ 48 24)) (<= 0 snip-y 24))
              (send pict toggle-add-grid?)]
             [(and (<= 72 snip-x (+ 72 24)) (<= 0 snip-y 24))
              (send pict toggle-add-wireframe)]
             [(and (<= scale-x-min snip-x scale-x-max) (<= scale-y-min snip-y scale-y-max))
              (cond [(< snip-x scale-x-mid)
                     (set! scale-index (min (+ scale-index 1) max-scale-index))]
                    [else
                     (set! scale-index (max (- scale-index 1) 0))])]
             [(and allow-capture? (not dragged?))
              ;; Center the mouse pointer (generates another mouse event)
              (define-values (x y) (snip-center-pointer pict))
              (cond [(eq? mouse-mode 'capturing)
                     ;; If capturing, stop capturing
                     (cond [editor  (queue-callback (λ () (send editor set-caret-owner #f)))]
                           [else    (set! mouse-mode 'none)
                                    (stop-frame-timer)])]
                    [(and x y)
                     ;; If not capturing and it worked, start capturing
                     (set! center-mouse-x x)
                     (set! center-mouse-y y)
                     (set! last-mouse-x x)
                     (set! last-mouse-y y)
                     (set! mouse-mode 'capturing)
                     (start-frame-timer)])])]
          [(right-down middle-down)
           (void)]
          [(right-up middle-up)
           (when (eq? mouse-mode 'none)
             (define editor-admin (and editor (send editor get-admin)))
             (send editor-admin popup-menu right-click-menu
                   (+ editorx snip-x 2)
                   (+ editory snip-y 2)))]
          [(motion)
           (cond
             [(eq? mouse-mode 'capturing)
              (unless (and (= x center-mouse-x) (= y center-mouse-y))
                ;; This event is almost certainly not generated by centering the pointer
                ;; Update the angle velocities using mouse position differences
                (unless (and (= dx 0) (= dy 0))
                  (set! yaw-vel (+ yaw-vel (fl (* 0.1 dx))))
                  (set! pitch-vel (- pitch-vel (fl (* 0.1 dy))))
                  ;; Center the mouse pointer (generates another mouse event)
                  (define-values (x y) (snip-center-pointer pict))
                  (cond [(and x y)
                         ;; Keep track of these so we can tell the next time through whether the
                         ;; mouse event was generated by centering
                         (set! center-mouse-x x)
                         (set! center-mouse-y y)
                         (start-frame-timer)]
                        ;; Both of these cases below stop mouse capture
                        [editor  (queue-callback (λ () (send editor set-caret-owner #f)))]
                        [else    (set! mouse-mode 'none)
                                 (stop-frame-timer)])))]
             [(eq? mouse-mode 'dragging)
              (mouse-moved snip-x snip-y)])])))
    
    (define/public (adjust-cursor dc snip-x snip-y e)
      (cond
        [(eq? mouse-mode 'capturing)  blank-cursor]
        [else
         (mouse-moved snip-x snip-y)
         (if (or (and (<= 0 snip-x (* 24 4)) (<= 0 snip-y 24))
                 (and (<= scale-x-min snip-x scale-x-max) (<= scale-y-min snip-y scale-y-max)))
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
          (send camera set-velocity (dir 0 0 0))
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
             [acc  (dir 0 0 0)]
             [acc  (if (keys-pressed? '(#\a #\A left))
                       (dir+ acc (dir-scale -x move-accel))
                       acc)]
             [acc  (if (keys-pressed? '(#\d #\D right))
                       (dir+ acc (dir-scale +x move-accel))
                       acc)]
             [acc  (if (keys-pressed? '(#\r #\R prior))
                       (dir+ acc (dir-scale -y move-accel))
                       acc)]
             [acc  (if (keys-pressed? '(#\f #\F next))
                       (dir+ acc (dir-scale +y move-accel))
                       acc)]
             [acc  (if (keys-pressed? '(#\s #\S down))
                       (dir+ acc (dir-scale -z move-accel))
                       acc)]
             [acc  (if (keys-pressed? '(#\w #\W up))
                       (dir+ acc (dir-scale +z move-accel))
                       acc)]
             [_    (unless (zero? (dir-dist acc))
                     (start-frame-timer))]
             [acc  (send camera rotate-direction acc)]
             [acc  (dir+ acc (dir-scale (send camera get-velocity) friction-accel))])
        (send camera accelerate acc dt))
      
      (cond
        [(eq? mouse-mode 'dragging)
         (define dx (fl (- last-snip-x drag-down-x)))
         (define dy (fl (- last-snip-y drag-down-y)))
         (define d (flsqrt (+ (sqr dx) (sqr dy))))
         (when (> d 12.0)
           (set! dragged? #t)
           (define nx (/ dx d))
           (define ny (/ dy d))
           (let ([dx  (- dx (* nx 12.0))]
                 [dy  (- dy (* ny 12.0))])
             (set! yaw-vel (+ yaw-vel (* 0.01 dx)))
             (set! pitch-vel (- pitch-vel (* 0.01 dy)))))
         (set! surface-changed? #t)
         (start-hud-timer)
         (start-frame-timer)]
        [else
         (set! dragged? #f)])
      
      (send camera change-angles yaw-vel pitch-vel)
      (set! yaw-vel (* yaw-vel #i1/3))
      (set! pitch-vel (* pitch-vel #i1/3))
      
      (maybe-redraw))
    
    ;(: render-queue (Async-Channel FlAffine3-))
    (define render-queue (make-async-channel))
    
    ;(: render-thread Thread)
    (define render-thread (make-snip-render-thread this render-queue))
    
    ;(: last-view-matrix (U #f FlAffine3-))
    (define last-view-matrix #f)
    
    ;(: maybe-redraw (-> Void))
    (define (maybe-redraw)
      (define view-matrix (send camera get-basis))
      (unless (equal? view-matrix last-view-matrix)
        (set! last-view-matrix view-matrix)
        (async-channel-put render-queue view-matrix)))
    
    (define (force-redraw)
      (set! last-view-matrix #f)
      (maybe-redraw))
    
    (maybe-redraw)
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
                check-version?
                width
                height
                z-near
                z-far
                fov
                background
                ambient
                add-sunlight?
                add-indicators?
                add-grid?
                add-wireframe
                auto-camera
                debug-shapes)
    
    (super-make-object)
    
    (send this set-snipclass snip-class)
    (send this set-flags (list* 'handles-events 'handles-all-mouse-events (send this get-flags)))
    
    (define/public (get-add-sunlight?) add-sunlight?)
    (define/public (get-add-indicators?) add-indicators?)
    (define/public (get-add-grid?) add-grid?)
    (define/public (get-add-wireframe) add-wireframe)
    (define/public (set-add-sunlight? b) (set! add-sunlight? b))
    (define/public (set-add-indicators? b) (set! add-indicators? b))
    (define/public (set-add-grid? b) (set! add-grid? b))
    (define/public (toggle-add-sunlight?) (set! add-sunlight? (not add-sunlight?)))
    (define/public (toggle-add-indicators?) (set! add-indicators? (not add-indicators?)))
    (define/public (toggle-add-grid?) (set! add-grid? (not add-grid?)))
    (define/public (toggle-add-wireframe)
      (case add-wireframe
        [(color)    (set! add-wireframe 'emitted)]
        [(emitted)  (set! add-wireframe #f)]
        [else       (set! add-wireframe 'color)]))
    
    (define/public (get-scene) scene)
    
    (define/public (get-size) (values width height))
    (define/public (get-auto-camera) auto-camera)
    
    ;(: copy (-> (Instance Pict3D%)))
    (define/override (copy)
      (make-object pict3d% scene legacy? check-version?
        width height z-near z-far fov background ambient
        add-sunlight? add-indicators? add-grid? add-wireframe
        auto-camera
        debug-shapes))
    
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
      (values legacy? check-version?
              width height z-near z-far fov background ambient
              add-sunlight? add-indicators? add-grid? add-wireframe
              auto-camera
              debug-shapes))
    
    (define/public (refresh)
      (queue-callback
       (λ ()
         (define admin (send this get-admin))
         (when admin
           (send admin needs-update this 0 0 (+ width 4) (+ height 4))))))
    
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
    (define gui-mutex (make-semaphore 1))
    (define (get-gui)
      (call-with-semaphore
       gui-mutex
       (λ ()
         (let ([gui-val  gui])
           (cond [gui-val  gui-val]
                 [else  (define gui-val (make-object pict3d-gui% this))
                        (set! gui gui-val)
                        gui-val])))))
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (with-reentry-lock draw
        (define smoothing (send dc get-smoothing))
        (send dc set-smoothing 'unsmoothed)
        (send dc set-brush trans-brush)
        (send dc set-pen black-pen)
        (send dc draw-rectangle (+ x 0.5) (+ y 0.5) (+ width 4) (+ height 4))
        (send dc set-pen white-pen)
        (send dc draw-rectangle (+ x 1.5) (+ y 1.5) (+ width 2) (+ height 2))
        (send dc draw-bitmap (get-the-bitmap) (+ x 2) (+ y 2))
        (send dc set-smoothing smoothing)
        (send (get-gui) draw dc x y)))
    
    (define snip-class-added? #f)
    
    (define/override (get-extent dc x y [w #f] [h #f] [descent #f] [space #f] [lspace #f] [rspace #f])
      (unless snip-class-added?
        (set! snip-class-added? #t)
        (send (get-the-snip-class-list) add snip-class))
      (when (box? w) (set-box! w (+ width 4)))
      (when (box? h) (set-box! h (+ height 4)))
      (when (box? descent) (set-box! descent 0))
      (when (box? space) (set-box! space 0))
      (when (box? lspace) (set-box! lspace 0))
      (when (box? rspace) (set-box! rspace 0)))
    
    (define/override (own-caret own-it?)
      (send (get-gui) own-caret own-it?)
      (super own-caret own-it?))
    
    (define/override (on-event dc x y editorx editory e)
      (with-reentry-lock on-event
        (send (get-gui) on-event dc x y editorx editory e)
        (super on-event dc x y editorx editory e)))
    
    (define/override (on-char dc x y editorx editory e)
      (with-reentry-lock on-char
        (send (get-gui) on-char e)
        (super on-char dc x y editorx editory e)))
    
    (define/override (adjust-cursor dc orig-x orig-y editorx editory e)
      (with-reentry-lock adjust-cursor
        (define snip-x (- (send e get-x) orig-x 2))
        (define snip-y (- (send e get-y) orig-y 2))
        (cond [(and (<= 0 snip-x width) (<= 0 snip-y height))
               (send (get-gui) adjust-cursor dc snip-x snip-y e)]
              [else  #f])))
    ))

(define (scene->pict3d% s)
  (make-object pict3d%
    s
    (current-pict3d-legacy?)
    (current-pict3d-check-version?)
    (current-pict3d-width)
    (current-pict3d-height)
    (current-pict3d-z-near)
    (current-pict3d-z-far)
    (current-pict3d-fov)
    (current-pict3d-background)
    (current-pict3d-ambient)
    (current-pict3d-add-sunlight?)
    (current-pict3d-add-indicators?)
    (current-pict3d-add-grid?)
    (current-pict3d-add-wireframe)
    (current-pict3d-auto-camera)
    (current-engine-debug-shapes)))

(define (pict3d%->scene p)
  (send p get-scene))
