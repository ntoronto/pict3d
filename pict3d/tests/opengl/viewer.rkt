;; A simple viewer window for OpenGL.
;; Allows user to rotate and zoom the scene.
#lang racket/gui

(require typed/opengl)

(provide view)

(define gl-viewer%
  (class canvas%
    (super-new)
    (inherit with-gl-context swap-gl-buffers refresh)

    (init-field draw)
    (init-field (setup void))

    (define setup-called #f)

    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (glViewport 0 0 width height)
         (glMatrixMode GL_PROJECTION)
         (glLoadIdentity)
         (if (< width height)
           (let ((h (/ height width)))
             (glFrustum -1.0 1.0 (- h) h 8.0 12.0))
           (let ((h (/ width height)))
             (glFrustum (- h) h -1.0 1.0 8.0 12.0)))
         (glMatrixMode GL_MODELVIEW)
         (glLoadIdentity)
         (glTranslated 0.0 0.0 -10.0))))

    (define x-rotation 0)
    (define y-rotation 0)
    (define zoom 1)

    (define/override (on-paint)
      (with-gl-context
        (lambda ()
          (unless setup-called
            (setup)
            (set! setup-called #t))
          (glClearColor 0.0 0.0 0.3 0.0) ; darkish blue
          (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
          (glPushMatrix)
          (glScaled zoom zoom zoom)
          (glRotated y-rotation 1 0 0)
          (glRotated x-rotation 0 1 0)
          (draw)
          (glPopMatrix)))
      (swap-gl-buffers))

    (define handle-motion void)

    (define/override (on-event event)
      (let ((x (send event get-x))
            (y (send event get-y)))
        (case (send event get-event-type)
          ((left-down)
           (set! handle-motion
             (let ((old-x x) (old-y y))
               (lambda (new-x new-y)
                 (set! x-rotation (+ x-rotation (- new-x old-x)))
                 (set! y-rotation (+ y-rotation (- new-y old-y)))
                 (set! old-x new-x)
                 (set! old-y new-y)
                 (refresh)))))
          ((left-up)
           (set! handle-motion void))
          ((motion) (handle-motion x y)))))

    (define/override (on-char event)
      (case (send event get-key-code)
        ((#\+) (set! zoom (* zoom 4/3)) (refresh))
        ((#\-) (set! zoom (/ zoom 4/3)) (refresh))
        ((wheel-up) (set! zoom (* zoom 9/8)) (refresh))
        ((wheel-down) (set! zoom (/ zoom 9/8)) (refresh))))))


(define (show-gl-info frame canvas)
  (let-values (((renderer version vendor extensions)
                (send canvas with-gl-context
                      (lambda ()
                        (values
                          (glGetString GL_RENDERER)
                          (glGetString GL_VERSION)
                          (glGetString GL_VENDOR)
                          (gl-extensions))))))
    (define label
      (format "RENDERER: ~a~%VERSION: ~a~%VENDOR: ~a"
              renderer version vendor))
    (define dialog (new dialog% [parent frame] [label "OpenGL info"]))
    (define msg (new message%
                     [parent dialog]
                     [label label]))
    (define extensions-list (new list-box%
                                 [parent dialog]
                                 [label "EXTENSIONS:"]
                                 [style '(single vertical-label)]
                                 [choices
                                   (sort
                                     (for/list ((ext (in-set extensions)))
                                       (symbol->string ext))
                                     string<?)]))
    (send dialog show #t)))


(define (view draw (setup void))
  (define frame
    (new frame%
         [label "OpenGL viewer"]
         [width 300]
         [height 300]))

  (define menubar
    (new menu-bar% [parent frame]))

  (define help-menu
    (new menu% [parent menubar] [label "&Help"]))

  (define c
    (new gl-viewer%
         (style '(gl no-autoclear))
         (parent frame)
         (draw draw) (setup setup)))

  (define gl-info-item
    (new menu-item% [parent help-menu] [label "GL info"]
         [callback (lambda (i e) (show-gl-info frame c))]))

  (send frame show #t))
