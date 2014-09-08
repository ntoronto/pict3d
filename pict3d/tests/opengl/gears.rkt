;; $Id: gears.ss,v 1.8 2005/01/12 12:49:10 mflatt Exp $
;;
;; This is a version of the venerable "gears" demo for PLT Scheme 200 using
;; Scott Owens' SGL OpenGL bindings.  It was ported from "glxgears.c" 1.3 from
;; XFree86, which had the following notices:
;;
;;     Copyright (C) 1999-2001  Brian Paul   All Rights Reserved.
;;
;;     Permission is hereby granted, free of charge, to any person obtaining a
;;     copy of this software and associated documentation files (the
;;     "Software"), to deal in the Software without restriction, including
;;     without limitation the rights to use, copy, modify, merge, publish,
;;     distribute, sublicense, and/or sell copies of the Software, and to
;;     permit persons to whom the Software is furnished to do so, subject to
;;     the following conditions:
;;
;;     The above copyright notice and this permission notice shall be included
;;     in all copies or substantial portions of the Software.
;;
;;     XFree86: xc/programs/glxgears/glxgears.c,v 1.3 2001/11/03 17:29:20 dawes
;;
;;     This is a port of the infamous "gears" demo to straight GLX (i.e. no
;;     GLUT).  Port by Brian Paul 23 March 2001.
;;
;; To run, evaluate this file in DrRacket in the "module" language level,
;; or execute "mred -qu gears.ss" from your OS shell.
;;
;; Scheme port by Neil W. Van Dyke <neil@neilvandyke.org>, 23 November 2002.
;; Originally called glxgears.ss.  Minor modifications since.
;; See "http://www.neilvandyke.org/opengl-plt/" for more information.
;;
;; Updated to newer sgl interface by Scott Owens
;; Modified to rgl interface by Stephan Houben

#lang racket/gui

(require typed/opengl)
(require typed/opengl/ffi)


(define controls? #t)

(define gears-canvas%
  (class* canvas% ()

    (inherit refresh with-gl-context swap-gl-buffers get-parent)

    (define rotation 0.0)

    (define view-rotx 20.0)
    (define view-roty 30.0)
    (define view-rotz 0.0)

    (define gear1 #f)
    (define gear2 #f)
    (define gear3 #f)

    (define step? #f)

    (define/public (run)
      (set! step? #t)
      (refresh))

    (define/public (move-left)
      (set! view-roty (+ view-roty 5.0))
      (refresh))

    (define/public (move-right)
      (set! view-roty (- view-roty 5.0))
      (refresh))

    (define/public (move-up)
      (set! view-rotx (+ view-rotx 5.0))
      (refresh))

    (define/public (move-down)
      (set! view-rotx (- view-rotx 5.0))
      (refresh))

    (define (build-gear inner-radius    ; radius of hole at center
                        outer-radius    ; radius at center of teeth
                        width           ; width of gear
                        teeth           ; number of teeth
                        tooth-depth)    ; depth of tooth
      (let* ((r0             inner-radius)
             (r1             (- outer-radius (/ tooth-depth 2.0)))
             (r2             (+ outer-radius (/ tooth-depth 2.0)))
             (da             (/ (* 2.0 pi) teeth 4.0))
             (da2            (* da 2))
             (da3            (* da 3))
             (half-width     (* width 0.5))
             (neg-half-width (- half-width)))

        ;; TODO: Generalize away some more redundant program text.

        (glShadeModel GL_FLAT)

        (glNormal3d 0.0 0.0 1.0)

	;; Draw front face.
        (glBegin GL_QUAD_STRIP)
        (do ((i 0 (+ 1 i))) ((> i teeth))
          (let* ((angle     (/ (* i 2.0 pi) teeth))
                 (cos-angle (cos angle))
                 (sin-angle (sin angle)))
            (glVertex3d (* r0 cos-angle) (* r0 sin-angle) half-width)
            (glVertex3d (* r1 cos-angle) (* r1 sin-angle) half-width)
            (when (< i teeth)
              (glVertex3d (* r0 cos-angle)
                            (* r0 sin-angle)
                            (* half-width))
              (glVertex3d (* r1 (cos (+ angle da3)))
                            (* r1 (sin (+ angle da3)))
                            half-width))))
        (glEnd)

        ;; Draw front sides of teeth.
        (glBegin GL_QUADS)
        (do ((i 0 (+ 1 i))) ((= i teeth))
          (let ((angle (/ (* i 2.0 pi) teeth)))
            (glVertex3d (* r1 (cos angle))
                       (* r1 (sin angle))
                       half-width)
            (glVertex3d (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       half-width)
            (glVertex3d (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       half-width)
            (glVertex3d (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       half-width)))
        (glEnd)

        (glNormal3d 0.0 0.0 -1.0)

        ;; Draw back face.
        (glBegin GL_QUAD_STRIP)
        (do ((i 0 (+ 1 i))) ((> i teeth))
          (let* ((angle     (/ (* i 2.0 pi) teeth))
                 (cos-angle (cos angle))
                 (sin-angle (sin angle)))
            (glVertex3d (* r1 cos-angle) (* r1 sin-angle) neg-half-width)
            (glVertex3d (* r0 cos-angle) (* r0 sin-angle) neg-half-width)
            (when (< i teeth)
              (glVertex3d (* r1 (cos (+ angle da3)))
                         (* r1 (sin (+ angle da3)))
                         neg-half-width)
              (glVertex3d (* r0 cos-angle)
                         (* r0 sin-angle)
                         neg-half-width))))
        (glEnd)

        ;; Draw back sides of teeth.
        (glBegin GL_QUADS)
        (do ((i 0 (+ 1 i))) ((= i teeth))
          (let ((angle (/ (* i 2.0 pi) teeth)))
            (glVertex3d (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       neg-half-width)
            (glVertex3d (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       neg-half-width)
            (glVertex3d (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       neg-half-width)
            (glVertex3d (* r1 (cos angle))
                       (* r1 (sin angle))
                       neg-half-width)))
        (glEnd)

        ;; Draw outward faces of teeth.
        (glBegin GL_QUAD_STRIP)
        (do ((i 0 (+ 1 i))) ((= i teeth))
          (let* ((angle     (/ (* i 2.0 pi) teeth))
                 (cos-angle (cos angle))
                 (sin-angle (sin angle)))

            (glVertex3d (* r1 cos-angle) (* r1 sin-angle) half-width)
            (glVertex3d (* r1 cos-angle) (* r1 sin-angle) neg-half-width)

            (let* ((u   (- (* r2 (cos (+ angle da))) (* r1 cos-angle)))
                   (v   (- (* r2 (sin (+ angle da))) (* r1 sin-angle)))
                   (len (sqrt (+ (* u u) (* v v)))))
              (glNormal3d (/ v len) (- (/ u len)) 0.0))

            (glVertex3d (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       half-width)
            (glVertex3d (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       neg-half-width)
            (glNormal3d cos-angle sin-angle 0.0)
            (glVertex3d (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       half-width)
            (glVertex3d (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       neg-half-width)

            (let ((u (- (* r1 (cos (+ angle da3)))
                        (* r2 (cos (+ angle da2)))))
                  (v (- (* r1 (sin (+ angle da3)))
                        (* r2 (sin (+ angle da2))))))
              (glNormal3d v (- u) 0.0))

            (glVertex3d (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       half-width)
            (glVertex3d (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       neg-half-width)
            (glNormal3d cos-angle sin-angle 0.0)))

        (glVertex3d (* r1 (cos 0)) (* r1 (sin 0)) half-width)
        (glVertex3d (* r1 (cos 0)) (* r1 (sin 0)) neg-half-width)
        (glEnd)

        (glShadeModel GL_SMOOTH)

        ;; Draw inside radius cylinder.
        (glBegin GL_QUAD_STRIP)
        (do ((i 0 (+ 1 i))) ((> i teeth))
          (let* ((angle     (/ (* i 2.0 pi) teeth))
                 (cos-angle (cos angle))
                 (sin-angle (sin angle)))
            (glNormal3d (- cos-angle) (- sin-angle) 0.0)
            (glVertex3d (* r0 cos-angle) (* r0 sin-angle) neg-half-width)
            (glVertex3d (* r0 cos-angle) (* r0 sin-angle) half-width)))
        (glEnd)))

    (define/override (on-size width height)
      (with-gl-context
       (lambda ()

         (unless gear1
           (printf "  RENDERER:   ~A\n" (glGetString GL_RENDERER))
           (printf "  VERSION:    ~A\n" (glGetString GL_VERSION))
           (printf "  VENDOR:     ~A\n" (glGetString GL_VENDOR))
           (printf "  EXTENSIONS: ~A\n" (glGetString GL_EXTENSIONS)))

         (glViewport 0 0 width height)
         (glMatrixMode GL_PROJECTION)
         (glLoadIdentity)
         (let ((h (/ height width)))
           (glFrustum -1.0 1.0 (- h) h 5.0 60.0))
         (glMatrixMode GL_MODELVIEW)
         (glLoadIdentity)
         (glTranslated 0.0 0.0 -40.0)

         (glLightfv GL_LIGHT0 GL_POSITION (f32vector 5.0 5.0 10.0 0.0))
         (glEnable GL_CULL_FACE)
         (glEnable GL_LIGHTING)
         (glEnable GL_LIGHT0)
         (glEnable GL_DEPTH_TEST)

         (unless gear1

           (set! gear1 (glGenLists 1))
           (glNewList gear1 GL_COMPILE)
           (glMaterialfv GL_FRONT
                           GL_AMBIENT_AND_DIFFUSE
                           (f32vector 0.8 0.1 0.0 1.0))
           (build-gear 1.0 4.0 1.0 20 0.7)
           (glEndList)

           (set! gear2 (glGenLists 1))
           (glNewList gear2 GL_COMPILE)
           (glMaterialfv GL_FRONT
                           GL_AMBIENT_AND_DIFFUSE
                           (f32vector 0.0 0.8 0.2 1.0))
           (build-gear 0.5 2.0 2.0 10 0.7)
           (glEndList)

           (set! gear3 (glGenLists 1))
           (glNewList gear3 GL_COMPILE)
           (glMaterialfv GL_FRONT
                           GL_AMBIENT_AND_DIFFUSE
                           (f32vector 0.2 0.2 1.0 1.0))
           (build-gear 1.3 2.0 0.5 10 0.7)
           (glEndList)

           (glEnable GL_NORMALIZE))))
      (refresh))

    (define sec (current-seconds))
    (define frames 0)
    
    (define/override (on-paint)
      (when gear1
	(when (>= (- (current-seconds) sec) 5)
	  (send (get-parent) set-status-text (format "~a fps" (/ (exact->inexact frames) 5)))
	  (set! sec (current-seconds))
	  (set! frames 0))
	(set! frames (add1 frames))
        
	(when step?
	  ;; TODO: Don't increment this infinitely.
	  (set! rotation (+ 2.0 rotation)))
	(with-gl-context
	 (lambda ()

	   (glClearColor 0.0 0.0 0.0 0.0)
	   (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

	   (glPushMatrix)
	   (glRotated view-rotx 1.0 0.0 0.0)
	   (glRotated view-roty 0.0 1.0 0.0)
	   (glRotated view-rotz 0.0 0.0 1.0)

	   (glPushMatrix)
	   (glTranslated -3.0 -2.0 0.0)
	   (glRotated rotation 0.0 0.0 1.0)
	   (glCallList gear1)
	   (glPopMatrix)

	   (glPushMatrix)
	   (glTranslated 3.1 -2.0 0.0)
	   (glRotated (- (* -2.0 rotation) 9.0) 0.0 0.0 1.0)
	   (glCallList gear2)
	   (glPopMatrix)

	   (glPushMatrix)
	   (glTranslated -3.1 4.2 0.0)
	   (glRotated (- (* -2.0 rotation) 25.0) 0.0 0.0 1.0)
	   (glCallList gear3)
	   (glPopMatrix)

	   (glPopMatrix)

	   (swap-gl-buffers)
	   (glFlush)))
	(when step?
	  (set! step? #f)
	  (queue-callback (lambda x (send this run)) #f))))

    (super-instantiate () (style '(gl no-autoclear)))))

(define (f)
  (let* ((f (make-object frame% "gears.ss" #f))
         (c (instantiate gears-canvas% (f) (min-width 300) (min-height 300))))
    (send f create-status-line)
    (when controls?
      (let ((h (instantiate horizontal-panel% (f)
                 (alignment '(center center)) (stretchable-height #f))))
        (instantiate button%
          ("Start" h (lambda (b e) (send b enable #f) (send c run)))
          (stretchable-width #t) (stretchable-height #t))
        (let ((h (instantiate horizontal-panel% (h)
                   (alignment '(center center)))))
          (instantiate button% ("Left" h (lambda x (send c move-left)))
            (stretchable-width #t))
          (let ((v (instantiate vertical-panel% (h)
                     (alignment '(center center)) (stretchable-width #f))))
            (instantiate button% ("Up" v (lambda x (send c move-up)))
              (stretchable-width #t))
            (instantiate button% ("Down" v (lambda x (send c move-down)))
              (stretchable-width #t)))
          (instantiate button% ("Right" h (lambda x (send c move-right)))
            (stretchable-width #t)))))
    (send f show #t)))
  (f)
;;eof
