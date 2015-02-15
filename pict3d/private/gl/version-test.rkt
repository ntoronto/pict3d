#lang racket

(provide test-opengl-version)

(module sane-version-test racket/base
  (require racket/class
           typed/opengl
           "../utils.rkt")
  
  (provide sane-test-opengl-version)
  
  (define (sane-test-opengl-version ctxt lowest)
    (log-pict3d-info "<engine> running sane version test")
    (with-handlers ([exn?  (λ (e) e)])
      (send ctxt call-as-current
            (λ () (list (gl-version-at-least? lowest)
                        (gl-version)
                        (gl-core-profile?))))))
  )

(module x11-version-test racket/base
  (require ffi/unsafe
           ffi/unsafe/define
           mred/private/wx/gtk/x11
           racket/class
           typed/opengl
           "../utils.rkt")
  
  (provide x11-test-opengl-version)
  
  (define (ffi-lib/complaint-on-failure name vers)
    (ffi-lib name vers
             #:fail (lambda ()
                      (log-warning "could not load library ~a ~a"
                                   name vers)
                      #f)))
  
  (define x-lib (ffi-lib/complaint-on-failure "libX11" '("")))
  
  (define-ffi-definer define-x x-lib
    #:default-make-fail make-not-available)
  
  ;; X #defines/typedefs/enums
  (define _Display (_cpointer 'Display))
  (define _XErrorEvent (_cpointer 'XErrorEvent))
  (define False 0)
  
  (define-x XSetErrorHandler
    (_fun (_fun _Display _XErrorEvent -> _int)
          -> (_fun _Display _XErrorEvent -> _int)))
  
  (define-x XSync
    (_fun _Display _int -> _void))
  
  (define (x11-test-opengl-version ctxt lowest)
    (log-pict3d-info "<engine> running X11 version test")
    (with-handlers ([exn?  (λ (e) e)])
      (define error? #f)
      (define (x-error-handler xdisplay xerrorevent)
        (set! error? #t)
        0)
      (define old-handler #f)
      
      (define display (send ctxt get-gtk-display))
      (define xdisplay (gdk_x11_display_get_xdisplay display))
      (XSync xdisplay False)
      
      (dynamic-wind
       (λ ()
         (set! old-handler (XSetErrorHandler x-error-handler)))
       (λ ()
         (send ctxt call-as-current
               (λ () (list (gl-version-at-least? lowest)
                           (gl-version)
                           (gl-core-profile?)))))
       (λ ()
         ;; Sync to ensure errors are processed
         (XSync xdisplay False)
         (XSetErrorHandler old-handler)
         (when error? (error "an X11 error occurred"))))))
  )

(require racket/lazy-require)

(lazy-require [(submod "." x11-version-test) (x11-test-opengl-version)]
              [(submod "." sane-version-test) (sane-test-opengl-version)])

(define (test-opengl-version ctxt lowest)
  (case (system-type 'os)
    [(unix)  (x11-test-opengl-version ctxt lowest)]
    [else    (sane-test-opengl-version ctxt lowest)]))
