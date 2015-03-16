#lang racket/base

(require racket/gui/base
         racket/class
         racket/draw
         typed/opengl
         "../utils.rkt")

(provide get-bitmap-context
         get-invisible-canvas-context)

(define invisible-context-max-width 1024)
(define invisible-context-max-height 1024)

;; Contains handles to objects necessary to keep OpenGL contexts alive (e.g. frames)
(define keep-alives (make-weak-hasheq))

(define (get-bitmap-context legacy? check-version?)
  (define config (new gl-config%))
  (send config set-legacy? legacy?)
  (define bm (make-gl-bitmap invisible-context-max-width invisible-context-max-height config))
  (define dc (make-object bitmap-dc% bm))
  (define ctxt (send dc get-gl-context))
  (cond
    [(or (not ctxt) (not (send ctxt ok?)))
     (log-pict3d-warning "<engine> could not get bitmap OpenGL context (legacy? = ~a)" legacy?)
     #f]
    [else
     (define version-ok
       (send ctxt call-as-current (λ () (with-handlers ([exn?  (λ (e) e)])
                                          (gl-version-at-least? 30)))))
     (cond
       [(exn? version-ok)
        (log-pict3d-error
         "<engine> exception querying bitmap OpenGL context version (legacy? = ~a): ~e"
         legacy? version-ok)
        #f]
       [(or (not check-version?) version-ok)
        (define version (send ctxt call-as-current gl-version))
        (log-pict3d-info "<engine> got bitmap OpenGL ~a context (legacy? = ~a)" version legacy?)
        ;; Keep the bitmap and dc alive
        (hash-set! keep-alives ctxt (list bm dc))
        ctxt]
       [else
        (define version (send ctxt call-as-current gl-version))
        (log-pict3d-warning "<engine> got bitmap OpenGL ~a context (legacy? = ~a)" version legacy?)
        #f])]))

(define (get-invisible-canvas-context legacy? check-version?)
  (define config (new gl-config%))
  (send config set-legacy? legacy?)
  (define frame (new frame%
                     [label "\"Invisible\" GL Frame"]
                     [width   invisible-context-max-width]
                     [height  invisible-context-max-height]
                     [min-width   invisible-context-max-width]
                     [min-height  invisible-context-max-height]
                     [stretchable-width #f]
                     [stretchable-height #f]))
  (define canvas (new canvas% [parent frame] [style '(gl no-autoclear)] [gl-config config]))
  (send frame show #t)
  (send frame show #f)
  (sleep/yield 1)
  (define ctxt (send (send canvas get-dc) get-gl-context))
  (cond
    [(or (not ctxt) (not (send ctxt ok?)))
     (log-pict3d-warning "<engine> could not get canvas OpenGL context (legacy? = ~a)" legacy?)
     #f]
    [else
     (define version-ok
       (send ctxt call-as-current (λ () (with-handlers ([exn?  (λ (e) e)])
                                          (gl-version-at-least? 30)))))
     (cond
       [(exn? version-ok)
        (log-pict3d-error
         "<engine> exception querying canvas OpenGL context version (legacy? = ~a): ~e"
         legacy? version-ok)
        #f]
       [(or (not check-version?) version-ok)
        (define version (send ctxt call-as-current gl-version))
        (log-pict3d-info "<engine> got canvas OpenGL ~a context (legacy? = ~a)" version legacy?)
        ;; Keep the frame and (just in case) the canvas alive
        (hash-set! keep-alives ctxt (list frame canvas))
        ctxt]
       [else
        (define version (send ctxt call-as-current gl-version))
        (log-pict3d-warning "<engine> got canvas OpenGL ~a context (legacy? = ~a)" version legacy?)
        #f])]))
