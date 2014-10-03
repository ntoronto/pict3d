#lang racket/base

(require racket/class
         racket/gui
         typed/opengl)

(provide (all-defined-out))

;; ===================================================================================================
;; Managed OpenGL contexts

(struct gl-context (number) #:transparent)

(define gl-contexts (make-weak-hasheq))

(define managed-gl-context
  (let ([next-number  0])
    (λ (ctxt)
      (define number next-number)
      (set! next-number (+ 1 number))
      (define key (gl-context number))
      (hash-set! gl-contexts key ctxt)
      key)))

(define (gl-context-context key)
  (hash-ref gl-contexts key (λ () (raise-argument-error 'gl-context-context "valid gl-context" key))))

(define (gl-context-ok? ctxt)
  (send (gl-context-context ctxt) ok?))

(define current-gl-context (make-parameter #f))

(define (call-with-gl-context thunk new-ctxt)
  (define ctxt (current-gl-context))
  (cond [(not ctxt)
         (send (gl-context-context new-ctxt) call-as-current
               (λ () (parameterize ([current-gl-context  new-ctxt]) (thunk))))]
        [(eq? new-ctxt ctxt)
         (send (gl-context-context new-ctxt) call-as-current thunk)]
        [else
         (error 'call-with-gl-context "already in another managed OpenGL context")]))

(define (get-current-gl-context name)
  (define ctxt (current-gl-context))
  (if ctxt ctxt (error name "not in a managed OpenGL context (use with-gl-context)")))

(define (gl-swap-buffers)
  (define ctxt (get-current-gl-context 'gl-swap-buffers))
  (send (gl-context-context ctxt) swap-buffers))

;; ===================================================================================================
;; Master GL context

(define master-context-max-width 4096)
(define master-context-max-height 4096)

(define master-gl-context-mutex (make-semaphore 1))

(define master-frame #f)
(define master-bitmap #f)
(define master-dc #f)
(define master-context #f)

(define (get-master-gl-context/bitmap)
  (define config (new gl-config%))
  (send config set-legacy? #f)
  (define bm (make-gl-bitmap master-context-max-width master-context-max-height config))
  (define dc (make-object bitmap-dc% bm))
  (define ctxt (send dc get-gl-context))
  (cond [(or (not ctxt) (not (send ctxt ok?)))
         (log-warning "get-master-gl-context: could not obtain bitmap OpenGL context")]
        [(send ctxt call-as-current (λ () (gl-version-at-least? '(3 0))))
         (match-define (list major minor) (send ctxt call-as-current (λ () (gl-version))))
         (log-info "get-master-gl-context: obtained OpenGL ~a.~a bitmap context" major minor)
         (set! master-bitmap bm)
         (set! master-dc dc)
         (let ([ctxt  (managed-gl-context ctxt)])
           (set! master-context ctxt)
           ctxt)]
        [else
         (match-define (list major minor) (send ctxt call-as-current (λ () (gl-version))))
         (log-warning "get-master-gl-context: obtained OpenGL ~a.~a bitmap context" major minor)
         #f]))

(define (get-master-gl-context/frame)
  (define config (new gl-config%))
  (send config set-legacy? #f)
  (define frame (new frame%
                     [label "Master GL context frame"]
                     [width   master-context-max-width]
                     [height  master-context-max-height]
                     [min-width   master-context-max-width]
                     [min-height  master-context-max-height]
                     [stretchable-width #f]
                     [stretchable-height #f]))
  (define canvas (new canvas% [parent frame] [style '(gl no-autoclear)] [gl-config config]))
  (send frame show #t)
  (send frame show #f)
  (sleep/yield 1)
  (define ctxt (send (send canvas get-dc) get-gl-context))
  (cond [(or (not ctxt) (not (send ctxt ok?)))
         (log-warning "get-master-gl-context: could not obtain canvas OpenGL context")]
        [(send ctxt call-as-current (λ () (gl-version-at-least? '(3 0))))
         (match-define (list major minor) (send ctxt call-as-current (λ () (gl-version))))
         (log-info "get-master-gl-context: obtained OpenGL ~a.~a canvas context" major minor)
         (set! master-frame frame)
         (let ([ctxt  (managed-gl-context ctxt)])
           (set! master-context ctxt)
           ctxt)]
        [else
         (match-define (list major minor) (send ctxt call-as-current (λ () (gl-version))))
         (log-warning "get-master-gl-context: obtained OpenGL ~a.~a canvas context" major minor)
         #f]))

(define (get-master-gl-context)
  (call-with-semaphore
   master-gl-context-mutex
   (λ ()
     (define ctxt master-context)
     (cond
       [ctxt  ctxt]
       [else
        (define ctxt (get-master-gl-context/bitmap))
        (cond
          [ctxt  ctxt]
          [else
           (define ctxt (get-master-gl-context/frame))
           (cond [ctxt  ctxt]
                 [else
                  (error 'get-master-gl-context
                         "could not get an OpenGL 3.0 context")])])]))))

(define (make-shared-gl-config)
  (define config (new gl-config%))
  (send config set-legacy? #f)
  (send config set-share-context (gl-context-context (get-master-gl-context)))
  config)
