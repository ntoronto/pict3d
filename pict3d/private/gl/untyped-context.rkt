#lang racket/base

(require racket/class
         racket/gui
         typed/opengl
         "../utils.rkt")

(provide (all-defined-out))

(define pict3d-legacy-contexts? (make-parameter #f))

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

(struct garbage (name handle delete) #:transparent)

;(: gl-context-garbage (HashTable gl-context (Listof garbage)))
(define gl-context-garbage (make-weak-hasheq))

(define (gl-delete-later ctxt name handle delete)
  (define gs (hash-ref gl-context-garbage ctxt (λ () empty)))
  (hash-set! gl-context-garbage ctxt (cons (garbage name handle delete) gs)))

(define (clean-gl-garbage ctxt)
  (define gs (hash-ref gl-context-garbage ctxt (λ () empty)))
  (for ([g  (in-list gs)])
    (match-define (garbage name handle delete) g)
    (delete handle))
  (hash-set! gl-context-garbage ctxt empty))

(define (call-with-gl-context thunk new-ctxt)
  (define ctxt (current-gl-context))
  (cond [(not ctxt)
         (send (gl-context-context new-ctxt) call-as-current
               (λ () (parameterize ([current-gl-context  new-ctxt])
                       (clean-gl-garbage new-ctxt)
                       (thunk))))]
        [(eq? new-ctxt ctxt)
         (send (gl-context-context new-ctxt) call-as-current thunk)]
        [else
         (error 'call-with-gl-context "already in another managed OpenGL context")]))

(define (get-current-managed-gl-context name)
  (define ctxt (current-gl-context))
  (if ctxt ctxt (error name "not in a managed OpenGL context (use with-gl-context)")))

(define (gl-swap-buffers)
  (define ctxt (get-current-managed-gl-context 'gl-swap-buffers))
  (send (gl-context-context ctxt) swap-buffers)
  ;; Windows' gl-context%'s swap-buffers can currently return #t
  (void))

;; ===================================================================================================
;; Master GL context

(define master-context-max-width 4096)
(define master-context-max-height 4096)

(define master-gl-context-mutex (make-semaphore 1))

(define master-legacy? #f)
(define master-frame #f)
(define master-bitmap #f)
(define master-dc #f)
(define master-context #f)

(define (get-master-gl-context/bitmap legacy?)
  (define config (new gl-config%))
  (send config set-legacy? legacy?)
  (define bm (make-gl-bitmap master-context-max-width master-context-max-height config))
  (define dc (make-object bitmap-dc% bm))
  (define ctxt (send dc get-gl-context))
  (cond
    [(or (not ctxt) (not (send ctxt ok?)))
     (log-pict3d-warning
      "<engine> could not obtain bitmap OpenGL context (pict3d-legacy-contexts? ~a)"
      legacy?)]
    [else
     (define version-ok
       (send ctxt call-as-current (λ () (with-handlers ([exn?  (λ (e) e)])
                                          (gl-version-at-least? 30)))))
     (cond
       [(exn? version-ok)
        (log-pict3d-error
         "<engine> exception querying bitmap OpenGL context version (pict3d-legacy-contexts? ~a): ~e"
         legacy? version-ok)
        #f]
       [version-ok
        (define version (send ctxt call-as-current gl-version))
        (log-pict3d-info
         "<engine> obtained bitmap OpenGL ~a context (pict3d-legacy-contexts? ~a)"
         version legacy?)
        (set! master-legacy? legacy?)
        (set! master-bitmap bm)
        (set! master-dc dc)
        (let ([ctxt  (managed-gl-context ctxt)])
          (set! master-context ctxt)
          ctxt)]
       [else
        (define version (send ctxt call-as-current gl-version))
        (log-pict3d-warning
         "<engine> obtained bitmap OpenGL ~a context (pict3d-legacy-contexts? ~a)"
         version legacy?)
        #f])]))

(define (get-master-gl-context/frame legacy?)
  (define config (new gl-config%))
  (send config set-legacy? legacy?)
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
  (cond
    [(or (not ctxt) (not (send ctxt ok?)))
     (log-pict3d-warning
      "<engine> could not obtain canvas OpenGL context (pict3d-legacy-contexts? ~a)"
      legacy?)]
    [else
     (define version-ok
       (send ctxt call-as-current (λ () (with-handlers ([exn?  (λ (e) e)])
                                          (gl-version-at-least? 30)))))
     (cond
       [(exn? version-ok)
        (log-pict3d-error
         "<engine> exception querying canvas OpenGL context version (pict3d-legacy-contexts? ~a): ~e"
         legacy? version-ok)
        #f]
       [version-ok
        (define version (send ctxt call-as-current gl-version))
        (log-pict3d-info
         "<engine> obtained canvas OpenGL ~a context (pict3d-legacy-contexts? ~a)"
         version legacy?)
        (set! master-legacy? legacy?)
        (set! master-frame frame)
        (let ([ctxt  (managed-gl-context ctxt)])
          (set! master-context ctxt)
          ctxt)]
       [else
        (define version (send ctxt call-as-current gl-version))
        (log-pict3d-warning
         "<engine> obtained canvas OpenGL ~a context (pict3d-legacy-contexts? ~a)"
         version legacy?)
        #f])]))

(define (get-master-gl-context legacy?)
  (call-with-semaphore
   master-gl-context-mutex
   (λ ()
     (define ctxt master-context)
     (cond
       [(and ctxt (eq? legacy? master-legacy?))  ctxt]
       [else
        ;; Don't try for bitmap contexts for now - they're too broken on Windows and possibly Mac
        (define ctxt #f #;(get-master-gl-context/bitmap legacy?))
        (cond
          [ctxt  ctxt]
          [else
           (define ctxt (get-master-gl-context/frame legacy?))
           (cond [ctxt  ctxt]
                 [else
                  (error 'get-master-gl-context
                         "could not obtain at least an OpenGL 30 context (pict3d-legacy-contexts? ~a)"
                         legacy?)])])]))))
