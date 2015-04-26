#lang typed/racket/base

(require (for-syntax racket/base)
         "gl.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Single-value memoization

(: cache-singleton (All (B A ...) (-> (-> A ... B)
                                      (-> A ... B))))
(define (cache-singleton f)
  (: cache (U #f (Pair (List A ...) B)))
  (define cache #f)
  
  (λ args
    (: apply-f (-> (List A ...) B))
    (define (apply-f args)
      (define value (apply f args))
      (set! cache (cons args value))
      value)
    
    (define entry cache)
    (cond [(not entry)  (apply-f args)]
          [else
           (define res (equal? args (car entry)))
           (cond [(eq? res #t)  (cdr entry)]
                 [else  (apply-f (if res res args))])])))

(: cache-thunk (All (B) (-> (-> B) (-> B))))
(define (cache-thunk f)
  (: cache (U #f B))
  (define cache #f)
  (λ ()
    (define entry cache)
    (cond [entry  entry]
          [else
           (define value (f))
           (set! cache value)
           value])))

(define-syntax (define-singleton stx)
  (syntax-case stx (:)
    [(_ (name) body ...)
     (syntax/loc stx
       (define name
         (cache-thunk (λ () body ...))))]
    [(_ (name arg ...) body ...)
     (syntax/loc stx
       (define name
         (cache-singleton (λ (arg ...) body ...))))]))

;; ===================================================================================================
;; OpenGL-context-sensitive, single-value memoization

(: cache-singleton/context (All (B A ...) (-> (-> A ... B)
                                              (-> A ... B))))
(define (cache-singleton/context f)
  (: cache (HashTable GL-Context (U #f (Pair (List A ...) B))))
  (define cache (make-weak-hasheq))
  
  (λ args
    (define ctxt (get-current-managed-gl-context 'cache-singleton/context))
    
    (: apply-f (-> (List A ...) B))
    (define (apply-f args)
      (define value (apply f args))
      (hash-set! cache ctxt (cons args value))
      value)
    
    (define entry (hash-ref! cache ctxt (λ () #f)))
    (cond [(not entry)  (apply-f args)]
          [else
           (define res (equal? args (car entry)))
           (cond [(eq? res #t)  (cdr entry)]
                 [else  (apply-f (if res res args))])])))

(: cache-thunk/context (All (B) (-> (-> B) (-> B))))
(define (cache-thunk/context f)
  (: cache (HashTable GL-Context (U #f B)))
  (define cache (make-weak-hasheq))
  (λ ()
    (define ctxt (get-current-managed-gl-context 'cache-thunk/context))
    (define entry (hash-ref! cache ctxt (λ () #f)))
    (cond [entry  entry]
          [else
           (define value (f))
           (hash-set! cache ctxt value)
           value])))

(define-syntax (define-singleton/context stx)
  (syntax-case stx (:)
    [(_ (name) body ...)
     (syntax/loc stx
       (define name
         (cache-thunk/context (λ () body ...))))]
    [(_ (name arg ...) body ...)
     (syntax/loc stx
       (define name
         (cache-singleton/context (λ (arg ...) body ...))))]))
