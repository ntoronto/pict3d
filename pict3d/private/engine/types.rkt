#lang typed/racket/base

(require "../gl.rkt")

(provide (all-defined-out))

(define-type (List-Hash A B) (Listof (Pair A B)))

(: list-hasheq-ref (All (A B) (->* [(List-Hash A B) A] [(-> B)] B)))
(define (list-hasheq-ref orig-h k [thnk #f])
  (let loop ([h orig-h])
    (cond [(null? h)  (if thnk (thnk) (error 'list-hasheq-ref "no key ~e in hash ~e" k orig-h))]
          [else  (define kv (car h))
                 (cond [(equal? (car kv) k)  (cdr kv)]
                       [else  (loop (cdr h))])])))

(struct program-spec ([program : gl-program]
                      [uniforms : (List-Hash String (U Symbol Uniform))])
  #:transparent)

(struct material ([ambient : Flonum]
                  [diffuse : Flonum]
                  [specular : Flonum]
                  [roughness : Flonum])
  #:transparent)
