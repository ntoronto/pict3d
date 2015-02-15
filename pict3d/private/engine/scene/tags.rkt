#lang typed/racket/base

(require racket/set)

(provide (all-defined-out))

(define-type Tag (U Symbol Integer))
(define-type Tags (Setof Tag))

(define empty-tags ((inst set Tag)))

(: empty-tags? (-> Tags Boolean))
(define (empty-tags? ns)
  (set-empty? ns))

(: singleton-tags (-> Tag Tags))
(define (singleton-tags n)
  (set n))

(: tags-destruct (-> Tags (Values Tag Tags)))
(define (tags-destruct ns)
  (let ([n  (set-first ns)])
    (values n (set-remove ns n))))

(: tags-union (-> Tags Tags Tags))
(define (tags-union ns1 ns2)
  (cond [(empty-tags? ns1)  ns2]
        [(empty-tags? ns2)  ns1]
        [else  (set-union ns1 ns2)]))

(: tags-contain? (-> Tags Tag Boolean))
(define (tags-contain? ns n)
  (set-member? ns n))

(: tags-add (-> Tags Tag Tags))
(define (tags-add ns n)
  (set-add ns n))

(: tags-remove (-> Tags Tag Tags))
(define (tags-remove ns n)
  (set-remove ns n))
