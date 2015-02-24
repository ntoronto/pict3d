#lang typed/racket/base

(require racket/set)

(provide (all-defined-out))

(define-type Tag (U Symbol Integer))

(: tag? (-> Any Boolean : Tag))
(define (tag? v)
  (or (symbol? v) (exact-integer? v)))

(define-type Tags (Setof Tag))

(define empty-tags ((inst set Tag)))

(: empty-tags? (-> Tags Boolean))
(define empty-tags? set-empty?)

(: singleton-tags (-> Tag Tags))
(define (singleton-tags n)
  (set n))

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
