#lang typed/racket/base

;; Homogenous points

(require (for-syntax racket/base)
         racket/match
         racket/list
         "../math/flv3.rkt"
         "../math/flrect3.rkt")

(provide FlPoints3 flpoints3? flpoints3-data flpoints3-vertices
         (rename-out [flpoints3* flpoints3])
         flpoints3-length
         flpoints3-rect)

(struct flpoints3 ([data : Any]
                   [vertices : (Vectorof FlVector)])
  #:transparent)

(define-type FlPoints3 flpoints3)

(: make-flpoints3 (-> Any (Vectorof FlVector) flpoints3))
(define (make-flpoints3 data vs)
  (define n (vector-length vs))
  (cond [(= n 0)  (raise-type-error 'flpoints3 "nonempty vector" 1 data vs)]
        [else  (flpoints3 data vs)]))

(define-match-expander flpoints3*
  (λ (stx)
    (syntax-case stx ()
      [(_ e1 e2)  (syntax/loc stx (flpoints3 e1 e2))]))
  (λ (stx)
    (syntax-case stx ()
      [(_ . args)  (syntax/loc stx (make-flpoints3 . args))]
      [_  (syntax/loc stx make-flpoints3)])))

(: flpoints3-length (-> flpoints3 Index))
(define (flpoints3-length p)
  (vector-length (flpoints3-vertices p)))

(: flpoints3-rect (-> flpoints3 FlRect3))
(define (flpoints3-rect p)
  (assert (flv3rect (flpoints3-vertices p)) values))
