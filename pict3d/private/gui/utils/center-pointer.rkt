#lang racket/base

(require racket/class
         racket/math)

(provide (all-defined-out))

#;(: snip-center-pointer (-> (Instance Snip%) (Values (U #f Integer) (U #f Integer))))
(define (snip-center-pointer snip)
  (define admin (send snip get-admin))
  (define editor (and admin (send admin get-editor)))
  (define canvas (and editor (send editor get-active-canvas)))
  (cond [(and editor canvas)
         (define loc-x0 (box 0))
         (define loc-y0 (box 0))
         (define loc-x1 (box 0))
         (define loc-y1 (box 0))
         (send editor get-snip-location snip loc-x0 loc-y0 #f)
         (send editor get-snip-location snip loc-x1 loc-y1 #t)
         (define-values (x0 y0)
           (let-values ([(x y)  (send editor editor-location-to-dc-location
                                      (unbox loc-x0) (unbox loc-y0))])
             (values (exact-floor x) (exact-floor y))))
         (define-values (x1 y1)
           (let-values ([(x y)  (send editor editor-location-to-dc-location
                                      (unbox loc-x1) (unbox loc-y1))])
             (values (exact-ceiling x) (exact-ceiling y))))
         (cond [(and (>= x0 0) (<= x1 (send canvas get-width))
                     (>= y0 0) (<= y1 (send canvas get-height)))
                (define x (quotient (+ x0 x1) 2))
                (define y (quotient (+ y0 y1) 2))
                (send canvas warp-pointer x y)
                (values x y)]
               [else
                (values #f #f)])]
        [else
         (values #f #f)]))
