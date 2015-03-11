#lang racket/base

(require racket/generic
         racket/contract
         "private/lazy-gui.rkt"
         "private/universe/big-bang.rkt")

(provide big-bang3d)

(provide (except-out (all-defined-out) world-state-big-bang3d)
         (contract-out
          [world-state-big-bang3d
           (->* [big-bang3d-state/c]
                [#:name string?
                 #:width exact-positive-integer?
                 #:height exact-positive-integer?
                 #:frame-delay (>/c 0)]
                big-bang3d-state/c)]))

(define-generics world-state
  (world-state-valid? world-state frame time)
  (world-state-stop? world-state frame time)
  (world-state-frame world-state frame time)
  (world-state-key world-state frame time code)
  (world-state-release world-state frame time code)
  (world-state-mouse world-state frame time x y code)
  (world-state-draw world-state frame time)
  #:fallbacks [(define (world-state-valid? s n t) #t)
               (define (world-state-stop? s n t) #f)
               (define (world-state-frame s n t) s)
               (define (world-state-key s n t k) s)
               (define (world-state-release s n t k) s)
               (define (world-state-mouse s n t x y e) s)
               (define (world-state-draw s n t) empty-pict3d)])

(define big-bang3d-state/c
  (world-state/c
   [world-state-valid?   (or/c #f (-> world-state? exact-nonnegative-integer? flonum? boolean?))]
   [world-state-stop?    (or/c #f (-> world-state? exact-nonnegative-integer? flonum? boolean?))]
   [world-state-frame    (or/c #f (-> world-state? exact-nonnegative-integer? flonum? world-state?))]
   [world-state-key      (or/c #f (-> world-state? exact-nonnegative-integer? flonum? string?
                                      world-state?))]
   [world-state-release  (or/c #f (-> world-state? exact-nonnegative-integer? flonum? string?
                                      world-state?))]
   [world-state-mouse    (or/c #f (-> world-state? exact-nonnegative-integer? flonum?
                                      exact-integer? exact-integer? string?
                                      world-state?))]
   [world-state-draw     (or/c #f (-> world-state? exact-nonnegative-integer? flonum? pict3d?))]))

(define (world-state-big-bang3d s
                                #:name [name "World3D"]
                                #:width [width 512]
                                #:height [height 512]
                                #:frame-delay [frame-delay #i1000/30])
  (big-bang3d
   s
   #:valid-state? world-state-valid?
   #:stop-state? world-state-stop?
   #:name name
   #:width width
   #:height height
   #:frame-delay frame-delay
   #:on-frame world-state-frame
   #:on-key world-state-key
   #:on-release world-state-release
   #:on-mouse world-state-mouse
   #:on-draw world-state-draw))
