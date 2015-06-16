#lang typed/racket/base

(require racket/list)

(require/typed
 racket/base
 [call-with-semaphore  (All (A) (-> Semaphore (-> A) A))])

(provide
 ;; Bloom buffer
 current-engine-bloom-buffer-size
 current-engine-bloom-levels
 ;; Debug passes
 get-engine-debug-passes
 add-engine-debug-passes!
 current-engine-debug-pass
 ;; Debug shapes
 get-engine-debug-shapes
 add-engine-debug-shapes!
 current-engine-debug-shapes)

;; ===================================================================================================

(define add-debug-mutex (make-semaphore 1))

(define-syntax-rule (append-unique/sema! new-lst lst)
  (call-with-semaphore
   add-debug-mutex
   (λ ()
     (set! lst (append lst (remove* lst new-lst))))))

;; ===================================================================================================
;; Bloom buffer control

(: current-engine-bloom-buffer-size (Parameterof Positive-Integer))
(define current-engine-bloom-buffer-size (make-parameter 256))

(: current-engine-bloom-levels (Parameterof Positive-Integer))
(define current-engine-bloom-levels (make-parameter 4))

;; ===================================================================================================
;; Results of drawing and compositing passes

(: engine-debug-passes (Listof Symbol))
(define engine-debug-passes empty)

(: get-engine-debug-passes (-> (Listof Symbol)))
(define (get-engine-debug-passes)
  engine-debug-passes)

(: add-engine-debug-passes! (-> (Listof Symbol) Void))
(define (add-engine-debug-passes! ss)
  (append-unique/sema! ss engine-debug-passes))

(: current-engine-debug-pass (Parameterof (U #f Symbol)))
(define current-engine-debug-pass (make-parameter #f))

;; ===================================================================================================
;; Shape-specific debugging output

(: engine-debug-shapes (Listof Symbol))
(define engine-debug-shapes empty)

(: get-engine-debug-shapes (-> (Listof Symbol)))
(define (get-engine-debug-shapes)
  engine-debug-shapes)

(: add-engine-debug-shapes! (-> (Listof Symbol) Void))
(define (add-engine-debug-shapes! ss)
  (append-unique/sema! ss engine-debug-shapes))

(: current-engine-debug-shapes (Parameterof (Listof Symbol)))
(define current-engine-debug-shapes (make-parameter empty))
