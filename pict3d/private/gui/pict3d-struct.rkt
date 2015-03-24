#lang typed/racket/base

(require typed/racket/class
         mzlib/pconvert-prop
         "../engine.rkt")

(provide
 ;; Pict3D printing
 current-pict3d-custom-write
 current-pict3d-print-converter
 ;; Pict3D types
 (rename-out [-Pict3D Pict3D])
 empty-pict3d
 empty-pict3d?
 pict3d?
 pict3d
 pict3d-scene)

;; current-pict3d-custom-write is set in "main.rkt" to print a snip
;; Doing so makes Pict3D instances print nicely in Racket

(: default-pict3d-custom-write (-> Scene Output-Port (U #t #f 0 1) Any))
(define (default-pict3d-custom-write p port mode)
  (write-string "#<pict3d>" port))

(: current-pict3d-custom-write (Parameterof (-> Scene Output-Port (U #t #f 0 1) Any)))
(define current-pict3d-custom-write (make-parameter default-pict3d-custom-write))

;; current-pict3d-print-converter is set in "main.rkt" to a function that returns a snip
;; Doing so makes Pict3D instances print nicely in HTDP languages

(: default-pict3d-print-converter (-> Scene (-> Any Any) (Object)))
(define (default-pict3d-print-converter p recur) (make-object object%))

(: current-pict3d-print-converter (Parameterof (-> Scene (-> Any Any) (Object))))
(define current-pict3d-print-converter (make-parameter default-pict3d-print-converter))

;; ===================================================================================================
;; Pict3D types

(struct Pict3D ([scene : Scene])
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (λ (p port mode) ((current-pict3d-custom-write) (pict3d-scene p) port mode))
  #:property prop:print-converter
  (λ (p recur) ((current-pict3d-print-converter) (pict3d-scene p) recur)))

(define-type -Pict3D Pict3D)
(define pict3d? Pict3D?)
(define pict3d-scene Pict3D-scene)

(define empty-pict3d (Pict3D empty-scene))

(: empty-pict3d? (-> Pict3D Boolean))
(define (empty-pict3d? p) (empty-scene? (Pict3D-scene p)))

(: pict3d (-> Scene Pict3D))
(define (pict3d s)
  (if (empty-scene? s) empty-pict3d (Pict3D s)))
