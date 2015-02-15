#lang typed/racket/base

(require racket/list
         math/flonum
         mzlib/pconvert-prop
         "../math/flt3.rkt"
         "../engine/scene.rkt"
         "../utils.rkt")

(provide current-pict3d-custom-write
         current-pict3d-print-converter
         (rename-out [-Pict3D Pict3D]
                     [Pict3D? pict3d?]
                     [Pict3D-scene  pict3d-scene])
         pict3d
         empty-pict3d
         pict3d-view-transform)

;; current-pict3d-custom-write is set in "pict3d-snip.rkt" to print a snip
;; Doing so makes Pict3D instances print nicely in Racket

(: default-pict3d-custom-write (-> Pict3D Output-Port (U #t #f 0 1) Any))
(define (default-pict3d-custom-write p port mode)
  (printf "#<pict3d>"))

(: current-pict3d-custom-write (Parameterof (-> Pict3D Output-Port (U #t #f 0 1) Any)))
(define current-pict3d-custom-write (make-parameter default-pict3d-custom-write))

;; current-pict3d-print-converter is set in "pict3d-snip.rkt" to a function that returns a snip
;; Doing so makes Pict3D instances print nicely in HTDP languages

(: default-pict3d-print-converter (-> Pict3D (-> Any Any) (U Pict3D (Instance (Class)))))
(define (default-pict3d-print-converter p recur) p)

(: current-pict3d-print-converter
   (Parameterof (-> Pict3D (-> Any Any) (U Pict3D (Instance (Class))))))
(define current-pict3d-print-converter (make-parameter default-pict3d-print-converter))

;; ===================================================================================================
;; Pict3D type

(struct Pict3D ([scene : Scene])
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (λ (p port mode) ((current-pict3d-custom-write) p port mode))
  #:property prop:print-converter (λ (p recur) ((current-pict3d-print-converter) p recur))
  )

(define-type -Pict3D Pict3D)
(define pict3d Pict3D)

(define empty-pict3d (Pict3D empty-scene))

(: pict3d-view-transform (All (F) (case-> (-> Pict3D FlAffine3-)
                                          (-> Pict3D (-> F) (U FlAffine3- F)))))
(define (pict3d-view-transform p [default (λ () (scale-flt3 (flvector 1.0 -1.0 -1.0)))])
  (define ts (scene-map-group/transform (Pict3D-scene p) 'camera (λ ([t : FlAffine3-] _) t)))
  (cond [(empty? ts)  (default)]
        [else  (define t (first ts))
               (flt3compose (scale-flt3 (flvector 1.0 -1.0 -1.0))
                            (flt3inverse t))]))
