#lang typed/racket/base

(require racket/list
         typed/racket/class
         math/flonum
         math/base
         mzlib/pconvert-prop
         "user-types.rkt"
         "../math/flt3.rkt"
         "../math/flv3.rkt"
         "../math/flrect3.rkt"
         "../engine/scene.rkt"
         (only-in "../engine/types.rkt" affine-transform)
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

(: pict3d-camera (-> Pict3D (U #f FlAffine3-)))
(define (pict3d-camera p)
  (define ts (scene-map-group/transform (Pict3D-scene p) 'camera (λ ([t : Affine] _) t)))
  (if (pair? ts) (affine-transform (first ts)) #f))

(: pict3d-auto-camera (-> Pict3D FlAffine3-))
(define (pict3d-auto-camera p)
  (let* ([s  (Pict3D-scene p)]
         [s  (scene-filter-shapes s (λ (a) (or (solid-shape? a) (frozen-scene-shape? a))))]
         [b  (scene-rect s)]
         [c  (if (empty-flrect3? b) (flvector 0.0 0.0 0.0) (flrect3-center b))]
         [d  (if (= 0.0 (flrect3-volume b))
                 1.0
                 (flv3mag (flv3- (flrect3-max b) c)))])
    (affine-transform
     (point-at #:from (flv3+ c (make-flvector 3 (/ (* d 1.25) (flsqrt 3.0)))) #:to c))))

(: pict3d-view-transform (-> Pict3D FlAffine3-))
(define (pict3d-view-transform p)
  (let* ([t  (pict3d-camera p)]
         [t  (if t t (pict3d-auto-camera p))])
    (flt3compose (scale-flt3 (flvector 1.0 -1.0 -1.0))
                 (flt3inverse t))))
