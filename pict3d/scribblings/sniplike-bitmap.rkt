#lang racket/base

(require racket/list
         racket/match
         racket/draw
         racket/class
         "../private/lazy-gui.rkt"
         "../private/gui/indicators.rkt"
         "../private/engine.rkt")

(provide pict3d->sniplike-bitmap)

(define wireframe-material
  (material #:ambient 1.0 #:diffuse 0.0 #:specular 0.0 #:roughness 1.0))

;(: pict3d->sniplike-bitmap (-> Pict3D Integer Integer (Instance Bitmap%)))
(define (pict3d->sniplike-bitmap p width height)
  (define add-sunlight? (current-pict3d-add-sunlight?))
  (define add-indicators? (current-pict3d-add-indicators?))
  (define add-grid? (current-pict3d-add-grid?))
  (define add-wireframe (current-pict3d-add-wireframe))
  
  (define s (pict3d-scene p))
  (define scale 1.0)
  
  (define sunlight-pict3ds
    (if add-sunlight?
        (list standard-over-light
              standard-under-light)
        empty))
  
  (define light-pict3ds
    (if add-indicators?
        (scene-light-indicators s)
        empty))
  
  (define axes-pos+picts
    (if add-indicators?
        (cons (cons origin (scene-origin-indicator scale))
              (scene-basis-indicators s scale))
        empty))
  
  (define grid-pict3ds
    (if add-grid?
        (list (light-grid (emitted 1.0 0.6 0.6 2.0)
                          (emitted 0.5 1.5 0.5 2.0)
                          (emitted 0.7 0.7 1.0 2.0)
                          scale))
        empty))
  
  (define wireframe-pict3ds
    (case add-wireframe
      [(color)    (list (parameterize ([current-color  (rgba "black")]
                                       [current-emitted  (emitted "black" 0)]
                                       [current-material  wireframe-material])
                          (wireframe p)))]
      [(emitted)  (list (parameterize ([current-color  (rgba "white")]
                                       [current-emitted  (emitted 1 0 1 1)]
                                       [current-material  wireframe-material])
                          (wireframe p #:width 1.0)))]
      [else       empty]))
  
  (define view
    (let ([t  (camera-transform p)])
      (if t t ((current-pict3d-auto-camera) p))))
  
  (define v0 (affine-origin view))
  (define axes-pict3ds
    (for/fold (;[picts : (Listof Pict3D)  empty]
               [picts empty]
               )
              ([pos+picts  (in-list axes-pos+picts)])
      (match-define (cons v p) pos+picts)
      (if (< (pos-dist v v0) (* scale 0.015))
          picts
          (cons p picts))))
  
  (define picts
    (cons p (append sunlight-pict3ds light-pict3ds axes-pict3ds wireframe-pict3ds)))
  
  (define pict-bm
    (parameterize ([current-pict3d-auto-camera  (λ (_) view)])
      (pict3d->bitmap (combine picts) width height)))
  
  (define bm (make-bitmap (+ width 4) (+ height 4)))
  (define dc (make-object bitmap-dc% bm))
  (send dc draw-rectangle 0 0 (+ width 4) (+ height 4))
  (send dc draw-bitmap pict-bm 2 2)
  bm)
