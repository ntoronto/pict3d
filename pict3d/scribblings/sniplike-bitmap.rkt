#lang racket/base

(require racket/list
         racket/match
         "../private/lazy-gui.rkt"
         "../private/gui/indicators.rkt"
         "../private/engine.rkt")

(provide pict3d->sniplike-bitmap)

;(: pict3d->sniplike-bitmap (-> Pict3D Integer Integer (Instance Bitmap%)))
(define (pict3d->sniplike-bitmap p width height)
  (define add-sunlight? (current-pict3d-add-sunlight?))
  (define add-indicators? (current-pict3d-add-indicators?))
  (define add-grid? (current-pict3d-add-grid?))
  
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
    (cons p (append sunlight-pict3ds light-pict3ds axes-pict3ds)))
  
  (parameterize ([current-pict3d-auto-camera  (λ (_) view)])
    (pict3d->bitmap (combine picts) width height)))
