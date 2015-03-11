#lang racket/base

(require racket/list
         math/flonum
         "../private/lazy-gui.rkt"
         "../private/gui/indicators.rkt"
         "../private/gui/utils/scales.rkt"
         "../private/math/flt3.rkt"
         "../private/engine/scene.rkt"
         (only-in "../private/engine/types.rkt" affine))

(provide pict3d->sniplike-bitmap)

(define (pict3d->sniplike-bitmap p width height)
  (define add-sunlight? (current-pict3d-add-sunlight?))
  (define add-indicators? (current-pict3d-add-indicators?))
  
  (define s (pict3d-scene p))
  (define scale (fl (scale-index->scale (flrect3->scale-index (scene-visible-rect s)))))
  
  (define sunlight-scenes
    (if add-sunlight?
        (list standard-over-light-scene
              standard-under-light-scene)
        empty))
  
  (define light-scenes
    (if add-indicators?
        (scene-light-indicators s)
        empty))
  
  (define axes-scenes
    (if add-indicators?
        (cons (scene-origin-indicator scale)
              (scene-basis-indicators s scale))
        empty))
  
  (define scenes
    (cons s (append sunlight-scenes light-scenes axes-scenes)))
  
  (define auto-camera-transform ((current-pict3d-auto-camera) p))
  (parameterize ([current-pict3d-auto-camera  (λ (_) auto-camera-transform)])
    (pict3d->bitmap (pict3d (scene-union* scenes)) width height)))
