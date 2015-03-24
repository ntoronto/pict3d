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
  
  (define s (pict3d-scene p))
  (define scale 1.0)
  
  (define sunlight-scenes
    (if add-sunlight?
        (list standard-over-light-scene
              standard-under-light-scene)
        empty))
  
  (define light-scenes
    (if add-indicators?
        (scene-light-indicators s)
        empty))
  
  (define axes-pos+scenes
    (if add-indicators?
        (cons (cons origin (scene-origin-indicator scale))
              (scene-basis-indicators s scale))
        empty))
  
  (define view
    (let ([t  (camera-transform p)])
      (if t t ((current-pict3d-auto-camera) p))))
  
  (define-values (_dx _dy _dz v0) (affine->cols view))
  (define axes-scenes
    (for/fold ([scenes empty]) ([pos+scene  (in-list axes-pos+scenes)])
      (match-define (cons v s) pos+scene)
      (if (< (pos-dist v v0) (* scale 0.015))
          scenes
          (cons s scenes))))
  
  (define scenes
    (cons s (append sunlight-scenes light-scenes axes-scenes)))
  
  (parameterize ([current-pict3d-auto-camera  (λ (_) view)])
    (pict3d->bitmap (pict3d (scene-union* scenes)) width height)))
