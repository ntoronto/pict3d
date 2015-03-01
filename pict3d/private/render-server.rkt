#lang racket

(require math/flonum
         racket/fasl
         "lazy-gui.rkt"
         "gui/indicators.rkt"
         "gui/utils/scales.rkt"
         "engine/scene.rkt"
         "engine/scene/marshal-scene.rkt")

(define tag (gensym))

(define (scene->sniplike-bitmap s width height)
  (define add-sunlight? (current-pict3d-add-sunlight?))
  (define add-indicators? (current-pict3d-add-indicators?))
  
  (define camera-s
    (let ([t  (camera-transform (pict3d s))])
      (cond
        [(not t)
         (define t (auto-camera-transform (pict3d s)))
         (scene-union (make-group-scene tag s)
                      (make-trans-scene t (make-group-scene 'camera empty-scene)))]
        [else  s])))
  
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
    (cons camera-s (append sunlight-scenes light-scenes axes-scenes)))
  
  (pict3d->bitmap (pict3d (scene-union* scenes)) width height))

(define in (current-input-port))
(define out (current-output-port))
(define err (current-error-port))

(let loop ()
  (match (fasl->s-exp in)
    [(list as-snip?
           width
           height
           z-near
           z-far
           fov-degrees
           (app rgba background)
           (app emitted ambient)
           add-sunlight?
           add-indicators?
           (app unmarshal-scene scene))
     (define bm
       (parameterize ([current-pict3d-z-near  z-near]
                      [current-pict3d-z-far   z-far]
                      [current-pict3d-fov-degrees  fov-degrees]
                      [current-pict3d-background  background]
                      [current-pict3d-ambient  ambient]
                      [current-pict3d-add-sunlight?  add-sunlight?]
                      [current-pict3d-add-indicators?  add-indicators?])
         (if as-snip?
             (scene->sniplike-bitmap scene width height)
             (pict3d->bitmap (pict3d scene) width height))))
     (define bs (make-bytes (* width height 4)))
     (send bm get-argb-pixels 0 0 width height bs)
     (s-exp->fasl bs out)
     (flush-output out)
     (loop)]
    [_  (void)]))
