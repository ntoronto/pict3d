#lang racket

(require rackunit/docs-complete)

(check-docs 'pict3d
            #:skip
            '(frustum-cull
              plane-cull
              rect-cull
              pict3d
              pict3d-scene
              current-pict3d-add-grid?
              current-pict3d-print-converter
              current-pict3d-custom-write
              canvas-projection
              bitmap-projection
              camera->view
              camera-ray
              light-grid
              affine->cols
              cols->affine
              ))

(check-docs 'pict3d/universe
            #:skip
            '(big-bang3d-state/c
              gen:world-state
              world-state-big-bang3d
              world-state-draw
              world-state-frame
              world-state-key
              world-state-mouse
              world-state-release
              world-state-stop?
              world-state-valid?
              world-state/c
              world-state?))

#;
(check-docs 'pict3d/engine
            #:skip '())
