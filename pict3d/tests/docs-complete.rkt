#lang racket

(require rackunit/docs-complete)

(check-docs 'pict3d
            #:skip
            '(frustum-cull
              plane-cull
              rect-cull
              pict3d
              pict3d-scene
              current-pict3d-print-converter
              current-pict3d-custom-write
              canvas-projective
              bitmap-projective
              camera->view
              light-grid
              wireframe
              affine->cols
              cols->affine
              ;; To document:
              current-pict3d-add-grid?
              current-pict3d-add-wireframe
              current-tessellate-segments
              current-tessellate-max-edge
              current-tessellate-max-angle
              current-adaptive-segments
              current-adaptive-max-edge
              current-adaptive-max-angle
              current-adaptive-max-iters
              set-vertex-color
              set-vertex-emitted
              set-vertex-material
              set-vertex-normal
              set-vertex-pos
              Arc
              arc?
              arc
              arc-min
              arc-max
              zero-arc
              circle-arc
              Interval
              interval?
              interval
              interval-min
              interval-max
              zero-interval
              unit-interval
              tessellate
              deform
              adaptive-tessellate
              adaptive-deform
              pipe
              disk
              ring
              tessellate
              Differentiable
              differentiable?
              differentiable
              differentiate
              Smooth
              smooth?
              smooth-function
              smooth-jacobian
              smooth-approximate
              smooth-between
              smooth-compose
              smooth-consistent?
              displace
              twist
              bend
              extend
              deform-pos
              deform-dir
              deform-norm
              deform-affine
              local-deform
              merge-vertex-normals
              plane-vertex-normals
              dir-norm
              freeze-in-groups
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
