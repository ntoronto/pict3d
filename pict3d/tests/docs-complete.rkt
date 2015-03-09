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
              ))
