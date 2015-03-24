#lang typed/racket/base

(require typed/racket/private/gui-types
         typed/racket/class
         "../gl.rkt"
         "pict3d-struct.rkt")

(provide Pict3D-Canvas%)

(define-type Pict3D-Canvas%
  (Class #:implements Canvas%
         (init [parent  (Instance Area-Container<%>)]
               [style   (Listof
                         (U 'transparent
                            'border
                            'vscroll
                            'hscroll
                            'deleted
                            'control-border
                            'combo
                            'resize-corner
                            'no-focus))
                        #:optional]
               [label    (U #f String) #:optional]
               [enabled  Any #:optional]
               [vert-margin   Natural #:optional]
               [horiz-margin  Natural #:optional]
               [min-width   (U #f Natural) #:optional]
               [min-height  (U #f Natural) #:optional]
               [stretchable-width   Any #:optional]
               [stretchable-height  Any #:optional])
         (init-field [pict3d  Pict3D #:optional])
         [set-pict3d  (-> Pict3D Void)]
         [get-pict3d  (-> Pict3D)]
         [get-managed-gl-context  (-> GL-Context)]
         [set-async-updates? (-> Boolean Void)]
         ))
