#lang scribble/manual

@(require "utils.rkt")

@title[#:tag "pict3d"]{Pict3D: Functional 3D Scenes}

Pict3D is written in Typed Racket, but can be used in untyped Racket without significant performance loss.

@defmodule[pict3d]

Pict3D provides a purely functional interface to rendering hardware, and is intended to be a performant, modern 3D engine.
It's getting there.

Pict3D draws on @racketmodname[pict] for inspiration, though some aspects of working in three dimensions make direct functional analogues impossible or very difficult.
For example,
@itemlist[
 @item{In a 3D scene, solid colors alone are insufficient to indicate the shapes of objects.}
 @item{Unlike 2D scenes, 3D scenes must be @emph{projected} onto two dimensions for display.
       The projection isn't unique, so displaying a 3D scene requires additional information.}
 @item{In 3D, it's possible to create analogues of combiners for stacking pictures vertically and horizontally.
       Unfortunately, there would be nine for each axis, to line up corners, edges, and centers, for a total of 27.}
 ]

Pict3D's solution is analogous to pinholes in @racketmodname[2htdp/image], or a generalization of attachment points in computer animation.



@section[#:tag "quick"]{Quick Start}

@section[#:tag "types"]{Basic Data Types}

@;{
   Pict3D pict3d?
   Dir dir?
   Emitted emitted?
   Material material?
   Pos pos?
   RGBA rgba?
   Tag tag?
}

@section[#:tag "shape-attributes"]{Shape Attributes}

@;{
   default-color
   default-emitted
   default-material
   current-color
   current-emitted
   current-material
   with-color
   with-emitted
   with-material
   set-color
   set-emitted
   set-material

   material
   make-material
   emitted
   emitted->flvector
   emitted-components
   emitted?
   flvector->emitted
   flvector->rgba
   
   rgba
   rgba->flvector
   rgba-components

   }

@section[#:tag "shapes"]{Pict3D Constructors}

@;{
   empty-pict3d
   arrow
   basis
   cone
   cylinder
   light
   quad
   sunlight
   triangle
   up-arrow
   }

@defproc[(sphere [c Pos] [r Real]) Pict3D]{
Returns a sphere.
}

@defproc[(ellipsoid [mn Pos] [mx Pos]) Pict3D]{
}

@defproc[(rectangle [mn Pos] [mx Pos]) Pict3D]{
}

@section[#:tag "combiners"]{Pict3D Combiners}

@;{
   Pict3Ds
   combine
   freeze
   group
   map-group
   map-group/transform
   remove-group
   remove-in-group
   replace-group
   replace-in-group
   pin
   ungroup
   weld
   set-origin
   }

@section[#:tag "vector"]{Position and Direction Vectors}

@;{
   dir
   dir+
   dir-
   dir->flvector
   dir-components
   dir-cross
   dir-dist
   dir-dist^2
   dir-dot
   dir-negate
   dir-normalize
   dir-scale
   angles->dir
   dir->angles
   flvector->dir
   +x +y +z -x -y -z
   +x+y +y+z +x+z, etc.
   flvector->pos
   pos
   pos+
   pos-
   pos->flvector
   pos-between
   pos-coordinates
   pos-dist
   pos-dist^2
   origin
   }

@section[#:tag "affine"]{Affine Transformations}

@;{
   Affine
   affine?
   
   transform
   
   move
   move-x
   move-y
   move-z
   
   rotate
   rotate-x
   rotate-y
   rotate-z
   
   scale
   scale-x
   scale-y
   scale-z
   
   point-at
   
   identity-affine
   
   affine->cols
   cols->affine
   
   affine-compose
   affine-inverse
   affine-consistent?
   }

@section[#:tag "information"]{Pict3D Information}

@;{
   empty-pict3d?
   
   surface
   trace
   
   bounding-rectangle
   center
   camera-transform
   auto-camera-transform
   
   group?
   group-contents
   group-name
   }

@section[#:tag "pict3d-parameters"]{Pict3D Parameters}

@;{
   current-pict3d-add-indicators?
   current-pict3d-add-sunlight?
   current-pict3d-ambient
   current-pict3d-background
   current-pict3d-custom-write
   current-pict3d-fov-degrees
   current-pict3d-height
   current-pict3d-legacy?
   current-pict3d-print-converter
   current-pict3d-width
   current-pict3d-z-far
   current-pict3d-z-near
   default-pict3d-ambient
   default-pict3d-background
   default-pict3d-fov-degrees
   default-pict3d-height
   default-pict3d-width
   default-pict3d-z-far
   default-pict3d-z-near
   }

@section[#:tag "rendering"]{Pict3D Rendering}

@;{
   Pict3D-Canvas%
   pict3d->bitmap
   pict3d-canvas%
   }

@section[#:tag "universe"]{Pict3D Universe}

@defmodule[pict3d/universe]

@defproc[(big-bang3d [init-state S] ...) S]{
}

@;{
   Not going to document:
   
   pict3d
   pict3d-scene
   pict3d-view-transform
   plane-cull
   rect-cull
   frustum-cull
}

@examples[#:eval pict3d-eval
                 (define x 3)
                 (require math/flonum)
                 (current-pict3d-add-sunlight? #f)
                 (current-pict3d-width 512)
                 (current-pict3d-height 512)
                 (combine (rectangle (pos -4 -4 -1) (pos 4 4 0))
                          (for/list ([_  (in-range 15)])
                            (light (pos (- (* 8 (random)) 4)
                                        (- (* 8 (random)) 4)
                                        1)
                                   (emitted "lightgreen")))
                          (basis 'camera (point-at (pos 4 5 1.5) origin)))]

@(close-pict3d-eval)
