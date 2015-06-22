#lang scribble/manual

@(require "utils.rkt"
          plot/pict
          racket/math)

@(define-syntax-rule (deftypedparam name id in-type out-type (options ...) pre-flow ...)
   (defproc* ([(name) out-type]
              [(name [id in-type]) Void options ...])
     pre-flow ...))

@title[#:tag "pict3d"]{Pict3D: Functional 3D Scenes}

@author{@(author+email "Neil Toronto" (author-email))}

Pict3D is written in Typed Racket, but can be used in untyped Racket without significant performance
loss.

@defmodule[pict3d]

Pict3D provides a purely functional interface to rendering hardware, and is intended to be a
performant, modern 3D engine.
It's getting there.

Pict3D draws on @racketmodname[pict] for inspiration, though some aspects of working in three
dimensions make direct analogues impossible or very difficult.
For example,
@itemlist[
 @item{In a 3D scene, solid colors alone are insufficient to indicate the shapes of objects.}
 @item{Unlike 2D scenes, 3D scenes must be @emph{projected} onto two dimensions for display.
       The projection isn't unique, so displaying a 3D scene requires additional information.}
 @item{In 3D, it's possible to create combiners that stack scenes vertically and horizontally.
       Unfortunately, there would be nine for each axis, to line up corners, edges, and centers,
       for a total of 27.}
 ]

Pict3D's solutions to these difficulties take more or less standard forms: lights, cameras, and affine
transformations.
But what is @emph{not} standard is the overall design:
@itemlist[
 @item{Pict3D makes no distinction between development and production/deployment.
       All functions for developing scenes are available during runtime, and vice-versa.}
 @item{The procedures used to create and update scenes are purely functional.}
]
Scenes are thus fully persistent, which offers many advantages.
For example, when running @racketmodname[pict3d/universe] programs, the current scene can be dumped
to DrRacket's REPL for inspection at any time by pressing @tt{F12}, without affecting the running
program.

@(table-of-contents)

@;{===================================================================================================
   ===================================================================================================
   ===================================================================================================
   }

@section[#:tag "quick"]{Quick Start}

This section is best understood by following along with the examples in DrRacket.

@(define-syntax-rule (define-png name path)
   (begin
     (define-runtime-path the-path path)
     (define name (read-bitmap the-path))))

@(define-png indicators-png "images/indicators.png")
@(define-png sunlight-off-png "images/sunlight-off.png")
@(define-png indicators-off-png "images/indicators-off.png")
@(define-png sunlight-off-indicators-off-png "images/sunlight-off-indicators-off.png")
@(define-png light-indicator-png "images/light-indicator.png")
@(define-png sunlight-icon-on-png "images/sunlight-icon-on.png")
@(define-png indicators-icon-on-png "images/indicators-icon-on.png")
@(define-png grid-icon-on-png "images/grid-icon-on.png")
@(define-png wireframe-icon-on-png "images/wireframe-icon-on.png")
@(define-png scale-png "images/scale.png")

@margin-note*{If you get OpenGL errors when using @racketmodname[pict3d], try adding
@racket[(current-pict3d-legacy? #t)] to the top of your programs.}
To make a @racket[Pict3D] instance containing a single shape, first load the @racketmodname[pict3d]
module, then apply a shape constructor:
@interaction[#:eval pict3d-eval
                    (eval:alts (require pict3d) (void))
                    (sphere origin 1/2)]
This sphere's center position is @racket[(pos 0 0 0)], or the @racket[origin], and it has radius
@racket[1/2].
The red, green and blue bars are @emph{not} part of the scene.
They're just extra arrows drawn from the origin along the @italic{x}, @italic{y} and @italic{z} axes.

When the mouse hovers over a displayed @racket[Pict3D] instance, it looks something like this:

@centered{@indicators-png}

If you're following along in DrRacket, click on it.
@margin-note*{You can also click and drag to look.
              ``Mouse look'' doesn't work on Mac OS X because of reasons.}
Move the mouse to look around, and use these keys to navigate:
@itemlist[
 @item{@tt{W} and @tt{S} (or @tt{↑} and @tt{↓}): forwards and backwards}
 @item{@tt{A} and @tt{D} (or @tt{←} and @tt{→}): left and right}
 @item{@tt{R} and @tt{F} (or @tt{PgUp} and @tt{PgDn}): up and down}
]
If you're familiar with first-person 3D games, this should feel fairly natural.
Click the mouse button again to return control of the keyboard and mouse to DrRacket.

Flying around demonstrates why the axis arrows are necessary.
The sphere looks similar from every direction, so it's easy to get lost.
(If you do get lost, press @tt{Esc}.)

The numbers in the lower right are information about the position and orientation of the closest
surface point in space under the mouse cursor.
Right-clicking allows you to copy the information to the clipboard.

The @scale-png in the upper right shows the scale the @racket[Pict3D] is rendered at.
Clicking on the @tt{-} and @tt{+} parts decrease and increase the scale.

@margin-note*{The @grid-icon-on-png icon toggles a global grid.
              The @wireframe-icon-on-png icon toggles triangle wireframes, which are good for showing
              @tech{tessellations}.}
Clicking the @sunlight-icon-on-png icon toggles the displayed @racket[Pict3D]'s default lighting,
which is also @emph{not} part of the scene.
Clicking the @indicators-icon-on-png icon toggles the origin axes and other indicators.
With default lighting off and indicators off, the display looks respectively like

@centered{@sunlight-off-png @hspace[4] @indicators-off-png}

With both off, the displayed @racket[Pict3D] looks like this:

@centered{@sunlight-off-indicators-off-png}

The above is the only @emph{faithful} display, identical to what you get by converting the sphere
into a bitmap:
@interaction[#:eval pict3d-eval
                    (pict3d->bitmap (sphere origin 1/2))]
Our @racket[Pict3D] clearly needs better lighting to communicate its shape.
We'll combine it with a point light source on the @italic{x = 0} plane:
@interaction[#:eval pict3d-eval
                    (pict3d->bitmap
                     (combine (sphere origin 1/2)
                              (light (pos 0 1 1))))]
If you evaluate @racket[(combine (sphere origin 1/2) (light (pos 0 1 1)))] (i.e. without converting to
a bitmap) and fly around the scene, you'll find the point light represented by a glowing octahedron:

@centered{@light-indicator-png}

If you then click @indicators-icon-on-png, the octahedron will disappear, because it's not part of the
scene, either.
The light itself is actually invisible.

In general, a @racket[Pict3D] is comprised of the following kinds of objects.
@itemlist[
 @item{Shapes: visible 2D surfaces in 3D space. Surfaces are visible on only one side.}
 @item{Lights: invisible color emitters.}
 @item{Groups: invisible, named, oriented collections of other objects.}
]

Only one group is special: the one named @racket['camera].
When a @racket[Pict3D] is renderered, if a group named @racket['camera] exists, that group's
orientation is used as the viewpoint.
To look down on our sphere, we can combine it with a downward-oriented camera group:
@interaction[#:eval pict3d-eval
                    (combine (sphere origin 1/2)
                             (basis 'camera (point-at (pos 0 0 2) origin)))]
The @racket[basis] function creates an empty group with a given orientation, while
@racket[(point-at (pos 0 0 2) origin)] creates an orientation looking at the @racket[origin] from the
point @racket[(pos 0 0 2)].

If a @racket['camera] group isn't in the scene, an orientation is chosen automatically by
@racket[(current-pict3d-auto-camera)].

@;{===================================================================================================
   ===================================================================================================
   ===================================================================================================
   }

@section[#:tag "constructors"]{Constructors}

The examples in this section are easiest to compare when using an auto camera that doesn't depend on
the @racket[Pict3D] instance.
@interaction[#:eval pict3d-eval
                    (current-pict3d-auto-camera (λ (_) (point-at (pos 1 1 1) origin)))]

@deftogether[(@defidform[#:kind "type" Pict3D]
              @defthing[#:kind "predicate" pict3d? (-> Any Boolean : Pict3D)])]{
The type and predicate for 3D scenes.
}

@deftogether[(@defidform[#:kind "type" Pict3Ds]
              @defproc[(combine [p Pict3Ds] ...) Pict3D])]{
A @racket[Pict3Ds] instance is either a @racket[Pict3D] or a list of @racket[Pict3D]s; i.e. a tree.

The @racket[combine] function returns a new @racket[Pict3D] that contains all of @racket[Pict3D]
instances in all of its arguments @racket[p ...].
Usually, the order they're given in doesn't affect the result.
When it does, don't rely on any particular order.
}

@deftogether[(@defthing[empty-pict3d Pict3D]
              @defproc[(empty-pict3d? [p Pict3D]) Boolean])]{
A @racket[Pict3D] containing nothing, and a function to detect empty @racket[Pict3D]s.
}

@defproc*[([(rectangle [corner1 Pos] [corner2 Pos] [#:inside? inside? Any #f]) Pict3D]
           [(rectangle [center Pos] [scale (U Dir Real)] [#:inside? inside? Any #f]) Pict3D]
           [(cube [center Pos] [scale Real] [#:inside? inside? Any #f]) Pict3D])]{
The first form returns a @racket[Pict3D] containing a single rectangle with corners @racket[corner1]
and @racket[corner2].
The corners may be any pair that are the opposite endpoints of a main diagonal.
@examples[#:eval pict3d-eval
                 (rectangle origin (pos 1/2 1/2 1/2))]
If the second argument is a @tech{direction vector} or a scale, the first argument is regarded as a
center point.
If @racket[scale] is a direction, the components are interpreted as axis-aligned half-widths.
@examples[#:eval pict3d-eval
                 (rectangle origin (dir 1/4 1/2 3/4))]
When @racket[scale] is a real number, @racket[(rectangle center scale)] is equivalent to
@racket[(rectangle center (dir scale scale scale))], and @racket[(cube center scale)] is equivalent to
@racket[(rectangle center scale)].

When @racket[inside?] is non-@racket[#f], the rectangle's surfaces face inward.
@examples[#:eval pict3d-eval
                 (rectangle origin (pos 1.25 1.25 1.25) #:inside? #t)
                 (deform
                   (tessellate
                    (combine (with-color (rgba "firebrick")
                               (combine 
                                (rectangle (pos 0 0 5/8) (dir 1/2 1/2 1/8))
                                (rectangle (pos 0 0 -5/8) (dir 1/2 1/2 1/8))))
                             (rectangle origin (dir 1/2 1/2 4/8))))
                   (twist 120))]
}

@defproc*[([(ellipsoid [corner1 Pos] [corner2 Pos] [#:inside? inside? Any #f]) Pict3D]
           [(ellipsoid [center Pos] [scale (U Dir Real)] [#:inside? inside? Any #f]) Pict3D]
           [(sphere [center Pos] [radius Real] [#:inside? inside? Any #f]) Pict3D])]{
Returns a @racket[Pict3D] containing the largest ellipsoid that fits inside
@racket[(rectangle corner1 corner2)] or @racket[(rectangle center scale)].
@examples[#:eval pict3d-eval
                 (combine (ellipsoid origin (pos 1/2 1/2 1/2))
                          (with-color (rgba "red" 0.5)
                            (rectangle origin (pos 1/2 1/2 1/2))))
                 (combine (ellipsoid origin (dir 1/4 1/2 3/4))
                          (with-color (rgba "red" 0.5)
                            (rectangle origin (dir 1/4 1/2 3/4))))]
As with @racket[cube] and @racket[rectangle], @racket[(sphere center radius)]
is equivalent to @racket[(ellipsoid center radius)].

@examples[#:eval pict3d-eval
                 (adaptive-deform
                  (with-color (rgba "turquoise")
                    (ellipsoid (pos 0 -1/2 0) (dir 1/4 1/4 1)))
                  (twist 360))]

When @racket[inside?] is non-@racket[#f], the ellipsoid surface faces inward.
(See @racket[rectangle].)
}

@defproc*[([(cylinder [corner1 Pos]
                      [corner2 Pos]
                      [#:inside? inside? Any #f]
                      [#:arc arc Arc circle-arc]
                      [#:top-cap? top-cap? Any #t]
                      [#:bottom-cap? bottom-cap? Any #t]
                      [#:start-cap? start-cap? Any #t]
                      [#:end-cap? end-cap? Any #t]
                      [#:outer-wall? outer-wall? Any #t])
            Pict3D]
           [(cylinder [center Pos]
                      [scale (U Dir Real)]
                      [#:<cylinder-keyword> <cylinder-keyword> <cylinder-keyword-type>]
                      ...)
            Pict3D])]{
Returns a @racket[Pict3D] containing the largest vertical cylinder that fits inside
@racket[(rectangle corner1 corner2)] or @racket[(rectangle center scale)].
@examples[#:eval pict3d-eval
                 (combine (cylinder origin (pos 1/2 1/2 1/2))
                          (with-color (rgba "red" 0.5)
                            (rectangle origin (pos 1/2 1/2 1/2))))
                 (combine (cylinder origin (dir 1/4 1/2 3/4))
                          (with-color (rgba "red" 0.5)
                            (rectangle origin (dir 1/4 1/2 3/4))))]
When @racket[inside?] is non-@racket[#f], the cylinder's surfaces face inward.
(See @racket[rectangle].)
The remaining boolean arguments determine which parts of the cylinder's surface are created.

The @racket[arc] argument determines the start and end angle swept out by the vertical cap
to create the cylinder.
@examples[#:eval pict3d-eval
                 (cylinder origin 1/2 #:arc (arc 90 360))
                 (move (bend (combine
                              (tessellate
                               (with-color (rgba "lightsteelblue")
                                 (cylinder (pos 0 0 1) (dir 1/4 1/4 1) #:arc (arc -90 90)))
                               #:max-edge 1/12)
                              (with-color (rgba "slategray")
                                (combine
                                 (rectangle (pos 1/8 0 -1/8) (dir 3/16 5/16 1/8))
                                 (rectangle (pos 1/8 0 (+ 2 1/8)) (dir 3/16 5/16 1/8)))))
                             -180 (interval 0 2))
                       (dir (/ 2 pi) 0 1/4))]
}

@defproc*[([(cone [corner1 Pos]
                  [corner2 Pos]
                  [#:inside? inside? Any #f]
                  [#:arc arc Arc circle-arc]
                  [#:bottom-cap? bottom-cap? Any #t]
                  [#:start-cap? start-cap? Any #t]
                  [#:end-cap? end-cap? Any #t]
                  [#:outer-wall? outer-wall? Any #t])
            Pict3D]
           [(cone [center Pos]
                  [scale (U Dir Real)]
                  [#:<cone-keyword> <cone-keyword> <cone-keyword-type>]
                  ...)
            Pict3D])]{
Returns a @racket[Pict3D] containing the largest upward-pointing cone that fits inside
@racket[(rectangle corner1 corner2)] or @racket[(rectangle center scale)].
@examples[#:eval pict3d-eval
                 (combine (cone origin (pos 1/2 1/2 1/2))
                          (with-color (rgba "red" 0.5)
                            (rectangle origin (pos 1/2 1/2 1/2))))
                 (combine (cone origin (dir 1/4 1/2 3/4))
                          (with-color (rgba "red" 0.5)
                            (rectangle origin (dir 1/4 1/2 3/4))))]
When @racket[inside?] is non-@racket[#f], the cone's surfaces face inward.
(See @racket[rectangle].)
The remaining boolean arguments determine which parts of the cone's surface are created.

The @racket[arc] argument determines the start and end angle swept out by the triangular cap
to create the cone.
@examples[#:eval pict3d-eval
                 (cone origin 1/2 #:arc (arc 90 360))]
}

@defproc*[([(pipe [corner1 Pos]
                  [corner2 Pos]
                  [#:inside? inside? Any #f]
                  [#:arc arc Arc circle-arc]
                  [#:bottom-radii bottom-radii Interval (interval 1/2 1)]
                  [#:top-radii top-radii Interval bottom-radii]
                  [#:top-cap? top-cap? Any #t]
                  [#:bottom-cap? bottom-cap? Any #t]
                  [#:start-cap? start-cap? Any #t]
                  [#:end-cap? end-cap? Any #t]
                  [#:inner-wall? inner-wall? Any #t]
                  [#:outer-wall? outer-wall? Any #t])
            Pict3D]
           [(pipe [center Pos]
                  [scale (U Dir Real)]
                  [#:<pipe-keyword> <pipe-keyword> <pipe-keyword-type>]
                  ...)
            Pict3D])]{
Returns a @racket[Pict3D] containing the largest vertical, hollow cylinder that fits inside
@racket[(rectangle corner1 corner2)] or @racket[(rectangle center scale)].
@examples[#:eval pict3d-eval
                 (combine (pipe origin (pos 1/2 1/2 1/2))
                          (with-color (rgba "red" 0.5)
                            (rectangle origin (pos 1/2 1/2 1/2))))
                 (combine (pipe origin (dir 1/4 1/2 3/4))
                          (with-color (rgba "red" 0.5)
                            (rectangle origin (dir 1/4 1/2 3/4))))]
When @racket[inside?] is non-@racket[#f], the pipe's surfaces face inward.
(See @racket[rectangle].)
The remaining boolean arguments determine which parts of the pipe's surface are created.

The @racket[arc] argument determines the start and end angle swept out by a trapezoid to create the
pipe.
@examples[#:eval pict3d-eval
                 (pipe origin 1/2 #:arc (arc 90 360))]

The @racket[bottom-radii] and @racket[top-radii] intervals give the @emph{fractional} radius
of the inner and outer wall, on the top and bottom of the pipe.
@examples[#:eval pict3d-eval
                 (pipe origin 3/4
                       #:arc (arc 90 0)
                       #:top-radii (interval 1/2 5/8)
                       #:bottom-radii (interval 1/4 1))]
The fractional radii may exceed @racket[1].
@examples[#:eval pict3d-eval
                 (let* ([a  (arc 135 45)]
                        [p  (with-color (rgba "crimson")
                              (pipe origin (dir 1/2 1/2 1/16)
                                    #:arc a #:bottom-radii (interval 7/8 9/8)))])
                   (deform
                     (tessellate
                      (combine (move-z p -11/16)
                               (move-z p 11/16)
                               (with-color (rgba "lavender")
                                 (pipe origin (dir 1/2 1/2 5/8)
                                       #:arc a #:bottom-radii (interval 7/8 1)))))
                     (bend 45 (interval -1/2 1/2))))]
}

@defproc*[([(ring [corner1 Pos]
                  [corner2 Pos]
                  [#:back? back? Any #f]
                  [#:arc arc Arc circle-arc]
                  [#:radii radii Interval unit-interval])
            Pict3D]
           [(ring [center Pos]
                  [scale (U Dir Real)]
                  [#:<ring-keyword> <ring-keyword> <ring-keyword-type>]
                  ...)
            Pict3D])]{
Returns a @racket[Pict3D] containing a disk or a ring, placed vertically in the center of the
rectangle defined by @racket[corner1] and @racket[corner2], or by @racket[center] and @racket[scale].
@examples[#:eval pict3d-eval
                 (combine (ring origin (pos 1/2 1/2 1/2))
                          (with-color (rgba "red" 0.5)
                            (rectangle origin (pos 1/2 1/2 1/2))))
                 (combine (ring origin (dir 1/4 1/2 3/4))
                          (with-color (rgba "red" 0.5)
                            (rectangle origin (dir 1/4 1/2 3/4))))]

When @racket[back?] is non-@racket[#f], the ring surface faces downward.

The @racket[arc] argument determine the start and end angle swept out by a line to create the ring.

The @racket[radii] interval argument gives the @emph{fractional} radii of the inner and outer circle.

@examples[#:eval pict3d-eval
                 (deform
                   (tessellate
                    (combine
                     (ring origin 2 #:arc (arc 90 360) #:radii (interval 3/4 1))
                     (with-color (rgba 1 1/4 1/4)
                       (ring origin 2 #:arc (arc 90 360) #:radii (interval 2/4 3/4)))
                     (with-color (rgba 1/4 1/2 1)
                       (ring origin 2 #:arc (arc 90 360) #:radii (interval 1/4 2/4))))
                    #:max-edge 1/8
                    #:max-angle 5)
                   (displace (λ (x y) (* 1/8 (+ (sin (* 4 (atan y x)))
                                                (sin (* pi (+ (sqr x) (sqr y)))))))))]
}

@defproc[(triangle [corner1 (U Pos Vertex)]
                   [corner2 (U Pos Vertex)]
                   [corner3 (U Pos Vertex)]
                   [#:back? back? Any #f])
         Pict3D]{
Returns a @racket[Pict3D] containing a triangle with the given corners.
By default, the triangle is visible only from viewpoints for which its corners appear to be in
counterclockwise order.
When @racket[back?] is @racket[#t], it's visible only from viewpoints for which its corners appear to
be in clockwise order.

A corner may be either a @tech{position vector} or a @tech{vertex}.
When a corner is a @racket[Pos] instance, its normal is that of the plane the triangle lies in, and
its reflected color, emitted color, and material are the values of @racket[current-color],
@racket[current-emitted] and @racket[current-material].
A @racket[Vertex] instance can override all of these attributes.
All attributes are interpolated across the face of the triangle.
@examples[#:eval pict3d-eval
                 (triangle (pos 3/4 0 0) (pos 0 3/4 0) (pos 0 0 3/4))
                 (triangle
                  (vertex (pos 3/4 0 0) #:normal +x)
                  (vertex (pos 0 3/4 0) #:normal +y)
                  (vertex (pos 0 0 3/4) #:normal +z))
                 (triangle
                  (vertex (pos 3/4 0 0) #:color (rgba "red"))
                  (vertex (pos 0 3/4 0) #:emitted (emitted "lightgreen" 2.0))
                  (vertex (pos 0 0 3/4) #:material (material #:specular 1.0
                                                             #:roughness 0.1)))]
}

@defproc[(quad [corner1 (U Pos Vertex)]
               [corner2 (U Pos Vertex)]
               [corner3 (U Pos Vertex)]
               [corner4 (U Pos Vertex)]
               [#:back? back? Any #f])
         Pict3D]{
Returns a @racket[Pict3D] containing a quadrilateral with the given corners.
The rule for its visibility, and the interpretation of positions and vertices, are the same as
for @racket[triangle].

A quad's corners are not required to lie in a plane, so its default normal is a best-fit direction
vector computed using @hyperlink["http://dl.acm.org/citation.cfm?id=130783"]{Newell's method}.
}

@(define light-plot
   (parameterize ([plot-x-ticks  no-ticks]
                  [plot-y-ticks  no-ticks])
     (plot
      (list
       (x-ticks (list (tick 0 #t "0")
                      (tick 4.5 #t "r/2")
                      (tick 9 #t "r")))
       (y-ticks (list (tick 0 #t "0")
                      (tick 4/81 #t "4/r²")))
       (function (λ (d) (/ (sqr d)))
                 #:color 4 #:width 3 #:style 'long-dash
                 #:label "1/d² (ideal)")
       (function (λ (d) (* (/ 16 (expt 9 4))
                           (sqr (- 9 d))))
                 #:color 3 #:width 3 #:style 'short-dash
                 #:label "16/r⁴·(r-d)²")
       (function (λ (d)
                   (if (<= d (/ 9 2))
                       (/ (sqr d))
                       (* (/ 16 (expt 9 4))
                          (sqr (max 0 (- 9 d))))))
                 #:color 'white
                 #:width 1.5)
       (function (λ (d)
                   (if (<= d (/ 9 2))
                       (/ (sqr d))
                       (* (/ 16 (expt 9 4))
                          (sqr (max 0 (- 9 d))))))
                 #:color 'red
                 #:width 1.5
                 #:style 'dot
                 #:label "Pict3D")
       (lines (list (list 4.5 -4/81) (list 4.5 4/81) (list +nan.0 +nan.0)
                    (list 0 4/81) (list 4.5 4/81) (list +nan.0 +nan.0)
                    (list 9 -4/81) (list 9 0) (list +nan.0 +nan.0)
                    (list 0 0) (list 9 0) (list +nan.0 +nan.0))
              #:color 0 #:width 0.5 #:style 'long-dash)
       (points (list (list 4.5 4/81)
                     (list 9 0))
               #:size 7
               #:sym 'fullcircle
               #:fill-color 'white
               #:line-width 1
               #:color 0))
      #:x-min 0
      #:x-max 12
      #:y-min -1/81
      #:y-max 12/81
      #:x-label "distance (d)"
      #:y-label "attenuation"
      #:title "Attenuation at distance from point light"
      #:legend-anchor 'top-right)))

@defproc[(light [position Pos]
                [color Emitted (emitted "white")]
                [#:range r Real ((current-light-range) color)]
                [#:radii radii Interval unit-interval])
         Pict3D]{
Returns a @racket[Pict3D] containing a point light source at @racket[position], with emitted color
@racket[color].
@examples[#:eval pict3d-eval
                 (pict3d->bitmap
                  (combine (sphere origin 1)
                           (light (pos 0 1.5 1.5) (emitted "oldlace" 5))))]

A naive rendering algorithm would take time proportional to @italic{n·m}, where @italic{n} is the
number of objects in a scene and @italic{m} is the number of lights.
Pict3D uses @hyperlink["https://en.wikipedia.org/wiki/Deferred_shading"]{deferred lighting}
algorithms that take time proportional to @italic{n+m}.

Deferred lighting algorithms ``draw'' each light on every fragment of the rendered scene that
shows a surface the light might illuminate.
An ideal point light illuminates surfaces at any distance, so each ideal point light would have to be
drawn on the entire rendered scene.
@margin-note*{Even with these optimizations, lights tend to take more time to render than solid
              objects. Try to use only a few thousand well-spaced lights.}
Adding ideal point lights quickly becomes prohibitively expensive, so Pict3D restricts each
light's effect to a light-specific range @racket[r].

Instead of forcing a light's effect to zero at distance @racket[r], Pict3D forces smooth attenuation
to zero over a finite range. Forced attenuation starts at distance @racket[(/ r 2)] and follows a
quadratic curve. The quadratic curve's first derivative matches the ideal's curve's first derivative
at @racket[(/ r 2)], and is zero at @racket[r] (so the overall attenuation is
@hyperlink["https://en.wikipedia.org/wiki/Smoothness"]{C¹-continuous}),
which reduces visual artifacts.

@centered{@light-plot}

With the default value of @racket[r], forced attenuation starts when ideal attenuation is
4/40 = 0.1, and ends when ideal attenuation would be 1/40 = 0.025.
(See @racket[current-light-range].)
Thus, a single light's effect with the default @racket[r] tends to be hard to distinguish from an
ideal light's effect.
Much smaller values of @racket[r] can result in major discrepancies.
@examples[#:eval pict3d-eval
                 (pict3d->bitmap
                  (combine (rectangle (pos 0 0 -1) (dir 10 10 1))
                           (light (pos 0 0 1) (emitted "orange" 1))))
                 
                 (pict3d->bitmap
                  (combine (rectangle (pos 0 0 -1) (dir 10 10 1))
                           (light (pos 0 0 1) (emitted "orange" 1) #:range +inf.0)))
                 
                 (pict3d->bitmap
                  (combine (rectangle (pos 0 0 -1) (dir 10 10 1))
                           (light (pos 0 0 1) (emitted "orange" 1) #:range 1.5)))]
In fact, with @racket[#:range 1] in the above example, the light has no apparent effect at all.

Small ranges are useful when a light is used for a local visual effect (such as a magical shine
on a blade) or when many lights in one area slow down rendering too much.

Large ranges are useful when many weak lights illuminate a surface.
Try to choose the smallest large range that gives an effect similar to that of
@racket[#:range +inf.0].
@examples[#:eval pict3d-eval
                 (pict3d->bitmap
                  (combine (rectangle (pos 0 0 -1) (dir 5 5 1))
                           (for*/list ([x  (in-range -5 5.5 0.5)]
                                       [y  (in-range -5 5.5 0.5)])
                             (light (pos x y 1/2) (emitted "orange" 1/100)))))
                 
                 (pict3d->bitmap
                  (combine (rectangle (pos 0 0 -1) (dir 5 5 1))
                           (for*/list ([x  (in-range -5 5.5 0.5)]
                                       [y  (in-range -5 5.5 0.5)])
                             (light (pos x y 1/2) (emitted "orange" 1/100)
                                    #:range (* (sqrt 1/100) 25)))))]

The @racket[radii] argument restricts the light's area of effect to minimum and maximum
@emph{fractions} of @racket[r].
@examples[#:eval pict3d-eval
                 (pict3d->bitmap 
                  (combine (rectangle (pos 0 0 -1) 1)
                           (light (pos 0 0 1) (emitted "orange" 10)
                                  #:range 30
                                  #:radii (interval 0.044 0.045))))]
Here, the light affects surfaces at distances only in the range [30·0.044,30·0.45] = [1.32,1.35].

Transforming a light may change its initially spherical area of effect to an ellipsoid.
This allows lights inside of solid objects to approximate light emission from the objects' surfaces.
@examples[#:eval pict3d-eval
                 (pict3d->bitmap
                  (combine (rectangle (pos 0 0 -1) (dir 2 2 1))
                           (transform
                            (combine (with-emitted (emitted "plum" 2)
                                       (cylinder origin 1))
                                     (light origin (emitted "plum" 5)))
                            (affine-compose
                             (rotate-z 30)
                             (move-z 1/4)
                             (rotate-x 90)
                             (scale (dir 1/8 1/8 1))))))]
Using many smaller, unscaled lights gives a better approximation, and works well for deformed objects.
@examples[#:eval pict3d-eval
                 (pict3d->bitmap
                  (combine (rectangle (pos 0 0 -1) (dir 2 2 1))
                           (deform
                             (combine (with-emitted (emitted "plum" 2)
                                        (tessellate
                                         (scale (cylinder origin 1) (dir 1/8 1/8 1))))
                                      (for/list ([z  (in-range 0 17)])
                                        (light (pos 0 0 (- (* 2 (/ z 16)) 1))
                                               (emitted "plum" 1/150))))
                             (smooth-compose
                              (rotate-z 30)
                              (move-z 1/4)
                              (rotate-x 90)
                              (bend 135 (interval -1 1))))))]
}

@deftogether[(@deftypedparam[current-light-range range-fun (-> Emitted Real) (-> Emitted Real)
                             (#:value default-light-range)]
              @defthing[default-light-range (-> Emitted Real)
                        #:value (λ (color) (sqrt (/ (emitted-intensity color) 1/40)))])]{
The default value producer for @racket[light]'s @racket[#:range] argument, and its default value.
These are provided to make it easy to change every light's range at once.

The default value is arrived at by solving the ideal attenuation equation @italic{l = i/r}²
for @italic{r}, where @italic{i} is @racket[(emitted-intensity color)] and @italic{l} = 1/40.
That is, it finds the range at which an ideal light's attenuation is 1/40.
}

@defproc[(sunlight [direction Dir] [color Emitted (emitted "white")]) Pict3D]{
Returns a @racket[Pict3D] containing an omnipresent, directional light source.
@examples[#:eval pict3d-eval
                 (pict3d->bitmap
                  (combine (sphere origin 1)
                           (sunlight (dir -1 2 0) (emitted "azure" 5))))]
}

@defproc*[([(arrow [start Pos] [end Pos] [#:normalize? normalize? Any #f]) Pict3D]
           [(arrow [start Pos] [direction Dir] [#:normalize? normalize? Any #f]) Pict3D])]{
Returns a @racket[Pict3D] containing an arrow drawn from @racket[start] to @racket[end], or from
@racket[start] in the direction @racket[direction].
When @racket[normalize?] is non-@racket[#f], the arrow has length @racket[1].

See @secref["vector" #:doc '(lib "pict3d/scribblings/pict3d.scrbl")] for examples.
}

@interaction-eval[#:eval pict3d-eval (current-pict3d-auto-camera default-pict3d-auto-camera)]

@;{===================================================================================================
   ===================================================================================================
   ===================================================================================================
   }

@section[#:tag "attributes"]{Shape Attributes}

Every occupied point in 3D space has associated @deftech{attributes}.
The attributes that Pict3D manages, which influence rendering, are reflected color, emitted color,
and material.

@deftogether[(@deftypedparam[current-color c RGBA RGBA (#:value default-color)]
              @deftypedparam[current-emitted e Emitted Emitted (#:value default-emitted)]
              @deftypedparam[current-material m Material Material
                                              (#:value default-material)])]{
New @racket[Pict3D] instances set their shapes' attributes using the values of these parameters.
@examples[#:eval pict3d-eval
                 (parameterize ([current-color  (rgba "chocolate")])
                   (sphere origin 1/2))]
}

@deftogether[(@defform[(with-color c body ...) #:contracts ([rgba RGBA])]
              @defform[(with-emitted e body ...) #:contracts ([emitted Emitted])]
              @defform[(with-material m body ...) #:contracts ([material Material])])]{
Equivalent to @racket[(parameterize ([current-color c]) body ...)], etc.
}

@deftogether[(@defthing[default-color RGBA #:value (rgba "white")]
              @defthing[default-emitted Emitted #:value (emitted "black" 0)]
              @defthing[default-material Material #:value (material #:ambient 0.05
                                                                    #:diffuse 0.6
                                                                    #:specular 0.35
                                                                    #:roughness 0.3)])]{
Default values of @racket[current-color], @racket[current-emitted] and @racket[current-material].
}

@deftogether[(@defproc[(set-color [p Pict3D] [c RGBA]) Pict3D]
              @defproc[(set-emitted [p Pict3D] [e Emitted]) Pict3D]
              @defproc[(set-material [p Pict3D] [m Material]) Pict3D])]{
Return new @racket[Pict3D] instances with the attributes of every shape changed.
@examples[#:eval pict3d-eval
                 (set-emitted
                  (parameterize ([current-color  (rgba "chocolate")])
                    (sphere origin 1/2))
                  (emitted "lightgreen" 2))]
In large scenes, especially in scenes created using @racket[freeze], these operations can be
time-consuming.
Prefer using @racket[with-color], @racket[with-emitted] and @racket[with-material].
If you must use these functions, use them with @racket[replace-group] and @racket[replace-in-group].
}

@subsection[#:tag "rgba"]{Reflected Color Attributes}

@deftogether[(@defidform[#:kind "type" RGBA]
              @defthing[#:kind "predicate" rgba? (-> Any Boolean : RGBA)])]{
The type and predicate of reflected colors with optional transparency.
}

@defproc*[([(rgba [color (U RGBA
                            Real
                            (Listof Real) (Vectorof Real) FlVector
                            String (Instance Color%))]
                  [alpha Real 1])
            RGBA]
           [(rgba [red Real] [green Real] [blue Real] [alpha Real 1]) RGBA])]{
Constructs an @racket[RGBA] value from its components, or converts a color in another representation
to an @racket[RGBA].
If @racket[color] already has an alpha component, it is multiplied by @racket[alpha] in the result.
@examples[#:eval normal-eval
                 (rgba "white")
                 (rgba "white" 0.5)
                 (rgba (rgba "white") 0.5)
                 (rgba 1/2)
                 (rgba 0.2 0.3 0.4)
                 (rgba '(1/2 1/4 1/8))
                 (rgba #(1/2 1/4 1/8))
                 (rgba (flvector 0.5 0.25 0.125))]
All component values are converted to flonums and clamped to the range [0,1].
}

@deftogether[(@defproc[(rgba-red [rgba RGBA]) Flonum]
              @defproc[(rgba-green [rgba RGBA]) Flonum]
              @defproc[(rgba-blue [rgba RGBA]) Flonum]
              @defproc[(rgba-alpha [rgba RGBA]) Flonum])]{
Return the components of @racket[rgba].

You'll usually want to use @racket[match] or @racket[match-define] instead:
@interaction[#:eval normal-eval
                    (match-define (rgba r g b a) (rgba "lavender" 0.75))
                    (list r g b a)]
}

@subsection[#:tag "emitted"]{Emitted Color Attributes}

@deftogether[(@defidform[#:kind "type" Emitted]
              @defthing[#:kind "predicate" emitted? (-> Any Boolean : Emitted)])]{
The type and predicate of emitted colors, which include red, green, blue and intensity components.
}

@defproc*[([(emitted [color (U Emitted
                               Real
                               (Listof Real) (Vectorof Real) FlVector
                               String (Instance Color%))]
                     [intensity Real 1])
            Emitted]
           [(emitted [red Real] [green Real] [blue Real] [intensity Real 1]) Emitted])]{
Constructs an @racket[Emitted] value from its components, or converts a color in another
representation to an @racket[Emitted].
If @racket[color] already has a fourth component, it is multiplied by @racket[intensity] in the
result.

All component values are converted to flonums, set to @racket[0.0] if negative, and normalized so
that the largest of @racket[red], @racket[green] and @racket[blue] is @racket[1.0].
@examples[#:eval normal-eval
                 (emitted "blue")
                 (emitted "darkblue")]
}

@deftogether[(@defproc[(emitted-red [emitted Emitted]) Flonum]
              @defproc[(emitted-green [emitted Emitted]) Flonum]
              @defproc[(emitted-blue [emitted Emitted]) Flonum]
              @defproc[(emitted-intensity [emitted Emitted]) Flonum])]{
Return the components of @racket[emitted].

You'll usually want to use @racket[match] or @racket[match-define] instead:
@interaction[#:eval normal-eval
                    (match-define (emitted r g b i) (emitted 1 2 3 1))
                    (list r g b i)]
}

@subsection[#:tag "material"]{Material Attributes}

@deftogether[(@defidform[#:kind "type" Material]
              @defthing[#:kind "predicate" material? (-> Any Boolean : Material)])]{
The type and predicate of materials, which include ambient, specular, diffuse, and roughness
components.
}

@defproc[(material [#:ambient ambient Real 0]
                   [#:diffuse diffuse Real 0]
                   [#:specular specular Real 0]
                   [#:roughness roughness Real 0.1])
         Material]{
Contructs a material from its components.
For realistic-looking materials, @racket[ambient], @racket[diffuse] and @racket[specular] should sum
to @racket[1] (but this is not enforced).

@examples[#:eval pict3d-eval
                 (combine
                  (with-material (material #:ambient  1.0) (sphere (pos 1 0 0) 1/2))
                  (with-material (material #:diffuse  1.0) (sphere (pos 0 1 0) 1/2))
                  (with-material (material #:specular 1.0) (sphere (pos 0 0 1) 1/2)))]
The @italic{x}-axis sphere has only ambient reflectance. It reflects an imaginary white
light in all directions with the same intensity.

The @italic{y}-axis sphere has only diffuse reflectance. It looks dull; not shiny at all.

The @italic{z}-axis sphere has only specular reflectance. It looks @emph{only} shiny.

The @racket[roughness] component affects only specular reflectance.
Generally, the less rough the sphere, the shinier it looks.
@examples[#:eval pict3d-eval
                 (combine
                  (with-material (material #:specular 1.0 #:roughness 0.1)
                    (sphere (pos 1 0 0) 1/2))
                  (with-material (material #:specular 1.0 #:roughness 0.2)
                    (sphere (pos 0 1 0) 1/2))
                  (with-material (material #:specular 1.0 #:roughness 0.4)
                    (sphere (pos 0 0 1) 1/2)))]
}

@deftogether[(@defproc[(material-ambient [material Material]) Flonum]
              @defproc[(material-diffuse [material Material]) Flonum]
              @defproc[(material-specular [material Material]) Flonum]
              @defproc[(material-roughness [material Material]) Flonum])]{
Return the components of @racket[material].
}

@subsection[#:tag "vertex"]{Vertex Attributes}

The functions @racket[triangle] and @racket[quad] accept not just position arguments, but
@deftech{vertex} arguments that contain positions and can override any current shape attributes.

@examples[#:eval pict3d-eval
                 (define (make-a-vertex dv)
                   (match-define (dir r g b) dv)
                   (vertex (pos+ origin dv 0.5)
                           #:normal (if (<= (- r g) 0) dv #f)
                           #:color (rgba (* 0.5 (+ 1 r))
                                         (* 0.5 (+ 1 g))
                                         (* 0.5 (+ 1 b)))
                           #:emitted (if (> b 0.9) (emitted 1 2) #f)))
                 (combine
                  (for*/list ([ρ  (in-range -90 91 10)]
                              [θ  (in-range -180 180 10)])
                    (define last-ρ (- ρ 10))
                    (define last-θ (- θ 10))
                    (define dvs (list (angles->dir θ ρ)
                                      (angles->dir last-θ ρ)
                                      (angles->dir last-θ last-ρ)
                                      (angles->dir θ last-ρ)))
                    (apply quad (map make-a-vertex dvs))))]

@deftogether[(@defidform[#:kind "type" Vertex]
              @defthing[#:kind "predicate" vertex? (-> Any Boolean : Vertex)])]{
The type and predicate of @tech{vertex} data.
}

@defproc[(vertex [pos Pos]
                 [#:normal normal (U #f Dir) #f]
                 [#:color color (U #f RGBA) #f]
                 [#:emitted emitted (U #f Emitted) #f]
                 [#:material material (U #f Material) #f])
         Vertex]{
Constructs data for one vertex.
When @racket[normal] is omitted or @racket[#f], the normal is computed using vertex positions.
(See @racket[triangle] and @racket[quad].)
For any other attribute, omission or @racket[#f] indicates that the attribute should have the value of
@racket[current-color], @racket[current-emitted] or @racket[current-material].
}

@deftogether[(@defproc[(vertex-pos [vertex Vertex]) Pos]
              @defproc[(vertex-normal [vertex Vertex]) (U #f Dir)]
              @defproc[(vertex-color [vertex Vertex]) (U #f RGBA)]
              @defproc[(vertex-emitted [vertex Vertex]) (U #f Emitted)]
              @defproc[(vertex-material [vertex Vertex]) (U #f Material)])]{
Return the attributes of @racket[vertex].
}

@subsection[#:tag "interval"]{Interval Arguments}

@deftogether[(@defidform[#:kind "type" Interval]
              @defthing[#:kind "predicate" interval? (-> Any Boolean : Interval)])]{
The type and predicate of closed intervals.
}

@defproc[(interval [min Real] [max Real]) Interval]{
Constructs an @racket[Interval] value from its endpoints.
Endpoints may be given in either order and are converted to flonums.
@examples[#:eval normal-eval
                 (interval 0 -1)
                 (interval -inf.0 +inf.0)]
}

@deftogether[(@defthing[zero-interval Interval #:value (interval 0 0)]
              @defthing[unit-interval Interval #:value (interval 0 1)])]{
Common intervals.
}

@deftogether[(@defproc[(interval-min [i Interval]) Flonum]
              @defproc[(interval-max [i Interval]) Flonum])]{
Return the endpoints of @racket[i].

You'll usually want to use @racket[match] or @racket[match-define] instead:
@interaction[#:eval normal-eval
                    (match-define (interval mn mx) (interval 1 -1))
                    (list mn mx)]
}

@subsection[#:tag "arc"]{Arc Arguments}

@deftogether[(@defidform[#:kind "type" Arc]
              @defthing[#:kind "predicate" arc? (-> Any Boolean : Arc)])]{
The type and predicate of closed arcs.
}

@defproc[(arc [start Real] [end Real]) Arc]{
Constructs an @racket[Arc] value from its start and end angles.
Angles are converted to flonums and normalized so that @racket[start] is in [0,360) and @racket[end]
is no less than @racket[start].
@examples[#:eval normal-eval
                 (arc 0 0)
                 (arc 0 360)
                 (arc -90 90)
                 (arc 90 -90)
                 (arc 0 450)]
}

@deftogether[(@defthing[zero-arc Arc #:value (arc 0 0)]
              @defthing[circle-arc Arc #:value (arc 0 360)])]{
Common arcs.
}

@deftogether[(@defproc[(arc-start [a Arc]) Flonum]
              @defproc[(arc-end [a Arc]) Flonum])]{
Return the start and end angles of @racket[a].

You'll usually want to use @racket[match] or @racket[match-define] instead:
@interaction[#:eval normal-eval
                    (match-define (arc start end) (arc 360 0))
                    (list start end)]
}

@;{===================================================================================================
   ===================================================================================================
   ===================================================================================================
   }

@section[#:tag "vector"]{Position and Direction Vectors}

A vector in 3D space is uniquely identified by three real numbers called @deftech{coordinates} or
@deftech{components}.
For example, the coordinates of @racket[(pos 1 2 3)] are @racket[1], @racket[2] and @racket[3].

Pict3D distinguishes between two kinds of vectors:
@itemlist[
 @item{A @deftech{position vector}, which is a value of type @racket[Pos], represents an absolute
       position in 3D space.}
 @item{A @deftech{direction vector}, which is a value of type @racket[Dir], represents a direction,
       with distance, in 3D space.}
]
Generally, think of direction vectors as how to get from one position vector in space to another.

@interaction-eval[#:eval pict3d-eval
                         (current-pict3d-auto-camera
                          (λ (_) (point-at (pos 0.4 -0.5 1.15) (pos 0.4 1.0 0))))]

Suppose we define a direction vector @racket[dv1] as
@interaction[#:eval pict3d-eval
                    (define dv1 (dir 1 1 0))]
To picture it, we need a starting position vector.
We'll arbitrarily choose the @racket[origin], which is @racket[(pos 0 0 0)].
@interaction[#:eval pict3d-eval
                    (define v origin)
                    (arrow v dv1)]
We can use the direction vector @racket[dv1] to move the center of a sphere.
@interaction[#:eval pict3d-eval
                    (define move-once
                      (combine (arrow v dv1)
                               (sphere v 0.1)
                               (sphere (pos+ v dv1) 0.1)))
                    (eval:alts move-once ((λ () move-once)))]
We might even define another direction vector @racket[dv2] (pointing upward) to move it again.
@interaction[#:eval pict3d-eval
                    (define dv2 (dir 0 0 1))
                    (define move-twice
                      (combine move-once
                               (arrow (pos+ v dv1) dv2)
                               (sphere (pos+ (pos+ v dv1) dv2) 0.1)))
                    (eval:alts move-twice ((λ () move-twice)))]
But we don't have to move it twice: we can move it just once, by adding the direction vectors
@racket[dv1] and @racket[dv2].
@interaction[#:eval pict3d-eval
                    (define dv (dir+ dv1 dv2))
                    (define move-twice-by-moving-once
                      (combine (arrow v dv)
                               (sphere v 0.1)
                               (sphere (pos+ v dv) 0.1)))
                    (combine move-twice-by-moving-once
                             (set-color move-twice (rgba "red" 0.75)))]
Composing transformations---not just movement, but all parallel-line-preserving transformations---is
discussed in the section @secref{affine}.

Directions are used to move entire scenes, not just points:
@interaction[#:eval pict3d-eval
                    (move (combine (with-color (rgba "red" 0.5)
                                     (cube origin 1/3))
                                   (sphere origin 0.05))
                          (dir 1/2 1/2 0))]
When used as scaling factors, as in @racket[(rectangle origin (dir 1/3 1/3 1))], directions represent
moving the center to a corner:
@interaction[#:eval pict3d-eval
                    (move (combine (with-color (rgba "red" 0.5)
                                     (rectangle origin (dir 1/3 1/3 1)))
                                   (sphere origin 0.05)
                                   (arrow origin (dir 1/3 1/3 1)))
                          (dir 1/2 1/2 0))]

@interaction-eval[#:eval pict3d-eval (current-pict3d-auto-camera default-pict3d-auto-camera)]

@subsection[#:tag "direction-vectors"]{Direction Vectors}

@deftogether[(@defidform[#:kind "type" Dir]
              @defthing[#:kind "predicate" dir? (-> Any Boolean : Dir)])]{
The type and predicate of @tech{direction vectors}.
}

@defproc*[([(dir [v (U FlVector (Listof Real) (Vectorof Real))]) Dir]
           [(dir [dx Real] [dy Real] [dz Real]) Dir])]{
Constructs a @tech{direction vector} from its components @racket[dx], @racket[dy] and @racket[dz], or
converts a vector @racket[v] in another representation to a direction vector.
}

@defthing[zero-dir Dir #:value (dir 0 0 0)]{
The zero direction. Analogous to @racket[origin].
}

@deftogether[(@defthing[+x Dir #:value (dir  1  0  0)]
              @defthing[-x Dir #:value (dir -1  0  0)]
              @defthing[+y Dir #:value (dir  0  1  0)]
              @defthing[-y Dir #:value (dir  0 -1  0)]
              @defthing[+z Dir #:value (dir  0  0  1)]
              @defthing[-z Dir #:value (dir  0  0 -1)]
              )]{
The positive and negative coordinate axis directions.
@examples[#:eval pict3d-eval
                 (with-color (rgba "black")
                   (combine (with-emitted (emitted "cyan" 2)
                              (arrow origin -x))
                            (with-emitted (emitted "magenta" 2)
                              (arrow origin -y))
                            (with-emitted (emitted "yellow" 2)
                              (arrow origin -z))))]
Alternatively, these are the face normals for an axis-aligned rectangle, or the directions from the
center of a @racket[(cube v 1)] to the centers of each of its faces.
}

@deftogether[(@defthing[+x+y Dir #:value (dir  1  1  0)]
              @defthing[+x-y Dir #:value (dir  1 -1  0)]
              @defthing[+y+z Dir #:value (dir  0  1  1)]
              @defthing[+y-z Dir #:value (dir  0  1 -1)]
              @defthing[+x+z Dir #:value (dir  1  0  1)]
              @defthing[+x-z Dir #:value (dir  1  0 -1)]
              @defthing[-x+y Dir #:value (dir -1  1  0)]
              @defthing[-x-y Dir #:value (dir -1 -1  0)]
              @defthing[-y+z Dir #:value (dir  0 -1  1)]
              @defthing[-y-z Dir #:value (dir  0 -1 -1)]
              @defthing[-x+z Dir #:value (dir -1  0  1)]
              @defthing[-x-z Dir #:value (dir -1  0 -1)]
              )]{
The directions from the center of a @racket[(cube v 1)] to the centers of each of its edges.
@interaction[#:eval pict3d-eval
                    (combine
                     (cube origin 1 #:inside? #t)
                     (with-color (rgba "cyan")
                       (for/list ([dv  (list +x+y +x-y +y+z +y-z +x+z +x-z
                                             -x+y -x-y -y+z -y-z -x+z -x-z)])
                         (arrow origin dv)))
                     (basis 'camera (point-at (pos 2 0.5 0.75) origin)))]
}

@deftogether[(@defthing[+x+y+z Dir #:value (dir  1  1  1)]
              @defthing[+x+y-z Dir #:value (dir  1  1 -1)]
              @defthing[+x-y+z Dir #:value (dir  1 -1  1)]
              @defthing[+x-y-z Dir #:value (dir  1 -1 -1)]
              @defthing[-x+y+z Dir #:value (dir -1  1  1)]
              @defthing[-x+y-z Dir #:value (dir -1  1 -1)]
              @defthing[-x-y+z Dir #:value (dir -1 -1  1)]
              @defthing[-x-y-z Dir #:value (dir -1 -1 -1)])]{
The directions from the center of a @racket[(cube v 1)] to each of its corners.
@interaction[#:eval pict3d-eval
                    (combine
                     (cube origin 1 #:inside? #t)
                     (with-color (rgba "magenta")
                       (for/list ([dv  (list +x+y+z +x+y-z +x-y+z +x-y-z
                                             -x+y+z -x+y-z -x-y+z -x-y-z)])
                         (arrow origin dv)))
                     (basis 'camera (point-at (pos 2 0.5 0.75) origin)))]
}

@deftogether[(@defproc[(dir+ [dv1 Dir] [dv2 Dir]) Dir]
              @defproc[(dir-scale [dv Dir] [s Real]) Dir]
              @defproc[(dir-negate [dv Dir]) Dir]
              @defproc[(dir- [dv1 Dir] [dv2 Dir]) Dir])]{
@racket[(dir+ dv1 dv2)] composes movement in the directions @racket[dv1] and @racket[dv2] by adding
their components.
 
@racket[(dir-scale dv s)] scales the direction @racket[dv] by multiplying each component by
@racket[s].

@racket[(dir-negate dv)] returns the direction opposite @racket[dv] by negating each component, and is
equivalent to @racket[(dir-scale dv -1)].

@racket[(dir- dv1 dv2)] is equivalent to @racket[(dir+ dv1 (dir-negate dv2))].
}

@deftogether[(@defproc[(dir-dist [dv Dir]) Flonum]
              @defproc[(dir-dist^2 [dv Dir]) Flonum])]{
Return the distance and squared distance represented by @racket[dv].
}

@defproc[(dir-norm [dv Dir] [p Nonnegative-Real]) Flonum]{
Return the distance represented by @racket[dv] under the L-@racket[p]
@hyperlink["https://en.wikipedia.org/wiki/Norm_%28mathematics%29#p-norm"]{norm}.
}

@defproc[(dir-normalize [dv Dir]) (U #f Dir)]{
Returns a new @tech{direction vector} in the same direction as @racket[dv], but distance @racket[1].
If @racket[dv] is @racket[(dir 0 0 0)], @racket[(dir-normalize dv)] returns @racket[#f].
}

@defproc[(dir-dot [dv1 Dir] [dv2 Dir]) Flonum]{
Returns the @hyperlink["http://en.wikipedia.org/wiki/Dot_product"]{dot product} of @racket[dv1] and
@racket[dv2].
}

@defproc[(dir-cross [dv1 Dir] [dv2 Dir]) Dir]{
Returns the @hyperlink["http://en.wikipedia.org/wiki/Cross_product"]{cross product} of @racket[dv1]
and @racket[dv2].
}

@defproc[(dir-project [dv1 Dir] [dv2 Dir]) (U #f Dir)]{
Returns the @hyperlink["http://en.wikipedia.org/wiki/Vector_projection"]{projection} of @racket[dv1]
onto @racket[dv2], or @racket[#f] if @racket[dv2] is @racket[(dir 0 0 0)].
}

@defproc[(dir-reject [dv1 Dir] [dv2 Dir] [s Real 1]) Dir]{
Returns the @hyperlink["http://en.wikipedia.org/wiki/Vector_projection"]{rejection} of @racket[dv1]
from @racket[dv2].
Equivalent to
@racketblock[(dir- dv1 (dir-scale (dir-project dv1 dv2) s))]
when @racket[(dir-project dv1 dv2)] doesn't return @racket[#f].
}

@defproc[(dir-reflect [dv1 Dir] [dv2 Dir]) Dir]{
Reflects @racket[dv1] in the direction @racket[dv2].
Equivalent to @racket[(dir-reject dv1 dv2 2)].

If @racket[dv] is the velocity of an object that hits an immovable surface with normal @racket[n],
then @racket[(dir-reject dv n)] is the object's new velocity, assuming an ideal
@hyperlink["http://en.wikipedia.org/wiki/Elastic_collision"]{elastic collision}.

In other words, use this function to bounce things off walls in games.
}

@defproc[(angles->dir [yaw Real] [pitch Real]) Dir]{
Converts @racket[yaw] (aka longitude, or rotation about the @italic{z} axis) and @racket[pitch]
(aka latitude, or rotation about the @italic{x} axis) into a direction with distance @racket[1].
Both angles are measured in degrees.
Yaw is applied first.
}

@defproc[(dir->angles [dv Dir]) (Values Flonum Flonum)]{
Converts a direction @racket[dv] into yaw (aka longitude) and pitch (aka latitude).
This is the inverse of @racket[angles->dir].
}

@deftogether[(@defproc[(dir-dx [dv Dir]) Flonum]
              @defproc[(dir-dy [dv Dir]) Flonum]
              @defproc[(dir-dz [dv Dir]) Flonum])]{
Return the components of @racket[dv].

You'll usually want to use @racket[match] or @racket[match-define] instead:
@interaction[#:eval normal-eval
                    (match-define (dir dx dy dz) (dir 1 2 3))
                    (list dx dy dz)]
}

@subsection[#:tag "position-vectors"]{Position Vectors}

@deftogether[(@defidform[#:kind "type" Pos]
              @defthing[#:kind "predicate" pos? (-> Any Boolean : Pos)])]{
The type and predicate of @tech{position vectors}.
}

@defproc*[([(pos [v (U FlVector (Listof Real) (Vectorof Real))]) Pos]
           [(pos [x Real] [y Real] [z Real]) Pos])]{
Constructs a @tech{position vector} from its coordinates @racket[x], @racket[y] and @racket[z], or
converts a vector @racket[v] in another representation to a position vector.
}

@defthing[origin Pos #:value (pos 0 0 0)]{
The origin. If 3D space has a center, this is it. Analogous to @racket[zero-dir].
}

@defproc[(pos+ [v Pos] [dv Dir] [s Real 1.0]) Pos]{
Moves @racket[v] in direction @racket[dv] optionally scaled by @racket[s].
@racket[(pos+ v dv s)] is equivalent to @racket[(pos+ v (dir-scale dv s))], but more accurate
when one of the resulting components is near @racket[0.0].
}

@defproc[(pos- [end Pos] [start Pos]) Dir]{
Returns the direction to move in to get from @racket[start] to @racket[end]; i.e. the @racket[dv] for
which @racket[(pos+ start dv)] is @racket[end].
}

@defproc[(pos-between [start Pos] [end Pos] [t Real]) Pos]{
Interpolates between @racket[start] and @racket[end] by fraction @racket[t].
The fraction @racket[t] should be between @racket[0] and @racket[1], but does not have to be.
@examples[#:eval pict3d-eval
                 (combine
                  (with-color (rgba "red")
                    (sphere (pos 1 0 0) 1/10))
                  (with-color (rgba "blue")
                    (sphere (pos 0 1 1) 1/10))
                  (for/list ([t  (in-range 1/10 1 1/10)])
                    (define v (pos-between (pos 1 0 0) (pos 0 1 1) t))
                    (sphere v 1/20)))]
}

@deftogether[(@defproc[(pos-dist [v1 Pos] [v2 Pos]) Flonum]
              @defproc[(pos-dist^2 [v1 Pos] [v2 Pos]) Flonum])]{
Return the distance and squared distance between @racket[v1] and @racket[v2].
}

@deftogether[(@defproc[(pos-x [v Pos]) Flonum]
              @defproc[(pos-y [v Pos]) Flonum]
              @defproc[(pos-z [v Pos]) Flonum])]{
Return the coordinates of @racket[v].

You'll usually want to use @racket[match] or @racket[match-define] instead:
@interaction[#:eval normal-eval
                    (match-define (pos x y z) (pos 1 2 3))
                    (list x y z)]
}

@;{===================================================================================================
   ===================================================================================================
   ===================================================================================================
   }

@section[#:tag "combining-scenes"]{Combining Scenes}

In 3D space, the terms @emph{horizontal} and @emph{vertical} are ambiguous.

When there's no unique or preferred way to view an object, even the terms @emph{top}, @emph{bottom},
@emph{left}, @emph{right}, @emph{front} and @emph{back} are ambiguous.

When an object is squashed or rotated, or is irregularly shaped, these terms are ambiguous even with
a preferred orientation.
For example, if we say that the following ellipsoid is leaning to the right, there are at least two
acceptable ways to interpret the term @emph{on the left side}:
@interaction[#:eval pict3d-eval
                    (define p1 (rotate-y (ellipsoid origin (dir 1/4 1/4 1)) -30))
                    (define camera (basis 'camera (point-at (pos 1 1 4) (dir -1 -1 0))))
                    (combine (move-z p1 4)
                             (with-emitted (emitted "green")
                               (arrow (pos 0.75 0 4.25) (dir -0.45 0 -0.25)))
                             (with-emitted (emitted "red")
                               (arrow (pos 1.05 0 3.25) (dir -0.5 0 0)))
                             camera)]
Now suppose we decide that the green arrow points at the ``left side,'' and that we want to attach the
face of a cube to it.
There are infinitely many ways to do so: one for each point on the face and each angle of rotation.

In general, combining two 3D scenes relationally requires specifying a lot of data.

Pict3D tries to reduce the burden by
@itemlist[
 @item{Allowing @racket[Pict3D] instances to be combined by naming groups in each instance to attach
       together.}
 @item{Making it easy to add such groups at specific points with specific orientations.}
]

For example, the following code defines a @deftech{basis}, which is an empty group, on the left side
of the ellipsoid @racket[p1], pointing away from the surface.
@interaction[#:eval pict3d-eval
                    (define-values (v dv) (surface/normal p1 +x))
                    (define left (basis 'left (point-at v dv)))]
Here, the @racket[surface/normal] function finds a point on the surface of @racket[p1] in the
direction @racket[+x].
It returns that point and a @deftech{surface normal}, which is the direction perpendicular to the
surface at that point.
The @racket[point-at] function returns an @tech{affine transformation} that represents pointing in the
direction @racket[dv] from position @racket[v].
Finally, @racket[basis] creates a @racket[Pict3D] out of the result.

We could display @racket[left], but it's more instructive to combine it with the ellipsoid whose
surface it's on.
@interaction[#:eval pict3d-eval
                    (define p1/left (move-z (combine p1 left) 4))
                    (combine p1/left camera)]
(We've moved it upward @racket[4] units only to keep the origin's axes from obscuring the new basis.)
Notice that the blue arrow points outward.

Now we'll create a cube to attach onto that basis.
@interaction[#:eval pict3d-eval
                    (define p2 (with-color (rgba "deepskyblue" 0.75)
                                 (cube origin 1/4)))
                    (define top (basis 'top (point-at (surface p2 +z) -z)))
                    (define p2/top (move-z (combine p2 top) 4))
                    (eval:alts p2/top ((λ () p2/top)))]
(Again, we've moved it upward, away from the origin's axes.)
This time, we used @racket[surface] instead of @racket[surface/normal] because we know the surface
normal: @racket[+z].
But we haven't applied @racket[point-at] to @racket[+z], we've used @racket[-z] instead, causing the
blue arrow to point in the direction opposite the surface normal: downward, into the cube.

All we have left is to @racket[pin] the scenes together:
@interaction[#:eval pict3d-eval
                    (define p3 (pin p1/left '(left)
                                    p2/top '(top)))
                    (combine p3 camera)]
@margin-note*{Because @racket[(pin pict1 path1 pict2 path2)] puts @racket[pict2] @emph{inside} of the
              group @racket[path1], it naturally creates group hierarchies. When this isn't what you
              want, use @racket[join] instead.}
This has applied the transformation necessary to make the basis named @racket['top] in @racket[p2/top]
match the basis named @racket['left] in @racket[p1/left], and combined the transformed @racket[p2/top]
with @racket[p1/left].
In other words, it's rotated and moved @racket[p2/top] so that @racket[top]'s red arrow is the same as
@racket[left]'s red arrow, @racket[top]'s green arrow is the same as @racket[left]'s green arrow, and
@racket[top]'s blue arrow is the same as @racket[left]'s blue arrow.

(We refer to the group named @racket['left] using the @tech{tag path} @racket['(left)].)

Suppose we want to rotate the cube @racket[30] degrees.
We have a few options.
@itemlist[
 @item{Recreate @racket[p1/left] with a rotated @racket[left] basis, and then recreate @racket[p3].}
 @item{Recreate @racket[p2/top] with a rotated @racket[top] basis, and then recreate @racket[p3].}
 @item{Replace the group named @racket['left] in @racket[p3] with the same group rotated @racket[30]
       degrees.}
]
The first option only requires us to replace @racket[(basis 'left (point-at v dv))] in the definition
of @racket[left] with @racket[(basis 'left (point-at v dv #:angle 30))].
The second option requires similar changes to @racket[top].
But at this point, with @racket[p3] already defined, both are more work than the third option, which
is simply this:
@interaction[#:eval pict3d-eval
                    (combine (replace-group p3 '(left) (λ (p) (rotate-z p 30)))
                             camera)]
This replaces, in @racket[p3], every group @racket[p] named @racket['left] with
@racket[(rotate-z p 30)].
The cube rotates because the group named @racket['left] in @racket[p3] isn't empty any longer: it's
been filled with a transformed @racket[p2/top].

It's instructive to consider what would have happened if we'd had @racket[p2/top]'s basis point in the
direction of the surface normal; i.e. if we had used @racket[+z] instead of @racket[-z].
@interaction[#:eval pict3d-eval
                    (define top (basis 'top (point-at (surface p2 +z) +z)))
                    (define p2/top (move-z (combine p2 top) 4))
                    (eval:alts p2/top ((λ () p2/top)))
                    (combine (pin p1/left '(left)
                                  p2/top '(top))
                             camera)]
Generally, when we want to pin two @racket[Pict3D] instances together using bases, one's basis should
point inward, and the other's should point outward.

@subsection[#:tag "combiners"]{Scene Combiners}

@deftogether[(@defidform[#:kind "type" Tag]
              @defthing[#:kind "predicate" tag? (-> Any Boolean : Tag)])]{
The type and predicate for names of groups.
Currently, @racket[Tag] is defined as @racket[(U Symbol Integer)].

Functions that operate on groups do not accept a @racket[Tag] value on its own.
They accept a @deftech{tag path}, which is a @emph{list} of @racket[Tag] values.
This allows them to operate on groups within groups.
For example, to remove the contents of the group named @racket['cannon] within a group named
@racket['player], we might write
@racketblock[(remove-in-group world '(player cannon))]
Then, only the poor player will be left without a printer.

To remove the contents of @emph{every} cannon group regardless of the group that contains it, we
might write
@racketblock[(remove-in-group world '(cannon))]

The tag path @racket['()] or @racket[empty] refers to an entire @racket[Pict3D].
Thus,
@racketblock[(remove-in-group world '())]
returns an empty @racket[Pict3D].
}

@deftogether[(@defproc[(group [pict Pict3D] [name (U #f Tag)]) Pict3D]
              @defproc[(group-tag [pict Pict3D]) (U #f Tag)]
              @defproc[(group-contents [pict Pict3D]) Pict3D])]{
@racket[(group pict name)] creates a @deftech{group}: a named collection of shapes.
If @racket[name] is @racket[#f], it simply returns @racket[pict], whose @tech{tag path} is
@racket['()].
Otherwise, the new group can be referred to by its tag path @racket[(list name)].

If @racket[pict] is a group, @racket[group-tag] returns its name and @racket[group-contents] returns
its contents.
If @racket[pict] isn't a group, @racket[group-tag] returns @racket[#f] and @racket[group-contents]
returns @racket[pict].

These seemingly strange rules preserve the following properties.
@itemlist[
 @item{@racket[(group-tag (group p n))] is equivalent to @racket[n].}
 @item{@racket[(group-contents (group p n))] is equivalent to @racket[p].}
 @item{@racket[(group (group-contents p) (group-tag p))] is equivalent to @racket[p].}
 ]
}

@defproc[(basis [name Tag] [t Affine]) Pict3D]{
Creates an empty group named @racket[name] with transformation @racket[t].
Equivalent to @racket[(transform (group empty-pict3d name) t)].
}

@defproc[(ungroup [pict Pict3D] [path (Listof Tag)]) Pict3D]{
Removes every group with the given path, but leaves the contents.
Equivalent to @racket[(replace-group pict path group-contents)].
}

@defproc[(remove-group [pict Pict3D] [path (Listof Tag)]) Pict3D]{
Removes every group with the given path, erasing the contents.
Equivalent to @racket[(replace-group pict path (λ (_) empty-pict3d))].
}

@defproc[(remove-in-group [pict Pict3D] [path (Listof Tag)]) Pict3D]{
Removes the contents of every group with the given path, but leaves the group.
Equivalent to @racket[(replace-in-group pict path (λ (_) empty-pict3d))].
}

@defproc[(replace-group [pict Pict3D] [path (Listof Tag)] [f (-> Pict3D Pict3D)]) Pict3D]{
Replaces every group @racket[p] in @racket[pict] with the given path with @racket[(f p)].
}

@defproc[(replace-in-group [pict Pict3D] [path (Listof Tag)] [f (-> Pict3D Pict3D)]) Pict3D]{
Replaces the @emph{contents} @racket[p] of every group in @racket[pict] with the given path
with @racket[(f p)].
Equivalent to @racket[(replace-group pict path (λ (p) (group (f (group-contents p)) (group-tag p))))].
}

@defproc[(set-origin [pict Pict3D] [path (U (Listof Tag) Affine)]) Pict3D]{
Transforms @racket[pict] so that the group with the given @tech{tag path}, or the given
@tech{affine transformation}, is aligned with the origin.
@examples[#:eval pict3d-eval
                 (define pict
                   (combine (cube origin 1/2)
                            (basis 'corner (point-at (pos 1/2 1/2 1/2) +x+y+z
                                                     #:angle 15))))
                 (eval:alts pict ((λ () pict)))
                 (set-origin pict '(corner))]
If there is more than one group with the given path, @racket[set-origin] raises an error.
}

@defproc[(join [pict1 Pict3D]
               [path1 (U (Listof Tag) Affine)]
               [pict2 Pict3D]
               [path2 (U (Listof Tag) Affine) '()]) Pict3D]{
Does two things:
@itemlist[#:style 'ordered
 @item{Transforms @racket[pict2] so that its group with path @racket[path2] aligns with the group 
       in @racket[pict1] with path @racket[path1].}
 @item{Combines the result with @racket[pict1].}
 ]
If @racket[path1] or @racket[path2] is given as an @tech{affine transformation}, @racket[join]
uses them directly instead of looking up a group transformation.

If there is more than one group with @racket[path1] in @racket[pict1], @racket[join] raises an error,
and likewise for @racket[path2] in @racket[pict2].

Recall that the @tech{tag path} @racket['()] represents the entire @racket[Pict3D].
Thus, when @racket[path2] is @racket['()] or is omitted, the origin and coordinate axes are used for
alignment.

@examples[#:eval pict3d-eval
                 (define p1 (combine (cylinder origin 1/2)
                                     (basis 'top (point-at (pos 0 0 1/2) +z))))
                 (eval:alts p1 ((λ () p1)))
                 (define p2 (cone origin 1/2))
                 (eval:alts p2 ((λ () p2)))
                 (join p1 '(top) (move-z p2 1/2))
                 (join p1 '(top)
                       (combine p2 (basis 'bot (point-at (pos 0 0 -1/2) +z)))
                       '(bot))]

When @racket[path1] and @racket[path2] are @tech{tag paths},
@racket[(join pict1 path1 pict2 path2)] is equivalent to
@racketblock[(combine
              pict1
              (relocate pict2
                        (find-group-transform pict2 path2)
                        (find-group-transform pict1 path1)))]
When @racket[path1] and @racket[path2] are @tech{affine transformations},
@racket[(join pict1 path1 pict2 path2)] is equivalent to
@racketblock[(combine pict1 (relocate pict2 path2 path1))]
}

@defproc[(glue [pict1 Pict3D]
               [path1 (U (Listof Tag) Affine)]
               [pict2 Pict3D]
               [path2 (U (Listof Tag) Affine) '()]) Pict3D]{
Like @racket[(join pict1 path1 pict2 path2)], but ungroups all groups in @racket[pict1] that have
path @racket[path1], and ungroups all groups in @racket[pict2] that have path @racket[path2].

Like @racket[join], @racket[glue] accepts @tech{affine transformations} as well as @tech{tag paths}.
}

@defproc[(pin [pict1 Pict3D]
              [path1 (Listof Tag)]
              [pict2 Pict3D]
              [path2 (U (Listof Tag) Affine) '()]) Pict3D]{
Similar to @racket[join], but additionally
@itemlist[
 @item{Ungroups the group @racket[path2] in @racket[pict2].}
 @item{Puts @racket[pict2] inside the group @racket[path1] in @racket[pict1].}
 ]
Use @racket[pin] to construct group hierarchies, in which updates to parent groups (using
@racket[replace-group] or @racket[replace-in-group]) affect child groups.
See @secref{combining-scenes} for an extended example.

In code, @racket[(pin pict1 path1 pict2 path2)] is equivalent to
@racketblock[(let ([pict2  (ungroup (set-origin pict2 path2) path2)])
               (replace-in-group pict1 path1 (λ (p) (combine p pict2))))]

Like @racket[join], @racket[pin] accepts an @tech{affine transformation} for its second argument.
}

@defproc[(weld [pict1 Pict3D]
               [path1 (Listof Tag)]
               [pict2 Pict3D]
               [path2 (U (Listof Tag) Affine) '()]) Pict3D]{
Like @racket[(pin pict1 path1 pict2 path2)], but additionally ungroups all groups in @racket[pict1]
that have path @racket[path1].

Use @racket[weld] instead of @racket[pin] when you don't intend to update the group with path
@racket[path1] in the result.
For example, use @racket[pin] to attach a swinging arm to a robot body, and use @racket[weld] to
place a roof on top of a house.
(Unless you intend to blow up the house later. Then you'll need to pin the roof.)

Like @racket[glue], @racket[weld] accepts an @tech{affine transformation} for its second argument.
}

@deftogether[[
  @defproc[(pin* [pict1 Pict3D]
                 [path1 (Listof Tag)]
                 [pict2 Pict3D]
                 [path2 (U (Listof Tag) Affine) '()]) Pict3D]
  @defproc[(weld* [pict1 Pict3D]
                  [path1 (Listof Tag)]
                  [pict2 Pict3D]
                  [path2 (U (Listof Tag) Affine) '()]) Pict3D]
  @defproc[(join* [pict1 Pict3D]
                  [path1 (U (Listof Tag) Affine)]
                  [pict2 Pict3D]
                  [path2 (U (Listof Tag) Affine) '()])
           Pict3D]
  @defproc[(glue* [pict1 Pict3D]
                  [path1 (U (Listof Tag) Affine)]
                  [pict2 Pict3D]
                  [path2 (U (Listof Tag) Affine) '()])
           Pict3D]
]]{
Like @racket[pin], @racket[weld], @racket[join], and @racket[glue], but if multiple groups with path
@racket[path1] exist in @racket[pict1], @racket[pict2] is pinned, welded, joined, or glued to all of
them.
}

@defproc[(map-group [pict Pict3D] [path (Listof Tag)] [f (-> Pict3D A)]) (Listof A)]{
Applies @racket[f] to every group with the given path in @racket[pict], and returns the results in a
list.
The list is in no particular order.
}

@defproc[(map-group/transform [pict Pict3D] [path (Listof Tag)] [f (-> Affine Pict3D A)]) (Listof A)]{
Like @racket[(map-group pict name f)], but @racket[f] is additionally supplied the transformation
from world coordinates to group coordinates.
}

@defproc[(find-group-transform [pict Pict3D] [path (Listof Tag)]) Affine]{
Finds the group within @racket[pict] with @racket[path], and returns the transformation from world
coordinates to that group's coordinates.

If multiple groups with @racket[path] exist, @racket[find-group-transform] raises an error.

This is used in the implementations of functions like @racket[join] and @racket[set-origin].
}

@defproc[(find-group-transforms [pict Pict3D] [path (Listof Tag)]) (Listof Affine)]{
Like @racket[find-group-transform], but if multiple groups with @racket[path] exist, returns a
transformation for each of them.

Equivalent to:
@racketblock[(map-group/transform pict path (λ ([t : Affine] _) t))]
}


@;{===================================================================================================
   ===================================================================================================
   ===================================================================================================
   }

@section[#:tag "affine"]{Transformation}

TODO: exposition about @deftech{linear transformations} and @deftech{affine transformations}

@subsection[#:tag "affine-types"]{Basic Transformation Data Types}

@deftogether[(@defidform[#:kind "type" Linear]
              @defthing[#:kind "predicate" linear? (-> Any Boolean : Linear)])]{
The type and predicate for linear transformations.
}

@defproc[(linear [dx Dir] [dy Dir] [dz Dir]) Linear]{
Converts three axes into a @tech{linear transformation}.
}

@deftogether[(@defproc[(linear-x-axis [t Linear]) Dir]
              @defproc[(linear-y-axis [t Linear]) Dir]
              @defproc[(linear-z-axis [t Linear]) Dir])]{
Return the axes of @racket[t] separately; i.e.
@racketblock[(match-define (linear dx dy dz) t)]
is equivalent to
@racketblock[(define dx (linear-x-axis t))
             (define dy (linear-y-axis t))
             (define dz (linear-z-axis t))]
}

@defthing[identity-linear Linear]{
The identity linear transformation: each axis is a coordinate axis.
@examples[#:eval normal-eval
                 identity-linear]
}

@defproc[(linear-compose [t Linear] ...) Linear]{
Composes any number of @tech{linear transformations}.
Applying the result applies each @racket[t] once, in reverse order (just like @racket[compose]).
}

@defproc[(linear-inverse [t Linear]) Linear]{
Returns the inverse of the transformation @racket[t].
Because @racket[Linear] instances store their inverses, this operation is cheap.

If @racket[t] isn't invertible, @racket[linear-inverse] raises an error.
See @racket[linear-singular?].
}

@defproc[(linear-singular? [t Linear]) Boolean]{
Returns @racket[#t] when @racket[(linear-inverse t)] would raise an error.
}

@deftogether[(@defidform[#:kind "type" Affine]
              @defthing[#:kind "predicate" affine? (-> Any Boolean : Affine)])]{
The type and predicate for parallel-line-preserving transformations.
}

@defproc[(affine [dx Dir] [dy Dir] [dz Dir] [p Pos]) Affine]{
Converts three axes and an origin into an @tech{affine transformation}.
}

@deftogether[(@defproc[(affine-x-axis [t Affine]) Dir]
              @defproc[(affine-y-axis [t Affine]) Dir]
              @defproc[(affine-z-axis [t Affine]) Dir]
              @defproc[(affine-origin [t Affine]) Pos])]{
Return the axes and origin of @racket[t] separately; i.e.
@racketblock[(match-define (affine dx dy dz p) t)]
is equivalent to
@racketblock[(define dx (affine-x-axis t))
             (define dy (affine-y-axis t))
             (define dz (affine-z-axis t))
             (define p  (affine-origin t))]
}

@defthing[identity-affine Affine]{
The identity affine transformation: each axis is a coordinate axis, and its origin is @racket[origin].
@examples[#:eval normal-eval
                 identity-affine
                 (affine-origin identity-affine)]
}

@defproc[(affine-compose [t Affine] ...) Affine]{
Composes any number of @tech{affine transformations}.
Applying the result applies each @racket[t] once, in reverse order (just like @racket[compose]).
}

@defproc[(affine-inverse [t Affine]) Affine]{
Returns the inverse of the transformation @racket[t].
Because @racket[Affine] instances store their inverses, this operation is cheap.

If @racket[t] isn't invertible, @racket[affine-inverse] raises an error.
See @racket[affine-singular?].
}

@defproc[(affine-singular? [t Affine]) Boolean]{
Returns @racket[#t] when @racket[(affine-inverse t)] would raise an error.
}

@deftogether[(@defproc[(linear-consistent? [t Linear]) Boolean]
              @defproc[(affine-consistent? [t Affine]) Boolean])]{
Return @racket[#t] when @racket[t] preserves
@hyperlink["http://en.wikipedia.org/wiki/Orientation_%28vector_space%29"]{orientation}, or handedness.
An inconsistent transformation turns clockwise-oriented shapes or directions counterclockwise, and
vice-versa.

Some 3D engines are sensitive to consistency: they will render shapes inside-out or turn normals the
wrong direction when inconsistent transformations are applied.
Pict3D's rendering engine is not sensitive to consistency.
@examples[#:eval pict3d-eval
                 (define pict (cube origin 1/2))
                 (define t (scale -1))
                 (affine-consistent? t)
                 (transform pict t)]
These functions are helpful for writing algorithms that preprocess geometric data in a
consistency-insensitive way.
}

@subsection[#:tag "affine-transforms"]{Transformation Combiners and Constructors}

@defproc[(transform [pict Pict3D] [t Affine]) Pict3D]{
Transforms @racket[pict] by applying @racket[t].
@examples[#:eval pict3d-eval
                 (define pict (cube origin 1/2))
                 (define t (affine-compose (move-z 1/4) (rotate-z 15)))
                 (transform pict t)]
}

@deftogether[(@defproc[(move [pict Pict3D] [dv Dir]) Pict3D]
              @defproc[(move-x [pict Pict3D] [dx Real]) Pict3D]
              @defproc[(move-y [pict Pict3D] [dy Real]) Pict3D]
              @defproc[(move-z [pict Pict3D] [dz Real]) Pict3D])]{
Move @racket[pict] in direction @racket[dv], or along an axis @racket[dx], @racket[dy] or @racket[dz]
units.
}

@deftogether[(@defproc[#:link-target? #f (move [dv Dir]) Affine]
              @defproc[#:link-target? #f (move-x [dx Real]) Affine]
              @defproc[#:link-target? #f (move-y [dy Real]) Affine]
              @defproc[#:link-target? #f (move-z [dz Real]) Affine])]{
Transformation-returning versions of the above.
Any @racket[(move pict ...)] is equivalent to @racket[(transform pict (move ...))].
}

@deftogether[(@defproc[(scale [pict Pict3D] [dv (U Real Dir)]) Pict3D]
              @defproc[(scale-x [pict Pict3D] [dx Real]) Pict3D]
              @defproc[(scale-y [pict Pict3D] [dy Real]) Pict3D]
              @defproc[(scale-z [pict Pict3D] [dz Real]) Pict3D])]{
Scales @racket[pict] by @racket[dv], or by @racket[dx], @racket[dy] or @racket[dz] units along an
axis.
If @racket[dv] is a real number @racket[d], it's equivalent to @racket[(dir d d d)] (i.e. uniform
scaling).

The center of scaling---i.e. the only point that does not move---is the @racket[origin].
This is often not what you want.
To scale @racket[pict] with a different center, see @racket[scale/center].
}

@deftogether[(@defproc[#:link-target? #f (scale [dv (U Real Dir)]) Linear]
              @defproc[#:link-target? #f (scale-x [dx Real]) Linear]
              @defproc[#:link-target? #f (scale-y [dy Real]) Linear]
              @defproc[#:link-target? #f (scale-z [dz Real]) Linear])]{
Transformation-returning versions of the above.
Any @racket[(scale pict ...)] is equivalent to @racket[(transform pict (scale ...))].
}

@deftogether[(@defproc[(rotate [pict Pict3D] [axis Dir] [angle Real]) Pict3D]
              @defproc[(rotate-x [pict Pict3D] [angle Real]) Pict3D]
              @defproc[(rotate-y [pict Pict3D] [angle Real]) Pict3D]
              @defproc[(rotate-z [pict Pict3D] [angle Real]) Pict3D])]{
Rotate @racket[pict], @racket[angle] degrees counterclockwise around @racket[axis], or around the
@racket[+x], @racket[+y] or @racket[+z] axis.

The center of rotation is the @racket[origin].
This is often not what you want.
To rotate @racket[pict] around a different center, see @racket[rotate/center].
}

@deftogether[(@defproc[#:link-target? #f (rotate [axis Dir] [angle Real]) Linear]
              @defproc[#:link-target? #f (rotate-x [angle Real]) Linear]
              @defproc[#:link-target? #f (rotate-y [angle Real]) Linear]
              @defproc[#:link-target? #f (rotate-z [angle Real]) Linear])]{
Transformation-returning versions of the above.
Any @racket[(rotate pict ...)] is equivalent to @racket[(transform pict (rotate ...))].
}

@deftogether[(@defproc[(scale/center [pict Pict3D] [dv (U Real Dir)] [v Pos (center pict)]) Pict3D]
              @defproc[(scale-x/center [pict Pict3D] [dx Real] [v Pos (center pict)]) Pict3D]
              @defproc[(scale-y/center [pict Pict3D] [dy Real] [v Pos (center pict)]) Pict3D]
              @defproc[(scale-z/center [pict Pict3D] [dz Real] [v Pos (center pict)]) Pict3D])]{
Scales @racket[pict] by @racket[dv], or by @racket[dx], @racket[dy] or @racket[dz] units along an
axis, with center @racket[v].

By default, @racket[v] is the center of @racket[pict]'s bounding box.
If @racket[pict] doesn't have a bounding box (for example, it's the @racket[empty-pict3d]), the
@racket[origin] is the default center.

@racket[(scale/center pict dv v)] is implemented as
@racketblock[(local-transform pict (scale dv) (move (pos- v origin)))]
In other words, @racket[pict] is scaled in the local coordinate space of an axis-aligned basis
with center @racket[v].
}

@deftogether[(@defproc[(rotate/center [pict Pict3D] [axis Dir] [angle Real]
                                      [v Pos (center pict)]) Pict3D]
              @defproc[(rotate-x/center [pict Pict3D] [angle Real] [v Pos (center pict)]) Pict3D]
              @defproc[(rotate-y/center [pict Pict3D] [angle Real] [v Pos (center pict)]) Pict3D]
              @defproc[(rotate-z/center [pict Pict3D] [angle Real] [v Pos (center pict)]) Pict3D])]{
Rotate @racket[pict], @racket[angle] degrees counterclockwise around @racket[axis], or around the
@racket[+x], @racket[+y] or @racket[+z] axis, with center @racket[v].

By default, @racket[v] is the center of @racket[pict]'s bounding box.
If @racket[pict] doesn't have a bounding box (for example, it's the @racket[empty-pict3d]), the
@racket[origin] is the default center.

@racket[(rotate/center pict axis angle v)] is implemented as
@racketblock[(local-transform pict (rotate axis angle) (move (pos- v origin)))]
In other words, @racket[pict] is rotated in the local coordinate space of an axis-aligned basis
with center @racket[v].
}

@defproc*[([(point-at [v Pos]
                      [dv Dir]
                      [#:angle angle Real 0]
                      [#:up up Dir +z]
                      [#:normalize? normalize? Any #t])
            Affine]
           [(point-at [v1 Pos]
                      [v2 Pos]
                      [#:angle angle Real 0]
                      [#:up up Dir +z]
                      [#:normalize? normalize? Any #t])
            Affine])]{
Returns a transformation that ``points'' from @racket[v] in direction @racket[dv], or from
@racket[v1] to @racket[v2].
                                              
More specifically, the @italic{z} axis of the transformation points as described.
If @racket[normalize?] isn't @racket[#f], the @italic{z} axis has distance @racket[1].
Otherwise, @racket[dv] or @racket[(pos- v2 v1)] is used as the @italic{z} axis directly.

The other axes always have distance @racket[1], are perpendicular to the @italic{z} axis and each
other, and are rotated about the @italic{z} axis @racket[angle] degrees counterclockwise (viewing the
@italic{z} axis head-on).
When @racket[angle] is @racket[0], the @italic{y} axis points opposite @racket[up] (i.e. downward),
and the @italic{x} axis points rightward.
When the @italic{z} axis is parallel to @racket[up], the rotation is arbitrary but always defined.

This function is really more intuitive than the above discription might suggest.
It's best used to place basis groups and cameras, and to stretch shapes between two points
(see @racket[relocate]).
}

@defproc*[([(relocate [t1 Affine] [t2 Affine]) Affine]
           [(relocate [pict Pict3D] [t1 Affine] [t2 Affine]) Pict3D])]{
Transforms @racket[pict] from the local coordinate space defined by @racket[t1] into the local
coordinate space defined by @racket[t2], or returns an @racket[Affine] that does so.

For example, suppose we define a cylinder centered on the origin, and we want to stretch it between
two arbitrary points.
@interaction[#:eval pict3d-eval
                    (define pict (with-color (rgba "red" 0.9)
                                   (cylinder origin (dir 1/4 1/4 1/2))))
                    (define pict-t (point-at (pos 0 0 -1/2) +z))
                    (combine pict
                             (basis 'pict-t pict-t))]
(We're only using @racket[basis] to visualize the transformation @racket[pict-t].)
To stretch it between @racket[v1] and @racket[v2], we define a @racket[point-at] transformation:
@interaction[#:eval pict3d-eval
                    (define v1 (pos 1 0 1))
                    (define v2 (pos 0 1 1))
                    (define new-t (point-at v1 v2 #:normalize? #f))
                    (combine (sphere v1 0.2)
                             (sphere v2 0.2)
                             (basis 'new-t new-t))]
Then we can use @racket[relocate] to move @racket[pict] into the new coordinate space:
@interaction[#:eval pict3d-eval
                    (combine (sphere v1 0.2)
                             (sphere v2 0.2)
                             (relocate pict pict-t new-t))]
@racket[(relocate t1 t2)] is implemented as
@racketblock[(affine-compose t2 (affine-inverse t1))]
In other words, undo transformation @racket[t1], then do transformation @racket[t2].
}

@defproc*[([(local-transform [t Affine] [local-t Affine]) Affine]
           [(local-transform [pict Pict3D] [t Affine] [local-t Affine]) Pict3D])]{
Applies @racket[t] to @racket[pict] in the local coordinate space defined by @racket[local-t], or
returns an @racket[Affine] that does so.

@racket[(local-transform t local-t)] is implemented as
@racketblock[(affine-compose local-t (relocate local-t t))]
which is equivalent to
@racketblock[(affine-compose local-t t (affine-inverse local-t))]
This operation is also known as a
@hyperlink["http://en.wikipedia.org/wiki/Change_of_basis"]{change of basis}.

The @racket[scale/center] and @racket[rotate/center] functions are defined using
@racket[local-transform], to apply scaling and rotating transformations in a local coordinate space
with a center other than the @racket[origin].
}

@deftogether[(@defproc[(transform-pos [v Pos] [t Affine]) Pos]
              @defproc[(transform-dir [dv Dir] [t Affine]) Dir]
              @defproc[(transform-norm [dv Dir] [t Affine]) Dir])]{
Apply @tech{affine transformation} @racket[t] to a position, direction or normal.

The difference between applying a transformation to a direction and to a normal is best communicated
with an extended example.
@examples[#:eval pict3d-eval
                 (define pict (sphere origin 1))
                 (define-values (vs dvs)
                   (for*/lists (vs dvs) ([dx  (in-range -1 5/4 1/4)]
                                         [dy  (in-range -1 5/4 1/4)])
                     (define data (surface/data pict (dir dx dy 1)))
                     (values (surface-data-pos data) (surface-data-normal data))))
                 
                 (combine
                  pict
                  (for/list ([v  (in-list vs)]
                             [dv  (in-list dvs)])
                    (arrow v (dir-scale dv 0.5))))
                 
                 (define t (scale-z 1/4))
                 
                 (combine
                  (transform pict t)
                  (for/list ([v  (in-list vs)]
                             [dv  (in-list dvs)])
                    (arrow (transform-pos v t)
                           (dir-scale (transform-dir dv t) 0.5))))
                 
                 (combine
                  (transform pict t)
                  (for/list ([v  (in-list vs)]
                             [dv  (in-list dvs)])
                    (arrow (transform-pos v t)
                           (dir-scale (transform-norm dv t) 0.5))))]
In the second @racket[Pict3D], the directions have been flattened along with the sphere.
In the third, they maintain their orthogonality to the surface.
}

@defproc[(camera-transform [pict Pict3D]) (U #f Affine)]{
Returns the camera used to orient the initial view, if at least one @racket['camera] @tech{basis} is
in @racket[pict].
If there are none, returns @racket[#f].
}

@defproc[(camera-ray-dir [t Affine]
                         [#:width width Integer (current-pict3d-width)]
                         [#:height height Integer (current-pict3d-height)]
                         [#:z-near z-near Real (current-pict3d-z-near)]
                         [#:z-far z-far Real (current-pict3d-z-far)]
                         [#:fov fov Real (current-pict3d-fov)])
         (-> Real Real Dir)]{
Creates a function that accepts screen coordinates and returns the direction of the ray from the
camera's origin to those coordinates in the camera's local coordinate space.

If @racket[(<= 0 x width)] and @racket[(<= 0 y height)], then the position
@racket[(pos+ (affine-origin t) ((camera-ray-dir t) x y))] lies on the z-near plane.

A common use of @racket[camera-ray-dir] is to find objects under a mouse cursor in a rendered
@racket[Pict3D].

A less common use is to build a ray tracer in very few lines of code, such as the following.

@interaction[#:eval pict3d-eval
                    (define p (combine
                               (cube origin 1/2)
                               (for*/list ([i  (in-range -1 2)]
                                           [j  (in-range -1 2)]
                                           [k  (in-range -1 2)])
                                 (sphere (pos (* 0.35 i) (* 0.35 j) (* 0.35 k)) 0.2))))
                    (eval:alts p ((λ () p)))
                    (define t ((current-pict3d-auto-camera) p))
                    (define v0 (affine-origin t))
                    (define ray-dir (camera-ray-dir t))
                    (define l (pos 1.0 1.5 2.0))
                    (require images/flomap)
                    (flomap->bitmap
                     (build-flomap
                      1 (current-pict3d-width) (current-pict3d-height)
                      (code:comment "for each screen coordinate on a grayscale surface...")
                      (λ (_ x y)
                        (code:comment "trace from the camera origin through the screen at x,y")
                        (define-values (v n) (trace/normal p v0 (ray-dir (+ x 0.5) (+ y 0.5))))
                        (cond [(and v n)
                               (code:comment "direction from surface point to light source")
                               (define dl (pos- l v))
                               (code:comment "distance squared to light source")
                               (define m^2 (dir-dist^2 dl))
                               (code:comment "3.0 brightness, inverse-square attenuation")
                               (define a (/ 3.0 m^2))
                               (code:comment "Lambert's cosine law (fully diffuse material)")
                               (define b (max 0.0 (/ (dir-dot n dl) (sqrt m^2))))
                               (code:comment "compute brightness, apply gamma correction")
                               (expt (* a b) (/ 1.0 2.2))]
                              [else  0.0]))))]
}

@;{===================================================================================================
   ===================================================================================================
   ===================================================================================================
   }

@section[#:tag "smooth"]{Deformation and Tessellation}

TODO: exposition about @deftech{deformation} and @deftech{tessellation} and @deftech{Jacobian}

@subsection[#:tag "smooth-types"]{Basic Deformation Data Types}

@deftogether[(@defidform[#:kind "type" Smooth]
              @defthing[#:kind "predicate" smooth? (-> Any Boolean : Smooth)])]{
The type and predicate for three-dimensional, almost-everywhere continuously differentiable
functions.
This includes @racket[Affine] (and thus @racket[Linear]) as a subtype.
}

@defproc*[([(smooth [f (-> Pos Pos)]) Smooth]
           [(smooth [f (-> Pos Pos)] [j (-> Pos Linear)]) Smooth])]{
Creates a deformation function. If only @racket[f] is given, it is numerically differentiated to
construct the @tech{Jacobian} function. Otherwise, @racket[j] is used.

@bold{Warning:} If @racket[f] is numerically differentiated, it may be evaluated outside of its
intended domain.
}

@deftogether[(@defproc[(smooth-function [t Smooth]) (-> Pos Pos)]
              @defproc[(smooth-jacobian [t Smooth]) (-> Pos Linear)])]{
Return the function represented by @racket[t] and its @tech{Jacobian} matrix of partial derivatives.

When @racket[t] is @racket[Linear] or @racket[Affine], @racket[(smooth-function t)] is equivalent to
@racket[(λ (v) (transform-pos v t))].

When @racket[t] is @racket[Linear], @racket[(smooth-jacobian t)] is equivalent to @racket[(λ (_) t)].

When @racket[t] is @racket[Affine], @racket[(smooth-jacobian t)] is equivalent to
@racketblock[(match-let ([(affine dx dy dz v)  t])
               (λ (_) (linear dx dy dz)))]
}

@defthing[identity-smooth Smooth]{
The identity deformation. Equivalent to @racket[identity-linear] and @racket[identity-affine].
}

@defproc[(smooth-compose [t Smooth] ...) Smooth]{
Composes any number of @tech{deformations}.
Applying the result applies each @racket[t] once, in reverse order (just like @racket[compose]).

@examples[#:eval pict3d-eval
                 (combine
                  (deform (tessellate (rectangle (pos 0 0 2) (dir 1/8 1/4 2))
                                      #:segments 48)
                    (smooth-compose
                     (move-x (/ -2 pi))
                     (bend 360 (interval 0 4))
                     (twist 45)
                     (rotate-z 45)))
                  (basis 'camera (point-at (pos 1/2 1 1/2) (pos 1/8 0 1/8))))]
}

@defproc[(smooth-approximate [t Smooth] [v Pos]) Affine]{
Returns the best linear approximation of @racket[t] at point @racket[v].

If @racket[t] is @racket[Linear] or @racket[Affine], this simply returns @racket[t].
Otherwise, @racket[(smooth-approximate t v)] is equivalent to
@racketblock[(match-let ([(linear dx dy dz)  ((smooth-jacobian f) v)])
               (define v0 ((smooth-function f) v))
               (affine dx dy dz (pos+ origin (pos- v0 v))))]
}

@defproc[(smooth-singular? [t Smooth] [v Pos]) Boolean]{
Returns @racket[#t] when inverting @racket[((smooth-jacobian t) v)] would raise an error.
See @racket[linear-inverse] and @racket[linear-singular?].
}

@defproc[(smooth-consistent? [t Smooth] [v Pos]) Boolean]{
Returns @racket[#t] when @racket[t] preserves orientation, or handedness, at @racket[v].
See @racket[linear-consistent?].
}

@subsection[#:tag "tesselation"]{Tessellation}

@defproc[(tessellate [pict Pict3D]
                     [#:segments segments Integer (current-tessellate-segments)]
                     [#:max-edge max-edge (U #f Real) (current-tessellate-max-edge)]
                     [#:max-angle max-angle Real (current-tessellate-max-angle)])
         Pict3D]{
Approximates @racket[pict] with triangles so @tech{deformations} can be applied to their vertices.

Each kind of shape has its own tessellation algorithm, which follows the these general guidelines:
@itemlist[
 @item{Edges that approximate part of an arc represent no more than @racket[max-angle] degrees
       of the arc.}
 @item{Other edges are shorter than @racket[max-edge].}
]
When @racket[max-edge] is @racket[#f] (the default), it's computed as @racket[(/ l segments)], where
@racket[l] is the length of the longest axis of @racket[pict]'s bounding rectangle.

@examples[#:eval pict3d-eval
                 (current-pict3d-add-wireframe 'color)
                 (combine
                  (tessellate (rectangle origin (dir 1/4 1/4 1))
                              #:segments 4)
                  (basis 'camera (point-at (pos 0.75 0.75 1.25) origin)))
                 (tessellate (sphere origin 1/2)
                             #:max-angle (/ 90 4))
                 (tessellate (cylinder origin 1/2 #:arc (arc 90 45))
                             #:max-angle 45 #:max-edge 1/4)
                 (tessellate (cone origin 1/2)
                             #:max-angle 30 #:segments 32)
                 (current-pict3d-add-wireframe #f)]
}

@defproc[(adaptive-tessellate
          [pict Pict3D]
          [t Smooth identity-smooth]
          [#:segments segments Integer (current-adaptive-segments)]
          [#:max-edge max-edge (U #f Real) (current-adaptive-max-edge)]
          [#:max-angle max-angle Real (current-adaptive-max-angle)]
          [#:max-iters max-iters Integer (current-adaptive-max-iters)])
         Pict3D]{
Tries to tessellate @racket[pict] in a way that reduces visual artifacts when deformed by @racket[t].
This function is available for completeness; usually @racket[adaptive-deform] is more appropriate.
                    
@examples[#:eval pict3d-eval
                 (current-pict3d-add-wireframe 'color)
                 (define c (basis 'camera (point-at (pos 2.5 1.5 1.0)
                                                    (dir -0.75 -0.5 -0.25))))
                 (define t (bend 90 (interval -1/2 1/2)))
                 (define p (adaptive-tessellate (cylinder origin (dir 1/2 1/2 2)) t))
                 (combine c p)
                 (combine c (deform p t))
                 (current-pict3d-add-wireframe #f)]

Except for @racket[max-iters], the keyword arguments have similar meaning to those of
@racket[tessellate].
However, @racket[adaptive-tessellate]
@itemlist[#:style 'ordered
 @item{Requests an initial tessellation of each shape with @racket[max-angle = 90] and
       @racket[max-edge = +inf.0].}
 @item{For up to @racket[max-iters] iterations, splits triangle edges that, @emph{when transformed},
       would be too long or represent too many degrees of an arc, and moves the new vertex onto the
       shape's surface.}
 @item{For up to 3 iterations, splits triangle edges that change orientation when transformed, 
       using @racket[flbracketed-root] to solve for locations at which @racket[t]'s
       @tech{Jacobian} determinant is zero.}
]
This is a lot of extra work, which usually can't be done at interactive speeds.
On the other hand, @racket[adaptive-tessellate] sometimes uses fewer triangles (this is still a
work in progress), and can deal better with deformations with discontinuities and local
noninvertibility.

@examples[#:eval pict3d-eval
                 (define t
                   (smooth (λ (v)
                             (match-define (pos x y z) v)
                             (pos x y (* x z)))))
                 (deform (tessellate (ellipsoid (pos -1/3 0 0) (dir 1 1/2 1/2))) t)
                 (adaptive-deform (ellipsoid (pos -1/3 0 0) (dir 1 1/2 1/2)) t)]
}

@deftogether[(@deftypedparam[current-tessellate-segments segments Integer Natural (#:value 12)]
              @deftypedparam[current-tessellate-max-edge max-edge (U #f Real) (U #f Positive-Flonum)
                                                         (#:value #f)]
              @deftypedparam[current-tessellate-max-angle max-angle Real Flonum (#:value 15.0)])]{
Default keyword argument values for @racket[tessellate].
}

@deftogether[(@deftypedparam[current-adaptive-segments segments Integer Natural (#:value 0)]
              @deftypedparam[current-adaptive-max-edge max-edge (U #f Real) (U #f Positive-Flonum)
                                                       (#:value #f)]
              @deftypedparam[current-adaptive-max-angle max-angle Real Flonum (#:value 15.0)]
              @deftypedparam[current-adaptive-max-iters max-iters Integer Natural (#:value 5)])]{
Default keyword argument values for @racket[adaptive-tessellate] and @racket[adaptive-deform].
}

@subsection[#:tag "smooth-deforms"]{Deformation Constructors and Combiners}

@deftogether[(@defproc[(deform-pos [v Pos] [t Smooth]) Pos]
              @defproc[(deform-dir [v Pos] [dv Dir] [t Smooth]) Dir]
              @defproc[(deform-norm [v Pos] [dv Dir] [t Smooth]) (U #f Dir)])]{
Apply @tech{deformation} @racket[t] to a position, direction or normal.

These are analogous to @racket[transform-pos], @racket[transform-dir] and @racket[transform-norm],
respectively.
(In fact, when @racket[t] is @racket[Affine], they simply apply their affine counterparts.)
The main difference is that @racket[deform-dir] and @racket[deform-norm] require a position
argument to apply the @tech{Jacobian} to.
}

@defproc[(deform-affine [t0 Affine] [t Smooth]) (U #f Affine)]{
Applies @tech{deformation} @racket[t] to an affine transformation.

If @racket[t] is @racket[Affine], this is equivalent to @racket[(affine-compose t t0)].
Otherwise, it's equivalent to
@racketblock[(match-let ([(affine dx dy dz v)  t0])
               (affine (deform-dir v dx t)
                       (deform-dir v dy t)
                       (deform-dir v dz t)
                       (deform-pos v t)))]

Group transformations, and untessellated solid objects such as @racket[sphere]s, are deformed
using @racket[deform-affine].
}

@defproc[(deform [pict Pict3D] [t Smooth]) Pict3D]{
Deforms @racket[pict] by applying @racket[t] to its positions, normals and affine transformations.
}

@defproc[(adaptive-deform
          [pict Pict3D]
          [t Smooth]
          [#:segments segments Integer (current-adaptive-segments)]
          [#:max-edge max-edge (U #f Real) (current-adaptive-max-edge)]
          [#:max-angle max-angle Real (current-adaptive-max-angle)]
          [#:max-iters max-iters Integer (current-adaptive-max-iters)])
         Pict3D]{
Tries to tessellate @racket[pict] in a way that reduces visual artifacts when deformed by @racket[t],
and then deforms the tessellation by @racket[t].
See @racket[adaptive-tessellate] for discussion and examples.
}

@defproc*[([(local-deform [t Smooth] [local-t Affine]) Smooth]
           [(local-deform [pict Pict3D] [t Smooth] [local-t Affine]) Pict3D])]{
Applies @racket[t] to @racket[pict] in the local coordinate space defined by @racket[local-t], or
returns a @racket[Smooth] function that does so. Analogous to @racket[local-transform].

The two-argument version is equivalent to
@racket[(smooth-compose local-t t (affine-inverse local-t))].
In English, undo @racket[local-t], do @racket[t], then redo @racket[local-t].
}

@defproc[(twist [pict Pict3D] [angle Real]) Pict3D]{
Twists @racket[pict] around the @italic{z} axis, @racket[angle] degrees per unit height.
@examples[#:eval pict3d-eval
                 (twist (tessellate (pipe origin (dir 1 1/2 1))) 90)]
In the above example, the pipe is twisted 180 degrees: 90 degrees for one unit below the origin,
and 90 degrees for one unit above the origin.
}

@defproc[(displace [pict Pict3D] [f (-> Flonum Flonum Real)]) Pict3D]{
Adds @racket[(f x y)] to every point's @italic{z} coordinate in @racket[pict].
@examples[#:eval pict3d-eval
                 (displace (tessellate (rectangle origin (dir 2/3 2/3 1/4)))
                           (λ (x y) (- (sqr x) (sqr y))))]
}

@defproc*[([(bend [pict Pict3D] [angle Real]) Pict3D]
           [(bend [pict Pict3D] [angle Real] [zrange Interval]) Pict3D])]{
Bends @racket[pict] by bending an interval of the @italic{z} axis @racket[angle] degrees
counterclockwise around the @italic{y} axis.

If @racket[zrange] is given, only that interval on the @italic{z} axis is bent; otherwise,
the entire @italic{z} extent of @racket[pict] is used.
The bent interval always retains its length.

@examples[#:eval pict3d-eval
                 (combine
                  (bend (tessellate (ellipsoid origin (dir 1/8 1/16 1)))
                        270)
                  (with-color (rgba "orange")
                    (bend (tessellate (rectangle origin (dir 1/8 1/4 1)))
                          -180))
                  (with-color (rgba "lightgreen" 0.5)
                    (bend (tessellate (cone (pos 0 0 1) (dir 1/4 1/8 1)))
                          90 (interval 1/2 2)))
                  (with-color (rgba "crimson" 0.5)
                    (bend (tessellate (cylinder (pos 0 0 -1) (dir 1/4 1/8 1)))
                          -45 (interval -1/2 -2)))
                  (basis 'camera (point-at (pos 0.8 1.6 0.5) (pos 0 0 1/4))))]
}

@deftogether[(@defproc[#:link-target? #f (bend [angle Real] [zrange Interval]) Smooth]
              @defproc[#:link-target? #f (twist [angle Real]) Smooth]
              @defproc[#:link-target? #f (displace [f (-> Flonum Flonum Real)]) Smooth])]{
Deformation-returning versions of the above.
}

@defproc[(smooth-between [t1 Smooth] [t2 Smooth] [α (U Real (-> Pos Real))]) Smooth]{
Returns an average of @racket[t1] and @racket[t2], weighted by @racket[α].
When @racket[α] is a function, the weight at each point @racket[v] is @racket[(α v)].

One use for @racket[smooth-between] is to apply a deformation to a small pocket of space, by
blending between that deformation and @racket[identity-smooth].
@examples[#:eval pict3d-eval
                 (eval:alts (require plot)
                            (require (only-in plot/pict plot function)))

                 (define (f1 z) (exp (- (sqr z))))
                 (define (f2 z) (if (> (sqr z) 1) 0 (sqrt (- 1 (sqr z)))))
                 
                 (plot (list (function f1 #:label "f1")
                             (function f2 #:color 2 #:label "f2"))
                       #:x-min -2 #:x-max 2 #:y-min 0 #:y-max 1
                       #:x-label "z" #:y-label "α")
                 
                 (define (α1 v) (f1 (pos-z v)))
                 (define (α2 v) (f2 (pos-z v)))
                 
                 (define p (tessellate (rectangle origin (dir 1/2 1/2 2)) #:segments 24))
                 (define c (basis 'camera (point-at (pos 2 2 1) (pos 0 0 1/4))))
                 (combine c (deform p (twist 90)))
                 (combine c (deform p (smooth-between identity-smooth (twist 90) α1)))
                 (combine c (deform p (smooth-between identity-smooth (twist 90) α2)))]
}

@;{===================================================================================================
   ===================================================================================================
   ===================================================================================================
   }

@section[#:tag "collision"]{Collision Detection}

TODO: exposition about intersection tests, @deftech{surface data}, etc.

@deftogether[(@defidform[#:kind "type" Surface-Data]
              @defthing[#:kind "predicate" surface-data? (-> Any Boolean : Surface-Data)])]{
The type and predicate of @tech{surface data}.
}

@defproc[(surface-data [dist Nonnegative-Flonum]
                       [pos Pos]
                       [#:normal normal (U #f Dir) #f]
                       [#:path path (Listof Tag) empty])
         Surface-Data]{
Constructs surface data. Usually, you would receive such data from @racket[trace/data]
or @racket[surface/data], rather than constructing it yourself.
}

@deftogether[(@defproc[(surface-data-dist [data Surface-Data]) Nonnegative-Flonum]
              @defproc[(surface-data-pos [data Surface-Data]) Pos]
              @defproc[(surface-data-normal [data Surface-Data]) (U #f Dir)]
              @defproc[(surface-data-path [data Surface-Data]) (Listof Tag)])]{
Return the attributes of @racket[data].
}


@defproc*[([(trace [pict Pict3D] [v Pos] [dv Dir]) (U #f Pos)]
           [(trace [pict Pict3D] [v1 Pos] [v2 Pos]) (U #f Pos)])]{
Finds the first surface in @racket[pict] intersected by a ray starting at @racket[v] with direction
@racket[dv], or between @racket[v1] and @racket[v2].
Returns the intersection point when such a surface exists; otherwise @racket[#f].
This does @emph{not} return points on the back faces of triangles or the insides of solid shapes.
}

@defproc[(surface [pict Pict3D] [dv Dir] [#:inside? inside? Any #f]) (U #f Pos)]{
If @racket[inside?] is @racket[#f], finds the outermost, outward-facing surface from
@racket[(center pict)] in direction @racket[dv].
Otherwise, @racket[surface] finds the innermost, inward-facing surface from @racket[(center pict)]
in direction @racket[dv].
In either case, when such a surface exists, @racket[surface] returns the intersection point;
otherwise it returns @racket[#f].

@examples[#:eval pict3d-eval
                 (define pict (with-color (rgba "red")
                                (sphere origin 1/2)))
                 (combine
                  pict
                  (for*/list ([ρ  (in-range -85 86 10)]
                              [θ  (in-range -180 180 10)])
                    (sphere (surface pict (angles->dir θ ρ)) 0.01)))]
}

@defproc*[([(trace/data [pict Pict3D] [v Pos] [dv Dir]) (U #f Surface-Data)]
           [(trace/data [pict Pict3D] [v1 Pos] [v2 Pos]) (U #f Surface-Data)])]{
Like @racket[trace], but additionally returns @tech{surface data}.
}

@defproc[(surface/data [pict Pict3D] [dv Dir] [#:inside? inside? Any #f]) (U #f Surface-Data)]{
Like @racket[surface], but additionally returns @tech{surface data}.
}

@defproc*[([(trace/normal [pict Pict3D] [v Pos] [dv Dir]) (Values (U False Pos) (U False Dir))]
           [(trace/normal [pict Pict3D] [v1 Pos] [v2 Pos]) (Values (U False Pos) (U False Dir))])]{
Like @racket[trace/data], but only additionally returns a @tech{surface normal}.
}

@defproc[(surface/normal [pict Pict3D] [dv Dir] [#:inside? inside? Any #f]) (Values (U False Pos)
                                                                                    (U False Dir))]{
Like @racket[surface/data], but only additionally returns a @tech{surface normal}.
}

@defproc[(bounding-rectangle [pict Pict3D]) (Values (U #f Pos) (U #f Pos))]{
Returns the minimum and maximum corners of a rectangle that contains the visible objects in
@racket[pict], if one exists.
}

@defproc[(center [pict Pict3D]) (U #f Pos)]{
Returns the center of @racket[pict]'s bounding rectangle, if one exists.
}

@;{===================================================================================================
   ===================================================================================================
   ===================================================================================================
   }

@section[#:tag "rendering"]{Rendering}

@subsection[#:tag "optimization"]{Rendering Optimization}

@defproc[(freeze [pict Pict3D]) Pict3D]{
Returns a @racket[Pict3D] with identical shapes and lights, which, when first rendered, is optimized
for streaming to the graphics card.
Nonempty groups are @racket[ungroup]ed.
Empty groups are retained.

The optimization takes more time than rendering the original @racket[pict], so use @racket[freeze] to
optimize unchanging, frequently drawn sub-scenes.
For static scenes with tens of thousands of shapes, order-of-magnitude speedups are typical.

The results of transformations, and of combiners such as @racket[combine] and @racket[pin], which do
not alter shapes or their attributes, retain the optimized streaming data.
The results of attribute-altering functions such as @racket[set-color] do not.
}

@subsection[#:tag "parameters"]{Rendering Parameters}

The parameters in this section are read when a @racket[Pict3D] is @emph{displayed}, not when it's
@emph{created}.
In other words, don't expect this to result in a 512x512 display:
@interaction[#:eval pict3d-eval
                    (parameterize ([current-pict3d-width 512]
                                   [current-pict3d-height 512])
                      (sphere origin 1/2))]
By the time the sphere is displayed, the parameters have returned to their original values.

This is correct behavior.
A @racket[Pict3D] represents the contents of 3D space, which has nothing to do with how those
contents are viewed.

For the parameters to have an effect on interactive displays, they should be mutated:
@interaction[#:eval pict3d-eval
                    (current-pict3d-width 512)
                    (current-pict3d-height 512)
                    (sphere origin 1/2)]

@interaction-eval[#:eval pict3d-eval
                         (begin
                           (current-pict3d-width default-pict3d-width)
                           (current-pict3d-height default-pict3d-height))]

See the @method[pict3d-canvas% set-pict3d] method of @racket[pict3d-canvas%], and
@racket[pict3d->bitmap],
for how these parameters are used when rendering on a canvas or to a bitmap.

@deftogether[(@deftypedparam[current-pict3d-add-sunlight? add-sunlight? Boolean Boolean (#:value #t)]
              @deftypedparam[current-pict3d-add-indicators? add-indicators? Boolean Boolean
                                                            (#:value #t)]
              @deftypedparam[current-pict3d-add-grid? add-grid? Boolean Boolean (#:value #f)]
              @deftypedparam[current-pict3d-add-wireframe add-wireframe
                                                          (U #f 'color 'emitted)
                                                          (U #f 'color 'emitted)
                                                          (#:value #f)])]{
Determine whether directional lights, indicators, the grid, and wireframes are initially added to
interactive @racket[Pict3D] displays.
See @secref{quick}.
}

@deftypedparam[current-pict3d-auto-camera fun (-> Pict3D Affine) (-> Pict3D Affine)
                                          (#:value default-pict3d-auto-camera)]{
When a @racket[Pict3D] contains no @racket['camera] @tech{basis}, determines the initial view.
See @secref{quick}.
}

@deftogether[(@deftypedparam[current-pict3d-width width Integer Positive-Index
                                                  (#:value default-pict3d-width)]
              @deftypedparam[current-pict3d-height height Integer Positive-Index
                                                   (#:value default-pict3d-height)])]{
The width and height of new interactive @racket[Pict3D] displays, and the default size arguments to
@racket[pict3d->bitmap].
}

@deftypedparam[current-pict3d-background background RGBA RGBA (#:value default-pict3d-background)]{
The color of a rendered @racket[Pict3D] wherever shapes aren't drawn.

While @racket[pict3d-canvas%] respects this parameter, be aware that it draws @racket[Pict3D]
instances over a black background.
The alpha component of @racket[background] thus only appears to darken the background color.

It's not generally possible to combine lighting effects with a non-default background in a plausible
way.
@interaction[#:eval pict3d-eval
                    (eval:alts
                     (parameterize ([current-pict3d-background  (rgba "white" 0)])
                       (pict3d->bitmap
                        (combine (sphere origin 1/2)
                                 (light (pos 0 1 1) (emitted "blue" 5)))))
                     (parameterize ([current-pict3d-background  (rgba "white" 0)])
                       (pict3d->png-bitmap
                        (combine (sphere origin 1/2)
                                 (light (pos 0 1 1) (emitted "blue" 5))))))]
Light should always brighten, never darken, but the rendering engine is doing as well as it can.
In fact, the rendering is correct when the image is alpha-composited over a black background.
}

@deftypedparam[current-pict3d-ambient ambient Emitted Emitted (#:value default-pict3d-ambient)]{
The color and intensity of ambient light.
@examples[#:eval pict3d-eval
                 (define p
                   (combine (sphere origin 1/2)
                            (light (pos 0 1 1) (emitted "azure" 2))))
                 (pict3d->bitmap p)
                 (parameterize ([current-pict3d-ambient  (emitted "orange" 1)])
                   (pict3d->bitmap p))]
Generally, prefer to use other forms of lighting such as @racket[light] and @racket[sunlight] for
illumination.
Too much ambient light easily ``washes out'' or ``flattens'' a scene.
Use it subtly, for mood, or to suggest environment such as indoor or outdoor.
}

@deftypedparam[current-pict3d-fov degrees Positive-Real Positive-Flonum (#:value default-pict3d-fov)]{
The minimum field of view, in degrees.
@examples[#:eval pict3d-eval
                 (current-pict3d-fov 150)
                 (cube origin 1/2)
                 (current-pict3d-fov 60)
                 (cube origin 1/2)
                 (current-pict3d-fov 90)
                 (cube origin 1/2)]
The effects are best understood by flying around in an interactive display.
}

@deftogether[(@deftypedparam[current-pict3d-z-near z-near Positive-Real Positive-Flonum
                                                   (#:value default-pict3d-z-near)]
              @deftypedparam[current-pict3d-z-far z-far Positive-Real Positive-Flonum
                                                  (#:value default-pict3d-z-far)])]{
The distance to the near and far view clipping planes.
The defaults are sufficient for nearly every use, allowing both microscopic and planetary scales, and
are orders of magnitude smaller and larger than most 3D engines allow.

The only feasible reason to change these is to set the far plane as a speed optimization.
(Pict3D's rendering engine won't process anything for drawing that it can prove is outside the
@hyperlink["http://en.wikipedia.org/wiki/Viewing_frustum"]{view frustum}.)
}

@deftypedparam[current-pict3d-legacy? legacy? Boolean Boolean (#:value #f)]{
Determines the value sent to the @method[gl-config% set-legacy?] method of @racket[gl-config%] when
creating an OpenGL rendering context.

In general, you should only try setting @racket[(current-pict3d-legacy? #t)] if you're getting
strange OpenGL errors.

On Mac OS X, this parameter @emph{must} be @racket[#f].
If @racket[#t], only OpenGL 2.1 is available.
If @racket[#f], OpenGL 3.2 or higher is available.
Pict3D requires at least OpenGL 3.0.

On Linux, this parameter may be (or may have to be) either @racket[#t] or @racket[#f], depending on
the graphics driver.

On Windows, this parameter is currently ignored.
}

@deftypedparam[current-pict3d-check-version? check-version? Boolean Boolean (#:value #t)]{
Determines whether Pict3D checks the OpenGL version.

When @racket[current-pict3d-check-version?] is @racket[#t], Pict3D raises an error when the OpenGL
version is less than 3.0.
Otherwise, it doesn't check, and counts on hardware and drivers implementing equivalent extensions.

@bold{Use this at your own risk.}
Pict3D will work on standards-compliant implementations of OpenGL 3.0 or higher, but there is
no such guarantee for lower versions, even when they implement all the required extensions.
Further, if Pict3D works on a lower version, don't expect it to keep doing so.
In the future, Pict3D might use a 3.0 feature that your hardware or driver doesn't implement an
extension for.
If Pict3D doesn't work when @racket[current-pict3d-check-version?] is @racket[#f], it may raise
errors, render garbage, or cause segfaults.

Currently, @hyperlink["http://www.mesa3d.org/"]{Mesa 3D}'s software rendering implements all the
required extensions.
On Linux, if your graphics driver uses Mesa's infrastructure, you can use its software renderer by
starting DrRacket with

@tt{     LIBGL_ALWAYS_SOFTWARE=TRUE <racket-binary-path>/drracket}

and putting @racket[(current-pict3d-check-version? #f)] at the top of your programs.
}

@deftogether[(@defthing[default-pict3d-width Positive-Index #:value 256]
              @defthing[default-pict3d-height Positive-Index #:value 256]
              @defthing[default-pict3d-auto-camera (-> Pict3D Affine)]
              @defthing[default-pict3d-background RGBA #:value (rgba "black")]
              @defthing[default-pict3d-ambient Emitted #:value (emitted "white")]
              @defthing[default-pict3d-fov Positive-Flonum #:value 90.0]
              @defthing[default-pict3d-z-near Positive-Flonum #:value (expt 2.0 -20.0)]
              @defthing[default-pict3d-z-far Positive-Flonum #:value (expt 2.0 32.0)])]{
Nontrivial rendering parameter default values.
}

@subsection[#:tag "targets"]{Rendering Targets}

@defproc[(pict3d->bitmap [pict Pict3D]
                         [width Integer (current-pict3d-width)]
                         [height Integer (current-pict3d-height)])
         (Instance Bitmap%)]{
Renders a @racket[Pict3D] to a @racket[width]-by-@racket[height] bitmap.
The result depends on the values of the parameters
@racket[current-pict3d-auto-camera],
@racket[current-pict3d-background],
@racket[current-pict3d-ambient],
@racket[current-pict3d-z-near],
@racket[current-pict3d-z-far] and
@racket[current-pict3d-fov].
The value of @racket[current-pict3d-legacy?] determines what kind of OpenGL context to use.

Extra directional lights and indicators added to interactive displays are not drawn.
See @secref{quick}.
}

@defidform[#:kind "type" Pict3D-Canvas%]{
The type of @racket[pict3d-canvas%].

Instances of @racket[pict3d-canvas%] have type @racket[(Instance Pict3D-Canvas%)].
}

@defclass[pict3d-canvas% canvas% ()]{
Instances represent a window whose purpose is to render @racket[Pict3D] instances in an OpenGL
context.
Rendering on a @racket[pict3d-canvas%] is faster than rendering a bitmap using
@racket[pict3d->bitmap] and drawing the bitmap on a @racket[canvas%].

One of the smallest interesting programs that uses @racket[pict3d-canvas%] is this:
@racketmod[racket

(require pict3d
         racket/gui)

(define frame (new frame% [label "Test"] [width 800] [height 600]))
(define canvas (new pict3d-canvas%
                    [parent frame]
                    [pict3d (combine (sphere origin 1/2)
                                     (light (pos 0 1 1)))]))
(send frame show #t)]

In Typed Racket, remember to use @racket[(require typed/racket/gui)] instead of
@racket[(require racket/gui)].

To animate a canvas, simply use @method[pict3d-canvas% set-pict3d] to change the rendered
@racket[Pict3D].
Or use @racket[big-bang3d], which is intended for games and animations.

@defconstructor[([parent (U (Instance Frame%) (Instance Dialog%)
                            (Instance Panel%) (Instance Pane%))]
                 [style (Listof (U 'border 'control-border 'combo
                                   'vscroll 'hscroll 'resize-corner
                                   'gl 'no-autoclear 'transparent
                                   'no-focus 'deleted))
                        null]
                 [label (U #f String) #f]
                 [enabled Any #t]
                 [vert-margin Natural 0]
                 [horiz-margin Natural 0]
                 [min-width (U #f Natural) #f]
                 [min-height (U #f Natural) #f]
                 [stretchable-width Any #t]
                 [stretchable-height Any #t]
                 [pict3d Pict3D empty-pict3d])]{
All initialization fields are the same as for @racket[canvas%] except for the additional
@racket[pict3d] field.
Use this field to set the first @racket[Pict3D] that will be rendered.

The symbols @racket['gl] and @racket['no-autoclear] are prepended to @racket[style] before invoking
the @racket[canvas%] constructor.

The value of the parameter @racket[current-pict3d-legacy?] is used to determine what kind of OpenGL
context to create for the canvas.
The kind of OpenGL context cannot be changed after the canvas is created.

The values of the view parameters
@racket[current-pict3d-auto-camera],
@racket[current-pict3d-background],
@racket[current-pict3d-ambient],
@racket[current-pict3d-z-near],
@racket[current-pict3d-z-far] and
@racket[current-pict3d-fov]
are used to render @racket[pict3d].

Extra directional lights and indicators added to interactive displays are not drawn.
See @secref{quick}.
}

@defmethod[(set-pict3d [pict Pict3D]) Void]{
Sets the @racket[Pict3D] to be rendered.
The values of the view parameters
@racket[current-pict3d-auto-camera],
@racket[current-pict3d-background],
@racket[current-pict3d-ambient],
@racket[current-pict3d-z-near],
@racket[current-pict3d-z-far] and
@racket[current-pict3d-fov]
are used to render @racket[pict].

Extra directional lights and indicators added to interactive displays are not drawn.
See @secref{quick}.

If updates are synchronous (which is the default behavior), @method[pict3d-canvas% set-pict3d]
returns only after rendering @racket[pict].
Otherwise, it returns immediately.
See @method[pict3d-canvas% set-async-updates?].
}

@defmethod[(get-pict3d) Pict3D]{
Returns the last @racket[Pict3D] rendered.
}

@defmethod[(set-async-updates? [async? Boolean]) Void]{
Set the update mode.
The default value is @racket[#f].
See @method[pict3d-canvas% set-pict3d].
}
}

@;{===================================================================================================
   ===================================================================================================
   ===================================================================================================
   }

@section[#:tag "universe"]{3D Universe}

@defmodule[pict3d/universe]

@(define-runtime-path dun-dun-dun.mp3 "sounds/215558__divenorth__dun-dun-duuun-v-01.mp3")

This module provides functionality for creating interactive, graphical programs in the style of
@racketmodname[2htdp/universe]; i.e. by defining plain mathematical functions.
The biggest difference is that scenes are in THE THIRD DIMENSION!
@;{@hyperlink[dun-dun-dun.mp3]{THE THIRD DIMENSION}.}

Another difference is that @racket[big-bang3d], the analogue of @racket[big-bang] in
@racketmodname[2htdp/universe], is a plain Racket function with keyword arguments.

Besides minor stylistic differences, there are some significant interface differences.
Callback functions receive not only the current state, but a frame count and an inexact number of
milliseconds since the big bang.
There is no separate pad handler, no separate draw handler for the initial and final states, and no
separate frame limit.
On the whole, the interface is both a little more streamlined and a little more complicated.

What did you expect? You're doing 3D. You're playing with the big kids now.

One of the simplest @racketmodname[pict3d/universe] programs is
@racketmod[racket
(require pict3d
         pict3d/universe)
(big-bang3d 0)]

This opens a 512x512 window named @tt{World3D} that renders @racket[empty-pict3d] at 30 frames per
second, which responds to no events except those that close the window.
When the window closes, @racket[big-bang3d] returns @racket[0], which was given as the initial state.

A slightly more complicated program does the ol' spinning cube animation:
@racketmod[racket

           (require pict3d
                    pict3d/universe)
           
           (current-material (material #:ambient 0.01
                                       #:diffuse 0.39
                                       #:specular 0.6
                                       #:roughness 0.2))
           
           (define lights+camera
             (combine (light (pos 0 1 2) (emitted "Thistle"))
                      (light (pos 0 -1 -2) (emitted "PowderBlue"))
                      (basis 'camera (point-at (pos 1 1 0) origin))))
           
           (define (on-draw s n t)
             (combine (rotate-z (rotate-y (rotate-x (cube origin 1/2)
                                                    (/ t 11))
                                          (/ t 13))
                                (/ t 17))
                      lights+camera))
           
           (big-bang3d 0 #:on-draw on-draw)]

Press @tt{F12} at any time to dump the most amazing ``screenshots'' ever taken---the current
@racket[Pict3D]s---into DrRacket's REPL.

There is currently no support for networked games.

@defproc[(big-bang3d [init-state S]
                     [#:valid-state? valid-state? (-> S Natural Flonum Boolean) (λ (s n t) #t)]
                     [#:stop-state? stop-state? (-> S Natural Flonum Boolean) (λ (s n t) #f)]
                     [#:name name String "World3D"]
                     [#:width width Positive-Integer 512]
                     [#:height height Positive-Integer 512]
                     [#:frame-delay frame-delay Positive-Real (/ 1000 30)]
                     [#:on-frame on-frame (-> S Natural Flonum S) (λ (s n t) s)]
                     [#:on-key on-key (-> S Natural Flonum String S) (λ (s n t k) s)]
                     [#:on-release on-release (-> S Natural Flonum String S) (λ (s n t k) s)]
                     [#:on-mouse on-mouse (-> S Natural Flonum Integer Integer String S)
                                 (λ (s n t x y e) s)]
                     [#:on-draw on-draw (-> S Natural Flonum Pict3D) (λ (s n t) empty-pict3d)]
                     )
         S]{
Runs a ``3D world'' program.

On startup, @racket[big-bang3d] begins to keep track of
@itemlist[
 @item{The current state @racket[s : S], initially set to @racket[init-state].}
 @item{The frame number @racket[n : Natural], initially set to @racket[0].}
 @item{The time @racket[t : Flonum], in milliseconds, initially set to @racket[0.0].}
]
All callback functions---@racket[valid-state?], @racket[stop-state?], @racket[on-frame],
@racket[on-key], @racket[on-release], @racket[on-mouse] and @racket[on-draw]---receive the current
values of @racket[s], @racket[n] and @racket[t].

There are two phases in running a 3D world program: initialization and frame loop.
In the initialization phase, @racket[big-bang3d] does the following once.
@itemlist[#:style 'ordered
 @item{Computes @racket[(valid-state? s n t)]; if @racket[#f], raises an error.}
 @item{Computes @racket[(stop-state? s n t)].}
 @item{Creates a window with title @racket[name] and a @racket[width]-by-@racket[height] client
       area. The window contains only a @racket[pict3d-canvas%] with @racket[(on-draw s n t)] as
       its initial @racket[Pict3D].}
 @item{Waits one second for the GUI machinery to start up.}
 @item{Synchronizes on a signal that the window has painted itself for the first time.}
]
If @racket[(stop-state? s n t)] returned @racket[#f], @racket[big-bang3d] enters the frame loop phase.
In the frame loop phase, @racket[big-bang3d] repeats the following.
@itemlist[#:style 'ordered
 @item{Updates @racket[n] to @racket[(+ n 1)].}
 @item{Updates @racket[t] to the numer of milliseconds since @racket[big-bang3d] was called.}
 @item{Computes @racket[(on-frame s n t)] to yield a new state @racket[s].}
 @item{Handles all accumulated keyboard and mouse events, which also update @racket[s].}
 @item{Computes @racket[(on-draw s n t)] to yield a new @racket[Pict3D], which it sets in the canvas.}
 @item{Computes the number of milliseconds @racket[ms] until @racket[frame-delay] milliseconds will
       have passed since the start of the frame, and sleeps for @racket[(max 1.0 ms)] milliseconds.}
]
Sleeping for at least one millisecond gives the canvas time to receive GUI events when a frame
loop iteration takes longer than @racket[frame-delay] milliseconds.

Key presses are handled by computing @racket[(on-key s n t k)] to yield a new state @racket[s].
Key releases are handled by computing @racket[(on-release s n t k)] to yield a new state @racket[s].
In both cases, the key event code @racket[k] is the value returned by @method[key-event% get-key-code]
or @method[key-event% get-key-release-code], converted to a string.

Mouse events are handled by computing @racket[(on-mouse s n t x y e)] to yield a new state @racket[s].
The values @racket[x] and @racket[y] are the mouse cursor's event position, and @racket[e] is a string
that indicates what kind of event occurred.
Values for @racket[e] are the symbols returned by the method @method[mouse-event% get-event-type]
of @racket[mouse-event%], converted to strings, unless the symbol is @racket['motion].
In that case, @racket[e] is @racket["drag"] if a mouse button is pressed; otherwise @racket["move"].

After every state update, the frame loop
@itemlist[#:style 'ordered
 @item{Checks @racket[(valid-state? s n t)]; if @racket[#f], raises an error.}
 @item{Checks @racket[(stop-state? s n t)]; if @racket[#t], flags the frame loop as being on its last
       iteration.}
]
If @racket[stop-state?] ever returns @racket[#t] or the window is closed, the frame loop exits after
completing its current iteration, and @racket[big-bang3d] returns @racket[s].

The callbacks @racket[valid-state?], @racket[stop-state?] and @racket[on-draw] can determine whether
they're being used during the initialization phase by checking @racket[(zero? n)].
}

@(close-evals)
