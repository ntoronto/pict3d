#lang racket

(require pict3d
         "utils.rkt")

(disclaimer)
(header "01: Lights")

(display
"
When displayed in DrRacket's REPL, a Pict3D has three things added to it:

 * A bright 'directional light' shining diagonally downward
 * A less bright directional light shining diagonally upward
 * A representation of the unit axes at the origin '(0 0 0)

The axes help you get your bearings. The lights help you see the shapes.

When Pict3Ds are used in games or to make bitmaps, these extra objects aren't
added to the scene.
")
(example
 (pict3d->bitmap
  (sphere origin 1/2)
  256 256))
(press-enter)

(display "
The sphere is only visible because it reflects 'ambient' light, which is part
of its material specification. (We'll get to materials later.) To see the
sphere better, let's add a 'point light' just above it and toward the camera.
")
(example
 (pict3d->bitmap
  (combine (sphere origin 1/2)
           (light (pos 1/2 1/2 1)))
  256 256))
(press-enter)

(display "
A point light is also a Pict3D, but it's invisible. You can't tell it's there
unless it illuminates another object.

The 'light' function accepts an optional argument for an emitted color, which
is a color that includes an extra intensity.
")
(example
 (pict3d->bitmap
  (combine (sphere origin 1/2)
           (light (pos 1/2 1/2 1) (emitted "violet" 10)))
  256 256))
(press-enter)

(display "
That's quite a bright light. But you might need one that bright, or brighter,
to illuminate a larger scene on its own.
")
(example
 (define spheres
   (combine*
    (for*/list ([x  (in-range -5 5 1)]
                [y  (in-range -5 5 1)]
                [z  (in-range -5 5 1)]
                #:when (even? (+ x y z)))
      (sphere (pos x y z) 1/4))))
 
 (pict3d->bitmap
  (combine spheres
           (light origin (emitted "violet" 50)))
  256 256))
(press-enter)

(display "
But a good rule of thumb is to use mostly local lights instead of one big,
global one.
")
(example
 (define lights
   (combine*
    (for*/list ([_  (in-range 50)])
      (define v (pos (- (* 10 (random)) 5)
                     (- (* 10 (random)) 5)
                     (- (* 10 (random)) 5)))
      (light v (emitted "violet" 1)))))
 
 (pict3d->bitmap (combine spheres lights) 256 256))
(press-enter)

(display "
You might have heard before that lights are expensive to render. That's not
true in Pict3D, which, like modern video games, uses the programmable features
of today's video cards to illuminate objects. Each light is no more expensive
to render than a sphere that's large enough to touch everything the light
illuminates.

When Pict3D is extended with lights that cast shadows, those lights may be
fairly expensive, but rendering them should take time logarithmic in the number
of objects in the scene.
")
(press-enter)

(display "
Directional lights, like those added to Pict3Ds before rendering in DrRacket's
REPL, can be added using the 'sunlight' function, which takes a *direction*
vector and optional color and intensity arguments.
")
(example
 (pict3d->bitmap
  (combine spheres
           (sunlight (dir 0.5 -0.5 -1) (emitted "chocolate" 2)))
  256 256))
(display "
Remember that if you want downward directional light, the z component of the
direction vector must be *negative*.
")

(header "End 01: Lights")
(press-enter)
