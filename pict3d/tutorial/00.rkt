#lang racket

(require racket/flonum
         "utils.rkt")

(disclaimer)
(header "00: Basics, illustrated with spheres")

(display "
A 3-dimensional scene, or Pict3D, displays in DrRacket's REPL as an interactive
widget. This, for example, displays a sphere at the origin with radius 1/2:
")
(example
 (require pict3d)
 (sphere origin 1/2))
(display "
Click on it, then use the mouse to look around. Clicking again gives control of
the mouse and keyboard back to DrRacket. Use

  * W and S to move forward and backward
  * A and D to move left and right
  * R and F to move up and down

If you're familiar with 3D shooters, this should feel natural.

When you're ready for the next example, click in the following box and press
Enter.
")

(press-enter)

(display "
Pict3D objecs can be combined in many different ways to create new Pict3Ds.
The most basic Pict3D combinator is 'combine', which merges the shapes in one
or more Pict3Ds.
")
(example
 (combine (sphere origin 1/2)
          (sphere (pos 1/2 0 0) 1/3)))
(press-enter)

(display "
One way to make squashed spheres is to use 'ellipsoid', which accepts two
position vectors. These vectors are regarded as opposite corners of a box that
the ellipsoid will fill.
")
(example
 (combine (ellipsoid (pos 0 0 0) (pos 1 1/2 1/4))
          (ellipsoid (pos 0 0 1/4) (pos 1 1/2 1/2))))
(press-enter)

(display "
Like 'append*', 'combine*' combines a list:
")
(example
 (combine*
  (for/list ([z  (in-range 0 2 0.5)]
             [r  (in-range 1 0 -0.25)])
    (sphere (pos 0 0 z) r))))
(header "End 00: Basics")
(press-enter)
