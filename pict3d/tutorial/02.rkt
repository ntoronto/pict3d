#lang racket

(require pict3d
         "utils.rkt")

(disclaimer)
(header "02: Colors")

(display "
In this tutorial, as in the last, we'll disable the extra directional lights.
")
(example (current-pict3d-add-sunlight? #f))
(press-enter)

(display "
Another rule of thumb, besides using many lights instead of one big bright one,
is to use colored *objects* instead of colored *lights* to add color. Colored
lights often end up making a scene feel 'flat' because their color channels are
multiplied with each object's color channels, and colored lights tend to have
channels whose values are close to zero.

As an extreme example, a solid red light does nothing to illuminate a solid
green sphere, because the red light's green color channel is 0.
")
(example
 (combine (with-color (rgba "green")
            (sphere origin 1/2))
          (light (pos 1/2 1/2 1) (emitted "red"))))
(press-enter)

(display "
A Pict3D is created using the current color, which by default is
(rgba \"white\"). To change the current color in the dynamic extent of an
expression, use 'with-color', as above, or set the parameter 'current-color'.

But 'with-color' won't update the color of an already-created Pict3D. For that,
use 'set-color'. In the following, only the sphere on the right, whose color is
updated to (rgba \"green\") using 'set-color', is actually updated from red to
green.
")
(example
 (combine (sunlight (dir 0 0 -1))
          (with-color (rgba "green")
            (with-color (rgba "red")
              (sphere (pos 1 0 0) 1/2)))
          (set-color
           (with-color (rgba "red")
             (sphere (pos 0 1 0) 1/2))
           (rgba "green"))))
(press-enter)

(display "
For 'cold' lights, use very light blue colors like \"azure\". For 'warm' lights,
use very light orange colors like \"oldlace\". ('Azure and Old Lace' sounds like
the title of a famous play, 'Arsenic and Old Lace.' Do yourself a favor and
watch the Cary Grant movie version of it.)
")
(example
 (combine (sphere origin 1/2)
          (light (pos 1 -1/2 1) (emitted "azure" 2))
          (light (pos -1/2 1 1) (emitted "oldlace" 2))))
(press-enter)

(display "
A Pict3D object can emit light as well as reflect light. This emitted light only
self-illuminates; i.e. it doesn't illuminate any objects around it. It's
therefore often useful to combine light-emitting objects with one or more
low-intensity lights.
")
(example
 (combine (sphere origin 1/2)
          (with-emitted (emitted "oldlace" 4)
            (sphere (pos 0 3/4 3/4) 0.025))
          (light (pos 0 3/4 3/4) (emitted "oldlace" 1))))
(display "
Just as 'with-color' has a corresponding function 'set-color' to update an
existing Pict3D, 'with-emitted' has a corresponding function 'set-emitted'.
")

(header "End 02: Colors")
(press-enter "")

(current-pict3d-add-sunlight? #t)
