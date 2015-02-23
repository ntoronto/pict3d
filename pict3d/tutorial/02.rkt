#lang racket

(require pict3d
         "utils.rkt")

(disclaimer)
(header "02: Colors")

(display "
Another rule of thumb, besides using many lights instead of one big bright one,
is to use colored *objects* instead of colored *lights* to add color. Highly
saturated, colored lights often do nothing to add vibrance to a scene because
they multiply their color channels against object color channels.

For example, a solid red light does nothing to illuminate a solid blue sphere,
because the red light's blue color channel is 0.
")
(example
 (pict3d->bitmap
  (combine (with-color "blue"
             (sphere '(0 0 0) 1/2))
           (light '(1/2 1/2 1) "red"))
  256 256))
(press-enter)

(display "
A Pict3D is created using the current color, which by default is \"white\". To
change the current color in the dynamic extent of an expression, use
'with-color', as above, or use (parameterize ([current-color ...]) ...).

But 'with-color' won't update the color of an already-created Pict3D. For that,
use 'set-color'. In the following, only the sphere on the right, whose color is
updated to \"blue\" using 'set-color', is actually blue:
")
(example
 (combine (with-color "blue"
            (with-color "turquoise"
              (sphere '(1 0 0) 1/2)))
          (set-color
           (with-color "turquoise"
             (sphere '(0 1 0) 1/2))
           "blue")))
(press-enter)

(display "
For 'cold' lights, use very light blue colors like \"azure\". For 'warm' lights,
use very light orange colors like \"oldlace\". ('Azure and Old Lace' sounds like
the title of a famous play, 'Arsenic and Old Lace.' Do yourself a favor and
watch the Cary Grant movie version of it.)
")
(example
 (pict3d->bitmap
  (combine (sphere '(0 0 0) 1/2)
           (light '(1 -1/2 1) "azure" 2)
           (light '(-1/2 1 1) "oldlace" 2))
  256 256))
(press-enter)

(display "
A Pict3D object can also *emit* light as well as reflect light. This emitted
light only self-illuminates; i.e. it doesn't illuminate any objects around it.
It's therefore often useful to combine light-emitting objects with one or more
low-intensity lights.
")
(example
 (pict3d->bitmap
  (combine (sphere '(0 0 0) 1/2)
           (with-emitted (intensity "oldlace" 3)
             (sphere '(1/2 1 1) 0.01))
           (light '(1/2 1 1) "oldlace" 1))
  256 256))
(display "
An emitted color has four components: red, green, blue, and intensity. It's
often convenient to increase the intensity of a named color using 'intensity',
as above.

Just as 'with-color' has a corresponding function 'set-color' to update an
existing Pict3D, 'with-emitted' has a corresponding function 'set-emitted'.
")

(header "End 02: Colors")
(press-enter)
