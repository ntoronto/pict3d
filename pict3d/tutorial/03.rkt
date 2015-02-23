#lang racket

(require pict3d
         "utils.rkt")

(disclaimer)
(header "03: Materials")

(display "
By default, Pict3Ds look like they're made of rubber. This aspect of a Pict3D's
appearance is controlled by its 'material.'

A material is (currently) comprised of four quantities, all in the range [0,1]:

 * Ambient reflectance
 * Diffuse reflectance
 * Specular reflectance
 * Roughness

For realistic-looking materials, the first three should sum to 1. Roughness
affects only specular reflectance.

The best way to communicate what these quantities mean is by shared experience,
so here's an interactive scene. Click on it, and fly around to take a look at
the objects at different angles. Remember that interactive scenes have two
white directional lights added: one underneath, and one above. All three of the
spheres are white.
")
(example
 (combine
  (with-material (material #:ambient  1.0) (sphere '(1 0 0) 1/2))
  (with-material (material #:diffuse  1.0) (sphere '(0 1 0) 1/2))
  (with-material (material #:specular 1.0) (sphere '(0 0 1) 1/2))))
(display "
The x-axis sphere has only ambient reflectance. It reflects an imaginary white
light in all directions with the same intensity.

The y-axis sphere has only diffuse reflectance. It looks dull; not shiny at all.
(For the curious: it's called a 'Lambertian surface' because it follows
Lambert's cosine law.)

The z-axis sphere has only specular reflectance. It looks *only* shiny. (Also
for the curious: this uses Ward's specular reflection model, limited to
isotropic reflection. It may be extended in the future to use the full Ward
model with anisotropic reflection, to model materials like brushed metal.)
")
(press-enter)

(display "
Here are more spheres with only specular reflectance, but different roughness.
Generally, the less rough the sphere, the shinier it looks.
")
(example
 (combine
  (with-material (material #:specular 1.0 #:roughness 0.1)
    (sphere '(1 0 0) 1/2))
  (with-material (material #:specular 1.0 #:roughness 0.2)
    (sphere '(0 1 0) 1/2))
  (with-material (material #:specular 1.0 #:roughness 0.4)
    (sphere '(0 0 1) 1/2))))
(press-enter)

(display "
The combination of these parameters creates distinctive-looking materials.
")
(example
 (define metal (material #:ambient 0.2 #:diffuse 0.2 #:specular 0.6 #:roughness 0.1))
 (define rubber default-material)
 (define plastic (material #:ambient 0.1 #:diffuse 0.6 #:specular 0.3 #:roughness 0.2))
 (combine
  (with-material metal   (sphere '(1 0 0) 1/2))
  (with-material rubber  (sphere '(0 1 0) 1/2))
  (with-material plastic (sphere '(0 0 1) 1/2))))

(header "End 03: Materials")
(press-enter)
