#lang typed/racket/base

;; Directional light impostors: vertex shader outputs a quad; fragment shader computes light

(require racket/match
         racket/flonum
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         "../../math/flt3.rkt"
         "../../math/flrect3.rkt"
         "../gl.rkt"
         "../types.rkt"
         "../utils.rkt"
         "../shader-lib.rkt"
         "../draw-pass.rkt"
         "types.rkt"
         "shader-lib.rkt")

(provide make-directional-light-shape
         make-directional-light-shape-passes
         directional-light-shape-rect
         directional-light-shape-transform
         )

;; ===================================================================================================
;; Constructors

(: make-directional-light-shape (-> FlVector FlVector directional-light-shape))
(define (make-directional-light-shape intensity direction)
  (directional-light-shape (box 'lazy) intensity direction))

;; ===================================================================================================
;; Program for pass 0: light

(define directional-light-vertex-code
  (string-append
   #<<code
#version 130

void main() {
    // output the right vertices for a triangle strip
  switch (gl_VertexID) {
  case 0:
    gl_Position = vec4(-1.0, -1.0, 0.0, 1.0);
    break;
  case 1:
    gl_Position = vec4(+1.0, -1.0, 0.0, 1.0);
    break;
  case 2:
    gl_Position = vec4(-1.0, +1.0, 0.0, 1.0);
    break;
  default:
    gl_Position = vec4(+1.0, +1.0, 0.0, 1.0);
    break;
  }
}
code
   ))

(define directional-light-fragment-code
  (string-append
   "#version 130\n\n"
   get-view-position-fragment-code
   light-fragment-code
   #<<code
uniform mat4 unview;
uniform mat3 unproj0;
uniform mat3 unproj1;
uniform float znear;
uniform float zfar;
uniform int width;
uniform int height;

uniform sampler2D depth;
uniform sampler2D material;

// Per-light attributes
uniform vec3 light_dir;
uniform vec3 light_color;

void main() {
  vec3 pos = get_view_position(depth, width, height, unproj0, unproj1, znear, zfar);
  vec3 L = normalize(-light_dir * mat3(unview));
  vec3 V = normalize(-pos);
  output_light(light_color, get_surface(material), L, V);
}
code
   ))

(define-singleton (directional-light-program-spec)
  (define struct (make-vao-struct))
  
  (define program
    (make-gl-program struct
                     (list (make-gl-shader GL_VERTEX_SHADER directional-light-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER directional-light-fragment-code))))
  
  (define uniforms
    (list (cons "unview" 'unview)
          (cons "unproj0" 'unproj0)
          (cons "unproj1" 'unproj1)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)
          (cons "width" 'width)
          (cons "height" 'height)
          (cons "depth" 'depth)
          (cons "material" 'material)))
  
  (program-spec program uniforms struct))

;; ===================================================================================================
;; Directional light shape passes

(: make-directional-light-shape-passes (-> directional-light-shape Passes))
(define (make-directional-light-shape-passes a)
  (match-define (directional-light-shape _ intensity direction) a)
  
  (define uniforms
    (list (cons "light_dir" (uniform-float direction 3))
          (cons "light_color" (uniform-float intensity 3))))
  
  (: passes Passes)
  (define passes
    (vector
     (vector (shape-params directional-light-program-spec uniforms 'both GL_TRIANGLE_STRIP
                           (multi-vertices 4 #"" (vector 0) (s32vector 4))))
     #()
     #()
     #()
     #()))
  passes)

;; ===================================================================================================
;; Bounding box

(define directional-light-shape-rect
  (nonempty-flrect3 (flvector -inf.0 -inf.0 -inf.0)
                    (flvector +inf.0 +inf.0 +inf.0)))

;; ===================================================================================================
;; Transform

(: directional-light-shape-transform (-> directional-light-shape FlAffine3- FlAffine3-
                                         (List directional-light-shape )))
(define (directional-light-shape-transform a t tinv)
  (list a))
