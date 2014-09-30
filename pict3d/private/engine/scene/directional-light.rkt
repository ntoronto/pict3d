#lang typed/racket/base

;; Directional light impostors: vertex shader outputs a quad; fragment shader computes light

(require racket/match
         racket/flonum
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         "../../math/flt3.rkt"
         "../../math/flrect3.rkt"
         "../../gl.rkt"
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

(: make-directional-light-shape (-> FlVector Flonum FlVector directional-light-shape))
(define (make-directional-light-shape color intensity direction)
  (cond [(not (= 3 (flvector-length color)))
         (raise-argument-error 'make-directional-light-shape "length-3 flvector"
                               0 color intensity direction)]
        [(not (= 3 (flvector-length direction)))
         (raise-argument-error 'make-directional-light-shape "length-3 flvector"
                               2 color intensity direction)]
        [else
         (directional-light-shape (box 'lazy) color intensity direction)]))

;; ===================================================================================================
;; Program for pass 0: light

(define directional-light-vertex-code
  (string-append
   #<<code
#version 130

uniform mat4 unproj;

smooth out vec3 frag_dir;

void main() {
    // output the right vertices for a triangle strip
  switch (gl_VertexID % 4) {
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

  vec4 dir = unproj * gl_Position;
  frag_dir = vec3(dir.xy / dir.z, 1.0);
}
code
   ))

(define directional-light-fragment-code
  (string-append
   "#version 130\n\n"
   light-fragment-code
   #<<code
uniform mat4 unview;

uniform sampler2D depth;
uniform sampler2D material;

// Per-light attributes
uniform vec3 light_dir;
uniform vec3 light_color;
uniform float light_intensity;

smooth in vec3 frag_dir;

void main() {
  float d = texelFetch(depth, ivec2(gl_FragCoord.xy), 0).r;
  if (d == 0.0) discard;
  float z = get_view_depth(d);
  vec3 pos = frag_dir * z;

  vec3 L = normalize(-light_dir * mat3(unview));
  vec3 V = normalize(-pos);
  output_light(pow(light_color, vec3(2.2)) * light_intensity, get_surface(material), L, V);
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
          (cons "unproj" 'unproj)
          (cons "depth" 'depth)
          (cons "material" 'material)))
  
  (program-spec program uniforms))

;; ===================================================================================================
;; Directional light shape passes

(: make-directional-light-shape-passes (-> directional-light-shape Passes))
(define (make-directional-light-shape-passes a)
  (match-define (directional-light-shape _ color intensity direction) a)
  
  (define uniforms
    (list (cons "light_dir" (uniform-float direction 3))
          (cons "light_color" (uniform-float color 3))
          (cons "light_intensity" (uniform-float intensity 1))))
  
  (: passes Passes)
  (define passes
    (vector
     (vector (shape-params directional-light-program-spec uniforms #t GL_TRIANGLE_STRIP
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

(: directional-light-shape-transform (-> directional-light-shape FlAffine3-
                                         (List directional-light-shape)))
(define (directional-light-shape-transform a t)
  (list a))
