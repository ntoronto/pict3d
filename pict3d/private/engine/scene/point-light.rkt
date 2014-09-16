#lang typed/racket/base

;; Point light impostors: vertex shader outputs a quad; fragment shader computes light

(require racket/match
         racket/list
         racket/flonum
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         "../../math/flv3.rkt"
         "../../math/flt3.rkt"
         "../../math/flrect3.rkt"
         "../draw-pass.rkt"
         "../gl.rkt"
         "../shader-lib.rkt"
         "../types.rkt"
         "../utils.rkt"
         "types.rkt"
         "shader-lib.rkt")

(provide make-point-light-shape
         make-point-light-shape-passes
         point-light-shape-rect
         point-light-shape-transform
         )

;; ===================================================================================================
;; Constructor

(: make-point-light-shape (-> FlVector Flonum FlVector Flonum point-light-shape))
(define (make-point-light-shape color intensity position radius)
  (cond [(not (= 3 (flvector-length color)))
         (raise-argument-error 'make-point-light-shape "length-3 flvector"
                               0 color intensity position radius)]
        [(not (= 3 (flvector-length position)))
         (raise-argument-error 'make-point-light-shape "length-3 flvector"
                               2 color intensity position radius)]
        [else
         (point-light-shape (box 'lazy) color intensity position radius)]))

;; ===================================================================================================
;; Program for pass 0: light

(define point-light-vertex-code
  (string-append
   "#version 130\n\n"
   output-impostor-quad-vertex-code
   model-vertex-code
   #<<code
uniform mat4 view;
uniform mat4 proj;

in vec3 vert_position;
in vec2 vert_intensity_radius;
in vec3 vert_color;

flat out vec3 frag_position;
flat out float frag_radius;
flat out vec3 frag_intensity;
smooth out float frag_is_degenerate;

void main() {
  mat4x3 model = get_model_transform();
  mat4 trans = view * a2p(model);

  float radius = vert_intensity_radius.y;
  vec3 wmin = vert_position - vec3(radius);
  vec3 wmax = vert_position + vec3(radius);
  frag_position = (trans * vec4(vert_position,1)).xyz;
  frag_radius = radius;
  frag_intensity = pow(vert_color / 255, vec3(2.2)) * vert_intensity_radius.x;
  frag_is_degenerate = output_impostor_quad(trans, proj, wmin, wmax);
}
code
   ))

(define point-light-fragment-code
  (string-append
   "#version 130\n\n"
   light-fragment-code
   #<<code
uniform int width;
uniform int height;
uniform mat4 unproj;

uniform sampler2D depth;
uniform sampler2D material;

flat in vec3 frag_position;
flat in float frag_radius;
flat in vec3 frag_intensity;
smooth in float frag_is_degenerate;

void main() {
  // all fragments should discard if this one does
  if (frag_is_degenerate > 0.0) discard;

  vec3 vpos = frag_coord_to_position(gl_FragCoord, depth, unproj, width, height);
  vec3 D = frag_position - vpos;
  float dist = length(D);
  if (dist > frag_radius) discard;  
  vec3 L = normalize(D);
  vec3 V = normalize(-vpos);

  vec3 light = attenuate_invsqr(frag_intensity, dist);
  //vec3 light = attenuate_linear(frag_intensity, frag_radius, dist);
  output_light(light, get_surface(material), L, V);
}
code
   ))

(define-singleton (point-light-program-spec)
  (define struct
    (make-vao-struct
     (make-vao-field "vert_position" 3 GL_FLOAT)
     (make-vao-field "vert_intensity_radius" 2 GL_FLOAT)
     (make-vao-field "vert_color" 3 GL_UNSIGNED_BYTE)))
  
  (define program
    (make-gl-program struct
                     (list (make-gl-shader GL_VERTEX_SHADER point-light-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER point-light-fragment-code))))
  
  (define uniforms
    (list (cons "view" 'view)
          (cons "proj" 'proj)
          (cons "width" 'width)
          (cons "height" 'height)
          (cons "unproj" 'unproj)
          (cons "depth" 'depth)
          (cons "material" 'material)))
  
  (program-spec program uniforms struct))

;; ===================================================================================================
;; Point light shape passes

(: make-point-light-shape-passes (-> point-light-shape Passes))
(define (make-point-light-shape-passes a)
  (match-define (point-light-shape _ color intensity position radius) a)
  
  (: datum GL-Data)
  (define datum
    (gl-data->bytes
     (list (flvector->f32vector position)
           (f32vector intensity radius)
           (pack-color color))))
  
  (define data (gl-data->bytes ((inst make-list GL-Data) 4 datum)))
  
  (: passes Passes)
  (define passes
    (vector
     (vector (shape-params point-light-program-spec empty 'both GL_QUADS (single-vertices 4 data)))
     #()
     #()
     #()
     #()))
  passes)

;; ===================================================================================================
;; Bounding box

(: point-light-shape-rect (-> point-light-shape Nonempty-FlRect3))
(define (point-light-shape-rect a)
  (define p (point-light-shape-position a))
  (define radius (point-light-shape-radius a))
  (define r (flvector radius radius radius))
  (nonempty-flrect3 (flv3- p r) (flv3+ p r)))

;; ===================================================================================================
;; Transform

(: point-light-shape-transform (-> point-light-shape FlAffine3- FlAffine3- (List point-light-shape)))
(define (point-light-shape-transform a t tinv)
  (match-define (point-light-shape passes color intensity position radius) a)
  (list (point-light-shape (box 'lazy) color intensity (flt3apply/pos t position) radius)))
