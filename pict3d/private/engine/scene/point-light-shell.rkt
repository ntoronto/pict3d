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
         "../../gl.rkt"
         "../../utils.rkt"
         "../draw-pass.rkt"
         "../shader-lib.rkt"
         "../types.rkt"
         "../utils.rkt"
         "types.rkt"
         "flags.rkt"
         "shader-lib.rkt")

(provide make-point-light-shell-shape
         make-point-light-shell-shape-passes
         point-light-shell-shape-rect
         point-light-shell-shape-easy-transform
         )

;; ===================================================================================================
;; Constructor

(: make-point-light-shell-shape (-> FlVector FlVector Flonum Flonum point-light-shell-shape))
(define (make-point-light-shell-shape e v r0 r1)
  (cond [(not (= 4 (flvector-length e)))
         (raise-argument-error 'make-point-light-shell-shape "length-4 flvector"
                               0 e v r0 r1)]
        [(not (= 3 (flvector-length v)))
         (raise-argument-error 'make-point-light-shell-shape "length-3 flvector"
                               1 e v r0 r1)]
        [else
         (define fs (flags-join invisible-flag transparent-flag (color-emitting-flag e)))
         (point-light-shell-shape (lazy-passes) fs e v r0 r1)]))

;; ===================================================================================================
;; Program for pass 0: light

(define point-light-shell-vertex-code
  (string-append
   output-impostor-strip-vertex-code
   model-vertex-code
   #<<code
uniform mat4 view;
uniform mat4 proj;
uniform mat4 unproj;

in vec3 vert_position;
in vec3 vert_intensity_radii;
in vec3 vert_color;
in float vert_id;

flat out vec3 frag_position;
flat out float frag_min_radius;
flat out float frag_max_radius;
flat out vec3 frag_color;
flat out float frag_intensity;
smooth out float frag_is_degenerate;
smooth out vec3 frag_dir;

void main() {
  mat4x3 model = get_model_transform();
  mat4 trans = view * a2p(model);

  float intensity = vert_intensity_radii.x;
  float min_radius = vert_intensity_radii.y;
  float max_radius = vert_intensity_radii.z;
  vec3 wmin = vert_position - vec3(max_radius);
  vec3 wmax = vert_position + vec3(max_radius);
  frag_position = (trans * vec4(vert_position,1)).xyz;
  frag_min_radius = min_radius;
  frag_max_radius = max_radius;
  frag_color = pow(vert_color / 255, vec3(2.2)); // * intensity;
  frag_intensity = intensity;
  frag_is_degenerate = output_impostor_strip(trans, proj, wmin, wmax, int(vert_id));

  vec4 dir = unproj * gl_Position;
  frag_dir = vec3(dir.xy / dir.z, 1.0);
}
code
   ))

(define point-light-shell-fragment-code
  (string-append
   light-fragment-code
   #<<code
uniform mat4 unview;
uniform sampler2D depth;
uniform sampler2D material;
uniform int width;
uniform int height;

flat in vec3 frag_position;
flat in float frag_min_radius;
flat in float frag_max_radius;
flat in vec3 frag_color;
flat in float frag_intensity;
smooth in float frag_is_degenerate;
smooth in vec3 frag_dir;

void radius_bounds(float r, float h, float s, out float r1, out float r2) {
  float c = s*r*h;
  float rph = r+h;
  float rmh = r-h;
  r1 = sqrt(rph*rph + c);
  r2 = sqrt(rmh*rmh - c);
}

float dash_multiplier(vec3 T, vec3 P, float h) {
  float x = abs(T.x) > abs(T.y) ? P.x : P.y;
  return mod(floor(x/h), 2.0);
}

void main() {
  // all fragments should discard if this one does
  if (frag_is_degenerate > 0.0) discard;

  float d = texelFetch(depth, ivec2(gl_FragCoord.xy), 0).r;
  if (d == 0.0) discard;
  float z = get_view_depth(d);
  vec3 vpos = frag_dir * z;

  vec3 D = frag_position - vpos;
  float dist = length(D);
  if (dist < frag_min_radius) discard;
  if (dist > frag_max_radius) discard;

  vec3 N = get_surface(material).normal;
  vec3 L = D/dist;
  float cs = dot(L,N);
  
  // cosine of angle between surface and L
  float sn = sqrt(1.0-cs*cs);
  // some other arbitrary value needed for computing radius bounds
  float s = 2.0*(sn-1.0);
  // max distance on surface from line
  float h = abs(z) * 2.0 / float(max(width,height));

  // Multiplier for dashed lines on back faces
  float c = 1.0/128.0;
  float b = c+c*c;
  float a = -c-b/(cs-c);
  float dash = cs <= 0.0 ? mix(dash_multiplier(cross(N,L), vpos, h*4.0), 1.0, a) : 1.0;

  float e = 0.05;
  for (int j = 0; j < 4; j++) {
    // compute radius bounds
    float r = sqrt(frag_intensity/e);
    float r1, r2;
    radius_bounds(r, h, s, r1, r2);
    float max_delta = abs(0.5*(r1-r2));
    float delta = abs(dist - 0.5*(r1+r2));
    if (delta <= max_delta) {
      float aa = 1.0 - delta / max_delta;  // linear falloff to simulate antialiasing
      out_diffuse = vec4(frag_color * e * aa * dash, 1.0);
      out_specular = vec4(frag_color * e * aa * dash, 1.0);
      return;
    } else {
      e = e * 2.0;
    }
  }

  discard;
}
code
   ))

(define-singleton/context (point-light-shell-program)
  (log-pict3d-info "<engine> creating point light shell program")
  (make-gl-program
   "point-light-shell-program"
   (list (cons "view" 'view)
         (cons "unview" 'unview)
         (cons "proj" 'proj)
         (cons "unproj" 'unproj)
         (cons "depth" 'depth)
         (cons "material" 'material)
         (cons "width" 'width)
         (cons "height" 'height))
   (make-vao-struct
    (make-vao-field "vert_position" 3 GL_FLOAT)
    (make-vao-field "vert_intensity_radii" 3 GL_FLOAT)
    (make-vao-field "vert_color" 3 GL_UNSIGNED_BYTE)
    (make-vao-field "vert_id" 1 GL_UNSIGNED_BYTE))
   (list "out_diffuse" "out_specular")
   (list (make-gl-shader GL_VERTEX_SHADER point-light-shell-vertex-code)
         (make-gl-shader GL_FRAGMENT_SHADER point-light-shell-fragment-code))))

;; ===================================================================================================
;; Point light shape passes

(: vertex-ids (Vectorof Index))
(define vertex-ids #(0 1 2 2 1 3))

(: make-point-light-shell-shape-passes (-> point-light-shell-shape passes))
(define (make-point-light-shell-shape-passes a)
  (match-define (point-light-shell-shape _ fs e v r0 r1) a)
  
  (cond
    [(flags-subset? emitting-flag fs)
     (define color (flvector-copy e 0 3))
     (define intensity (flvector-ref e 3))
     (define data
       (gl-data->bytes
        ((inst append* (U F32Vector Bytes))
         (for/list ([id  (in-range 4)])
           (list (flvector->f32vector v)
                 (f32vector intensity r0 r1)
                 (pack-color color)
                 (bytes id))))))
     
     (passes
      (vector (shape-params point-light-shell-program
                            empty #t GL_TRIANGLES (vertices 4 data vertex-ids)))
      #()
      #()
      #()
      #())]
    [else  empty-passes]))

;; ===================================================================================================
;; Bounding box

(: point-light-shell-shape-rect (-> point-light-shell-shape Nonempty-FlRect3))
(define (point-light-shell-shape-rect a)
  (define p (point-light-shell-shape-position a))
  (define radius (point-light-shell-shape-max-radius a))
  (define r (flvector radius radius radius))
  (nonempty-flrect3 (flv3- p r) (flv3+ p r)))

;; ===================================================================================================
;; Transform

(: point-light-shell-shape-easy-transform (-> point-light-shell-shape Affine point-light-shell-shape))
(define (point-light-shell-shape-easy-transform a t)
  (match-define (point-light-shell-shape _ fs e v r0 r1) a)
  (define new-v (flt3apply/pos (affine-transform t) v))
  (point-light-shell-shape (lazy-passes) fs e new-v r0 r1))
