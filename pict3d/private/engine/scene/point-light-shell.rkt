#lang typed/racket/base

;; Point light impostors: vertex shader outputs a quad; fragment shader computes light

(require racket/unsafe/ops
         racket/match
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
         "../shader-code.rkt"
         "../serialize-vertices.rkt"
         "../types.rkt"
         "../utils.rkt"
         "types.rkt"
         "flags.rkt")

(provide make-point-light-shell-shape
         make-point-light-shell-shape-passes
         point-light-shell-shape-rect
         point-light-shell-shape-easy-transform
         )

;; ===================================================================================================
;; Constructor

(: make-point-light-shell-shape (-> FlVector Affine Flonum Flonum point-light-shell-shape))
(define (make-point-light-shell-shape e t r0 r1)
  (cond [(not (= 4 (flvector-length e)))
         (raise-argument-error 'make-point-light-shell-shape "length-4 flvector"
                               0 e t r0 r1)]
        [else
         (define fs (flags-join invisible-flag transparent-flag (color-emitting-flag e)))
         (point-light-shell-shape (lazy-passes) fs e t r0 r1)]))

;; ===================================================================================================
;; Program for pass 0: light

(define point-light-shell-fragment-attributes
  (list (attribute "flat" 'vec3 "frag_position")
        (attribute "flat" 'mat4 "frag_trans")
        (attribute "flat" 'mat4 "frag_untrans")
        (attribute "flat" 'float "frag_min_radius")
        (attribute "flat" 'float "frag_max_radius")
        (attribute "flat" 'vec3 "frag_color")
        (attribute "flat" 'float "frag_intensity")
        (attribute "smooth" 'float "frag_is_degenerate")
        (attribute "smooth" 'vec3 "frag_dir")))

(define point-light-shell-vertex-code
  (make-vertex-code
   "point-light-shell-vertex"
   #:includes
   (list output-impostor-vertex-code
         model-vertex-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "view" 'view)
         (standard-uniform "" 'mat4 "unview" 'unview)
         (standard-uniform "" 'mat4 "proj" 'proj)
         (standard-uniform "" 'mat4 "unproj" 'unproj))
   #:in-attributes
   (list (attribute "" 'vec4 "sphere0")
         (attribute "" 'vec4 "sphere1")
         (attribute "" 'vec4 "sphere2")
         (attribute "" 'vec3 "vert_intensity_radii")
         (attribute "" 'vec4/bytes "vert_color_id"))
   #:out-attributes
   point-light-shell-fragment-attributes
   #<<code
mat4x3 sphere = rows2mat4x3(sphere0, sphere1, sphere2);
mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(sphere));
mat4x3 unmodel = affine_inverse(model);
mat4 trans = view * a2p(model);
mat4 untrans = a2p(unmodel) * unview;

float intensity = vert_intensity_radii.x;
float min_radius = vert_intensity_radii.y;
float max_radius = vert_intensity_radii.z;
vec3 color = vert_color_id.rgb;
int id = int(vert_color_id.a);
frag_position = trans[3].xyz;
frag_trans = trans;
frag_untrans = untrans;
frag_min_radius = min_radius;
frag_max_radius = max_radius;
frag_color = pow(color / 255, vec3(2.2)); // * intensity;
frag_intensity = intensity;
frag_is_degenerate = output_impostor_vertex(trans, proj, vec3(-max_radius), vec3(max_radius), id);

vec4 dir = unproj * gl_Position;
frag_dir = vec3(dir.xy / dir.z, 1.0);
code
   ))

(define point-light-shell-fragment-code
  (make-fragment-code
   "point-light-shell-fragment"
   #:includes
   (list light-fragment-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "unview" 'unview)
         (standard-uniform "" 'sampler2D "depth" 'depth)
         (standard-uniform "" 'sampler2D "material" 'material)
         (standard-uniform "" 'int "width" 'width)
         (standard-uniform "" 'int "height" 'height))
   #:in-attributes
   point-light-shell-fragment-attributes
   #:definitions
   (list
   #<<code
void radius_bounds(float r, float h, float s, out float r1, out float r2) {
  float c = s*r*h;
  float rph = r+h;
  float rmh = r-h;
  r1 = sqrt(rph*rph + c);
  r2 = sqrt(rmh*rmh - c);
}
code
   #<<code
float dash_multiplier(vec3 T, vec3 P, float h) {
  float x = abs(T.x) > abs(T.y) ? P.x : P.y;
  return mod(floor(x/h), 2.0);
}
code
   )
   #<<code
// all fragments should discard if this one does
if (frag_is_degenerate > 0.0) discard;

float d = texelFetch(depth, ivec2(gl_FragCoord.xy), 0).r;
if (d == 0.0) discard;
float z = get_view_depth(d);
vec3 vpos = frag_dir * z;
vec3 mpos = (frag_untrans * vec4(vpos,1)).xyz;

float dist = length(mpos);
if (dist < frag_min_radius) discard;
if (dist > frag_max_radius) discard;

vec3 N = get_surface(material).normal;
vec3 L = normalize(frag_position - vpos);
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

// This doesn't fix line widths for arbitrary transformations, just uniform scaling
float mag = length((frag_untrans * vec4(-L,0)).xyz);

float e = 0.05;
for (int j = 0; j < 4; j++) {
  // compute radius bounds
  float r = sqrt(frag_intensity/e);
  float r1, r2;
  radius_bounds(r, h, s, r1, r2);
  float max_delta = abs(0.5*(r1-r2)*mag);
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
code
   ))

(define point-light-shell-program-code
  (make-program-code
   "point-light-shell-program"
   point-light-shell-vertex-code
   point-light-shell-fragment-code))

(define-singleton/context (point-light-shell-program)
  (log-pict3d-info "<engine> creating point light shell program")
  (program-code->gl-program point-light-shell-program-code))

;; ===================================================================================================
;; Point light shape passes

(: vertex-ids (Vectorof Index))
(define vertex-ids #(0 1 2 2 1 3))

(: make-point-light-shell-shape-passes (-> point-light-shell-shape passes))
(define (make-point-light-shell-shape-passes a)
  (match-define (point-light-shell-shape _ fs e t r0 r1) a)
  
  (cond
    [(flags-subset? emitting-flag fs)
     (define size (program-code-vao-size point-light-shell-program-code))
     (define data (make-bytes (* 4 size)))
     (define data-ptr (u8vector->cpointer data))
     (let* ([i  (serialize-affine data 0 t)]
            [i  (serialize-float data i (flvector-ref e 3))]
            [i  (serialize-float data i r0)]
            [i  (serialize-float data i r1)]
            [i  (serialize-vec3/bytes data i e)])
       (for ([k : Nonnegative-Fixnum  (in-range 1 4)])
         (memcpy data-ptr (unsafe-fx* k size) data-ptr size _byte)
         (bytes-set! data (unsafe-fx+ (unsafe-fx* k size) i) k)))
     
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
  (define t (affine-transform (point-light-shell-shape-affine a)))
  (define r1 (point-light-shell-shape-max-radius a))
  (transformed-sphere-flrect3 (flt3compose t (scale-flt3 (flvector r1 r1 r1)))))

;; ===================================================================================================
;; Transform

(: point-light-shell-shape-easy-transform (-> point-light-shell-shape Affine point-light-shell-shape))
(define (point-light-shell-shape-easy-transform a t)
  (match-define (point-light-shell-shape passes fs e t0 r0 r1) a)
  (point-light-shell-shape (lazy-passes) fs e (affine-compose t t0) r0 r1))

