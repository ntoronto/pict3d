#lang typed/racket/base

;; Point light impostors: vertex shader outputs a quad; fragment shader computes light

(require racket/unsafe/ops
         racket/match
         racket/list
         math/flonum
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         "../../math.rkt"
         "../../memo.rkt"
         "../../engine.rkt"
         "../../shape.rkt"
         "../../utils.rkt")

(provide make-point-light-shell-shape
         (struct-out point-light-shell-shape))

(struct point-light-shell-shape shape
  ([affine : FlAffine3]
   [emitted : FlV4]
   [max-quad : Flonum]
   [min-radius : Flonum]
   [max-radius : Flonum])
  #:transparent)

;; ===================================================================================================
;; Constructor

(: make-point-light-shell-shape (-> FlAffine3 FlV4 Flonum Flonum Flonum point-light-shell-shape))
(define (make-point-light-shell-shape t e qmax rmin rmax)
  (point-light-shell-shape (lazy-passes) point-light-shell-shape-functions t e qmax rmin rmax))

;; ===================================================================================================
;; Set attributes

(: set-point-light-shell-shape-emitted (-> shape FlV4 point-light-shell-shape))
(define (set-point-light-shell-shape-emitted s e)
  (match-define (point-light-shell-shape _ _ t _ qmax rmin rmax) s)
  (make-point-light-shell-shape t e qmax rmin rmax))

;; ===================================================================================================
;; Program for pass 0: light

(define point-light-shell-vertex-attributes
  (list (attribute "" 'vec4 "sphere0")
        (attribute "" 'vec4 "sphere1")
        (attribute "" 'vec4 "sphere2")
        (attribute "" 'vec4 "vert_quad_radii_intensity")
        (attribute "" 'vec4/bytes "vert_color_id")))

(define point-light-shell-fragment-attributes
  (list (attribute "flat" 'vec3 "frag_position")
        (attribute "flat" 'mat4 "frag_untrans")
        (attribute "flat" 'float "frag_max_quad")
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
   #:in-attributes  point-light-shell-vertex-attributes
   #:out-attributes point-light-shell-fragment-attributes
   #<<code
mat4x3 sphere = rows2mat4x3(sphere0, sphere1, sphere2);
mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(sphere));
mat4x3 unmodel = affine_inverse(model);
mat4 trans = view * a2p(model);
mat4 untrans = a2p(unmodel) * unview;

frag_position = trans[3].xyz;
frag_untrans = untrans;

float max_quad = vert_quad_radii_intensity.x;
frag_max_quad = max_quad;
frag_min_radius = vert_quad_radii_intensity.y;
float max_radius = vert_quad_radii_intensity.z;
frag_max_radius = max_radius;
frag_intensity = vert_quad_radii_intensity.w;

vec3 color = vert_color_id.rgb;
frag_color = pow(color / 255, vec3(2.2));

int id = int(vert_color_id.a);
float mx = min(max_radius, max_quad);
frag_is_degenerate = output_impostor_vertex(trans, proj, vec3(-mx), vec3(mx), id);

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
   (list (standard-uniform "" 'mat4 "proj" 'proj)
         (standard-uniform "" 'mat4 "view" 'view)
         (standard-uniform "" 'mat4 "unview" 'unview)
         (standard-uniform "" 'sampler2D "depth" 'depth)
         (standard-uniform "" 'sampler2D "material" 'material)
         (standard-uniform "" 'int "width" 'width)
         (standard-uniform "" 'int "height" 'height))
   #:in-attributes
   point-light-shell-fragment-attributes
   #:definitions
   (list
    #<<code
float implicit_line_alpha(float dist, vec3 vpos, vec3 N, float d, float w, float s) {
  float max_dist = max(0.0, w * 0.5 - 0.5);
  float dx = dFdx(dist);
  float dy = dFdy(dist);
  
  float delta = max(0.0, abs(dist - d) / length(vec2(dx, dy)) - max_dist);
  float a = 1 - step(1.2, delta);

  vec3 S = normalize(cross(dFdx(vpos),dFdy(vpos)));
  float b = pow(abs(dot(S,N)), 64);

  float x = abs(dy) > abs(dx) ? gl_FragCoord.x : gl_FragCoord.y;
  float c = 1 - mod(floor(x / (w * s)), 2.0);

  return a * b * c * exp2(delta * delta * -9.0);
}
code
    #<<code
float blended_dashed_line_alpha(float dist, vec3 vpos, vec3 N, float d, float w, float dash) {
  float aa = implicit_line_alpha(dist, vpos, N, d, w, 0.0);
  float ab = implicit_line_alpha(dist, vpos, N, d, w, 3.0);
  return mix(aa,ab,dash);
}
code
    )
   #<<code
// all fragments should discard if this one does
if (frag_is_degenerate > 0.0) discard;

float d = texelFetch(depth, ivec2(gl_FragCoord.xy), 0).r;
if (d == 0.0) discard;
// Work around zero-derivative problem when z is very large
float z = d == 0.0 ? 0.0 : get_view_depth(d);
vec3 vpos = frag_dir * z;
vec3 mpos = (frag_untrans * vec4(vpos,1)).xyz;

float dist = length(mpos);
float ddist = length(vec2(dFdx(dist), dFdy(dist)));
if (dist < frag_min_radius - ddist * 3) discard;
if (dist > min(frag_max_quad, frag_max_radius) + ddist * 3) discard;

vec3 N = get_surface(material).normal;
vec3 L = normalize(frag_position - vpos);
float cs = dot(L,N);

// Smoothly blend between dashed lines on back faces and solid lines on front
float c = 1.0/128.0;
float b = c+c*c;
float a = 1 - (-c-b/(cs-c));
float dash = a * (1 - step(0.0, cs));

float diff = max(frag_max_radius, frag_max_quad) - frag_min_radius;
float a1 = blended_dashed_line_alpha(dist, vpos, N, sqrt(frag_intensity/0.5), 1.5, dash);
float a2 = 0.5 * blended_dashed_line_alpha(dist, vpos, N, sqrt(frag_intensity/0.25), 1.5, dash);
float a3 = 0.25 * blended_dashed_line_alpha(dist, vpos, N, sqrt(frag_intensity/0.125), 1.5, dash);
float a4 = 0.125 * blended_dashed_line_alpha(dist, vpos, N, sqrt(frag_intensity/0.0625), 1.5, dash);
float a5 = 0.0625 * blended_dashed_line_alpha(dist, vpos, N, sqrt(frag_intensity/0.03125), 1.5, dash);
float aa = max(max(0.0, a1), max(max(a2,a3), max(a4, a5)));
out_diffuse = vec4(frag_color * aa, 1.0);
out_specular = vec4(frag_color * aa, 1.0);
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

(: get-point-light-shell-shape-passes (-> shape passes))
(define (get-point-light-shell-shape-passes s)
  (match-define (point-light-shell-shape _ _ t e qmax rmin rmax) s)
  
  (define size (program-code-vao-size point-light-shell-program-code))
  (define data (make-bytes (* 4 size)))
  (define data-ptr (u8vector->cpointer data))
  (call/flv4-values e
    (λ (r g b int)
      (let* ([i  (serialize-affine data 0 t)]
             [i  (serialize-floats data i (flvector qmax rmin rmax int) 4)]
             [i  (serialize-vec3/bytes data i (flv3 r g b))])
        ;; i is the index of id
        ;; Make 3 copies of the data, but with incremented ids
        (for ([id : Nonnegative-Fixnum  (in-range 1 4)])
          (memcpy data-ptr (unsafe-fx* id size) data-ptr size _byte)
          (bytes-set! data (unsafe-fx+ (unsafe-fx* id size) i) id)))))
  
  (passes
   (vector (shape-params point-light-shell-program
                         empty #t GL_TRIANGLES (vertices 4 data vertex-ids)))
   #()
   #()
   #()
   #()))

;; ===================================================================================================
;; Bounding box

(: get-point-light-shell-shape-bbox (-> shape FlAffine3 bbox))
(define (get-point-light-shell-shape-bbox s t)
  (match-define (point-light-shell-shape _ _ t0 _ qmax rmin rmax) s)
  (define r (min qmax rmax))
  (if (> r 1e100)
      (bbox inf-flrect3 0.0)
      (bbox (transformed-sphere-flrect3 (flt3compose t (flt3compose t0 (scale-flt3 (flv3 r r r)))))
            0.0)))

;; ===================================================================================================
;; Transform

(: point-light-shell-shape-transform (-> shape FlAffine3 point-light-shell-shape))
(define (point-light-shell-shape-transform s t)
  (match-define (point-light-shell-shape _ _ t0 e qmax rmin rmax) s)
  (make-point-light-shell-shape (flt3compose t t0) e qmax rmin rmax))

;; ===================================================================================================
;; Warp (approximately)

(: point-light-shell-shape-deform (-> shape FlSmooth3 (U Null (List point-light-shell-shape))))
(define (point-light-shell-shape-deform s t)
  (match-define (point-light-shell-shape _ _ t0 e qmax rmin rmax) s)
  (let ([t  (fls3apply/affine t t0)])
    (if t (list (make-point-light-shell-shape t e qmax rmin rmax)) empty)))

;; ===================================================================================================

(define point-light-shell-shape-functions
  (deform-shape-functions
    get-point-light-shell-shape-passes
    (λ (s kind t) (and (eq? kind 'invisible) (get-point-light-shell-shape-bbox s t)))
    point-light-shell-shape-transform
    (λ (s t) (list (point-light-shell-shape-transform s t)))
    default-ray-intersect
    default-set-color
    set-point-light-shell-shape-emitted
    default-set-material
    default-extract-faces
    default-tessellate
    point-light-shell-shape-deform))
