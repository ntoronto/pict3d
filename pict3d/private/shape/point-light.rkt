#lang typed/racket/base

;; Point light impostors: vertex shader outputs a quad; fragment shader computes light

(require racket/unsafe/ops
         racket/match
         racket/list
         math/flonum
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         "../gl.rkt"
         "../math.rkt"
         "../memo.rkt"
         "../engine.rkt"
         "../utils.rkt"
         "types.rkt")

(provide make-point-light-shape
         (struct-out point-light-shape))

(struct point-light-shape shape
  ([affine : FlAffine3]
   [emitted : FlV4]
   [max-quad : Flonum]
   [min-radius : Flonum]
   [max-radius : Flonum])
  #:transparent)

;; ===================================================================================================
;; Constructor

(: make-point-light-shape (-> FlAffine3 FlV4 Flonum Flonum Flonum point-light-shape))
(define (make-point-light-shape t e qmax rmin rmax)
  (point-light-shape (lazy-passes) point-light-shape-functions t e qmax rmin rmax))

;; ===================================================================================================
;; Set attributes

(: set-point-light-shape-emitted (-> shape FlV4 point-light-shape))
(define (set-point-light-shape-emitted s e)
  (match-define (point-light-shape _ _ t _ qmax rmin rmax) s)
  (make-point-light-shape t e qmax rmin rmax))

;; ===================================================================================================
;; Program for pass 0: light

(define point-light-vertex-attributes
  (list (attribute "" 'vec4 "sphere0")
        (attribute "" 'vec4 "sphere1")
        (attribute "" 'vec4 "sphere2")
        (attribute "" 'vec4 "vert_quad_radii_intensity")
        (attribute "" 'vec4/bytes "vert_color_id")))

(define point-light-fragment-attributes
  (list (attribute "flat" 'vec3 "frag_position")
        (attribute "flat" 'mat4 "frag_untrans")
        (attribute "flat" 'float "frag_max_quad")
        (attribute "flat" 'float "frag_min_radius")
        (attribute "flat" 'float "frag_max_radius")
        (attribute "flat" 'vec3 "frag_intensity")
        (attribute "smooth" 'float "frag_is_degenerate")
        (attribute "smooth" 'vec3 "frag_dir")))
#|
(λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (define dx (fl3mag m00 m01 m02))
      (define dy (fl3mag m10 m11 m12))
      (define dz (fl3mag m20 m21 m22))
      (flrect3 (flv3 (- m03 dx) (- m13 dy) (- m23 dz))
               (flv3 (+ m03 dx) (+ m13 dy) (+ m23 dz))))
|#

(define point-light-vertex-code
  (make-vertex-code
   "point-light-vertex"
   #:includes
   (list output-impostor-vertex-code
         model-vertex-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "view" 'view)
         (standard-uniform "" 'mat4 "unview" 'unview)
         (standard-uniform "" 'mat4 "proj" 'proj)
         (standard-uniform "" 'mat4 "unproj" 'unproj))
   #:in-attributes  point-light-vertex-attributes
   #:out-attributes point-light-fragment-attributes
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

vec3 color = vert_color_id.rgb;
frag_intensity = pow(color / 255, vec3(2.2)) * vert_quad_radii_intensity.w;

int id = int(vert_color_id.a);
float mx = min(max_radius, max_quad);

mat4 t = trans * mat4(vec4(mx,0,0,0), vec4(0,mx,0,0), vec4(0,0,mx,0), vec4(0,0,0,1));
float dx = length(t[0].xyz);
float dy = length(t[1].xyz);
float dz = length(t[2].xyz);
vec3 mins = t[3].xyz - vec3(dx,dy,dz);
vec3 maxs = t[3].xyz + vec3(dy,dy,dz);
frag_is_degenerate = output_impostor_vertex(mat4(1), proj, mins, maxs, id);
//frag_is_degenerate = output_impostor_vertex(trans, proj, vec3(-mx), vec3(mx), id);

vec4 dir = unproj * gl_Position;
frag_dir = vec3(dir.xy / dir.z, 1.0);
code
   ))

(define point-light-fragment-code
  (make-fragment-code
   "point-light-vertex"
   #:includes (list light-fragment-code)
   #:standard-uniforms (list (standard-uniform "" 'sampler2D "depth" 'depth)
                             (standard-uniform "" 'sampler2D "material" 'material))
   #:in-attributes point-light-fragment-attributes
   #<<code
// all fragments should discard if this one does
if (frag_is_degenerate > 0.0) discard;

float d = texelFetch(depth, ivec2(gl_FragCoord.xy), 0).r;
if (d == 0.0) discard;
float z = get_view_depth(d);
vec3 vpos = frag_dir * z;
vec3 mpos = (frag_untrans * vec4(vpos,1)).xyz;

float dist = length(mpos);
if (dist <= frag_min_radius || dist > min(frag_max_radius, frag_max_quad)) discard;
vec3 light = attenuate_invsqr_quad(frag_intensity, frag_max_quad, dist);

vec3 L = normalize(frag_position - vpos);
vec3 V = normalize(-vpos);
output_light(light, get_surface(material), L, V);
code
   ))

(define point-light-program-code
  (make-program-code
   "point-light-program"
   point-light-vertex-code
   point-light-fragment-code))

(define-singleton/context (point-light-program)
  (log-pict3d-info "<engine> creating point light program")
  (program-code->gl-program point-light-program-code))

;; ===================================================================================================
;; Point light shape passes

(: vertex-ids (Vectorof Index))
(define vertex-ids #(0 1 2 2 1 3))

(: get-point-light-shape-passes (-> shape passes))
(define (get-point-light-shape-passes s)
  (match-define (point-light-shape _ _ t e qmax rmin rmax) s)
  
  (define size (program-code-vao-size point-light-program-code))
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
   (vector (shape-params point-light-program empty #t GL_TRIANGLES (vertices 4 data vertex-ids)))
   #()
   #()
   #()
   #()))

;; ===================================================================================================
;; Bounding box

(: get-point-light-shape-bbox (-> shape FlAffine3 bbox))
(define (get-point-light-shape-bbox s t)
  (match-define (point-light-shape _ _ t0 _ qmax rmin rmax) s)
  (define r (min qmax rmax))
  (if (> r 1e100)
      (bbox inf-flrect3 0.0)
      (bbox (transformed-sphere-flrect3 (flt3compose t (flt3compose t0 (scale-flt3 (flv3 r r r)))))
            0.0)))

;; ===================================================================================================
;; Transform

(: point-light-shape-transform (-> shape FlAffine3 point-light-shape))
(define (point-light-shape-transform s t)
  (match-define (point-light-shape _ _ t0 e qmax rmin rmax) s)
  (make-point-light-shape (flt3compose t t0) e qmax rmin rmax))

;; ===================================================================================================
;; Warp (approximately)

(: point-light-shape-deform (-> shape FlSmooth3 (U Null (List point-light-shape))))
(define (point-light-shape-deform s t)
  (match-define (point-light-shape _ _ t0 e qmax rmin rmax) s)
  (let ([t  (fls3apply/affine t t0)])
    (if t (list (make-point-light-shape t e qmax rmin rmax)) empty)))

;; ===================================================================================================

(define point-light-shape-functions
  (deform-shape-functions
    get-point-light-shape-passes
    (λ (s kind t) (and (eq? kind 'invisible) (get-point-light-shape-bbox s t)))
    point-light-shape-transform
    (λ (s t) (list (point-light-shape-transform s t)))
    default-ray-intersect
    default-set-color
    set-point-light-shape-emitted
    default-set-material
    default-extract-faces
    default-tessellate
    point-light-shape-deform))
