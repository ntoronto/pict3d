#lang typed/racket/base

;; Point light impostors: vertex shader outputs a quad; fragment shader computes light

(require racket/unsafe/ops
         racket/match
         racket/list
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         "../math.rkt"
         "../memo.rkt"
         "../engine.rkt"
         "../utils.rkt"
         "types.rkt")

(provide make-point-light-shape
         (struct-out point-light-shape))

(struct point-light-shape shape
  ([emitted : FlV4]
   [affine : FlAffine3]
   [min-radius : Flonum]
   [max-radius : Flonum])
  #:transparent)

;; ===================================================================================================
;; Constructor

(: make-point-light-shape (-> FlV4 FlAffine3 Flonum Flonum point-light-shape))
(define (make-point-light-shape e t r0 r1)
  (point-light-shape (lazy-passes) point-light-shape-functions
                     e t r0 r1))

;; ===================================================================================================
;; Set attributes

(: set-point-light-shape-emitted (-> shape FlV4 point-light-shape))
(define (set-point-light-shape-emitted s e)
  (match-define (point-light-shape _ _ _ t r0 r1) s)
  (make-point-light-shape e t r0 r1))

;; ===================================================================================================
;; Program for pass 0: light

(define point-light-vertex-attributes
  (list (attribute "" 'vec4 "sphere0")
        (attribute "" 'vec4 "sphere1")
        (attribute "" 'vec4 "sphere2")
        (attribute "" 'vec3 "vert_intensity_radii")
        (attribute "" 'vec4/bytes "vert_color_id")))

(define point-light-fragment-attributes
  (list (attribute "flat" 'vec3 "frag_position")
        (attribute "flat" 'mat4 "frag_untrans")
        (attribute "flat" 'float "frag_min_radius")
        (attribute "flat" 'float "frag_max_radius")
        (attribute "flat" 'vec3 "frag_intensity")
        (attribute "smooth" 'float "frag_is_degenerate")
        (attribute "smooth" 'vec3 "frag_dir")))

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
   #:in-attributes
   point-light-vertex-attributes
   #:out-attributes
   point-light-fragment-attributes
   #<<code
mat4x3 sphere = rows2mat4x3(sphere0, sphere1, sphere2);
mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(sphere));
mat4x3 unmodel = affine_inverse(model);
mat4 trans = view * a2p(model);
mat4 untrans = a2p(unmodel) * unview;

float min_radius = vert_intensity_radii.y;
float max_radius = vert_intensity_radii.z;
vec3 color = vert_color_id.rgb;
int id = int(vert_color_id.a);
frag_position = trans[3].xyz;
frag_untrans = untrans;
frag_min_radius = min_radius;
frag_max_radius = max_radius;
frag_intensity = pow(color / 255, vec3(2.2)) * vert_intensity_radii.x;
frag_is_degenerate = output_impostor_vertex(trans, proj, vec3(-max_radius), vec3(max_radius), id);

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
if (dist < frag_min_radius) discard;
if (dist > frag_max_radius) discard;
vec3 light = attenuate_invsqr(frag_intensity, dist);

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
  (match-define (point-light-shape _ _ e t r0 r1) s)
  
  (define size (program-code-vao-size point-light-program-code))
  (define data (make-bytes (* 4 size)))
  (define data-ptr (u8vector->cpointer data))
  (call/flv4-values e
    (λ (r g b int)
      (let* ([i  (serialize-affine data 0 t)]
             [i  (serialize-float data i int)]
             [i  (serialize-float data i r0)]
             [i  (serialize-float data i r1)]
             [i  (serialize-vec3/bytes data i (flv3 r g b))])
        (for ([k : Nonnegative-Fixnum  (in-range 1 4)])
          (memcpy data-ptr (unsafe-fx* k size) data-ptr size _byte)
          (bytes-set! data (unsafe-fx+ (unsafe-fx* k size) i) k)))))
  
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
  (match-define (point-light-shape _ _ e t0 r0 r1) s)
  (bbox (transformed-sphere-flrect3 (flt3compose t (flt3compose t0 (scale-flt3 (flv3 r1 r1 r1)))))
        0.0))

;; ===================================================================================================
;; Transform

(: point-light-shape-transform (-> shape FlAffine3 point-light-shape))
(define (point-light-shape-transform s t)
  (match-define (point-light-shape _ _ e t0 r0 r1) s)
  (make-point-light-shape e (flt3compose t t0) r0 r1))

;; ===================================================================================================
;; Warp (approximately)

(: point-light-shape-deform (-> shape FlSmooth3 (U Null (List point-light-shape))))
(define (point-light-shape-deform s t)
  (match-define (point-light-shape _ _ e t0 r0 r1) s)
  (let ([t  (fls3apply/affine t t0)])
    (if t (list (make-point-light-shape e t r0 r1)) empty)))

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
