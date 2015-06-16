#lang typed/racket/base

;; Sphere impostors: vertex shader outputs a quad; fragment shaders ray-trace a sphere

(require racket/unsafe/ops
         racket/list
         racket/match
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         "../../math.rkt"
         "../../memo.rkt"
         "../../engine.rkt"
         "../../utils.rkt"
         "sphere-type.rkt")

(provide get-sphere-shape-passes)

;; ===================================================================================================
;; Program for pass 1: material

(define sphere-mat-vertex-attributes
  (list (attribute "" 'vec4 "t0")
        (attribute "" 'vec4 "t1")
        (attribute "" 'vec4 "t2")
        (attribute "" 'vec4/bytes "vert_roughness_inside_id")  ; vec4(roughness, inside, id, 0)
        ))

(define sphere-mat-fragment-attributes
  (list (attribute "flat" 'vec4 "frag_trans_z")
        (attribute "flat" 'mat3 "frag_untrans")
        (attribute "flat" 'float "frag_roughness")
        (attribute "flat" 'float "frag_inside")
        (attribute "smooth" 'float "frag_is_degenerate")
        (attribute "smooth" 'vec3 "frag_start")
        (attribute "smooth" 'vec3 "frag_dir")))

(define-singleton (sphere-mat-vertex-code)
  (make-vertex-code
   "sphere-mat-vertex-30"
   #:includes
   (list output-impostor-vertex-code
         model-vertex-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "view" 'view)
         (standard-uniform "" 'mat4 "unview" 'unview)
         (standard-uniform "" 'mat4 "proj" 'proj)
         (standard-uniform "" 'mat4 "unproj" 'unproj))
   #:in-attributes
   sphere-mat-vertex-attributes
   #:out-attributes
   sphere-mat-fragment-attributes
   #<<code
mat4x3 sphere = rows2mat4x3(t0, t1, t2);
mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(sphere));
mat4x3 unmodel = affine_inverse(model);
mat4 trans = view * a2p(model);
mat4 untrans = a2p(unmodel) * unview;
  
frag_trans_z = transpose(trans)[2];
frag_untrans = mat3(untrans);
frag_roughness = vert_roughness_inside_id.x / 255;
frag_inside = vert_roughness_inside_id.y;
int vert_id = int(vert_roughness_inside_id.z);
frag_is_degenerate = output_impostor_vertex(trans, proj, vec3(-1.0), vec3(+1.0), vert_id);
  
vec4 dir = unproj * gl_Position;
frag_dir = mat3(untrans) * (dir.xyz / dir.w);
frag_start = untrans[3].xyz;
code
   ))

(define-singleton (sphere-mat-fragment-code)
  (make-fragment-code
   "sphere-mat-fragment-30"
   #:includes
   (list output-mat-fragment-code
         ray-trace-fragment-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "proj" 'proj))
   #:in-attributes
   sphere-mat-fragment-attributes
   #<<code
// all fragments should discard if this one does
if (frag_is_degenerate > 0.0) discard;

vec3 start = frag_start;
vec3 dir = normalize(frag_dir);
vec2 ts = unit_sphere_intersect(start, dir);
float t = mix(ts.x, ts.y, frag_inside);
// many nearby fragments should discard if this one does
if (t <= 0.0) discard;
  
vec3 pos = start + dir * t;
float vpos_z = dot(frag_trans_z, vec4(pos,1.0));  // transformed pos z coord only
vec3 vnorm = pos * frag_untrans;
output_mat(mix(vnorm,-vnorm,frag_inside), frag_roughness, vpos_z);
code
   ))

(define-singleton (sphere-mat-program-code)
  (make-program-code
   "sphere-mat-program-30"
   (sphere-mat-vertex-code)
   (sphere-mat-fragment-code)))

(define-singleton/context (sphere-mat-program)
  (log-pict3d-info "<engine> creating sphere material program for OpenGL >= 30")
  (program-code->gl-program (sphere-mat-program-code)))

;; ===================================================================================================
;; Program for pass 2: color

(define sphere-draw-vertex-attributes
  (list (attribute "" 'vec4 "t0")
        (attribute "" 'vec4 "t1")
        (attribute "" 'vec4 "t2")
        ;; vec4(r, g, b, a)
        (attribute "" 'vec4/bytes "vert_rcolor")
        ;; vec4(r, g, intensity.lo, intensity.hi)
        (attribute "" 'vec4/bytes "vert_ecolor")
        ;; vec4(ambient, diffuse, specular, inside | id)
        (attribute "" 'vec4/bytes "vert_material_inside_id")
        ))

(define sphere-draw-fragment-attributes
  (list (attribute "flat" 'vec4 "frag_trans_z")
        (attribute "flat" 'vec3 "frag_rcolor")
        (attribute "flat" 'vec3 "frag_ecolor")
        (attribute "flat" 'float "frag_alpha")
        (attribute "flat" 'float "frag_ambient")
        (attribute "flat" 'float "frag_diffuse")
        (attribute "flat" 'float "frag_specular")
        (attribute "flat" 'float "frag_inside")
        (attribute "smooth" 'float "frag_is_degenerate")
        (attribute "smooth" 'vec3 "frag_start")
        (attribute "smooth" 'vec3 "frag_dir")))

(define-singleton (sphere-draw-vertex-code)
  (make-vertex-code
   "sphere-draw-vertex-30"
   #:includes
   (list output-impostor-vertex-code
         model-vertex-code
         unpack-emitted-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "view" 'view)
         (standard-uniform "" 'mat4 "unview" 'unview)
         (standard-uniform "" 'mat4 "proj" 'proj)
         (standard-uniform "" 'mat4 "unproj" 'unproj))
   #:in-attributes
   sphere-draw-vertex-attributes
   #:out-attributes
   sphere-draw-fragment-attributes
   #<<code
mat4x3 sphere = rows2mat4x3(t0, t1, t2);
mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(sphere));
mat4x3 unmodel = affine_inverse(model);
mat4 trans = view * a2p(model);
mat4 untrans = a2p(unmodel) * unview;

frag_trans_z = transpose(trans)[2];
frag_rcolor = pow(vert_rcolor.rgb / 255, vec3(2.2));
frag_alpha = vert_rcolor.a / 255;
frag_ecolor = unpack_emitted(vert_ecolor);
frag_ambient = vert_material_inside_id.x / 255;
frag_diffuse = vert_material_inside_id.y / 255;
frag_specular = vert_material_inside_id.z / 255;
int inside_id = int(vert_material_inside_id.w);
frag_inside = (inside_id & 4) >> 2;
int vert_id = inside_id & 3;
frag_is_degenerate = output_impostor_vertex(trans, proj, vec3(-1.0), vec3(+1.0), vert_id);

vec4 dir = unproj * gl_Position;
frag_dir = mat3(untrans) * (dir.xyz / dir.w);
frag_start = untrans[3].xyz;
code
   ))

(define-singleton (sphere-opaq-fragment-code)
  (make-fragment-code
   "sphere-opaq-fragment-30"
   #:includes
   (list output-opaq-fragment-code
         ray-trace-fragment-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "proj" 'proj)
         (standard-uniform "" 'mat4 "unproj" 'unproj)
         (standard-uniform "" 'vec3 "ambient" 'ambient)
         (standard-uniform "" 'sampler2D "diffuse" 'diffuse)
         (standard-uniform "" 'sampler2D "specular" 'specular))
   #:in-attributes
   sphere-draw-fragment-attributes
   #<<code
// all fragments should discard if this one does
if (frag_is_degenerate > 0.0) discard;

vec3 start = frag_start;
vec3 dir = normalize(frag_dir);
vec2 ts = unit_sphere_intersect(start, dir);
float t = mix(ts.x, ts.y, frag_inside);
// many nearby fragments should discard if this one does
if (t <= 0.0) discard;
  
vec3 pos = start + dir * t;
float vpos_z = dot(frag_trans_z, vec4(pos,1.0));
  
ivec2 xy = ivec2(gl_FragCoord.xy);
vec3 diff = texelFetch(diffuse, xy, 0).rgb;
vec3 spec = texelFetch(specular, xy, 0).rgb;
vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
vec3 color = frag_ecolor + frag_rcolor.rgb * light;
output_opaq(color, vpos_z);
code
   ))

(define-singleton (sphere-tran-fragment-code)
  (make-fragment-code
   "sphere-tran-fragment-30"
   #:includes
   (list output-tran-fragment-code
         ray-trace-fragment-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "proj" 'proj)
         (standard-uniform "" 'mat4 "unproj" 'unproj)
         (standard-uniform "" 'vec3 "ambient" 'ambient)
         (standard-uniform "" 'sampler2D "diffuse" 'diffuse)
         (standard-uniform "" 'sampler2D "specular" 'specular))
   #:in-attributes
   sphere-draw-fragment-attributes
   #<<code
// all fragments should discard if this one does
if (frag_is_degenerate > 0.0) discard;

vec3 start = frag_start;
vec3 dir = normalize(frag_dir);
vec2 ts = unit_sphere_intersect(start, dir);
float t = mix(ts.x, ts.y, frag_inside);
// many nearby fragments should discard if this one does
if (t <= 0.0) discard;
  
vec3 pos = start + dir * t;
float vpos_z = dot(frag_trans_z, vec4(pos,1.0));
  
ivec2 xy = ivec2(gl_FragCoord.xy);
vec3 diff = texelFetch(diffuse, xy, 0).rgb;
vec3 spec = texelFetch(specular, xy, 0).rgb;
vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
vec3 color = frag_ecolor + frag_rcolor.rgb * light;
output_tran(color, frag_alpha, vpos_z);
code
   ))

(define-singleton (sphere-opaq-program-code)
  (make-program-code
   "sphere-opaq-program-30"
   (sphere-draw-vertex-code)
   (sphere-opaq-fragment-code)))

(define-singleton (sphere-tran-program-code)
  (make-program-code
   "sphere-tran-program-30"
   (sphere-draw-vertex-code)
   (sphere-tran-fragment-code)))

(define-singleton/context (sphere-opaq-program)
  (log-pict3d-info "<engine> creating sphere opaque color pass program for OpenGL >= 30")
  (program-code->gl-program (sphere-opaq-program-code)))

(define-singleton/context (sphere-tran-program)
  (log-pict3d-info "<engine> creating sphere transparent color pass program for OpenGL >= 30")
  (program-code->gl-program (sphere-tran-program-code)))

;; ===================================================================================================
;; Sphere shape passes

(: vertex-ids (Vectorof Index))
(define vertex-ids #(0 1 2 2 1 3))

(: get-sphere-shape-passes (-> shape passes))
(define (get-sphere-shape-passes s)
  (match-define (sphere-shape _ _ t c e m inside?) s)
  
  (define mat-size (program-code-vao-size (sphere-mat-program-code)))
  (define mat-data (make-bytes (* 4 mat-size)))
  (let* ([i  (serialize-affine mat-data 0 t)]
         [i  (serialize-float/byte mat-data i (unsafe-flv4-ref m 3))]
         [i  (serialize-byte mat-data i (if inside? 1 0))])
    (define mat-data-ptr (u8vector->cpointer mat-data))
    (for ([k : Nonnegative-Fixnum  (in-range 1 4)])
      (memcpy mat-data-ptr (unsafe-fx* k mat-size) mat-data-ptr mat-size _byte)
      (bytes-set! mat-data (unsafe-fx+ (unsafe-fx* k mat-size) i) k)))
  
  (define inside-bit (if inside? #b100 0))
  
  (define draw-size (program-code-vao-size (sphere-opaq-program-code)))
  (define draw-data (make-bytes (* 4 draw-size)))
  (let* ([i  (serialize-affine draw-data 0 t)]
         [i  (serialize-vec4/bytes draw-data i c)]
         [i  (serialize-emitted/bytes draw-data i e)]
         [i  (serialize-material-reflectances/bytes draw-data i m)]
         [j  i]  ; offset of inside|id
         [i  (serialize-byte draw-data i inside-bit)])
    (define draw-data-ptr (u8vector->cpointer draw-data))
    (for ([k : Nonnegative-Fixnum  (in-range 1 4)])
      (memcpy draw-data-ptr (unsafe-fx* k draw-size) draw-data-ptr draw-size _byte)
      (bytes-set! draw-data (unsafe-fx+ (unsafe-fx* k draw-size) j)
                  (bitwise-ior inside-bit k))))
  
  (if (< (flv4-ref c 3) 1.0)
      (passes
       #()
       #()
       #()
       (vector (shape-params sphere-mat-program empty #t GL_TRIANGLES
                             (vertices 4 mat-data vertex-ids)))
       (vector (shape-params sphere-tran-program empty #t GL_TRIANGLES
                             (vertices 4 draw-data vertex-ids))))
      (passes
       #()
       (vector (shape-params sphere-mat-program empty #t GL_TRIANGLES
                             (vertices 4 mat-data vertex-ids)))
       (vector (shape-params sphere-opaq-program empty #t GL_TRIANGLES
                             (vertices 4 draw-data vertex-ids)))
       #()
       #())))
