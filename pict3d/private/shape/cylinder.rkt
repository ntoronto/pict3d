#lang typed/racket/base

(require racket/unsafe/ops
         racket/match
         racket/list
         racket/promise
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         math/flonum
         math/base
         "../math.rkt"
         "../memo.rkt"
         "../engine.rkt"
         "../soup.rkt"
         "../utils.rkt"
         "types.rkt"
         "triangle-mesh.rkt")

(provide make-cylinder-shape
         make-cylinder-wall-shape
         (struct-out cylinder-shape)
         (struct-out cylinder-wall-shape))

(struct cylinder-shape shape
  ([affine : FlAffine3]
   [top-radius : Nonnegative-Flonum]
   [max-angle : Positive-Flonum]
   [color : FlV4]
   [emitted : FlV4]
   [material : FlV4]
   [inside? : Boolean])
  #:transparent)

(struct cylinder-wall-shape shape
  ([affine : FlAffine3]
   [top1 : Nonnegative-Flonum]
   [top2 : Nonnegative-Flonum]
   [bot1 : Nonnegative-Flonum]
   [bot2 : Nonnegative-Flonum]
   [angle : Flonum]
   [color : FlV4]
   [emitted : FlV4]
   [material : FlV4]
   [back? : Boolean]
   [lazy-mesh : (Promise triangle-mesh-shape)])
  #:transparent)

;; ===================================================================================================
;; Constructors

(: make-cylinder-shape (-> FlAffine3 Nonnegative-Flonum Positive-Flonum FlV4 FlV4 FlV4 Boolean
                           cylinder-shape))
(define (make-cylinder-shape t r a c e m inside?)
  (cylinder-shape (lazy-passes) cylinder-shape-functions t r a c e m inside?))

(: make-cylinder-wall-shape (-> FlAffine3
                                Nonnegative-Flonum Nonnegative-Flonum
                                Nonnegative-Flonum Nonnegative-Flonum
                                Flonum FlV4 FlV4 FlV4 Boolean
                                cylinder-wall-shape))
(define (make-cylinder-wall-shape t r11 r12 r21 r22 a c e m back?)
  (: s cylinder-wall-shape)
  (define s
    (cylinder-wall-shape (lazy-passes) cylinder-wall-shape-functions t r11 r12 r21 r22 a c e m back?
                         (delay (cylinder-wall-shape->triangle-mesh-shape s))))
  s)

(: cylinder-wall-shape->vtxs (-> cylinder-wall-shape (Values vtx vtx vtx vtx)))
(define (cylinder-wall-shape->vtxs s)
  (match-define (cylinder-wall-shape _ _ t r11 r12 r21 r22 a c e m back? _) s)
  (define sa (sin a))
  (define ca (cos a))
  (define v1 (flt3apply/pos t (flv3 (* r11 ca) (* r11 sa) +1.0)))
  (define v2 (flt3apply/pos t (flv3 (* r21 ca) (* r21 sa) -1.0)))
  (define v3 (flt3apply/pos t (flv3 (* r22 ca) (* r22 sa) -1.0)))
  (define v4 (flt3apply/pos t (flv3 (* r12 ca) (* r12 sa) +1.0)))
  (define n (let ([n  (assert (flt3apply/norm t (flv3 sa (- ca) 0.0)) values)])
              (if back? (flv3neg n) n)))
  (define vtx1 (vtx v1 n c e m))
  (define vtx2 (vtx v2 n c e m))
  (define vtx3 (vtx v3 n c e m))
  (define vtx4 (vtx v4 n c e m))
  (define reverse? (if (flt3consistent? t) back? (not back?)))
  (cond [reverse?  (values vtx4 vtx3 vtx2 vtx1)]
        [else      (values vtx1 vtx2 vtx3 vtx4)]))

(: cylinder-wall-shape->triangle-mesh-shape (-> cylinder-wall-shape triangle-mesh-shape))
(define (cylinder-wall-shape->triangle-mesh-shape s)
  (define-values (vtx1 vtx2 vtx3 vtx4) (cylinder-wall-shape->vtxs s))
  (make-quad-triangle-mesh-shape (vector vtx1 vtx2 vtx3 vtx4) #f))

(: cylinder-wall-shape-mesh (-> cylinder-wall-shape triangle-mesh-shape))
(define (cylinder-wall-shape-mesh s)
  (force (cylinder-wall-shape-lazy-mesh s)))

;; ===================================================================================================
;; Set attributes

(: set-cylinder-shape-color (-> shape FlV4 cylinder-shape))
(define (set-cylinder-shape-color s c)
  (match-define (cylinder-shape _ _ t r a _ e m inside?) s)
  (make-cylinder-shape t r a c e m inside?))

(: set-cylinder-shape-emitted (-> shape FlV4 cylinder-shape))
(define (set-cylinder-shape-emitted s e)
  (match-define (cylinder-shape _ _ t r a c _ m inside?) s)
  (make-cylinder-shape t r a c e m inside?))

(: set-cylinder-shape-material (-> shape FlV4 cylinder-shape))
(define (set-cylinder-shape-material s m)
  (match-define (cylinder-shape _ _ t r a c e _ inside?) s)
  (make-cylinder-shape t r a c e m inside?))

(: set-cylinder-wall-shape-color (-> shape FlV4 cylinder-wall-shape))
(define (set-cylinder-wall-shape-color s c)
  (match-define (cylinder-wall-shape _ _ t r11 r12 r21 r22 a _ e m back? _) s)
  (make-cylinder-wall-shape t r11 r12 r21 r22 a c e m back?))

(: set-cylinder-wall-shape-emitted (-> shape FlV4 cylinder-wall-shape))
(define (set-cylinder-wall-shape-emitted s e)
  (match-define (cylinder-wall-shape _ _ t r11 r12 r21 r22 a c _ m back? _) s)
  (make-cylinder-wall-shape t r11 r12 r21 r22 a c e m back?))

(: set-cylinder-wall-shape-material (-> shape FlV4 cylinder-wall-shape))
(define (set-cylinder-wall-shape-material s m)
  (match-define (cylinder-wall-shape _ _ t r11 r12 r21 r22 a c e _ back? _) s)
  (make-cylinder-wall-shape t r11 r12 r21 r22 a c e m back?))

;; ===================================================================================================
;; Program for pass 1: material

(define cylinder-mat-vertex-attributes
  (list (attribute "" 'vec4 "t0")
        (attribute "" 'vec4 "t1")
        (attribute "" 'vec4 "t2")
        (attribute "" 'vec2 "vert_top_radius_max_angle")
        (attribute "" 'vec4/bytes "vert_roughness_inside_id")  ; vec4(roughness, inside, id, 0)
        ))

(define cylinder-mat-fragment-attributes
  (list (attribute "flat" 'vec4 "frag_trans_z")
        (attribute "flat" 'mat3 "frag_untrans")
        (attribute "flat" 'float "frag_top_radius")
        (attribute "flat" 'float "frag_max_angle")
        (attribute "flat" 'float "frag_roughness")
        (attribute "flat" 'float "frag_inside")
        (attribute "smooth" 'vec3 "frag_start")
        (attribute "smooth" 'vec3 "frag_dir")))

(define output-unit-cube-vertex2-code
  (make-partial-code
   "output-unit-cube-vertex"
   #:standard-uniforms
   (list (standard-uniform "" 'float "znear" 'znear)
         (standard-uniform "" 'float "zfar" 'zfar))
   #:definitions
   (list #<<code
void output_unit_cube_vertex2(mat4 trans, mat4 proj, int vertex_id) {
  vec4 p = vec4(mix(-1.0, +1.0, vertex_id & 1),
                mix(-1.0, +1.0, (vertex_id & 2) >> 1),
                mix(-1.0, +1.0, (vertex_id & 4) >> 2),
                1.0);
  float z1 = (proj * trans * vec4(-1.0,-1.0,-1.0,1.0)).z;
  float z2 = (proj * trans * vec4(+1.0,-1.0,-1.0,1.0)).z;
  float z3 = (proj * trans * vec4(-1.0,+1.0,-1.0,1.0)).z;
  float z4 = (proj * trans * vec4(+1.0,+1.0,-1.0,1.0)).z;
  float z5 = (proj * trans * vec4(-1.0,-1.0,+1.0,1.0)).z;
  float z6 = (proj * trans * vec4(+1.0,-1.0,+1.0,1.0)).z;
  float z7 = (proj * trans * vec4(-1.0,+1.0,+1.0,1.0)).z;
  float z8 = (proj * trans * vec4(+1.0,+1.0,+1.0,1.0)).z;
  float zmin = min(min(min(z1,z2),min(z3,z4)),min(min(z5,z6),min(z7,z8)));
  float zmax = max(max(max(z1,z2),max(z3,z4)),max(max(z5,z6),max(z7,z8)));
  if (zmax < znear || zmin > zfar) {
    gl_Position = vec4(0.0);
  } else if (zmin < znear && zmax >= znear) {
    // Let OpenGL do the perspective divide and clip, and hope for the best
    gl_Position = proj * trans * p;
  } else {
    // Do the perspective divide ourselves
    // For some reason, letting OpenGL do it causes shapes to flicker when far from the camera
    p = proj * trans * p;
    gl_Position = vec4(p.xy / p.w, 0.0, 1.0);
  }
}
code
         )))

(define-singleton (cylinder-mat-vertex-code)
  (make-vertex-code
   "cylinder-mat-vertex-30"
   #:includes
   (list output-unit-cube-vertex2-code
         model-vertex-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "view" 'view)
         (standard-uniform "" 'mat4 "unview" 'unview)
         (standard-uniform "" 'mat4 "proj" 'proj)
         (standard-uniform "" 'mat4 "unproj" 'unproj))
   #:in-attributes
   cylinder-mat-vertex-attributes
   #:out-attributes
   cylinder-mat-fragment-attributes
   #<<code
mat4x3 cylinder = rows2mat4x3(t0, t1, t2);
mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(cylinder));
mat4x3 unmodel = affine_inverse(model);
mat4 trans = view * a2p(model);
mat4 untrans = a2p(unmodel) * unview;

frag_trans_z = transpose(trans)[2];
frag_untrans = mat3(untrans);
frag_top_radius = vert_top_radius_max_angle.x;
frag_max_angle = vert_top_radius_max_angle.y;
frag_roughness = vert_roughness_inside_id.x / 255;
frag_inside = vert_roughness_inside_id.y;
int vert_id = int(vert_roughness_inside_id.z);
output_unit_cube_vertex2(trans, proj, vert_id);

vec4 dir = unproj * gl_Position;
frag_dir = mat3(untrans) * (dir.xyz / dir.w);
frag_start = untrans[3].xyz;
code
   ))

(define-singleton (cylinder-mat-fragment-code)
  (make-fragment-code
   "cylinder-mat-fragment-30"
   #:includes
   (list output-mat-fragment-code
         ray-trace-fragment-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "proj" 'proj))
   #:in-attributes
   cylinder-mat-fragment-attributes
   #<<code
vec3 start = frag_start;
vec3 dir = normalize(frag_dir);
vec2 ts = unit_cylinder_intersect(start, dir, frag_top_radius);
float t = mix(ts.x, ts.y, frag_inside);
// many nearby fragments should discard if this one does
if (t <= 0.0) discard;
  
vec3 pos = start + dir * t;
float angle = atan(pos.y, pos.x);
angle = angle < 0.0 ? angle + 6.28318530718 : angle;
if (angle > frag_max_angle) discard;

float vpos_z = dot(frag_trans_z, vec4(pos,1.0));  // transformed pos z coord only
vec3 vnorm = vec3(normalize(pos.xy), 0.5 * (1.0 - frag_top_radius)) * frag_untrans;
output_mat(mix(vnorm,-vnorm,frag_inside), frag_roughness, vpos_z);
code
   ))

(define-singleton (cylinder-mat-program-code)
  (make-program-code
   "cylinder-mat-program-30"
   (cylinder-mat-vertex-code)
   (cylinder-mat-fragment-code)))

(define-singleton/context (cylinder-mat-program)
  (log-pict3d-info "<engine> creating cylinder material program for OpenGL >= 30")
  (program-code->gl-program (cylinder-mat-program-code)))

;; ===================================================================================================
;; Program for pass 2: color

(define cylinder-draw-vertex-attributes
  (list (attribute "" 'vec4 "t0")
        (attribute "" 'vec4 "t1")
        (attribute "" 'vec4 "t2")
        (attribute "" 'vec2 "vert_top_radius_max_angle")
        ;; vec4(r, g, b, a)
        (attribute "" 'vec4/bytes "vert_rcolor")
        ;; vec4(r, g, intensity.lo, intensity.hi)
        (attribute "" 'vec4/bytes "vert_ecolor")
        ;; vec4(ambient, diffuse, specular, inside | id)
        (attribute "" 'vec4/bytes "vert_material_inside_id")
        ))

(define cylinder-draw-fragment-attributes
  (list (attribute "flat" 'vec4 "frag_trans_z")
        (attribute "flat" 'float "frag_top_radius")
        (attribute "flat" 'float "frag_max_angle")
        (attribute "flat" 'vec3 "frag_rcolor")
        (attribute "flat" 'vec3 "frag_ecolor")
        (attribute "flat" 'float "frag_alpha")
        (attribute "flat" 'float "frag_ambient")
        (attribute "flat" 'float "frag_diffuse")
        (attribute "flat" 'float "frag_specular")
        (attribute "flat" 'float "frag_inside")
        (attribute "smooth" 'vec3 "frag_start")
        (attribute "smooth" 'vec3 "frag_dir")))

(define-singleton (cylinder-draw-vertex-code)
  (make-vertex-code
   "cylinder-draw-vertex-30"
   #:includes
   (list output-unit-cube-vertex2-code
         model-vertex-code
         unpack-emitted-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "view" 'view)
         (standard-uniform "" 'mat4 "unview" 'unview)
         (standard-uniform "" 'mat4 "proj" 'proj)
         (standard-uniform "" 'mat4 "unproj" 'unproj))
   #:in-attributes
   cylinder-draw-vertex-attributes
   #:out-attributes
   cylinder-draw-fragment-attributes
   #<<code
mat4x3 cylinder = rows2mat4x3(t0, t1, t2);
mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(cylinder));
mat4x3 unmodel = affine_inverse(model);
mat4 trans = view * a2p(model);
mat4 untrans = a2p(unmodel) * unview;

frag_trans_z = transpose(trans)[2];
frag_top_radius = vert_top_radius_max_angle.x;
frag_max_angle = vert_top_radius_max_angle.y;
frag_rcolor = pow(vert_rcolor.rgb / 255, vec3(2.2));
frag_alpha = vert_rcolor.a / 255;
frag_ecolor = unpack_emitted(vert_ecolor);
frag_ambient = vert_material_inside_id.x / 255;
frag_diffuse = vert_material_inside_id.y / 255;
frag_specular = vert_material_inside_id.z / 255;
int inside_id = int(vert_material_inside_id.w);
frag_inside = (inside_id & 8) >> 3;
int vert_id = inside_id & 7;
output_unit_cube_vertex2(trans, proj, vert_id);

vec4 dir = unproj * gl_Position;
frag_dir = mat3(untrans) * (dir.xyz / dir.w);
frag_start = untrans[3].xyz;
code
   ))

(define-singleton (cylinder-opaq-fragment-code)
  (make-fragment-code
   "cylinder-opaq-fragment-30"
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
   cylinder-draw-fragment-attributes
   #<<code
vec3 start = frag_start;
vec3 dir = normalize(frag_dir);
vec2 ts = unit_cylinder_intersect(start, dir, frag_top_radius);
float t = mix(ts.x, ts.y, frag_inside);
// many nearby fragments should discard if this one does
if (t <= 0.0) discard;

vec3 pos = start + dir * t;
float angle = atan(pos.y, pos.x);
angle = angle < 0.0 ? angle + 6.28318530718 : angle;
if (angle > frag_max_angle) discard;

float vpos_z = dot(frag_trans_z, vec4(pos,1.0));

ivec2 xy = ivec2(gl_FragCoord.xy);
vec3 diff = texelFetch(diffuse, xy, 0).rgb;
vec3 spec = texelFetch(specular, xy, 0).rgb;
vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
vec3 color = frag_ecolor + frag_rcolor.rgb * light;
output_opaq(color, vpos_z);
code
   ))

(define-singleton (cylinder-tran-fragment-code)
  (make-fragment-code
   "cylinder-tran-fragment-30"
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
   cylinder-draw-fragment-attributes
   #<<code
vec3 start = frag_start;
vec3 dir = normalize(frag_dir);
vec2 ts = unit_cylinder_intersect(start, dir, frag_top_radius);
float t = mix(ts.x, ts.y, frag_inside);
// many nearby fragments should discard if this one does
if (t <= 0.0) discard;

vec3 pos = start + dir * t;
float angle = atan(pos.y, pos.x);
angle = angle < 0.0 ? angle + 6.28318530718 : angle;
if (angle > frag_max_angle) discard;

float vpos_z = dot(frag_trans_z, vec4(pos,1.0));

ivec2 xy = ivec2(gl_FragCoord.xy);
vec3 diff = texelFetch(diffuse, xy, 0).rgb;
vec3 spec = texelFetch(specular, xy, 0).rgb;
vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
vec3 color = frag_ecolor + frag_rcolor.rgb * light;
output_tran(color, frag_alpha, vpos_z);
code
   ))

(define-singleton (cylinder-opaq-program-code)
  (make-program-code
   "cylinder-opaq-program-30"
   (cylinder-draw-vertex-code)
   (cylinder-opaq-fragment-code)))

(define-singleton (cylinder-tran-program-code)
  (make-program-code
   "cylinder-tran-program-30"
   (cylinder-draw-vertex-code)
   (cylinder-tran-fragment-code)))

(define-singleton/context (cylinder-opaq-program)
  (log-pict3d-info "<engine> creating cylinder opaque color pass program for OpenGL >= 30")
  (program-code->gl-program (cylinder-opaq-program-code)))

(define-singleton/context (cylinder-tran-program)
  (log-pict3d-info "<engine> creating cylinder transparent color pass program for OpenGL >= 30")
  (program-code->gl-program (cylinder-tran-program-code)))

;; ===================================================================================================
;; Sphere shape passes

#|
Rectangle indexes

   6------7
  /|     /|
 / |    / |
4------5  |
|  |   |  |
|  2---|--3
| /    | /
|/     |/
0------1
|#

(define cylinder-idxs
  ((inst vector Index)
   ;; Bottom face
   0 2 1
   1 2 3
   ;; Front face
   0 1 4
   4 1 5
   ;; Right face
   1 3 5
   5 3 7
   ;; Back face
   3 2 7
   7 2 6
   ;; Left face
   0 4 2
   2 4 6
   ;; Top face
   4 5 6
   6 5 7
   ))

(define reverse-cylinder-idxs
  (vector-reverse cylinder-idxs))

(: get-cylinder-shape-passes (-> shape passes))
(define (get-cylinder-shape-passes s)
  (match-define (cylinder-shape _ _ t r a c e m inside?) s)
  
  (define mat-size (program-code-vao-size (cylinder-mat-program-code)))
  (define mat-data (make-bytes (* 8 mat-size)))
  (let* ([i  (serialize-affine mat-data 0 t)]
         [i  (serialize-float mat-data i r)]
         [i  (serialize-float mat-data i a)]
         [i  (serialize-float/byte mat-data i (unsafe-flv4-ref m 3))]
         [i  (serialize-byte mat-data i (if inside? 1 0))])
    (define mat-data-ptr (u8vector->cpointer mat-data))
    (for ([k : Nonnegative-Fixnum  (in-range 1 8)])
      (memcpy mat-data-ptr (unsafe-fx* k mat-size) mat-data-ptr mat-size _byte)
      (bytes-set! mat-data (unsafe-fx+ (unsafe-fx* k mat-size) i) k)))
  
  (define inside-bit (if inside? #b1000 0))
  
  (define draw-size (program-code-vao-size (cylinder-opaq-program-code)))
  (define draw-data (make-bytes (* 8 draw-size)))
  (let* ([i  (serialize-affine draw-data 0 t)]
         [i  (serialize-float draw-data i r)]
         [i  (serialize-float draw-data i a)]
         [i  (serialize-vec4/bytes draw-data i c)]
         [i  (serialize-emitted/bytes draw-data i e)]
         [i  (serialize-material-reflectances/bytes draw-data i m)]
         [j  i]  ; offset of inside|id
         [i  (serialize-byte draw-data i inside-bit)])
    (define draw-data-ptr (u8vector->cpointer draw-data))
    (for ([k : Nonnegative-Fixnum  (in-range 1 8)])
      (memcpy draw-data-ptr (unsafe-fx* k draw-size) draw-data-ptr draw-size _byte)
      (bytes-set! draw-data (unsafe-fx+ (unsafe-fx* k draw-size) j)
                  (bitwise-ior inside-bit k))))
  
  (define reverse? (if (flt3consistent? t) inside? (not inside?)))
  ;; Always drawing the inside faces keeps OpenGL from clipping the cylinder away when the camera
  ;; is near an edge or a corner
  (define idxs (if #t #;reverse? reverse-cylinder-idxs cylinder-idxs))
  
  (define mat-verts  (vertices 8 mat-data  idxs))
  (define draw-verts (vertices 8 draw-data idxs))
  
  (if (< (flv4-ref c 3) 1.0)
      (passes
       #()
       #()
       #()
       (vector (shape-params cylinder-mat-program empty #f GL_TRIANGLES mat-verts))
       (vector (shape-params cylinder-tran-program empty #f GL_TRIANGLES draw-verts)))
      (passes
       #()
       (vector (shape-params cylinder-mat-program empty #f GL_TRIANGLES mat-verts))
       (vector (shape-params cylinder-opaq-program empty #f GL_TRIANGLES draw-verts))
       #()
       #()))
  )

(: get-cylinder-wall-shape-passes (-> shape passes))
(define (get-cylinder-wall-shape-passes s)
  (get-triangle-mesh-shape-passes (cylinder-wall-shape-mesh (assert s cylinder-wall-shape?))))

;; ===================================================================================================
;; Bounding box

(define top-t (move-flt3 +z-flv3))
(define bot-t (move-flt3 -z-flv3))

(: get-cylinder-shape-bbox (-> shape FlAffine3 bbox))
(define (get-cylinder-shape-bbox s t)
  (let ([s  (assert s cylinder-shape?)])
    (let* ([t  (flt3compose t (cylinder-shape-affine s))]
           [top-t  (flt3compose t top-t)]
           [bot-t  (flt3compose t bot-t)])
      (bbox (flrect3-join (transformed-disk-flrect3 top-t)
                          (transformed-disk-flrect3 bot-t))
            0.0))))

(: get-cylinder-wall-shape-bbox (-> shape FlAffine3 bbox))
(define (get-cylinder-wall-shape-bbox s t)
  (get-triangle-mesh-shape-bbox (cylinder-wall-shape-mesh (assert s cylinder-wall-shape?)) t))

;; ===================================================================================================
;; Transform

(: cylinder-shape-transform (-> shape FlAffine3 cylinder-shape))
(define (cylinder-shape-transform s t)
  (match-define (cylinder-shape _ _ t0 r a c e m inside?) s)
  (make-cylinder-shape (flt3compose t t0) r a c e m inside?))

(: cylinder-wall-shape-transform (-> shape FlAffine3 cylinder-wall-shape))
(define (cylinder-wall-shape-transform s t)
  (match-define (cylinder-wall-shape _ _ t0 r11 r12 r21 r22 a c e m back? _) s)
  (make-cylinder-wall-shape (flt3compose t t0) r11 r12 r21 r22 a c e m back?))

;; ===================================================================================================
;; Ray intersection

(: cylinder-surface-normal/angle (-> Nonnegative-Flonum Flonum FlV3))
(define (cylinder-surface-normal/angle r θ)
  (define s/2 (* 0.5 (- 1.0 r)))
  (define m (flsqrt (+ 1.0 (sqr s/2))))
  (define nx (/ (cos θ) m))
  (define ny (/ (sin θ) m))
  (define nz (/ s/2 m))
  (flv3 nx ny nz))

(: cylinder-surface-normal (-> Nonnegative-Flonum FlV3 (U #f FlV3)))
(define (cylinder-surface-normal r p)
  (define s/2 (* 0.5 (- 1.0 r)))
  (define m (flsqrt (+ 1.0 (sqr s/2))))
  (define px (flv3-ref p 0))
  (define py (flv3-ref p 1))
  (define mm (* m (flsqrt (+ (sqr px) (sqr py)))))
  (define nx (/ px mm))
  (define ny (/ py mm))
  (define nz (/ s/2 m))
  (and (< -inf.0 (min nx ny nz))
       (< (max nx ny nz) +inf.0)
       (flv3 nx ny nz)))

(define cylinder-eps (* 128.0 epsilon.0))

(: flatan2+ (-> Flonum Flonum Flonum))
(define (flatan2+ y x)
  (define θ (atan y x))
  (if (< θ 0.0) (+ θ (* 2.0 pi)) θ))

(: unit-cylinder-line-intersects (-> Nonnegative-Flonum Positive-Flonum
                                     (-> FlV3 FlV3 (Values (U #f Flonum) (U #f Flonum)))))
(define ((unit-cylinder-line-intersects r angle) p d)
  (call/flv3-values p
    (λ (px py pz)
      (call/flv3-values d
        (λ (dx dy dz)
          (define s/2 (* 0.5 (- 1.0 r)))
          (define dz1 (* dz s/2))
          (define pz1 (- 1.0 (* s/2 (+ pz 1.0))))
          (define a (+ (* dx dx) (* dy dy) (- (* dz1 dz1))))
          (define b (- (/ (+ (* px dx) (* py dy) (* dz1 pz1)) a)))
          (define c (/ (+ (* px px) (* py py) (- (* pz1 pz1))) a))
          (define discr (- (sqr b) c))
          (cond
            [(< discr (- cylinder-eps))  (values #f #f)]
            [else
             (define q (* (sgn a) (flsqrt (max 0.0 discr))))
             (define t1 (- b q))
             (define t2 (+ b q))
             (values (and (<= (abs (+ pz (* dz t1))) (+ 1.0 cylinder-eps))
                          (<= (flatan2+ (+ py (* dy t1)) (+ px (* dx t1))) angle)
                          t1)
                     (and (<= (abs (+ pz (* dz t2))) (+ 1.0 cylinder-eps))
                          (<= (flatan2+ (+ py (* dy t2)) (+ px (* dx t2))) angle)
                          t2))]))))))

(: unit-cylinder-intersect-normal (-> Nonnegative-Flonum
                                      (-> FlV3 FlV3 Nonnegative-Flonum (U #f FlV3))))
(define ((unit-cylinder-intersect-normal r) p d time)
  (cylinder-surface-normal r (flv3fma d time p)))

(: cylinder-shape-ray-intersect (-> shape FlV3 FlV3 Nonnegative-Flonum
                                (Values (U #f Nonnegative-Flonum) (U #f (Promise trace-data)))))
(define (cylinder-shape-ray-intersect s v dv max-time)
  (let ([s  (assert s cylinder-shape?)])
    (define r (cylinder-shape-top-radius s))
    (define a (cylinder-shape-max-angle s))
    (transformed-shape-intersect (cylinder-shape-affine s)
                                 (cylinder-shape-inside? s)
                                 v dv max-time
                                 (unit-cylinder-line-intersects r a)
                                 (unit-cylinder-intersect-normal r))))

(: cylinder-wall-shape-ray-intersect (-> shape FlV3 FlV3 Nonnegative-Flonum
                                         (Values (U #f Nonnegative-Flonum)
                                                 (U #f (Promise trace-data)))))
(define (cylinder-wall-shape-ray-intersect s v dv max-time)
  (let ([s  (cylinder-wall-shape-mesh (assert s cylinder-wall-shape?))])
    (triangle-mesh-shape-ray-intersect s v dv max-time)))

;; ===================================================================================================
;; Tessellation

(: make-cylinder-deform-data (-> FlAffine3 Nonnegative-Flonum Boolean deform-data))
(define (make-cylinder-deform-data t r inside?)
  (define tinv (flt3inverse t))
  (if tinv
      (deform-data
        (λ (v1 v2 α)
          (call/flv3-values (flt3apply/pos tinv v1)
            (λ (x1 y1 z1)
              (call/flv3-values (flt3apply/pos tinv v2)
                (λ (x2 y2 z2)
                  (let* ([x  (flblend x1 x2 α)]
                         [y  (flblend y1 y2 α)]
                         [z  (flblend z1 z2 α)]
                         [m1  (flsqrt (+ (* x1 x1) (* y1 y1)))]
                         [m2  (flsqrt (+ (* x2 x2) (* y2 y2)))]
                         [s  (/ (flblend m1 m2 α)
                                (flsqrt (+ (* x x) (* y y))))]
                         [x  (* s x)]
                         [y  (* s y)]
                         [v  (if (and (< -inf.0 (min x y)) (< (max x y) +inf.0))
                                 (flv3 x y z)
                                 (flv3 0.0 0.0 z))]
                         [v  (flt3apply/pos t v)])
                    v))))))
        (λ (vtx1 vtx2 v)
          (let* ([vtx1   (flt3apply/vtx tinv vtx1)]
                 [vtx2   (flt3apply/vtx tinv vtx2)]
                 [v      (flt3apply/pos tinv v)]
                 [vtx12  (vtx-interpolate vtx1 vtx2 v)]
                 [n      (cylinder-surface-normal r v)]
                 [vtx12  (set-vtx-normal vtx12 (if n (if inside? (flv3neg n) n) +z-flv3))]
                 [vtx12  (flt3apply/vtx t vtx12)])
            vtx12)))
      linear-deform-data))

(: cylinder-shape-tessellate (-> shape FlAffine3 Positive-Flonum Nonnegative-Flonum
                                 (Values Null (Listof (face deform-data #f)))))
(define (cylinder-shape-tessellate s t0 max-edge max-angle)
  (match-define (cylinder-shape _ _ t r a c e m inside?) s)
  
  (define min-θnum (min 4 (exact-ceiling (/ a (* 0.5 pi)))))
  (define-values (s1 s2) (if (= a (* 2.0 pi)) (values 0.5 2) (values 1.0 1)))
  (define θnum (max min-θnum (* s2 (exact-ceiling (* s1 (/ a (max min-angle max-angle)))))))
  (define θstep (/ a θnum))
  
  (define θmajor (flaffine3-ellipse-angle-zero t))
  (define rdist
    (let ([t0  (flt3compose t0 t)])
      (define wmin (flt3apply/pos t0 (flv3 (* r (cos θmajor)) (* r (sin θmajor)) 1.0)))
      (define wmax (flt3apply/pos t0 (flv3 (cos θmajor) (sin θmajor) -1.0)))
      (flv3dist wmin wmax)))
  (define rnum (max 1 (exact-ceiling (/ rdist max-edge))))
  (define rstep (/ (fl rnum)))
  
  (define vtxss
    (list->vector
     (for/list : (Listof (Listof vtx)) ([j  (in-range (+ rnum 1))])
       (define h (- 1.0 (* 2.0 (fl j) rstep)))
       (define d (+ r (* (- 1.0 r) (fl j) rstep)))
       (for/list : (Listof vtx) ([i  (in-range (+ θnum 1))])
         (define θ (* (fl i) θstep))
         (define v (let ([v  (flv3 (* d (cos θ)) (* d (sin θ)) h)])
                     (flt3apply/pos t v)))
         (define n (let ([n  (assert (flt3apply/norm t (cylinder-surface-normal/angle r θ))
                                     values)])
                     (if inside? (flv3neg n) n)))
         (vtx v n c e m)))))
  
  (define data (make-cylinder-deform-data t r inside?))
  (define reverse? (if (flt3consistent? t) inside? (not inside?)))
  (define fs
    (append*
     (for/list : (Listof (Listof (face deform-data #f))) ([j1  (in-range rnum)])
       (define vtxs1 (vector-ref vtxss j1))
       (define vtxs2 (vector-ref vtxss (+ j1 1)))
       (append*
        (for/list : (Listof (Listof (face deform-data #f))) ([vtx11  (in-list vtxs1)]
                                                             [vtx12  (in-list (rest vtxs1))]
                                                             [vtx21  (in-list vtxs2)]
                                                             [vtx22  (in-list (rest vtxs2))])
          (if (and (= j1 0) (= r 0.0))
              (let ([vtx1  (vtx-blend vtx11 vtx12 0.5)])
                (make-triangle-face vtx1 vtx21 vtx22 data #f #f #f reverse?))
              (make-quad-faces vtx11 vtx21 vtx22 vtx12 data #f #f #f #f #f #f t0 reverse?)))))))
  
  (values empty fs))

(: cylinder-wall-shape-tessellate (-> shape FlAffine3 Positive-Flonum Nonnegative-Flonum
                                      (Values Null (Listof (face deform-data #f)))))
(define (cylinder-wall-shape-tessellate s t0 max-edge max-angle)
  (match-define (cylinder-wall-shape _ _ t r11 r12 r21 r22 a c e m back? _) s)
  
  (define θmajor (flaffine3-ellipse-angle-zero t))
  (define-values (vdist hdist-top hdist-bot)
    (let ([t0  (flt3compose t0 t)]
          [cθ  (cos θmajor)]
          [sθ  (sin θmajor)])
      (define v1 (flt3apply/pos t0 (flv3 (* r11 cθ) (* r11 sθ) +1.0)))
      (define v2 (flt3apply/pos t0 (flv3 (* r21 cθ) (* r21 sθ) -1.0)))
      (define v3 (flt3apply/pos t0 (flv3 (* r22 cθ) (* r22 sθ) -1.0)))
      (define v4 (flt3apply/pos t0 (flv3 (* r12 cθ) (* r12 sθ) +1.0)))
      (values (flv3dist v4 v3) (flv3dist v1 v4) (flv3dist v2 v3))))
  (define vnum (max 1 (exact-ceiling (/ vdist max-edge))))
  (define hnum-top (max 1 (exact-ceiling (/ hdist-top max-edge))))
  (define hnum-bot (max 1 (exact-ceiling (/ hdist-bot max-edge))))
  
  (define-values (vtx1 vtx2 vtx3 vtx4)
    (let ([ca  (cos a)]
          [sa  (sin a)])
      (define v1 (flt3apply/pos t (flv3 (* r11 ca) (* r11 sa) +1.0)))
      (define v2 (flt3apply/pos t (flv3 (* r21 ca) (* r21 sa) -1.0)))
      (define v3 (flt3apply/pos t (flv3 (* r22 ca) (* r22 sa) -1.0)))
      (define v4 (flt3apply/pos t (flv3 (* r12 ca) (* r12 sa) +1.0)))
      (define n (let ([n  (assert (flt3apply/norm t (flv3 sa (- ca) 0.0)) values)])
                  (if back? (flv3neg n) n)))
      (values (vtx v1 n c e m) (vtx v2 n c e m) (vtx v3 n c e m) (vtx v4 n c e m))))
  
  (define vtxss
    (list->vector
     (for/list : (Listof (Vectorof vtx)) ([j  (in-range (+ vnum 1))])
       (define α (/ (fl j) (fl vnum)))
       (define vtx12 (vtx-blend vtx1 vtx2 α))
       (define vtx43 (vtx-blend vtx4 vtx3 α))
       (define hnum (exact-round (+ (* (- 1.0 α) hnum-top) (* α hnum-bot))))
       (list->vector
        (for/list : (Listof vtx) ([i  (in-range (+ hnum 1))])
          (define β (/ (fl i) (fl hnum)))
          (vtx-blend vtx12 vtx43 β))))))
  
  (define data linear-deform-data)
  
  (define reverse? (if (flt3consistent? t) back? (not back?)))
  (define fs
    (append*
     (for/list : (Listof (Listof (face deform-data #f))) ([j1  (in-range vnum)])
       (define vtxs1 (vector-ref vtxss j1))
       (define vtxs2 (vector-ref vtxss (+ j1 1)))
       (define n1 (vector-length vtxs1))
       (define n2 (vector-length vtxs2))
       (append*
        (for/list : (Listof (Listof (face deform-data #f))) ([i11  (in-range (- n1 1))]
                                                             [i12  (in-range 1 n1)])
          (define i21 (max 0 (min (- n2 1) (exact-round (* (/ n2 n1) i11)))))
          (define i22 (max 0 (min (- n2 1) (exact-round (* (/ n2 n1) i12)))))
          (cond
            [(= i22 i21)
             (define vtx11 (vector-ref vtxs1 i11))
             (define vtx12 (vector-ref vtxs1 i12))
             (define vtx21 (vector-ref vtxs2 i21))
             (make-triangle-face vtx11 vtx21 vtx12 data #f #f #f reverse?)]
            [(= i22 (+ i21 1))
             (define vtx11 (vector-ref vtxs1 i11))
             (define vtx12 (vector-ref vtxs1 i12))
             (define vtx21 (vector-ref vtxs2 i21))
             (define vtx22 (vector-ref vtxs2 i22))
             (make-quad-faces vtx11 vtx21 vtx22 vtx12 data #f #f #f #f #f #f t0 reverse?)]
            [(= i22 (+ i21 2))
             (define vtx11 (vector-ref vtxs1 i11))
             (define vtx12 (vector-ref vtxs1 i12))
             (define vtx21 (vector-ref vtxs2 i21))
             (define vtx22 (vector-ref vtxs2 (+ i21 1)))
             (define vtx23 (vector-ref vtxs2 i22))
             (append (make-triangle-face vtx11 vtx21 vtx22 data #f #f #f reverse?)
                     (make-triangle-face vtx11 vtx22 vtx12 data #f #f #f reverse?)
                     (make-triangle-face vtx12 vtx22 vtx23 data #f #f #f reverse?))]
            [else
             ;; Never been able to make this happen - might be impossible
             empty]))))))
  
  (values empty fs))

;; ===================================================================================================
;; Deform

(: cylinder-shape-deform (-> shape FlSmooth3 (U Null (List cylinder-shape))))
(define (cylinder-shape-deform s t0)
  (match-define (cylinder-shape _ _ t r a c e m inside?) s)
  (let ([t  (fls3apply/affine t0 t)])
    (if t (list (make-cylinder-shape t r a c e m inside?)) empty)))

(: cylinder-wall-shape-deform (-> shape FlSmooth3 (U Null (List cylinder-wall-shape))))
(define (cylinder-wall-shape-deform s t0)
  (match-define (cylinder-wall-shape _ _ t r11 r12 r21 r22 a c e m inside? _) s)
  (let ([t  (fls3apply/affine t0 t)])
    (if t (list (make-cylinder-wall-shape t r11 r12 r21 r22 a c e m inside?)) empty)))

;; ===================================================================================================

(define cylinder-shape-functions
  (deform-shape-functions
    get-cylinder-shape-passes
    (λ (s kind t) (and (eq? kind 'visible) (get-cylinder-shape-bbox s t)))
    cylinder-shape-transform
    (λ (s t) (list (cylinder-shape-transform s t)))
    cylinder-shape-ray-intersect
    set-cylinder-shape-color
    set-cylinder-shape-emitted
    set-cylinder-shape-material
    default-extract-faces
    cylinder-shape-tessellate
    cylinder-shape-deform))

(define cylinder-wall-shape-functions
  (deform-shape-functions
    get-cylinder-wall-shape-passes
    (λ (s kind t) (and (eq? kind 'visible) (get-cylinder-wall-shape-bbox s t)))
    cylinder-wall-shape-transform
    (λ (s t) (list (cylinder-wall-shape-transform s t)))
    cylinder-wall-shape-ray-intersect
    set-cylinder-wall-shape-color
    set-cylinder-wall-shape-emitted
    set-cylinder-wall-shape-material
    default-extract-faces
    cylinder-wall-shape-tessellate
    cylinder-wall-shape-deform))
