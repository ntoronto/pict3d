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
         "types.rkt")

(provide make-disk-shape
         (struct-out disk-shape))

(struct disk-shape shape
  ([affine : FlAffine3]
   [inner-radius : Nonnegative-Flonum]
   [z-offset : Flonum]
   [max-angle : Positive-Flonum]
   [color : FlV4]
   [emitted : FlV4]
   [material : FlV4]
   [back? : Boolean])
  #:transparent)

;; ===================================================================================================
;; Constructors

(define 2pi (assert (* 2.0 pi) positive?))

(: make-disk-shape (-> FlAffine3 Nonnegative-Flonum Flonum Positive-Flonum FlV4 FlV4 FlV4 Boolean
                       disk-shape))
(define (make-disk-shape t r z a c e m back?)
  (disk-shape (lazy-passes) disk-shape-functions t r z (min 2pi a) c e m back?))

;; ===================================================================================================
;; Set attributes

(: set-disk-shape-color (-> shape FlV4 disk-shape))
(define (set-disk-shape-color s c)
  (match-define (disk-shape _ _ t r z a _ e m back?) s)
  (make-disk-shape t r z a c e m back?))

(: set-disk-shape-emitted (-> shape FlV4 disk-shape))
(define (set-disk-shape-emitted s e)
  (match-define (disk-shape _ _ t r z a c _ m back?) s)
  (make-disk-shape t r z a c e m back?))

(: set-disk-shape-material (-> shape FlV4 disk-shape))
(define (set-disk-shape-material s m)
  (match-define (disk-shape _ _ t r z a c e _ back?) s)
  (make-disk-shape t r z a c e m back?))

;; ===================================================================================================
;; Program for pass 1: material

(define disk-mat-vertex-attributes
  (list (attribute "" 'vec4 "t0")
        (attribute "" 'vec4 "t1")
        (attribute "" 'vec4 "t2")
        (attribute "" 'vec2 "vert_inner_radius_max_angle")
        (attribute "" 'vec4/bytes "vert_roughness_back_id")  ; vec4(roughness, back, id, 0)
        ))

(define disk-mat-fragment-attributes
  (list (attribute "flat" 'vec4 "frag_trans_z")
        (attribute "flat" 'mat3 "frag_untrans")
        (attribute "flat" 'float "frag_inner_radius")
        (attribute "flat" 'float "frag_max_angle")
        (attribute "flat" 'float "frag_roughness")
        (attribute "flat" 'float "frag_back")
        (attribute "smooth" 'vec3 "frag_start")
        (attribute "smooth" 'vec3 "frag_dir")))

(define output-unit-quad-vertex2-code
  (make-partial-code
   "output-unit-quad-vertex"
   #:standard-uniforms
   (list (standard-uniform "" 'float "znear" 'znear)
         (standard-uniform "" 'float "zfar" 'zfar))
   #:definitions
   (list #<<code
void output_unit_quad_vertex2(mat4 trans, mat4 proj, int vertex_id) {
  vec4 p = vec4(mix(-1.0, +1.0, vertex_id & 1),
                mix(-1.0, +1.0, (vertex_id & 2) >> 1),
                0.0,
                1.0);
  float z1 = (proj * trans * vec4(-1.0,-1.0,0.0,1.0)).z;
  float z2 = (proj * trans * vec4(+1.0,-1.0,0.0,1.0)).z;
  float z3 = (proj * trans * vec4(-1.0,+1.0,0.0,1.0)).z;
  float z4 = (proj * trans * vec4(+1.0,+1.0,0.0,1.0)).z;
  float zmin = min(min(z1,z2),min(z3,z4));
  float zmax = max(max(z1,z2),max(z3,z4));
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

(define-singleton (disk-mat-vertex-code)
  (make-vertex-code
   "disk-mat-vertex-30"
   #:includes
   (list output-unit-quad-vertex2-code
         model-vertex-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "view" 'view)
         (standard-uniform "" 'mat4 "unview" 'unview)
         (standard-uniform "" 'mat4 "proj" 'proj)
         (standard-uniform "" 'mat4 "unproj" 'unproj))
   #:in-attributes
   disk-mat-vertex-attributes
   #:out-attributes
   disk-mat-fragment-attributes
   #<<code
mat4x3 disk = rows2mat4x3(t0, t1, t2);
mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(disk));
mat4x3 unmodel = affine_inverse(model);
mat4 trans = view * a2p(model);
mat4 untrans = a2p(unmodel) * unview;

frag_trans_z = transpose(trans)[2];
frag_untrans = mat3(untrans);
frag_inner_radius = vert_inner_radius_max_angle.x;
frag_max_angle = vert_inner_radius_max_angle.y;
frag_roughness = vert_roughness_back_id.x / 255;
frag_back = vert_roughness_back_id.y;
int vert_id = int(vert_roughness_back_id.z);
output_unit_quad_vertex2(trans, proj, vert_id);

vec4 dir = unproj * gl_Position;
frag_dir = mat3(untrans) * (dir.xyz / dir.w);
frag_start = untrans[3].xyz;
code
   ))

(define-singleton (disk-mat-fragment-code)
  (make-fragment-code
   "disk-mat-fragment-30"
   #:includes
   (list output-mat-fragment-code
         ray-trace-fragment-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "proj" 'proj))
   #:in-attributes
   disk-mat-fragment-attributes
   #<<code
vec3 start = frag_start;
vec3 dir = normalize(frag_dir);
vec2 ts = unit_disk_intersect(start, dir, frag_inner_radius);
float t = mix(ts.x, ts.y, frag_back);
// many nearby fragments should discard if this one does
if (t <= 0.0) discard;

vec3 pos = start + dir * t;
float angle = atan(pos.y, pos.x);
angle = angle < 0.0 ? angle + 6.28318530718 : angle;
if (angle > frag_max_angle) discard;

float vpos_z = dot(frag_trans_z, vec4(pos,1.0));  // transformed pos z coord only
vec3 vnorm = vec3(0,0,1) * frag_untrans;
output_mat(mix(vnorm,-vnorm,frag_back), frag_roughness, vpos_z);
code
   ))

(define-singleton (disk-mat-program-code)
  (make-program-code
   "disk-mat-program-30"
   (disk-mat-vertex-code)
   (disk-mat-fragment-code)))

(define-singleton/context (disk-mat-program)
  (log-pict3d-info "<engine> creating disk material program for OpenGL >= 30")
  (program-code->gl-program (disk-mat-program-code)))

;; ===================================================================================================
;; Program for pass 2: color

(define disk-draw-vertex-attributes
  (list (attribute "" 'vec4 "t0")
        (attribute "" 'vec4 "t1")
        (attribute "" 'vec4 "t2")
        (attribute "" 'vec2 "vert_inner_radius_max_angle")
        ;; vec4(r, g, b, a)
        (attribute "" 'vec4/bytes "vert_rcolor")
        ;; vec4(r, g, intensity.lo, intensity.hi)
        (attribute "" 'vec4/bytes "vert_ecolor")
        ;; vec4(ambient, diffuse, specular, back | id)
        (attribute "" 'vec4/bytes "vert_material_back_id")
        ))

(define disk-draw-fragment-attributes
  (list (attribute "flat" 'vec4 "frag_trans_z")
        (attribute "flat" 'float "frag_inner_radius")
        (attribute "flat" 'float "frag_max_angle")
        (attribute "flat" 'vec3 "frag_rcolor")
        (attribute "flat" 'vec3 "frag_ecolor")
        (attribute "flat" 'float "frag_alpha")
        (attribute "flat" 'float "frag_ambient")
        (attribute "flat" 'float "frag_diffuse")
        (attribute "flat" 'float "frag_specular")
        (attribute "flat" 'float "frag_back")
        (attribute "smooth" 'vec3 "frag_start")
        (attribute "smooth" 'vec3 "frag_dir")))

(define-singleton (disk-draw-vertex-code)
  (make-vertex-code
   "disk-draw-vertex-30"
   #:includes
   (list output-unit-quad-vertex2-code
         model-vertex-code
         unpack-emitted-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "view" 'view)
         (standard-uniform "" 'mat4 "unview" 'unview)
         (standard-uniform "" 'mat4 "proj" 'proj)
         (standard-uniform "" 'mat4 "unproj" 'unproj))
   #:in-attributes
   disk-draw-vertex-attributes
   #:out-attributes
   disk-draw-fragment-attributes
   #<<code
mat4x3 disk = rows2mat4x3(t0, t1, t2);
mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(disk));
mat4x3 unmodel = affine_inverse(model);
mat4 trans = view * a2p(model);
mat4 untrans = a2p(unmodel) * unview;

frag_trans_z = transpose(trans)[2];
frag_inner_radius = vert_inner_radius_max_angle.x;
frag_max_angle = vert_inner_radius_max_angle.y;
frag_rcolor = pow(vert_rcolor.rgb / 255, vec3(2.2));
frag_alpha = vert_rcolor.a / 255;
frag_ecolor = unpack_emitted(vert_ecolor);
frag_ambient = vert_material_back_id.x / 255;
frag_diffuse = vert_material_back_id.y / 255;
frag_specular = vert_material_back_id.z / 255;
int back_id = int(vert_material_back_id.w);
frag_back = (back_id & 4) >> 2;
int vert_id = back_id & 3;
output_unit_quad_vertex2(trans, proj, vert_id);

vec4 dir = unproj * gl_Position;
frag_dir = mat3(untrans) * (dir.xyz / dir.w);
frag_start = untrans[3].xyz;
code
   ))

(define-singleton (disk-opaq-fragment-code)
  (make-fragment-code
   "disk-opaq-fragment-30"
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
   disk-draw-fragment-attributes
   #<<code
vec3 start = frag_start;
vec3 dir = normalize(frag_dir);
vec2 ts = unit_disk_intersect(start, dir, frag_inner_radius);
float t = mix(ts.x, ts.y, frag_back);
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

(define-singleton (disk-tran-fragment-code)
  (make-fragment-code
   "disk-tran-fragment-30"
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
   disk-draw-fragment-attributes
   #<<code
vec3 start = frag_start;
vec3 dir = normalize(frag_dir);
vec2 ts = unit_disk_intersect(start, dir, frag_inner_radius);
float t = mix(ts.x, ts.y, frag_back);
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

(define-singleton (disk-opaq-program-code)
  (make-program-code
   "disk-opaq-program-30"
   (disk-draw-vertex-code)
   (disk-opaq-fragment-code)))

(define-singleton (disk-tran-program-code)
  (make-program-code
   "disk-tran-program-30"
   (disk-draw-vertex-code)
   (disk-tran-fragment-code)))

(define-singleton/context (disk-opaq-program)
  (log-pict3d-info "<engine> creating disk opaque color pass program for OpenGL >= 30")
  (program-code->gl-program (disk-opaq-program-code)))

(define-singleton/context (disk-tran-program)
  (log-pict3d-info "<engine> creating disk transparent color pass program for OpenGL >= 30")
  (program-code->gl-program (disk-tran-program-code)))

;; ===================================================================================================
;; Sphere shape passes

(define disk-idxs ((inst vector Index) 0 1 2 2 1 3))
(define reverse-disk-idxs (vector-reverse disk-idxs))

(: move-z-forward-data (-> FlAffine3 Flonum FlVector))
(define (move-z-forward-data t z)
  (call/flaffine3-forward t
    (λ (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23)
      (flvector m00 m01 m02 (+ m03 (* m02 z))
                m10 m11 m12 (+ m13 (* m12 z))
                m20 m21 m22 (+ m23 (* m22 z))))))

(: get-disk-shape-passes (-> shape passes))
(define (get-disk-shape-passes s)
  (match-define (disk-shape _ _ t r z a c e m back?) s)
  
  (define ts (move-z-forward-data t z))
  
  (define mat-size (program-code-vao-size (disk-mat-program-code)))
  (define mat-data (make-bytes (* 4 mat-size)))
  (let* ([i  (serialize-floats mat-data 0 ts 12)]
         [i  (serialize-float mat-data i r)]
         [i  (serialize-float mat-data i a)]
         [i  (serialize-float/byte mat-data i (unsafe-flv4-ref m 3))]
         [i  (serialize-byte mat-data i (if back? 1 0))])
    (define mat-data-ptr (u8vector->cpointer mat-data))
    (for ([k : Nonnegative-Fixnum  (in-range 1 4)])
      (memcpy mat-data-ptr (unsafe-fx* k mat-size) mat-data-ptr mat-size _byte)
      (bytes-set! mat-data (unsafe-fx+ (unsafe-fx* k mat-size) i) k)))
  
  (define back-bit (if back? #b100 0))
  
  (define draw-size (program-code-vao-size (disk-opaq-program-code)))
  (define draw-data (make-bytes (* 4 draw-size)))
  (let* ([i  (serialize-floats draw-data 0 ts 12)]
         [i  (serialize-float draw-data i r)]
         [i  (serialize-float draw-data i a)]
         [i  (serialize-vec4/bytes draw-data i c)]
         [i  (serialize-emitted/bytes draw-data i e)]
         [i  (serialize-material-reflectances/bytes draw-data i m)]
         [j  i]  ; offset of back|id
         [i  (serialize-byte draw-data i back-bit)])
    (define draw-data-ptr (u8vector->cpointer draw-data))
    (for ([k : Nonnegative-Fixnum  (in-range 1 4)])
      (memcpy draw-data-ptr (unsafe-fx* k draw-size) draw-data-ptr draw-size _byte)
      (bytes-set! draw-data (unsafe-fx+ (unsafe-fx* k draw-size) j)
                  (bitwise-ior back-bit k))))
  
  (define reverse? (if (flt3consistent? t) back? (not back?)))
  (define idxs (if reverse? reverse-disk-idxs disk-idxs))
  
  (if (< (flv4-ref c 3) 1.0)
      (passes
       #()
       #()
       #()
       (vector (shape-params disk-mat-program empty #f GL_TRIANGLES (vertices 4 mat-data idxs)))
       (vector (shape-params disk-tran-program empty #f GL_TRIANGLES (vertices 4 draw-data idxs))))
      (passes
       #()
       (vector (shape-params disk-mat-program empty #f GL_TRIANGLES (vertices 4 mat-data idxs)))
       (vector (shape-params disk-opaq-program empty #f GL_TRIANGLES (vertices 4 draw-data idxs)))
       #()
       #()))
  )

;; ===================================================================================================
;; Bounding box

(: get-disk-shape-bbox (-> shape FlAffine3 bbox))
(define (get-disk-shape-bbox s t)
  (let* ([s  (assert s disk-shape?)])
    (define disk-t (flt3compose (disk-shape-affine s)
                                (move-z-flt3 (disk-shape-z-offset s))))
    (bbox (transformed-disk-flrect3 (flt3compose t disk-t))
          0.0)))

;; ===================================================================================================
;; Transform

(: disk-shape-transform (-> shape FlAffine3 disk-shape))
(define (disk-shape-transform s t)
  (match-define (disk-shape _ _ t0 r z a c e m back?) s)
  (make-disk-shape (flt3compose t t0) r z a c e m back?))

;; ===================================================================================================
;; Ray intersection

(define radius-eps (* 128.0 epsilon.0))

(: unit-disk-line-intersect (-> FlV3 FlV3 Nonnegative-Flonum (U #f Flonum)))
(define (unit-disk-line-intersect p d r0)
  ;; Find time until intersection with z = 0 plane
  (define t (- (/ (flv3-ref p 2) (flv3-ref d 2))))
  ;; Find intersection coordinates
  (define x (+ (flv3-ref p 0) (* t (flv3-ref d 0))))
  (define y (+ (flv3-ref p 1) (* t (flv3-ref d 1))))
  ;; Keep if r0 <= sqrt(x^2 + y^2) <= 1
  (and (<= (* r0 r0 (- 1.0 radius-eps))
           (+ (* x x) (* y y))
           (+ 1.0 radius-eps))
       t))

(: disk-shape-ray-intersect (-> shape FlV3 FlV3 Nonnegative-Flonum
                                (Values (U #f Nonnegative-Flonum) (U #f (Promise trace-data)))))
(define (disk-shape-ray-intersect s v dv max-time)
  (let ([s  (assert s disk-shape?)])
    (define t (disk-shape-affine s))
    (define tinv (flt3inverse t))
    (cond [tinv
           (define back? (disk-shape-back? s))
           (define vz (flv3 0.0 0.0 (disk-shape-z-offset s)))
           (define max-angle (disk-shape-max-angle s))
           ;; Convert ray to local coordinates
           (define sv (flv3- (flt3apply/pos tinv v) vz))
           (define sdv (flt3apply/dir tinv dv))
           (define sdz (flv3-ref sdv 2))
           (cond
             ;; Back face culling
             [(= sdz 0.0)                    (values #f #f)]
             [(and (< sdz 0.0) back?)        (values #f #f)]
             [(and (> sdz 0.0) (not back?))  (values #f #f)]
             [else
              ;; Compute intersection
              (define time (unit-disk-line-intersect sv sdv (disk-shape-inner-radius s)))
              (cond [(and time (>= time 0.0) (<= time max-time))
                     ;; Known: time is not +nan.0 (which can happen when dv is zero-flv3)
                     (define sp (flv3fma sdv time sv))
                     (define angle (let ([angle  (atan (flv3-ref sp 1) (flv3-ref sp 0))])
                                     (if (< angle 0.0) (+ angle (* 2.0 pi)) angle)))
                     (cond [(<= angle max-angle)
                            (define data
                              (delay (define p (flv3fma dv time v))
                                     (define n (flt3apply/norm t (if back? -z-flv3 +z-flv3)))
                                     (trace-data p n empty)))
                            (values time data)]
                           [else
                            (values #f #f)])]
                    [else
                     (values #f #f)])])]
          [else
           (values #f #f)])))

;; ===================================================================================================
;; Tessellation

(: make-disk-deform-data (-> FlAffine3 Flonum Boolean deform-data))
(define (make-disk-deform-data t z back?)
  (define tinv (flt3inverse t))
  (define vz (flv3 0.0 0.0 z))
  (if tinv
      (deform-data
        (λ (v1 v2 α)
          (let* ([v1  (flt3apply/pos tinv v1)]
                 [v2  (flt3apply/pos tinv v2)]
                 [v1  (flv3- v1 vz)]
                 [v2  (flv3- v2 vz)]
                 [v  (flv3normalize (flv3blend v1 v2 α))]
                 [v  (if v (flv3* v (flblend (flv3mag v1) (flv3mag v2) α)) zero-flv3)]
                 [v  (flv3+ v vz)]
                 [v  (flt3apply/pos t v)])
            v))
        (λ (vtx1 vtx2 v)
          (let* ([vtx1   (flt3apply/vtx tinv vtx1)]
                 [vtx2   (flt3apply/vtx tinv vtx2)]
                 [v      (flt3apply/pos tinv v)]
                 [vtx12  (vtx-interpolate vtx1 vtx2 v)]
                 [vtx12  (set-vtx-normal vtx12 (if back? -z-flv3 +z-flv3))]
                 [vtx12  (flt3apply/vtx t vtx12)])
            vtx12)))
      linear-deform-data))

(: disk-shape-tessellate (-> shape FlAffine3 Positive-Flonum Nonnegative-Flonum
                             (Values Null (Listof (face deform-data #f)))))
(define (disk-shape-tessellate s t0 max-edge max-angle)
  (match-define (disk-shape _ _ t r z a c e m back?) s)
  
  (define min-θnum (min 4 (exact-ceiling (/ a (* 0.5 pi)))))
  (define-values (s1 s2) (if (= a (* 2.0 pi)) (values 0.5 2) (values 1.0 1)))
  (define θnum (max min-θnum (* s2 (exact-ceiling (* s1 (/ a (max min-angle max-angle)))))))
  (define θstep (/ a θnum))
  
  (define θmajor (flaffine3-ellipse-angle-zero t))
  (define rdist
    (let ([t0  (flt3compose t0 t)])
      (define wmin (flt3apply/pos t0 (flv3 (* r (cos θmajor)) (* r (sin θmajor)) z)))
      (define wmax (flt3apply/pos t0 (flv3 (cos θmajor) (sin θmajor) z)))
      (flv3dist wmin wmax)))
  (define rnum (max 1 (exact-ceiling (/ rdist max-edge))))
  (define rstep (/ (- 1.0 r) rnum))
  
  (define n (assert (flt3apply/norm t (if back? -z-flv3 +z-flv3)) values))
  (define vtx0 (vtx (flt3apply/pos t (flv3 0.0 0.0 z)) n c e m))
  
  (define vtxss
    (list->vector
     (for/list : (Listof (Listof vtx)) ([j  (in-range (+ rnum 1))])
       (define d (+ r (* (fl j) rstep)))
       (for/list : (Listof vtx) ([i  (in-range (+ θnum 1))])
         (cond
           [(< d 1e-8)  vtx0]
           [else
            (define θ (* (fl i) θstep))
            (define v (flv3 (* d (cos θ)) (* d (sin θ)) z))
            (vtx (flt3apply/pos t v) n c e m)])))))
  
  (define data (make-disk-deform-data t z back?))
  (define reverse? (if (flt3consistent? t) back? (not back?)))
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
          (make-quad-faces vtx11 vtx21 vtx22 vtx12 data #f #f #f #f #f #f t0 reverse?))))))
  
  (values empty fs))

;; ===================================================================================================
;; Warp

(: disk-shape-deform (-> shape FlSmooth3 (U Null (List disk-shape))))
(define (disk-shape-deform s t0)
  (match-define (disk-shape _ _ t r z a c e m back?) s)
  (let ([t  (fls3apply/affine t0 t)])
    (if t (list (make-disk-shape t r z a c e m back?)) empty)))

;; ===================================================================================================

(define disk-shape-functions
  (deform-shape-functions
    get-disk-shape-passes
    (λ (s kind t) (and (eq? kind 'visible) (get-disk-shape-bbox s t)))
    disk-shape-transform
    (λ (s t) (list (disk-shape-transform s t)))
    disk-shape-ray-intersect
    set-disk-shape-color
    set-disk-shape-emitted
    set-disk-shape-material
    default-extract-faces
    disk-shape-tessellate
    disk-shape-deform))
