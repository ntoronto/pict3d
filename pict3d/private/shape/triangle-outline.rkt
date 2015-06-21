#lang typed/racket/base

(require racket/list
         racket/vector
         racket/match
         typed/opengl
         "../math.rkt"
         "../memo.rkt"
         "../engine.rkt"
         "../soup.rkt"
         "../utils.rkt"
         "types.rkt")

(provide make-triangle-outline-shape
         (struct-out triangle-outline-shape))

;; ===================================================================================================
;; Shape data types

(struct triangle-outline-shape shape
  ([vtxs : (Vectorof vtx)]
   [corners : (Vectorof Boolean)]
   [edges : (Vectorof Boolean)]
   [width : Positive-Flonum]
   [back? : Boolean])
  #:transparent)

;; ===================================================================================================
;; Constructors

(: make-triangle-outline-shape (-> (Vectorof vtx) (Vectorof Boolean) (Vectorof Boolean)
                                   Positive-Flonum Boolean
                                   triangle-outline-shape))
(define (make-triangle-outline-shape vtxs corners edges width back?)
  (unless (= (vector-length vtxs) 3)
    (raise-argument-error 'make-triangle-outline-shape "length-3 (Vectorof vtx)"
                          0 vtxs corners edges back?))
  (unless (= (vector-length corners) 3)
    (raise-argument-error 'make-triangle-outline-shape "length-3 (Vectorof Boolean)"
                          1 vtxs corners edges back?))
  (unless (= (vector-length edges) 3)
    (raise-argument-error 'make-triangle-outline-shape "length-3 (Vectorof Boolean)"
                          2 vtxs corners edges back?))
  (triangle-outline-shape (lazy-passes) triangle-outline-shape-functions
                          vtxs corners edges width back?))

;; ===================================================================================================
;; Set attributes

(: set-triangle-outline-shape-color (-> shape FlV4 triangle-outline-shape))
(define (set-triangle-outline-shape-color s c)
  (match-define (triangle-outline-shape _ _ vtxs corners edges width back?) s)
  (triangle-outline-shape (lazy-passes) triangle-outline-shape-functions
                          (vector-map (λ ([v : vtx]) (set-vtx-color v c)) vtxs)
                          corners edges width back?))

(: set-triangle-outline-shape-emitted (-> shape FlV4 triangle-outline-shape))
(define (set-triangle-outline-shape-emitted s e)
  (match-define (triangle-outline-shape _ _ vtxs corners edges width back?) s)
  (triangle-outline-shape (lazy-passes) triangle-outline-shape-functions
                          (vector-map (λ ([v : vtx]) (set-vtx-emitted v e)) vtxs)
                          corners edges width back?))

(: set-triangle-outline-shape-material (-> shape FlV4 triangle-outline-shape))
(define (set-triangle-outline-shape-material s m)
  (match-define (triangle-outline-shape _ _ vtxs corners edges width back?) s)
  (triangle-outline-shape (lazy-passes) triangle-outline-shape-functions
                          (vector-map (λ ([v : vtx]) (set-vtx-material v m)) vtxs)
                          corners edges width back?))

;; ===================================================================================================
;; Program for pass 1: material

(define polygon-mat-vertex-attributes
   (list (attribute "" 'vec4 "vert_position_width")
         (attribute "" 'vec4/bytes "vert_normal_roughness")
         (attribute "" 'vec4/bytes "vert_id")))

(define polygon-mat-fragment-attributes
  (list (attribute "smooth" 'vec4 "frag_position")
        (attribute "smooth" 'vec3 "frag_normal")
        (attribute "smooth" 'float "frag_roughness")
        (attribute "smooth" 'vec3 "frag_bary")
        (attribute "smooth" 'float "frag_width")))

(define polygon-mat-vertex-code
  (make-vertex-code
   "polygon-mat-vertex"
   #:includes
   (list model-vertex-code
         pack-unpack-normal-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "view" 'view)
         (standard-uniform "" 'mat4 "unview" 'unview)
         (standard-uniform "" 'mat4 "proj" 'proj))
   #:in-attributes
   polygon-mat-vertex-attributes
   #:out-attributes
   polygon-mat-fragment-attributes
   #<<code
mat4x3 model = get_model_transform();
mat4x3 unmodel = affine_inverse(model);
vec4 position = view * (a2p(model) * vec4(vert_position_width.xyz,1));
gl_Position = proj * position;
vec4 norm = (vec4(unpack_normal(vert_normal_roughness.xyz),0) * a2p(unmodel)) * unview;
frag_position = position;
frag_normal = normalize(norm.xyz);
frag_roughness = vert_normal_roughness.w / 255;
vec3 bary;
switch (int(vert_id.x) & 3) {
  case 0: bary = vec3(1,0,0); break;
  case 1: bary = vec3(0,1,0); break;
  default: bary = vec3(0,0,1); break;
}
frag_bary = bary;
frag_width = vert_position_width.w;
code
   ))

(define polygon-mat-fragment-code
  (make-fragment-code
   "polygon-mat-fragment"
   #:includes
   (list output-mat-fragment-code)
   #:in-attributes
   polygon-mat-fragment-attributes
   #<<code
float max_dist = max(0.0, frag_width * 0.5 - 0.5);
vec3 bary = frag_bary;
bary.x /= length(vec2(dFdx(bary.x), dFdy(bary.x)));
bary.y /= length(vec2(dFdx(bary.y), dFdy(bary.y)));
bary.z /= length(vec2(dFdx(bary.z), dFdy(bary.z)));
float dist = min(bary.x, min(bary.y, bary.z));
// Discard when 3 standard deviations out or more
if (dist > max_dist + 1.2) discard;

float z = frag_position.z;
float depth = sign(z) * pow(abs(z), abs(z) < 1.0 ? 1.001 : 0.999);
output_mat(frag_normal, frag_roughness, depth);
code
   ))

(define polygon-mat-program-code
  (make-program-code
   "polygon-mat-program"
   polygon-mat-vertex-code
   polygon-mat-fragment-code))

(define-singleton/context (polygon-mat-program)
  (log-pict3d-info "<engine> creating polygon material pass program")
  (program-code->gl-program polygon-mat-program-code))

;; ===================================================================================================
;; Program for pass 2: color

(define polygon-draw-vertex-attributes
  (list (attribute "" 'vec4 "vert_position_width")
        (attribute "" 'vec4/bytes "vert_rcolor")    ; vec4(r, g, b, a)
        (attribute "" 'vec4/bytes "vert_ecolor")    ; vec4(r, g, intensity.lo, intensity.hi)
        (attribute "" 'vec4/bytes "vert_material_id_bits")  ; vec3(ambient, diffuse, specular, id)
        ))

(define polygon-draw-fragment-attributes
  (list (attribute "smooth" 'vec4 "frag_position")
        (attribute "smooth" 'vec3 "frag_rcolor")
        (attribute "smooth" 'vec3 "frag_ecolor")
        (attribute "smooth" 'float "frag_alpha")
        (attribute "smooth" 'float "frag_ambient")
        (attribute "smooth" 'float "frag_diffuse")
        (attribute "smooth" 'float "frag_specular")
        (attribute "smooth" 'vec3 "frag_bary")
        (attribute "smooth" 'float "frag_width")
        (attribute "flat" 'vec3 "frag_edges")
        (attribute "flat" 'vec3 "frag_corners")))

(define polygon-draw-vertex-code
  (make-vertex-code
   "polygon-draw-vertex"
   #:includes
   (list model-vertex-code
         unpack-emitted-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "view" 'view)
         (standard-uniform "" 'mat4 "proj" 'proj))
   #:in-attributes
   polygon-draw-vertex-attributes
   #:out-attributes
   polygon-draw-fragment-attributes
   #<<code
mat4x3 model = get_model_transform();
vec4 position = view * (a2p(model) * vec4(vert_position_width.xyz,1));
gl_Position = proj * position;
frag_position = position;
frag_rcolor = pow(vert_rcolor.rgb / 255, vec3(2.2));
frag_alpha = vert_rcolor.a / 255;
frag_ecolor = unpack_emitted(vert_ecolor);
frag_ambient = vert_material_id_bits.x / 255;
frag_diffuse = vert_material_id_bits.y / 255;
frag_specular = vert_material_id_bits.z / 255;
vec3 bary;
switch (int(vert_material_id_bits.w) & 3) {
  case 0: bary = vec3(1,0,0); break;
  case 1: bary = vec3(0,1,0); break;
  default: bary = vec3(0,0,1); break;
}
frag_bary = bary;
frag_width = vert_position_width.w;
int edges = int(vert_material_id_bits.w) >> 2;
frag_edges = vec3(1 - (edges & 1), 1 - (edges & 2) * 0.5, 1 - (edges & 4) * 0.25);
int corners = int(vert_material_id_bits.w) >> 5;
frag_corners = vec3(1 - (corners & 1), 1 - (corners & 2) * 0.5, 1 - (corners & 4) * 0.25);
code
   ))

(define polygon-tran-fragment-code
  (make-fragment-code
   "polygon-tran-fragment"
   #:includes
   (list output-tran-fragment-code)
   #:standard-uniforms
   (list (standard-uniform "" 'vec3 "ambient" 'ambient)
         (standard-uniform "" 'sampler2D "diffuse" 'diffuse)
         (standard-uniform "" 'sampler2D "specular" 'specular))
   #:in-attributes
   polygon-draw-fragment-attributes
   #<<code
float max_dist = max(0.0, frag_width * 0.5 - 0.5);
vec3 bary = frag_bary;
float dx = length(vec2(dFdx(bary.x), dFdy(bary.x)));
float dy = length(vec2(dFdx(bary.y), dFdy(bary.y)));
float dz = length(vec2(dFdx(bary.z), dFdy(bary.z)));
vec3 cbary = (vec3(1) - bary + frag_corners) / vec3(dx, dy, dz);
vec3 ebary = (bary + frag_edges.yzx) / vec3(dx, dy, dz);
float cdist = min(cbary.x, min(cbary.y, cbary.z));
float edist = min(ebary.x, min(ebary.y, ebary.z));
float dist = min(cdist, edist);
// Discard when 3 standard deviations out or more
if (dist > max_dist + 1.2) discard;

float d = max(0.0, dist - max_dist);
// Gaussian with standard deviation 0.4
float line_alpha = exp2(d*d*-9.0);
float falloff = pow(1.0 - min(1, frag_width * 0.25 * max(dx, max(dy, dz))), 4);
float size_alpha = min(1.0, frag_width) * falloff;

vec3 diff = texelFetch(diffuse, ivec2(gl_FragCoord.xy), 0).rgb;
vec3 spec = texelFetch(specular, ivec2(gl_FragCoord.xy), 0).rgb;
vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
vec3 color = frag_ecolor + frag_rcolor * light;
float z = frag_position.z;
float depth = sign(z) * pow(abs(z), abs(z) < 1.0 ? 1.001 : 0.999);
output_tran(color, frag_alpha * line_alpha * size_alpha, depth);
code
   ))

(define polygon-tran-program-code
  (make-program-code
   "polygon-tran-program"
   polygon-draw-vertex-code
   polygon-tran-fragment-code))

(define-singleton/context (polygon-tran-program)
  (log-pict3d-info "<engine> creating triangle outline color pass program")
  (program-code->gl-program polygon-tran-program-code))

;; ===================================================================================================
;; Triangle mesh shape passes

(: triangle-idxs (Vectorof Index))
(define triangle-idxs (vector 0 1 2))

(: get-triangle-outline-shape-passes (-> shape passes))
(define (get-triangle-outline-shape-passes s)
  (match-define (triangle-outline-shape _ _ vtxs corners edges width back?) s)
  (define n (vector-length vtxs))
  
  (define bits
    (bitwise-ior (if (vector-ref edges 0) 4 0)
                 (if (vector-ref edges 1) 8 0)
                 (if (vector-ref edges 2) 16 0)
                 (if (vector-ref corners 0) 32 0)
                 (if (vector-ref corners 1) 64 0)
                 (if (vector-ref corners 2) 128 0)))
  
  (define mat-struct-size (program-code-vao-size polygon-mat-program-code))
  (define mat-data (make-bytes (* n mat-struct-size)))
  (for/fold ([i : Nonnegative-Fixnum  0]) ([j  (in-range n)])
    (define vert (vector-ref vtxs j))
    (define v (vtx-position vert))
    (define n (vtx-normal vert))
    (define m (vtx-material vert))
    (let* ([i  (serialize-vec3 mat-data i v)]
           [i  (serialize-float mat-data i width)]
           [i  (serialize-normal/bytes mat-data i n back?)]
           [i  (serialize-float/byte mat-data i (unsafe-flv4-ref m 3))]
           [i  (serialize-bytes mat-data i (bytes j 0 0 0) 4)])
      i))
  
  (define opaq-struct-size (program-code-vao-size polygon-tran-program-code))
  (define draw-data (make-bytes (* n opaq-struct-size)))
  (for/fold ([i : Nonnegative-Fixnum  0]) ([j  (in-range n)])
    (define vert (vector-ref vtxs j))
    (define v (vtx-position vert))
    (define c (vtx-color vert))
    (define e (vtx-emitted vert))
    (define m (vtx-material vert))
    (let* ([i  (serialize-vec3 draw-data i v)]
           [i  (serialize-float draw-data i width)]
           [i  (serialize-vec4/bytes draw-data i c)]
           [i  (serialize-emitted/bytes draw-data i e)]
           [i  (serialize-material-reflectances/bytes draw-data i m)]
           [i  (serialize-byte draw-data i (bitwise-ior j bits))])
      i))
  
  (define idxs (if back? (vector-reverse triangle-idxs) triangle-idxs))
  
  (passes
   #()
   #()
   #()
   (vector (shape-params polygon-mat-program empty #f GL_TRIANGLES (vertices n mat-data idxs)))
   (vector (shape-params polygon-tran-program empty #f GL_TRIANGLES (vertices n draw-data idxs)))))

;; ===================================================================================================
;; Bounding box

(: get-triangle-outline-shape-bbox (-> shape FlAffine3 bbox))
(define (get-triangle-outline-shape-bbox s t)
  (match-define (triangle-outline-shape _ _ vtxs corners edges width back?) s)
  (bbox (flrect3 (vtx-position (vector-ref vtxs 0))
                 (vtx-position (vector-ref vtxs 1))
                 (vtx-position (vector-ref vtxs 2)))
        0.0))

;; ===================================================================================================
;; Transform

(: triangle-outline-shape-transform (-> shape FlAffine3 triangle-outline-shape))
(define (triangle-outline-shape-transform s t)
  (match-define (triangle-outline-shape _ _ vtxs corners edges width back?) s)
  
  (define consistent? (flt3consistent? t))
  (define new-back? (if consistent? back? (not back?)))
  
  (: transform-vtx (-> vtx vtx))
  (define (transform-vtx vert)
    (define v (vtx-position vert))
    (define n (vtx-normal vert))
    (set-vtx-vecs
     vert
     (flt3apply/pos t v)
     (let ([n  (flt3apply/norm t n)])
       (if consistent?
           (if n n +z-flv3)
           (if n (flv3neg n) +z-flv3)))))
  
  (triangle-outline-shape (lazy-passes) triangle-outline-shape-functions
                          (vector-map transform-vtx vtxs) corners edges width new-back?))

;; ===================================================================================================
;; Deform

(: triangle-outline-shape-deform (-> shape FlSmooth3 (Listof triangle-outline-shape)))
(define (triangle-outline-shape-deform s t)
  (match-define (triangle-outline-shape _ _ (vector vtx1 vtx2 vtx3) corners edges width back?) s)
  (match-define (face new-vtx1 new-vtx2 new-vtx3 _ _ _ _)
    (fls3apply/face t (face vtx1 vtx2 vtx3 #f #f #f #f)))
  (define new-vtxs (vector new-vtx1 new-vtx2 new-vtx3))
  (define-values (test-vtx2 d2 c2?) (fls3apply/vtx t vtx2))
  (define flipped? (not (equal? new-vtx2 test-vtx2)))
  (define new-corners
    (cond [flipped?  (vector (vector-ref corners 0) (vector-ref corners 2) (vector-ref corners 1))]
          [else  corners]))
  (define new-edges
    (cond [flipped?  (vector (vector-ref edges 2) (vector-ref edges 1) (vector-ref edges 0))]
          [else  edges]))
  (list (triangle-outline-shape (lazy-passes) triangle-outline-shape-functions
                                new-vtxs new-corners new-edges width #f)))

;; ===================================================================================================

(define triangle-outline-shape-functions
  (deform-shape-functions
    get-triangle-outline-shape-passes
    (λ (s kind t) (and (eq? kind 'visible) (get-triangle-outline-shape-bbox s t)))
    triangle-outline-shape-transform
    (λ (s t) (list (triangle-outline-shape-transform s t)))
    default-ray-intersect
    set-triangle-outline-shape-color
    set-triangle-outline-shape-emitted
    set-triangle-outline-shape-material
    default-extract-faces
    default-tessellate
    triangle-outline-shape-deform))
