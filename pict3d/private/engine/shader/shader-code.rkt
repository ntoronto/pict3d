#lang typed/racket/base

(require racket/string
         racket/match
         racket/list
         typed/opengl
         "../../gl.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Attributes

(struct attribute ([qualifiers : String]
                   [type : Symbol]
                   [name : String])
  #:transparent)

(: attribute-type-length-hash (HashTable Symbol Index))
(define attribute-type-length-hash
  #hasheq((vec4 . 4)
          (vec3 . 3)
          (vec2 . 2)
          (float . 1)
          (vec4/bytes . 4)
          (vec3/bytes . 3)
          (vec2/bytes . 2)
          (float/byte . 1)))

(: attribute-type-gl-type-hash (HashTable Symbol Integer))
(define attribute-type-gl-type-hash
  (make-immutable-hasheq
   (list (cons 'vec4 GL_FLOAT)
         (cons 'vec3 GL_FLOAT)
         (cons 'vec2 GL_FLOAT)
         (cons 'float GL_FLOAT)
         (cons 'vec4/bytes GL_UNSIGNED_BYTE)
         (cons 'vec3/bytes GL_UNSIGNED_BYTE)
         (cons 'vec2/bytes GL_UNSIGNED_BYTE)
         (cons 'float/byte GL_UNSIGNED_BYTE))))

(: attribute-type-string-hash (HashTable Symbol String))
(define attribute-type-string-hash
  #hasheq((vec4/bytes . "vec4")
          (vec3/bytes . "vec3")
          (vec2/bytes . "vec2")
          (float/byte . "float")))

(: attribute-type-length (-> Symbol Index))
(define (attribute-type-length t)
  (hash-ref attribute-type-length-hash t
            (λ () (error 'attribute-type-length "unknown attribute type ~e" t))))

(: attribute-type-gl-type (-> Symbol Integer))
(define (attribute-type-gl-type t)
  (hash-ref attribute-type-gl-type-hash t
            (λ () (error 'attribute-type-gl-type "unknown attribute type ~e" t))))

(: attribute-type-string (-> Symbol String))
(define (attribute-type-string t)
  (hash-ref attribute-type-string-hash t (λ () (symbol->string t))))

(: attribute->string (-> (U 'in 'out 'array-in 'uniform) attribute String))
(define (attribute->string kind a)
  (match-define (attribute qualifiers t name) a)
  (define sp (if (= 0 (string-length qualifiers)) "" " "))
  (define type (attribute-type-string t))
  (define inoutuniform (if (eq? kind 'array-in) 'in kind))
  (define tail (if (eq? kind 'array-in) "[]" ""))
  (format "~a~a~a ~a ~a~a;" qualifiers sp inoutuniform type name tail))

(: attributes->string (-> (U 'in 'out 'array-in 'uniform) (Listof attribute) String))
(define (attributes->string kind as)
  (string-join (map (λ ([a : attribute]) (attribute->string kind a)) as)
               "\n"))

(: attribute->vao-field (-> attribute vao-field))
(define (attribute->vao-field a)
  (match-define (attribute _ type name) a)
  (make-vao-field name
                  (attribute-type-length type)
                  (attribute-type-gl-type type)))

(: attributes->vao-struct (-> (Listof attribute) vao-struct))
(define (attributes->vao-struct as)
  (apply make-vao-struct (map attribute->vao-field as)))
                           
(struct standard-uniform attribute ([symbol : Symbol])
  #:transparent)

;; ===================================================================================================
;; Partial programs

(struct partial-code
  ([name : String]
   [includes : (Listof partial-code)]
   [definitions : (Listof String)]
   [standard-uniforms : (Listof standard-uniform)]
   [program-uniforms : (Listof attribute)]
   [in-attributes : (Listof attribute)]
   [out-attributes : (Listof attribute)])
  #:transparent)

(struct shader-code partial-code ([main : String]) #:transparent)
(struct vertex-code shader-code () #:transparent)
(struct geometry-code shader-code () #:transparent)
(struct fragment-code shader-code () #:transparent)

(: partial-code-all-includes (-> partial-code (Listof partial-code)))
(define (partial-code-all-includes code)
  (append* (append (map partial-code-all-includes (partial-code-includes code))
                   (list (list code)))))

(: partial-code->string/no-includes (-> partial-code String))
(define (partial-code->string/no-includes code)
  (match-define (partial-code name _ defs std-unifs prog-unifs in-attrs out-attrs) code)
  (define in-kind (if (geometry-code? code) 'array-in 'in))
  (string-join
   (append
    (list (format "~n/* ========== ~a ========== */~n" name))
    (if (empty? std-unifs) empty (list (attributes->string 'uniform std-unifs) ""))
    (if (empty? prog-unifs) empty (list (attributes->string 'uniform prog-unifs) ""))
    (if (empty? in-attrs) empty (list (attributes->string in-kind in-attrs) ""))
    (if (empty? out-attrs) empty (list (attributes->string 'out out-attrs) ""))
    (if (empty? defs) empty (list (string-join defs "\n\n"))))
   "\n"))

(: partial-code->string (-> partial-code String))
(define (partial-code->string code)
  (define all-incs
    ((inst remove-duplicates partial-code String)
     (append* (map partial-code-all-includes (partial-code-includes code)))
     #:key partial-code-name))
  (string-join
   (append
    (map partial-code->string/no-includes all-incs)
    (list (partial-code->string/no-includes code)))
   "\n"))

(: shader-code->string (-> shader-code String))
(define (shader-code->string code)
  (string-append
   (partial-code->string code)
   "\nvoid main() {\n"
   (shader-code-main code)
   "\n}\n"))

(: standard-uniform->pair (-> standard-uniform (Pair String Symbol)))
(define (standard-uniform->pair u)
  (match-define (standard-uniform q t n s) u)
  (cons n s))

(: partial-code-standard-uniform-pairs (-> partial-code (Listof (Pair String Symbol))))
(define (partial-code-standard-uniform-pairs code)
  (remove-duplicates
   (append (append* (map partial-code-standard-uniform-pairs (partial-code-includes code)))
           (map standard-uniform->pair (partial-code-standard-uniforms code)))))

(: partial-code-all-in-attributes (-> partial-code (Listof attribute)))
(define (partial-code-all-in-attributes code)
  (remove-duplicates
   (append (append* (map partial-code-all-in-attributes (partial-code-includes code)))
           (partial-code-in-attributes code))))

(: partial-code-all-out-attributes (-> partial-code (Listof attribute)))
(define (partial-code-all-out-attributes code)
  (remove-duplicates
   (append (append* (map partial-code-all-out-attributes (partial-code-includes code)))
           (partial-code-out-attributes code))))

(: partial-code-plain-in-attributes (-> partial-code (Listof attribute)))
(define (partial-code-plain-in-attributes code)
  (filter (λ ([a : attribute])
            (define name (attribute-name a))
            (or (= 0 (string-length name))
                (not (char=? (string-ref name 0) #\_))))
          (partial-code-in-attributes code)))
  
(: vertex-code-vao-struct (-> vertex-code vao-struct))
(define (vertex-code-vao-struct code)
  (attributes->vao-struct (partial-code-plain-in-attributes code)))

;; ===================================================================================================
;; Convenience functions

(: make-partial-code
   (->* [String]
        [#:includes (Listof partial-code)
         #:definitions (Listof String)
         #:standard-uniforms (Listof standard-uniform)
         #:program-uniforms (Listof attribute)
         #:in-attributes (Listof attribute)
         #:out-attributes (Listof attribute)]
        partial-code))
(define (make-partial-code name
                           #:includes [includes empty]
                           #:definitions [definitions empty]
                           #:standard-uniforms [standard-uniforms empty]
                           #:program-uniforms [program-uniforms empty]
                           #:in-attributes [in-attributes empty]
                           #:out-attributes [out-attributes empty])
  (partial-code name
                includes definitions standard-uniforms program-uniforms
                in-attributes out-attributes))

(: make-vertex-code
   (->* [String String]
        [#:includes (Listof partial-code)
         #:definitions (Listof String)
         #:standard-uniforms (Listof standard-uniform)
         #:program-uniforms (Listof attribute)
         #:in-attributes (Listof attribute)
         #:out-attributes (Listof attribute)]
        vertex-code))
(define (make-vertex-code name main
                          #:includes [includes empty]
                          #:definitions [definitions empty]
                          #:standard-uniforms [standard-uniforms empty]
                          #:program-uniforms [program-uniforms empty]
                          #:in-attributes [in-attributes empty]
                          #:out-attributes [out-attributes empty])
  (vertex-code name
               includes definitions standard-uniforms program-uniforms
               in-attributes out-attributes
               main))

(: make-geometry-code
   (->* [String String]
        [#:includes (Listof partial-code)
         #:definitions (Listof String)
         #:standard-uniforms (Listof standard-uniform)
         #:program-uniforms (Listof attribute)
         #:in-attributes (Listof attribute)
         #:out-attributes (Listof attribute)]
        geometry-code))
(define (make-geometry-code name main
                            #:includes [includes empty]
                            #:definitions [definitions empty]
                            #:standard-uniforms [standard-uniforms empty]
                            #:program-uniforms [program-uniforms empty]
                            #:in-attributes [in-attributes empty]
                            #:out-attributes [out-attributes empty])
  (geometry-code name
                 includes definitions standard-uniforms program-uniforms
                 in-attributes out-attributes
                 main))

(: make-fragment-code
   (->* [String String]
        [#:includes (Listof partial-code)
         #:definitions (Listof String)
         #:standard-uniforms (Listof standard-uniform)
         #:program-uniforms (Listof attribute)
         #:in-attributes (Listof attribute)
         #:out-attributes (Listof attribute)]
        fragment-code))
(define (make-fragment-code name main
                            #:includes [includes empty]
                            #:definitions [definitions empty]
                            #:standard-uniforms [standard-uniforms empty]
                            #:program-uniforms [program-uniforms empty]
                            #:in-attributes [in-attributes empty]
                            #:out-attributes [out-attributes empty])
  (fragment-code name
                 includes definitions standard-uniforms program-uniforms
                 in-attributes out-attributes
                 main))

;; ===================================================================================================
;; Programs

(struct program-code ([name : String]
                      [vertex-code : vertex-code]
                      [geometry-code : (U #f geometry-code)]
                      [fragment-code : fragment-code]
                      [standard-uniform-pairs : (Listof (Pair String Symbol))]
                      [vao-struct : vao-struct]
                      [output-names : (Listof String)])
  #:transparent)

(: make-program-code (-> String
                         vertex-code
                         fragment-code
                         [#:geometry (U #f geometry-code)]
                         program-code))
(define (make-program-code name vcode fcode #:geometry [gcode #f])
  (program-code name
                vcode
                gcode
                fcode
                (remove-duplicates
                 (append (partial-code-standard-uniform-pairs vcode)
                         (partial-code-standard-uniform-pairs fcode)))
                (vertex-code-vao-struct vcode)
                (remove-duplicates
                 (map attribute-name (partial-code-all-out-attributes fcode)))))

(: program-code-vao-size (-> program-code Index))
(define (program-code-vao-size prog)
  (vao-struct-size (program-code-vao-struct prog)))

(: program-code-shaders (-> program-code (Listof gl-shader)))
(define (program-code-shaders prog)
  (define vcode (program-code-vertex-code prog))
  (define gcode (program-code-geometry-code prog))
  (define fcode (program-code-fragment-code prog))
  (append
   (list (make-gl-shader GL_VERTEX_SHADER (shader-code->string vcode)))
   (if gcode (list (make-gl-shader GL_GEOMETRY_SHADER (shader-code->string gcode))) empty)
   (list (make-gl-shader GL_FRAGMENT_SHADER (shader-code->string fcode)))))

(: program-code->gl-program (-> program-code gl-program))
(define (program-code->gl-program prog)
  (make-gl-program
   (program-code-name prog)
   (program-code-standard-uniform-pairs prog)
   (program-code-vao-struct prog)
   (program-code-output-names prog)
   (program-code-shaders prog)))

;; ===================================================================================================
;; Standard shader library

(define matrix-code
  (make-partial-code
   "matrix"
   #:definitions
   (list
    #<<code
mat4x3 rows2mat4x3(vec4 row0, vec4 row1, vec4 row2) {
  return transpose(mat3x4(row0, row1, row2));
}
code
    #<<code
mat4 a2p(mat4x3 m) {
  return mat4(vec4(m[0],0), vec4(m[1],0), vec4(m[2],0), vec4(m[3],1));
}
code
    #<<code
mat3 linear_inverse(mat3 m) {
  float det = dot(m[0], cross(m[1], m[2]));
  return transpose(mat3(cross(m[1],m[2]),
                        cross(m[2],m[0]),
                        cross(m[0],m[1]))) / det;
}
code
    #<<code
mat4x3 affine_inverse(mat4x3 m) {
  mat3 n = linear_inverse(mat3(m[0],m[1],m[2]));
  return mat4x3(n[0], n[1], n[2], -(n*m[3]));
}
code
    )))

(define model-vertex-code
  (make-partial-code
   "model-vertex"
   #:includes (list matrix-code)
   #:in-attributes
   (list (attribute "" 'vec4 "_model0")
         (attribute "" 'vec4 "_model1")
         (attribute "" 'vec4 "_model2"))
   #:definitions
   (list
    #<<code
mat4x3 get_model_transform() {
  return rows2mat4x3(_model0, _model1, _model2);
}
code
    )))

(define rect-code
  (make-partial-code
   "rect"
   #:definitions
   (list
    #<<code
struct rect {
  vec3 mins;
  vec3 maxs;
  float is_degenerate;
};
code
    )))

(define output-2d-rect-vertex-code
  (make-partial-code
   "output-2d-rect-vertex"
   #:includes (list rect-code)
   #:definitions
   (list
    #<<code
void output_2d_rect_vertex(rect bbx, int vertex_id) {
  // output the correct vertices for a triangle strip
  switch (vertex_id) {
    case 0:  gl_Position = vec4(bbx.mins.xy, 0.0, 1.0); break;
    case 1:  gl_Position = vec4(bbx.maxs.x, bbx.mins.y, 0.0, 1.0); break;
    case 2:  gl_Position = vec4(bbx.mins.x, bbx.maxs.y, 0.0, 1.0); break;
    default: gl_Position = vec4(bbx.maxs.xy, 0.0, 1.0); break;
  }
}
code
    )))

(define output-unit-cube-vertex-code
  (make-partial-code
   "output-unit-cube-vertex"
   #:definitions
   (list #<<code
void output_unit_cube_vertex(mat4 trans, mat4 proj, int vertex_id) {
  vec4 p = vec4(mix(-1.0, +1.0, vertex_id & 1),
                mix(-1.0, +1.0, (vertex_id & 2) >> 1),
                mix(-1.0, +1.0, (vertex_id & 4) >> 2),
                1.0);
  p = proj * trans * p;
  gl_Position = vec4(p.xy / p.w, 0.0, 1.0);
}
code
         )))

(define output-unit-quad-vertex-code
  (make-partial-code
   "output-unit-quad-vertex"
   #:definitions
   (list #<<code
void output_unit_quad_vertex(mat4 trans, mat4 proj, int vertex_id) {
  vec4 p = vec4(mix(-1.0, +1.0, vertex_id & 1),
                mix(-1.0, +1.0, (vertex_id & 2) >> 1),
                0.0,
                1.0);
  p = proj * trans * p;
  gl_Position = vec4(p.xy / p.w, 0.0, 1.0);
}
code
         )))

(define float-constants-code
  (make-partial-code
   "infinity"
   #:definitions
   (list "const float pos_infinity = 1.0/0.0;"
         "const float neg_infinity = -pos_infinity;"
         "const float epsilon = 1.1920929e-07;")))

(define impostor-bounds-code
  (make-partial-code
   "impostor-bounds"
   #:includes
   (list rect-code
         float-constants-code)
   #:definitions
   (list
    #<<code
rect impostor_bounds(mat4 view, mat4 proj, vec3 wmin, vec3 wmax) {
  vec4 vs[8];
  vs[0] = vec4(wmin, 1.0);
  vs[1] = vec4(wmin.xy, wmax.z, 1.0);
  vs[2] = vec4(wmin.x, wmax.y, wmin.z, 1.0);
  vs[3] = vec4(wmin.x, wmax.yz, 1.0);
  vs[4] = vec4(wmax.x, wmin.yz, 1.0);
  vs[5] = vec4(wmax.x, wmin.y, wmax.z, 1.0);
  vs[6] = vec4(wmax.xy, wmin.z, 1.0);
  vs[7] = vec4(wmax, 1.0);
  
  // view space min and max
  vec3 vmin = vec3(pos_infinity);
  vec3 vmax = vec3(neg_infinity);
  
  // clip space min and max
  vec3 cmin = vec3(pos_infinity);
  vec3 cmax = vec3(neg_infinity);
  
  for (int i = 0; i < 8; i++) {
    vec4 vpos = view * vs[i];
    vpos /= vpos.w;
    vmin = min(vmin, vpos.xyz);
    vmax = max(vmax, vpos.xyz);
    
    vec4 cpos = proj * vpos;
    cpos /= abs(cpos.w);
    cmin = min(cmin, cpos.xyz);
    cmax = max(cmax, cpos.xyz);
  }
  
  if (vmin.z > 0.0) return rect(cmin, cmax, 1.0);

  // if we're inside it, we should draw on the whole screen
  if (max(vmin.x, max(vmin.y, vmin.z)) <= 0.0 &&
      min(vmax.x, min(vmax.y, vmax.z)) >= 0.0) {
    cmin = vec3(-1.0);
    cmax = vec3(+1.0);
  }

  return rect(cmin, cmax, 0.0);
}
code
    )))

(define output-impostor-vertex-code
  (make-partial-code
   "output-impostor-vertex"
   #:includes
   (list rect-code
         impostor-bounds-code
         output-2d-rect-vertex-code)
   #:definitions
   (list
    #<<code
float output_impostor_vertex(mat4 view, mat4 proj, vec3 wmin, vec3 wmax, int vertex_id) {
  rect bbx = impostor_bounds(view, proj, wmin, wmax);
  output_2d_rect_vertex(bbx, vertex_id);
  return bbx.is_degenerate;
}
code
    )))

(define pack-unpack-normal-code
  (make-partial-code
   "pack-unpack-normal"
   #:definitions
   (list
    #<<code
// Returns +/-1
vec2 signNotZero(vec2 v) {
  return vec2((v.x >= 0.0) ? +1.0 : -1.0, (v.y >= 0.0) ? +1.0 : -1.0);
}
code
    #<<code
// Assume normalized input.  Output is on [-1, 1] for each component.
vec2 float32x3_to_oct(in vec3 v) {
  // Project the sphere onto the octahedron, and then onto the xy plane
  vec2 p = v.xy * (1.0 / (abs(v.x) + abs(v.y) + abs(v.z)));
  // Reflect the folds of the lower hemisphere over the diagonals
  return (v.z <= 0.0) ? ((1.0 - abs(p.yx)) * signNotZero(p)) : p;
}
code
    #<<code
vec3 oct_to_float32x3(vec2 e) {
  vec3 v = vec3(e.xy, 1.0 - abs(e.x) - abs(e.y));
  if (v.z < 0) v.xy = (1.0 - abs(v.yx)) * signNotZero(v.xy);
  return normalize(v);
}
code
    #<<code
vec3 snorm12x2_to_unorm8x3(vec2 f) {
  vec2 u = vec2(round(clamp(f, -1.0, 1.0) * 2047 + 2047));
  float t = floor(u.y / 256.0);
  return floor(vec3(u.x / 16.0, fract(u.x / 16.0) * 256.0 + t, u.y - t * 256.0));
}
code
    #<<code
vec2 unorm8x3_to_snorm12x2(vec3 u) {
  u.y *= (1.0 / 16.0);
  vec2 s = vec2(u.x * 16.0 + floor(u.y), fract(u.y) * (16.0 * 256.0) + u.z);
  return clamp(s * (1.0 / 2047.0) - 1.0, vec2(-1.0), vec2(1.0));
}
code
    #<<code
vec3 unpack_normal(vec3 u) {
  return oct_to_float32x3(unorm8x3_to_snorm12x2(u));
}
code
    #<<code
vec3 pack_normal(vec3 n) {
  return snorm12x2_to_unorm8x3(float32x3_to_oct(n));
}
code
    )))

(define depth-fragment-code
  (make-partial-code
   "depth-fragment"
   #:standard-uniforms
   (list (standard-uniform "" 'float "zfar" 'zfar)
         (standard-uniform "" 'float "log2_znear_zfar" 'log2_znear_zfar))
   #:definitions
   (list
    #<<code
float get_frag_depth(float z) {
  return log2(-z/zfar) / log2_znear_zfar;
}
code
    #<<code
float get_view_depth(float depth) {
  return -exp2(depth * log2_znear_zfar) * zfar;
}
code
    )))

(define surface-code
  (make-partial-code
   "surface"
   #:definitions
   (list
    #<<code
struct surface {
  vec3 normal;
  float roughness;
};
code
    )))

(define get-surface-fragment-code
  (make-partial-code
   "get-surface-fragment"
   #:includes (list surface-code)
   #:definitions
   (list
    #<<code
surface get_surface(sampler2D matTex) {
  vec4 mat = texelFetch(matTex, ivec2(gl_FragCoord.xy), 0);
  return surface(mat.rgb, mat.a);
}
code
    )))

(define light-fragment-code
  (make-partial-code
   "light-fragment"
   #:includes
   (list float-constants-code
         depth-fragment-code
         get-surface-fragment-code)
   #:out-attributes
   (list (attribute "" 'vec4 "out_diffuse")
         (attribute "" 'vec4 "out_specular"))
   #:definitions
   (list
    #<<code
vec3 frag_coord_to_position(vec4 frag_coord, sampler2D depth, mat4 unproj, int width, int height) {
  // compute view z from depth buffer
  float d = texelFetch(depth, ivec2(frag_coord.xy), 0).r;
  if (d == 0.0) discard;
  float z = get_view_depth(d);

  // compute direction
  vec2 clip_xy = (frag_coord.xy / vec2(width,height) - vec2(0.5)) * 2.0;
  vec4 vpos = unproj * vec4(clip_xy, 0.0, 1.0);
  return vpos.xyz / vpos.z * z;
}
code
    #<<code
vec3 attenuate_invsqr_quad(vec3 light_color, float q, float d) {
  // Inverse-square attenuation from 0
  float i0 = min(1.0/epsilon, 1.0/(d*d));
  // Quadratic attenuation from q/2 to q
  float i1 = min(1.0/epsilon, 16.0/pow(q,4.0) * pow(max(0.0, q-d), 2.0));
  // Choose quadratic if d > q/2, inverse square if d <= q/2
  return max(vec3(0.0), light_color) * mix(i0, i1, step(q*0.5, d));
}
code
    #<<code
// Ward model for anisotropic, but without the anisotropy (so that it's consistent with the
// full anisotropic model if we ever want to use it)
float specular(vec3 N, vec3 L, vec3 V, float dotLN, float dotVN, float m) {
  vec3 uH = L+V;  // unnormalized half vector
  float dotsum = dotVN + dotLN;
  float dd = dotsum * dotsum;
  float uu = dot(uH,uH);
  float mm = m * m;
  float tt = dotLN * dotVN;
  float sqrt_part = sqrt(tt) / (0.1 + tt);
  return min(16.0, exp(-(uu-dd)/(mm*dd)) * sqrt_part / (12.566371 * mm));
}
code
    #<<code
void output_light(vec3 light, surface s, vec3 L, vec3 V) {
  vec3 N = s.normal;

  // Diffuse
  float dotNL = max(0.0,dot(N,L));
  if (dotNL == 0.0) discard;  // if true, every light value is 0.0
  out_diffuse = vec4(light * dotNL, 1.0);

  // Specular
  float dotNV = dot(N,V);
  float spec = dotNV <= 0.0 ? 0.0 : specular(N,L,V,dotNL,dotNV,max(0.03125,s.roughness));
  out_specular = vec4(light * dotNL * spec, 1.0);
}
code
    )))

(define output-mat-fragment-code
  (make-partial-code
   "output-mat-fragment"
   #:includes
   (list depth-fragment-code)
   #:out-attributes
   (list (attribute "" 'vec4 "out_mat"))
   #:definitions
   (list
    #<<code
void output_mat(vec3 dir, float roughness, float z) {
  gl_FragDepth = get_frag_depth(z);
  out_mat = vec4(normalize(dir), roughness);
}
code
    )))

(define output-opaq-fragment-code
  (make-partial-code
   "output-opaq-fragment"
   #:includes
   (list depth-fragment-code)
   #:out-attributes
   (list (attribute "" 'vec4 "out_color"))
   #:definitions
   (list
    #<<code
void output_opaq(vec3 color, float z) {
  gl_FragDepth = get_frag_depth(z);
  out_color = vec4(color, 1.0);
}
code
    )))

(define output-tran-fragment-code
  (make-partial-code
   "output-tran-fragment"
   #:includes
   (list depth-fragment-code)
   #:out-attributes
   (list (attribute "" 'vec4 "out_color")
         (attribute "" 'float "out_weight"))
   #:definitions
   (list
    #<<code
void output_tran(vec3 color, float a, float z) {
  //float weight = a * clamp(10/(1e-5 + pow(abs(z)/5,2) + pow(abs(z)/200,6)), 1e-2, 3e3);     // (7)
  float weight = max(1e-5, a * exp2(z));
  gl_FragDepth = get_frag_depth(z);
  out_color = vec4(color * a, a) * weight;
  out_weight = weight;
}
code
    )))

(define unpack-emitted-code
  (make-partial-code
   "unpack-emitted"
   #:definitions
   (list
    #<<code
vec3 unpack_emitted(vec4 e) {
  e = e / 255.0;
  float b = 1.0 - e.r - e.g;
  float m = max(e.r, max(e.g, b));
  return pow(vec3(e.rg,b)/m, vec3(2.2)) * (e.b + e.a * 256.0) * m;
}
code
    )))

(define ray-trace-fragment-code
  (make-partial-code
   "ray-trace-fragment"
   #:definitions
   (list
    #<<code
vec3 frag_coord_to_direction(vec4 frag_coord, mat4 unproj, int width, int height) {
  vec2 clip_xy = (frag_coord.xy / vec2(width,height) - vec2(0.5)) * 2.0;
  vec4 vpos = unproj * vec4(clip_xy, 0.0, 1.0);
  return normalize(vpos.xyz);
}
code
    #<<code
vec2 unit_sphere_intersect(vec3 origin, vec3 dir) {
  float b = dot(origin,dir);
  float disc = b*b - dot(origin,origin) + 1;
  if (disc < 0.0) discard;
  float q = sqrt(disc);
  return vec2(-q,q) - vec2(b);
}
code
    #<<code
vec2 unit_disk_intersect(vec3 origin, vec3 dir, float r) {
  float t = -origin.z / dir.z;
  float d = length(origin + t * dir);
  t = (r <= d && d <= 1.0) ? t : -1.0;
  return (dir.z < 0.0) ? vec2(t,-1.0) : vec2(-1.0,t);
}
code
    #<<code
vec2 unit_cylinder_intersect(vec3 p, vec3 d, float r) {
  float s = 0.5 * (1.0 - r);
  float dz1 = s * d.z;
  float pz1 = 1.0 - s * (p.z + 1.0);
  float a =  dot(vec3(d.xy,dz1), vec3(d.xy,-dz1));
  float b = -dot(vec3(d.xy,dz1), vec3(p.xy, pz1)) / a;
  float c =  dot(vec3(p.xy,pz1), vec3(p.xy,-pz1)) / a;
  float disc = b*b - c;
  if (disc < 0.0) discard;
  float q = sign(a) * sqrt(disc);
  vec2 t = vec2(b) + vec2(-q,q);
  return mix(vec2(-1.0), t, step(abs(p.zz + d.zz * t), vec2(1.0)));
  // Equivalent:
  //return vec2(1.0 < abs(p.z + d.z * t1) ? -1.0 : t1,
  //            1.0 < abs(p.z + d.z * t2) ? -1.0 : t2);
}
code
    )))

(define rgb-hsv-code
  (make-partial-code
   "rgb-hsv"
   #:definitions
   (list
   #<<code
vec3 rgb_to_hsv(vec3 c) {
  vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
  vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
  vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));
  
  float d = q.x - min(q.w, q.y);
  float e = 1.0e-10;
  return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}
code
   #<<code
vec3 hsv_to_rgb(vec3 c) {
  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}
code
   )))
