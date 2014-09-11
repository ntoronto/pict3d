#lang typed/racket/base

(require racket/match
         racket/list
         racket/vector
         racket/unsafe/ops
         math/flonum
         math/base
         typed/opengl
         (except-in typed/opengl/ffi cast ->)
         "../math/flv3.rkt"
         "../math/flt3.rkt"
         "../math/flaabb3.rkt"
         "../types.rkt"
         "../utils.rkt"
         "fltriangle3.rkt"
         "flscene3.rkt"
         "gl.rkt"
         "types.rkt"
         "utils.rkt"
         "shader-lib.rkt"
         "draw-pass.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Types

(struct material ([ambient : Flonum]
                  [diffuse : Flonum]
                  [specular : Flonum]
                  [roughness : Flonum])
  #:transparent)

(define null-flv3 (flvector 0.0 0.0 0.0))
(define null-rgba (flvector 0.0 0.0 0.0 0.0))
(define null-rgb null-flv3)
(define null-material (material 0.0 0.0 0.0 1.0))

(struct shape ([lazy-passes : (Lazy-Box Passes)]) #:transparent)

(struct solid-shape shape () #:transparent)

(struct triangle-shape solid-shape
  ([fltriangle3 : FlTriangle3]
   [normals : (U FlVector (Vectorof FlVector))]
   [colors : (U FlVector (Vectorof FlVector))]
   [emitted-colors : (U FlVector (Vectorof FlVector))]
   [materials : (U material (Vectorof material))]
   [face : Face])
  #:transparent)

(struct quad-shape solid-shape
  ([vertices : (Vectorof FlVector)]
   [normals : (U FlVector (Vectorof FlVector))]
   [colors : (U FlVector (Vectorof FlVector))]
   [emitted-colors : (U FlVector (Vectorof FlVector))]
   [materials : (U material (Vectorof material))]
   [face : Face])
  #:transparent)

(struct rectangle-shape solid-shape
  ([aabb : FlAABB3]
   [color : FlVector]
   [emitted-color : FlVector]
   [material : material]
   [face : Face])
  #:transparent)

(struct sphere-shape solid-shape
  ([transform : affine]
   [color : FlVector]
   [emitted-color : FlVector]
   [material : material]
   [inside? : Boolean])
  #:transparent)

(struct light-shape shape ([intensity : FlVector]) #:transparent)
(struct directional-light-shape light-shape ([direction : FlVector]) #:transparent)
(struct point-light-shape light-shape ([position : FlVector] [radius : Flonum]) #:transparent)

(struct frozen-scene-shape shape
  ([scene : Nonempty-Scene])
  #:transparent)

(define-type Shape (U triangle-shape
                      quad-shape
                      rectangle-shape
                      sphere-shape
                      directional-light-shape
                      point-light-shape
                      frozen-scene-shape))

(define-type Nonempty-Scene (Nonempty-FlScene3 Shape))
(define-type Scene (FlScene3 Shape))

;; ===================================================================================================
;; Shape constructors

(: make-triangle-shape* (-> FlTriangle3
                            (U FlVector (Vectorof FlVector))
                            (U FlVector (Vectorof FlVector))
                            (U FlVector (Vectorof FlVector))
                            (U material (Vectorof material))
                            Face
                            triangle-shape))
(define (make-triangle-shape* t ns cs es ms face)
  (triangle-shape (box 'lazy) t ns cs es ms face))

(: well-formed-flvectors? (-> (U FlVector (Vectorof FlVector)) Index Index Boolean))
(define (well-formed-flvectors? vs n i)
  (if (flvector? vs)
      (= i (flvector-length vs))
      (and (= n (vector-length vs))
           (for/and ([v  (in-vector vs)])
             (= i (flvector-length v))))))

(: make-triangle-shape (-> (Vectorof FlVector)
                           (U FlVector (Vectorof FlVector))
                           (U FlVector (Vectorof FlVector))
                           (U FlVector (Vectorof FlVector))
                           (U material (Vectorof material))
                           Face
                           triangle-shape))
(define (make-triangle-shape vs ns cs es ms face)
  (cond [(not (well-formed-flvectors? vs 3 3))
         (raise-argument-error 'make-triangle-shape
                               "length-3 flvector, or length-3 vector of length-3 flvectors"
                               0 vs ns cs es ms face)]
        [(not (well-formed-flvectors? ns 3 3))
         (raise-argument-error 'make-triangle-shape
                               "length-3 flvector, or length-3 vector of length-3 flvectors"
                               1 vs ns cs es ms face)]
        [(not (well-formed-flvectors? cs 3 4))
         (raise-argument-error 'make-triangle-shape
                               "length-4 flvector, or length-3 vector of length-4 flvectors"
                               2 vs ns cs es ms face)]
        [(not (well-formed-flvectors? es 3 3))
         (raise-argument-error 'make-triangle-shape
                               "length-3 flvector, or length-3 vector of length-3 flvectors"
                               3 vs ns cs es ms face)]
        [(not (or (material? ms) (= 3 (vector-length ms))))
         (raise-argument-error 'make-triangle-shape
                               "material, or length-3 vector of materials"
                               4 vs ns cs es ms face)]
        [else
         (make-triangle-shape* (fltriangle3 vs) ns cs es ms face)]))

(: make-quad-shape (-> (Vectorof FlVector)
                       (U FlVector (Vectorof FlVector))
                       (U FlVector (Vectorof FlVector))
                       (U FlVector (Vectorof FlVector))
                       (U material (Vectorof material))
                       Face
                       quad-shape))
(define (make-quad-shape vs ns cs es ms face)
  (cond [(not (well-formed-flvectors? vs 4 3))
         (raise-argument-error 'make-quad-shape
                               "length-3 flvector, or length-4 vector of length-3 flvectors"
                               0 vs ns cs es ms face)]
        [(not (well-formed-flvectors? ns 4 3))
         (raise-argument-error 'make-quad-shape
                               "length-3 flvector, or length-4 vector of length-3 flvectors"
                               1 vs ns cs es ms face)]
        [(not (well-formed-flvectors? cs 4 4))
         (raise-argument-error 'make-quad-shape
                               "length-4 flvector, or length-4 vector of length-4 flvectors"
                               2 vs ns cs es ms face)]
        [(not (well-formed-flvectors? es 4 3))
         (raise-argument-error 'make-quad-shape
                               "length-3 flvector, or length-4 vector of length-3 flvectors"
                               3 vs ns cs es ms face)]
        [(not (or (material? ms) (= 4 (vector-length ms))))
         (raise-argument-error 'make-quad-shape
                               "material, or length-4 vector of materials"
                               4 vs ns cs es ms face)]
        [else
         (quad-shape (box 'lazy) vs ns cs es ms face)]))

(: make-rectangle-shape (-> FlAABB3 FlVector FlVector material Face rectangle-shape))
(define (make-rectangle-shape b c e m face)
  (cond [(not (= 4 (flvector-length c)))
         (raise-argument-error 'make-rectangle-shape "length-4 flvector" 1 b c e m face)]
        [(not (= 3 (flvector-length e)))
         (raise-argument-error 'make-rectangle-shape "length-3 flvector" 2 b c e m face)]
        [else
         (rectangle-shape (box 'lazy) b c e m face)]))

(: make-sphere-shape (-> (U FlAffine3- affine) FlVector FlVector material Boolean sphere-shape))
(define (make-sphere-shape t c e m inside?)
  (cond [(not (= 4 (flvector-length c)))
         (raise-argument-error 'make-rectangle-shape "length-4 flvector" 1 t c e m inside?)]
        [(not (= 3 (flvector-length e)))
         (raise-argument-error 'make-rectangle-shape "length-3 flvector" 2 t c e m inside?)]
        [else
         (sphere-shape (box 'lazy) (->affine t) c e m inside?)]))

(: make-directional-light-shape (-> FlVector FlVector directional-light-shape))
(define (make-directional-light-shape intensity direction)
  (directional-light-shape (box 'lazy) intensity direction))

(: make-point-light-shape (-> FlVector FlVector Flonum point-light-shape))
(define (make-point-light-shape intensity position radius)
  (point-light-shape (box 'lazy) intensity position radius))

(: make-frozen-scene-shape (-> Nonempty-Scene Shape))
(define (make-frozen-scene-shape s)
  (frozen-scene-shape (box 'lazy) s))

;; ===================================================================================================
;; Shape bounding box

(define sphere-aabb
  (assert (make-flaabb3 (flvector -1.0 -1.0 -1.0)
                        (flvector +1.0 +1.0 +1.0))
          values))

(define directional-light-aabb
  (assert (make-flaabb3 (flvector -inf.0 -inf.0 -inf.0)
                        (flvector +inf.0 +inf.0 +inf.0))
          values))

(: shape-aabb (-> Shape FlAABB3))
(define (shape-aabb s)
  (cond
    [(solid-shape? s)
     (cond [(triangle-shape? s)  (fltriangle3-aabb (triangle-shape-fltriangle3 s))]
           [(quad-shape? s)  (assert (flv3aabb (quad-shape-vertices s)) values)]
           [(rectangle-shape? s)  (rectangle-shape-aabb s)]
           [(sphere-shape? s)  sphere-aabb])]
    [(light-shape? s)
     (cond [(directional-light-shape? s)  directional-light-aabb]
           [(point-light-shape? s)
            (define p (point-light-shape-position s))
            (define radius (point-light-shape-radius s))
            (define r (flvector radius radius radius))
            (assert (make-flaabb3 (flv3- p r) (flv3+ p r)) values)])]
    [(frozen-scene-shape? s)
     (flscene3-aabb (frozen-scene-shape-scene s))]))

;; ===================================================================================================
;; Shape transformation

(: shape-transform (-> Shape FlAffine3- FlAffine3- (Listof Shape)))
(define (shape-transform a t tinv)
  (cond
    [(flidentity3? t)  (list a)]
    [else
     (: transform-pos (-> FlVector FlVector))
     (define (transform-pos v)
       (flv4->pos (flt3apply t (pos->flv4 v))))
     
     (: transform-norm (-> FlVector FlVector))
     (define (transform-norm v)
       (let ([v  (flv3normalize (flv4->dir (flt3tapply tinv (dir->flv4 v))))])
         (if v v (flvector 0.0 0.0 0.0))))
     
     (cond
       [(solid-shape? a)
        (cond
          [(triangle-shape? a)
           (match-define (triangle-shape passes tri ns cs es ms face) a)
           (define vs (fltriangle3-vertices tri))
           (define new-vs (vector-map transform-pos vs))
           (define new-ns (if (vector? ns) (vector-map transform-norm ns) (transform-norm ns)))
           (list (triangle-shape (box 'lazy) (fltriangle3 new-vs) new-ns cs es ms
                                 (if (flt3consistent? t) face (opposite-gl-face face))))]
          ;; Quad: transform vertices
          [(quad-shape? a)
           (match-define (quad-shape passes vs ns cs es ms face) a)
           (define new-vs (vector-map transform-pos vs))
           (define new-ns (if (vector? ns) (vector-map transform-norm ns) (transform-norm ns)))
           (list (quad-shape (box 'lazy) new-vs new-ns cs es ms
                             (if (flt3consistent? t) face (opposite-gl-face face))))]
          ;; Rectangle: split into quads
          [(rectangle-shape? a)
           (match-define (rectangle-shape passes b c e m old-face) a)
           (define-values (xmin ymin zmin xmax ymax zmax) (flaabb3-values b))
           
           (define face (if (flt3consistent? t) old-face (opposite-gl-face old-face)))
           
           (define v1 (transform-pos (flvector xmin ymin zmin)))
           (define v2 (transform-pos (flvector xmax ymin zmin)))
           (define v3 (transform-pos (flvector xmax ymax zmin)))
           (define v4 (transform-pos (flvector xmin ymax zmin)))
           (define v5 (transform-pos (flvector xmin ymin zmax)))
           (define v6 (transform-pos (flvector xmax ymin zmax)))
           (define v7 (transform-pos (flvector xmax ymax zmax)))
           (define v8 (transform-pos (flvector xmin ymax zmax)))
           (define nleft  (transform-norm (flvector -1.0 0.0 0.0)))
           (define nright (transform-norm (flvector +1.0 0.0 0.0)))
           (define nfront (transform-norm (flvector 0.0 -1.0 0.0)))
           (define nback  (transform-norm (flvector 0.0 +1.0 0.0)))
           (define nbot   (transform-norm (flvector 0.0 0.0 -1.0)))
           (define ntop   (transform-norm (flvector 0.0 0.0 +1.0)))
           
           (list (make-quad-shape (vector v4 v3 v2 v1) nbot   c e m face)
                 (make-quad-shape (vector v5 v6 v7 v8) ntop   c e m face)
                 (make-quad-shape (vector v1 v2 v6 v5) nfront c e m face)
                 (make-quad-shape (vector v3 v4 v8 v7) nback  c e m face)
                 (make-quad-shape (vector v4 v1 v5 v8) nleft  c e m face)
                 (make-quad-shape (vector v2 v3 v7 v6) nright c e m face))]
          [(sphere-shape? a)
           (match-define (sphere-shape passes t0 c e m inside?) a)
           (list (sphere-shape (box 'lazy) (affine-compose t t0) c e m inside?))])]
       [(light-shape? a)
        (cond
          [(directional-light-shape? a)  (list a)]
          [(point-light-shape? a)
           (match-define (point-light-shape passes intensity position radius) a)
           (list (point-light-shape (box 'lazy) intensity (transform-pos position) radius))])]
       ;; Frozen scene
       [(frozen-scene-shape? a)
        (: transformed-shape (-> Shape FlAffine3- (Listof Shape)))
        (define (transformed-shape s t)
          (shape-transform s t (flt3inverse t)))
        
        (append* (flscene3-extract (flscene3-transform-shapes
                                    (frozen-scene-shape-scene a)
                                    t tinv)
                                   identity-flt3
                                   flt3compose
                                   transformed-shape))])]))

;; ===================================================================================================
;; Shape-specific scene functions

(define-values (shape->flscene3 flscene3-transform-shapes)
  ((inst make-flscene3-ops Shape)
   shape-aabb
   shape-transform))

;; ===================================================================================================
;; Utils for packing and unpacking vertex data

(: normal->rgb-bytes (-> FlVector Bytes))
(define (normal->rgb-bytes v)
  (define-values (x y z) (flv3-values v))
  (: flonum->byte (-> Flonum Byte))
  (define (flonum->byte x)
    (assert (max 0 (min 255 (+ 127 (exact-ceiling (* x 127.0))))) byte?))
  (bytes (flonum->byte x)
         (flonum->byte y)
         (flonum->byte z)))

(: decode-normal (-> FlVector FlVector))
(define (decode-normal v)
  (define zero #i127/255)
  (let ([v  (flv3normalize (flv3- v (flvector zero zero zero)))])
    (if v v (flvector 0.0 0.0 0.0))))

(: pack-emitted (-> FlVector Bytes))
(define (pack-emitted c)
  (define-values (h s v) (flv3-values (rgb->hsv c)))
  (let* ([v  (max 0 (min 65535 (exact-floor (* v 255.0))))]
         [v.hi  (quotient v 256)]
         [v.lo  (- v (* v.hi 256))])
    (bytes (flonum->byte h)
           (flonum->byte s)
           v.hi
           v.lo)))

(: decode-emitted (-> FlVector FlVector))
(define (decode-emitted c)
  (define h (flvector-ref c 0))
  (define s (flvector-ref c 1))
  (define v.hi (flvector-ref c 2))
  (define v.lo (flvector-ref c 3))
  (hsv->rgb (flvector h s (+ v.lo (* 256.0 v.hi)))))

;; ===================================================================================================
;; Program pieces

(define model-vertex-code
  (string-append
   matrix-code
   #<<code
in vec4 _model0;
in vec4 _model1;
in vec4 _model2;

mat4x3 get_model_transform() {
  return rows2mat4x3(_model0, _model1, _model2);
}
code
   "\n\n"))

(define output-mat-fragment-code
  (string-append
   depth-fragment-code
   pack-unpack-normal-code
   #<<code
void output_mat(vec3 dir, float roughness, float z, float znear, float zfar) {
  gl_FragDepth = frag_depth(znear, zfar, z);
  gl_FragColor = vec4(pack_normal(normalize(dir)), 1.0, roughness);
}
code
   "\n\n"))

(define output-opaq-fragment-code
  (string-append
   depth-fragment-code
   #<<code
void output_opaq(vec3 color, float a, float z, float znear, float zfar) {
  gl_FragDepth = frag_depth(znear, zfar, z);
  gl_FragColor = vec4(color, a);
}
code
   "\n\n"))

(define output-tran-fragment-code
  (string-append
   depth-fragment-code
   #<<code
void output_tran(vec3 color, float a, float z, float znear, float zfar) {
  float depth = frag_depth(znear, zfar, z);
  float d = 1 - depth;
  float weight = a * clamp(1 / (d*d*d) - 1, 0.001953125, 32768.0);
  gl_FragDepth = depth;
  gl_FragData[0] = vec4(color * weight * a, a);
  gl_FragData[1] = vec4(a * weight);
}
code
   "\n\n"))

(define light-fragment-code
  (string-append
   get-surface-fragment-code
   #<<code
vec3 attenuate_invsqr(vec3 light_color, float dist) {
  return max(vec3(0.0), (light_color/(dist*dist) - 0.05) / 0.95);
}

vec3 attenuate_linear(vec3 light_color, float radius, float dist) {
  return light_color * max(0.0, (radius - dist) / radius);
}

// Ward model for anisotropic, but without the anisotropy (so that it's consistent with the
// full anisotropic model if we ever want to use it)
float specular(vec3 N, vec3 L, vec3 V, float dotLN, float dotVN, float m) {
  vec3 uH = L+V;  // unnormalized half vector
  float dotsum = dotVN + dotLN;
  float dotHNsqr = dotsum * dotsum / dot(uH,uH);  // pow(dot(N,normalize(uH)),2)
  float mm = m * m;
  return sqrt(dotLN/dotVN) / (12.566371 * mm) * exp((dotHNsqr - 1.0) / (mm * dotHNsqr));
}

void output_light(vec3 light, surface s, vec3 L, vec3 V) {
  vec3 N = s.normal;
  float dotNL = dot(N,L);
  if (dotNL < 1e-7) discard;
  float dotNV = dot(N,V);
  if (dotNV < 1e-7) discard;
  gl_FragData[0] = vec4(light * dotNL, 0.0);
  gl_FragData[1] = vec4(light * specular(N,L,V,dotNL,dotNV,s.roughness), 0.0);
}
code
   "\n\n"))

;; ===================================================================================================
;; Solid polygon programs

;; ---------------------------------------------------------------------------------------------------
;; Program 1: material

(define polygon-mat-vertex-code
  (string-append
   "#version 130\n\n"
   model-vertex-code
   #<<code
uniform mat4 view;
uniform mat4 unview;
uniform mat4 proj;

in vec4 vert_normal_roughness;
in vec3 vert_position;

smooth out vec4 frag_position;
smooth out vec3 frag_normal;
smooth out float frag_roughness;

void main() {
  mat4x3 model = get_model_transform();
  mat4x3 unmodel = affine_inverse(model);
  vec4 position = view * (a2p(model) * vec4(vert_position,1));
  gl_Position = proj * position;
  vec3 normal = normalize(vert_normal_roughness.xyz - vec3(127.0/255.0));
  vec4 norm = (vec4(normal,0) * a2p(unmodel)) * unview;
  frag_position = position;
  frag_normal = normalize(norm.xyz);
  frag_roughness = vert_normal_roughness.w;
}
code
   ))

(define polygon-mat-fragment-code
  (string-append
   "#version 130\n\n"
   output-mat-fragment-code
   #<<code
uniform float znear;
uniform float zfar;

smooth in vec4 frag_position;
smooth in vec3 frag_normal;
smooth in float frag_roughness;

void main() {
  output_mat(frag_normal, frag_roughness, frag_position.z, znear, zfar);
}
code
   ))

(define-singleton (polygon-mat-program-spec)
  (define struct
    (make-vao-struct
     (make-vao-field "vert_normal_roughness" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_position" 3 GL_FLOAT)))
  
  (define program
    (make-gl-program
     struct
     (list (make-gl-shader GL_VERTEX_SHADER polygon-mat-vertex-code)
           (make-gl-shader GL_FRAGMENT_SHADER polygon-mat-fragment-code))))
  
  (define uniforms
    (list (cons "view" 'view)
          (cons "unview" 'unview)
          (cons "proj" 'proj)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)))
  
  (program-spec program uniforms struct))

;; ---------------------------------------------------------------------------------------------------
;; Program 2: color

(define polygon-draw-vertex-code
  (string-append
   "#version 130\n\n"
   model-vertex-code
   rgb-hsv-code
   #<<code
uniform mat4 view;
uniform mat4 proj;

in vec4 vert_rcolor;
in vec4 vert_ecolor;    // vec4(hue, saturation, value.hi, value.lo)
in vec4 vert_material;  // vec4(ambient, diffuse, specular, 0)
in vec3 vert_position;

smooth out vec4 frag_position;
smooth out vec4 frag_rcolor;
smooth out vec3 frag_ecolor;
smooth out float frag_ambient;
smooth out float frag_diffuse;
smooth out float frag_specular;

void main() {
  mat4x3 model = get_model_transform();
  vec4 position = view * (a2p(model) * vec4(vert_position,1));
  gl_Position = proj * position;
  frag_position = position;
  frag_rcolor = vert_rcolor;
  frag_ecolor = hsv_to_rgb(vec3(vert_ecolor.xy, vert_ecolor.w + 256.0 * vert_ecolor.z));
  frag_ambient = vert_material.x;
  frag_diffuse = vert_material.y;
  frag_specular = vert_material.z;
}
code
   ))

(define polygon-opaq-fragment-code
  (string-append
   "#version 130\n\n"
   output-opaq-fragment-code
   #<<code
uniform float znear;
uniform float zfar;

uniform vec3 ambient;
uniform sampler2D diffuse;
uniform sampler2D specular;

smooth in vec4 frag_position;
smooth in vec4 frag_rcolor;
smooth in vec3 frag_ecolor;
smooth in float frag_ambient;
smooth in float frag_diffuse;
smooth in float frag_specular;

void main() {
  vec3 diff = texelFetch(diffuse, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 spec = texelFetch(specular, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
  vec3 color = frag_ecolor + frag_rcolor.rgb * light;
  output_opaq(color, frag_rcolor.a, frag_position.z, znear, zfar);
}
code
   ))

(define polygon-tran-fragment-code
  (string-append
   "#version 130\n\n"
   output-tran-fragment-code
   #<<code
uniform float znear;
uniform float zfar;

uniform vec3 ambient;
uniform sampler2D diffuse;
uniform sampler2D specular;

smooth in vec4 frag_position;
smooth in vec4 frag_rcolor;
smooth in vec3 frag_ecolor;
smooth in float frag_ambient;
smooth in float frag_diffuse;
smooth in float frag_specular;

void main() {
  vec3 diff = texelFetch(diffuse, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 spec = texelFetch(specular, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
  vec3 color = frag_ecolor + frag_rcolor.rgb * light;
  output_tran(color, frag_rcolor.a, frag_position.z, znear, zfar);
}
code
   ))

(define-singleton (polygon-opaq-program-spec)
  (define struct
    (make-vao-struct
     (make-vao-field "vert_rcolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_ecolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_material" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_position" 3 GL_FLOAT)))
  
  (define program
    (make-gl-program struct
                     (list (make-gl-shader GL_VERTEX_SHADER polygon-draw-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER polygon-opaq-fragment-code))))
  
  (define uniforms
    (list (cons "view" 'view)
          (cons "proj" 'proj)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)
          (cons "ambient" 'ambient)
          (cons "diffuse" 'diffuse)
          (cons "specular" 'specular)))
  
  (program-spec program uniforms struct))

(define-singleton (polygon-tran-program-spec)
  (define struct
    (make-vao-struct
     (make-vao-field "vert_rcolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_ecolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_material" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_position" 3 GL_FLOAT)))
  
  (define program
    (make-gl-program struct
                     (list (make-gl-shader GL_VERTEX_SHADER polygon-draw-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER polygon-tran-fragment-code))))
  
  (define uniforms
    (list (cons "view" 'view)
          (cons "proj" 'proj)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)
          (cons "diffuse" 'diffuse)
          (cons "specular" 'specular)))
  
  (program-spec program uniforms struct))

;; ===================================================================================================
;; Sphere programs

;; ---------------------------------------------------------------------------------------------------
;; Program 1: material

(define sphere-mat-vertex-code
  (string-append
   "#version 130\n\n"
   output-impostor-quad-vertex-code
   model-vertex-code
   #<<code
uniform mat4 view;
uniform mat4 unview;
uniform mat4 proj;

in vec4 sphere0;
in vec4 sphere1;
in vec4 sphere2;
in vec4 vert_roughness_inside;

flat out mat4x3 frag_trans;
flat out mat4x3 frag_untrans;
flat out float frag_roughness;
flat out float frag_inside;
smooth out float frag_is_degenerate;

void main() {
  mat4x3 sphere = rows2mat4x3(sphere0, sphere1, sphere2);
  mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(sphere));
  mat4x3 unmodel = affine_inverse(model);
  mat4 trans = view * a2p(model);
  mat4 untrans = a2p(unmodel) * unview;

  frag_trans = mat4x3(trans);
  frag_untrans = mat4x3(untrans);
  frag_roughness = vert_roughness_inside.x;
  frag_inside = vert_roughness_inside.y;
  frag_is_degenerate = output_impostor_quad(trans, proj, vec3(-1.0), vec3(+1.0));
}
code
   ))

(define sphere-mat-fragment-code
  (string-append
   "#version 130\n\n"
   output-mat-fragment-code
   ray-trace-fragment-code
   #<<code
uniform mat4 proj;
uniform mat4 unproj;
uniform float znear;
uniform float zfar;
uniform int width;
uniform int height;

flat in mat4x3 frag_trans;
flat in mat4x3 frag_untrans;
flat in float frag_roughness;
flat in float frag_inside;
smooth in float frag_is_degenerate;

void main() {
  // all fragments should discard if this one does
  if (frag_is_degenerate > 0.0) discard;

  vec3 vdir = frag_coord_to_direction(gl_FragCoord, unproj, width, height);
  vec3 start = frag_untrans[3];  // equiv. to multiplying by vec3(0)
  vec3 dir = normalize(mat3(frag_untrans) * vdir);
  vec2 ts = unit_sphere_intersect(start, dir);
  float t = mix(ts.x, ts.y, frag_inside);
  // many nearby fragments should discard if this one does
  if (t <= 0.0) discard;
  
  vec3 pos = start + dir * t;
  vec3 vpos = frag_trans * vec4(pos, 1.0);
  vec3 vnorm = pos * mat3(frag_untrans);
  output_mat(mix(vnorm,-vnorm,frag_inside), frag_roughness, vpos.z, znear, zfar);
}
code
   ))

(define-singleton (sphere-mat-program-spec)
  (define struct
    (make-vao-struct
     (make-vao-field "sphere0" 4 GL_FLOAT)
     (make-vao-field "sphere1" 4 GL_FLOAT)
     (make-vao-field "sphere2" 4 GL_FLOAT)
     (make-vao-field "vert_roughness_inside" 4 GL_UNSIGNED_BYTE)))
  
  (define program
    (make-gl-program struct
                     (list (make-gl-shader GL_VERTEX_SHADER sphere-mat-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER sphere-mat-fragment-code))))
  
  (define uniforms
    (list (cons "view" 'view)
          (cons "unview" 'unview)
          (cons "proj" 'proj)
          (cons "unproj" 'unproj)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)
          (cons "width" 'width)
          (cons "height" 'height)))
  
  (program-spec program uniforms struct))

;; ---------------------------------------------------------------------------------------------------
;; Program 2: color

(define sphere-draw-vertex-code
  (string-append
   "#version 130\n\n"
   output-impostor-quad-vertex-code
   rgb-hsv-code
   model-vertex-code
   #<<code
uniform mat4 view;
uniform mat4 unview;
uniform mat4 proj;

in vec4 sphere0;
in vec4 sphere1;
in vec4 sphere2;
in vec4 vert_rcolor;
in vec4 vert_ecolor;           // vec4(hue, saturation, value.hi, value.lo)
in vec4 vert_material_inside;  // vec4(ambient, diffuse, specular, inside?)

flat out mat4x3 frag_trans;
flat out mat4x3 frag_untrans;
flat out vec4 frag_rcolor;
flat out vec3 frag_ecolor;
flat out float frag_ambient;
flat out float frag_diffuse;
flat out float frag_specular;
flat out float frag_inside;
smooth out float frag_is_degenerate;

void main() {
  mat4x3 sphere = rows2mat4x3(sphere0, sphere1, sphere2);
  mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(sphere));
  mat4x3 unmodel = affine_inverse(model);
  mat4 trans = view * a2p(model);
  mat4 untrans = a2p(unmodel) * unview;

  frag_trans = mat4x3(trans);
  frag_untrans = mat4x3(untrans);
  frag_rcolor = vert_rcolor;
  frag_ecolor = hsv_to_rgb(vec3(vert_ecolor.xy, vert_ecolor.w + 256.0 * vert_ecolor.z));
  frag_ambient = vert_material_inside.x;
  frag_diffuse = vert_material_inside.y;
  frag_specular = vert_material_inside.z;
  frag_inside = vert_material_inside.w;
  frag_is_degenerate = output_impostor_quad(trans, proj, vec3(-1.0), vec3(+1.0));
}
code
   ))

(define sphere-opaq-fragment-code
  (string-append
   "#version 130\n\n"
   output-opaq-fragment-code
   ray-trace-fragment-code
   #<<code
uniform mat4 proj;
uniform mat4 unproj;
uniform float znear;
uniform float zfar;
uniform int width;
uniform int height;

uniform vec3 ambient;
uniform sampler2D diffuse;
uniform sampler2D specular;

flat in mat4x3 frag_trans;
flat in mat4x3 frag_untrans;
flat in vec4 frag_rcolor;
flat in vec3 frag_ecolor;
flat in float frag_ambient;
flat in float frag_diffuse;
flat in float frag_specular;
flat in float frag_inside;
smooth in float frag_is_degenerate;

void main() {
  // all fragments should discard if this one does
  if (frag_is_degenerate > 0.0) discard;

  vec3 vdir = frag_coord_to_direction(gl_FragCoord, unproj, width, height);
  vec3 start = frag_untrans[3];  // equiv. to multiplying by vec3(0)
  vec3 dir = normalize(mat3(frag_untrans) * vdir);
  vec2 ts = unit_sphere_intersect(start, dir);
  float t = mix(ts.x, ts.y, frag_inside);
  // many nearby fragments should discard if this one does
  if (t <= 0.0) discard;
  
  vec3 pos = start + dir * t;
  vec3 vpos = frag_trans * vec4(pos, 1.0);
  
  vec3 diff = texelFetch(diffuse, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 spec = texelFetch(specular, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
  vec3 color = frag_ecolor + frag_rcolor.rgb * light;
  output_opaq(color, frag_rcolor.a, vpos.z, znear, zfar);
}
code
   ))

(define sphere-tran-fragment-code
  (string-append
   "#version 130\n\n"
   output-tran-fragment-code
   ray-trace-fragment-code
   #<<code
uniform mat4 proj;
uniform mat4 unproj;
uniform float znear;
uniform float zfar;
uniform int width;
uniform int height;

uniform vec3 ambient;
uniform sampler2D diffuse;
uniform sampler2D specular;

flat in mat4x3 frag_trans;
flat in mat4x3 frag_untrans;
flat in vec4 frag_rcolor;
flat in vec3 frag_ecolor;
flat in float frag_ambient;
flat in float frag_diffuse;
flat in float frag_specular;
flat in float frag_inside;
smooth in float frag_is_degenerate;

void main() {
  // all fragments should discard if this one does
  if (frag_is_degenerate > 0.0) discard;

  vec3 vdir = frag_coord_to_direction(gl_FragCoord, unproj, width, height);
  vec3 start = frag_untrans[3];  // equiv. to multiplying by vec3(0)
  vec3 dir = normalize(mat3(frag_untrans) * vdir);
  vec2 ts = unit_sphere_intersect(start, dir);
  float t = mix(ts.x, ts.y, frag_inside);
  // many nearby fragments should discard if this one does
  if (t <= 0.0) discard;
  
  vec3 pos = start + dir * t;
  vec3 vpos = frag_trans * vec4(pos, 1.0);
  
  vec3 diff = texelFetch(diffuse, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 spec = texelFetch(specular, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
  vec3 color = frag_ecolor + frag_rcolor.rgb * light;
  output_tran(color, frag_rcolor.a, vpos.z, znear, zfar);
}
code
   ))

(define-singleton (sphere-opaq-program-spec)
  (define struct
    (make-vao-struct
     (make-vao-field "sphere0" 4 GL_FLOAT)
     (make-vao-field "sphere1" 4 GL_FLOAT)
     (make-vao-field "sphere2" 4 GL_FLOAT)
     (make-vao-field "vert_rcolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_ecolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_material_inside" 4 GL_UNSIGNED_BYTE)))
  
  (define program
    (make-gl-program struct
                     (list (make-gl-shader GL_VERTEX_SHADER sphere-draw-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER sphere-opaq-fragment-code))))
  
  (define uniforms
    (list (cons "view" 'view)
          (cons "proj" 'proj)
          (cons "unview" 'unview)
          (cons "unproj" 'unproj)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)
          (cons "width" 'width)
          (cons "height" 'height)
          (cons "ambient" 'ambient)
          (cons "diffuse" 'diffuse)
          (cons "specular" 'specular)))
  
  (program-spec program uniforms struct))

(define-singleton (sphere-tran-program-spec)
  (define struct
    (make-vao-struct
     (make-vao-field "sphere0" 4 GL_FLOAT)
     (make-vao-field "sphere1" 4 GL_FLOAT)
     (make-vao-field "sphere2" 4 GL_FLOAT)
     (make-vao-field "vert_rcolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_ecolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_material_inside" 4 GL_UNSIGNED_BYTE)))
  
  (define program
    (make-gl-program struct
                     (list (make-gl-shader GL_VERTEX_SHADER sphere-draw-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER sphere-tran-fragment-code))))
  
  (define uniforms
    (list (cons "view" 'view)
          (cons "proj" 'proj)
          (cons "unview" 'unview)
          (cons "unproj" 'unproj)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)
          (cons "width" 'width)
          (cons "height" 'height)
          (cons "ambient" 'ambient)
          (cons "diffuse" 'diffuse)
          (cons "specular" 'specular)))
  
  (program-spec program uniforms struct))

;; ===================================================================================================
;; Directional light program

(define directional-light-vertex-code
  (string-append
   #<<code
#version 130

void main() {
    // output the right vertices for a triangle strip
  switch (gl_VertexID) {
  case 0:
    gl_Position = vec4(-1.0, -1.0, 0.0, 1.0);
    break;
  case 1:
    gl_Position = vec4(+1.0, -1.0, 0.0, 1.0);
    break;
  case 2:
    gl_Position = vec4(-1.0, +1.0, 0.0, 1.0);
    break;
  default:
    gl_Position = vec4(+1.0, +1.0, 0.0, 1.0);
    break;
  }
}
code
   ))

(define directional-light-fragment-code
  (string-append
   "#version 130\n\n"
   get-view-position-fragment-code
   light-fragment-code
   #<<code
uniform mat4 unview;
uniform mat3 unproj0;
uniform mat3 unproj1;
uniform float znear;
uniform float zfar;
uniform int width;
uniform int height;

uniform sampler2D depth;
uniform sampler2D material;

// Per-light attributes
uniform vec3 light_dir;
uniform vec3 light_color;

void main() {
  vec3 pos = get_view_position(depth, width, height, unproj0, unproj1, znear, zfar);
  vec3 L = normalize(-light_dir * mat3(unview));
  vec3 V = normalize(-pos);
  output_light(light_color, get_surface(material), L, V);
}
code
   ))

(define-singleton (directional-light-program-spec)
  (define struct (make-vao-struct))
  
  (define program
    (make-gl-program struct
                     (list (make-gl-shader GL_VERTEX_SHADER directional-light-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER directional-light-fragment-code))))
  
  (define uniforms
    (list (cons "unview" 'unview)
          (cons "unproj0" 'unproj0)
          (cons "unproj1" 'unproj1)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)
          (cons "width" 'width)
          (cons "height" 'height)
          (cons "depth" 'depth)
          (cons "material" 'material)))
  
  (program-spec program uniforms struct))

;; ===================================================================================================
;; Point light program

(define point-light-vertex-code
  (string-append
   "#version 130\n\n"
   output-impostor-quad-vertex-code
   model-vertex-code
   #<<code
uniform mat4 view;
uniform mat4 proj;

in vec4 vert_position_radius;
in vec3 vert_intensity;

flat out vec3 frag_position;
flat out float frag_radius;
flat out vec3 frag_intensity;
smooth out float frag_is_degenerate;

void main() {
  mat4x3 model = get_model_transform();
  mat4 trans = view * a2p(model);
  vec3 position = vert_position_radius.xyz;
  float radius = vert_position_radius.w;
  vec3 wmin = position - vec3(radius);
  vec3 wmax = position + vec3(radius);
  frag_position = (trans * vec4(position.xyz,1)).xyz;
  frag_radius = radius;
  frag_intensity = vert_intensity;
  frag_is_degenerate = output_impostor_quad(trans, proj, wmin, wmax);
}
code
   ))

(define point-light-fragment-code
  (string-append
   "#version 130\n\n"
   get-view-position-fragment-code
   light-fragment-code
   #<<code
uniform float znear;
uniform float zfar;
uniform int width;
uniform int height;
uniform mat3 unproj0;
uniform mat3 unproj1;

uniform sampler2D depth;
uniform sampler2D material;

flat in vec3 frag_position;
flat in float frag_radius;
flat in vec3 frag_intensity;
smooth in float frag_is_degenerate;

void main() {
  // all fragments should discard if this one does
  if (frag_is_degenerate > 0.0) discard;

  vec3 vpos = get_view_position(depth, width, height, unproj0, unproj1, znear, zfar);
  vec3 D = frag_position - vpos;
  float dist = length(D);
  if (dist > frag_radius) discard;
  
  vec3 L = normalize(D);
  vec3 V = normalize(-vpos);
  vec3 light = attenuate_invsqr(frag_intensity, dist);
  //vec3 light = attenuate_linear(frag_intensity, frag_radius, dist);
  output_light(light, get_surface(material), L, V);
}
code
   ))

(define-singleton (point-light-program-spec)
  (define struct
    (make-vao-struct
     (make-vao-field "vert_position_radius" 4 GL_FLOAT)
     (make-vao-field "vert_intensity" 3 GL_FLOAT)))
  
  (define program
    (make-gl-program struct
                     (list (make-gl-shader GL_VERTEX_SHADER point-light-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER point-light-fragment-code))))
  
  (define uniforms
    (list (cons "view" 'view)
          (cons "proj" 'proj)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)
          (cons "width" 'width)
          (cons "height" 'height)
          (cons "unproj0" 'unproj0)
          (cons "unproj1" 'unproj1)
          (cons "depth" 'depth)
          (cons "material" 'material)))
  
  (program-spec program uniforms struct))

;; ===================================================================================================
;; Polygon shape passes

(: make-polygon-shape-passes (-> Integer
                                 Index
                                 (Vectorof FlVector)
                                 (U FlVector (Vectorof FlVector))
                                 (U FlVector (Vectorof FlVector))
                                 (U FlVector (Vectorof FlVector))
                                 (U material (Vectorof material))
                                 Face
                                 Passes))
(define (make-polygon-shape-passes mode len vs ns cs es ms face)
  (define mat-data-size (* len (vao-struct-size (program-spec-struct (polygon-mat-program-spec)))))
  (define mat-data (make-bytes mat-data-size))
  (define mat-ptr (u8vector->cpointer mat-data))
  (for/fold ([i : Nonnegative-Fixnum  0]
             ) ([v  (in-vector vs)]
                [n  (in-vector (if (vector? ns) ns (make-vector len ns)))]
                [m  (in-vector (if (vector? ms) ms (make-vector len ms)))])
    (let* ([i  (begin (bytes-copy! mat-data i (normal->rgb-bytes n) 0 3)
                      (unsafe-fx+ i 3))]
           [i  (begin (bytes-set! mat-data i (flonum->byte (material-roughness m)))
                      (unsafe-fx+ i 1))]
           [i  (begin (memcpy mat-ptr i (f32vector->cpointer (flvector->f32vector v)) 12 _byte)
                      (unsafe-fx+ i 12))])
      i))
  
  (define draw-data-size (* len (vao-struct-size (program-spec-struct (polygon-opaq-program-spec)))))
  (define draw-data (make-bytes draw-data-size))
  (define draw-ptr (u8vector->cpointer draw-data))
  (for/fold ([i : Nonnegative-Fixnum  0]
             ) ([c  (in-vector (if (vector? cs) cs (make-vector len cs)))]
                [e  (in-vector (if (vector? es) es (make-vector len es)))]
                [m  (in-vector (if (vector? ms) ms (make-vector len ms)))]
                [v  (in-vector vs)])
    (let* ([i  (begin (bytes-copy! draw-data i (pack-color c) 0 4)
                      (unsafe-fx+ i 4))]
           [i  (begin (bytes-copy! draw-data i (pack-emitted e) 0 4)
                      (unsafe-fx+ i 4))]
           [i  (begin (bytes-set! draw-data i (flonum->byte (material-ambient m)))
                      (unsafe-fx+ i 1))]
           [i  (begin (bytes-set! draw-data i (flonum->byte (material-diffuse m)))
                      (unsafe-fx+ i 1))]
           [i  (begin (bytes-set! draw-data i (flonum->byte (material-specular m)))
                      ;; last byte is unused
                      (unsafe-fx+ i 2))]
           [i  (begin (memcpy draw-ptr i (f32vector->cpointer (flvector->f32vector v)) 12 _byte)
                      (unsafe-fx+ i 12))])
      i))
  
  (: transparent? Boolean)
  (define transparent?
    (if (vector? cs)
        (for/or ([c  (in-vector cs)])
          (< (flvector-ref c 3) 1.0))
        (< (flvector-ref cs 3) 1.0)))
  
  (: passes Passes)
  (define passes
    (if transparent?
        (vector
         #()
         #()
         #()
         (vector (shape-params polygon-mat-program-spec empty face mode
                               (single-vertices len mat-data)))
         (vector (shape-params polygon-tran-program-spec empty face mode
                               (single-vertices len draw-data))))
        (vector
         #()
         (vector (shape-params polygon-mat-program-spec empty face mode
                               (single-vertices len mat-data)))
         (vector (shape-params polygon-opaq-program-spec empty face mode
                               (single-vertices len draw-data)))
         #()
         #())))
  passes)

(: make-triangle-shape-passes (-> triangle-shape Passes))
(define (make-triangle-shape-passes a)
  (match-define (triangle-shape _ t ns cs es ms face) a)
  (define vs (fltriangle3-vertices t))
  (make-polygon-shape-passes GL_TRIANGLES 3 vs ns cs es ms face))

(: make-quad-shape-passes (-> quad-shape Passes))
(define (make-quad-shape-passes a)
  (match-define (quad-shape _ vs ns cs es ms face) a)
  (make-polygon-shape-passes GL_QUADS 4 vs ns cs es ms face))

;; ===================================================================================================
;; Rectangle shape passes

(: make-rectangle-shape-passes (-> rectangle-shape Passes))
(define (make-rectangle-shape-passes a)
  (match-define (rectangle-shape _ aabb c e m face) a)
  
  (define-values (xmin ymin zmin xmax ymax zmax) (flaabb3-values aabb))
  
  (define v1 (f32vector xmin ymin zmin))
  (define v2 (f32vector xmax ymin zmin))
  (define v3 (f32vector xmax ymax zmin))
  (define v4 (f32vector xmin ymax zmin))
  (define v5 (f32vector xmin ymin zmax))
  (define v6 (f32vector xmax ymin zmax))
  (define v7 (f32vector xmax ymax zmax))
  (define v8 (f32vector xmin ymax zmax))
  (define nleft  (normal->rgb-bytes (flvector -1.0 0.0 0.0)))
  (define nright (normal->rgb-bytes (flvector +1.0 0.0 0.0)))
  (define nfront (normal->rgb-bytes (flvector 0.0 -1.0 0.0)))
  (define nback  (normal->rgb-bytes (flvector 0.0 +1.0 0.0)))
  (define nbot   (normal->rgb-bytes (flvector 0.0 0.0 -1.0)))
  (define ntop   (normal->rgb-bytes (flvector 0.0 0.0 +1.0)))
  
  (define vs (vector v4 v3 v2 v1
                     v5 v6 v7 v8
                     v1 v2 v6 v5
                     v3 v4 v8 v7
                     v4 v1 v5 v8
                     v2 v3 v7 v6))
  (define ns (vector nbot nbot nbot nbot
                     ntop ntop ntop ntop
                     nfront nfront nfront nfront
                     nback nback nback nback
                     nleft nleft nleft nleft
                     nright nright nright nright))
  
  (define r (flonum->byte (material-roughness m)))
  
  (define mat-data-size (* 24 (vao-struct-size (program-spec-struct (polygon-mat-program-spec)))))
  (define mat-data (make-bytes mat-data-size))
  (define mat-ptr (u8vector->cpointer mat-data))
  (for/fold ([i : Nonnegative-Fixnum  0]
             ) ([v  (in-vector vs)]
                [n  (in-vector ns)])
    (let* ([i  (begin (bytes-copy! mat-data i n 0 3)
                      (unsafe-fx+ i 3))]
           [i  (begin (bytes-set! mat-data i r)
                      (unsafe-fx+ i 1))]
           [i  (begin (memcpy mat-ptr i (f32vector->cpointer v) 12 _byte)
                      (unsafe-fx+ i 12))])
      i))
  
  (define material-data-size
    (assert (- (vao-struct-size (program-spec-struct (polygon-opaq-program-spec))) 12) positive?))
  (define material-data (make-bytes material-data-size))
  (define material-ptr (u8vector->cpointer material-data))
  (let* ([i  (begin (bytes-copy! material-data 0 (pack-color c) 0 4)
                    4)]
         [i  (begin (bytes-copy! material-data i (pack-emitted e) 0 4)
                    (unsafe-fx+ i 4))]
         [i  (begin (bytes-set! material-data i (flonum->byte (material-ambient m)))
                    (unsafe-fx+ i 1))]
         [i  (begin (bytes-set! material-data i (flonum->byte (material-diffuse m)))
                    (unsafe-fx+ i 1))]
         [i  (begin (bytes-set! material-data i (flonum->byte (material-specular m)))
                    (unsafe-fx+ i 1))])
    (void))
  
  (define draw-data-size (* 24 (vao-struct-size (program-spec-struct (polygon-opaq-program-spec)))))
  (define draw-data (make-bytes draw-data-size))
  (define draw-ptr (u8vector->cpointer draw-data))
  (for/fold ([i : Nonnegative-Fixnum  0]) ([v  (in-vector vs)])
    (let* ([i  (begin (bytes-copy! draw-data i material-data 0 material-data-size)
                      (unsafe-fx+ i material-data-size))]
           [i  (begin (memcpy draw-ptr i (f32vector->cpointer v) 12 _byte)
                      (unsafe-fx+ i 12))])
      i))
  
  (define transparent? (< (flvector-ref c 3) 1.0))
  
  (: passes Passes)
  (define passes
    (if transparent?
        (vector
         #()
         #()
         #()
         (vector (shape-params polygon-mat-program-spec empty face GL_QUADS
                               (single-vertices 24 mat-data)))
         (vector (shape-params polygon-tran-program-spec empty face GL_QUADS
                               (single-vertices 24 draw-data))))
        (vector
         #()
         (vector (shape-params polygon-mat-program-spec empty face GL_QUADS
                               (single-vertices 24 mat-data)))
         (vector (shape-params polygon-opaq-program-spec empty face GL_QUADS
                               (single-vertices 24 draw-data)))
         #()
         #())))
  passes)

;; ===================================================================================================
;; Sphere shape passes

(: make-sphere-shape-passes (-> sphere-shape Passes))
(define (make-sphere-shape-passes a)
  (match-define (sphere-shape _ t c e m inside?) a)
  
  (define affine-ptr (f32vector->cpointer (affine-data t)))
  (define roughness (flonum->byte (material-roughness m)))
  (define inside (if inside? 255 0))
  
  (define mat-datum-size (+ affine-data-size 4))  ; last two bytes are unused
  (define mat-data-size (* 4 mat-datum-size))
  (define mat-data (make-bytes mat-data-size))
  (memcpy (u8vector->cpointer mat-data) affine-ptr affine-data-size)
  (bytes-set! mat-data affine-data-size roughness)
  (bytes-set! mat-data (unsafe-fx+ 1 affine-data-size) inside)
  (for ([j  (in-range 1 4)])
    (bytes-copy! mat-data (unsafe-fx* j mat-datum-size) mat-data 0 mat-datum-size))
  
  (define draw-datum-size (vao-struct-size (program-spec-struct (sphere-opaq-program-spec))))
  (define draw-data-size (* 4 draw-datum-size))
  (define draw-data (make-bytes draw-data-size))
  (let* ([i  0]
         [i  (begin (memcpy (u8vector->cpointer draw-data) i affine-ptr affine-data-size)
                    (unsafe-fx+ i affine-data-size))]
         [i  (begin (bytes-copy! draw-data i (pack-color c) 0 4)
                    (unsafe-fx+ i 4))]
         [i  (begin (bytes-copy! draw-data i (pack-emitted e) 0 4)
                    (unsafe-fx+ i 4))])
    (bytes-set! draw-data i (flonum->byte (material-ambient m)))
    (bytes-set! draw-data (unsafe-fx+ i 1) (flonum->byte (material-diffuse m)))
    (bytes-set! draw-data (unsafe-fx+ i 2) (flonum->byte (material-specular m)))
    (bytes-set! draw-data (unsafe-fx+ i 3) inside))
  (for ([j  (in-range 1 4)])
    (bytes-copy! draw-data (unsafe-fx* j draw-datum-size) draw-data 0 draw-datum-size))
  
  (define transparent? (< (flvector-ref c 3) 1.0))
  
  (: passes Passes)
  (define passes
    (if transparent?
        (vector
         #()
         #()
         #()
         (vector (shape-params sphere-mat-program-spec empty 'both GL_QUADS
                               (single-vertices 4 mat-data)))
         (vector (shape-params sphere-tran-program-spec empty 'both GL_QUADS
                               (single-vertices 4 draw-data))))
        (vector
         #()
         (vector (shape-params sphere-mat-program-spec empty 'both GL_QUADS
                               (single-vertices 4 mat-data)))
         (vector (shape-params sphere-opaq-program-spec empty 'both GL_QUADS
                               (single-vertices 4 draw-data)))
         #()
         #())))
  passes)

;; ===================================================================================================
;; Directional light shape passes

(: make-directional-light-shape-passes (-> directional-light-shape Passes))
(define (make-directional-light-shape-passes a)
  (match-define (directional-light-shape _ intensity direction) a)
  
  (define uniforms
    (list (cons "light_dir" (uniform-float direction 3))
          (cons "light_color" (uniform-float intensity 3))))
  
  (: passes Passes)
  (define passes
    (vector
     (vector (shape-params directional-light-program-spec uniforms 'both GL_TRIANGLE_STRIP
                           (multi-vertices 4 #"" (vector 0) (s32vector 4))))
     #()
     #()
     #()
     #()))
  passes)

;; ===================================================================================================
;; Point light shape passes

(: make-point-light-shape-passes (-> point-light-shape Passes))
(define (make-point-light-shape-passes a)
  (match-define (point-light-shape _ intensity position radius) a)
  
  (: datum GL-Data)
  (define datum
    (gl-data->bytes
     (list (flvector->f32vector position)
           (f32vector radius)
           (flvector->f32vector intensity))))
  
  (define data (gl-data->bytes ((inst make-list GL-Data) 4 datum)))
  
  (: passes Passes)
  (define passes
    (vector
     (vector (shape-params point-light-program-spec empty 'both GL_QUADS (single-vertices 4 data)))
     #()
     #()
     #()
     #()))
  passes)

;; ===================================================================================================
;; Frozen scene shape passes

(: merge-single-vertices (-> program-spec
                             (List-Hash String (U Symbol Uniform))
                             Face
                             Integer
                             (Vectorof shape-params)
                             Nonnegative-Fixnum
                             Nonnegative-Fixnum
                             (U (List shape-params) Null)))
(define (merge-single-vertices pd uniforms face mode ps start end)
  (define struct-size (vao-struct-size (program-spec-struct pd)))
  
  (define vertex-count
    (for/fold ([vertex-count : Nonnegative-Fixnum  0]) ([i  (in-range start end)])
      (define v (shape-params-vertices (unsafe-vector-ref ps i)))
      (if (single-vertices? v)
          (unsafe-fx+ vertex-count (vertices-vertex-count v))
          vertex-count)))
  
  (cond
    [(> vertex-count 0)
     ;; Allocate enough space for all the vertex data
     (define buffer-size (unsafe-fx* vertex-count struct-size))
     (define all-vertex-data (make-bytes buffer-size))
     (define all-vertex-data-ptr (u8vector->cpointer all-vertex-data))
     ;; Copy the vertex data into the buffer
     (for/fold ([vertex-num : Nonnegative-Fixnum  0]) ([i  (in-range start end)])
       (define v (shape-params-vertices (unsafe-vector-ref ps i)))
       (cond
         [(single-vertices? v)
          (define vertex-count (vertices-vertex-count v))
          (define vertex-data (vertices-vertex-data v))
          (memcpy all-vertex-data-ptr
                  (unsafe-fx* vertex-num struct-size)
                  (u8vector->cpointer vertex-data)
                  (unsafe-fx* vertex-count struct-size)
                  _byte)
          (unsafe-fx+ vertex-num vertex-count)]
         [else
          vertex-num]))
     
     (list (shape-params (λ () pd)
                         uniforms
                         face
                         mode
                         (single-vertices (assert vertex-count index?) all-vertex-data)))]
    [else  empty]))

(: merge-multi-vertices (-> program-spec
                            (List-Hash String (U Symbol Uniform))
                            Face
                            Integer
                            (Vectorof shape-params)
                            Nonnegative-Fixnum
                            Nonnegative-Fixnum
                            (U (List shape-params) Null)))
(define (merge-multi-vertices pd uniforms face mode ps start end)
  (define struct-size (vao-struct-size (program-spec-struct pd)))
  
  (define-values (vertex-count prim-count)
    (for/fold ([vertex-count : Nonnegative-Fixnum  0]
               [prim-count : Nonnegative-Fixnum  0]
               ) ([i  (in-range start end)])
      (define v (shape-params-vertices (unsafe-vector-ref ps i)))
      (if (multi-vertices? v)
          (values
           (unsafe-fx+ vertex-count (vertices-vertex-count v))
           (unsafe-fx+ prim-count (min (vector-length (multi-vertices-starts v))
                                       (s32vector-length (multi-vertices-counts v)))))
          (values vertex-count prim-count))))
  
  (cond
    [(and (> vertex-count 0) (> prim-count 0))
     ;; Allocate enough space for all the vertex data
     (define buffer-size (unsafe-fx* vertex-count struct-size))
     (define all-vertex-data (make-bytes buffer-size))
     (define all-vertex-data-ptr (u8vector->cpointer all-vertex-data))
     (define all-starts ((inst make-vector Nonnegative-Fixnum) prim-count 0))
     (define all-counts (make-s32vector prim-count))
     (define all-counts-ptr (s32vector->cpointer all-counts))
     ;; Copy the vertex data into the buffer
     (for/fold ([vertex-num : Nonnegative-Fixnum  0]
                [prim-num : Nonnegative-Fixnum  0]
                ) ([i  (in-range start end)])
       (define v (shape-params-vertices (unsafe-vector-ref ps i)))
       (cond
         [(multi-vertices? v)
          (define vertex-count (vertices-vertex-count v))
          (define vertex-data (vertices-vertex-data v))
          (define starts (multi-vertices-starts v))
          (define counts (multi-vertices-counts v))
          (define prim-count (min (vector-length starts) (s32vector-length counts)))
          ;; Copy the vertex data directly
          (memcpy all-vertex-data-ptr
                  (unsafe-fx* vertex-num struct-size)
                  (u8vector->cpointer vertex-data)
                  (unsafe-fx* vertex-count struct-size)
                  _byte)
          ;; Copy the starts, shifted by the number of vertices already copied
          (for ([j  (in-range prim-count)])
            (define start (unsafe-vector-ref starts j))
            (define shifted-j (unsafe-fx+ prim-num j))
            (define shifted-start (unsafe-fx+ vertex-num start))
            (unsafe-vector-set! all-starts shifted-j shifted-start))
          ;; Copy the counts directly
          (memcpy all-counts-ptr
                  (unsafe-fx* prim-num 4)
                  (s32vector->cpointer counts)
                  (unsafe-fx* prim-count 4)
                  _byte)
          ;; Increment the counters
          (values (unsafe-fx+ vertex-num vertex-count)
                  (unsafe-fx+ prim-num prim-count))]
         [else
          (values vertex-num prim-num)]))
     
     (list (shape-params (λ () pd)
                         uniforms
                         face
                         mode
                         (multi-vertices (assert vertex-count index?)
                                         all-vertex-data
                                         all-starts
                                         all-counts)))]
    [else  empty]))

(: make-frozen-scene-shape-passes (-> frozen-scene-shape Passes))
(define (make-frozen-scene-shape-passes a)
  (define s (frozen-scene-shape-scene a))
  
  (: transformed-passes (-> Shape FlAffine3- (Listof Passes)))
  (define (transformed-passes s t)
    (map shape-passes (shape-transform s t (flt3inverse t))))
  
  (define ps (append* (flscene3-extract (flscene3-transform-shapes s identity-flt3 identity-flt3)
                                        identity-flt3
                                        flt3compose
                                        transformed-passes)))
  (define num-passes (apply max (map vector-length ps)))
  
  (: get-swap-params (-> Integer (Vectorof shape-params)))
  (define (get-swap-params n)
    (make-vector n empty-shape-params))
  
  (list->vector
   (for/list : (Listof (Vectorof shape-params)) ([pass  (in-range num-passes)])
     (let* ([ps  (map (λ ([p : Passes])
                        (if (< pass (vector-length p)) (vector-ref p pass) #()))
                      ps)]
            [ps  (apply vector-append ps)])
       (list->vector
        (append*
         (for*/list : (Listof (Listof shape-params))
           ([s  (in-list (group-by-key! ps get-swap-params 0 (vector-length ps)
                                        shape-params-program-spec))]
            [pd  (in-value ((span-key s)))]
            [s  (in-list (group-by-key! ps get-swap-params (span-start s) (span-end s)
                                        shape-params-uniforms))]
            [uniforms  (in-value (span-key s))]
            [s  (in-list ((inst group-by-key! shape-params Face)
                          ps get-swap-params (span-start s) (span-end s)
                          shape-params-face))]
            [face  (in-value (span-key s))]
            [s  (in-list (group-by-key! ps get-swap-params (span-start s) (span-end s)
                                        shape-params-mode))]
            [mode  (in-value (span-key s))])
           (append
            (merge-single-vertices pd uniforms face mode ps (span-start s) (span-end s))
            (merge-multi-vertices  pd uniforms face mode ps (span-start s) (span-end s))))))))))

;; ===================================================================================================
;; Pass-construction dispatch

(: shape-passes (-> Shape Passes))
(define (shape-passes a)
  (lazy-box-ref!
   (shape-lazy-passes a)
   (λ ()
     (cond [(solid-shape? a)
            (cond [(triangle-shape? a)   (make-triangle-shape-passes a)]
                  [(quad-shape? a)       (make-quad-shape-passes a)]
                  [(rectangle-shape? a)  (make-rectangle-shape-passes a)]
                  [(sphere-shape? a)     (make-sphere-shape-passes a)])]
           [(light-shape? a)
            (cond [(directional-light-shape? a)  (make-directional-light-shape-passes a)]
                  [(point-light-shape? a)        (make-point-light-shape-passes a)])]
           [(frozen-scene-shape? a)
            (make-frozen-scene-shape-passes a)]))))

(: flscene3-draw-passes (-> Scene (Listof draw-passes)))
(define (flscene3-draw-passes s)
  ((inst flscene3-extract Shape draw-passes affine)
   s
   identity-affine
   affine-compose
   (λ ([a : Shape] [m : affine])
     (draw-passes (shape-passes a) m))))
