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
  ([transform : FlAffine3-]
   [color : FlVector]
   [emitted-color : FlVector]
   [material : material]
   [inside? : Boolean])
  #:transparent)

(struct light-shape shape ([intensity : FlVector]) #:transparent)
(struct directional-light-shape light-shape ([direction : FlVector]) #:transparent)
(struct point-light-shape light-shape ([position : FlVector] [radius : Flonum]) #:transparent)

(struct frozen-scene-shape shape
  ([transform : FlAffine3-]
   [inverse : FlAffine3-]
   [scene : Nonempty-Scene])
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

(: make-sphere-shape (-> FlAffine3- FlVector FlVector material Boolean sphere-shape))
(define (make-sphere-shape t c e m inside?)
  (cond [(not (= 4 (flvector-length c)))
         (raise-argument-error 'make-rectangle-shape "length-4 flvector" 1 t c e m inside?)]
        [(not (= 3 (flvector-length e)))
         (raise-argument-error 'make-rectangle-shape "length-3 flvector" 2 t c e m inside?)]
        [else
         (sphere-shape (box 'lazy) t c e m inside?)]))

(: make-directional-light-shape (-> FlVector FlVector directional-light-shape))
(define (make-directional-light-shape intensity direction)
  (directional-light-shape (box 'lazy) intensity direction))

(: make-point-light-shape (-> FlVector FlVector Flonum point-light-shape))
(define (make-point-light-shape intensity position radius)
  (point-light-shape (box 'lazy) intensity position radius))

(: make-frozen-scene-shape (-> Nonempty-Scene Shape))
(define (make-frozen-scene-shape s)
  (frozen-scene-shape (box 'lazy) identity-flt3 identity-flt3 s))

;; ===================================================================================================
;; Shape bounding box

(: shape-aabb (-> Shape FlAABB3))
(define (shape-aabb s)
  (cond
    [(solid-shape? s)
     (cond [(triangle-shape? s)  (fltriangle3-aabb (triangle-shape-fltriangle3 s))]
           [(quad-shape? s)  (assert (flv3aabb (quad-shape-vertices s)) values)]
           [(rectangle-shape? s)  (rectangle-shape-aabb s)]
           [(sphere-shape? s)
            (flaabb3-transform (assert (make-flaabb3 (flvector -1.0 -1.0 -1.0)
                                                     (flvector +1.0 +1.0 +1.0))
                                       values)
                               (sphere-shape-transform s))])]
    [(light-shape? s)
     (match s
       [(? directional-light-shape?)
        (assert (make-flaabb3 (flvector -inf.0 -inf.0 -inf.0)
                              (flvector +inf.0 +inf.0 +inf.0))
                values)]
       [(? point-light-shape? s)
        (define p (point-light-shape-position s))
        (define radius (point-light-shape-radius s))
        (define r (flvector radius radius radius))
        (assert (make-flaabb3 (flv3- p r) (flv3+ p r)) values)])]
    [(frozen-scene-shape? s)
     (flaabb3-transform
      (flscene3-aabb (frozen-scene-shape-scene s))
      (frozen-scene-shape-transform s))]))

;; ===================================================================================================
;; Shape transformation

(: shape-transform (->* [Shape FlAffine3- FlAffine3-] [Boolean] (Listof Shape)))
(define (shape-transform a t tinv [transform-frozen? #f])
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
        (match a
          ;; Triangle: transform vertices
          [(triangle-shape passes tri ns cs es ms face)
           (define vs (fltriangle3-vertices tri))
           (define new-vs (vector-map transform-pos vs))
           (define new-ns (if (vector? ns) (vector-map transform-norm ns) (transform-norm ns)))
           (list (triangle-shape (box 'lazy) (fltriangle3 new-vs) new-ns cs es ms
                                 (if (flt3consistent? t) face (opposite-gl-face face))))]
          ;; Quad: transform vertices
          [(quad-shape passes vs ns cs es ms face)
           (define new-vs (vector-map transform-pos vs))
           (define new-ns (if (vector? ns) (vector-map transform-norm ns) (transform-norm ns)))
           (list (quad-shape (box 'lazy) new-vs new-ns cs es ms
                             (if (flt3consistent? t) face (opposite-gl-face face))))]
          ;; Rectangle: split into quads
          [(rectangle-shape passes b c e m old-face)
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
          ;; Sphere: apply transform
          [(sphere-shape passes t0 c e m inside?)
           (list (sphere-shape (box 'lazy) (flt3compose t t0) c e m inside?))])]
       [(light-shape? a)
        (match a
          ;; Directional light: transform just direction
          [(directional-light-shape passes intensity direction)
           (list (directional-light-shape (box 'lazy) intensity (transform-norm direction)))]
          ;; Point light: transform just center
          [(point-light-shape passes intensity position radius)
           (list (point-light-shape (box 'lazy) intensity (transform-pos position) radius))])]
       ;; Frozen scene
       [(frozen-scene-shape? a)
        (match-define (frozen-scene-shape passes t0 tinv0 s) a)
        (cond
          [transform-frozen?
           ;; Transform the frozen scene piece at a time; don't re-freeze
           (append*
            (flscene3-extract
             (flscene3-transform s t0 tinv0)
             (λ ([a : Shape] [t0 : FlAffine3-] [tinv0 : FlAffine3-])
               (shape-transform a (flt3compose t t0) (flt3compose tinv0 tinv) #t))))]
          [else
           ;; Just compose the transformation
           (list (frozen-scene-shape passes (flt3compose t t0) (flt3compose tinv0 tinv) s))])])]))

;; ===================================================================================================
;; Shape-specific scene functions

(define-values (shape->flscene3 flscene3-transform)
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
;; Material program

(define mat-vertex-code
  (string-append
   #<<code
#version 130

uniform mat4 model;
uniform mat4 unmodel;
uniform mat4 view;
uniform mat4 unview;
uniform mat4 proj;

in vec4 vert_normal_roughness;
in vec3 vert_position;

smooth out vec4 frag_position;
smooth out vec3 frag_normal;
smooth out float frag_roughness;

void main() {
  vec4 position = view * (model * vec4(vert_position,1));
  gl_Position = proj * position;
  vec3 normal = normalize(vert_normal_roughness.xyz - vec3(127.0/255.0));
  vec4 norm = (vec4(normal,0) * unmodel) * unview;
  frag_position = position;
  frag_normal = normalize(norm.xyz);
  frag_roughness = vert_normal_roughness.w;
}
code
   ))

(define mat-fragment-code
  (string-append
   "#version 130\n\n"
   pack-unpack-normal-code
   depth-fragment-code
   #<<code
uniform float znear;
uniform float zfar;

smooth in vec4 frag_position;
smooth in vec3 frag_normal;
smooth in float frag_roughness;

void main() {
  gl_FragDepth = frag_depth(znear, zfar, frag_position.z);
  gl_FragColor.rg = pack_normal(normalize(frag_normal));
  gl_FragColor.b = 1.0;  // reserved
  gl_FragColor.a = frag_roughness;
}
code
   ))

(define-singleton (mat-program-spec)
  (define program
    (make-gl-program (list (make-gl-shader GL_VERTEX_SHADER mat-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER mat-fragment-code))))
  
  (define uniforms
    (list (cons "model" 'model)
          (cons "unmodel" 'unmodel)
          (cons "view" 'view)
          (cons "unview" 'unview)
          (cons "proj" 'proj)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)))
  
  (define handle (gl-object-handle program))
  (define vert_normal_roughness (cast (glGetAttribLocation handle "vert_normal_roughness") Natural))
  (define vert_position (cast (glGetAttribLocation handle "vert_position") Natural))
  
  (define struct
    (make-vao-struct
     (make-vao-field vert_normal_roughness 4 GL_UNSIGNED_BYTE)
     (make-vao-field vert_position 3 GL_FLOAT)))
  
  (program-spec program uniforms struct))

;; ===================================================================================================
;; Sphere programs

;; ---------------------------------------------------------------------------------------------------
;; Program 1: material

(define matrix-code
  (string-append
   #<<code
mat4 affine_to_projective(mat4x3 m) {
  return mat4(vec4(m[0],0), vec4(m[1],0), vec4(m[2],0), vec4(m[3],1));
}

mat3 linear_inverse(mat3 m) {
  float det = dot(m[0], cross(m[1], m[2]));
  return transpose(mat3(cross(m[1],m[2]),
                        cross(m[2],m[0]),
                        cross(m[0],m[1]))) / det;
}

mat4x3 affine_inverse(mat4x3 m) {
  mat3 n = linear_inverse(mat3(m[0],m[1],m[2]));
  return mat4x3(n[0], n[1], n[2], -(n*m[3]));
}
code
   "\n\n"))

(define sphere-mat-vertex-code
  (string-append
   "#version 130\n\n"
   output-impostor-quad-vertex-code
   ;matrix-code
   #<<code
uniform mat4 model;
uniform mat4 view;
uniform mat4 unmodel;
uniform mat4 unview;
uniform mat4 proj;

in vec4 trans0;
in vec4 trans1;
in vec4 trans2;
in vec4 untrans0;
in vec4 untrans1;
in vec4 untrans2;
in vec4 vert_roughness_inside;

flat out mat4 sphere_to_view;
flat out mat4 view_to_sphere;
flat out float frag_roughness;
flat out float frag_inside;

smooth out float is_degenerate;

void main() {
  sphere_to_view = view * model * transpose(mat4(trans0,trans1,trans2,vec4(0,0,0,1)));
  view_to_sphere = transpose(mat4(untrans0,untrans1,untrans2,vec4(0,0,0,1))) * unmodel * unview;
  frag_roughness = vert_roughness_inside.x;
  frag_inside = vert_roughness_inside.y;
  is_degenerate = output_impostor_quad(sphere_to_view, proj, vec3(-1.0), vec3(+1.0));
}
code
   ))

(define sphere-mat-fragment-code
  (string-append
   "#version 130\n\n"
   pack-unpack-normal-code
   depth-fragment-code
   ray-trace-fragment-code
   #<<code
uniform mat4 proj;
uniform mat4 unproj;
uniform float znear;
uniform float zfar;
uniform int width;
uniform int height;

flat in mat4 sphere_to_view;
flat in mat4 view_to_sphere;
flat in float frag_roughness;
flat in float frag_inside;

smooth in float is_degenerate;

void main() {
  // all fragments should discard if this one does
  if (is_degenerate > 0.0) discard;

  vec3 vdir = frag_coord_to_direction(gl_FragCoord, unproj, width, height);
  vec3 start = (view_to_sphere * vec4(0,0,0,1)).xyz;
  vec3 end = (view_to_sphere * vec4(vdir,1)).xyz;
  vec3 dir = normalize(end - start);
  vec2 ts = unit_sphere_intersect(start, dir);
  float t = (frag_inside == 0.0) ? ts.x : ts.y;
  // many nearby fragments should discard if this one does
  if (t < 0.0) discard;
  
  vec3 pos = start + dir * t;
  vec3 vpos = (sphere_to_view * vec4(pos, 1.0)).xyz;
  vec3 vnorm = normalize((vec4(pos, 0.0) * view_to_sphere).xyz);
  
  gl_FragDepth = frag_depth(znear, zfar, vpos.z);
  gl_FragColor.rg = pack_normal((frag_inside == 0.0) ? vnorm : -vnorm);
  gl_FragColor.b = 1.0;  // reserved
  gl_FragColor.a = frag_roughness;
}
code
   ))

(define-singleton (sphere-mat-program-spec)
  (define program
    (make-gl-program (list (make-gl-shader GL_VERTEX_SHADER sphere-mat-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER sphere-mat-fragment-code))))
  
  (define uniforms
    (list (cons "model" 'model)
          (cons "unmodel" 'unmodel)
          (cons "view" 'view)
          (cons "unview" 'unview)
          (cons "proj" 'proj)
          (cons "unproj" 'unproj)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)
          (cons "width" 'width)
          (cons "height" 'height)))
  
  (define handle (gl-object-handle program))
  (define trans0 (cast (glGetAttribLocation handle "trans0") Natural))
  (define trans1 (cast (glGetAttribLocation handle "trans1") Natural))
  (define trans2 (cast (glGetAttribLocation handle "trans2") Natural))
  (define untrans0 (cast (glGetAttribLocation handle "untrans0") Natural))
  (define untrans1 (cast (glGetAttribLocation handle "untrans1") Natural))
  (define untrans2 (cast (glGetAttribLocation handle "untrans2") Natural))
  (define vert_roughness_inside (cast (glGetAttribLocation handle "vert_roughness_inside") Natural))
  
  (define struct
    (make-vao-struct
     (make-vao-field trans0 4 GL_FLOAT)
     (make-vao-field trans1 4 GL_FLOAT)
     (make-vao-field trans2 4 GL_FLOAT)
     (make-vao-field untrans0 4 GL_FLOAT)
     (make-vao-field untrans1 4 GL_FLOAT)
     (make-vao-field untrans2 4 GL_FLOAT)
     (make-vao-field vert_roughness_inside 4 GL_UNSIGNED_BYTE)))
  
  (program-spec program uniforms struct))

;; ---------------------------------------------------------------------------------------------------
;; Program 2: color

(define sphere-vertex-code
  (string-append
   "#version 130\n\n"
   output-impostor-quad-vertex-code
   rgb-hsv-code
   matrix-code
   #<<code
uniform mat4 model;
uniform mat4 view;
uniform mat4 unmodel;
uniform mat4 unview;
uniform mat4 proj;

in vec4 trans0;
in vec4 trans1;
in vec4 trans2;
in vec4 untrans0;
in vec4 untrans1;
in vec4 untrans2;
in vec4 vert_rcolor;
in vec4 vert_ecolor;           // vec4(hue, saturation, value.hi, value.lo)
in vec4 vert_material_inside;  // vec4(ambient, diffuse, specular, inside?)

flat out mat4 sphere_to_view;
flat out mat4 view_to_sphere;
flat out vec4 frag_rcolor;
flat out vec3 frag_ecolor;
flat out float frag_ambient;
flat out float frag_diffuse;
flat out float frag_specular;
flat out float frag_inside;

smooth out float is_degenerate;

void main() {
  sphere_to_view = view * model * transpose(mat4(trans0,trans1,trans2,vec4(0,0,0,1)));
  view_to_sphere = transpose(mat4(untrans0,untrans1,untrans2,vec4(0,0,0,1))) * unmodel * unview;
  frag_rcolor = vert_rcolor;
  frag_ecolor = hsv_to_rgb(vec3(vert_ecolor.xy, vert_ecolor.w + 256.0 * vert_ecolor.z));
  frag_ambient = vert_material_inside.x;
  frag_diffuse = vert_material_inside.y;
  frag_specular = vert_material_inside.z;
  frag_inside = vert_material_inside.w;
  is_degenerate = output_impostor_quad(sphere_to_view, proj, vec3(-1.0), vec3(+1.0));
}
code
   ))

(define sphere-opaq-fragment-code
  (string-append
   "#version 130\n\n"
   pack-unpack-normal-code
   depth-fragment-code
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

flat in mat4 sphere_to_view;
flat in mat4 view_to_sphere;
flat in vec4 frag_rcolor;
flat in vec3 frag_ecolor;
flat in float frag_ambient;
flat in float frag_diffuse;
flat in float frag_specular;
flat in float frag_inside;

smooth in float is_degenerate;

void main() {
  // all fragments should discard if this one does
  if (is_degenerate > 0.0) discard;

  vec3 vdir = frag_coord_to_direction(gl_FragCoord, unproj, width, height);
  vec3 start = (view_to_sphere * vec4(0,0,0,1)).xyz;
  vec3 end = (view_to_sphere * vec4(vdir,1)).xyz;
  vec3 dir = normalize(end - start);
  vec2 ts = unit_sphere_intersect(start, dir);
  float t = (frag_inside == 0.0) ? ts.x : ts.y;
  // many nearby fragments should discard if this one does
  if (t < 0.0) discard;
  
  vec3 pos = start + dir * t;
  vec3 vpos = (sphere_to_view * vec4(pos, 1.0)).xyz;
  
  gl_FragDepth = frag_depth(znear, zfar, vpos.z);
  vec3 diff = texelFetch(diffuse, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 spec = texelFetch(specular, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
  gl_FragColor.rgb = frag_ecolor + frag_rcolor.rgb * light;
  gl_FragColor.a = frag_rcolor.a;
}
code
   ))

(define-singleton (sphere-opaq-program-spec)
  (define program
    (make-gl-program (list (make-gl-shader GL_VERTEX_SHADER sphere-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER sphere-opaq-fragment-code))))
  
  (define uniforms
    (list (cons "model" 'model)
          (cons "view" 'view)
          (cons "proj" 'proj)
          (cons "unmodel" 'unmodel)
          (cons "unview" 'unview)
          (cons "unproj" 'unproj)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)
          (cons "width" 'width)
          (cons "height" 'height)
          (cons "ambient" 'ambient)
          (cons "diffuse" 'diffuse)
          (cons "specular" 'specular)))
  
  (define handle (gl-object-handle program))
  (define trans0 (cast (glGetAttribLocation handle "trans0") Natural))
  (define trans1 (cast (glGetAttribLocation handle "trans1") Natural))
  (define trans2 (cast (glGetAttribLocation handle "trans2") Natural))
  (define untrans0 (cast (glGetAttribLocation handle "untrans0") Natural))
  (define untrans1 (cast (glGetAttribLocation handle "untrans1") Natural))
  (define untrans2 (cast (glGetAttribLocation handle "untrans2") Natural))
  (define vert_rcolor (cast (glGetAttribLocation handle "vert_rcolor") Natural))
  (define vert_ecolor (cast (glGetAttribLocation handle "vert_ecolor") Natural))
  (define vert_material_inside (cast (glGetAttribLocation handle "vert_material_inside") Natural))
  
  (define struct
    (make-vao-struct
     (make-vao-field trans0 4 GL_FLOAT)
     (make-vao-field trans1 4 GL_FLOAT)
     (make-vao-field trans2 4 GL_FLOAT)
     (make-vao-field untrans0 4 GL_FLOAT)
     (make-vao-field untrans1 4 GL_FLOAT)
     (make-vao-field untrans2 4 GL_FLOAT)
     (make-vao-field vert_rcolor 4 GL_UNSIGNED_BYTE)
     (make-vao-field vert_ecolor 4 GL_UNSIGNED_BYTE)
     (make-vao-field vert_material_inside 4 GL_UNSIGNED_BYTE)))
  
  (program-spec program uniforms struct))

(define sphere-tran-fragment-code
  (string-append
   "#version 130\n\n"
   pack-unpack-normal-code
   depth-fragment-code
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

flat in mat4 sphere_to_view;
flat in mat4 view_to_sphere;
flat in vec4 frag_rcolor;
flat in vec3 frag_ecolor;
flat in float frag_ambient;
flat in float frag_diffuse;
flat in float frag_specular;
flat in float frag_inside;

smooth in float is_degenerate;

void main() {
  // all fragments should discard if this one does
  if (is_degenerate > 0.0) discard;

  vec3 vdir = frag_coord_to_direction(gl_FragCoord, unproj, width, height);
  vec3 start = (view_to_sphere * vec4(0,0,0,1)).xyz;
  vec3 end = (view_to_sphere * vec4(vdir,1)).xyz;
  vec3 dir = normalize(end - start);
  vec2 ts = unit_sphere_intersect(start, dir);
  float t = (frag_inside == 0.0) ? ts.x : ts.y;
  // many nearby fragments should discard if this one does
  if (t < 0.0) discard;
  
  vec3 pos = start + dir * t;
  vec3 vpos = (sphere_to_view * vec4(pos, 1.0)).xyz;
  
  vec3 diff = texelFetch(diffuse, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 spec = texelFetch(specular, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
  vec3 light_color = frag_ecolor + frag_rcolor.rgb * light;
  
  float depth = frag_depth(znear, zfar, vpos.z);
  float a = frag_rcolor.a;
  float d = 1 - depth;
  float weight = a * clamp(1 / (d*d*d) - 1, 0.001953125, 32768.0);
  gl_FragDepth = depth;
  gl_FragData[0] = vec4(light_color * weight * a, a);
  gl_FragData[1] = vec4(a * weight);
}
code
   ))

(define-singleton (sphere-tran-program-spec)
  (define program
    (make-gl-program (list (make-gl-shader GL_VERTEX_SHADER sphere-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER sphere-tran-fragment-code))))
  
  (define uniforms
    (list (cons "model" 'model)
          (cons "view" 'view)
          (cons "proj" 'proj)
          (cons "unmodel" 'unmodel)
          (cons "unview" 'unview)
          (cons "unproj" 'unproj)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)
          (cons "width" 'width)
          (cons "height" 'height)
          (cons "ambient" 'ambient)
          (cons "diffuse" 'diffuse)
          (cons "specular" 'specular)))
  
  (define handle (gl-object-handle program))
  (define trans0 (cast (glGetAttribLocation handle "trans0") Natural))
  (define trans1 (cast (glGetAttribLocation handle "trans1") Natural))
  (define trans2 (cast (glGetAttribLocation handle "trans2") Natural))
  (define untrans0 (cast (glGetAttribLocation handle "untrans0") Natural))
  (define untrans1 (cast (glGetAttribLocation handle "untrans1") Natural))
  (define untrans2 (cast (glGetAttribLocation handle "untrans2") Natural))
  (define vert_rcolor (cast (glGetAttribLocation handle "vert_rcolor") Natural))
  (define vert_ecolor (cast (glGetAttribLocation handle "vert_ecolor") Natural))
  (define vert_material_inside (cast (glGetAttribLocation handle "vert_material_inside") Natural))
  
  (define struct
    (make-vao-struct
     (make-vao-field trans0 4 GL_FLOAT)
     (make-vao-field trans1 4 GL_FLOAT)
     (make-vao-field trans2 4 GL_FLOAT)
     (make-vao-field untrans0 4 GL_FLOAT)
     (make-vao-field untrans1 4 GL_FLOAT)
     (make-vao-field untrans2 4 GL_FLOAT)
     (make-vao-field vert_rcolor 4 GL_UNSIGNED_BYTE)
     (make-vao-field vert_ecolor 4 GL_UNSIGNED_BYTE)
     (make-vao-field vert_material_inside 4 GL_UNSIGNED_BYTE)))
  
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
   get-surface-fragment-code
   light-code
   #<<code
uniform mat4 unmodel;
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
  vec4 pos = get_view_position(depth, width, height, unproj0, unproj1, znear, zfar);
  surface s = get_surface(material);
  vec3 N = s.normal;
  vec3 L = normalize(-light_dir * mat3(unmodel) * mat3(unview));
  float dotNL = dot(N,L);
  if (dotNL <= 0.0) discard;

  float m = s.roughness;
  vec3 V = normalize(-pos.xyz);
  
  gl_FragData[0] = vec4(light_color * dotNL, 0.0);
  gl_FragData[1] = vec4(light_color * specular(N,L,dotNL,V,m), 0.0);
}
code
   ))

(define-singleton (directional-light-program-spec)
  (define program
    (make-gl-program (list (make-gl-shader GL_VERTEX_SHADER directional-light-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER directional-light-fragment-code))))
  
  (define uniforms
    (list (cons "unmodel" 'unmodel)
          (cons "unview" 'unview)
          (cons "unproj0" 'unproj0)
          (cons "unproj1" 'unproj1)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)
          (cons "width" 'width)
          (cons "height" 'height)
          (cons "depth" 'depth)
          (cons "material" 'material)))
  
  (program-spec program uniforms (make-vao-struct)))

;; ===================================================================================================
;; Point light program

(define point-light-vertex-code
  (string-append
   "#version 130\n\n"
   output-impostor-quad-vertex-code
   #<<code
uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

in vec4 vert_position_radius;
in vec3 vert_intensity;

flat out vec3 frag_position;
flat out float frag_radius;
flat out vec3 frag_intensity;

smooth out float is_degenerate;

void main() {
  mat4 vm = view * model;
  vec3 position = vert_position_radius.xyz;
  float radius = vert_position_radius.w;
  vec3 wmin = position - vec3(radius);
  vec3 wmax = position + vec3(radius);
  frag_position = (vm * vec4(position.xyz,1)).xyz;
  frag_radius = radius;
  frag_intensity = vert_intensity;
  is_degenerate = output_impostor_quad(vm, proj, wmin, wmax);
}
code
   ))

(define point-light-fragment-code
  (string-append
   "#version 130\n\n"
   get-view-position-fragment-code
   get-surface-fragment-code
   light-code
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

smooth in float is_degenerate;

void main() {
  // all fragments should discard if this one does
  if (is_degenerate > 0.0) discard;

  vec3 vpos = get_view_position(depth, width, height, unproj0, unproj1, znear, zfar).xyz;
  
  // D = vector from fragment to light source
  vec3 D = frag_position - vpos;
  float dist = length(D);
  
  if (dist > frag_radius) discard;
  
  // get surface normal, direction to light, roughness, direction to viewer
  surface s = get_surface(material);
  vec3 N = s.normal;
  vec3 L = normalize(D);
  float dotNL = dot(N,L);
  if (dotNL <= 0.0) discard;

  float m = s.roughness;
  vec3 V = normalize(-vpos);

  vec3 light = attenuate_invsqr(frag_intensity, dist);
  //vec3 light = attenuate_linear(frag_intensity, frag_radius, dist);  
  gl_FragData[0] = vec4(light * dotNL, 0.0);
  gl_FragData[1] = vec4(light * specular(N,L,dotNL,V,m), 0.0);
}
code
   ))

(define-singleton (point-light-program-spec)
  (define program
    (make-gl-program (list (make-gl-shader GL_VERTEX_SHADER point-light-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER point-light-fragment-code))))
  
  (define uniforms
    (list (cons "model" 'model)
          (cons "view" 'view)
          (cons "proj" 'proj)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)
          (cons "width" 'width)
          (cons "height" 'height)
          (cons "unproj0" 'unproj0)
          (cons "unproj1" 'unproj1)
          (cons "depth" 'depth)
          (cons "material" 'material)))
  
  (define handle (gl-object-handle program))
  (define struct
    (make-vao-struct
     (make-vao-field (cast (glGetAttribLocation handle "vert_position_radius") Natural) 4 GL_FLOAT)
     (make-vao-field (cast (glGetAttribLocation handle "vert_intensity") Natural) 3 GL_FLOAT)))
  
  (program-spec program uniforms struct))

;; ===================================================================================================
;; Solid polygon programs

(define shape-vertex-code
  (string-append
   "#version 130\n\n"
   rgb-hsv-code
   #<<code
uniform mat4 model;
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
  vec4 position = view * (model * vec4(vert_position,1));
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

(define opaq-fragment-code
  (string-append
   "#version 130\n\n"
   depth-fragment-code
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
  gl_FragDepth = frag_depth(znear, zfar, frag_position.z);
  vec3 diff = texelFetch(diffuse, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 spec = texelFetch(specular, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
  gl_FragColor.rgb = frag_ecolor + frag_rcolor.rgb * light;
  gl_FragColor.a = frag_rcolor.a;
}
code
   ))

(define-singleton (opaq-program-spec)
  (define program
    (make-gl-program (list (make-gl-shader GL_VERTEX_SHADER shape-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER opaq-fragment-code))))
  
  (define uniforms
    (list (cons "model" 'model)
          (cons "view" 'view)
          (cons "proj" 'proj)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)
          (cons "ambient" 'ambient)
          (cons "diffuse" 'diffuse)
          (cons "specular" 'specular)))
  
  (define handle (gl-object-handle program))
  (define struct
    (make-vao-struct
     (make-vao-field (cast (glGetAttribLocation handle "vert_rcolor") Natural) 4 GL_UNSIGNED_BYTE)
     (make-vao-field (cast (glGetAttribLocation handle "vert_ecolor") Natural) 4 GL_UNSIGNED_BYTE)
     (make-vao-field (cast (glGetAttribLocation handle "vert_material") Natural) 4 GL_UNSIGNED_BYTE)
     (make-vao-field (cast (glGetAttribLocation handle "vert_position") Natural) 3 GL_FLOAT)))
  
  (program-spec program uniforms struct))

(define tran-fragment-code
  (string-append
   "#version 130\n\n"
   depth-fragment-code
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
  vec3 light_color = frag_ecolor + frag_rcolor.rgb * light;
  
  float depth = frag_depth(znear, zfar, frag_position.z);
  float a = frag_rcolor.a;
  float d = 1 - depth;
  float weight = a * clamp(1 / (d*d*d) - 1, 0.001953125, 32768.0);
  gl_FragDepth = depth;
  gl_FragData[0] = vec4(light_color * weight * a, a);
  gl_FragData[1] = vec4(a * weight);
}
code
   ))

(define-singleton (tran-program-spec)
  (define program
    (make-gl-program (list (make-gl-shader GL_VERTEX_SHADER shape-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER tran-fragment-code))))
  
  (define uniforms
    (list (cons "model" 'model)
          (cons "view" 'view)
          (cons "proj" 'proj)
          (cons "znear" 'znear)
          (cons "zfar" 'zfar)
          (cons "diffuse" 'diffuse)
          (cons "specular" 'specular)))
  
  (define handle (gl-object-handle program))
  (define struct
    (make-vao-struct
     (make-vao-field (cast (glGetAttribLocation handle "vert_rcolor") Natural) 4 GL_UNSIGNED_BYTE)
     (make-vao-field (cast (glGetAttribLocation handle "vert_ecolor") Natural) 4 GL_UNSIGNED_BYTE)
     (make-vao-field (cast (glGetAttribLocation handle "vert_material") Natural) 4 GL_UNSIGNED_BYTE)
     (make-vao-field (cast (glGetAttribLocation handle "vert_position") Natural) 3 GL_FLOAT)))
  
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
  (define mat-data-size (* len (vao-struct-size (program-spec-struct (mat-program-spec)))))
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
  
  (define draw-data-size (* len (vao-struct-size (program-spec-struct (opaq-program-spec)))))
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
         (vector (shape-params mat-program-spec empty face mode (single-vertices len mat-data)))
         (vector (shape-params tran-program-spec empty face mode (single-vertices len draw-data))))
        (vector
         #()
         (vector (shape-params mat-program-spec empty face mode (single-vertices len mat-data)))
         (vector (shape-params opaq-program-spec empty face mode (single-vertices len draw-data)))
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
  
  (define mat-data-size (* 24 (vao-struct-size (program-spec-struct (mat-program-spec)))))
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
    (assert (- (vao-struct-size (program-spec-struct (opaq-program-spec))) 12) positive?))
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
  
  (define draw-data-size (* 24 (vao-struct-size (program-spec-struct (opaq-program-spec)))))
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
         (vector (shape-params mat-program-spec empty face GL_QUADS (single-vertices 24 mat-data)))
         (vector (shape-params tran-program-spec empty face GL_QUADS (single-vertices 24 draw-data))))
        (vector
         #()
         (vector (shape-params mat-program-spec empty face GL_QUADS (single-vertices 24 mat-data)))
         (vector (shape-params opaq-program-spec empty face GL_QUADS (single-vertices 24 draw-data)))
         #()
         #())))
  passes)

;; ===================================================================================================
;; Sphere shape passes

(: make-sphere-shape-passes (-> sphere-shape Passes))
(define (make-sphere-shape-passes a)
  (match-define (sphere-shape _ t0 c e m inside?) a)
  
  (define inside (if inside? 255 0))
  
  (define t (->flaffine3 t0))
  (define tinv (flt3inverse t))
  (define t-ptr (f32vector->cpointer (flvector->f32vector (flaffine3-entries t))))
  (define tinv-ptr (f32vector->cpointer (flvector->f32vector (flaffine3-entries tinv))))
  
  (define trans-datum-size (* 24 4))
  (define trans-datum (make-bytes trans-datum-size))
  (define trans-ptr (u8vector->cpointer trans-datum))
  (memcpy trans-ptr 0 t-ptr (* 12 4) _byte)
  (memcpy trans-ptr (* 12 4) tinv-ptr (* 12 4) _byte)
  
  (define mat-datum-size (+ trans-datum-size 4))  ; last two bytes are unused
  (define mat-data-size (* 4 mat-datum-size))
  (define mat-data (make-bytes mat-data-size))
  (bytes-copy! mat-data 0 trans-datum)
  (bytes-set! mat-data trans-datum-size (flonum->byte (material-roughness m)))
  (bytes-set! mat-data (unsafe-fx+ trans-datum-size 1) inside)
  (for ([j  (in-range 1 4)])
    (bytes-copy! mat-data (* j mat-datum-size) mat-data 0 mat-datum-size))
  
  (define draw-datum-size (vao-struct-size (program-spec-struct (sphere-opaq-program-spec))))
  (define draw-data-size (* 4 draw-datum-size))
  (define draw-data (make-bytes draw-data-size))
  (let* ([i  0]
         [i  (begin (bytes-copy! draw-data i trans-datum 0 (* 24 4))
                    (unsafe-fx+ i (* 24 4)))]
         [i  (begin (bytes-copy! draw-data i (pack-color c) 0 4)
                    (unsafe-fx+ i 4))]
         [i  (begin (bytes-copy! draw-data i (pack-emitted e) 0 4)
                    (unsafe-fx+ i 4))])
    (bytes-set! draw-data i (flonum->byte (material-ambient m)))
    (bytes-set! draw-data (unsafe-fx+ i 1) (flonum->byte (material-diffuse m)))
    (bytes-set! draw-data (unsafe-fx+ i 2) (flonum->byte (material-specular m)))
    (bytes-set! draw-data (unsafe-fx+ i 3) inside))
  (for ([j  (in-range 1 4)])
    (bytes-copy! draw-data (* j draw-datum-size) draw-data 0 draw-datum-size))
  
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
  
  (: transformed-passes (-> Shape FlAffine3- FlAffine3- (Listof Passes)))
  (define (transformed-passes s t tinv)
    (map shape-passes (shape-transform s t tinv #t)))
  
  (define ps (append* (flscene3-extract s transformed-passes)))
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
            [s  (in-list ((inst group-by-key! shape-params Face)
                          ps get-swap-params (span-start s) (span-end s)
                          shape-params-face))]
            [face  (in-value (span-key s))]
            [s  (in-list (group-by-key! ps get-swap-params (span-start s) (span-end s)
                                        shape-params-uniforms))]
            [uniforms  (in-value (span-key s))]
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
