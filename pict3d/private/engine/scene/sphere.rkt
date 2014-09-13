#lang typed/racket/base

;; Sphere impostors: vertex shader outputs a quad; fragment shaders ray-trace a sphere

(require racket/unsafe/ops
         racket/list
         racket/match
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         math/flonum
         "../../math/flt3.rkt"
         "../../math/flrect3.rkt"
         "../affine.rkt"
         "../gl.rkt"
         "../types.rkt"
         "../utils.rkt"
         "../shader-lib.rkt"
         "../draw-pass.rkt"
         "types.rkt"
         "shader-lib.rkt")

(provide make-sphere-shape
         make-sphere-shape-passes
         sphere-shape-rect
         sphere-shape-transform
         )

;; ===================================================================================================
;; Constructors

(: make-sphere-shape (-> (U FlAffine3- affine) FlVector FlVector material Boolean sphere-shape))
(define (make-sphere-shape t c e m inside?)
  (cond [(not (= 4 (flvector-length c)))
         (raise-argument-error 'make-rectangle-shape "length-4 flvector" 1 t c e m inside?)]
        [(not (= 3 (flvector-length e)))
         (raise-argument-error 'make-rectangle-shape "length-3 flvector" 2 t c e m inside?)]
        [else
         (sphere-shape (box 'lazy) (->affine t) c e m inside?)]))

;; ===================================================================================================
;; Program for pass 1: material

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
  output_mat(mix(vnorm,-vnorm,frag_inside), frag_roughness, vpos.z);
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
          (cons "width" 'width)
          (cons "height" 'height)))
  
  (program-spec program uniforms struct))

;; ===================================================================================================
;; Program for pass 2: color

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
  output_opaq(color, frag_rcolor.a, vpos.z);
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
  output_tran(color, frag_rcolor.a, vpos.z);
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
          (cons "width" 'width)
          (cons "height" 'height)
          (cons "ambient" 'ambient)
          (cons "diffuse" 'diffuse)
          (cons "specular" 'specular)))
  
  (program-spec program uniforms struct))

;; ===================================================================================================
;; Sphere shape passes

(: make-sphere-shape-passes (-> sphere-shape Passes))
(define (make-sphere-shape-passes a)
  (match-define (sphere-shape _ t c e m inside?) a)
  
  (define affine-ptr (f32vector->cpointer (affine-data t)))
  (define roughness (flonum->byte (material-roughness m)))
  (define inside (if inside? 255 0))
  
  (define mat-datum-size (+ affine-size 4))  ; last two bytes are unused
  (define mat-data-size (* 4 mat-datum-size))
  (define mat-data (make-bytes mat-data-size))
  (memcpy (u8vector->cpointer mat-data) affine-ptr affine-size)
  (bytes-set! mat-data affine-size roughness)
  (bytes-set! mat-data (unsafe-fx+ 1 affine-size) inside)
  (for ([j  (in-range 1 4)])
    (bytes-copy! mat-data (unsafe-fx* j mat-datum-size) mat-data 0 mat-datum-size))
  
  (define draw-datum-size (vao-struct-size (program-spec-struct (sphere-opaq-program-spec))))
  (define draw-data-size (* 4 draw-datum-size))
  (define draw-data (make-bytes draw-data-size))
  (let* ([i  0]
         [i  (begin (memcpy (u8vector->cpointer draw-data) i affine-ptr affine-size)
                    (unsafe-fx+ i affine-size))]
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
;; Bounding box

(define unit-sphere-rect
  (nonempty-flrect3 (flvector -1.0 -1.0 -1.0)
                    (flvector +1.0 +1.0 +1.0)))

(: sphere-shape-rect (-> sphere-shape Nonempty-FlRect3))
(define (sphere-shape-rect a)
  (flrect3-transform unit-sphere-rect (affine-transform (sphere-shape-affine a))))

;; ===================================================================================================
;; Transform

(: sphere-shape-transform (-> sphere-shape FlAffine3- FlAffine3- (List sphere-shape)))
(define (sphere-shape-transform a t tinv)
  (match-define (sphere-shape passes t0 c e m inside?) a)
  (list (sphere-shape (box 'lazy) (affine-compose t t0) c e m inside?)))
