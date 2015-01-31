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
         "../../gl.rkt"
         "../../utils.rkt"
         "../affine.rkt"
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
        [(not (= 4 (flvector-length e)))
         (raise-argument-error 'make-rectangle-shape "length-4 flvector" 2 t c e m inside?)]
        [else
         (sphere-shape (box 'lazy) (->affine t) c e m inside?)]))

;; ===================================================================================================
;; Program for pass 1: material

(define sphere-mat-vertex-code
  (string-append
   output-impostor-strip-vertex-code
   model-vertex-code
   #<<code
uniform mat4 view;
uniform mat4 unview;
uniform mat4 proj;
uniform mat4 unproj;

in vec4 sphere0;
in vec4 sphere1;
in vec4 sphere2;
in vec4 vert_roughness_inside_id;

flat out vec4 frag_trans_z;
flat out mat3 frag_untrans;
flat out float frag_roughness;
flat out float frag_inside;
smooth out float frag_is_degenerate;
smooth out vec3 frag_start;
smooth out vec3 frag_dir;

void main() {
  mat4x3 sphere = rows2mat4x3(sphere0, sphere1, sphere2);
  mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(sphere));
  mat4x3 unmodel = affine_inverse(model);
  mat4 trans = view * a2p(model);
  mat4 untrans = a2p(unmodel) * unview;
  
  frag_trans_z = transpose(trans)[2];
  frag_untrans = mat3(untrans);
  frag_roughness = vert_roughness_inside_id.x / 255;
  frag_inside = vert_roughness_inside_id.y;
  int vert_id = int(vert_roughness_inside_id.z);
  frag_is_degenerate = output_impostor_strip(trans, proj, vec3(-1.0), vec3(+1.0), vert_id);
  
  vec4 dir = unproj * gl_Position;
  frag_dir = mat3(untrans) * (dir.xyz / dir.w);
  frag_start = untrans[3].xyz;
}
code
   ))

(define sphere-mat-fragment-code
  (string-append
   output-mat-fragment-code
   ray-trace-fragment-code
   #<<code
uniform mat4 proj;
uniform int width;
uniform int height;

flat in vec4 frag_trans_z;
flat in mat3 frag_untrans;
flat in float frag_roughness;
flat in float frag_inside;
smooth in float frag_is_degenerate;
smooth in vec3 frag_dir;
smooth in vec3 frag_start;

void main() {
  // all fragments should discard if this one does
  if (frag_is_degenerate > 0.0) discard;

  vec3 dir = normalize(frag_dir);
  vec2 ts = unit_sphere_intersect(frag_start, dir);
  float t = mix(ts.x, ts.y, frag_inside);
  // many nearby fragments should discard if this one does
  if (t <= 0.0) discard;
  
  vec3 pos = frag_start + dir * t;
  float vpos_z = dot(frag_trans_z, vec4(pos,1.0));  // transformed pos z coord only
  vec3 vnorm = pos * frag_untrans;
  output_mat(mix(vnorm,-vnorm,frag_inside), frag_roughness, vpos_z);
}
code
   ))

(define-singleton/context (sphere-mat-program-spec)
  (log-pict3d-info "<engine> creating sphere material program")
  
  (define struct
    (make-vao-struct
     (make-vao-field "sphere0" 4 GL_FLOAT)
     (make-vao-field "sphere1" 4 GL_FLOAT)
     (make-vao-field "sphere2" 4 GL_FLOAT)
     (make-vao-field "vert_roughness_inside_id" 4 GL_UNSIGNED_BYTE)))
  
  (define program
    (make-gl-program struct
                     (list "out_mat")
                     (list (make-gl-shader GL_VERTEX_SHADER sphere-mat-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER sphere-mat-fragment-code))))
  
  (define uniforms
    (list (cons "view" 'view)
          (cons "unview" 'unview)
          (cons "proj" 'proj)
          (cons "unproj" 'unproj)
          (cons "width" 'width)
          (cons "height" 'height)))
  
  (program-spec program uniforms))

;; ===================================================================================================
;; Program for pass 2: color

(define sphere-draw-vertex-code
  (string-append
   output-impostor-strip-vertex-code
   rgb-hsv-code
   model-vertex-code
   #<<code
uniform mat4 view;
uniform mat4 unview;
uniform mat4 proj;
uniform mat4 unproj;

in vec4 sphere0;
in vec4 sphere1;
in vec4 sphere2;
in vec4 vert_rcolor;    // vec4(r, g, b, a)
in vec4 vert_ecolor;    // vec4(r, g, b, intensity.hi)
in vec4 vert_material;  // vec4(ambient, diffuse, specular, intensity.lo)
in vec2 vert_inside_id; // vec2(inside, id)

flat out vec4 frag_trans_z;
flat out vec3 frag_rcolor;
flat out vec3 frag_ecolor;
flat out float frag_alpha;
flat out float frag_ambient;
flat out float frag_diffuse;
flat out float frag_specular;
flat out float frag_inside;
smooth out float frag_is_degenerate;
smooth out vec3 frag_start;
smooth out vec3 frag_dir;

void main() {
  mat4x3 sphere = rows2mat4x3(sphere0, sphere1, sphere2);
  mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(sphere));
  mat4x3 unmodel = affine_inverse(model);
  mat4 trans = view * a2p(model);
  mat4 untrans = a2p(unmodel) * unview;

  frag_trans_z = transpose(trans)[2];
  frag_rcolor = pow(vert_rcolor.rgb / 255, vec3(2.2));
  frag_alpha = vert_rcolor.a / 255;
  float intensity = (vert_ecolor.a * 256 + vert_material.w) / 255;
  frag_ecolor = pow(vert_ecolor.rgb / 255, vec3(2.2)) * intensity;
  frag_ambient = vert_material.x / 255;
  frag_diffuse = vert_material.y / 255;
  frag_specular = vert_material.z / 255;
  frag_inside = vert_inside_id.x;
  int vert_id = int(vert_inside_id.y);
  frag_is_degenerate = output_impostor_strip(trans, proj, vec3(-1.0), vec3(+1.0), vert_id);

  vec4 vdir = unproj * gl_Position;
  frag_start = untrans[3].xyz;
  frag_dir = mat3(untrans) * (vdir.xyz / vdir.w);
}
code
   ))

(define sphere-opaq-fragment-code
  (string-append
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

flat in vec4 frag_trans_z;
flat in vec3 frag_rcolor;
flat in vec3 frag_ecolor;
flat in float frag_alpha;
flat in float frag_ambient;
flat in float frag_diffuse;
flat in float frag_specular;
flat in float frag_inside;
smooth in float frag_is_degenerate;
smooth in vec3 frag_start;
smooth in vec3 frag_dir;

void main() {
  // all fragments should discard if this one does
  if (frag_is_degenerate > 0.0) discard;

  vec3 dir = normalize(frag_dir);
  vec3 start = frag_start;
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
}
code
   ))

(define sphere-tran-fragment-code
  (string-append
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

flat in vec4 frag_trans_z;
flat in vec3 frag_rcolor;
flat in vec3 frag_ecolor;
flat in float frag_alpha;
flat in float frag_ambient;
flat in float frag_diffuse;
flat in float frag_specular;
flat in float frag_inside;
smooth in float frag_is_degenerate;
smooth in vec3 frag_start;
smooth in vec3 frag_dir;

void main() {
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
}
code
   ))

(define draw-program-struct
  (make-vao-struct
   (make-vao-field "sphere0" 4 GL_FLOAT)
   (make-vao-field "sphere1" 4 GL_FLOAT)
   (make-vao-field "sphere2" 4 GL_FLOAT)
   (make-vao-field "vert_rcolor" 4 GL_UNSIGNED_BYTE)
   (make-vao-field "vert_ecolor" 4 GL_UNSIGNED_BYTE)
   (make-vao-field "vert_material" 4 GL_UNSIGNED_BYTE)
   (make-vao-field "vert_inside_id" 2 GL_UNSIGNED_BYTE)))

(define draw-program-uniforms
  (list (cons "view" 'view)
        (cons "proj" 'proj)
        (cons "unview" 'unview)
        (cons "unproj" 'unproj)
        (cons "width" 'width)
        (cons "height" 'height)
        (cons "ambient" 'ambient)
        (cons "diffuse" 'diffuse)
        (cons "specular" 'specular)))

(define-singleton/context (sphere-opaq-program-spec)
  (log-pict3d-info "<engine> creating sphere opaque color pass program")
  (program-spec
   (make-gl-program draw-program-struct
                    (list "out_color")
                    (list (make-gl-shader GL_VERTEX_SHADER sphere-draw-vertex-code)
                          (make-gl-shader GL_FRAGMENT_SHADER sphere-opaq-fragment-code)))
   draw-program-uniforms))

(define-singleton/context (sphere-tran-program-spec)
  (log-pict3d-info "<engine> creating sphere transparent color pass program")
  (program-spec
   (make-gl-program draw-program-struct
                    (list "out_color" "out_weight")
                    (list (make-gl-shader GL_VERTEX_SHADER sphere-draw-vertex-code)
                          (make-gl-shader GL_FRAGMENT_SHADER sphere-tran-fragment-code)))
   draw-program-uniforms))

;; ===================================================================================================
;; Sphere shape passes

(: vertex-ids (Vectorof Index))
(define vertex-ids #(0 1 2 2 1 3))

(: make-sphere-shape-passes (-> sphere-shape Passes))
(define (make-sphere-shape-passes a)
  (match-define (sphere-shape _ t c e m inside?) a)
  
  (define affine-ptr (f32vector->cpointer (affine-data t)))
  (define roughness (flonum->byte (material-roughness m)))
  (define inside (if inside? 1 0))
  
  (define mat-datum-size (+ affine-size 4))  ; last byte is unused
  (define mat-data-size (* 4 mat-datum-size))
  (define mat-data (make-bytes mat-data-size))
  (memcpy (u8vector->cpointer mat-data) affine-ptr affine-size)
  (bytes-set! mat-data affine-size roughness)
  (bytes-set! mat-data (unsafe-fx+ 1 affine-size) inside)
  (bytes-set! mat-data (unsafe-fx+ 2 affine-size) 0)
  (for ([j  (in-range 1 4)])
    (bytes-copy! mat-data (unsafe-fx* j mat-datum-size) mat-data 0 mat-datum-size)
    (bytes-set! mat-data
                (unsafe-fx+ (unsafe-fx* j mat-datum-size)
                            (unsafe-fx+ 2 affine-size))
                j))
  
  (define draw-datum-size
    (vao-struct-size (gl-program-struct (program-spec-program (sphere-opaq-program-spec)))))
  (define draw-data-size (* 4 draw-datum-size))
  (define draw-data (make-bytes draw-data-size))
  (define-values (ecolor i.lo) (pack-emitted e))
  (define i
    (let* ([i  0]
           [i  (begin (memcpy (u8vector->cpointer draw-data) i affine-ptr affine-size)
                      (unsafe-fx+ i affine-size))]
           [i  (begin (bytes-copy! draw-data i (pack-color c) 0 4)
                      (unsafe-fx+ i 4))]
           [i  (begin (bytes-copy! draw-data i ecolor 0 4)
                      (unsafe-fx+ i 4))])
      (bytes-set! draw-data i (flonum->byte (material-ambient m)))
      (bytes-set! draw-data (unsafe-fx+ i 1) (flonum->byte (material-diffuse m)))
      (bytes-set! draw-data (unsafe-fx+ i 2) (flonum->byte (material-specular m)))
      (bytes-set! draw-data (unsafe-fx+ i 3) i.lo)
      (bytes-set! draw-data (unsafe-fx+ i 4) inside)
      (bytes-set! draw-data (unsafe-fx+ i 5) 0)
      (unsafe-fx+ i 5)))
  (for ([j  (in-range 1 4)])
    (bytes-copy! draw-data (unsafe-fx* j draw-datum-size) draw-data 0 draw-datum-size)
    (bytes-set! draw-data
                (unsafe-fx+ (unsafe-fx* j draw-datum-size) i)
                j))
  
  (define transparent? (< (flvector-ref c 3) 1.0))
  
  (: passes Passes)
  (define passes
    (if transparent?
        (vector
         #()
         #()
         #()
         (vector (shape-params sphere-mat-program-spec empty #t GL_TRIANGLES
                               (vertices 4 mat-data vertex-ids)))
         (vector (shape-params sphere-tran-program-spec empty #t GL_TRIANGLES
                               (vertices 4 draw-data vertex-ids))))
        (vector
         #()
         (vector (shape-params sphere-mat-program-spec empty #t GL_TRIANGLES
                               (vertices 4 mat-data vertex-ids)))
         (vector (shape-params sphere-opaq-program-spec empty #t GL_TRIANGLES
                               (vertices 4 draw-data vertex-ids)))
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

(: sphere-shape-transform (-> sphere-shape FlAffine3- (List sphere-shape)))
(define (sphere-shape-transform a t)
  (match-define (sphere-shape passes t0 c e m inside?) a)
  (list (sphere-shape (box 'lazy) (affine-compose t t0) c e m inside?)))
