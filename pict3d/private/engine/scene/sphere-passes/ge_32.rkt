#lang typed/racket/base

(require racket/unsafe/ops
         racket/list
         racket/match
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         math/flonum
         "../../../gl.rkt"
         "../../../utils.rkt"
         "../../types.rkt"
         "../../utils.rkt"
         "../../shader-lib.rkt"
         "../../draw-pass.rkt"
         "../types.rkt"
         "../flags.rkt"
         "../shader-lib.rkt")

(provide make-sphere-shape-passes)

;; ===================================================================================================
;; Program for pass 1: material

(define-singleton (sphere-mat-vertex-code)
  (string-append
   output-impostor-strip-vertex-code
   model-vertex-code
   #<<code
uniform mat4 view;
uniform mat4 unview;
uniform mat4 proj;
uniform mat4 unproj;
//uniform mat4x3 _model;

in vec4 sphere0;
in vec4 sphere1;
in vec4 sphere2;
in vec4 vert_roughness_inside_id;

invariant out mat4 geom_trans;
invariant out vec4 geom_trans_z;
invariant out mat4 geom_untrans;
out float geom_roughness;
out float geom_inside;

void main() {
  mat4x3 sphere = rows2mat4x3(sphere0, sphere1, sphere2);
  mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(sphere));
  //mat4x3 model = mat4x3(a2p(_model) * a2p(sphere));
  mat4x3 unmodel = affine_inverse(model);
  mat4 trans = view * a2p(model);
  mat4 untrans = a2p(unmodel) * unview;
  
  geom_trans = trans;
  geom_trans_z = transpose(trans)[2];
  geom_untrans = untrans;
  geom_roughness = vert_roughness_inside_id.x / 255;
  geom_inside = vert_roughness_inside_id.y;
}
code
   ))

(define-singleton (sphere-mat-geometry-code)
  (string-append
   "invariant gl_Position;\n\n"
   impostor-bounds-code
   output-impostor-strip-geometry-code
   #<<code
layout(points) in;
layout(triangle_strip, max_vertices=4) out;

uniform mat4 proj;
uniform mat4 unproj;

invariant in mat4 geom_trans[];
invariant in vec4 geom_trans_z[];
invariant in mat4 geom_untrans[];
in float geom_roughness[];
in float geom_inside[];

invariant flat out vec4 frag_trans_z;
flat out mat3 frag_untrans;
flat out float frag_roughness;
flat out float frag_inside;

invariant smooth out vec3 frag_start;
invariant smooth out vec3 frag_dir;

void main() {
  rect bbx = impostor_bounds(geom_trans[0], proj, vec3(-1.0), vec3(+1.0));
  if (bbx.is_degenerate == 0.0) {
    for (int id = 0; id < 4; id++) {
      output_impostor_strip_vertex(bbx, id);
      gl_PrimitiveID = gl_PrimitiveIDIn;

      frag_trans_z = geom_trans_z[0];
      frag_untrans = mat3(geom_untrans[0]);
      frag_roughness = geom_roughness[0];
      frag_inside = geom_inside[0];
      
      vec4 dir = unproj * gl_Position;
      frag_dir = mat3(geom_untrans[0]) * (dir.xyz / dir.w);
      frag_start = geom_untrans[0][3].xyz;

      EmitVertex();
    }
    EndPrimitive();
  }
}
code
   ))

(define-singleton (sphere-mat-fragment-code)
  (string-append
   output-mat-fragment-code
   ray-trace-fragment-code
   #<<code
uniform mat4 proj;

invariant flat in vec4 frag_trans_z;
flat in mat3 frag_untrans;
flat in float frag_roughness;
flat in float frag_inside;

invariant smooth in vec3 frag_dir;
invariant smooth in vec3 frag_start;

void main() {
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
}
code
   ))

(define-singleton/context (sphere-mat-program)
  (log-pict3d-info "<engine> creating sphere material program for OpenGL >= 32")
  (make-gl-program
   "sphere-mat-program-32"
   (list (cons "view" 'view)
         (cons "unview" 'unview)
         (cons "proj" 'proj)
         (cons "unproj" 'unproj))
   (make-vao-struct
    (make-vao-field "sphere0" 4 GL_FLOAT)
    (make-vao-field "sphere1" 4 GL_FLOAT)
    (make-vao-field "sphere2" 4 GL_FLOAT)
    (make-vao-field "vert_roughness_inside_id" 4 GL_UNSIGNED_BYTE))
   (list "out_mat")
   (list (make-gl-shader GL_VERTEX_SHADER (sphere-mat-vertex-code))
         (make-gl-shader GL_GEOMETRY_SHADER (sphere-mat-geometry-code))
         (make-gl-shader GL_FRAGMENT_SHADER (sphere-mat-fragment-code)))))

;; ===================================================================================================
;; Program for pass 2: color

(define-singleton (sphere-draw-vertex-code)
  (string-append
   output-impostor-strip-vertex-code
   rgb-hsv-code
   model-vertex-code
   #<<code
uniform mat4 view;
uniform mat4 unview;
uniform mat4 proj;
uniform mat4 unproj;
//uniform mat4x3 _model;

in vec4 sphere0;
in vec4 sphere1;
in vec4 sphere2;
in vec4 vert_rcolor;    // vec4(r, g, b, a)
in vec4 vert_ecolor;    // vec4(r, g, b, intensity.hi)
in vec4 vert_material;  // vec4(ambient, diffuse, specular, intensity.lo)
in vec2 vert_inside_id; // vec2(inside, id)

invariant out mat4 geom_trans;
invariant out vec4 geom_trans_z;
invariant out mat4 geom_untrans;
out vec3 geom_rcolor;
out vec3 geom_ecolor;
out float geom_alpha;
out float geom_ambient;
out float geom_diffuse;
out float geom_specular;
out float geom_inside;

void main() {
  mat4x3 sphere = rows2mat4x3(sphere0, sphere1, sphere2);
  mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(sphere));
  //mat4x3 model = mat4x3(a2p(_model) * a2p(sphere));
  mat4x3 unmodel = affine_inverse(model);
  mat4 trans = view * a2p(model);
  mat4 untrans = a2p(unmodel) * unview;

  geom_trans = trans;
  geom_trans_z = transpose(trans)[2];
  geom_untrans = untrans;
  geom_rcolor = pow(vert_rcolor.rgb / 255, vec3(2.2));
  geom_alpha = vert_rcolor.a / 255;
  float intensity = (vert_ecolor.a * 256 + vert_material.w) / 255;
  geom_ecolor = pow(vert_ecolor.rgb / 255, vec3(2.2)) * intensity;
  geom_ambient = vert_material.x / 255;
  geom_diffuse = vert_material.y / 255;
  geom_specular = vert_material.z / 255;
  geom_inside = vert_inside_id.x;
}
code
   ))

(define-singleton (sphere-draw-geometry-code)
  (string-append
   "invariant gl_Position;\n\n"
   impostor-bounds-code
   output-impostor-strip-geometry-code
   #<<code
layout(points) in;
layout(triangle_strip, max_vertices=4) out;

uniform mat4 proj;
uniform mat4 unproj;

invariant in mat4 geom_trans[];
invariant in vec4 geom_trans_z[];
invariant in mat4 geom_untrans[];
in vec3 geom_rcolor[];
in vec3 geom_ecolor[];
in float geom_alpha[];
in float geom_ambient[];
in float geom_diffuse[];
in float geom_specular[];
in float geom_inside[];

invariant flat out vec4 frag_trans_z;
flat out vec3 frag_rcolor;
flat out vec3 frag_ecolor;
flat out float frag_alpha;
flat out float frag_ambient;
flat out float frag_diffuse;
flat out float frag_specular;
flat out float frag_inside;

invariant smooth out vec3 frag_start;
invariant smooth out vec3 frag_dir;

void main() {
  rect bbx = impostor_bounds(geom_trans[0], proj, vec3(-1.0), vec3(+1.0));
  if (bbx.is_degenerate == 0.0) {
    for (int id = 0; id < 4; id++) {
      output_impostor_strip_vertex(bbx, id);
      gl_PrimitiveID = gl_PrimitiveIDIn;

      frag_trans_z = geom_trans_z[0];
      frag_rcolor = geom_rcolor[0];
      frag_ecolor = geom_ecolor[0];
      frag_alpha = geom_alpha[0];
      frag_ambient = geom_ambient[0];
      frag_diffuse = geom_diffuse[0];
      frag_specular = geom_specular[0];
      frag_inside = geom_inside[0];

      vec4 dir = unproj * gl_Position;
      frag_dir = mat3(geom_untrans[0]) * (dir.xyz / dir.w);
      frag_start = geom_untrans[0][3].xyz;

      EmitVertex();
    }
    EndPrimitive();
  }
}
code
   ))

(define-singleton (sphere-opaq-fragment-code)
  (string-append
   output-opaq-fragment-code
   ray-trace-fragment-code
   #<<code
uniform mat4 proj;
uniform mat4 unproj;

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

smooth in vec3 frag_start;
smooth in vec3 frag_dir;

// Make everything that could affect vpos_z or the output invariant
invariant frag_trans_z, frag_dir, frag_start;

void main() {
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
}
code
   ))

(define-singleton (sphere-tran-fragment-code)
  (string-append
   output-tran-fragment-code
   ray-trace-fragment-code
   #<<code
uniform mat4 proj;
uniform mat4 unproj;

uniform vec3 ambient;
uniform sampler2D diffuse;
uniform sampler2D specular;

invariant flat in vec4 frag_trans_z;
flat in vec3 frag_rcolor;
flat in vec3 frag_ecolor;
flat in float frag_alpha;
flat in float frag_ambient;
flat in float frag_diffuse;
flat in float frag_specular;
flat in float frag_inside;

invariant smooth in vec3 frag_start;
invariant smooth in vec3 frag_dir;

void main() {
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

(define-singleton (draw-program-struct)
  (make-vao-struct
   (make-vao-field "sphere0" 4 GL_FLOAT)
   (make-vao-field "sphere1" 4 GL_FLOAT)
   (make-vao-field "sphere2" 4 GL_FLOAT)
   (make-vao-field "vert_rcolor" 4 GL_UNSIGNED_BYTE)
   (make-vao-field "vert_ecolor" 4 GL_UNSIGNED_BYTE)
   (make-vao-field "vert_material" 4 GL_UNSIGNED_BYTE)
   (make-vao-field "vert_inside_id" 2 GL_UNSIGNED_BYTE)))

(define-singleton (draw-program-uniforms)
  (list (cons "view" 'view)
        (cons "proj" 'proj)
        (cons "unview" 'unview)
        (cons "unproj" 'unproj)
        (cons "ambient" 'ambient)
        (cons "diffuse" 'diffuse)
        (cons "specular" 'specular)))

(define-singleton/context (sphere-opaq-program)
  (log-pict3d-info "<engine> creating sphere opaque color pass program for OpenGL >= 32")
  (make-gl-program
   "sphere-opaq-program-32"
   (draw-program-uniforms)
   (draw-program-struct)
   (list "out_color")
   (list (make-gl-shader GL_VERTEX_SHADER (sphere-draw-vertex-code))
         (make-gl-shader GL_GEOMETRY_SHADER (sphere-draw-geometry-code))
         (make-gl-shader GL_FRAGMENT_SHADER (sphere-opaq-fragment-code)))))

(define-singleton/context (sphere-tran-program)
  (log-pict3d-info "<engine> creating sphere transparent color pass program for OpenGL >= 32")
  (make-gl-program
   "sphere-tran-program-32"
   (draw-program-uniforms)
   (draw-program-struct)
   (list "out_color" "out_weight")
   (list (make-gl-shader GL_VERTEX_SHADER (sphere-draw-vertex-code))
         (make-gl-shader GL_GEOMETRY_SHADER (sphere-draw-geometry-code))
         (make-gl-shader GL_FRAGMENT_SHADER (sphere-tran-fragment-code)))))

;; ===================================================================================================
;; Sphere shape passes

(: make-sphere-shape-passes (-> sphere-shape passes))
(define (make-sphere-shape-passes a)
  (match-define (sphere-shape _ fs t c e m inside?) a)
  
  (define affine-ptr (f32vector->cpointer (affine-data t)))
  (define roughness (flonum->byte (material-roughness m)))
  (define inside (if inside? 1 0))
  
  (define mat-datum-size (+ affine-data-size 4))  ; last byte is unused
  (define mat-data-size mat-datum-size)
  (define mat-data (make-bytes mat-data-size))
  (memcpy (u8vector->cpointer mat-data) affine-ptr affine-data-size)
  (bytes-set! mat-data affine-data-size roughness)
  (bytes-set! mat-data (unsafe-fx+ 1 affine-data-size) inside)
  (bytes-set! mat-data (unsafe-fx+ 2 affine-data-size) 0)
  
  (define draw-datum-size (vao-struct-size (gl-program-struct (sphere-opaq-program))))
  (define draw-data-size draw-datum-size)
  (define draw-data (make-bytes draw-data-size))
  (define-values (ecolor i.lo) (pack-emitted e))
  (define i
    (let* ([i  0]
           [i  (begin (memcpy (u8vector->cpointer draw-data) i affine-ptr affine-data-size)
                      (unsafe-fx+ i affine-data-size))]
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
  
  (if (flags-subset? transparent-flag fs)
      (passes
       #()
       #()
       #()
       (vector (shape-params sphere-mat-program empty #t GL_POINTS (vertices 1 mat-data #f)))
       (vector (shape-params sphere-tran-program empty #t GL_POINTS (vertices 1 draw-data #f))))
      (passes
       #()
       (vector (shape-params sphere-mat-program empty #t GL_POINTS (vertices 1 mat-data #f)))
       (vector (shape-params sphere-opaq-program empty #t GL_POINTS (vertices 1 draw-data #f)))
       #()
       #())))
