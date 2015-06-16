#lang typed/racket/base

(require racket/list
         racket/match
         typed/opengl
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
        (attribute "" 'vec4/bytes "vert_roughness_inside")  ; vec4(roughness, inside, 0, 0)
        ))

(define sphere-mat-geometry-attributes
  (list (attribute "" 'mat4 "geom_trans")
        (attribute "" 'vec4 "geom_trans_z")
        (attribute "" 'mat4 "geom_untrans")
        (attribute "" 'float "geom_roughness")
        (attribute "" 'float "geom_inside")))

(define sphere-mat-fragment-attributes
  (list (attribute "flat" 'vec4 "frag_trans_z")
        (attribute "flat" 'mat3 "frag_untrans")
        (attribute "flat" 'float "frag_roughness")
        (attribute "flat" 'float "frag_inside")
        (attribute "smooth" 'vec3 "frag_start")
        (attribute "smooth" 'vec3 "frag_dir")))

(define-singleton (sphere-mat-vertex-code)
  (make-vertex-code
   "sphere-mat-vertex-32"
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
   sphere-mat-geometry-attributes
   #<<code
mat4x3 sphere = rows2mat4x3(t0, t1, t2);
mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(sphere));
mat4x3 unmodel = affine_inverse(model);
mat4 trans = view * a2p(model);
mat4 untrans = a2p(unmodel) * unview;
  
geom_trans = trans;
geom_trans_z = transpose(trans)[2];
geom_untrans = untrans;
geom_roughness = vert_roughness_inside.x / 255;
geom_inside = vert_roughness_inside.y;
code
   ))

(define-singleton (sphere-mat-geometry-code)
  (make-geometry-code
   "sphere-mat-geometry-32"
   #:includes
   (list impostor-bounds-code
         output-2d-rect-vertex-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "proj" 'proj)
         (standard-uniform "" 'mat4 "unproj" 'unproj))
   #:in-attributes
   sphere-mat-geometry-attributes
   #:out-attributes
   sphere-mat-fragment-attributes
   #:definitions
   (list
    #<<code
layout(points) in;
layout(triangle_strip, max_vertices=4) out;
code
    )
   #<<code
rect bbx = impostor_bounds(geom_trans[0], proj, vec3(-1.0), vec3(+1.0));
if (bbx.is_degenerate == 0.0) {
  for (int id = 0; id < 4; id++) {
    output_2d_rect_vertex(bbx, id);
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
code
   ))

(define-singleton (sphere-mat-fragment-code)
  (make-fragment-code
   "sphere-mat-fragment-32"
   #:includes
   (list output-mat-fragment-code
         ray-trace-fragment-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "proj" 'proj))
   #:in-attributes
   sphere-mat-fragment-attributes
   #<<code
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
   "sphere-mat-program-32"
   (sphere-mat-vertex-code)
   #:geometry (sphere-mat-geometry-code)
   (sphere-mat-fragment-code)))

(define-singleton/context (sphere-mat-program)
  (log-pict3d-info "<engine> creating sphere material program for OpenGL >= 32")
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
        ;; vec4(ambient, diffuse, specular, inside)
        (attribute "" 'vec4/bytes "vert_material_inside")
        ))

(define sphere-draw-geometry-attributes
  (list (attribute "" 'mat4 "geom_trans")
        (attribute "" 'vec4 "geom_trans_z")
        (attribute "" 'mat4 "geom_untrans")
        (attribute "" 'vec3 "geom_rcolor")
        (attribute "" 'vec3 "geom_ecolor")
        (attribute "" 'float "geom_alpha")
        (attribute "" 'float "geom_ambient")
        (attribute "" 'float "geom_diffuse")
        (attribute "" 'float "geom_specular")
        (attribute "" 'float "geom_inside")))

(define sphere-draw-fragment-attributes
  (list (attribute "flat" 'vec4 "frag_trans_z")
        (attribute "flat" 'vec3 "frag_rcolor")
        (attribute "flat" 'vec3 "frag_ecolor")
        (attribute "flat" 'float "frag_alpha")
        (attribute "flat" 'float "frag_ambient")
        (attribute "flat" 'float "frag_diffuse")
        (attribute "flat" 'float "frag_specular")
        (attribute "flat" 'float "frag_inside")
        (attribute "smooth" 'vec3 "frag_start")
        (attribute "smooth" 'vec3 "frag_dir")))

(define-singleton (sphere-draw-vertex-code)
  (make-vertex-code
   "sphere-draw-vertex-32"
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
   sphere-draw-geometry-attributes
   #<<code
mat4x3 sphere = rows2mat4x3(t0, t1, t2);
mat4x3 model = mat4x3(a2p(get_model_transform()) * a2p(sphere));
mat4x3 unmodel = affine_inverse(model);
mat4 trans = view * a2p(model);
mat4 untrans = a2p(unmodel) * unview;

geom_trans = trans;
geom_trans_z = transpose(trans)[2];
geom_untrans = untrans;
geom_rcolor = pow(vert_rcolor.rgb / 255, vec3(2.2));
geom_alpha = vert_rcolor.a / 255;
geom_ecolor = unpack_emitted(vert_ecolor);
geom_ambient = vert_material_inside.x / 255;
geom_diffuse = vert_material_inside.y / 255;
geom_specular = vert_material_inside.z / 255;
geom_inside = vert_material_inside.w;
code
   ))

(define-singleton (sphere-draw-geometry-code)
  (make-geometry-code
   "sphere-draw-geometry-32"
   #:includes
   (list impostor-bounds-code
         output-2d-rect-vertex-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "proj" 'proj)
         (standard-uniform "" 'mat4 "unproj" 'unproj))
   #:in-attributes
   sphere-draw-geometry-attributes
   #:out-attributes
   sphere-draw-fragment-attributes
   #:definitions
   (list
    #<<code
layout(points) in;
layout(triangle_strip, max_vertices=4) out;
code
    )
   #<<code
rect bbx = impostor_bounds(geom_trans[0], proj, vec3(-1.0), vec3(+1.0));
if (bbx.is_degenerate == 0.0) {
  for (int id = 0; id < 4; id++) {
    output_2d_rect_vertex(bbx, id);
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
code
   ))

(define-singleton (sphere-opaq-fragment-code)
  (make-fragment-code
   "sphere-opaq-fragment-32"
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
   "sphere-tran-fragment-32"
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
   "sphere-opaq-program-32"
   (sphere-draw-vertex-code)
   #:geometry (sphere-draw-geometry-code)
   (sphere-opaq-fragment-code)))

(define-singleton (sphere-tran-program-code)
  (make-program-code
   "sphere-tran-program-32"
   (sphere-draw-vertex-code)
   #:geometry (sphere-draw-geometry-code)
   (sphere-tran-fragment-code)))

(define-singleton/context (sphere-opaq-program)
  (log-pict3d-info "<engine> creating sphere opaque color pass program for OpenGL >= 32")
  (program-code->gl-program (sphere-opaq-program-code)))

(define-singleton/context (sphere-tran-program)
  (log-pict3d-info "<engine> creating sphere transparent color pass program for OpenGL >= 32")
  (program-code->gl-program (sphere-tran-program-code)))

;; ===================================================================================================
;; Sphere shape passes

(: sphere-indexes (Vectorof Index))
(define sphere-indexes (vector 0))

(: get-sphere-shape-passes (-> shape passes))
(define (get-sphere-shape-passes s)
  (match-define (sphere-shape _ _ t c e m inside?) s)
  
  (define mat-size (program-code-vao-size (sphere-mat-program-code)))
  (define mat-data (make-bytes mat-size))
  (let* ([i  (serialize-affine mat-data 0 t)]
         [i  (serialize-float/byte mat-data i (unsafe-flv4-ref m 3))]
         [i  (serialize-byte mat-data i (if inside? 1 0))])
    (void))
  
  (define draw-size (program-code-vao-size (sphere-opaq-program-code)))
  (define draw-data (make-bytes draw-size))
  (let* ([i  (serialize-affine draw-data 0 t)]
         [i  (serialize-vec4/bytes draw-data i c)]
         [i  (serialize-emitted/bytes draw-data i e)]
         [i  (serialize-material-reflectances/bytes draw-data i m)]
         [i  (serialize-byte draw-data i (if inside? 1 0))])
    (void))
  
  (if (< (flv4-ref c 3) 1.0)
      (passes
       #()
       #()
       #()
       (vector (shape-params sphere-mat-program empty #t GL_POINTS
                             (vertices 1 mat-data sphere-indexes)))
       (vector (shape-params sphere-tran-program empty #t GL_POINTS
                             (vertices 1 draw-data sphere-indexes))))
      (passes
       #()
       (vector (shape-params sphere-mat-program empty #t GL_POINTS
                             (vertices 1 mat-data sphere-indexes)))
       (vector (shape-params sphere-opaq-program empty #t GL_POINTS
                             (vertices 1 draw-data sphere-indexes)))
       #()
       #())))
