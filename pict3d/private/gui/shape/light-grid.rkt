#lang typed/racket/base

(require racket/unsafe/ops
         racket/match
         racket/list
         racket/flonum
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         "../../math.rkt"
         "../../gl.rkt"
         "../../memo.rkt"
         "../../engine.rkt"
         "../../utils.rkt")

(provide make-light-grid-shape
         (struct-out light-grid-shape))

(struct light-grid-shape shape
  ([x-emitted : FlV4]
   [y-emitted : FlV4]
   [z-emitted : FlV4]
   [scale : Flonum])
  #:transparent)

;; ===================================================================================================
;; Constructor

(: make-light-grid-shape (-> FlV4 FlV4 FlV4 Flonum light-grid-shape))
(define (make-light-grid-shape ex ey ez scale)
  (light-grid-shape (lazy-passes) light-grid-shape-functions ex ey ez scale))

;; ===================================================================================================
;; Program for pass 0: light

(define light-grid-vertex-attributes
  (list (attribute "" 'float/byte "vertex_id")))

(define light-grid-fragment-attributes
  (list (attribute "smooth" 'vec3 "frag_dir")
        (attribute "smooth" 'vec3 "frag_dir0")
        (attribute "smooth" 'vec3 "frag_dir1")
        (attribute "smooth" 'vec3 "frag_dir2")
        (attribute "smooth" 'vec3 "frag_dir3")))

(define light-grid-vertex-code
  (make-vertex-code
   "light-grid-vertex"
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "unproj" 'unproj)
         (standard-uniform "" 'int "width" 'width)
         (standard-uniform "" 'int "height" 'height))
   #:in-attributes light-grid-vertex-attributes
   #:out-attributes light-grid-fragment-attributes
   #<<code
// output the right vertices for a triangle strip
vec4 p = vec4(mix(-1.0, +1.0, int(vertex_id) & 1),
              mix(-1.0, +1.0, (int(vertex_id) & 2) >> 1),
              0.0,
              1.0);
gl_Position = p;

vec4 dir = unproj * p;
vec4 dir0 = unproj * (p + vec4(0,+4.0/height,0,0));
vec4 dir1 = unproj * (p + vec4(0,-4.0/height,0,0));
vec4 dir2 = unproj * (p + vec4(+4.0/width,0,0,0));
vec4 dir3 = unproj * (p + vec4(-4.0/width,0,0,0));

frag_dir = vec3(dir.xy / dir.z, 1.0);
frag_dir0 = vec3(dir0.xy / dir0.z, 1.0);
frag_dir1 = vec3(dir1.xy / dir1.z, 1.0);
frag_dir2 = vec3(dir2.xy / dir2.z, 1.0);
frag_dir3 = vec3(dir3.xy / dir3.z, 1.0);
code
   ))

(define light-grid-fragment-code
  (make-fragment-code
   "light-grid-fragment"
   #:includes
   (list light-fragment-code)
   #:standard-uniforms
   (list (standard-uniform "" 'mat4 "proj" 'proj)
         (standard-uniform "" 'mat4 "unproj" 'unproj)
         (standard-uniform "" 'mat4 "view" 'view)
         (standard-uniform "" 'mat4 "unview" 'unview)
         (standard-uniform "" 'sampler2D "depth" 'depth)
         (standard-uniform "" 'sampler2D "material" 'material)
         (standard-uniform "" 'int "width" 'width)
         (standard-uniform "" 'int "height" 'height))
   #:program-uniforms
   (list (attribute "" 'vec3 "x_color")
         (attribute "" 'float "x_intensity")
         (attribute "" 'vec3 "y_color")
         (attribute "" 'float "y_intensity")
         (attribute "" 'vec3 "z_color")
         (attribute "" 'float "z_intensity")
         (attribute "" 'float "grid_scale"))
   #:in-attributes
   light-grid-fragment-attributes
   #:definitions
   (list
   #<<code
float dash_multiplier(vec3 T, vec3 P, float h) {
  float x = abs(T.x) > abs(T.y) ? P.x : P.y;
  return max(0.0,sin(x*1.5707963/h));
}
code
   #<<code
float implicit_fun(float x) {
  float y = 0.5 * (x + 0.5 * grid_scale) / grid_scale;
  return abs(y - round(y)) - 0.25;
}
code
   #<<code
float line_intensity(float x, float x0, float x1, float x2, float x3) {
  float d = implicit_fun(x);
  float d0 = implicit_fun(x0);
  float d1 = implicit_fun(x1);
  float d2 = implicit_fun(x2);
  float d3 = implicit_fun(x3);

  float s = sign(d);
  float v = 0.0;
  if (s != sign(d0) || s != sign(d1) || s != sign(d2) || s != sign(d3)) {
    // Estimate distance from center
    v = abs(d) / max(max(abs(d-d0), abs(d-d1)), max(abs(d-d2), abs(d-d3)));
    // Gaussian kernel, stddev 1/3
    v = 0.5 * exp(-0.5*v*v/(1.0/9.0));
  }
  return v;
}
code
   #<<code
vec3 view_cross_proj(vec3 vpos, vec3 N, vec3 M) {
  vec3 V = cross(N,M);
  vec4 V1 = proj * vec4(vpos,1);
  vec4 V2 = proj * vec4(vpos + V,1);
  return V2.xyz / V2.w - V1.xyz / V1.w;
}
code
   )
   #<<code
float max_dist = 24.0 * grid_scale;

float d = texelFetch(depth, ivec2(gl_FragCoord.xy), 0).r;
if (d == 0.0) discard;

float z = get_view_depth(d);
if (abs(z) > max_dist) discard;

float d0 = texelFetch(depth, ivec2(gl_FragCoord.xy) + ivec2(0,+1), 0).r;
float d1 = texelFetch(depth, ivec2(gl_FragCoord.xy) + ivec2(0,-1), 0).r;
float d2 = texelFetch(depth, ivec2(gl_FragCoord.xy) + ivec2(+1,0), 0).r;
float d3 = texelFetch(depth, ivec2(gl_FragCoord.xy) + ivec2(-1,0), 0).r;
if (d0 == 0.0 || d1 == 0.0 || d2 == 0.0 || d3 == 0.0) discard;

float z0 = get_view_depth(d0);
float z1 = get_view_depth(d1);
float z2 = get_view_depth(d2);
float z3 = get_view_depth(d3);

vec3 vpos = frag_dir * z;
vec3 vpos0 = frag_dir0 * z0;
vec3 vpos1 = frag_dir1 * z1;
vec3 vpos2 = frag_dir2 * z2;
vec3 vpos3 = frag_dir3 * z3;
vec3 mpos = (unview * vec4(vpos,1)).xyz;
vec3 mpos0 = (unview * vec4(vpos0,1)).xyz;
vec3 mpos1 = (unview * vec4(vpos1,1)).xyz;
vec3 mpos2 = (unview * vec4(vpos2,1)).xyz;
vec3 mpos3 = (unview * vec4(vpos3,1)).xyz;

float xv = line_intensity(mpos.x, mpos0.x, mpos1.x, mpos2.x, mpos3.x);
float yv = line_intensity(mpos.y, mpos0.y, mpos1.y, mpos2.y, mpos3.y);
float zv = line_intensity(mpos.z, mpos0.z, mpos1.z, mpos2.z, mpos3.z);

// Dash length
float h = 2.0 * abs(z) / float(max(width,height));

// Multipliers for dashed lines
vec3 N = get_surface(material).normal;
vec3 xS = view_cross_proj(vpos, N, (transpose(unview) * vec4(1,0,0,0)).xyz);
vec3 yS = view_cross_proj(vpos, N, (transpose(unview) * vec4(0,1,0,0)).xyz);
vec3 zS = view_cross_proj(vpos, N, (transpose(unview) * vec4(0,0,1,0)).xyz);
float xdash = dash_multiplier(xS, vpos, h);
float ydash = dash_multiplier(yS, vpos, h);
float zdash = dash_multiplier(zS, vpos, h);

vec3 xline = xdash * xv * pow(x_color,vec3(2.2)) * x_intensity;
vec3 yline = ydash * yv * pow(y_color,vec3(2.2)) * y_intensity;
vec3 zline = zdash * zv * pow(z_color,vec3(2.2)) * z_intensity;

vec3 M = normalize((transpose(view) * vec4(N,0)).xyz);
float xatt = abs(M.x) > 0.95 ? 0.0 : 1.0;
float yatt = abs(M.y) > 0.95 ? 0.0 : 1.0;
float zatt = abs(M.z) > 0.95 ? 0.0 : 1.0;
float dist = length(vpos);
float att = 0.0;
if (dist < max_dist) {
  float a = (max_dist - dist) / max_dist;
  att = a*a;
}

out_diffuse = vec4((xline * xatt + yline * yatt + zline * zatt) * att,1);
out_specular = vec4((xline * xatt + yline * yatt + zline * zatt) * att,1);

code
   ))

(define light-grid-program-code
  (make-program-code
   "light-grid-program"
   light-grid-vertex-code
   light-grid-fragment-code))

(define-singleton/context (light-grid-program)
  (log-pict3d-info "<engine> creating light grid program")
  (program-code->gl-program light-grid-program-code))

;; ===================================================================================================
;; Light grid shape passes

(define data (list->bytes '(0 1 2 3)))

(: vertex-ids (Vectorof Index))
(define vertex-ids #(0 1 2 2 1 3))

(: get-light-grid-shape-passes (-> shape passes))
(define (get-light-grid-shape-passes s)
  (match-define (light-grid-shape _ _ ex ey ez scale) s)
  
  (define-values (rx gx bx ix) (call/flv4-values ex values))
  (define-values (ry gy by iy) (call/flv4-values ey values))
  (define-values (rz gz bz iz) (call/flv4-values ez values))
  
  (define uniforms
    (list (cons "x_color" (uniform-float (flvector rx gx bx)))
          (cons "y_color" (uniform-float (flvector ry gy by)))
          (cons "z_color" (uniform-float (flvector rz gz bz)))
          (cons "x_intensity" (uniform-float ix))
          (cons "y_intensity" (uniform-float iy))
          (cons "z_intensity" (uniform-float iz))
          (cons "grid_scale" (uniform-float scale))))
  
  (passes
   (vector (shape-params light-grid-program uniforms #t GL_TRIANGLES
                         (vertices 4 data vertex-ids)))
   #()
   #()
   #()
   #()))

;; ===================================================================================================
;; Bounding box

(define light-grid-shape-bbox
  (bbox inf-flrect3 0.0))

;; ===================================================================================================

(define light-grid-shape-functions
  (shape-functions
   get-light-grid-shape-passes
   (λ (s kind t) (and (eq? kind 'invisible) light-grid-shape-bbox))
   (λ (s t) s)
   (λ (s t) (list s))
   (λ (s v dv max-time) (values #f #f))))
