#lang typed/racket/base

(require "../shader-lib.rkt")

(provide (all-defined-out))

(define pack-unpack-normal-code
  (string-append
   #<<code
vec2 pack_normal(vec3 norm) {
  vec2 res;
  res = 0.5 * (norm.xy + vec2(1.0, 1.0));
  res.x *= (norm.z < 0 ? -1.0 : 1.0);
  return res;
}

vec3 unpack_normal(vec2 norm) {
  vec3 res;
  res.xy = (2.0 * abs(norm)) - vec2(1.0, 1.0);
  res.z = (norm.x < 0 ? -1.0 : 1.0) * sqrt(abs(1.0 - res.x * res.x - res.y * res.y));
  return res;
}
code
   "\n\n"))

(define depth-fragment-code
  (string-append
   #<<code
uniform float zfar;
uniform float znear;
uniform float log2_znear_zfar;

float get_frag_depth(float z) {
  return log2(-z/zfar) / log2_znear_zfar;
}

float get_view_depth(float depth) {
  return -exp2(depth * log2_znear_zfar) * zfar;
}
code
   "\n\n"))

(define get-surface-fragment-code
  (string-append
   pack-unpack-normal-code
   #<<code
struct surface {
  vec3 normal;
  float roughness;
};

surface get_surface(sampler2D matTex) {
  vec4 mat = texelFetch(matTex, ivec2(gl_FragCoord.xy), 0);
  //return surface(unpack_normal(mat.rg), mat.a);
  return surface(mat.rgb, mat.a);
}
code
   "\n\n"))

(define impostor-bounds-code
  (string-append
   #<<code
struct rect {
  vec3 mins;
  vec3 maxs;
  float is_degenerate;
};

const float pos_infinity = 1.0/0.0;
const float neg_infinity = -pos_infinity;

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
   "\n\n"))

(define output-impostor-strip-geometry-code
  (string-append
   #<<code
void output_impostor_strip_vertex(rect bbx, int vertex_id) {
  // output the correct vertices for a triangle strip
  switch (vertex_id) {
  case 0:
    gl_Position = vec4(bbx.mins.xy, 0.0, 1.0);
    break;
  case 1:
    gl_Position = vec4(bbx.maxs.x, bbx.mins.y, 0.0, 1.0);
    break;
  case 2:
    gl_Position = vec4(bbx.mins.x, bbx.maxs.y, 0.0, 1.0);
    break;
  default:
    gl_Position = vec4(bbx.maxs.xy, 0.0, 1.0);
    break;
  }
}
code
   "\n\n"))

(define output-impostor-strip-vertex-code
  (string-append
   impostor-bounds-code
   output-impostor-strip-geometry-code
   #<<code
float output_impostor_strip(mat4 view, mat4 proj, vec3 wmin, vec3 wmax, int vertex_id) {
  rect bbx = impostor_bounds(view, proj, wmin, wmax);
  output_impostor_strip_vertex(bbx, vertex_id);
  return bbx.is_degenerate;
}
code
   "\n\n"))

(define ray-trace-fragment-code
  (string-append
   #<<code
vec3 frag_coord_to_direction(vec4 frag_coord, mat4 unproj, int width, int height) {
  vec2 clip_xy = (frag_coord.xy / vec2(width,height) - vec2(0.5)) * 2.0;
  vec4 vpos = unproj * vec4(clip_xy, 0.0, 1.0);
  return normalize(vpos.xyz);
}

vec2 unit_sphere_intersect(vec3 origin, vec3 dir) {
  float b = dot(origin,dir);
  float disc = b*b - dot(origin,origin) + 1;
  if (disc < 0.0) discard;
  float q = sqrt(disc);
  return vec2(-q,q) - vec2(b);
}
code
   "\n\n"))

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
out vec4 out_mat;

void output_mat(vec3 dir, float roughness, float z) {
  gl_FragDepth = get_frag_depth(z);
  //out_mat = vec4(pack_normal(normalize(dir)), 1.0, roughness);
  out_mat = vec4(normalize(dir), roughness);
}
code
   "\n\n"))

(define output-opaq-fragment-code
  (string-append
   depth-fragment-code
   #<<code
out vec4 out_color;

void output_opaq(vec3 color, float z) {
  gl_FragDepth = get_frag_depth(z);
  out_color = vec4(color, 1.0);
}
code
   "\n\n"))

(define output-tran-fragment-code
  (string-append
   depth-fragment-code
   #<<code
out vec4 out_color;
out float out_weight;

void output_tran(vec3 color, float a, float z) {
  //float weight = a * clamp(10/(1e-5 + pow(abs(z)/5,2) + pow(abs(z)/200,6)), 1e-2, 3e3);     // (7)
  float weight = max(1e-5, a * exp2(z));
  gl_FragDepth = get_frag_depth(z);
  out_color = vec4(color * a, a) * weight;
  out_weight = weight;
}
code
   "\n\n"))

(define light-fragment-code
  (string-append
   depth-fragment-code
   get-surface-fragment-code
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

vec3 attenuate_invsqr(vec3 light_color, float dist) {
  //return max(vec3(0.0), light_color/(dist*dist));
  return max(vec3(0.0), (light_color/(dist*dist) - 0.05) / 0.95);
}

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

out vec4 out_diffuse;
out vec4 out_specular;

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
   "\n\n"))
