#lang typed/racket/base

(provide (all-defined-out))

(define matrix-code
  (string-append
   #<<code
mat4x3 rows2mat4x3(vec4 row0, vec4 row1, vec4 row2) {
  return transpose(mat3x4(row0, row1, row2));
}

mat4 a2p(mat4x3 m) {
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

(define rgb-hsv-code
  (string-append
   #<<code
vec3 rgb_to_hsv(vec3 c) {
  vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
  vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
  vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));
  
  float d = q.x - min(q.w, q.y);
  float e = 1.0e-10;
  return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

vec3 hsv_to_rgb(vec3 c) {
  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}
code
   "\n\n"))

(define rgb-yuv-code
  (string-append
   #<<code
const mat3 rgb_to_yuv = transpose(
  mat3(+0.299000, +0.587000, +0.114000,
       -0.168736, -0.331264, +0.500000,
       +0.500000, -0.418688, -0.081312));

const mat3 yuv_to_rgb = transpose(
  mat3(1.0, -0.000000, +1.402000,
       1.0, -0.344136, -0.714136,
       1.0, +1.772000, +0.000000));
code
   "\n\n"))

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
float frag_depth(float znear, float zfar, float z) {
  return log2(-z/zfar) / log2(znear/zfar);
}

float unfrag_depth(float znear, float zfar, float logz) {
  return -exp2(logz * log2(znear/zfar)) * zfar;
}
code
   "\n\n"))

(define get-view-position-fragment-code
  (string-append
   depth-fragment-code
   #<<code
vec3 get_view_position(sampler2D depthTex, int width, int height, mat3 unproj0, mat3 unproj1,
                       float znear, float zfar) {
  // compute view z from depth buffer
  float depth = texelFetch(depthTex, ivec2(gl_FragCoord.xy), 0).r;
  if (depth == 0.0) discard;
  float z = unfrag_depth(znear, zfar, depth);
  
  // clip xy
  vec3 cpos = vec3((gl_FragCoord.xy / vec2(width,height) - vec2(0.5)) * 2.0, 1.0);
  
  // compute view position from clip xy and view z
  vec3 p0 = unproj0 * cpos;
  vec3 p1 = unproj1 * cpos;
  return (p0*z+p1) / p0.z;
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
  return surface(unpack_normal(mat.rg), mat.a);
}
code
   "\n\n"))

(define impostor-bounds-code
  (string-append
   #<<code
struct aabb {
  vec3 mins;
  vec3 maxs;
  float is_degenerate;
};

aabb impostor_bounds(mat4 view, mat4 proj, vec3 wmin, vec3 wmax) {
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
  vec3 vmin = vec3(+1e39);  // closest 32-bit float is +Inf
  vec3 vmax = vec3(-1e39);
  
  // clip space min and max
  vec3 cmin = vec3(+1e39);
  vec3 cmax = vec3(-1e39);
  
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
  
  if (vmin.z > 0.0) return aabb(cmin, cmax, 1.0);

  // if we're inside it, we should draw on the whole screen
  if (max(vmin.x, max(vmin.y, vmin.z)) <= 0.0 &&
      min(vmax.x, min(vmax.y, vmax.z)) >= 0.0) {
    cmin = vec3(-1.0);
    cmax = vec3(+1.0);
  }

  return aabb(cmin, cmax, 0.0);
}
code
   "\n\n"))

(define output-impostor-strip-vertex-code
  (string-append
   impostor-bounds-code
   #<<code
float output_impostor_strip(mat4 view, mat4 proj, vec3 wmin, vec3 wmax) {
  aabb bbx = impostor_bounds(view, proj, wmin, wmax);

  // output the correct vertices for a triangle strip
  switch (gl_VertexID) {
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

  return bbx.is_degenerate;
}
code
   "\n\n"))

(define output-impostor-quad-vertex-code
  (string-append
   impostor-bounds-code
   #<<code
float output_impostor_quad(mat4 view, mat4 proj, vec3 wmin, vec3 wmax) {
  aabb bbx = impostor_bounds(view, proj, wmin, wmax);

  // output the correct vertices for a quad
  switch (gl_VertexID % 4) {
  case 0:
    gl_Position = vec4(bbx.mins.xy, 0.0, 1.0);
    break;
  case 1:
    gl_Position = vec4(bbx.maxs.x, bbx.mins.y, 0.0, 1.0);
    break;
  case 2:
    gl_Position = vec4(bbx.maxs.xy, 0.0, 1.0);
    break;
  default:
    gl_Position = vec4(bbx.mins.x, bbx.maxs.y, 0.0, 1.0);
    break;
  }

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
