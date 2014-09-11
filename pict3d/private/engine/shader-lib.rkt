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

