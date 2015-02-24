#lang typed/racket/base

;; Point light impostors: vertex shader outputs a quad; fragment shader computes light

(require racket/match
         racket/list
         racket/flonum
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         "../../math/flv3.rkt"
         "../../math/flt3.rkt"
         "../../math/flrect3.rkt"
         "../../gl.rkt"
         "../../utils.rkt"
         "../draw-pass.rkt"
         "../shader-lib.rkt"
         "../types.rkt"
         "../utils.rkt"
         "types.rkt"
         "flags.rkt"
         "shader-lib.rkt")

(provide make-point-light-shape
         make-point-light-shape-passes
         set-point-light-shape-emitted
         point-light-shape-rect
         point-light-shape-easy-transform
         )

;; ===================================================================================================
;; Constructor

(: make-point-light-shape (-> FlVector FlVector Flonum Flonum point-light-shape))
(define (make-point-light-shape e v r0 r1)
  (cond [(not (= 4 (flvector-length e)))
         (raise-argument-error 'make-point-light-shape "length-4 flvector"
                               0 e v r0 r1)]
        [(not (= 3 (flvector-length v)))
         (raise-argument-error 'make-point-light-shape "length-3 flvector"
                               1 e v r0 r1)]
        [else
         (define fs (flags-join light-flag transparent-flag (color-emitting-flag e)))
         (point-light-shape (lazy-passes) fs e v r0 r1)]))

;; ===================================================================================================
;; Set attributes

(: set-point-light-shape-emitted (-> point-light-shape FlVector point-light-shape))
(define (set-point-light-shape-emitted a e)
  (cond [(not (= 4 (flvector-length e)))
         (raise-argument-error 'set-point-light-shape-emitted "length-4 flvector"
                               1 a e)]
        [else
         (match-define (point-light-shape _ fs old-e v r0 r1) a)
         (cond [(equal? old-e e)  a]
               [else
                (define new-fs (flags-join (flags-subtract fs emitting-flags)
                                           (color-emitting-flag e)))
                (point-light-shape (lazy-passes) new-fs e v r0 r1)])]))

;; ===================================================================================================
;; Program for pass 0: light

(define point-light-vertex-code
  (string-append
   output-impostor-strip-vertex-code
   model-vertex-code
   #<<code
uniform mat4 view;
uniform mat4 proj;
uniform mat4 unproj;

in vec3 vert_position;
in vec3 vert_intensity_radii;
in vec3 vert_color;
in float vert_id;

flat out vec3 frag_position;
flat out float frag_min_radius;
flat out float frag_max_radius;
flat out vec3 frag_intensity;
smooth out float frag_is_degenerate;
smooth out vec3 frag_dir;

void main() {
  mat4x3 model = get_model_transform();
  mat4 trans = view * a2p(model);

  float min_radius = vert_intensity_radii.y;
  float max_radius = vert_intensity_radii.z;
  vec3 wmin = vert_position - vec3(max_radius);
  vec3 wmax = vert_position + vec3(max_radius);
  frag_position = (trans * vec4(vert_position,1)).xyz;
  frag_min_radius = min_radius;
  frag_max_radius = max_radius;
  frag_intensity = pow(vert_color / 255, vec3(2.2)) * vert_intensity_radii.x;
  frag_is_degenerate = output_impostor_strip(trans, proj, wmin, wmax, int(vert_id));

  vec4 dir = unproj * gl_Position;
  frag_dir = vec3(dir.xy / dir.z, 1.0);
}
code
   ))

(define point-light-fragment-code
  (string-append
   light-fragment-code
   #<<code
uniform sampler2D depth;
uniform sampler2D material;

flat in vec3 frag_position;
flat in float frag_min_radius;
flat in float frag_max_radius;
flat in vec3 frag_intensity;
smooth in float frag_is_degenerate;
smooth in vec3 frag_dir;

void main() {
  // all fragments should discard if this one does
  if (frag_is_degenerate > 0.0) discard;

  float d = texelFetch(depth, ivec2(gl_FragCoord.xy), 0).r;
  if (d == 0.0) discard;
  float z = get_view_depth(d);
  vec3 vpos = frag_dir * z;

  vec3 D = frag_position - vpos;
  float dist = length(D);
  if (dist < frag_min_radius) discard;
  if (dist > frag_max_radius) discard;  
  vec3 L = D / dist;
  vec3 V = normalize(-vpos);

  vec3 light = attenuate_invsqr(frag_intensity, dist);
  output_light(light, get_surface(material), L, V);
}
code
   ))

(define-singleton/context (point-light-program)
  (log-pict3d-info "<engine> creating point light program")
  (make-gl-program
   "point-light-program"
   (list (cons "view" 'view)
         (cons "proj" 'proj)
         (cons "unproj" 'unproj)
         (cons "depth" 'depth)
         (cons "material" 'material))
   (make-vao-struct
    (make-vao-field "vert_position" 3 GL_FLOAT)
    (make-vao-field "vert_intensity_radii" 3 GL_FLOAT)
    (make-vao-field "vert_color" 3 GL_UNSIGNED_BYTE)
    (make-vao-field "vert_id" 1 GL_UNSIGNED_BYTE))
   (list "out_diffuse" "out_specular")
   (list (make-gl-shader GL_VERTEX_SHADER point-light-vertex-code)
         (make-gl-shader GL_FRAGMENT_SHADER point-light-fragment-code))))

;; ===================================================================================================
;; Point light shape passes

(: vertex-ids (Vectorof Index))
(define vertex-ids #(0 1 2 2 1 3))

(: max-light-radius (-> Flonum Flonum))
(define (max-light-radius intensity)
  (flsqrt (* 20.0 intensity)))

(: make-point-light-shape-passes (-> point-light-shape passes))
(define (make-point-light-shape-passes a)
  (match-define (point-light-shape _ fs e v r0 r1) a)
  
  (cond
    [(flags-subset? emitting-flag fs)
     (define color (flvector-copy e 0 3))
     (define intensity (flvector-ref e 3))
     (define radius (max-light-radius intensity))
     (define data
       (gl-data->bytes
        ((inst append* (U F32Vector Bytes))
         (for/list ([id  (in-range 4)])
           (list (flvector->f32vector v)
                 (f32vector intensity (* radius r0) (* radius r1))
                 (pack-color color)
                 (bytes id))))))
     
     (passes
      (vector (shape-params point-light-program empty #t GL_TRIANGLES (vertices 4 data vertex-ids)))
      #()
      #()
      #()
      #())]
    [else  empty-passes]))

;; ===================================================================================================
;; Bounding box

(: point-light-shape-rect (-> point-light-shape Nonempty-FlRect3))
(define (point-light-shape-rect a)
  (define p (point-light-shape-position a))
  (define intensity (flvector-ref (light-shape-emitted a) 3))
  (define radius (* (max-light-radius intensity) (point-light-shape-max-radius a)))
  (define r (flvector radius radius radius))
  (nonempty-flrect3 (flv3- p r) (flv3+ p r)))

;; ===================================================================================================
;; Transform

(: point-light-shape-easy-transform (-> point-light-shape Affine point-light-shape))
(define (point-light-shape-easy-transform a t)
  (match-define (point-light-shape passes fs e v r0 r1) a)
  (define new-v (flt3apply/pos (affine-transform t) v))
  (point-light-shape (lazy-passes) fs e new-v r0 r1))
