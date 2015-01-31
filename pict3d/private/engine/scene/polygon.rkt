#lang typed/racket/base

;; Triangles, quads, rectangles

(require racket/unsafe/ops
         racket/list
         racket/vector
         racket/match
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         math/flonum
         "../../math/flv3.rkt"
         "../../math/flt3.rkt"
         "../../math/flrect3.rkt"
         "../../gl.rkt"
         "../../utils.rkt"
         "../types.rkt"
         "../utils.rkt"
         "../shader-lib.rkt"
         "../draw-pass.rkt"
         "types.rkt"
         "shader-lib.rkt")

(provide make-triangle-shape
         make-triangle-shape-passes
         triangle-shape-rect
         triangle-shape-transform
         
         make-quad-shapes
         
         make-rectangle-shape
         make-rectangle-shape-passes
         ;rectangle-shape-rect  ; already an accessor
         rectangle-shape-transform
         
         rectangle-shape->triangle-shapes
         )

;; ===================================================================================================
;; Constructors

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
                           Boolean
                           triangle-shape))
(define (make-triangle-shape vs ns cs es ms back?)
  (cond [(not (well-formed-flvectors? vs 3 3))
         (raise-argument-error 'make-triangle-shape
                               "length-3 flvector, or length-3 vector of length-3 flvectors"
                               0 vs ns cs es ms back?)]
        [(not (well-formed-flvectors? ns 3 3))
         (raise-argument-error 'make-triangle-shape
                               "length-3 flvector, or length-3 vector of length-3 flvectors"
                               1 vs ns cs es ms back?)]
        [(not (well-formed-flvectors? cs 3 4))
         (raise-argument-error 'make-triangle-shape
                               "length-4 flvector, or length-3 vector of length-4 flvectors"
                               2 vs ns cs es ms back?)]
        [(not (well-formed-flvectors? es 3 4))
         (raise-argument-error 'make-triangle-shape
                               "length-3 flvector, or length-3 vector of length-4 flvectors"
                               3 vs ns cs es ms back?)]
        [(not (or (material? ms) (= 3 (vector-length ms))))
         (raise-argument-error 'make-triangle-shape
                               "material, or length-3 vector of materials"
                               4 vs ns cs es ms back?)]
        [else
         (triangle-shape (box 'lazy) vs ns cs es ms back?)]))

(: take-triangle (All (A) (-> (Vectorof A) Index Index Index (Vectorof A))))
(define (take-triangle vs i1 i2 i3)
  (vector (vector-ref vs i1)
          (vector-ref vs i2)
          (vector-ref vs i3)))

(: make-quad-shapes (-> (Vectorof FlVector)
                        (U FlVector (Vectorof FlVector))
                        (U FlVector (Vectorof FlVector))
                        (U FlVector (Vectorof FlVector))
                        (U material (Vectorof material))
                        Boolean
                        (List triangle-shape triangle-shape)))
(define (make-quad-shapes vs ns cs es ms back?)
  (cond [(not (well-formed-flvectors? vs 4 3))
         (raise-argument-error 'make-quad-shape
                               "length-3 flvector, or length-4 vector of length-3 flvectors"
                               0 vs ns cs es ms back?)]
        [(not (well-formed-flvectors? ns 4 3))
         (raise-argument-error 'make-quad-shape
                               "length-3 flvector, or length-4 vector of length-3 flvectors"
                               1 vs ns cs es ms back?)]
        [(not (well-formed-flvectors? cs 4 4))
         (raise-argument-error 'make-quad-shape
                               "length-4 flvector, or length-4 vector of length-4 flvectors"
                               2 vs ns cs es ms back?)]
        [(not (well-formed-flvectors? es 4 4))
         (raise-argument-error 'make-quad-shape
                               "length-3 flvector, or length-4 vector of length-4 flvectors"
                               3 vs ns cs es ms back?)]
        [(not (or (material? ms) (= 4 (vector-length ms))))
         (raise-argument-error 'make-quad-shape
                               "material, or length-4 vector of materials"
                               4 vs ns cs es ms back?)]
        [else
         (list (triangle-shape (box 'lazy)
                               (take-triangle vs 0 1 2)
                               (if (vector? ns) (take-triangle ns 0 1 2) ns)
                               (if (vector? cs) (take-triangle cs 0 1 2) cs)
                               (if (vector? es) (take-triangle es 0 1 2) es)
                               (if (vector? ms) (take-triangle ms 0 1 2) ms)
                               back?)
               (triangle-shape (box 'lazy)
                               (take-triangle vs 2 3 0)
                               (if (vector? ns) (take-triangle ns 2 3 0) ns)
                               (if (vector? cs) (take-triangle cs 2 3 0) cs)
                               (if (vector? es) (take-triangle es 2 3 0) es)
                               (if (vector? ms) (take-triangle ms 2 3 0) ms)
                               back?))]))

(: make-rectangle-shape (-> Nonempty-FlRect3 FlVector FlVector material Boolean rectangle-shape))
(define (make-rectangle-shape b c e m back?)
  (cond [(not (= 4 (flvector-length c)))
         (raise-argument-error 'make-rectangle-shape "length-4 flvector" 1 b c e m back?)]
        [(not (= 4 (flvector-length e)))
         (raise-argument-error 'make-rectangle-shape "length-4 flvector" 2 b c e m back?)]
        [else
         (rectangle-shape (box 'lazy) b c e m back?)]))

;; ===================================================================================================
;; Program for pass 1: material

(define polygon-mat-vertex-code
  (string-append
   model-vertex-code
   #<<code
uniform mat4 view;
uniform mat4 unview;
uniform mat4 proj;

in vec4 vert_normal_roughness;
in vec3 vert_position;

smooth out vec4 frag_position;
smooth out vec3 frag_normal;
smooth out float frag_roughness;

void main() {
  mat4x3 model = get_model_transform();
  mat4x3 unmodel = affine_inverse(model);
  vec4 position = view * (a2p(model) * vec4(vert_position,1));
  gl_Position = proj * position;
  vec3 normal = normalize(vert_normal_roughness.xyz - vec3(127.0));
  vec4 norm = (vec4(normal,0) * a2p(unmodel)) * unview;
  frag_position = position;
  frag_normal = normalize(norm.xyz);
  frag_roughness = vert_normal_roughness.w / 255;
}
code
   ))

(define polygon-mat-fragment-code
  (string-append
   output-mat-fragment-code
   #<<code
smooth in vec4 frag_position;
smooth in vec3 frag_normal;
smooth in float frag_roughness;

void main() {
  output_mat(frag_normal, frag_roughness, frag_position.z);
}
code
   ))

(define-singleton/context (polygon-mat-program-spec)
  (log-pict3d-info "<engine> creating polygon material pass program")
  
  (define struct
    (make-vao-struct
     (make-vao-field "vert_normal_roughness" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_position" 3 GL_FLOAT)))
  
  (define program
    (make-gl-program
     struct
     (list "out_mat")
     (list (make-gl-shader GL_VERTEX_SHADER polygon-mat-vertex-code)
           (make-gl-shader GL_FRAGMENT_SHADER polygon-mat-fragment-code))))
  
  (define uniforms
    (list (cons "view" 'view)
          (cons "unview" 'unview)
          (cons "proj" 'proj)))
  
  (program-spec program uniforms))

;; ===================================================================================================
;; Program for pass 2: color

(define polygon-draw-vertex-code
  (string-append
   model-vertex-code
   rgb-hsv-code
   #<<code
uniform mat4 view;
uniform mat4 proj;

in vec4 vert_rcolor;    // vec4(r, g, b, a)
in vec4 vert_ecolor;    // vec4(r, g, b, intensity.hi)
in vec4 vert_material;  // vec4(ambient, diffuse, specular, intensity.lo)
in vec3 vert_position;

smooth out vec4 frag_position;
smooth out vec3 frag_rcolor;
smooth out vec3 frag_ecolor;
smooth out float frag_alpha;
smooth out float frag_ambient;
smooth out float frag_diffuse;
smooth out float frag_specular;

void main() {
  mat4x3 model = get_model_transform();
  vec4 position = view * (a2p(model) * vec4(vert_position,1));
  gl_Position = proj * position;
  frag_position = position;
  frag_rcolor = pow(vert_rcolor.rgb / 255, vec3(2.2));
  frag_alpha = vert_rcolor.a / 255;
  float intensity = (vert_ecolor.a * 256 + vert_material.w) / 255;
  frag_ecolor = pow(vert_ecolor.rgb / 255, vec3(2.2)) * intensity;
  frag_ambient = vert_material.x / 255;
  frag_diffuse = vert_material.y / 255;
  frag_specular = vert_material.z / 255;
}
code
   ))

(define polygon-opaq-fragment-code
  (string-append
   output-opaq-fragment-code
   #<<code
uniform vec3 ambient;
uniform sampler2D diffuse;
uniform sampler2D specular;

smooth in vec4 frag_position;
smooth in vec3 frag_rcolor;
smooth in vec3 frag_ecolor;
smooth in float frag_alpha;
smooth in float frag_ambient;
smooth in float frag_diffuse;
smooth in float frag_specular;

void main() {
  vec3 diff = texelFetch(diffuse, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 spec = texelFetch(specular, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
  vec3 color = frag_ecolor + frag_rcolor * light;
  output_opaq(color, frag_position.z);
}
code
   ))

(define polygon-tran-fragment-code
  (string-append
   output-tran-fragment-code
   #<<code
uniform vec3 ambient;
uniform sampler2D diffuse;
uniform sampler2D specular;

smooth in vec4 frag_position;
smooth in vec3 frag_rcolor;
smooth in vec3 frag_ecolor;
smooth in float frag_alpha;
smooth in float frag_ambient;
smooth in float frag_diffuse;
smooth in float frag_specular;

void main() {
  vec3 diff = texelFetch(diffuse, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 spec = texelFetch(specular, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
  vec3 color = frag_ecolor + frag_rcolor * light;
  output_tran(color, frag_alpha, frag_position.z);
}
code
   ))

(define-singleton/context (polygon-opaq-program-spec)
  (log-pict3d-info "<engine> creating polygon opaque color pass program")
  
  (define struct
    (make-vao-struct
     (make-vao-field "vert_rcolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_ecolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_material" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_position" 3 GL_FLOAT)))
  
  (define program
    (make-gl-program struct
                     (list "out_color")
                     (list (make-gl-shader GL_VERTEX_SHADER polygon-draw-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER polygon-opaq-fragment-code))))
  
  (define uniforms
    (list (cons "view" 'view)
          (cons "proj" 'proj)
          (cons "ambient" 'ambient)
          (cons "diffuse" 'diffuse)
          (cons "specular" 'specular)))
  
  (program-spec program uniforms))

(define-singleton/context (polygon-tran-program-spec)
  (log-pict3d-info "<engine> creating polygon transparent color pass program")
  
  (define struct
    (make-vao-struct
     (make-vao-field "vert_rcolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_ecolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_material" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_position" 3 GL_FLOAT)))
  
  (define program
    (make-gl-program struct
                     (list "out_color" "out_weight")
                     (list (make-gl-shader GL_VERTEX_SHADER polygon-draw-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER polygon-tran-fragment-code))))
  
  (define uniforms
    (list (cons "view" 'view)
          (cons "proj" 'proj)
          (cons "ambient" 'ambient)
          (cons "diffuse" 'diffuse)
          (cons "specular" 'specular)))
  
  (program-spec program uniforms))

;; ===================================================================================================
;; Triangle shape passes

(: make-triangle-shape-passes (-> triangle-shape Passes))
(define (make-triangle-shape-passes a)
  (match-define (triangle-shape _ vs ns orig-cs orig-es ms back?) a)
  (define-values (start stop step)
    (if back?
        (values 2 -1 -1)
        (values 0 3 1)))
  
  (define cs (if (vector? orig-cs) orig-cs (make-vector 3 orig-cs)))
  (define es (if (vector? orig-es) orig-es (make-vector 3 orig-es)))
  
  (define mat-struct-size
    (vao-struct-size (gl-program-struct (program-spec-program (polygon-mat-program-spec)))))
  (define mat-data-size (* 3 mat-struct-size))
  (define mat-data (make-bytes mat-data-size))
  (define mat-ptr (u8vector->cpointer mat-data))
  (for/fold ([i : Nonnegative-Fixnum  0]) ([j  (in-range start stop step)])
    (define v (vector-ref vs j))
    (define n (if (vector? ns) (vector-ref ns j) ns))
    (define m (if (vector? ms) (vector-ref ms j) ms))
    (let* ([i  (begin (bytes-copy! mat-data i (normal->rgb-bytes (if back? (flv3neg n) n)) 0 3)
                      (unsafe-fx+ i 3))]
           [i  (begin (bytes-set! mat-data i (flonum->byte (material-roughness m)))
                      (unsafe-fx+ i 1))]
           [i  (begin (memcpy mat-ptr i (f32vector->cpointer (flvector->f32vector v)) 12 _byte)
                      (unsafe-fx+ i 12))])
      i))
  
  (define opaq-struct-size
    (vao-struct-size (gl-program-struct (program-spec-program (polygon-opaq-program-spec)))))
  (define draw-data-size (* 3 opaq-struct-size))
  (define draw-data (make-bytes draw-data-size))
  (define draw-ptr (u8vector->cpointer draw-data))
  (for/fold ([i : Nonnegative-Fixnum  0]) ([j  (in-range start stop step)])
    (define v (vector-ref vs j))
    (define c (if (vector? cs) (vector-ref cs j) cs))
    (define e (if (vector? es) (vector-ref es j) es))
    (define m (if (vector? ms) (vector-ref ms j) ms))
    (define-values (ecolor i.lo) (pack-emitted e))
    (let* ([i  (begin (bytes-copy! draw-data i (pack-color c) 0 4)
                      (unsafe-fx+ i 4))]
           [i  (begin (bytes-copy! draw-data i ecolor 0 4)
                      (unsafe-fx+ i 4))]
           [i  (begin (bytes-set! draw-data i (flonum->byte (material-ambient m)))
                      (unsafe-fx+ i 1))]
           [i  (begin (bytes-set! draw-data i (flonum->byte (material-diffuse m)))
                      (unsafe-fx+ i 1))]
           [i  (begin (bytes-set! draw-data i (flonum->byte (material-specular m)))
                      (unsafe-fx+ i 1))]
           [i  (begin (bytes-set! draw-data i i.lo)
                      (unsafe-fx+ i 1))]
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
         (vector (shape-params polygon-mat-program-spec empty #f GL_TRIANGLES
                               (vertices 3 mat-data #f)))
         (vector (shape-params polygon-tran-program-spec empty #f GL_TRIANGLES
                               (vertices 3 draw-data #f))))
        (vector
         #()
         (vector (shape-params polygon-mat-program-spec empty #f GL_TRIANGLES
                               (vertices 3 mat-data #f)))
         (vector (shape-params polygon-opaq-program-spec empty #f GL_TRIANGLES
                               (vertices 3 draw-data #f)))
         #()
         #())))
  passes)

;; ===================================================================================================
;; Rectangle shape passes

(: make-rectangle-shape-passes (-> rectangle-shape Passes))
(define (make-rectangle-shape-passes a)
  (define as (rectangle-shape->triangle-shapes a))
  (define ps (map make-triangle-shape-passes as))
  (: new-ps Passes)
  (define new-ps (make-vector 5 #()))
  (for ([pass  (in-range 5)])
    (vector-set! new-ps pass
                 (apply vector-append (map (λ ([p : (Vectorof (Vectorof shape-params))])
                                             (vector-ref p pass))
                                           ps))))
  new-ps)

;; ===================================================================================================
;; Bounding box

(: triangle-shape-rect (-> triangle-shape Nonempty-FlRect3))
(define (triangle-shape-rect a)
  (assert (flv3rect (triangle-shape-vertices a)) nonempty-flrect3?))

;; rectangle-shape-rect is already an accessor

;; ===================================================================================================
;; Transform

(: triangle-shape-transform (-> triangle-shape FlAffine3- (List triangle-shape)))
(define (triangle-shape-transform a t)
  (match-define (triangle-shape passes vs ns cs es ms back?) a)
  (define consistent? (flt3consistent? t))
  (define transform-pos (λ ([v : FlVector]) (flt3apply/pos t v)))
  (define transform-norm (λ ([n : FlVector])
                           (if consistent?
                               (flt3apply/nrm t n)
                               (flv3neg (flt3apply/nrm t n)))))
  (list (triangle-shape (box 'lazy)
                        (vector-map transform-pos vs)
                        (if (vector? ns) (vector-map transform-norm ns) (transform-norm ns))
                        cs es ms
                        (if consistent? back? (not back?)))))

(: rectangle-shape-transform (-> rectangle-shape FlAffine3- (Listof triangle-shape)))
(define (rectangle-shape-transform a t)
  (append* (map (λ ([a : triangle-shape])
                  (triangle-shape-transform a t))
                (rectangle-shape->triangle-shapes a))))

;; ===================================================================================================
;; Conversions

(define vec-x- (flvector -1.0 0.0 0.0))
(define vec-x+ (flvector +1.0 0.0 0.0))
(define vec-y- (flvector 0.0 -1.0 0.0))
(define vec-y+ (flvector 0.0 +1.0 0.0))
(define vec-z- (flvector 0.0 0.0 -1.0))
(define vec-z+ (flvector 0.0 0.0 +1.0))

(: rectangle-shape->triangle-shapes (-> rectangle-shape (Listof triangle-shape)))
(define (rectangle-shape->triangle-shapes a)
  (match-define (rectangle-shape _ b c e m inside?) a)
  (define-values (xmin ymin zmin xmax ymax zmax) (flrect3-values b))
  (define v1 (flvector xmin ymin zmin))
  (define v2 (flvector xmax ymin zmin))
  (define v3 (flvector xmax ymax zmin))
  (define v4 (flvector xmin ymax zmin))
  (define v5 (flvector xmin ymin zmax))
  (define v6 (flvector xmax ymin zmax))
  (define v7 (flvector xmax ymax zmax))
  (define v8 (flvector xmin ymax zmax))
  (append*
   (list (make-quad-shapes (vector v4 v3 v2 v1) vec-z- c e m inside?)
         (make-quad-shapes (vector v5 v6 v7 v8) vec-z+ c e m inside?)
         (make-quad-shapes (vector v1 v2 v6 v5) vec-y- c e m inside?)
         (make-quad-shapes (vector v3 v4 v8 v7) vec-y+ c e m inside?)
         (make-quad-shapes (vector v4 v1 v5 v8) vec-x- c e m inside?)
         (make-quad-shapes (vector v2 v3 v7 v6) vec-x+ c e m inside?))))
