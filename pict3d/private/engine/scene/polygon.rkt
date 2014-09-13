#lang typed/racket/base

;; Triangles, quads, rectangles

(require racket/unsafe/ops
         racket/list
         racket/vector
         racket/match
         typed/opengl
         (except-in typed/opengl/ffi -> cast)
         math/flonum
         "../../math/flt3.rkt"
         "../../math/flrect3.rkt"
         "../gl.rkt"
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
         
         make-quad-shape
         make-quad-shape-passes
         quad-shape-rect
         quad-shape-transform
         
         make-rectangle-shape
         make-rectangle-shape-passes
         ;rectangle-shape-rect  ; already an accessor
         rectangle-shape-transform
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
                           Face
                           triangle-shape))
(define (make-triangle-shape vs ns cs es ms face)
  (cond [(not (well-formed-flvectors? vs 3 3))
         (raise-argument-error 'make-triangle-shape
                               "length-3 flvector, or length-3 vector of length-3 flvectors"
                               0 vs ns cs es ms face)]
        [(not (well-formed-flvectors? ns 3 3))
         (raise-argument-error 'make-triangle-shape
                               "length-3 flvector, or length-3 vector of length-3 flvectors"
                               1 vs ns cs es ms face)]
        [(not (well-formed-flvectors? cs 3 4))
         (raise-argument-error 'make-triangle-shape
                               "length-4 flvector, or length-3 vector of length-4 flvectors"
                               2 vs ns cs es ms face)]
        [(not (well-formed-flvectors? es 3 3))
         (raise-argument-error 'make-triangle-shape
                               "length-3 flvector, or length-3 vector of length-3 flvectors"
                               3 vs ns cs es ms face)]
        [(not (or (material? ms) (= 3 (vector-length ms))))
         (raise-argument-error 'make-triangle-shape
                               "material, or length-3 vector of materials"
                               4 vs ns cs es ms face)]
        [else
         (triangle-shape (box 'lazy) vs ns cs es ms face)]))

(: make-quad-shape (-> (Vectorof FlVector)
                       (U FlVector (Vectorof FlVector))
                       (U FlVector (Vectorof FlVector))
                       (U FlVector (Vectorof FlVector))
                       (U material (Vectorof material))
                       Face
                       quad-shape))
(define (make-quad-shape vs ns cs es ms face)
  (cond [(not (well-formed-flvectors? vs 4 3))
         (raise-argument-error 'make-quad-shape
                               "length-3 flvector, or length-4 vector of length-3 flvectors"
                               0 vs ns cs es ms face)]
        [(not (well-formed-flvectors? ns 4 3))
         (raise-argument-error 'make-quad-shape
                               "length-3 flvector, or length-4 vector of length-3 flvectors"
                               1 vs ns cs es ms face)]
        [(not (well-formed-flvectors? cs 4 4))
         (raise-argument-error 'make-quad-shape
                               "length-4 flvector, or length-4 vector of length-4 flvectors"
                               2 vs ns cs es ms face)]
        [(not (well-formed-flvectors? es 4 3))
         (raise-argument-error 'make-quad-shape
                               "length-3 flvector, or length-4 vector of length-3 flvectors"
                               3 vs ns cs es ms face)]
        [(not (or (material? ms) (= 4 (vector-length ms))))
         (raise-argument-error 'make-quad-shape
                               "material, or length-4 vector of materials"
                               4 vs ns cs es ms face)]
        [else
         (quad-shape (box 'lazy) vs ns cs es ms face)]))

(: make-rectangle-shape (-> Nonempty-FlRect3 FlVector FlVector material Face rectangle-shape))
(define (make-rectangle-shape b c e m face)
  (cond [(not (= 4 (flvector-length c)))
         (raise-argument-error 'make-rectangle-shape "length-4 flvector" 1 b c e m face)]
        [(not (= 3 (flvector-length e)))
         (raise-argument-error 'make-rectangle-shape "length-3 flvector" 2 b c e m face)]
        [else
         (rectangle-shape (box 'lazy) b c e m face)]))

;; ===================================================================================================
;; Program for pass 1: material

(define polygon-mat-vertex-code
  (string-append
   "#version 130\n\n"
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
  vec3 normal = normalize(vert_normal_roughness.xyz - vec3(127.0/255.0));
  vec4 norm = (vec4(normal,0) * a2p(unmodel)) * unview;
  frag_position = position;
  frag_normal = normalize(norm.xyz);
  frag_roughness = vert_normal_roughness.w;
}
code
   ))

(define polygon-mat-fragment-code
  (string-append
   "#version 130\n\n"
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

(define-singleton (polygon-mat-program-spec)
  (define struct
    (make-vao-struct
     (make-vao-field "vert_normal_roughness" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_position" 3 GL_FLOAT)))
  
  (define program
    (make-gl-program
     struct
     (list (make-gl-shader GL_VERTEX_SHADER polygon-mat-vertex-code)
           (make-gl-shader GL_FRAGMENT_SHADER polygon-mat-fragment-code))))
  
  (define uniforms
    (list (cons "view" 'view)
          (cons "unview" 'unview)
          (cons "proj" 'proj)))
  
  (program-spec program uniforms struct))

;; ===================================================================================================
;; Program for pass 2: color

(define polygon-draw-vertex-code
  (string-append
   "#version 130\n\n"
   model-vertex-code
   rgb-hsv-code
   #<<code
uniform mat4 view;
uniform mat4 proj;

in vec4 vert_rcolor;
in vec4 vert_ecolor;    // vec4(hue, saturation, value.hi, value.lo)
in vec4 vert_material;  // vec4(ambient, diffuse, specular, 0)
in vec3 vert_position;

smooth out vec4 frag_position;
smooth out vec4 frag_rcolor;
smooth out vec3 frag_ecolor;
smooth out float frag_ambient;
smooth out float frag_diffuse;
smooth out float frag_specular;

void main() {
  mat4x3 model = get_model_transform();
  vec4 position = view * (a2p(model) * vec4(vert_position,1));
  gl_Position = proj * position;
  frag_position = position;
  frag_rcolor = vert_rcolor;
  frag_ecolor = hsv_to_rgb(vec3(vert_ecolor.xy, vert_ecolor.w + 256.0 * vert_ecolor.z));
  frag_ambient = vert_material.x;
  frag_diffuse = vert_material.y;
  frag_specular = vert_material.z;
}
code
   ))

(define polygon-opaq-fragment-code
  (string-append
   "#version 130\n\n"
   output-opaq-fragment-code
   #<<code
uniform vec3 ambient;
uniform sampler2D diffuse;
uniform sampler2D specular;

smooth in vec4 frag_position;
smooth in vec4 frag_rcolor;
smooth in vec3 frag_ecolor;
smooth in float frag_ambient;
smooth in float frag_diffuse;
smooth in float frag_specular;

void main() {
  vec3 diff = texelFetch(diffuse, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 spec = texelFetch(specular, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
  vec3 color = frag_ecolor + frag_rcolor.rgb * light;
  output_opaq(color, frag_rcolor.a, frag_position.z);
}
code
   ))

(define polygon-tran-fragment-code
  (string-append
   "#version 130\n\n"
   output-tran-fragment-code
   #<<code
uniform vec3 ambient;
uniform sampler2D diffuse;
uniform sampler2D specular;

smooth in vec4 frag_position;
smooth in vec4 frag_rcolor;
smooth in vec3 frag_ecolor;
smooth in float frag_ambient;
smooth in float frag_diffuse;
smooth in float frag_specular;

void main() {
  vec3 diff = texelFetch(diffuse, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 spec = texelFetch(specular, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 light = frag_ambient * ambient + frag_diffuse * diff + frag_specular * spec;
  vec3 color = frag_ecolor + frag_rcolor.rgb * light;
  output_tran(color, frag_rcolor.a, frag_position.z);
}
code
   ))

(define-singleton (polygon-opaq-program-spec)
  (define struct
    (make-vao-struct
     (make-vao-field "vert_rcolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_ecolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_material" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_position" 3 GL_FLOAT)))
  
  (define program
    (make-gl-program struct
                     (list (make-gl-shader GL_VERTEX_SHADER polygon-draw-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER polygon-opaq-fragment-code))))
  
  (define uniforms
    (list (cons "view" 'view)
          (cons "proj" 'proj)
          (cons "ambient" 'ambient)
          (cons "diffuse" 'diffuse)
          (cons "specular" 'specular)))
  
  (program-spec program uniforms struct))

(define-singleton (polygon-tran-program-spec)
  (define struct
    (make-vao-struct
     (make-vao-field "vert_rcolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_ecolor" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_material" 4 GL_UNSIGNED_BYTE)
     (make-vao-field "vert_position" 3 GL_FLOAT)))
  
  (define program
    (make-gl-program struct
                     (list (make-gl-shader GL_VERTEX_SHADER polygon-draw-vertex-code)
                           (make-gl-shader GL_FRAGMENT_SHADER polygon-tran-fragment-code))))
  
  (define uniforms
    (list (cons "view" 'view)
          (cons "proj" 'proj)
          (cons "diffuse" 'diffuse)
          (cons "specular" 'specular)))
  
  (program-spec program uniforms struct))

;; ===================================================================================================
;; Triangle and quad shape passes

(: make-polygon-shape-passes (-> Integer
                                 Index
                                 (Vectorof FlVector)
                                 (U FlVector (Vectorof FlVector))
                                 (U FlVector (Vectorof FlVector))
                                 (U FlVector (Vectorof FlVector))
                                 (U material (Vectorof material))
                                 Face
                                 Passes))
(define (make-polygon-shape-passes mode len vs ns cs es ms face)
  (define mat-data-size (* len (vao-struct-size (program-spec-struct (polygon-mat-program-spec)))))
  (define mat-data (make-bytes mat-data-size))
  (define mat-ptr (u8vector->cpointer mat-data))
  (for/fold ([i : Nonnegative-Fixnum  0]
             ) ([v  (in-vector vs)]
                [n  (in-vector (if (vector? ns) ns (make-vector len ns)))]
                [m  (in-vector (if (vector? ms) ms (make-vector len ms)))])
    (let* ([i  (begin (bytes-copy! mat-data i (normal->rgb-bytes n) 0 3)
                      (unsafe-fx+ i 3))]
           [i  (begin (bytes-set! mat-data i (flonum->byte (material-roughness m)))
                      (unsafe-fx+ i 1))]
           [i  (begin (memcpy mat-ptr i (f32vector->cpointer (flvector->f32vector v)) 12 _byte)
                      (unsafe-fx+ i 12))])
      i))
  
  (define draw-data-size (* len (vao-struct-size (program-spec-struct (polygon-opaq-program-spec)))))
  (define draw-data (make-bytes draw-data-size))
  (define draw-ptr (u8vector->cpointer draw-data))
  (for/fold ([i : Nonnegative-Fixnum  0]
             ) ([c  (in-vector (if (vector? cs) cs (make-vector len cs)))]
                [e  (in-vector (if (vector? es) es (make-vector len es)))]
                [m  (in-vector (if (vector? ms) ms (make-vector len ms)))]
                [v  (in-vector vs)])
    (let* ([i  (begin (bytes-copy! draw-data i (pack-color c) 0 4)
                      (unsafe-fx+ i 4))]
           [i  (begin (bytes-copy! draw-data i (pack-emitted e) 0 4)
                      (unsafe-fx+ i 4))]
           [i  (begin (bytes-set! draw-data i (flonum->byte (material-ambient m)))
                      (unsafe-fx+ i 1))]
           [i  (begin (bytes-set! draw-data i (flonum->byte (material-diffuse m)))
                      (unsafe-fx+ i 1))]
           [i  (begin (bytes-set! draw-data i (flonum->byte (material-specular m)))
                      ;; last byte is unused
                      (unsafe-fx+ i 2))]
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
         (vector (shape-params polygon-mat-program-spec empty face mode
                               (single-vertices len mat-data)))
         (vector (shape-params polygon-tran-program-spec empty face mode
                               (single-vertices len draw-data))))
        (vector
         #()
         (vector (shape-params polygon-mat-program-spec empty face mode
                               (single-vertices len mat-data)))
         (vector (shape-params polygon-opaq-program-spec empty face mode
                               (single-vertices len draw-data)))
         #()
         #())))
  passes)

(: make-triangle-shape-passes (-> triangle-shape Passes))
(define (make-triangle-shape-passes a)
  (match-define (triangle-shape _ vs ns cs es ms face) a)
  (make-polygon-shape-passes GL_TRIANGLES 3 vs ns cs es ms face))

(: make-quad-shape-passes (-> quad-shape Passes))
(define (make-quad-shape-passes a)
  (match-define (quad-shape _ vs ns cs es ms face) a)
  (make-polygon-shape-passes GL_QUADS 4 vs ns cs es ms face))

;; ===================================================================================================
;; Rectangle shape passes

(: make-rectangle-shape-passes (-> rectangle-shape Passes))
(define (make-rectangle-shape-passes a)
  (match-define (rectangle-shape _ rect c e m face) a)
  
  (define-values (xmin ymin zmin xmax ymax zmax) (flrect3-values rect))
  
  (define v1 (f32vector xmin ymin zmin))
  (define v2 (f32vector xmax ymin zmin))
  (define v3 (f32vector xmax ymax zmin))
  (define v4 (f32vector xmin ymax zmin))
  (define v5 (f32vector xmin ymin zmax))
  (define v6 (f32vector xmax ymin zmax))
  (define v7 (f32vector xmax ymax zmax))
  (define v8 (f32vector xmin ymax zmax))
  (define nleft  (normal->rgb-bytes (flvector -1.0 0.0 0.0)))
  (define nright (normal->rgb-bytes (flvector +1.0 0.0 0.0)))
  (define nfront (normal->rgb-bytes (flvector 0.0 -1.0 0.0)))
  (define nback  (normal->rgb-bytes (flvector 0.0 +1.0 0.0)))
  (define nbot   (normal->rgb-bytes (flvector 0.0 0.0 -1.0)))
  (define ntop   (normal->rgb-bytes (flvector 0.0 0.0 +1.0)))
  
  (define vs (vector v4 v3 v2 v1
                     v5 v6 v7 v8
                     v1 v2 v6 v5
                     v3 v4 v8 v7
                     v4 v1 v5 v8
                     v2 v3 v7 v6))
  (define ns (vector nbot nbot nbot nbot
                     ntop ntop ntop ntop
                     nfront nfront nfront nfront
                     nback nback nback nback
                     nleft nleft nleft nleft
                     nright nright nright nright))
  
  (define r (flonum->byte (material-roughness m)))
  
  (define mat-data-size (* 24 (vao-struct-size (program-spec-struct (polygon-mat-program-spec)))))
  (define mat-data (make-bytes mat-data-size))
  (define mat-ptr (u8vector->cpointer mat-data))
  (for/fold ([i : Nonnegative-Fixnum  0]
             ) ([v  (in-vector vs)]
                [n  (in-vector ns)])
    (let* ([i  (begin (bytes-copy! mat-data i n 0 3)
                      (unsafe-fx+ i 3))]
           [i  (begin (bytes-set! mat-data i r)
                      (unsafe-fx+ i 1))]
           [i  (begin (memcpy mat-ptr i (f32vector->cpointer v) 12 _byte)
                      (unsafe-fx+ i 12))])
      i))
  
  (define material-data-size
    (assert (- (vao-struct-size (program-spec-struct (polygon-opaq-program-spec))) 12) positive?))
  (define material-data (make-bytes material-data-size))
  (define material-ptr (u8vector->cpointer material-data))
  (let* ([i  (begin (bytes-copy! material-data 0 (pack-color c) 0 4)
                    4)]
         [i  (begin (bytes-copy! material-data i (pack-emitted e) 0 4)
                    (unsafe-fx+ i 4))]
         [i  (begin (bytes-set! material-data i (flonum->byte (material-ambient m)))
                    (unsafe-fx+ i 1))]
         [i  (begin (bytes-set! material-data i (flonum->byte (material-diffuse m)))
                    (unsafe-fx+ i 1))]
         [i  (begin (bytes-set! material-data i (flonum->byte (material-specular m)))
                    (unsafe-fx+ i 1))])
    (void))
  
  (define draw-data-size (* 24 (vao-struct-size (program-spec-struct (polygon-opaq-program-spec)))))
  (define draw-data (make-bytes draw-data-size))
  (define draw-ptr (u8vector->cpointer draw-data))
  (for/fold ([i : Nonnegative-Fixnum  0]) ([v  (in-vector vs)])
    (let* ([i  (begin (bytes-copy! draw-data i material-data 0 material-data-size)
                      (unsafe-fx+ i material-data-size))]
           [i  (begin (memcpy draw-ptr i (f32vector->cpointer v) 12 _byte)
                      (unsafe-fx+ i 12))])
      i))
  
  (define transparent? (< (flvector-ref c 3) 1.0))
  
  (: passes Passes)
  (define passes
    (if transparent?
        (vector
         #()
         #()
         #()
         (vector (shape-params polygon-mat-program-spec empty face GL_QUADS
                               (single-vertices 24 mat-data)))
         (vector (shape-params polygon-tran-program-spec empty face GL_QUADS
                               (single-vertices 24 draw-data))))
        (vector
         #()
         (vector (shape-params polygon-mat-program-spec empty face GL_QUADS
                               (single-vertices 24 mat-data)))
         (vector (shape-params polygon-opaq-program-spec empty face GL_QUADS
                               (single-vertices 24 draw-data)))
         #()
         #())))
  passes)

;; ===================================================================================================
;; Bounding box

(: triangle-shape-rect (-> triangle-shape Nonempty-FlRect3))
(define (triangle-shape-rect a)
  (assert (flv3rect (triangle-shape-vertices a)) nonempty-flrect3?))

(: quad-shape-rect (-> quad-shape Nonempty-FlRect3))
(define (quad-shape-rect a)
  (assert (flv3rect (quad-shape-vertices a)) nonempty-flrect3?))

;; rectangle-shape-rect is already an accessor

;; ===================================================================================================
;; Transform

(: triangle-shape-transform (-> triangle-shape FlAffine3- FlAffine3- (List triangle-shape)))
(define (triangle-shape-transform a t tinv)
  (match-define (triangle-shape passes vs ns cs es ms face) a)
  (define new-vs (vector-map (λ ([v : FlVector]) (flt3apply/pos t v)) vs))
  (define new-ns (if (vector? ns)
                     (vector-map (λ ([n : FlVector]) (flt3apply/norm tinv n)) ns)
                     (flt3apply/norm tinv ns)))
  (define new-face (if (flt3consistent? t) face (opposite-gl-face face)))
  (list (triangle-shape (box 'lazy) new-vs new-ns cs es ms new-face)))

(: quad-shape-transform (-> quad-shape FlAffine3- FlAffine3- (List quad-shape)))
(define (quad-shape-transform a t tinv)
  (match-define (quad-shape passes vs ns cs es ms face) a)
  (define new-vs (vector-map (λ ([v : FlVector]) (flt3apply/pos t v)) vs))
  (define new-ns (if (vector? ns)
                     (vector-map (λ ([n : FlVector]) (flt3apply/norm tinv n)) ns)
                     (flt3apply/norm tinv ns)))
  (define new-face (if (flt3consistent? t) face (opposite-gl-face face)))
  (list (quad-shape (box 'lazy) new-vs new-ns cs es ms new-face)))

(: rectangle-shape-transform (-> rectangle-shape FlAffine3- FlAffine3-
                                 (List quad-shape quad-shape
                                       quad-shape quad-shape
                                       quad-shape quad-shape)))
(define (rectangle-shape-transform a t tinv)
  (match-define (rectangle-shape passes b c e m old-face) a)
  (define-values (xmin ymin zmin xmax ymax zmax) (flrect3-values b))
  
  (define face (if (flt3consistent? t) old-face (opposite-gl-face old-face)))
  
  (define v1 (flt3apply/pos t (flvector xmin ymin zmin)))
  (define v2 (flt3apply/pos t (flvector xmax ymin zmin)))
  (define v3 (flt3apply/pos t (flvector xmax ymax zmin)))
  (define v4 (flt3apply/pos t (flvector xmin ymax zmin)))
  (define v5 (flt3apply/pos t (flvector xmin ymin zmax)))
  (define v6 (flt3apply/pos t (flvector xmax ymin zmax)))
  (define v7 (flt3apply/pos t (flvector xmax ymax zmax)))
  (define v8 (flt3apply/pos t (flvector xmin ymax zmax)))
  (define nleft  (flt3apply/norm tinv (flvector -1.0 0.0 0.0)))
  (define nright (flt3apply/norm tinv (flvector +1.0 0.0 0.0)))
  (define nfront (flt3apply/norm tinv (flvector 0.0 -1.0 0.0)))
  (define nback  (flt3apply/norm tinv (flvector 0.0 +1.0 0.0)))
  (define nbot   (flt3apply/norm tinv (flvector 0.0 0.0 -1.0)))
  (define ntop   (flt3apply/norm tinv (flvector 0.0 0.0 +1.0)))
  
  (list (make-quad-shape (vector v4 v3 v2 v1) nbot   c e m face)
        (make-quad-shape (vector v5 v6 v7 v8) ntop   c e m face)
        (make-quad-shape (vector v1 v2 v6 v5) nfront c e m face)
        (make-quad-shape (vector v3 v4 v8 v7) nback  c e m face)
        (make-quad-shape (vector v4 v1 v5 v8) nleft  c e m face)
        (make-quad-shape (vector v2 v3 v7 v6) nright c e m face)))
