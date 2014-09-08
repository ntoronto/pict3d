#lang racket

(require srfi/13)

; It would be really cool if we could do strict checking of
; the enums. And in fact the info appears to be available in enum.spec
; and I wrote the code to do it. But I quickly found out that enum.spec
; is too buggy to be useful for that purpose. 
; Eveything a C compiler doesn't check is likely to be unreliable in that file...
(define strongly-typed-enums #f)

(define-struct param-spec
               (name type mode shape size))

(define-struct function-spec
               (name
                (params #:mutable)
                (return #:mutable)
                (version #:mutable)
                (category #:mutable)
                (alias #:mutable)
                (deprecated #:mutable)))

(define-struct constant-spec
               (name
                (value #:mutable)))

(define-struct enum-spec
               (name
                 type
                 (constants #:mutable)))

(define-struct mode-dependent-type (in out))

(define type-to-vector
  (make-hash
    '(
      (_int8 . _s8vector)
      (_uint16 . _u16vector)
      (_int16 . _s16vector)
      (_uint32 . _u32vector)
      (_int32 . _s32vector)
      (_uint64 . _u64vector)
      (_int64 . _s64vector)
      (_float . _f32vector)
      (_double* . _f64vector))))

(define vector-to-contract
  (make-hash
    '(
      (_bytes . bytes?)
      (_s8vector . s8vector?)
      (_u16vector . u16vector?)
      (_s16vector . s16vector?)
      (_u32vector . u32vector?)
      (_s32vector . s32vector?)
      (_u64vector . u64vector?)
      (_s64vector . s64vector?)
      (_f32vector . f32vector?)
      (_f64vector . f64vector?))))

(define flat-to-contract
  (make-hash
   '(
     (_void . void?)
     (_int8 . (integer-in -128 127))
     (_uint8 . (integer-in 0 255))
     (_int16 . (integer-in -32768 32767))
     (_uint16 . (integer-in 0 65535))
     (_int32 . exact-integer?)
     (_intptr . exact-integer?)
     (_int64 . exact-integer?)
     (_uint32 . exact-nonnegative-integer?)
     (_uint64 . exact-nonnegative-integer?)
     (_GLsync . GLsync?)
     (_float . flonum?)
     (_double* . real?)
     (_bool . boolean?)
     (_pointer . cpointer?)
     (_pointer/intptr . gl-pointer?)
     (_string/utf-8 . string?)
     (_string*/utf-8 . (or/c bytes? string?)))))

(define vector-to-TR-type
  (make-hash
    '(
      (_bytes . Bytes)
      (_s8vector . S8Vector)
      (_u16vector . U16Vector)
      (_s16vector . S16Vector)
      (_u32vector . U32Vector)
      (_s32vector . S32Vector)
      (_u64vector . U64Vector)
      (_s64vector . S64Vector)
      (_f32vector . F32Vector)
      (_f64vector . F64Vector))))

(define flat-to-TR-type
  (make-hash
   '(
     (_void . Void)
     (_int8 . Fixnum)
     (_uint8 . Byte)
     (_int16 . Fixnum)
     (_uint16 . Nonnegative-Fixnum)
     (_int32 . Integer)
     (_intptr . Integer)
     (_int64 . Integer)
     (_uint32 . Natural)
     (_uint64 . Natural)
     (_GLsync . GLSync)
     (_float . Flonum)
     (_double* . Real)
     (_bool . Boolean)
     (_pointer . CPointer)
     (_pointer/intptr . GLPointer)
     (_string/utf-8 . String)
     (_string*/utf-8 . (U Bytes String)))))

(define (pointer-to type . args)
  (if (and (equal? args '(1)) (not (eq? type '_void)))
    (mode-dependent-type
      `(_ptr i ,type) `(_ptr o ,type))
    (case type
      ((_void) '_pointer/intptr)
      ((_byte _uint8) (mode-dependent-type 
                        '_string*/utf-8 
                        (if (null? args)
                          '_bytes
                          `(_bytes o ,@ args))))
      (else
        (let ((vt (hash-ref type-to-vector type #f)))
          (if vt
            (mode-dependent-type
              `(,vt i)
              (if (null? args) 
                vt
                `(,vt o ,@ args))) 
            (mode-dependent-type
              `(_vector i ,type)
              (if (null? args) 
                '_pointer
                `(_vector o ,type ,@ args)))))))))


(define basic-type-map
  (make-hash
    (list
      (cons "GLshort" '_int16)
      (cons "GLvoid" '_void)
      (cons "const GLubyte *" (pointer-to '_uint8))
      (cons "GLsync" '_GLsync)
      (cons "GLhandleARB" '_uint32)
      (cons "GLboolean" '_bool)
      (cons "struct _cl_event *" '_pointer)
      (cons "GLint64EXT" '_int64)
      (cons "GLsizeiptrARB" '_intptr)
      ;    (cons "GLDEBUGPROCARB" _GLDEBUGPROCARB)
      (cons "GLenum" '_int32)
      (cons "GLint" '_int32)
      (cons "GLclampd" '_double*)
      (cons "GLvoid*" (pointer-to '_void))
      (cons "GLhalfNV"'_uint16) ; A 16-bit floating point number. You get the bits, good luck. ;-)
      ;    (cons "_GLfuncptr" __GLfuncptr)
      (cons "GLubyte" '_uint8)
      ;    (cons "GLvdpauSurfaceNV" _GLvdpauSurfaceNV)
      (cons "GLcharARB*" (pointer-to '_byte))
      (cons "GLdouble*" (pointer-to '_double*))
      (cons "struct _cl_context *" '_pointer)
      (cons "GLcharARB" '_byte)
      (cons "GLfloat" '_float)
      (cons "GLuint64" '_uint64)
      (cons "GLbyte" '_int8)
      (cons "GLbitfield" '_uint32)
      (cons "GLuint64EXT" '_uint64)
      (cons "GLchar*" (pointer-to '_byte))
      (cons "GLchar* const" (pointer-to '_byte))
      (cons "GLsizeiptr" '_intptr)
      (cons "GLchar" '_byte)
      ;    (cons "GLUquadric*" _GLUquadric*)
      (cons "GLdouble" '_double*)
      (cons "GLintptr" '_intptr)
      ;    (cons "GLUtesselator*" _GLUtesselator*)
      (cons "GLsizei" '_int32)
      (cons "GLvoid* const" (pointer-to '_void))
      ;    (cons "GLDEBUGPROCAMD" _GLDEBUGPROCAMD)
      (cons "GLboolean*" (pointer-to '_bool))
      (cons "GLint64" '_int64)
      (cons "GLintptrARB" '_intptr)
      ;    (cons "GLUnurbs*" _GLUnurbs*)
      (cons "GLuint" '_uint32)
      (cons "GLclampf" '_float)
      (cons "GLushort" '_uint16)
      (cons "GLfloat*" (pointer-to '_float)))))

(define (read-type-map input-port)
  (let ((result '()))

    (for ((l (in-lines input-port)))
         (cond
           ((regexp-match #px"^#" l) => void)
           (else
             (let ((lst (regexp-split #px"\\s*,\\s*" l)))
               (set! result
                 (cons (cons (list-ref lst 0) (list-ref lst 3))
                       result))))))

    result))

(define type-map (make-hash (call-with-input-file "specfiles/gl.tm" read-type-map)))
(hash-set! type-map "void" "GLvoid")

(define (print-enum spec)
  (let ((name (enum-spec-name spec))
        (vals (for/list ((cs (in-list (enum-spec-constants spec)))
                         #:when (constant-spec-value cs))
                        (string->symbol (format "GL_~a" (constant-spec-name cs))))))
    (printf "(define-enum gl~a? ~s)~%"
            name
            vals)))

(define (print-bitfield spec)
  (let ((name (enum-spec-name spec))
        (vals (for/list ((cs (in-list (enum-spec-constants spec)))
                         #:when (constant-spec-value cs))
                        (string->symbol (format "GL_~a" (constant-spec-name cs))))))
    (printf "(define-bitfield gl~a? ~s)~%"
            name
            vals)))

(define (get-extension-url extension)
  (let* ((mo (regexp-match  #rx"^([0-9A-Z]+)_(.*)$" extension))
         (prefix (list-ref mo 1))
         (base (list-ref mo 2)))
    (format "http://www.opengl.org/registry/specs/~a/~a.txt"
            prefix base)))

(define (read-manpages input-port)
  (for/set ((l (in-lines input-port)))
           (string-trim l)))

(define manpages (call-with-input-file "specfiles/manpages.txt" read-manpages))
(define manpages4 (call-with-input-file "specfiles/manpages4.txt" read-manpages))

(define (get-manpage-helper name)
  (cond
    ((set-member? manpages4 name)
     (list
       (format "http://www.opengl.org/sdk/docs/man4/xhtml/gl~a.xml" name)
       name))
    ((set-member? manpages name)
     (list
       (format "http://www.opengl.org/sdk/docs/man2/xhtml/gl~a.xml" name)
       name))
    (else #f)))

(define (get-manpage name)
  (cond
    ((get-manpage-helper name) => values)
    ((regexp-match #rx"^(.*?[^0-9])[1-4]?(u?[bsi]|[fd])v?$" name)
     =>
     (λ (m) (get-manpage-helper (list-ref m 1))))
    (else #f)))

;; Convert a hash to a list in a deterministic order.
(define (hash->sorted-list h)
  (sort (hash->list h)
        string<?
        #:key (λ (p) (symbol->string (car p)))
        #:cache-keys? #t))


(define (read-enums input-port)

  (let ((visited (set))
        (pname-map (make-hasheqv))
        (constants (make-hash))
        (enums (make-hash))
        (current-enum #f))

    (define (get-constant-spec name)
      (hash-ref constants name
                (lambda ()
                  (let ((new-spec (constant-spec name #f)))
                    (hash-set! constants name new-spec)
                    new-spec))))

    (define (define-enum name)
      (let ((spec (hash-ref enums name
                            (lambda ()
                              (let ((type (hash-ref type-map name #f)))
                                (if (or (equal? type "GLenum") (equal? type "GLbitfield"))
                                  (let ((new-spec (enum-spec name type '())))
                                    (hash-set! enums name new-spec)
                                    new-spec)
                                  #f))))))
        (set! current-enum spec)))

    (define (add-to-current-enum cs)
      (when current-enum
        (set-enum-spec-constants! current-enum (cons cs (enum-spec-constants current-enum)))))


    (define (parse-pname name line)
      (cond
        ((regexp-match #px"#\\s+([0-9]+)\\s+" line)
         => (lambda (m) (hash-set! pname-map (string->symbol (format "GL_~a" name))
                                   (string->number (list-ref m 1)))))))

    (define (define-constant name value line)
      (unless (set-member? visited name)
        (printf "(define-const GL_~a ~a)~%" name value)
        (set! visited (set-add visited name))
        (let ((cs (get-constant-spec name)))
          (set-constant-spec-value! cs value)
          (add-to-current-enum cs))
        (parse-pname name line)))


    (for ((l (in-lines input-port)))
         (cond
           ((regexp-match #px"^\\s*([a-zA-Z0-9_]+)\\s+enum:" l)
            =>
            (lambda (m)
              (define-enum (list-ref m 1))))

           ((regexp-match #px"^\\s*use\\s+([a-zA-Z0-9_]+)\\s+([a-zA-Z0-9_]+)" l)
            =>
            (lambda (m)
              (add-to-current-enum (get-constant-spec (list-ref m 2)))))
 
           ((regexp-match #px"^\\s+([a-zA-Z0-9_]+)\\s*=\\s*([0-9]+)\\s*(#.*)?$" l)
            =>
            (lambda (m)
              (define-constant (list-ref m 1) (list-ref m 2) l)))

           ((regexp-match #px"^\\s+([a-zA-Z0-9_]+)\\s*=\\s*0[xX]([0-9a-fA-F]+)\\s*(#.*)?$" l)
            =>
            (lambda (m)
              (define-constant (list-ref m 1) (string-append "#x" (list-ref m 2)) l)))))

    (printf "~s~%"
            `(define pname-map (make-hasheqv
                                (list ,@(for*/list ((p (in-list (hash->sorted-list pname-map)))
                                                    (k (in-value (car p)))
                                                    (v (in-value (cdr p)))
                                                    #:when (not (= v 1)))
                                          `(cons ,k ,v))))))

    (when strongly-typed-enums
      (for (((k v) (in-hash enums)))
           (let ((type (enum-spec-type v)))
             (cond
               ((equal? type "GLenum") (print-enum v))
               ((equal? type "GLbitfield") (print-bitfield v))))))

    enums))


(define (read-function-specs input-port)
  (let ((result '())
        (current-function-spec #f))

    (define (new-function m)
      (let* ((name (list-ref m 1))
             (spec (function-spec name '() #f #f #f #f #f)))
        (set! result (cons spec result))
        (set! current-function-spec spec)))

    (define (handle-param m)
      (let ((spec (apply param-spec (cdr m))))
        (set-function-spec-params! current-function-spec
                                   (append
                                     (function-spec-params current-function-spec)
                                     (list spec)))))

    (define (handle-return m)
      (let ((return-type (list-ref m 1)))
        (set-function-spec-return! current-function-spec return-type)))

    (define (handle-version m)
      (set-function-spec-version! current-function-spec (list-ref m 1)))

    (define (handle-category m)
      (set-function-spec-category! current-function-spec (list-ref m 1)))

    (define (handle-alias m)
      (set-function-spec-alias! current-function-spec (list-ref m 1)))

    (define (handle-deprecated m)
      (set-function-spec-deprecated! current-function-spec (list-ref m 1)))

    (for ((l (in-lines input-port)))
         (cond
           ((regexp-match #px"^([a-zA-Z0-9_]+)\\(.*\\)" l) => new-function)
           ((regexp-match #px"^\\s+return\\s+(\\S+)" l) => handle-return)
           ((regexp-match #px"^\\s+version\\s+([0-9.]+)" l) => handle-version)
           ((regexp-match #px"^\\s+category\\s+(\\S+)" l) => handle-category)
           ((regexp-match #px"^\\s+alias\\s+(\\S+)" l) => handle-alias)
           ((regexp-match #px"^\\s+deprecated\\s+([0-9.]+)" l) => handle-deprecated)
           ((regexp-match #px"^\\s+param\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)(?:\\s+\\[(.*)\\])?" l) => handle-param)))

    (sort result string<? #:key function-spec-name)))

(define enums
  (call-with-input-file "specfiles/enum.spec" read-enums))

(define (base-to-ffi-type type)
  (cond
    ((hash-ref enums type #f) 
     =>
     (lambda (t2) t2))
    ((hash-ref type-map type #f)
     =>
     (lambda (t2) (hash-ref basic-type-map t2 t2)))
    (else type)))

(define (parse-param-spec-size str)
  (if str
    (begin
      (cond
        ((regexp-match #px"^[0-9]+$" str) (list (string->number str)))
        ((regexp-match #px"^[a-zA-Z]+$" str) (list (string->symbol str)))
        ((regexp-match #px"^([a-zA-Z]+)\\*([0-9]+)$" str) 
         =>
         (lambda (m) (list '* (string->symbol (list-ref m 1)) (string->number (list-ref m 2)))))
        ((equal? str "COMPSIZE(pname)") '((hash-ref pname-map pname 1)))
        (else 
          (printf "; Unparseable array size expression: ~a~%" str)
          '())))
    '()))

(define (to-ffi-type param-spec)

  (let ((t (base-to-ffi-type (param-spec-type param-spec)))
        (shape (param-spec-shape param-spec))
        (size-args (parse-param-spec-size (param-spec-size param-spec))))

    (let ((result
            (cond
              ((equal? shape "value") t)
              ((equal? shape "array")
               (if t (apply pointer-to t size-args) (pointer-to '_void)))
              ((equal? shape "reference")
               (if (and t (not (eq? t '_void))) 
                 (mode-dependent-type `(_ptr i ,t) `(_ptr o ,t))
                 (pointer-to '_void)))
              (else t))))
      (select-mode result (param-spec-mode param-spec)))))

(define (return-to-ffi-type rettype)
  (cond
    ((equal? rettype "String")
     '_string/utf-8)
    (else (select-mode (base-to-ffi-type rettype) "out"))))

(define (output-type? t)
  (and (list? t) (memq 'o t)))

(define (sanitize name)
  (if (eq? name 'values) 'the-values name))

; return function type and arity
(define (to-ffi-fun funspec)
  (let* ((params (map 
                   (lambda (p)
                     (list (sanitize (string->symbol (param-spec-name p)))
                           ': (to-ffi-type p)))
                   (function-spec-params funspec)))
         (output-params (map car (filter (lambda (p) (output-type? (list-ref p 2))) params)))
         (rettype (return-to-ffi-type (function-spec-return funspec)))
         (arity (- (length params) (length output-params))))
    (values
      (cond
        ((null? output-params)
         `(,@params -> ,rettype))
        ((eq? rettype '_void)
         (if (= 1 (length output-params))
           `(,@params -> ,rettype -> ,(car output-params))
           `(,@params -> ,rettype -> (values ,@ output-params))))
        (else
          `(,@params -> (result : ,rettype) -> (values result ,@ output-params))))
      arity)))


(define (select-mode t mode)
  (cond
    ((mode-dependent-type? t)
     (cond
       ((equal? mode "in")
        (select-mode (mode-dependent-type-in t) mode))
       ((equal? mode "out")
        (select-mode (mode-dependent-type-out t) mode))
       (else (error "Unknown mode:" mode))))
    ((pair? t)
     (cons (select-mode (car t) mode)
           (select-mode (cdr t) mode)))
    (else t)))

(define (contains-string? expr)
  (cond
    ((string? expr) #t)
    ((pair? expr)
     (or (contains-string? (car expr)) (contains-string? (cdr expr))))
    (else #f)))

(define (cleanup-type-for-ffi type)
  (cond
    ((pair? type)
     (cons (cleanup-type-for-ffi (car type))
           (cleanup-type-for-ffi (cdr type))))
    ((enum-spec? type)
     (base-to-ffi-type (enum-spec-type type)))
    (else type)))

(define (cleanup-type-for-doc type)
  (cond
    ((enum-spec? type)
     (if strongly-typed-enums
       (string->symbol (format "gl~a?" (enum-spec-name type)))
       (cleanup-type-for-doc (base-to-ffi-type (enum-spec-type type)))))
    ((list? type)
     (let ((head (car type)))
       (case head
         ((_ptr) (cleanup-type-for-doc (list-ref type 2)))
         ((_vector) `(vectorof ,(cleanup-type-for-doc (list-ref type 2))))
         (else
           (hash-ref vector-to-contract head type)))))
    ((symbol? type)
     (hash-ref flat-to-contract type
               (λ () (hash-ref vector-to-contract type type))))
    (else type)))

(define (cleanup-type-for-TR type)
  (cond
    ((enum-spec? type)
     (if strongly-typed-enums
       (string->symbol (format "gl~a?" (enum-spec-name type)))
       (cleanup-type-for-TR (base-to-ffi-type (enum-spec-type type)))))
    ((list? type)
     (let ((head (car type)))
       (case head
         ((_ptr) (cleanup-type-for-TR (list-ref type 2)))
         ((_vector) `(Vectorof ,(cleanup-type-for-TR (list-ref type 2))))
         (else
           (hash-ref vector-to-TR-type head type)))))
    ((symbol? type)
     (hash-ref flat-to-TR-type type
               (λ () (hash-ref vector-to-TR-type type type))))
    (else type)))

(define (fun-type->doc fun-type)
  (define (scan-inputs fun-type inputs outputs)
    (let ((head (car fun-type)))
      (cond
        ((eq? head '->)
         (values (reverse inputs) 
                 (scan-outputs (cdr fun-type) outputs)))
        (else
          (let* ((name (car head))
                 (type (list-ref head 2))
                 (clean-type (cleanup-type-for-TR type)))
            (if (output-type? type)
              (scan-inputs (cdr fun-type) inputs (cons (cons name clean-type) outputs))
              (scan-inputs (cdr fun-type)
                       (cons (list name clean-type) inputs)
                       outputs)))))))

  (define (process-outputs output output-assoc)
    (cond
      ((assq output output-assoc) => cdr)
      (else 'any/c)))

  (define (named-type? expr)
    (and (list? expr) (= (length expr) 3) (eq? (list-ref expr 1) ':)))

  (define (scan-outputs fun-type output-assoc)
    (let* ((ret-type (car fun-type))
           (ret-type (if (named-type? ret-type) (list-ref ret-type 2) ret-type))
           (ret-type (cleanup-type-for-TR ret-type))
           (ret-expr (if (= (length fun-type) 3)
                       (list-ref fun-type 2)
                       #f))
           (output-assoc (cons (cons 'result ret-type) output-assoc)))
    (cond
      ((not ret-expr) 
       (if (eq? ret-type 'void?) 'any ret-type))
      ((and (pair? ret-expr) (eq? (car ret-expr) 'values))
       (cons
         'values
         (map (lambda (o) (process-outputs o output-assoc)) (cdr ret-expr))))
      (else (process-outputs ret-expr output-assoc)))))

  (scan-inputs fun-type '() '()))

(define (print-doc port name fun-type spec)
  (let-values (((args-doc return-doc) (fun-type->doc fun-type)))
    (fprintf port "@defproc[~s ~s]{~%" 
             (cons (string->symbol (string-append "gl" name)) args-doc) 
             return-doc)
    (let ((extension-proc? (regexp-match #px"[A-Z][A-Z]$" name))
          (version (function-spec-version spec))
          (cat (function-spec-category spec)))
      (if extension-proc?
        (fprintf port "Extension @hyperlink[\"~a\"]{@racket[GL_~a]}.~%" 
                 (get-extension-url cat)
                 cat)
        (when (and version (not (string=? version "1.0")))
          (fprintf port "Version ~a.~%" (function-spec-version spec)))))
    (when (function-spec-deprecated spec)
      (fprintf port "Deprecated in version ~a.~%" (function-spec-deprecated spec)))
    (cond
      ((get-manpage name)
       =>
       (λ (url)
         (apply fprintf port "~%See the @hyperlink[\"~a\"]{gl~a manpage}.~%" url))))
    (when (function-spec-alias spec)
      (fprintf port "~%Alias of @racket[gl~a].~%" (function-spec-alias spec)))
    (fprintf port "}~%")))

(define (print-doc-header doc-port first-letter)
  (fprintf doc-port "#lang scribble/manual~%")
  (fprintf doc-port "@title{gl~a...}~%" first-letter))

(define doc-files '())

(define get-doc-port
  (let ((doc-hash (make-hash)))
    (lambda (name)
      (let ((first-letter (string-ref name 0)))
        (hash-ref doc-hash first-letter
                  (lambda ()
                    (let* ((filename (format "generated/gl_specs_~a.inc" first-letter))
                           (port (open-output-file filename
                                                  #:exists 'replace)))
                      (print-doc-header port first-letter)
                      (hash-set! doc-hash first-letter port)
                      (set! doc-files (cons filename doc-files))
                      port)))))))

(define (fun-type->contract fun-type)
  (let-values (((args-doc return-doc) (fun-type->doc fun-type)))
    `(->> ,@(map cadr args-doc) ,return-doc)))


(define name->checker
  (hash 
    "Begin" 'check-gl-error-begin
    "End" 'check-gl-error-end
    "GetError" 'void))

(for ((spec (in-list (call-with-input-file "specfiles/gl.spec.FIXED" read-function-specs))))
  (let*-values (((fun-t arity) (to-ffi-fun spec))
                ((name) (function-spec-name spec))
                ((fun-t-ffi) (cleanup-type-for-ffi fun-t)))

    (if (contains-string? fun-t-ffi) 
      (display "; ")
      (when #t ; (not (regexp-match #px"[A-Z]$" name))
        (let ((doc-port (get-doc-port name)))
          (print-doc doc-port name fun-t spec))))

    (printf "(define-gl gl~a ~s ~s ~s ~s)~%" name arity fun-t-ffi (fun-type->contract fun-t)
            (hash-ref name->checker name 'check-gl-error))))

(call-with-output-file "generated/gl_specs.txt" #:exists 'replace
  (λ (out)                       
    (for ((doc-file (in-list (sort doc-files string<?))))
      (fprintf out "@include-section[\"~a\"]~%" doc-file))))
