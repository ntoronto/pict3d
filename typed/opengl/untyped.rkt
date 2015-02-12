#lang racket/base

(require 
  (for-syntax racket/base)
  racket/promise racket/future racket/include racket/set racket/class racket/draw
  (rename-in racket/contract (-> ->>))
  ffi/unsafe ffi/vector)

(define stype (system-type))

(define win32?
  (and (eq? 'windows stype)
       (equal? "win32\\i386" (path->string (system-library-subpath #f)))))

(define-syntax _fun*
  (syntax-rules ()
    [(_fun* x ...)
     (if win32? (_fun #:abi 'stdcall x ...) (_fun x ...))]))

(define gl-lib
  (case stype
    [(windows) (ffi-lib "opengl32")]
    [(macosx)  (ffi-lib "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL")]
    [else      (ffi-lib "libGL" '("1" ""))]))

(define system-get-proc-address-names
  (case stype
    [(windows)  '("wglGetProcAddress")]
    [(macosx)   '()]
    ;; Boldly assume everybody else uses X11
    [else       '("glXGetProcAddressARB" "glXGetProcAddress")]))

(define system-get-proc-address
  (delay
    (let loop ([names  system-get-proc-address-names])
      (if (null? names)
          (λ (x) (ffi-obj-ref x gl-lib (λ () #f)))
          (get-ffi-obj (car names) gl-lib (_fun* _string -> _pointer)
                       (λ () (loop (cdr names))))))))

;; The default-gl-procedure-loader procedure dynamically loads a GL procedure.
(define (default-gl-procedure-loader name-str)
  ((force system-get-proc-address) name-str))

(define gl-procedure-loader default-gl-procedure-loader)
(define (set-gl-procedure-loader! new-loader) (set! gl-procedure-loader new-loader))

(provide/contract
  (default-gl-procedure-loader (->> string? (or/c cpointer? procedure? #f))) 
  (set-gl-procedure-loader! (->> (->> string? (or/c cpointer? procedure? #f)) any)))

;; A hashtable from (weak) OpenGL contexts to hashtables from function names to pointers
;; Required to make OpenGL API calls on Windows fast - see below
(define context-function-hash (make-weak-hasheq))

(define (lookup-gl-procedure name-str fun-type)
  ;; This is strictly only necessary on Windows, but enforcing it everywhere helps keeps client
  ;; programs portable
  (unless (get-current-gl-context)
    (error 'lookup-gl-procedure "no current OpenGL context looking up ~a" name-str))
  
  (define (missing)
    (error 'lookup-gl-procedure "OpenGL function not available: ~a" name-str))
  
  (case stype
    [(windows)
     ;; Any OpenGL version <= 1.1 function MUST be loaded using GetProcAddress
     (get-ffi-obj
      name-str gl-lib fun-type
      (λ ()
        ;; If that fails, it's probably a > 1.1 function, which means two things:
        ;;   1. We MUST load it using wglGetProcAddress.
        ;;   2. The function implementation is specific to the GL context.
        ;; Racket's `sgl/gl` looks up the function pointer every time this happens, but that's
        ;; really slow. (I've simulated it on Linux, and found that each API lookup takes about
        ;; 0.01ms. It may be slower on Windows.) So we keep a weak hash table of function pointers
        ;; per context to speed up lookup.
        (λ args
          (define function-hash (hash-ref! context-function-hash (get-current-gl-context) make-hash))
          (define proc (hash-ref! function-hash name-str
                                  (λ () (function-ptr (gl-procedure-loader name-str) fun-type))))
          (cond [proc  (apply proc args)]
                [else  (missing)]))))]
    [else
     ;; The other OSes make this so easy...
     (define proc (function-ptr (gl-procedure-loader name-str) fun-type))
     (if proc proc (missing))]))

; Load everything lazily to speed up loading this library
(define-syntax (define-gl stx)
  (syntax-case stx ()
    [(_ name arity (type ...) contract checker)
     (with-syntax ([(arg ...)  (generate-temporaries
                                (build-list (syntax->datum #'arity)
                                            (λ (n) (format "arg~a" n))))]
                   [name-str  (symbol->string (syntax->datum #'name))])
       (syntax/loc stx
         (begin
           ;; Main idea: have the function look up its implementation when first used, then
           ;; mutate itself into a wrapper for the implementation that checks for errors
           
           ;; Problem: if the exported function is mutated, any contract wrapper is very expensive,
           ;; so make the exported function a wrapper for a private, self-mutating function
           (define (name arg ...) (self-mutating-fun arg ...))
           
           (define (self-mutating-fun arg ...)
             (define proc (lookup-gl-procedure name-str (_fun* type ...)))
             
             (define (fun arg ...)
               ;; Why would you be calling an OpenGL function without a context? Even getting the
               ;; version string is context-dependent
               (unless (get-current-gl-context) (error 'name "no current OpenGL context"))
               (begin0
                 (proc arg ...)
                 (checker 'name)))
             
             (set! self-mutating-fun fun)
             (fun arg ...))
           
           (provide name))))]))

(define-syntax define-const
  (syntax-rules ()
    ((_ name val)
     (begin
       (define name val)
       (provide name)))))

(define-syntax define-enum
  (syntax-rules ()
   ((_ name (constants ...))
    (begin
      (define name (let ((s (seteqv constants ...)))
                     (lambda (v) (set-member? s v))))
      (provide name)))))

(define-syntax define-bitfield
  (syntax-rules ()
   ((_ name (constants ...))
    (begin
      (define name (let ((m (bitwise-ior constants ...)))
                     (lambda (v) (= v (bitwise-and v m)))))
      (provide name)))))

; Check GL result
(define (check-gl-error name)
  (unless between-begin-end
    (let ((err (glGetError)))
      (unless (zero? err)
        (let ((msg (hash-ref error-messages err
                             (lambda () (format "Error code ~s." err)))))
          (error (format "OpenGL error in procedure ~a: ~a" name msg)))))))

(define (check-gl-error-begin name)
  (set! between-begin-end #t))

(define (check-gl-error-end name)
  (set! between-begin-end #f)
  (check-gl-error name))

(provide/contract
  (GLsync? (->> any/c boolean?)))

(define-cpointer-type _GLsync)

; Some functions take a pointer parameter, but when a VBO is bound the pointer
; is really interpreted as an integer offset.
; On the Racket side, we want the function to simply accept either a pointer or an integer in that
; case.
; So here is a conversion function to be used as a pre:-code sequence.
(define (convert-vbo-pointer v)
  (cond
    ((cpointer? v) v)
    ((exact-integer? v) (ptr-add #f v))
    (else (gl-vector->cpointer v))))

(define-fun-syntax _pointer/intptr
  (syntax-id-rules (_pointer/intptr)
    [_pointer/intptr (type: _pointer pre: (x => (convert-vbo-pointer x)))]))

(define (gl-vector->info vec)
  (cond
    ((bytes? vec) (values GL_UNSIGNED_BYTE values bytes-length))
    ((s8vector? vec) (values GL_BYTE s8vector->cpointer s8vector-length))
    ((u16vector? vec) (values GL_UNSIGNED_SHORT u16vector->cpointer u16vector-length))
    ((s16vector? vec) (values GL_SHORT s16vector->cpointer s16vector-length))
    ((u32vector? vec) (values GL_UNSIGNED_INT u32vector->cpointer u32vector-length))
    ((s32vector? vec) (values GL_INT s32vector->cpointer s32vector-length))
    ((f32vector? vec) (values GL_FLOAT f32vector->cpointer f32vector-length))
    ((f64vector? vec) (values GL_DOUBLE f64vector->cpointer f64vector-length))))

(define (gl-vector->cpointer vec)
  (let-values (((type ->cpointer length) (gl-vector->info vec)))
              (->cpointer vec)))

(provide gl-vector->info
         gl-vector->cpointer)

(include "generated/gl_specs.inc")

(define error-messages (hasheqv
        GL_NO_ERROR "No error has been recorded."
        GL_INVALID_ENUM "An unacceptable value is specified for an enumerated argument."
        GL_INVALID_VALUE "A numeric argument is out of range."
        GL_INVALID_OPERATION "The specified operation is not allowed in the current state."
        GL_STACK_OVERFLOW "This command would cause a stack overflow."
        GL_STACK_UNDERFLOW "This command would cause a stack underflow."
        GL_OUT_OF_MEMORY "There is not enough memory left to execute the command."))

(define between-begin-end #f)
