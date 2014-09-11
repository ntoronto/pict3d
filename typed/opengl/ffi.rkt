#lang s-exp typed-racket/base-env/extra-env-lang

(require (except-in ffi/unsafe 
                    ;; Renamed:
                    -> cast
                    ;; Provided by ffi-types.rkt:
                    cpointer?
                    ctype?)
         (prefix-in ffi- (only-in ffi/unsafe -> cast))
         (except-in ffi/vector
                    ;; Provided by ffi-types.rkt:
                    s8vector?
                    s16vector?
                    s32vector?
                    s64vector?
                    u16vector?
                    u32vector?
                    u64vector?
                    f32vector?
                    f64vector?)
         typed/racket/base
         "ffi-types.rkt"
         (for-syntax (submod "ffi-types.rkt" #%type-decl)))

(provide (all-from-out
          ffi/unsafe
          ffi/vector
          "ffi-types.rkt")
         (rename-out [ffi--> ->]
                     [ffi-cast cast]))

(type-environment
 [ _int8   (parse-type #'CType)]
 [_sint8   (parse-type #'CType)]
 [_uint8   (parse-type #'CType)]
 [ _int16  (parse-type #'CType)]
 [_sint16  (parse-type #'CType)]
 [_uint16  (parse-type #'CType)]
 [ _int32  (parse-type #'CType)]
 [_sint32  (parse-type #'CType)]
 [_uint32  (parse-type #'CType)]
 [ _int64  (parse-type #'CType)]
 [_sint64  (parse-type #'CType)]
 [_uint64  (parse-type #'CType)]
 [_float   (parse-type #'CType)]
 [_double  (parse-type #'CType)]
 [ctype-sizeof   (parse-type #'(-> CType Index))]
 [ctype-alignof  (parse-type #'(-> CType Index))]
 
 [memmove  (parse-type #'(case->
                          (-> CPointer CPointer Integer Void)
                          (-> CPointer CPointer Integer CType Void)
                          (-> CPointer Integer CPointer Integer Void)
                          (-> CPointer Integer CPointer Integer CType Void)
                          (-> CPointer Integer CPointer Integer Integer Void)
                          (-> CPointer Integer CPointer Integer Integer CType Void)))]
 [memcpy  (parse-type #'(case->
                         (-> CPointer CPointer Integer Void)
                         (-> CPointer CPointer Integer CType Void)
                         (-> CPointer Integer CPointer Integer Void)
                         (-> CPointer Integer CPointer Integer CType Void)
                         (-> CPointer Integer CPointer Integer Integer Void)
                         (-> CPointer Integer CPointer Integer Integer CType Void)))]
 [memset  (parse-type #'(case->
                         (-> CPointer Byte Integer Void)
                         (-> CPointer Byte Integer CType Void)
                         (-> CPointer Integer Byte Integer Void)
                         (-> CPointer Integer Byte Integer CType Void)))]
 [_byte  (parse-type #'CType)]
 
 ;; from ffi/vector
 [u8vector->cpointer   (parse-type #'(-> Bytes CPointer))]
 
 [make-s8vector        (parse-type #'(-> Integer S8Vector))]
 [s8vector             (parse-type #'(-> Integer * S8Vector))]
 [s8vector-length      (parse-type #'(-> S8Vector Index))]
 [s8vector-ref         (parse-type #'(-> S8Vector Integer Integer))]
 [s8vector-set!        (parse-type #'(-> S8Vector Integer Integer Void))]
 [list->s8vector       (parse-type #'(-> (Listof Integer) S8Vector))]
 [s8vector->list       (parse-type #'(-> S8Vector (Listof Integer)))]
 [s8vector->cpointer   (parse-type #'(-> S8Vector CPointer))]
 
 [make-s16vector       (parse-type #'(-> Integer S16Vector))]
 [s16vector            (parse-type #'(-> Integer * S16Vector))]
 [s16vector-length     (parse-type #'(-> S16Vector Index))]
 [s16vector-ref        (parse-type #'(-> S16Vector Integer Integer))]
 [s16vector-set!       (parse-type #'(-> S16Vector Integer Integer Void))]
 [list->s16vector      (parse-type #'(-> (Listof Integer) S16Vector))]
 [s16vector->list      (parse-type #'(-> S16Vector (Listof Integer)))]
 [s16vector->cpointer  (parse-type #'(-> S16Vector CPointer))]
 
 [make-s32vector       (parse-type #'(-> Integer S32Vector))]
 [s32vector            (parse-type #'(-> Integer * S32Vector))]
 [s32vector-length     (parse-type #'(-> S32Vector Index))]
 [s32vector-ref        (parse-type #'(-> S32Vector Integer Integer))]
 [s32vector-set!       (parse-type #'(-> S32Vector Integer Integer Void))]
 [list->s32vector      (parse-type #'(-> (Listof Integer) S32Vector))]
 [s32vector->list      (parse-type #'(-> S32Vector (Listof Integer)))]
 [s32vector->cpointer  (parse-type #'(-> S32Vector CPointer))]
 
 [make-s64vector       (parse-type #'(-> Integer S64Vector))]
 [s64vector            (parse-type #'(-> Integer * S64Vector))]
 [s64vector-length     (parse-type #'(-> S64Vector Index))]
 [s64vector-ref        (parse-type #'(-> S64Vector Integer Integer))]
 [s64vector-set!       (parse-type #'(-> S64Vector Integer Integer Void))]
 [list->s64vector      (parse-type #'(-> (Listof Integer) S64Vector))]
 [s64vector->list      (parse-type #'(-> S64Vector (Listof Integer)))]
 [s64vector->cpointer  (parse-type #'(-> S64Vector CPointer))]
 
 [make-u16vector       (parse-type #'(-> Integer U16Vector))]
 [u16vector            (parse-type #'(-> Integer * U16Vector))]
 [u16vector-length     (parse-type #'(-> U16Vector Index))]
 [u16vector-ref        (parse-type #'(-> U16Vector Integer Index))]
 [u16vector-set!       (parse-type #'(-> U16Vector Integer Integer Void))]
 [list->u16vector      (parse-type #'(-> (Listof Integer) U16Vector))]
 [u16vector->list      (parse-type #'(-> U16Vector (Listof Index)))]
 [u16vector->cpointer  (parse-type #'(-> U16Vector CPointer))]
 
 [make-u32vector       (parse-type #'(-> Integer U32Vector))]
 [u32vector            (parse-type #'(-> Integer * U32Vector))]
 [u32vector-length     (parse-type #'(-> U32Vector Index))]
 [u32vector-ref        (parse-type #'(-> U32Vector Integer Natural))]
 [u32vector-set!       (parse-type #'(-> U32Vector Integer Integer Void))]
 [list->u32vector      (parse-type #'(-> (Listof Integer) U32Vector))]
 [u32vector->list      (parse-type #'(-> U32Vector (Listof Natural)))]
 [u32vector->cpointer  (parse-type #'(-> U32Vector CPointer))]
 
 [make-u64vector       (parse-type #'(-> Integer U64Vector))]
 [u64vector            (parse-type #'(-> Integer * U64Vector))]
 [u64vector-length     (parse-type #'(-> U64Vector Index))]
 [u64vector-ref        (parse-type #'(-> U64Vector Integer Natural))]
 [u64vector-set!       (parse-type #'(-> U64Vector Integer Integer Void))]
 [list->u64vector      (parse-type #'(-> (Listof Integer) U64Vector))]
 [u64vector->list      (parse-type #'(-> U64Vector (Listof Natural)))]
 [u64vector->cpointer  (parse-type #'(-> U64Vector CPointer))]
 
 [make-f32vector       (parse-type #'(-> Integer F32Vector))]
 [f32vector            (parse-type #'(-> Real * F32Vector))]
 [f32vector-length     (parse-type #'(-> F32Vector Index))]
 [f32vector-ref        (parse-type #'(-> F32Vector Integer Real))]
 [f32vector-set!       (parse-type #'(-> F32Vector Integer Real Void))]
 [list->f32vector      (parse-type #'(-> (Listof Real) F32Vector))]
 [f32vector->list      (parse-type #'(-> F32Vector (Listof Real)))]
 [f32vector->cpointer  (parse-type #'(-> F32Vector CPointer))]
 
 [make-f64vector       (parse-type #'(-> Integer F64Vector))]
 [f64vector            (parse-type #'(-> Real * F64Vector))]
 [f64vector-length     (parse-type #'(-> F64Vector Index))]
 [f64vector-ref        (parse-type #'(-> F64Vector Integer Real))]
 [f64vector-set!       (parse-type #'(-> F64Vector Integer Real Void))]
 [list->f64vector      (parse-type #'(-> (Listof Real) F64Vector))]
 [f64vector->list      (parse-type #'(-> F64Vector (Listof Real)))]
 [f64vector->cpointer  (parse-type #'(-> F64Vector CPointer))]
 
 [register-finalizer  (parse-type #'(All (a) (-> a (-> a Any) Void)))]
 )
