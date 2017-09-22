#lang typed/racket/base

(provide
 CPointer cpointer?
 CType ctype?
 S8Vector  s8vector?
 S16Vector s16vector?
 S32Vector s32vector?
 S64Vector s64vector?
 U16Vector u16vector?
 U32Vector u32vector?
 U64Vector u64vector?
 F32Vector f32vector?
 F64Vector f64vector?
 )

(require typed/racket/unsafe) ; only for cpointer? and ctype?

(unsafe-require/typed
 ffi/unsafe
 [#:opaque CPointer cpointer?]  ; includes Bytes and other things that can be used as cpointers
 [#:opaque CType ctype?]
 )

(require/typed
 ffi/vector
 [#:opaque  S8Vector  s8vector?]
 [#:opaque S16Vector s16vector?]
 [#:opaque S32Vector s32vector?]
 [#:opaque S64Vector s64vector?]
 [#:opaque U16Vector u16vector?]
 [#:opaque U32Vector u32vector?]
 [#:opaque U64Vector u64vector?]
 [#:opaque F32Vector f32vector?]
 [#:opaque F64Vector f64vector?]
 )
