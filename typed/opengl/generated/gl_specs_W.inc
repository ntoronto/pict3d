#lang scribble/manual
@title{glW...}
@defproc[(glWaitSync (sync GLSync) (flags Natural) (timeout Natural)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glWaitSync.xml"]{glWaitSync manpage}.
}
@defproc[(glWeightPathsNV (resultPath Natural) (numPaths Integer) (paths U32Vector) (weights F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glWeightPointerARB (size Integer) (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightbvARB (size Integer) (weights S8Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightdvARB (size Integer) (weights F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightfvARB (size Integer) (weights F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightivARB (size Integer) (weights S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightsvARB (size Integer) (weights S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightubvARB (size Integer) (weights (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightuivARB (size Integer) (weights U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightusvARB (size Integer) (weights U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWindowPos2d (x Real) (y Real)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2dARB (x Real) (y Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2d].
}
@defproc[(glWindowPos2dMESA (x Real) (y Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2dARB].
}
@defproc[(glWindowPos2dv (v F64Vector)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2dvARB (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2dv].
}
@defproc[(glWindowPos2dvMESA (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2dvARB].
}
@defproc[(glWindowPos2f (x Flonum) (y Flonum)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2fARB (x Flonum) (y Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2f].
}
@defproc[(glWindowPos2fMESA (x Flonum) (y Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2fARB].
}
@defproc[(glWindowPos2fv (v F32Vector)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2fvARB (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2fv].
}
@defproc[(glWindowPos2fvMESA (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2fvARB].
}
@defproc[(glWindowPos2i (x Integer) (y Integer)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2iARB (x Integer) (y Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2i].
}
@defproc[(glWindowPos2iMESA (x Integer) (y Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2iARB].
}
@defproc[(glWindowPos2iv (v S32Vector)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2ivARB (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2iv].
}
@defproc[(glWindowPos2ivMESA (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2ivARB].
}
@defproc[(glWindowPos2s (x Fixnum) (y Fixnum)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2sARB (x Fixnum) (y Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2s].
}
@defproc[(glWindowPos2sMESA (x Fixnum) (y Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2sARB].
}
@defproc[(glWindowPos2sv (v S16Vector)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2svARB (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2sv].
}
@defproc[(glWindowPos2svMESA (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2svARB].
}
@defproc[(glWindowPos3d (x Real) (y Real) (z Real)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3dARB (x Real) (y Real) (z Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3d].
}
@defproc[(glWindowPos3dMESA (x Real) (y Real) (z Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3dARB].
}
@defproc[(glWindowPos3dv (v F64Vector)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3dvARB (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3dv].
}
@defproc[(glWindowPos3dvMESA (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3dvARB].
}
@defproc[(glWindowPos3f (x Flonum) (y Flonum) (z Flonum)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3fARB (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3f].
}
@defproc[(glWindowPos3fMESA (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3fARB].
}
@defproc[(glWindowPos3fv (v F32Vector)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3fvARB (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3fv].
}
@defproc[(glWindowPos3fvMESA (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3fvARB].
}
@defproc[(glWindowPos3i (x Integer) (y Integer) (z Integer)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3iARB (x Integer) (y Integer) (z Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3i].
}
@defproc[(glWindowPos3iMESA (x Integer) (y Integer) (z Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3iARB].
}
@defproc[(glWindowPos3iv (v S32Vector)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3ivARB (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3iv].
}
@defproc[(glWindowPos3ivMESA (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3ivARB].
}
@defproc[(glWindowPos3s (x Fixnum) (y Fixnum) (z Fixnum)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3sARB (x Fixnum) (y Fixnum) (z Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3s].
}
@defproc[(glWindowPos3sMESA (x Fixnum) (y Fixnum) (z Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3sARB].
}
@defproc[(glWindowPos3sv (v S16Vector)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3svARB (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3sv].
}
@defproc[(glWindowPos3svMESA (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3svARB].
}
@defproc[(glWindowPos4dMESA (x Real) (y Real) (z Real) (w Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWindowPos4dvMESA (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWindowPos4fMESA (x Flonum) (y Flonum) (z Flonum) (w Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWindowPos4fvMESA (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWindowPos4iMESA (x Integer) (y Integer) (z Integer) (w Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWindowPos4ivMESA (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWindowPos4sMESA (x Fixnum) (y Fixnum) (z Fixnum) (w Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWindowPos4svMESA (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWriteMaskEXT (res Natural) (in Natural) (outX Integer) (outY Integer) (outZ Integer) (outW Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
