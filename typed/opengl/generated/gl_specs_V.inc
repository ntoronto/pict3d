#lang scribble/manual
@title{glV...}
@defproc[(glVDPAUFiniNV) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vdpau_interop.txt"]{@racket[GL_NV_vdpau_interop]}.
}
@defproc[(glVDPAUInitNV (vdpDevice GLPointer) (getProcAddress GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vdpau_interop.txt"]{@racket[GL_NV_vdpau_interop]}.
}
@defproc[(glValidateProgram (program Natural)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glValidateProgram.xml"]{glValidateProgram manpage}.
}
@defproc[(glValidateProgramARB (programObj Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glValidateProgram].
}
@defproc[(glValidateProgramPipeline (pipeline Natural)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glValidateProgramPipeline.xml"]{glValidateProgramPipeline manpage}.
}
@defproc[(glVariantArrayObjectATI (id Natural) (type Integer) (stride Integer) (buffer Natural) (offset Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glVariantPointerEXT (id Natural) (type Integer) (stride Natural) (addr GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantbvEXT (id Natural) (addr S8Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantdvEXT (id Natural) (addr F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantfvEXT (id Natural) (addr F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantivEXT (id Natural) (addr S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantsvEXT (id Natural) (addr S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantubvEXT (id Natural) (addr (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantuivEXT (id Natural) (addr U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantusvEXT (id Natural) (addr U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVertex2d (x Real) (y Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex2dv (v F64Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex2f (x Flonum) (y Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex2fv (v F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex2hNV (x Nonnegative-Fixnum) (y Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertex2hvNV (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertex2i (x Integer) (y Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex2iv (v S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex2s (x Fixnum) (y Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex2sv (v S16Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3d (x Real) (y Real) (z Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3dv (v F64Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3f (x Flonum) (y Flonum) (z Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3fv (v F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3hNV (x Nonnegative-Fixnum) (y Nonnegative-Fixnum) (z Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertex3hvNV (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertex3i (x Integer) (y Integer) (z Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3iv (v S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3s (x Fixnum) (y Fixnum) (z Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3sv (v S16Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4d (x Real) (y Real) (z Real) (w Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4dv (v F64Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4f (x Flonum) (y Flonum) (z Flonum) (w Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4fv (v F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4hNV (x Nonnegative-Fixnum) (y Nonnegative-Fixnum) (z Nonnegative-Fixnum) (w Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertex4hvNV (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertex4i (x Integer) (y Integer) (z Integer) (w Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4iv (v S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4s (x Fixnum) (y Fixnum) (z Fixnum) (w Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4sv (v S16Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertexArrayBindVertexBufferEXT (vaobj Natural) (bindingindex Natural) (buffer Natural) (offset Integer) (stride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_attrib_binding.txt"]{@racket[GL_ARB_vertex_attrib_binding]}.
}
@defproc[(glVertexArrayParameteriAPPLE (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_array_range.txt"]{@racket[GL_APPLE_vertex_array_range]}.
}
@defproc[(glVertexArrayRangeAPPLE (length Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_array_range.txt"]{@racket[GL_APPLE_vertex_array_range]}.
}
@defproc[(glVertexArrayRangeNV (length Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_array_range.txt"]{@racket[GL_NV_vertex_array_range]}.
}
@defproc[(glVertexArrayVertexAttribBindingEXT (vaobj Natural) (attribindex Natural) (bindingindex Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_attrib_binding.txt"]{@racket[GL_ARB_vertex_attrib_binding]}.
}
@defproc[(glVertexArrayVertexAttribFormatEXT (vaobj Natural) (attribindex Natural) (size Integer) (type Integer) (normalized Boolean) (relativeoffset Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_attrib_binding.txt"]{@racket[GL_ARB_vertex_attrib_binding]}.
}
@defproc[(glVertexArrayVertexAttribIFormatEXT (vaobj Natural) (attribindex Natural) (size Integer) (type Integer) (relativeoffset Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_attrib_binding.txt"]{@racket[GL_ARB_vertex_attrib_binding]}.
}
@defproc[(glVertexArrayVertexAttribLFormatEXT (vaobj Natural) (attribindex Natural) (size Integer) (type Integer) (relativeoffset Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_attrib_binding.txt"]{@racket[GL_ARB_vertex_attrib_binding]}.
}
@defproc[(glVertexArrayVertexAttribLOffsetEXT (vaobj Natural) (buffer Natural) (index Natural) (size Integer) (type Integer) (stride Integer) (offset Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexArrayVertexBindingDivisorEXT (vaobj Natural) (bindingindex Natural) (divisor Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_attrib_binding.txt"]{@racket[GL_ARB_vertex_attrib_binding]}.
}
@defproc[(glVertexAttrib1d (index Natural) (x Real)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib1dARB (index Natural) (x Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib1d].
}
@defproc[(glVertexAttrib1dNV (index Natural) (x Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib1d].
}
@defproc[(glVertexAttrib1dv (index Natural) (v Real)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib1dvARB (index Natural) (v Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib1dv].
}
@defproc[(glVertexAttrib1dvNV (index Natural) (v Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib1dv].
}
@defproc[(glVertexAttrib1f (index Natural) (x Flonum)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib1fARB (index Natural) (x Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib1f].
}
@defproc[(glVertexAttrib1fNV (index Natural) (x Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib1f].
}
@defproc[(glVertexAttrib1fv (index Natural) (v Flonum)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib1fvARB (index Natural) (v Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib1fv].
}
@defproc[(glVertexAttrib1fvNV (index Natural) (v Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib1fv].
}
@defproc[(glVertexAttrib1hNV (index Natural) (x Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib1hvNV (index Natural) (v Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib1s (index Natural) (x Fixnum)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib1sARB (index Natural) (x Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib1s].
}
@defproc[(glVertexAttrib1sNV (index Natural) (x Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib1s].
}
@defproc[(glVertexAttrib1sv (index Natural) (v Fixnum)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib1svARB (index Natural) (v Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib1sv].
}
@defproc[(glVertexAttrib1svNV (index Natural) (v Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib1sv].
}
@defproc[(glVertexAttrib2d (index Natural) (x Real) (y Real)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib2dARB (index Natural) (x Real) (y Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib2d].
}
@defproc[(glVertexAttrib2dNV (index Natural) (x Real) (y Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib2d].
}
@defproc[(glVertexAttrib2dv (index Natural) (v F64Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib2dvARB (index Natural) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib2dv].
}
@defproc[(glVertexAttrib2dvNV (index Natural) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib2dv].
}
@defproc[(glVertexAttrib2f (index Natural) (x Flonum) (y Flonum)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib2fARB (index Natural) (x Flonum) (y Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib2f].
}
@defproc[(glVertexAttrib2fNV (index Natural) (x Flonum) (y Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib2f].
}
@defproc[(glVertexAttrib2fv (index Natural) (v F32Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib2fvARB (index Natural) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib2fv].
}
@defproc[(glVertexAttrib2fvNV (index Natural) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib2fv].
}
@defproc[(glVertexAttrib2hNV (index Natural) (x Nonnegative-Fixnum) (y Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib2hvNV (index Natural) (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib2s (index Natural) (x Fixnum) (y Fixnum)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib2sARB (index Natural) (x Fixnum) (y Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib2s].
}
@defproc[(glVertexAttrib2sNV (index Natural) (x Fixnum) (y Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib2s].
}
@defproc[(glVertexAttrib2sv (index Natural) (v S16Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib2svARB (index Natural) (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib2sv].
}
@defproc[(glVertexAttrib2svNV (index Natural) (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib2sv].
}
@defproc[(glVertexAttrib3d (index Natural) (x Real) (y Real) (z Real)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib3dARB (index Natural) (x Real) (y Real) (z Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib3d].
}
@defproc[(glVertexAttrib3dNV (index Natural) (x Real) (y Real) (z Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib3d].
}
@defproc[(glVertexAttrib3dv (index Natural) (v F64Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib3dvARB (index Natural) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib3dv].
}
@defproc[(glVertexAttrib3dvNV (index Natural) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib3dv].
}
@defproc[(glVertexAttrib3f (index Natural) (x Flonum) (y Flonum) (z Flonum)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib3fARB (index Natural) (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib3f].
}
@defproc[(glVertexAttrib3fNV (index Natural) (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib3f].
}
@defproc[(glVertexAttrib3fv (index Natural) (v F32Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib3fvARB (index Natural) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib3fv].
}
@defproc[(glVertexAttrib3fvNV (index Natural) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib3fv].
}
@defproc[(glVertexAttrib3hNV (index Natural) (x Nonnegative-Fixnum) (y Nonnegative-Fixnum) (z Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib3hvNV (index Natural) (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib3s (index Natural) (x Fixnum) (y Fixnum) (z Fixnum)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib3sARB (index Natural) (x Fixnum) (y Fixnum) (z Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib3s].
}
@defproc[(glVertexAttrib3sNV (index Natural) (x Fixnum) (y Fixnum) (z Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib3s].
}
@defproc[(glVertexAttrib3sv (index Natural) (v S16Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib3svARB (index Natural) (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib3sv].
}
@defproc[(glVertexAttrib3svNV (index Natural) (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib3sv].
}
@defproc[(glVertexAttrib4Nbv (index Natural) (v S8Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttrib4NbvARB (index Natural) (v S8Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4Nbv].
}
@defproc[(glVertexAttrib4Niv (index Natural) (v S32Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttrib4NivARB (index Natural) (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4Niv].
}
@defproc[(glVertexAttrib4Nsv (index Natural) (v S16Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttrib4NsvARB (index Natural) (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4Nsv].
}
@defproc[(glVertexAttrib4Nub (index Natural) (x Byte) (y Byte) (z Byte) (w Byte)) Void]{
Version 2.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttrib4NubARB (index Natural) (x Byte) (y Byte) (z Byte) (w Byte)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4Nub].
}
@defproc[(glVertexAttrib4Nubv (index Natural) (v (U Bytes String))) Void]{
Version 2.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttrib4NubvARB (index Natural) (v (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4Nubv].
}
@defproc[(glVertexAttrib4Nuiv (index Natural) (v U32Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttrib4NuivARB (index Natural) (v U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4Nuiv].
}
@defproc[(glVertexAttrib4Nusv (index Natural) (v U16Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttrib4NusvARB (index Natural) (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4Nusv].
}
@defproc[(glVertexAttrib4bv (index Natural) (v S8Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4bvARB (index Natural) (v S8Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4bv].
}
@defproc[(glVertexAttrib4d (index Natural) (x Real) (y Real) (z Real) (w Real)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4dARB (index Natural) (x Real) (y Real) (z Real) (w Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4d].
}
@defproc[(glVertexAttrib4dNV (index Natural) (x Real) (y Real) (z Real) (w Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4d].
}
@defproc[(glVertexAttrib4dv (index Natural) (v F64Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4dvARB (index Natural) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4dv].
}
@defproc[(glVertexAttrib4dvNV (index Natural) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4dv].
}
@defproc[(glVertexAttrib4f (index Natural) (x Flonum) (y Flonum) (z Flonum) (w Flonum)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4fARB (index Natural) (x Flonum) (y Flonum) (z Flonum) (w Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4f].
}
@defproc[(glVertexAttrib4fNV (index Natural) (x Flonum) (y Flonum) (z Flonum) (w Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4f].
}
@defproc[(glVertexAttrib4fv (index Natural) (v F32Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4fvARB (index Natural) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4fv].
}
@defproc[(glVertexAttrib4fvNV (index Natural) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4fv].
}
@defproc[(glVertexAttrib4hNV (index Natural) (x Nonnegative-Fixnum) (y Nonnegative-Fixnum) (z Nonnegative-Fixnum) (w Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib4hvNV (index Natural) (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib4iv (index Natural) (v S32Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4ivARB (index Natural) (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4iv].
}
@defproc[(glVertexAttrib4s (index Natural) (x Fixnum) (y Fixnum) (z Fixnum) (w Fixnum)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4sARB (index Natural) (x Fixnum) (y Fixnum) (z Fixnum) (w Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4s].
}
@defproc[(glVertexAttrib4sNV (index Natural) (x Fixnum) (y Fixnum) (z Fixnum) (w Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4s].
}
@defproc[(glVertexAttrib4sv (index Natural) (v S16Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4svARB (index Natural) (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4sv].
}
@defproc[(glVertexAttrib4svNV (index Natural) (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4sv].
}
@defproc[(glVertexAttrib4ubNV (index Natural) (x Byte) (y Byte) (z Byte) (w Byte)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4Nub].
}
@defproc[(glVertexAttrib4ubv (index Natural) (v (U Bytes String))) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4ubvARB (index Natural) (v (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4ubv].
}
@defproc[(glVertexAttrib4ubvNV (index Natural) (v (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4Nubv].
}
@defproc[(glVertexAttrib4uiv (index Natural) (v U32Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4uivARB (index Natural) (v U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4uiv].
}
@defproc[(glVertexAttrib4usv (index Natural) (v U16Vector)) Void]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4usvARB (index Natural) (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4usv].
}
@defproc[(glVertexAttribArrayObjectATI (index Natural) (size Integer) (type Integer) (normalized Boolean) (stride Integer) (buffer Natural) (offset Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_attrib_array_object.txt"]{@racket[GL_ATI_vertex_attrib_array_object]}.
}
@defproc[(glVertexAttribBinding (attribindex Natural) (bindingindex Natural)) Void]{
Version 4.3.
}
@defproc[(glVertexAttribDivisor (index Natural) (divisor Natural)) Void]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttribDivisor.xml"]{glVertexAttribDivisor manpage}.
}
@defproc[(glVertexAttribDivisorARB (index Natural) (divisor Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/instanced_arrays.txt"]{@racket[GL_ARB_instanced_arrays]}.
}
@defproc[(glVertexAttribFormat (attribindex Natural) (size Integer) (type Integer) (normalized Boolean) (relativeoffset Natural)) Void]{
Version 4.3.
}
@defproc[(glVertexAttribFormatNV (index Natural) (size Integer) (type Integer) (normalized Boolean) (stride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glVertexAttribI1i (index Natural) (x Integer)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI1iEXT (index Natural) (x Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI1i].
}
@defproc[(glVertexAttribI1iv (index Natural) (v Integer)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI1ivEXT (index Natural) (v Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI1iv].
}
@defproc[(glVertexAttribI1ui (index Natural) (x Natural)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI1uiEXT (index Natural) (x Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI1ui].
}
@defproc[(glVertexAttribI1uiv (index Natural) (v Natural)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI1uivEXT (index Natural) (v Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI1uiv].
}
@defproc[(glVertexAttribI2i (index Natural) (x Integer) (y Integer)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI2iEXT (index Natural) (x Integer) (y Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI2i].
}
@defproc[(glVertexAttribI2iv (index Natural) (v S32Vector)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI2ivEXT (index Natural) (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI2iv].
}
@defproc[(glVertexAttribI2ui (index Natural) (x Natural) (y Natural)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI2uiEXT (index Natural) (x Natural) (y Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI2ui].
}
@defproc[(glVertexAttribI2uiv (index Natural) (v U32Vector)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI2uivEXT (index Natural) (v U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI2uiv].
}
@defproc[(glVertexAttribI3i (index Natural) (x Integer) (y Integer) (z Integer)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI3iEXT (index Natural) (x Integer) (y Integer) (z Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI3i].
}
@defproc[(glVertexAttribI3iv (index Natural) (v S32Vector)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI3ivEXT (index Natural) (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI3iv].
}
@defproc[(glVertexAttribI3ui (index Natural) (x Natural) (y Natural) (z Natural)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI3uiEXT (index Natural) (x Natural) (y Natural) (z Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI3ui].
}
@defproc[(glVertexAttribI3uiv (index Natural) (v U32Vector)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI3uivEXT (index Natural) (v U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI3uiv].
}
@defproc[(glVertexAttribI4bv (index Natural) (v S8Vector)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4bvEXT (index Natural) (v S8Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4bv].
}
@defproc[(glVertexAttribI4i (index Natural) (x Integer) (y Integer) (z Integer) (w Integer)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4iEXT (index Natural) (x Integer) (y Integer) (z Integer) (w Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4i].
}
@defproc[(glVertexAttribI4iv (index Natural) (v S32Vector)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4ivEXT (index Natural) (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4iv].
}
@defproc[(glVertexAttribI4sv (index Natural) (v S16Vector)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4svEXT (index Natural) (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4sv].
}
@defproc[(glVertexAttribI4ubv (index Natural) (v (U Bytes String))) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4ubvEXT (index Natural) (v (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4ubv].
}
@defproc[(glVertexAttribI4ui (index Natural) (x Natural) (y Natural) (z Natural) (w Natural)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4uiEXT (index Natural) (x Natural) (y Natural) (z Natural) (w Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4ui].
}
@defproc[(glVertexAttribI4uiv (index Natural) (v U32Vector)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4uivEXT (index Natural) (v U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4uiv].
}
@defproc[(glVertexAttribI4usv (index Natural) (v U16Vector)) Void]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4usvEXT (index Natural) (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4usv].
}
@defproc[(glVertexAttribIFormat (attribindex Natural) (size Integer) (type Integer) (relativeoffset Natural)) Void]{
Version 4.3.
}
@defproc[(glVertexAttribIFormatNV (index Natural) (size Integer) (type Integer) (stride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glVertexAttribIPointer (index Natural) (size Integer) (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Version 3.0.
}
@defproc[(glVertexAttribIPointerEXT (index Natural) (size Integer) (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribIPointer].
}
@defproc[(glVertexAttribL1d (index Natural) (x Real)) Void]{
Version 4.1.
}
@defproc[(glVertexAttribL1dEXT (index Natural) (x Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL1dv (index Natural) (v Real)) Void]{
Version 4.1.
}
@defproc[(glVertexAttribL1dvEXT (index Natural) (v Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL1i64NV (index Natural) (x Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL1i64vNV (index Natural) (v Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL1ui64NV (index Natural) (x Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL1ui64vNV (index Natural) (v Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL2d (index Natural) (x Real) (y Real)) Void]{
Version 4.1.
}
@defproc[(glVertexAttribL2dEXT (index Natural) (x Real) (y Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL2dv (index Natural) (v F64Vector)) Void]{
Version 4.1.
}
@defproc[(glVertexAttribL2dvEXT (index Natural) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL2i64NV (index Natural) (x Integer) (y Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL2i64vNV (index Natural) (v S64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL2ui64NV (index Natural) (x Natural) (y Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL2ui64vNV (index Natural) (v U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL3d (index Natural) (x Real) (y Real) (z Real)) Void]{
Version 4.1.
}
@defproc[(glVertexAttribL3dEXT (index Natural) (x Real) (y Real) (z Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL3dv (index Natural) (v F64Vector)) Void]{
Version 4.1.
}
@defproc[(glVertexAttribL3dvEXT (index Natural) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL3i64NV (index Natural) (x Integer) (y Integer) (z Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL3i64vNV (index Natural) (v S64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL3ui64NV (index Natural) (x Natural) (y Natural) (z Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL3ui64vNV (index Natural) (v U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL4d (index Natural) (x Real) (y Real) (z Real) (w Real)) Void]{
Version 4.1.
}
@defproc[(glVertexAttribL4dEXT (index Natural) (x Real) (y Real) (z Real) (w Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL4dv (index Natural) (v F64Vector)) Void]{
Version 4.1.
}
@defproc[(glVertexAttribL4dvEXT (index Natural) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL4i64NV (index Natural) (x Integer) (y Integer) (z Integer) (w Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL4i64vNV (index Natural) (v S64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL4ui64NV (index Natural) (x Natural) (y Natural) (z Natural) (w Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL4ui64vNV (index Natural) (v U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribLFormat (attribindex Natural) (size Integer) (type Integer) (relativeoffset Natural)) Void]{
Version 4.3.
}
@defproc[(glVertexAttribLFormatNV (index Natural) (size Integer) (type Integer) (stride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribLPointer (index Natural) (size Integer) (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Version 4.1.
}
@defproc[(glVertexAttribLPointerEXT (index Natural) (size Integer) (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribP1ui (index Natural) (type Integer) (normalized Boolean) (value Natural)) Void]{
Version 1.2.
}
@defproc[(glVertexAttribP1uiv (index Natural) (type Integer) (normalized Boolean) (value Natural)) Void]{
Version 1.2.
}
@defproc[(glVertexAttribP2ui (index Natural) (type Integer) (normalized Boolean) (value Natural)) Void]{
Version 1.2.
}
@defproc[(glVertexAttribP2uiv (index Natural) (type Integer) (normalized Boolean) (value Natural)) Void]{
Version 1.2.
}
@defproc[(glVertexAttribP3ui (index Natural) (type Integer) (normalized Boolean) (value Natural)) Void]{
Version 1.2.
}
@defproc[(glVertexAttribP3uiv (index Natural) (type Integer) (normalized Boolean) (value Natural)) Void]{
Version 1.2.
}
@defproc[(glVertexAttribP4ui (index Natural) (type Integer) (normalized Boolean) (value Natural)) Void]{
Version 1.2.
}
@defproc[(glVertexAttribP4uiv (index Natural) (type Integer) (normalized Boolean) (value Natural)) Void]{
Version 1.2.
}
@defproc[(glVertexAttribPointer (index Natural) (size Integer) (type Integer) (normalized Boolean) (stride Integer) (pointer GLPointer)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttribPointer.xml"]{glVertexAttribPointer manpage}.
}
@defproc[(glVertexAttribPointerARB (index Natural) (size Integer) (type Integer) (normalized Boolean) (stride Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttribPointer].
}
@defproc[(glVertexAttribPointerNV (index Natural) (fsize Integer) (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs1dvNV (index Natural) (count Integer) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs1fvNV (index Natural) (count Integer) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs1hvNV (index Natural) (n Integer) (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttribs1svNV (index Natural) (count Integer) (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs2dvNV (index Natural) (count Integer) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs2fvNV (index Natural) (count Integer) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs2hvNV (index Natural) (n Integer) (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttribs2svNV (index Natural) (count Integer) (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs3dvNV (index Natural) (count Integer) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs3fvNV (index Natural) (count Integer) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs3hvNV (index Natural) (n Integer) (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttribs3svNV (index Natural) (count Integer) (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs4dvNV (index Natural) (count Integer) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs4fvNV (index Natural) (count Integer) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs4hvNV (index Natural) (n Integer) (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttribs4svNV (index Natural) (count Integer) (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs4ubvNV (index Natural) (count Integer) (v (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexBindingDivisor (bindingindex Natural) (divisor Natural)) Void]{
Version 4.3.
}
@defproc[(glVertexBlendARB (count Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glVertexBlendEnvfATI (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexBlendEnviATI (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexFormatNV (size Integer) (type Integer) (stride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glVertexP2ui (type Integer) (value Natural)) Void]{
Version 1.2.
}
@defproc[(glVertexP2uiv (type Integer) (value Natural)) Void]{
Version 1.2.
}
@defproc[(glVertexP3ui (type Integer) (value Natural)) Void]{
Version 1.2.
}
@defproc[(glVertexP3uiv (type Integer) (value Natural)) Void]{
Version 1.2.
}
@defproc[(glVertexP4ui (type Integer) (value Natural)) Void]{
Version 1.2.
}
@defproc[(glVertexP4uiv (type Integer) (value Natural)) Void]{
Version 1.2.
}
@defproc[(glVertexPointer (size Integer) (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertexPointer.xml"]{glVertexPointer manpage}.
}
@defproc[(glVertexPointerEXT (size Integer) (type Integer) (stride Integer) (count Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.
}
@defproc[(glVertexPointerListIBM (size Integer) (type Integer) (stride Integer) (pointer (Vectorof GLPointer)) (ptrstride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt"]{@racket[GL_IBM_vertex_array_lists]}.
}
@defproc[(glVertexPointervINTEL (size Integer) (type Integer) (pointer (Vectorof GLPointer))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/INTEL/parallel_arrays.txt"]{@racket[GL_INTEL_parallel_arrays]}.
}
@defproc[(glVertexStream1dATI (stream Integer) (x Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream1dvATI (stream Integer) (coords Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream1fATI (stream Integer) (x Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream1fvATI (stream Integer) (coords Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream1iATI (stream Integer) (x Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream1ivATI (stream Integer) (coords Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream1sATI (stream Integer) (x Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream1svATI (stream Integer) (coords Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2dATI (stream Integer) (x Real) (y Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2dvATI (stream Integer) (coords F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2fATI (stream Integer) (x Flonum) (y Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2fvATI (stream Integer) (coords F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2iATI (stream Integer) (x Integer) (y Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2ivATI (stream Integer) (coords S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2sATI (stream Integer) (x Fixnum) (y Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2svATI (stream Integer) (coords S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3dATI (stream Integer) (x Real) (y Real) (z Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3dvATI (stream Integer) (coords F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3fATI (stream Integer) (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3fvATI (stream Integer) (coords F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3iATI (stream Integer) (x Integer) (y Integer) (z Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3ivATI (stream Integer) (coords S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3sATI (stream Integer) (x Fixnum) (y Fixnum) (z Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3svATI (stream Integer) (coords S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4dATI (stream Integer) (x Real) (y Real) (z Real) (w Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4dvATI (stream Integer) (coords F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4fATI (stream Integer) (x Flonum) (y Flonum) (z Flonum) (w Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4fvATI (stream Integer) (coords F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4iATI (stream Integer) (x Integer) (y Integer) (z Integer) (w Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4ivATI (stream Integer) (coords S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4sATI (stream Integer) (x Fixnum) (y Fixnum) (z Fixnum) (w Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4svATI (stream Integer) (coords S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexWeightPointerEXT (size Integer) (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_weighting.txt"]{@racket[GL_EXT_vertex_weighting]}.
}
@defproc[(glVertexWeightfEXT (weight Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_weighting.txt"]{@racket[GL_EXT_vertex_weighting]}.
}
@defproc[(glVertexWeightfvEXT (weight Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_weighting.txt"]{@racket[GL_EXT_vertex_weighting]}.
}
@defproc[(glVertexWeighthNV (weight Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexWeighthvNV (weight Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVideoCaptureNV (video_capture_slot Natural)) (values Integer Natural Natural)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glVideoCaptureStreamParameterdvNV (video_capture_slot Natural) (stream Natural) (pname Integer) (params F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glVideoCaptureStreamParameterfvNV (video_capture_slot Natural) (stream Natural) (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glVideoCaptureStreamParameterivNV (video_capture_slot Natural) (stream Natural) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glViewport (x Integer) (y Integer) (width Integer) (height Integer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glViewport.xml"]{glViewport manpage}.
}
@defproc[(glViewportArrayv (first Natural) (count Integer) (v F32Vector)) Void]{
Version 4.1.
}
@defproc[(glViewportIndexedf (index Natural) (x Flonum) (y Flonum) (w Flonum) (h Flonum)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glViewportIndexed.xml"]{glViewportIndexed manpage}.
}
@defproc[(glViewportIndexedfv (index Natural) (v F32Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glViewportIndexed.xml"]{glViewportIndexed manpage}.
}
