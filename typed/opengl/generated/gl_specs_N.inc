#lang scribble/manual
@title{glN...}
@defproc[(glNamedBufferDataEXT (buffer Natural) (size Integer) (data GLPointer) (usage Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedBufferSubDataEXT (buffer Natural) (offset Integer) (size Integer) (data GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedCopyBufferSubDataEXT (readBuffer Natural) (writeBuffer Natural) (readOffset Integer) (writeOffset Integer) (size Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedFramebufferParameteriEXT (framebuffer Natural) (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/framebuffer_no_attachments.txt"]{@racket[GL_ARB_framebuffer_no_attachments]}.
}
@defproc[(glNamedFramebufferRenderbufferEXT (framebuffer Natural) (attachment Integer) (renderbuffertarget Integer) (renderbuffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedFramebufferTexture1DEXT (framebuffer Natural) (attachment Integer) (textarget Integer) (texture Natural) (level Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedFramebufferTexture2DEXT (framebuffer Natural) (attachment Integer) (textarget Integer) (texture Natural) (level Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedFramebufferTexture3DEXT (framebuffer Natural) (attachment Integer) (textarget Integer) (texture Natural) (level Integer) (zoffset Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedFramebufferTextureEXT (framebuffer Natural) (attachment Integer) (texture Natural) (level Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedFramebufferTextureFaceEXT (framebuffer Natural) (attachment Integer) (texture Natural) (level Integer) (face Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedFramebufferTextureLayerEXT (framebuffer Natural) (attachment Integer) (texture Natural) (level Integer) (layer Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameter4dEXT (program Natural) (target Integer) (index Natural) (x Real) (y Real) (z Real) (w Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameter4dvEXT (program Natural) (target Integer) (index Natural) (params F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameter4fEXT (program Natural) (target Integer) (index Natural) (x Flonum) (y Flonum) (z Flonum) (w Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameter4fvEXT (program Natural) (target Integer) (index Natural) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameterI4iEXT (program Natural) (target Integer) (index Natural) (x Integer) (y Integer) (z Integer) (w Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameterI4ivEXT (program Natural) (target Integer) (index Natural) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameterI4uiEXT (program Natural) (target Integer) (index Natural) (x Natural) (y Natural) (z Natural) (w Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameterI4uivEXT (program Natural) (target Integer) (index Natural) (params U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameters4fvEXT (program Natural) (target Integer) (index Natural) (count Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParametersI4ivEXT (program Natural) (target Integer) (index Natural) (count Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParametersI4uivEXT (program Natural) (target Integer) (index Natural) (count Integer) (params U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramStringEXT (program Natural) (target Integer) (format Integer) (len Integer) (string GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedRenderbufferStorageEXT (renderbuffer Natural) (internalformat Integer) (width Integer) (height Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedRenderbufferStorageMultisampleCoverageEXT (renderbuffer Natural) (coverageSamples Integer) (colorSamples Integer) (internalformat Integer) (width Integer) (height Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedRenderbufferStorageMultisampleEXT (renderbuffer Natural) (samples Integer) (internalformat Integer) (width Integer) (height Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedStringARB (type Integer) (namelen Integer) (name (U Bytes String)) (stringlen Integer) (string (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shading_language_include.txt"]{@racket[GL_ARB_shading_language_include]}.
}
@defproc[(glNewList (list Natural) (mode Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNewList.xml"]{glNewList manpage}.
}
@defproc[(glNewObjectBufferATI (size Integer) (pointer GLPointer) (usage Integer)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glNormal3b (nx Fixnum) (ny Fixnum) (nz Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3bv (v S8Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3d (nx Real) (ny Real) (nz Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3dv (v F64Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3f (nx Flonum) (ny Flonum) (nz Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3fVertex3fSUN (nx Flonum) (ny Flonum) (nz Flonum) (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glNormal3fVertex3fvSUN (n F32Vector) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glNormal3fv (v F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3hNV (nx Nonnegative-Fixnum) (ny Nonnegative-Fixnum) (nz Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glNormal3hvNV (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glNormal3i (nx Integer) (ny Integer) (nz Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3iv (v S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3s (nx Fixnum) (ny Fixnum) (nz Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3sv (v S16Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormalFormatNV (type Integer) (stride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glNormalP3ui (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glNormalP3uiv (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glNormalPointer (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormalPointer.xml"]{glNormalPointer manpage}.
}
@defproc[(glNormalPointerEXT (type Integer) (stride Integer) (count Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.
}
@defproc[(glNormalPointerListIBM (type Integer) (stride Integer) (pointer (Vectorof GLPointer)) (ptrstride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt"]{@racket[GL_IBM_vertex_array_lists]}.
}
@defproc[(glNormalPointervINTEL (type Integer) (pointer (Vectorof GLPointer))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/INTEL/parallel_arrays.txt"]{@racket[GL_INTEL_parallel_arrays]}.
}
@defproc[(glNormalStream3bATI (stream Integer) (nx Fixnum) (ny Fixnum) (nz Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3bvATI (stream Integer) (coords S8Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3dATI (stream Integer) (nx Real) (ny Real) (nz Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3dvATI (stream Integer) (coords F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3fATI (stream Integer) (nx Flonum) (ny Flonum) (nz Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3fvATI (stream Integer) (coords F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3iATI (stream Integer) (nx Integer) (ny Integer) (nz Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3ivATI (stream Integer) (coords S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3sATI (stream Integer) (nx Fixnum) (ny Fixnum) (nz Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3svATI (stream Integer) (coords S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
