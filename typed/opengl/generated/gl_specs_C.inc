#lang scribble/manual
@title{glC...}
@defproc[(glCallList (list Natural)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glCallList.xml"]{glCallList manpage}.
}
@defproc[(glCallLists (n Integer) (type Integer) (lists GLPointer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glCallLists.xml"]{glCallLists manpage}.
}
@defproc[(glCheckFramebufferStatus (target Integer)) Integer]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCheckFramebufferStatus.xml"]{glCheckFramebufferStatus manpage}.
}
@defproc[(glCheckFramebufferStatusEXT (target Integer)) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glCheckFramebufferStatus].
}
@defproc[(glCheckNamedFramebufferStatusEXT (framebuffer Natural) (target Integer)) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glClampColor (target Integer) (clamp Integer)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClampColor.xml"]{glClampColor manpage}.
}
@defproc[(glClampColorARB (target Integer) (clamp Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/color_buffer_float.txt"]{@racket[GL_ARB_color_buffer_float]}.

Alias of @racket[glClampColor].
}
@defproc[(glClear (mask Natural)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClear.xml"]{glClear manpage}.
}
@defproc[(glClearAccum (red Flonum) (green Flonum) (blue Flonum) (alpha Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glClearAccum.xml"]{glClearAccum manpage}.
}
@defproc[(glClearBufferData (target Integer) (internalformat Integer) (format Integer) (type Integer) (data GLPointer)) Void]{
Version 4.3.
}
@defproc[(glClearBufferSubData (target Integer) (internalformat Integer) (offset Integer) (size Integer) (format Integer) (type Integer) (data GLPointer)) Void]{
Version 4.3.
}
@defproc[(glClearBufferfi (buffer Integer) (drawbuffer Integer) (depth Flonum) (stencil Integer)) Void]{
Version 3.0.
}
@defproc[(glClearBufferfv (buffer Integer) (drawbuffer Integer) (value F32Vector)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClearBuffer.xml"]{glClearBuffer manpage}.
}
@defproc[(glClearBufferiv (buffer Integer) (drawbuffer Integer) (value S32Vector)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClearBuffer.xml"]{glClearBuffer manpage}.
}
@defproc[(glClearBufferuiv (buffer Integer) (drawbuffer Integer) (value U32Vector)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClearBuffer.xml"]{glClearBuffer manpage}.
}
@defproc[(glClearColor (red Flonum) (green Flonum) (blue Flonum) (alpha Flonum)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClearColor.xml"]{glClearColor manpage}.
}
@defproc[(glClearColorIiEXT (red Integer) (green Integer) (blue Integer) (alpha Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_integer.txt"]{@racket[GL_EXT_texture_integer]}.
}
@defproc[(glClearColorIuiEXT (red Natural) (green Natural) (blue Natural) (alpha Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_integer.txt"]{@racket[GL_EXT_texture_integer]}.
}
@defproc[(glClearDepth (depth Real)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClearDepth.xml"]{glClearDepth manpage}.
}
@defproc[(glClearDepthdNV (depth Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/depth_buffer_float.txt"]{@racket[GL_NV_depth_buffer_float]}.
}
@defproc[(glClearDepthf (d Flonum)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClearDepth.xml"]{glClearDepth manpage}.
}
@defproc[(glClearIndex (c Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glClearIndex.xml"]{glClearIndex manpage}.
}
@defproc[(glClearNamedBufferDataEXT (buffer Natural) (internalformat Integer) (format Integer) (type Integer) (data GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/clear_buffer_object.txt"]{@racket[GL_ARB_clear_buffer_object]}.
}
@defproc[(glClearNamedBufferSubDataEXT (buffer Natural) (internalformat Integer) (offset Integer) (size Integer) (format Integer) (type Integer) (data GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/clear_buffer_object.txt"]{@racket[GL_ARB_clear_buffer_object]}.
}
@defproc[(glClearStencil (s Integer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClearStencil.xml"]{glClearStencil manpage}.
}
@defproc[(glClientActiveTexture (texture Integer)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glClientActiveTexture.xml"]{glClientActiveTexture manpage}.
}
@defproc[(glClientActiveTextureARB (texture Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glClientActiveTexture].
}
@defproc[(glClientActiveVertexStreamATI (stream Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glClientAttribDefaultEXT (mask Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glClientWaitSync (sync GLSync) (flags Natural) (timeout Natural)) Integer]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClientWaitSync.xml"]{glClientWaitSync manpage}.
}
@defproc[(glClipPlane (plane Integer) (equation F64Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glClipPlane.xml"]{glClipPlane manpage}.
}
@defproc[(glColor3b (red Fixnum) (green Fixnum) (blue Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3bv (v S8Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3d (red Real) (green Real) (blue Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3dv (v F64Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3f (red Flonum) (green Flonum) (blue Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3fVertex3fSUN (r Flonum) (g Flonum) (b Flonum) (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor3fVertex3fvSUN (c F32Vector) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor3fv (v F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3hNV (red Nonnegative-Fixnum) (green Nonnegative-Fixnum) (blue Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glColor3hvNV (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glColor3i (red Integer) (green Integer) (blue Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3iv (v S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3s (red Fixnum) (green Fixnum) (blue Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3sv (v S16Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3ub (red Byte) (green Byte) (blue Byte)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3ubv (v (U Bytes String))) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3ui (red Natural) (green Natural) (blue Natural)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3uiv (v U32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3us (red Nonnegative-Fixnum) (green Nonnegative-Fixnum) (blue Nonnegative-Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3usv (v U16Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4b (red Fixnum) (green Fixnum) (blue Fixnum) (alpha Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4bv (v S8Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4d (red Real) (green Real) (blue Real) (alpha Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4dv (v F64Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4f (red Flonum) (green Flonum) (blue Flonum) (alpha Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4fNormal3fVertex3fSUN (r Flonum) (g Flonum) (b Flonum) (a Flonum) (nx Flonum) (ny Flonum) (nz Flonum) (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor4fNormal3fVertex3fvSUN (c F32Vector) (n F32Vector) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor4fv (v F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4hNV (red Nonnegative-Fixnum) (green Nonnegative-Fixnum) (blue Nonnegative-Fixnum) (alpha Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glColor4hvNV (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glColor4i (red Integer) (green Integer) (blue Integer) (alpha Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4iv (v S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4s (red Fixnum) (green Fixnum) (blue Fixnum) (alpha Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4sv (v S16Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4ub (red Byte) (green Byte) (blue Byte) (alpha Byte)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4ubVertex2fSUN (r Byte) (g Byte) (b Byte) (a Byte) (x Flonum) (y Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor4ubVertex2fvSUN (c (U Bytes String)) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor4ubVertex3fSUN (r Byte) (g Byte) (b Byte) (a Byte) (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor4ubVertex3fvSUN (c (U Bytes String)) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor4ubv (v (U Bytes String))) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4ui (red Natural) (green Natural) (blue Natural) (alpha Natural)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4uiv (v U32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4us (red Nonnegative-Fixnum) (green Nonnegative-Fixnum) (blue Nonnegative-Fixnum) (alpha Nonnegative-Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4usv (v U16Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColorFormatNV (size Integer) (type Integer) (stride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glColorFragmentOp1ATI (op Integer) (dst Natural) (dstMask Natural) (dstMod Natural) (arg1 Natural) (arg1Rep Natural) (arg1Mod Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glColorFragmentOp2ATI (op Integer) (dst Natural) (dstMask Natural) (dstMod Natural) (arg1 Natural) (arg1Rep Natural) (arg1Mod Natural) (arg2 Natural) (arg2Rep Natural) (arg2Mod Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glColorFragmentOp3ATI (op Integer) (dst Natural) (dstMask Natural) (dstMod Natural) (arg1 Natural) (arg1Rep Natural) (arg1Mod Natural) (arg2 Natural) (arg2Rep Natural) (arg2Mod Natural) (arg3 Natural) (arg3Rep Natural) (arg3Mod Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glColorMask (red Boolean) (green Boolean) (blue Boolean) (alpha Boolean)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glColorMask.xml"]{glColorMask manpage}.
}
@defproc[(glColorMaskIndexedEXT (index Natural) (r Boolean) (g Boolean) (b Boolean) (a Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/draw_buffers2.txt"]{@racket[GL_EXT_draw_buffers2]}.

Alias of @racket[glColorMaski].
}
@defproc[(glColorMaski (index Natural) (r Boolean) (g Boolean) (b Boolean) (a Boolean)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glColorMask.xml"]{glColorMask manpage}.
}
@defproc[(glColorMaterial (face Integer) (mode Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColorMaterial.xml"]{glColorMaterial manpage}.
}
@defproc[(glColorP3ui (type Integer) (color Natural)) Void]{
Version 1.2.
}
@defproc[(glColorP3uiv (type Integer) (color Natural)) Void]{
Version 1.2.
}
@defproc[(glColorP4ui (type Integer) (color Natural)) Void]{
Version 1.2.
}
@defproc[(glColorP4uiv (type Integer) (color Natural)) Void]{
Version 1.2.
}
@defproc[(glColorPointer (size Integer) (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColorPointer.xml"]{glColorPointer manpage}.
}
@defproc[(glColorPointerEXT (size Integer) (type Integer) (stride Integer) (count Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.
}
@defproc[(glColorPointerListIBM (size Integer) (type Integer) (stride Integer) (pointer (Vectorof GLPointer)) (ptrstride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt"]{@racket[GL_IBM_vertex_array_lists]}.
}
@defproc[(glColorPointervINTEL (size Integer) (type Integer) (pointer (Vectorof GLPointer))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/INTEL/parallel_arrays.txt"]{@racket[GL_INTEL_parallel_arrays]}.
}
@defproc[(glColorSubTable (target Integer) (start Integer) (count Integer) (format Integer) (type Integer) (data GLPointer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColorSubTable.xml"]{glColorSubTable manpage}.
}
@defproc[(glColorSubTableEXT (target Integer) (start Integer) (count Integer) (format Integer) (type Integer) (data GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/color_subtable.txt"]{@racket[GL_EXT_color_subtable]}.

Alias of @racket[glColorSubTable].
}
@defproc[(glColorTable (target Integer) (internalformat Integer) (width Integer) (format Integer) (type Integer) (table GLPointer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColorTable.xml"]{glColorTable manpage}.
}
@defproc[(glColorTableEXT (target Integer) (internalFormat Integer) (width Integer) (format Integer) (type Integer) (table GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/paletted_texture.txt"]{@racket[GL_EXT_paletted_texture]}.

Alias of @racket[glColorTable].
}
@defproc[(glColorTableParameterfv (target Integer) (pname Integer) (params F32Vector)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColorTableParameter.xml"]{glColorTableParameter manpage}.
}
@defproc[(glColorTableParameterfvSGI (target Integer) (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGI/color_table.txt"]{@racket[GL_SGI_color_table]}.

Alias of @racket[glColorTableParameterfv].
}
@defproc[(glColorTableParameteriv (target Integer) (pname Integer) (params S32Vector)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColorTableParameter.xml"]{glColorTableParameter manpage}.
}
@defproc[(glColorTableParameterivSGI (target Integer) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGI/color_table.txt"]{@racket[GL_SGI_color_table]}.

Alias of @racket[glColorTableParameteriv].
}
@defproc[(glColorTableSGI (target Integer) (internalformat Integer) (width Integer) (format Integer) (type Integer) (table GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGI/color_table.txt"]{@racket[GL_SGI_color_table]}.

Alias of @racket[glColorTable].
}
@defproc[(glCombinerInputNV (stage Integer) (portion Integer) (variable Integer) (input Integer) (mapping Integer) (componentUsage Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glCombinerOutputNV (stage Integer) (portion Integer) (abOutput Integer) (cdOutput Integer) (sumOutput Integer) (scale Integer) (bias Integer) (abDotProduct Boolean) (cdDotProduct Boolean) (muxSum Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glCombinerParameterfNV (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glCombinerParameterfvNV (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glCombinerParameteriNV (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glCombinerParameterivNV (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glCombinerStageParameterfvNV (stage Integer) (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners2.txt"]{@racket[GL_NV_register_combiners2]}.
}
@defproc[(glCompileShader (shader Natural)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCompileShader.xml"]{glCompileShader manpage}.
}
@defproc[(glCompileShaderARB (shaderObj Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glCompileShader].
}
@defproc[(glCompileShaderIncludeARB (shader Natural) (count Integer) (path (Vectorof (U Bytes String))) (length S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shading_language_include.txt"]{@racket[GL_ARB_shading_language_include]}.
}
@defproc[(glCompressedMultiTexImage1DEXT (texunit Integer) (target Integer) (level Integer) (internalformat Integer) (width Integer) (border Integer) (imageSize Integer) (bits GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedMultiTexImage2DEXT (texunit Integer) (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (border Integer) (imageSize Integer) (bits GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedMultiTexImage3DEXT (texunit Integer) (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (depth Integer) (border Integer) (imageSize Integer) (bits GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedMultiTexSubImage1DEXT (texunit Integer) (target Integer) (level Integer) (xoffset Integer) (width Integer) (format Integer) (imageSize Integer) (bits GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedMultiTexSubImage2DEXT (texunit Integer) (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (width Integer) (height Integer) (format Integer) (imageSize Integer) (bits GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedMultiTexSubImage3DEXT (texunit Integer) (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (zoffset Integer) (width Integer) (height Integer) (depth Integer) (format Integer) (imageSize Integer) (bits GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedTexImage1D (target Integer) (level Integer) (internalformat Integer) (width Integer) (border Integer) (imageSize Integer) (data GLPointer)) Void]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCompressedTexImage1D.xml"]{glCompressedTexImage1D manpage}.
}
@defproc[(glCompressedTexImage1DARB (target Integer) (level Integer) (internalformat Integer) (width Integer) (border Integer) (imageSize Integer) (data GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_compression.txt"]{@racket[GL_ARB_texture_compression]}.

Alias of @racket[glCompressedTexImage1D].
}
@defproc[(glCompressedTexImage2D (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (border Integer) (imageSize Integer) (data GLPointer)) Void]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCompressedTexImage2D.xml"]{glCompressedTexImage2D manpage}.
}
@defproc[(glCompressedTexImage2DARB (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (border Integer) (imageSize Integer) (data GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_compression.txt"]{@racket[GL_ARB_texture_compression]}.

Alias of @racket[glCompressedTexImage2D].
}
@defproc[(glCompressedTexImage3D (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (depth Integer) (border Integer) (imageSize Integer) (data GLPointer)) Void]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCompressedTexImage3D.xml"]{glCompressedTexImage3D manpage}.
}
@defproc[(glCompressedTexImage3DARB (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (depth Integer) (border Integer) (imageSize Integer) (data GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_compression.txt"]{@racket[GL_ARB_texture_compression]}.

Alias of @racket[glCompressedTexImage3D].
}
@defproc[(glCompressedTexSubImage1D (target Integer) (level Integer) (xoffset Integer) (width Integer) (format Integer) (imageSize Integer) (data GLPointer)) Void]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCompressedTexSubImage1D.xml"]{glCompressedTexSubImage1D manpage}.
}
@defproc[(glCompressedTexSubImage1DARB (target Integer) (level Integer) (xoffset Integer) (width Integer) (format Integer) (imageSize Integer) (data GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_compression.txt"]{@racket[GL_ARB_texture_compression]}.

Alias of @racket[glCompressedTexSubImage1D].
}
@defproc[(glCompressedTexSubImage2D (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (width Integer) (height Integer) (format Integer) (imageSize Integer) (data GLPointer)) Void]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCompressedTexSubImage2D.xml"]{glCompressedTexSubImage2D manpage}.
}
@defproc[(glCompressedTexSubImage2DARB (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (width Integer) (height Integer) (format Integer) (imageSize Integer) (data GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_compression.txt"]{@racket[GL_ARB_texture_compression]}.

Alias of @racket[glCompressedTexSubImage2D].
}
@defproc[(glCompressedTexSubImage3D (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (zoffset Integer) (width Integer) (height Integer) (depth Integer) (format Integer) (imageSize Integer) (data GLPointer)) Void]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCompressedTexSubImage3D.xml"]{glCompressedTexSubImage3D manpage}.
}
@defproc[(glCompressedTexSubImage3DARB (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (zoffset Integer) (width Integer) (height Integer) (depth Integer) (format Integer) (imageSize Integer) (data GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_compression.txt"]{@racket[GL_ARB_texture_compression]}.

Alias of @racket[glCompressedTexSubImage3D].
}
@defproc[(glCompressedTextureImage1DEXT (texture Natural) (target Integer) (level Integer) (internalformat Integer) (width Integer) (border Integer) (imageSize Integer) (bits GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedTextureImage2DEXT (texture Natural) (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (border Integer) (imageSize Integer) (bits GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedTextureImage3DEXT (texture Natural) (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (depth Integer) (border Integer) (imageSize Integer) (bits GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedTextureSubImage1DEXT (texture Natural) (target Integer) (level Integer) (xoffset Integer) (width Integer) (format Integer) (imageSize Integer) (bits GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedTextureSubImage2DEXT (texture Natural) (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (width Integer) (height Integer) (format Integer) (imageSize Integer) (bits GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedTextureSubImage3DEXT (texture Natural) (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (zoffset Integer) (width Integer) (height Integer) (depth Integer) (format Integer) (imageSize Integer) (bits GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glConvolutionFilter1D (target Integer) (internalformat Integer) (width Integer) (format Integer) (type Integer) (image GLPointer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionFilter1D.xml"]{glConvolutionFilter1D manpage}.
}
@defproc[(glConvolutionFilter1DEXT (target Integer) (internalformat Integer) (width Integer) (format Integer) (type Integer) (image GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glConvolutionFilter1D].
}
@defproc[(glConvolutionFilter2D (target Integer) (internalformat Integer) (width Integer) (height Integer) (format Integer) (type Integer) (image GLPointer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionFilter2D.xml"]{glConvolutionFilter2D manpage}.
}
@defproc[(glConvolutionFilter2DEXT (target Integer) (internalformat Integer) (width Integer) (height Integer) (format Integer) (type Integer) (image GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glConvolutionFilter2D].
}
@defproc[(glConvolutionParameterf (target Integer) (pname Integer) (params Flonum)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml"]{glConvolutionParameter manpage}.
}
@defproc[(glConvolutionParameterfEXT (target Integer) (pname Integer) (params Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glConvolutionParameterf].
}
@defproc[(glConvolutionParameterfv (target Integer) (pname Integer) (params F32Vector)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml"]{glConvolutionParameter manpage}.
}
@defproc[(glConvolutionParameterfvEXT (target Integer) (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glConvolutionParameterfv].
}
@defproc[(glConvolutionParameteri (target Integer) (pname Integer) (params Integer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml"]{glConvolutionParameter manpage}.
}
@defproc[(glConvolutionParameteriEXT (target Integer) (pname Integer) (params Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glConvolutionParameteri].
}
@defproc[(glConvolutionParameteriv (target Integer) (pname Integer) (params S32Vector)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml"]{glConvolutionParameter manpage}.
}
@defproc[(glConvolutionParameterivEXT (target Integer) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glConvolutionParameteriv].
}
@defproc[(glCopyBufferSubData (readTarget Integer) (writeTarget Integer) (readOffset Integer) (writeOffset Integer) (size Integer)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCopyBufferSubData.xml"]{glCopyBufferSubData manpage}.
}
@defproc[(glCopyColorSubTable (target Integer) (start Integer) (x Integer) (y Integer) (width Integer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glCopyColorSubTable.xml"]{glCopyColorSubTable manpage}.
}
@defproc[(glCopyColorSubTableEXT (target Integer) (start Integer) (x Integer) (y Integer) (width Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/color_subtable.txt"]{@racket[GL_EXT_color_subtable]}.

Alias of @racket[glCopyColorSubTable].
}
@defproc[(glCopyColorTable (target Integer) (internalformat Integer) (x Integer) (y Integer) (width Integer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glCopyColorTable.xml"]{glCopyColorTable manpage}.
}
@defproc[(glCopyColorTableSGI (target Integer) (internalformat Integer) (x Integer) (y Integer) (width Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGI/color_table.txt"]{@racket[GL_SGI_color_table]}.

Alias of @racket[glCopyColorTable].
}
@defproc[(glCopyConvolutionFilter1D (target Integer) (internalformat Integer) (x Integer) (y Integer) (width Integer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glCopyConvolutionFilter1D.xml"]{glCopyConvolutionFilter1D manpage}.
}
@defproc[(glCopyConvolutionFilter1DEXT (target Integer) (internalformat Integer) (x Integer) (y Integer) (width Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glCopyConvolutionFilter1D].
}
@defproc[(glCopyConvolutionFilter2D (target Integer) (internalformat Integer) (x Integer) (y Integer) (width Integer) (height Integer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glCopyConvolutionFilter2D.xml"]{glCopyConvolutionFilter2D manpage}.
}
@defproc[(glCopyConvolutionFilter2DEXT (target Integer) (internalformat Integer) (x Integer) (y Integer) (width Integer) (height Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glCopyConvolutionFilter2D].
}
@defproc[(glCopyImageSubData (srcName Natural) (srcTarget Integer) (srcLevel Integer) (srcX Integer) (srcY Integer) (srcZ Integer) (dstName Natural) (dstTarget Integer) (dstLevel Integer) (dstX Integer) (dstY Integer) (dstZ Integer) (srcWidth Integer) (srcHeight Integer) (srcDepth Integer)) Void]{
Version 4.3.
}
@defproc[(glCopyImageSubDataNV (srcName Natural) (srcTarget Integer) (srcLevel Integer) (srcX Integer) (srcY Integer) (srcZ Integer) (dstName Natural) (dstTarget Integer) (dstLevel Integer) (dstX Integer) (dstY Integer) (dstZ Integer) (width Integer) (height Integer) (depth Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/copy_image.txt"]{@racket[GL_NV_copy_image]}.
}
@defproc[(glCopyMultiTexImage1DEXT (texunit Integer) (target Integer) (level Integer) (internalformat Integer) (x Integer) (y Integer) (width Integer) (border Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyMultiTexImage2DEXT (texunit Integer) (target Integer) (level Integer) (internalformat Integer) (x Integer) (y Integer) (width Integer) (height Integer) (border Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyMultiTexSubImage1DEXT (texunit Integer) (target Integer) (level Integer) (xoffset Integer) (x Integer) (y Integer) (width Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyMultiTexSubImage2DEXT (texunit Integer) (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (x Integer) (y Integer) (width Integer) (height Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyMultiTexSubImage3DEXT (texunit Integer) (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (zoffset Integer) (x Integer) (y Integer) (width Integer) (height Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyPathNV (resultPath Natural) (srcPath Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glCopyPixels (x Integer) (y Integer) (width Integer) (height Integer) (type Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glCopyPixels.xml"]{glCopyPixels manpage}.
}
@defproc[(glCopyTexImage1D (target Integer) (level Integer) (internalformat Integer) (x Integer) (y Integer) (width Integer) (border Integer)) Void]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCopyTexImage1D.xml"]{glCopyTexImage1D manpage}.
}
@defproc[(glCopyTexImage1DEXT (target Integer) (level Integer) (internalformat Integer) (x Integer) (y Integer) (width Integer) (border Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/copy_texture.txt"]{@racket[GL_EXT_copy_texture]}.

Alias of @racket[glCopyTexImage1D].
}
@defproc[(glCopyTexImage2D (target Integer) (level Integer) (internalformat Integer) (x Integer) (y Integer) (width Integer) (height Integer) (border Integer)) Void]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCopyTexImage2D.xml"]{glCopyTexImage2D manpage}.
}
@defproc[(glCopyTexImage2DEXT (target Integer) (level Integer) (internalformat Integer) (x Integer) (y Integer) (width Integer) (height Integer) (border Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/copy_texture.txt"]{@racket[GL_EXT_copy_texture]}.

Alias of @racket[glCopyTexImage2D].
}
@defproc[(glCopyTexSubImage1D (target Integer) (level Integer) (xoffset Integer) (x Integer) (y Integer) (width Integer)) Void]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCopyTexSubImage1D.xml"]{glCopyTexSubImage1D manpage}.
}
@defproc[(glCopyTexSubImage1DEXT (target Integer) (level Integer) (xoffset Integer) (x Integer) (y Integer) (width Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/copy_texture.txt"]{@racket[GL_EXT_copy_texture]}.

Alias of @racket[glCopyTexSubImage1D].
}
@defproc[(glCopyTexSubImage2D (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (x Integer) (y Integer) (width Integer) (height Integer)) Void]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCopyTexSubImage2D.xml"]{glCopyTexSubImage2D manpage}.
}
@defproc[(glCopyTexSubImage2DEXT (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (x Integer) (y Integer) (width Integer) (height Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/copy_texture.txt"]{@racket[GL_EXT_copy_texture]}.

Alias of @racket[glCopyTexSubImage2D].
}
@defproc[(glCopyTexSubImage3D (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (zoffset Integer) (x Integer) (y Integer) (width Integer) (height Integer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCopyTexSubImage3D.xml"]{glCopyTexSubImage3D manpage}.
}
@defproc[(glCopyTexSubImage3DEXT (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (zoffset Integer) (x Integer) (y Integer) (width Integer) (height Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/copy_texture.txt"]{@racket[GL_EXT_copy_texture]}.

Alias of @racket[glCopyTexSubImage3D].
}
@defproc[(glCopyTextureImage1DEXT (texture Natural) (target Integer) (level Integer) (internalformat Integer) (x Integer) (y Integer) (width Integer) (border Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyTextureImage2DEXT (texture Natural) (target Integer) (level Integer) (internalformat Integer) (x Integer) (y Integer) (width Integer) (height Integer) (border Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyTextureSubImage1DEXT (texture Natural) (target Integer) (level Integer) (xoffset Integer) (x Integer) (y Integer) (width Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyTextureSubImage2DEXT (texture Natural) (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (x Integer) (y Integer) (width Integer) (height Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyTextureSubImage3DEXT (texture Natural) (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (zoffset Integer) (x Integer) (y Integer) (width Integer) (height Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCoverFillPathInstancedNV (numPaths Integer) (pathNameType Integer) (paths GLPointer) (pathBase Natural) (coverMode Integer) (transformType Integer) (transformValues F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glCoverFillPathNV (path Natural) (coverMode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glCoverStrokePathInstancedNV (numPaths Integer) (pathNameType Integer) (paths GLPointer) (pathBase Natural) (coverMode Integer) (transformType Integer) (transformValues F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glCoverStrokePathNV (path Natural) (coverMode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glCreateProgram) Natural]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCreateProgram.xml"]{glCreateProgram manpage}.
}
@defproc[(glCreateProgramObjectARB) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glCreateProgram].
}
@defproc[(glCreateShader (type Integer)) Natural]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCreateShader.xml"]{glCreateShader manpage}.
}
@defproc[(glCreateShaderObjectARB (shaderType Integer)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glCreateShader].
}
@defproc[(glCreateShaderProgramEXT (type Integer) (string (U Bytes String))) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/separate_shader_objects.txt"]{@racket[GL_EXT_separate_shader_objects]}.
}
@defproc[(glCreateShaderProgramv (type Integer) (count Integer) (strings (Vectorof (U Bytes String)))) Natural]{
Version 4.1.
}
@defproc[(glCreateSyncFromCLeventARB (context CPointer) (event CPointer) (flags Natural)) GLSync]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/cl_event.txt"]{@racket[GL_ARB_cl_event]}.
}
@defproc[(glCullFace (mode Integer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCullFace.xml"]{glCullFace manpage}.
}
@defproc[(glCullParameterdvEXT (pname Integer)) F64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/cull_vertex.txt"]{@racket[GL_EXT_cull_vertex]}.
}
@defproc[(glCullParameterfvEXT (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/cull_vertex.txt"]{@racket[GL_EXT_cull_vertex]}.
}
@defproc[(glCurrentPaletteMatrixARB (index Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/matrix_palette.txt"]{@racket[GL_ARB_matrix_palette]}.
}
