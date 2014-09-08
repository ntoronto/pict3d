#lang scribble/manual
@title{glM...}
@defproc[(glMakeBufferNonResidentNV (target Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glMakeBufferResidentNV (target Integer) (access Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glMakeImageHandleNonResidentNV (handle Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glMakeImageHandleResidentNV (handle Natural) (access Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glMakeNamedBufferNonResidentNV (buffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glMakeNamedBufferResidentNV (buffer Natural) (access Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glMakeTextureHandleNonResidentNV (handle Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glMakeTextureHandleResidentNV (handle Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glMap1d (target Integer) (u1 Real) (u2 Real) (stride Integer) (order Integer) (points F64Vector)) Void]{
Deprecated in version 3.1.
}
@defproc[(glMap1f (target Integer) (u1 Flonum) (u2 Flonum) (stride Integer) (order Integer) (points F32Vector)) Void]{
Deprecated in version 3.1.
}
@defproc[(glMap2d (target Integer) (u1 Real) (u2 Real) (ustride Integer) (uorder Integer) (v1 Real) (v2 Real) (vstride Integer) (vorder Integer) (points F64Vector)) Void]{
Deprecated in version 3.1.
}
@defproc[(glMap2f (target Integer) (u1 Flonum) (u2 Flonum) (ustride Integer) (uorder Integer) (v1 Flonum) (v2 Flonum) (vstride Integer) (vorder Integer) (points F32Vector)) Void]{
Deprecated in version 3.1.
}
@defproc[(glMapBuffer (target Integer) (access Integer)) GLPointer]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glMapBuffer.xml"]{glMapBuffer manpage}.
}
@defproc[(glMapBufferARB (target Integer) (access Integer)) GLPointer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glMapBuffer].
}
@defproc[(glMapBufferRange (target Integer) (offset Integer) (length Integer) (access Natural)) GLPointer]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glMapBufferRange.xml"]{glMapBufferRange manpage}.
}
@defproc[(glMapControlPointsNV (target Integer) (index Natural) (type Integer) (ustride Integer) (vstride Integer) (uorder Integer) (vorder Integer) (packed Boolean) (points GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glMapGrid1d (un Integer) (u1 Real) (u2 Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml"]{glMapGrid manpage}.
}
@defproc[(glMapGrid1f (un Integer) (u1 Flonum) (u2 Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml"]{glMapGrid manpage}.
}
@defproc[(glMapGrid2d (un Integer) (u1 Real) (u2 Real) (vn Integer) (v1 Real) (v2 Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml"]{glMapGrid manpage}.
}
@defproc[(glMapGrid2f (un Integer) (u1 Flonum) (u2 Flonum) (vn Integer) (v1 Flonum) (v2 Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml"]{glMapGrid manpage}.
}
@defproc[(glMapNamedBufferEXT (buffer Natural) (access Integer)) GLPointer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMapNamedBufferRangeEXT (buffer Natural) (offset Integer) (length Integer) (access Natural)) GLPointer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMapObjectBufferATI (buffer Natural)) GLPointer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/map_object_buffer.txt"]{@racket[GL_ATI_map_object_buffer]}.
}
@defproc[(glMapParameterfvNV (target Integer) (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glMapParameterivNV (target Integer) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glMapVertexAttrib1dAPPLE (index Natural) (size Natural) (u1 Real) (u2 Real) (stride Integer) (order Integer) (points F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_program_evaluators.txt"]{@racket[GL_APPLE_vertex_program_evaluators]}.
}
@defproc[(glMapVertexAttrib1fAPPLE (index Natural) (size Natural) (u1 Flonum) (u2 Flonum) (stride Integer) (order Integer) (points F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_program_evaluators.txt"]{@racket[GL_APPLE_vertex_program_evaluators]}.
}
@defproc[(glMapVertexAttrib2dAPPLE (index Natural) (size Natural) (u1 Real) (u2 Real) (ustride Integer) (uorder Integer) (v1 Real) (v2 Real) (vstride Integer) (vorder Integer) (points F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_program_evaluators.txt"]{@racket[GL_APPLE_vertex_program_evaluators]}.
}
@defproc[(glMapVertexAttrib2fAPPLE (index Natural) (size Natural) (u1 Flonum) (u2 Flonum) (ustride Integer) (uorder Integer) (v1 Flonum) (v2 Flonum) (vstride Integer) (vorder Integer) (points F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_program_evaluators.txt"]{@racket[GL_APPLE_vertex_program_evaluators]}.
}
@defproc[(glMaterialf (face Integer) (pname Integer) (param Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml"]{glMaterial manpage}.
}
@defproc[(glMaterialfv (face Integer) (pname Integer) (params F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml"]{glMaterial manpage}.
}
@defproc[(glMateriali (face Integer) (pname Integer) (param Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml"]{glMaterial manpage}.
}
@defproc[(glMaterialiv (face Integer) (pname Integer) (params S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml"]{glMaterial manpage}.
}
@defproc[(glMatrixFrustumEXT (mode Integer) (left Real) (right Real) (bottom Real) (top Real) (zNear Real) (zFar Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixIndexPointerARB (size Integer) (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/matrix_palette.txt"]{@racket[GL_ARB_matrix_palette]}.
}
@defproc[(glMatrixIndexubvARB (size Integer) (indices (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/matrix_palette.txt"]{@racket[GL_ARB_matrix_palette]}.
}
@defproc[(glMatrixIndexuivARB (size Integer) (indices U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/matrix_palette.txt"]{@racket[GL_ARB_matrix_palette]}.
}
@defproc[(glMatrixIndexusvARB (size Integer) (indices U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/matrix_palette.txt"]{@racket[GL_ARB_matrix_palette]}.
}
@defproc[(glMatrixLoadIdentityEXT (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixLoadTransposedEXT (mode Integer) (m F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixLoadTransposefEXT (mode Integer) (m F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixLoaddEXT (mode Integer) (m F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixLoadfEXT (mode Integer) (m F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixMode (mode Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMatrixMode.xml"]{glMatrixMode manpage}.
}
@defproc[(glMatrixMultTransposedEXT (mode Integer) (m F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixMultTransposefEXT (mode Integer) (m F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixMultdEXT (mode Integer) (m F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixMultfEXT (mode Integer) (m F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixOrthoEXT (mode Integer) (left Real) (right Real) (bottom Real) (top Real) (zNear Real) (zFar Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixPopEXT (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixPushEXT (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixRotatedEXT (mode Integer) (angle Real) (x Real) (y Real) (z Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixRotatefEXT (mode Integer) (angle Flonum) (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixScaledEXT (mode Integer) (x Real) (y Real) (z Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixScalefEXT (mode Integer) (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixTranslatedEXT (mode Integer) (x Real) (y Real) (z Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixTranslatefEXT (mode Integer) (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMemoryBarrier (barriers Natural)) Void]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glMemoryBarrier.xml"]{glMemoryBarrier manpage}.
}
@defproc[(glMemoryBarrierEXT (barriers Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/shader_image_load_store.txt"]{@racket[GL_EXT_shader_image_load_store]}.
}
@defproc[(glMinSampleShading (value Flonum)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glMinSampleShading.xml"]{glMinSampleShading manpage}.
}
@defproc[(glMinSampleShadingARB (value Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/sample_shading.txt"]{@racket[GL_ARB_sample_shading]}.

Alias of @racket[glMinSampleShading].
}
@defproc[(glMinmax (target Integer) (internalformat Integer) (sink Boolean)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMinmax.xml"]{glMinmax manpage}.
}
@defproc[(glMinmaxEXT (target Integer) (internalformat Integer) (sink Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/histogram.txt"]{@racket[GL_EXT_histogram]}.

Alias of @racket[glMinmax].
}
@defproc[(glMultMatrixd (m F64Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultMatrix.xml"]{glMultMatrix manpage}.
}
@defproc[(glMultMatrixf (m F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultMatrix.xml"]{glMultMatrix manpage}.
}
@defproc[(glMultTransposeMatrixd (m F64Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultTransposeMatrix.xml"]{glMultTransposeMatrix manpage}.
}
@defproc[(glMultTransposeMatrixdARB (m F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/transpose_matrix.txt"]{@racket[GL_ARB_transpose_matrix]}.

Alias of @racket[glMultTransposeMatrixd].
}
@defproc[(glMultTransposeMatrixf (m F32Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultTransposeMatrix.xml"]{glMultTransposeMatrix manpage}.
}
@defproc[(glMultTransposeMatrixfARB (m F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/transpose_matrix.txt"]{@racket[GL_ARB_transpose_matrix]}.

Alias of @racket[glMultTransposeMatrixf].
}
@defproc[(glMultiDrawArrays (mode Integer) (first S32Vector) (count S32Vector) (drawcount Integer)) Void]{
Version 1.4.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glMultiDrawArrays.xml"]{glMultiDrawArrays manpage}.
}
@defproc[(glMultiDrawArraysEXT (mode Integer) (first S32Vector) (count S32Vector) (primcount Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/multi_draw_arrays.txt"]{@racket[GL_EXT_multi_draw_arrays]}.

Alias of @racket[glMultiDrawArrays].
}
@defproc[(glMultiDrawArraysIndirect (mode Integer) (indirect GLPointer) (drawcount Integer) (stride Integer)) Void]{
Version 4.3.
}
@defproc[(glMultiDrawArraysIndirectAMD (mode Integer) (indirect GLPointer) (primcount Integer) (stride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/multi_draw_indirect.txt"]{@racket[GL_AMD_multi_draw_indirect]}.
}
@defproc[(glMultiDrawElementArrayAPPLE (mode Integer) (first S32Vector) (count S32Vector) (primcount Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/element_array.txt"]{@racket[GL_APPLE_element_array]}.
}
@defproc[(glMultiDrawElements (mode Integer) (count S32Vector) (type Integer) (indices (Vectorof GLPointer)) (drawcount Integer)) Void]{
Version 1.4.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glMultiDrawElements.xml"]{glMultiDrawElements manpage}.
}
@defproc[(glMultiDrawElementsBaseVertex (mode Integer) (count S32Vector) (type Integer) (indices (Vectorof GLPointer)) (drawcount Integer) (basevertex S32Vector)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glMultiDrawElementsBaseVertex.xml"]{glMultiDrawElementsBaseVertex manpage}.
}
@defproc[(glMultiDrawElementsEXT (mode Integer) (count S32Vector) (type Integer) (indices (Vectorof GLPointer)) (primcount Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/multi_draw_arrays.txt"]{@racket[GL_EXT_multi_draw_arrays]}.

Alias of @racket[glMultiDrawElements].
}
@defproc[(glMultiDrawElementsIndirect (mode Integer) (type Integer) (indirect GLPointer) (drawcount Integer) (stride Integer)) Void]{
Version 4.3.
}
@defproc[(glMultiDrawElementsIndirectAMD (mode Integer) (type Integer) (indirect GLPointer) (primcount Integer) (stride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/multi_draw_indirect.txt"]{@racket[GL_AMD_multi_draw_indirect]}.
}
@defproc[(glMultiDrawRangeElementArrayAPPLE (mode Integer) (start Natural) (end Natural) (first S32Vector) (count S32Vector) (primcount Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/element_array.txt"]{@racket[GL_APPLE_element_array]}.
}
@defproc[(glMultiModeDrawArraysIBM (mode (Vectorof Integer)) (first S32Vector) (count S32Vector) (primcount Integer) (modestride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/multimode_draw_arrays.txt"]{@racket[GL_IBM_multimode_draw_arrays]}.
}
@defproc[(glMultiModeDrawElementsIBM (mode (Vectorof Integer)) (count S32Vector) (type Integer) (indices (Vectorof GLPointer)) (primcount Integer) (modestride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/multimode_draw_arrays.txt"]{@racket[GL_IBM_multimode_draw_arrays]}.
}
@defproc[(glMultiTexBufferEXT (texunit Integer) (target Integer) (internalformat Integer) (buffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexCoord1d (target Integer) (s Real)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1dARB (target Integer) (s Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord1dv (target Integer) (v Real)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1dvARB (target Integer) (v Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord1dv].
}
@defproc[(glMultiTexCoord1f (target Integer) (s Flonum)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1fARB (target Integer) (s Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord1fv (target Integer) (v Flonum)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1fvARB (target Integer) (v Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord1fv].
}
@defproc[(glMultiTexCoord1hNV (target Integer) (s Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord1hvNV (target Integer) (v Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord1i (target Integer) (s Integer)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1iARB (target Integer) (s Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord1iv (target Integer) (v Integer)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1ivARB (target Integer) (v Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord1iv].
}
@defproc[(glMultiTexCoord1s (target Integer) (s Fixnum)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1sARB (target Integer) (s Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord1sv (target Integer) (v Fixnum)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1svARB (target Integer) (v Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord1sv].
}
@defproc[(glMultiTexCoord2d (target Integer) (s Real) (t Real)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2dARB (target Integer) (s Real) (t Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord2dv (target Integer) (v F64Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2dvARB (target Integer) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord2dv].
}
@defproc[(glMultiTexCoord2f (target Integer) (s Flonum) (t Flonum)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2fARB (target Integer) (s Flonum) (t Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord2fv (target Integer) (v F32Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2fvARB (target Integer) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord2fv].
}
@defproc[(glMultiTexCoord2hNV (target Integer) (s Nonnegative-Fixnum) (t Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord2hvNV (target Integer) (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord2i (target Integer) (s Integer) (t Integer)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2iARB (target Integer) (s Integer) (t Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord2iv (target Integer) (v S32Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2ivARB (target Integer) (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord2iv].
}
@defproc[(glMultiTexCoord2s (target Integer) (s Fixnum) (t Fixnum)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2sARB (target Integer) (s Fixnum) (t Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord2sv (target Integer) (v S16Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2svARB (target Integer) (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord2sv].
}
@defproc[(glMultiTexCoord3d (target Integer) (s Real) (t Real) (r Real)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3dARB (target Integer) (s Real) (t Real) (r Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord3dv (target Integer) (v F64Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3dvARB (target Integer) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord3dv].
}
@defproc[(glMultiTexCoord3f (target Integer) (s Flonum) (t Flonum) (r Flonum)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3fARB (target Integer) (s Flonum) (t Flonum) (r Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord3fv (target Integer) (v F32Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3fvARB (target Integer) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord3fv].
}
@defproc[(glMultiTexCoord3hNV (target Integer) (s Nonnegative-Fixnum) (t Nonnegative-Fixnum) (r Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord3hvNV (target Integer) (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord3i (target Integer) (s Integer) (t Integer) (r Integer)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3iARB (target Integer) (s Integer) (t Integer) (r Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord3iv (target Integer) (v S32Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3ivARB (target Integer) (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord3iv].
}
@defproc[(glMultiTexCoord3s (target Integer) (s Fixnum) (t Fixnum) (r Fixnum)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3sARB (target Integer) (s Fixnum) (t Fixnum) (r Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord3sv (target Integer) (v S16Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3svARB (target Integer) (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord3sv].
}
@defproc[(glMultiTexCoord4d (target Integer) (s Real) (t Real) (r Real) (q Real)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4dARB (target Integer) (s Real) (t Real) (r Real) (q Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord4dv (target Integer) (v F64Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4dvARB (target Integer) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord4dv].
}
@defproc[(glMultiTexCoord4f (target Integer) (s Flonum) (t Flonum) (r Flonum) (q Flonum)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4fARB (target Integer) (s Flonum) (t Flonum) (r Flonum) (q Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord4fv (target Integer) (v F32Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4fvARB (target Integer) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord4fv].
}
@defproc[(glMultiTexCoord4hNV (target Integer) (s Nonnegative-Fixnum) (t Nonnegative-Fixnum) (r Nonnegative-Fixnum) (q Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord4hvNV (target Integer) (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord4i (target Integer) (s Integer) (t Integer) (r Integer) (q Integer)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4iARB (target Integer) (s Integer) (t Integer) (r Integer) (q Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord4iv (target Integer) (v S32Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4ivARB (target Integer) (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord4iv].
}
@defproc[(glMultiTexCoord4s (target Integer) (s Fixnum) (t Fixnum) (r Fixnum) (q Fixnum)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4sARB (target Integer) (s Fixnum) (t Fixnum) (r Fixnum) (q Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord4sv (target Integer) (v S16Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4svARB (target Integer) (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord4sv].
}
@defproc[(glMultiTexCoordP1ui (texture Integer) (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glMultiTexCoordP1uiv (texture Integer) (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glMultiTexCoordP2ui (texture Integer) (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glMultiTexCoordP2uiv (texture Integer) (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glMultiTexCoordP3ui (texture Integer) (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glMultiTexCoordP3uiv (texture Integer) (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glMultiTexCoordP4ui (texture Integer) (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glMultiTexCoordP4uiv (texture Integer) (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glMultiTexCoordPointerEXT (texunit Integer) (size Integer) (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexEnvfEXT (texunit Integer) (target Integer) (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexEnvfvEXT (texunit Integer) (target Integer) (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexEnviEXT (texunit Integer) (target Integer) (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexEnvivEXT (texunit Integer) (target Integer) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexGendEXT (texunit Integer) (coord Integer) (pname Integer) (param Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexGendvEXT (texunit Integer) (coord Integer) (pname Integer) (params F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexGenfEXT (texunit Integer) (coord Integer) (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexGenfvEXT (texunit Integer) (coord Integer) (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexGeniEXT (texunit Integer) (coord Integer) (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexGenivEXT (texunit Integer) (coord Integer) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexImage1DEXT (texunit Integer) (target Integer) (level Integer) (internalformat Integer) (width Integer) (border Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexImage2DEXT (texunit Integer) (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (border Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexImage3DEXT (texunit Integer) (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (depth Integer) (border Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexParameterIivEXT (texunit Integer) (target Integer) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexParameterIuivEXT (texunit Integer) (target Integer) (pname Integer) (params U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexParameterfEXT (texunit Integer) (target Integer) (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexParameterfvEXT (texunit Integer) (target Integer) (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexParameteriEXT (texunit Integer) (target Integer) (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexParameterivEXT (texunit Integer) (target Integer) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexRenderbufferEXT (texunit Integer) (target Integer) (renderbuffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexSubImage1DEXT (texunit Integer) (target Integer) (level Integer) (xoffset Integer) (width Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexSubImage2DEXT (texunit Integer) (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (width Integer) (height Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexSubImage3DEXT (texunit Integer) (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (zoffset Integer) (width Integer) (height Integer) (depth Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
