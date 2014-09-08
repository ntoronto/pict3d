#lang scribble/manual
@title{glT...}
@defproc[(glTagSampleBufferSGIX) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/tag_sample_buffer.txt"]{@racket[GL_SGIX_tag_sample_buffer]}.
}
@defproc[(glTangent3bEXT (tx Fixnum) (ty Fixnum) (tz Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3bvEXT (v S8Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3dEXT (tx Real) (ty Real) (tz Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3dvEXT (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3fEXT (tx Flonum) (ty Flonum) (tz Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3fvEXT (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3iEXT (tx Integer) (ty Integer) (tz Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3ivEXT (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3sEXT (tx Fixnum) (ty Fixnum) (tz Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3svEXT (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangentPointerEXT (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTbufferMask3DFX (mask Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/3DFX/tbuffer.txt"]{@racket[GL_3DFX_tbuffer]}.
}
@defproc[(glTessellationFactorAMD (factor Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/vertex_shader_tesselator.txt"]{@racket[GL_AMD_vertex_shader_tesselator]}.
}
@defproc[(glTessellationModeAMD (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/vertex_shader_tesselator.txt"]{@racket[GL_AMD_vertex_shader_tesselator]}.
}
@defproc[(glTestFenceAPPLE (fence Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/fence.txt"]{@racket[GL_APPLE_fence]}.
}
@defproc[(glTestFenceNV (fence Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fence.txt"]{@racket[GL_NV_fence]}.
}
@defproc[(glTestObjectAPPLE (object Integer) (name Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/fence.txt"]{@racket[GL_APPLE_fence]}.
}
@defproc[(glTexBuffer (target Integer) (internalformat Integer) (buffer Natural)) Void]{
Version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexBuffer.xml"]{glTexBuffer manpage}.
}
@defproc[(glTexBufferARB (target Integer) (internalformat Integer) (buffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_buffer_object.txt"]{@racket[GL_ARB_texture_buffer_object]}.

Alias of @racket[glTexBuffer].
}
@defproc[(glTexBufferEXT (target Integer) (internalformat Integer) (buffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_buffer_object.txt"]{@racket[GL_EXT_texture_buffer_object]}.

Alias of @racket[glTexBufferARB].
}
@defproc[(glTexBufferRange (target Integer) (internalformat Integer) (buffer Natural) (offset Integer) (size Integer)) Void]{
Version 4.3.
}
@defproc[(glTexBumpParameterfvATI (pname Integer) (param F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/envmap_bumpmap.txt"]{@racket[GL_ATI_envmap_bumpmap]}.
}
@defproc[(glTexBumpParameterivATI (pname Integer) (param S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/envmap_bumpmap.txt"]{@racket[GL_ATI_envmap_bumpmap]}.
}
@defproc[(glTexCoord1d (s Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord1dv (v Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord1f (s Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord1fv (v Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord1hNV (s Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord1hvNV (v Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord1i (s Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord1iv (v Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord1s (s Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord1sv (v Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2d (s Real) (t Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2dv (v F64Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2f (s Flonum) (t Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2fColor3fVertex3fSUN (s Flonum) (t Flonum) (r Flonum) (g Flonum) (b Flonum) (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fColor3fVertex3fvSUN (tc F32Vector) (c F32Vector) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fColor4fNormal3fVertex3fSUN (s Flonum) (t Flonum) (r Flonum) (g Flonum) (b Flonum) (a Flonum) (nx Flonum) (ny Flonum) (nz Flonum) (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fColor4fNormal3fVertex3fvSUN (tc F32Vector) (c F32Vector) (n F32Vector) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fColor4ubVertex3fSUN (s Flonum) (t Flonum) (r Byte) (g Byte) (b Byte) (a Byte) (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fColor4ubVertex3fvSUN (tc F32Vector) (c (U Bytes String)) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fNormal3fVertex3fSUN (s Flonum) (t Flonum) (nx Flonum) (ny Flonum) (nz Flonum) (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fNormal3fVertex3fvSUN (tc F32Vector) (n F32Vector) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fVertex3fSUN (s Flonum) (t Flonum) (x Flonum) (y Flonum) (z Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fVertex3fvSUN (tc F32Vector) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fv (v F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2hNV (s Nonnegative-Fixnum) (t Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord2hvNV (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord2i (s Integer) (t Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2iv (v S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2s (s Fixnum) (t Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2sv (v S16Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3d (s Real) (t Real) (r Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3dv (v F64Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3f (s Flonum) (t Flonum) (r Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3fv (v F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3hNV (s Nonnegative-Fixnum) (t Nonnegative-Fixnum) (r Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord3hvNV (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord3i (s Integer) (t Integer) (r Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3iv (v S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3s (s Fixnum) (t Fixnum) (r Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3sv (v S16Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4d (s Real) (t Real) (r Real) (q Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4dv (v F64Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4f (s Flonum) (t Flonum) (r Flonum) (q Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4fColor4fNormal3fVertex4fSUN (s Flonum) (t Flonum) (p Flonum) (q Flonum) (r Flonum) (g Flonum) (b Flonum) (a Flonum) (nx Flonum) (ny Flonum) (nz Flonum) (x Flonum) (y Flonum) (z Flonum) (w Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord4fColor4fNormal3fVertex4fvSUN (tc F32Vector) (c F32Vector) (n F32Vector) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord4fVertex4fSUN (s Flonum) (t Flonum) (p Flonum) (q Flonum) (x Flonum) (y Flonum) (z Flonum) (w Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord4fVertex4fvSUN (tc F32Vector) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord4fv (v F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4hNV (s Nonnegative-Fixnum) (t Nonnegative-Fixnum) (r Nonnegative-Fixnum) (q Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord4hvNV (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord4i (s Integer) (t Integer) (r Integer) (q Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4iv (v S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4s (s Fixnum) (t Fixnum) (r Fixnum) (q Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4sv (v S16Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoordFormatNV (size Integer) (type Integer) (stride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glTexCoordP1ui (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glTexCoordP1uiv (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glTexCoordP2ui (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glTexCoordP2uiv (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glTexCoordP3ui (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glTexCoordP3uiv (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glTexCoordP4ui (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glTexCoordP4uiv (type Integer) (coords Natural)) Void]{
Version 1.2.
}
@defproc[(glTexCoordPointer (size Integer) (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoordPointer.xml"]{glTexCoordPointer manpage}.
}
@defproc[(glTexCoordPointerEXT (size Integer) (type Integer) (stride Integer) (count Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.
}
@defproc[(glTexCoordPointerListIBM (size Integer) (type Integer) (stride Integer) (pointer (Vectorof GLPointer)) (ptrstride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt"]{@racket[GL_IBM_vertex_array_lists]}.
}
@defproc[(glTexCoordPointervINTEL (size Integer) (type Integer) (pointer (Vectorof GLPointer))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/INTEL/parallel_arrays.txt"]{@racket[GL_INTEL_parallel_arrays]}.
}
@defproc[(glTexEnvf (target Integer) (pname Integer) (param Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml"]{glTexEnv manpage}.
}
@defproc[(glTexEnvfv (target Integer) (pname Integer) (params F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml"]{glTexEnv manpage}.
}
@defproc[(glTexEnvi (target Integer) (pname Integer) (param Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml"]{glTexEnv manpage}.
}
@defproc[(glTexEnviv (target Integer) (pname Integer) (params S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml"]{glTexEnv manpage}.
}
@defproc[(glTexFilterFuncSGIS (target Integer) (filter Integer) (n Integer) (weights F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/texture_filter4.txt"]{@racket[GL_SGIS_texture_filter4]}.
}
@defproc[(glTexGend (coord Integer) (pname Integer) (param Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml"]{glTexGen manpage}.
}
@defproc[(glTexGendv (coord Integer) (pname Integer) (params F64Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml"]{glTexGen manpage}.
}
@defproc[(glTexGenf (coord Integer) (pname Integer) (param Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml"]{glTexGen manpage}.
}
@defproc[(glTexGenfv (coord Integer) (pname Integer) (params F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml"]{glTexGen manpage}.
}
@defproc[(glTexGeni (coord Integer) (pname Integer) (param Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml"]{glTexGen manpage}.
}
@defproc[(glTexGeniv (coord Integer) (pname Integer) (params S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml"]{glTexGen manpage}.
}
@defproc[(glTexImage1D (target Integer) (level Integer) (internalformat Integer) (width Integer) (border Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexImage1D.xml"]{glTexImage1D manpage}.
}
@defproc[(glTexImage2D (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (border Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexImage2D.xml"]{glTexImage2D manpage}.
}
@defproc[(glTexImage2DMultisample (target Integer) (samples Integer) (internalformat Integer) (width Integer) (height Integer) (fixedsamplelocations Boolean)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexImage2DMultisample.xml"]{glTexImage2DMultisample manpage}.
}
@defproc[(glTexImage2DMultisampleCoverageNV (target Integer) (coverageSamples Integer) (colorSamples Integer) (internalFormat Integer) (width Integer) (height Integer) (fixedSampleLocations Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/texture_multisample.txt"]{@racket[GL_NV_texture_multisample]}.
}
@defproc[(glTexImage3D (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (depth Integer) (border Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexImage3D.xml"]{glTexImage3D manpage}.
}
@defproc[(glTexImage3DEXT (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (depth Integer) (border Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture3D.txt"]{@racket[GL_EXT_texture3D]}.

Alias of @racket[glTexImage3D].
}
@defproc[(glTexImage3DMultisample (target Integer) (samples Integer) (internalformat Integer) (width Integer) (height Integer) (depth Integer) (fixedsamplelocations Boolean)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexImage3DMultisample.xml"]{glTexImage3DMultisample manpage}.
}
@defproc[(glTexImage3DMultisampleCoverageNV (target Integer) (coverageSamples Integer) (colorSamples Integer) (internalFormat Integer) (width Integer) (height Integer) (depth Integer) (fixedSampleLocations Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/texture_multisample.txt"]{@racket[GL_NV_texture_multisample]}.
}
@defproc[(glTexImage4DSGIS (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (depth Integer) (size4d Integer) (border Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/texture4D.txt"]{@racket[GL_SGIS_texture4D]}.
}
@defproc[(glTexParameterIiv (target Integer) (pname Integer) (params S32Vector)) Void]{
Version 3.0.
}
@defproc[(glTexParameterIivEXT (target Integer) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_integer.txt"]{@racket[GL_EXT_texture_integer]}.

Alias of @racket[glTexParameterIiv].
}
@defproc[(glTexParameterIuiv (target Integer) (pname Integer) (params U32Vector)) Void]{
Version 3.0.
}
@defproc[(glTexParameterIuivEXT (target Integer) (pname Integer) (params U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_integer.txt"]{@racket[GL_EXT_texture_integer]}.

Alias of @racket[glTexParameterIuiv].
}
@defproc[(glTexParameterf (target Integer) (pname Integer) (param Flonum)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexParameter.xml"]{glTexParameter manpage}.
}
@defproc[(glTexParameterfv (target Integer) (pname Integer) (params F32Vector)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexParameter.xml"]{glTexParameter manpage}.
}
@defproc[(glTexParameteri (target Integer) (pname Integer) (param Integer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexParameter.xml"]{glTexParameter manpage}.
}
@defproc[(glTexParameteriv (target Integer) (pname Integer) (params S32Vector)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexParameter.xml"]{glTexParameter manpage}.
}
@defproc[(glTexRenderbufferNV (target Integer) (renderbuffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/explicit_multisample.txt"]{@racket[GL_NV_explicit_multisample]}.
}
@defproc[(glTexStorage1D (target Integer) (levels Integer) (internalformat Integer) (width Integer)) Void]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexStorage1D.xml"]{glTexStorage1D manpage}.
}
@defproc[(glTexStorage2D (target Integer) (levels Integer) (internalformat Integer) (width Integer) (height Integer)) Void]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexStorage2D.xml"]{glTexStorage2D manpage}.
}
@defproc[(glTexStorage2DMultisample (target Integer) (samples Integer) (internalformat Integer) (width Integer) (height Integer) (fixedsamplelocations Boolean)) Void]{
Version 4.3.
}
@defproc[(glTexStorage3D (target Integer) (levels Integer) (internalformat Integer) (width Integer) (height Integer) (depth Integer)) Void]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexStorage3D.xml"]{glTexStorage3D manpage}.
}
@defproc[(glTexStorage3DMultisample (target Integer) (samples Integer) (internalformat Integer) (width Integer) (height Integer) (depth Integer) (fixedsamplelocations Boolean)) Void]{
Version 4.3.
}
@defproc[(glTexStorageSparseAMD (target Integer) (internalFormat Integer) (width Integer) (height Integer) (depth Integer) (layers Integer) (flags Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/sparse_texture.txt"]{@racket[GL_AMD_sparse_texture]}.
}
@defproc[(glTexSubImage1D (target Integer) (level Integer) (xoffset Integer) (width Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexSubImage1D.xml"]{glTexSubImage1D manpage}.
}
@defproc[(glTexSubImage1DEXT (target Integer) (level Integer) (xoffset Integer) (width Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/subtexture.txt"]{@racket[GL_EXT_subtexture]}.

Alias of @racket[glTexSubImage1D].
}
@defproc[(glTexSubImage2D (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (width Integer) (height Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexSubImage2D.xml"]{glTexSubImage2D manpage}.
}
@defproc[(glTexSubImage2DEXT (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (width Integer) (height Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/subtexture.txt"]{@racket[GL_EXT_subtexture]}.

Alias of @racket[glTexSubImage2D].
}
@defproc[(glTexSubImage3D (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (zoffset Integer) (width Integer) (height Integer) (depth Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexSubImage3D.xml"]{glTexSubImage3D manpage}.
}
@defproc[(glTexSubImage3DEXT (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (zoffset Integer) (width Integer) (height Integer) (depth Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture3D.txt"]{@racket[GL_EXT_texture3D]}.

Alias of @racket[glTexSubImage3D].
}
@defproc[(glTexSubImage4DSGIS (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (zoffset Integer) (woffset Integer) (width Integer) (height Integer) (depth Integer) (size4d Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/texture4D.txt"]{@racket[GL_SGIS_texture4D]}.
}
@defproc[(glTextureBarrierNV) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/texture_barrier.txt"]{@racket[GL_NV_texture_barrier]}.
}
@defproc[(glTextureBufferEXT (texture Natural) (target Integer) (internalformat Integer) (buffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureBufferRangeEXT (texture Natural) (target Integer) (internalformat Integer) (buffer Natural) (offset Integer) (size Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_buffer_range.txt"]{@racket[GL_ARB_texture_buffer_range]}.
}
@defproc[(glTextureColorMaskSGIS (red Boolean) (green Boolean) (blue Boolean) (alpha Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/texture_color_mask.txt"]{@racket[GL_SGIS_texture_color_mask]}.
}
@defproc[(glTextureImage1DEXT (texture Natural) (target Integer) (level Integer) (internalformat Integer) (width Integer) (border Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureImage2DEXT (texture Natural) (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (border Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureImage2DMultisampleCoverageNV (texture Natural) (target Integer) (coverageSamples Integer) (colorSamples Integer) (internalFormat Integer) (width Integer) (height Integer) (fixedSampleLocations Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/texture_multisample.txt"]{@racket[GL_NV_texture_multisample]}.
}
@defproc[(glTextureImage2DMultisampleNV (texture Natural) (target Integer) (samples Integer) (internalFormat Integer) (width Integer) (height Integer) (fixedSampleLocations Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/texture_multisample.txt"]{@racket[GL_NV_texture_multisample]}.
}
@defproc[(glTextureImage3DEXT (texture Natural) (target Integer) (level Integer) (internalformat Integer) (width Integer) (height Integer) (depth Integer) (border Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureImage3DMultisampleCoverageNV (texture Natural) (target Integer) (coverageSamples Integer) (colorSamples Integer) (internalFormat Integer) (width Integer) (height Integer) (depth Integer) (fixedSampleLocations Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/texture_multisample.txt"]{@racket[GL_NV_texture_multisample]}.
}
@defproc[(glTextureImage3DMultisampleNV (texture Natural) (target Integer) (samples Integer) (internalFormat Integer) (width Integer) (height Integer) (depth Integer) (fixedSampleLocations Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/texture_multisample.txt"]{@racket[GL_NV_texture_multisample]}.
}
@defproc[(glTextureLightEXT (pname Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/light_texture.txt"]{@racket[GL_EXT_light_texture]}.
}
@defproc[(glTextureMaterialEXT (face Integer) (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/light_texture.txt"]{@racket[GL_EXT_light_texture]}.
}
@defproc[(glTextureNormalEXT (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_perturb_normal.txt"]{@racket[GL_EXT_texture_perturb_normal]}.
}
@defproc[(glTextureParameterIivEXT (texture Natural) (target Integer) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureParameterIuivEXT (texture Natural) (target Integer) (pname Integer) (params U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureParameterfEXT (texture Natural) (target Integer) (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureParameterfvEXT (texture Natural) (target Integer) (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureParameteriEXT (texture Natural) (target Integer) (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureParameterivEXT (texture Natural) (target Integer) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureRangeAPPLE (target Integer) (length Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/texture_range.txt"]{@racket[GL_APPLE_texture_range]}.
}
@defproc[(glTextureRenderbufferEXT (texture Natural) (target Integer) (renderbuffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureStorage1DEXT (texture Natural) (target Integer) (levels Integer) (internalformat Integer) (width Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_storage.txt"]{@racket[GL_ARB_texture_storage]}.
}
@defproc[(glTextureStorage2DEXT (texture Natural) (target Integer) (levels Integer) (internalformat Integer) (width Integer) (height Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_storage.txt"]{@racket[GL_ARB_texture_storage]}.
}
@defproc[(glTextureStorage2DMultisampleEXT (texture Natural) (target Integer) (samples Integer) (internalformat Integer) (width Integer) (height Integer) (fixedsamplelocations Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_storage_multisample.txt"]{@racket[GL_ARB_texture_storage_multisample]}.
}
@defproc[(glTextureStorage3DEXT (texture Natural) (target Integer) (levels Integer) (internalformat Integer) (width Integer) (height Integer) (depth Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_storage.txt"]{@racket[GL_ARB_texture_storage]}.
}
@defproc[(glTextureStorage3DMultisampleEXT (texture Natural) (target Integer) (samples Integer) (internalformat Integer) (width Integer) (height Integer) (depth Integer) (fixedsamplelocations Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_storage_multisample.txt"]{@racket[GL_ARB_texture_storage_multisample]}.
}
@defproc[(glTextureStorageSparseAMD (texture Natural) (target Integer) (internalFormat Integer) (width Integer) (height Integer) (depth Integer) (layers Integer) (flags Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/sparse_texture.txt"]{@racket[GL_AMD_sparse_texture]}.
}
@defproc[(glTextureSubImage1DEXT (texture Natural) (target Integer) (level Integer) (xoffset Integer) (width Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureSubImage2DEXT (texture Natural) (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (width Integer) (height Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureSubImage3DEXT (texture Natural) (target Integer) (level Integer) (xoffset Integer) (yoffset Integer) (zoffset Integer) (width Integer) (height Integer) (depth Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureView (texture Natural) (target Integer) (origtexture Natural) (internalformat Integer) (minlevel Natural) (numlevels Natural) (minlayer Natural) (numlayers Natural)) Void]{
Version 4.3.
}
@defproc[(glTrackMatrixNV (target Integer) (address Natural) (matrix Integer) (transform Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glTransformFeedbackAttribsNV (count Natural) (attribs S32Vector) (bufferMode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.
}
@defproc[(glTransformFeedbackStreamAttribsNV (count Integer) (attribs S32Vector) (nbuffers Integer) (bufstreams S32Vector) (bufferMode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.
}
@defproc[(glTransformFeedbackVaryings (program Natural) (count Integer) (varyings (Vectorof (U Bytes String))) (bufferMode Integer)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTransformFeedbackVaryings.xml"]{glTransformFeedbackVaryings manpage}.
}
@defproc[(glTransformFeedbackVaryingsEXT (program Natural) (count Integer) (varyings (Vectorof (U Bytes String))) (bufferMode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/transform_feedback.txt"]{@racket[GL_EXT_transform_feedback]}.

Alias of @racket[glTransformFeedbackVaryings].
}
@defproc[(glTransformFeedbackVaryingsNV (program Natural) (count Integer) (locations S32Vector) (bufferMode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.

Alias of @racket[glTransformFeedbackVaryings].
}
@defproc[(glTransformPathNV (resultPath Natural) (srcPath Natural) (transformType Integer) (transformValues F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glTranslated (x Real) (y Real) (z Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTranslate.xml"]{glTranslate manpage}.
}
@defproc[(glTranslatef (x Flonum) (y Flonum) (z Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTranslate.xml"]{glTranslate manpage}.
}
