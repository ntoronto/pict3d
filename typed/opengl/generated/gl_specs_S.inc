#lang scribble/manual
@title{glS...}
@defproc[(glSampleCoverage (value Flonum) (invert Boolean)) Void]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glSampleCoverage.xml"]{glSampleCoverage manpage}.
}
@defproc[(glSampleCoverageARB (value Flonum) (invert Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multisample.txt"]{@racket[GL_ARB_multisample]}.

Alias of @racket[glSampleCoverage].
}
@defproc[(glSampleMapATI (dst Natural) (interp Natural) (swizzle Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glSampleMaskEXT (value Flonum) (invert Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/multisample.txt"]{@racket[GL_EXT_multisample]}.
}
@defproc[(glSampleMaskIndexedNV (index Natural) (mask Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/explicit_multisample.txt"]{@racket[GL_NV_explicit_multisample]}.
}
@defproc[(glSampleMaskSGIS (value Flonum) (invert Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/multisample.txt"]{@racket[GL_SGIS_multisample]}.

Alias of @racket[glSampleMaskEXT].
}
@defproc[(glSampleMaski (index Natural) (mask Natural)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glSampleMaski.xml"]{glSampleMaski manpage}.
}
@defproc[(glSamplePatternEXT (pattern Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/multisample.txt"]{@racket[GL_EXT_multisample]}.
}
@defproc[(glSamplePatternSGIS (pattern Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/multisample.txt"]{@racket[GL_SGIS_multisample]}.

Alias of @racket[glSamplePatternEXT].
}
@defproc[(glSamplerParameterIiv (sampler Natural) (pname Integer) (param S32Vector)) Void]{
Version 1.2.
}
@defproc[(glSamplerParameterIuiv (sampler Natural) (pname Integer) (param U32Vector)) Void]{
Version 1.2.
}
@defproc[(glSamplerParameterf (sampler Natural) (pname Integer) (param Flonum)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glSamplerParameter.xml"]{glSamplerParameter manpage}.
}
@defproc[(glSamplerParameterfv (sampler Natural) (pname Integer) (param F32Vector)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glSamplerParameter.xml"]{glSamplerParameter manpage}.
}
@defproc[(glSamplerParameteri (sampler Natural) (pname Integer) (param Integer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glSamplerParameter.xml"]{glSamplerParameter manpage}.
}
@defproc[(glSamplerParameteriv (sampler Natural) (pname Integer) (param S32Vector)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glSamplerParameter.xml"]{glSamplerParameter manpage}.
}
@defproc[(glScaled (x Real) (y Real) (z Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glScale.xml"]{glScale manpage}.
}
@defproc[(glScalef (x Flonum) (y Flonum) (z Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glScale.xml"]{glScale manpage}.
}
@defproc[(glScissor (x Integer) (y Integer) (width Integer) (height Integer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glScissor.xml"]{glScissor manpage}.
}
@defproc[(glScissorArrayv (first Natural) (count Integer) (v S32Vector)) Void]{
Version 4.1.
}
@defproc[(glScissorIndexed (index Natural) (left Integer) (bottom Integer) (width Integer) (height Integer)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glScissorIndexed.xml"]{glScissorIndexed manpage}.
}
@defproc[(glScissorIndexedv (index Natural) (v S32Vector)) Void]{
Version 4.1.
}
@defproc[(glSecondaryColor3b (red Fixnum) (green Fixnum) (blue Fixnum)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3bEXT (red Fixnum) (green Fixnum) (blue Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3b].
}
@defproc[(glSecondaryColor3bv (v S8Vector)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3bvEXT (v S8Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3bv].
}
@defproc[(glSecondaryColor3d (red Real) (green Real) (blue Real)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3dEXT (red Real) (green Real) (blue Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3d].
}
@defproc[(glSecondaryColor3dv (v F64Vector)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3dvEXT (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3dv].
}
@defproc[(glSecondaryColor3f (red Flonum) (green Flonum) (blue Flonum)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3fEXT (red Flonum) (green Flonum) (blue Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3f].
}
@defproc[(glSecondaryColor3fv (v F32Vector)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3fvEXT (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3fv].
}
@defproc[(glSecondaryColor3hNV (red Nonnegative-Fixnum) (green Nonnegative-Fixnum) (blue Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glSecondaryColor3hvNV (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glSecondaryColor3i (red Integer) (green Integer) (blue Integer)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3iEXT (red Integer) (green Integer) (blue Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3i].
}
@defproc[(glSecondaryColor3iv (v S32Vector)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3ivEXT (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3iv].
}
@defproc[(glSecondaryColor3s (red Fixnum) (green Fixnum) (blue Fixnum)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3sEXT (red Fixnum) (green Fixnum) (blue Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3s].
}
@defproc[(glSecondaryColor3sv (v S16Vector)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3svEXT (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3sv].
}
@defproc[(glSecondaryColor3ub (red Byte) (green Byte) (blue Byte)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3ubEXT (red Byte) (green Byte) (blue Byte)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3ub].
}
@defproc[(glSecondaryColor3ubv (v (U Bytes String))) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3ubvEXT (v (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3ubv].
}
@defproc[(glSecondaryColor3ui (red Natural) (green Natural) (blue Natural)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3uiEXT (red Natural) (green Natural) (blue Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3ui].
}
@defproc[(glSecondaryColor3uiv (v U32Vector)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3uivEXT (v U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3uiv].
}
@defproc[(glSecondaryColor3us (red Nonnegative-Fixnum) (green Nonnegative-Fixnum) (blue Nonnegative-Fixnum)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3usEXT (red Nonnegative-Fixnum) (green Nonnegative-Fixnum) (blue Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3us].
}
@defproc[(glSecondaryColor3usv (v U16Vector)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3usvEXT (v U16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3usv].
}
@defproc[(glSecondaryColorFormatNV (size Integer) (type Integer) (stride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glSecondaryColorP3ui (type Integer) (color Natural)) Void]{
Version 1.2.
}
@defproc[(glSecondaryColorP3uiv (type Integer) (color Natural)) Void]{
Version 1.2.
}
@defproc[(glSecondaryColorPointer (size Integer) (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColorPointer.xml"]{glSecondaryColorPointer manpage}.
}
@defproc[(glSecondaryColorPointerEXT (size Integer) (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColorPointer].
}
@defproc[(glSecondaryColorPointerListIBM (size Integer) (type Integer) (stride Integer) (pointer (Vectorof GLPointer)) (ptrstride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt"]{@racket[GL_IBM_vertex_array_lists]}.
}
@defproc[(glSelectBuffer (size Integer)) U32Vector]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSelectBuffer.xml"]{glSelectBuffer manpage}.
}
@defproc[(glSelectPerfMonitorCountersAMD (monitor Natural) (enable Boolean) (group Natural) (numCounters Integer) (counterList U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glSeparableFilter2D (target Integer) (internalformat Integer) (width Integer) (height Integer) (format Integer) (type Integer) (row GLPointer) (column GLPointer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSeparableFilter2D.xml"]{glSeparableFilter2D manpage}.
}
@defproc[(glSeparableFilter2DEXT (target Integer) (internalformat Integer) (width Integer) (height Integer) (format Integer) (type Integer) (row GLPointer) (column GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glSeparableFilter2D].
}
@defproc[(glSetFenceAPPLE (fence Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/fence.txt"]{@racket[GL_APPLE_fence]}.
}
@defproc[(glSetFenceNV (fence Natural) (condition Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fence.txt"]{@racket[GL_NV_fence]}.
}
@defproc[(glSetFragmentShaderConstantATI (dst Natural) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glSetInvariantEXT (id Natural) (type Integer) (addr GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glSetLocalConstantEXT (id Natural) (type Integer) (addr GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glSetMultisamplefvAMD (pname Integer) (index Natural) (val F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/sample_positions.txt"]{@racket[GL_AMD_sample_positions]}.
}
@defproc[(glShadeModel (mode Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glShadeModel.xml"]{glShadeModel manpage}.
}
@defproc[(glShaderBinary (count Integer) (shaders U32Vector) (binaryformat Integer) (binary GLPointer) (length Integer)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glShaderBinary.xml"]{glShaderBinary manpage}.
}
@defproc[(glShaderOp1EXT (op Integer) (res Natural) (arg1 Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glShaderOp2EXT (op Integer) (res Natural) (arg1 Natural) (arg2 Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glShaderOp3EXT (op Integer) (res Natural) (arg1 Natural) (arg2 Natural) (arg3 Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glShaderSource (shader Natural) (count Integer) (string (Vectorof (U Bytes String))) (length S32Vector)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glShaderSource.xml"]{glShaderSource manpage}.
}
@defproc[(glShaderSourceARB (shaderObj Natural) (count Integer) (string (Vectorof (U Bytes String))) (length S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glShaderSource].
}
@defproc[(glShaderStorageBlockBinding (program Natural) (storageBlockIndex Natural) (storageBlockBinding Natural)) Void]{
Version 4.3.
}
@defproc[(glSharpenTexFuncSGIS (target Integer) (n Integer) (points F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/sharpen_texture.txt"]{@racket[GL_SGIS_sharpen_texture]}.
}
@defproc[(glSpriteParameterfSGIX (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/sprite.txt"]{@racket[GL_SGIX_sprite]}.
}
@defproc[(glSpriteParameterfvSGIX (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/sprite.txt"]{@racket[GL_SGIX_sprite]}.
}
@defproc[(glSpriteParameteriSGIX (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/sprite.txt"]{@racket[GL_SGIX_sprite]}.
}
@defproc[(glSpriteParameterivSGIX (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/sprite.txt"]{@racket[GL_SGIX_sprite]}.
}
@defproc[(glStartInstrumentsSGIX) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/instruments.txt"]{@racket[GL_SGIX_instruments]}.
}
@defproc[(glStencilClearTagEXT (stencilTagBits Integer) (stencilClearTag Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/stencil_clear_tag.txt"]{@racket[GL_EXT_stencil_clear_tag]}.
}
@defproc[(glStencilFillPathInstancedNV (numPaths Integer) (pathNameType Integer) (paths GLPointer) (pathBase Natural) (fillMode Integer) (mask Natural) (transformType Integer) (transformValues F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glStencilFillPathNV (path Natural) (fillMode Integer) (mask Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glStencilFunc (func Integer) (ref Integer) (mask Natural)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glStencilFunc.xml"]{glStencilFunc manpage}.
}
@defproc[(glStencilFuncSeparate (face Integer) (func Integer) (ref Integer) (mask Natural)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glStencilFuncSeparate.xml"]{glStencilFuncSeparate manpage}.
}
@defproc[(glStencilFuncSeparateATI (frontfunc Integer) (backfunc Integer) (ref Integer) (mask Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/separate_stencil.txt"]{@racket[GL_ATI_separate_stencil]}.

Alias of @racket[glStencilFuncSeparate].
}
@defproc[(glStencilMask (mask Natural)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glStencilMask.xml"]{glStencilMask manpage}.
}
@defproc[(glStencilMaskSeparate (face Integer) (mask Natural)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glStencilMaskSeparate.xml"]{glStencilMaskSeparate manpage}.
}
@defproc[(glStencilOp (fail Integer) (zfail Integer) (zpass Integer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glStencilOp.xml"]{glStencilOp manpage}.
}
@defproc[(glStencilOpSeparate (face Integer) (sfail Integer) (dpfail Integer) (dppass Integer)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glStencilOpSeparate.xml"]{glStencilOpSeparate manpage}.
}
@defproc[(glStencilOpSeparateATI (face Integer) (sfail Integer) (dpfail Integer) (dppass Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/separate_stencil.txt"]{@racket[GL_ATI_separate_stencil]}.

Alias of @racket[glStencilOpSeparate].
}
@defproc[(glStencilOpValueAMD (face Integer) (value Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/stencil_operation_extended.txt"]{@racket[GL_AMD_stencil_operation_extended]}.
}
@defproc[(glStencilStrokePathInstancedNV (numPaths Integer) (pathNameType Integer) (paths GLPointer) (pathBase Natural) (reference Integer) (mask Natural) (transformType Integer) (transformValues F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glStencilStrokePathNV (path Natural) (reference Integer) (mask Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glStopInstrumentsSGIX (marker Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/instruments.txt"]{@racket[GL_SGIX_instruments]}.
}
@defproc[(glStringMarkerGREMEDY (len Integer) (string GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/GREMEDY/string_marker.txt"]{@racket[GL_GREMEDY_string_marker]}.
}
@defproc[(glSwizzleEXT (res Natural) (in Natural) (outX Integer) (outY Integer) (outZ Integer) (outW Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
