#lang scribble/manual
@title{glG...}
@defproc[(glGenAsyncMarkersSGIX (range Integer)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/async.txt"]{@racket[GL_SGIX_async]}.
}
@defproc[(glGenBuffers (n Integer)) U32Vector]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenBuffers.xml"]{glGenBuffers manpage}.
}
@defproc[(glGenBuffersARB (n Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glGenBuffers].
}
@defproc[(glGenFencesAPPLE (n Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/fence.txt"]{@racket[GL_APPLE_fence]}.
}
@defproc[(glGenFencesNV (n Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fence.txt"]{@racket[GL_NV_fence]}.
}
@defproc[(glGenFragmentShadersATI (range Natural)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glGenFramebuffers (n Integer)) U32Vector]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenFramebuffers.xml"]{glGenFramebuffers manpage}.
}
@defproc[(glGenFramebuffersEXT (n Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glGenFramebuffers].
}
@defproc[(glGenLists (range Integer)) Natural]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGenLists.xml"]{glGenLists manpage}.
}
@defproc[(glGenNamesAMD (identifier Integer) (num Natural)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/name_gen_delete.txt"]{@racket[GL_AMD_name_gen_delete]}.
}
@defproc[(glGenOcclusionQueriesNV (n Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/occlusion_query.txt"]{@racket[GL_NV_occlusion_query]}.
}
@defproc[(glGenPathsNV (range Integer)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGenPerfMonitorsAMD (n Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glGenProgramPipelines (n Integer)) U32Vector]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenProgramPipelines.xml"]{glGenProgramPipelines manpage}.
}
@defproc[(glGenProgramsARB (n Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glGenProgramsNV (n Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glGenProgramsARB].
}
@defproc[(glGenQueries (n Integer)) U32Vector]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenQueries.xml"]{glGenQueries manpage}.
}
@defproc[(glGenQueriesARB (n Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/occlusion_query.txt"]{@racket[GL_ARB_occlusion_query]}.

Alias of @racket[glGenQueries].
}
@defproc[(glGenRenderbuffers (n Integer)) U32Vector]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenRenderbuffers.xml"]{glGenRenderbuffers manpage}.
}
@defproc[(glGenRenderbuffersEXT (n Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glGenRenderbuffers].
}
@defproc[(glGenSamplers (count Integer)) U32Vector]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenSamplers.xml"]{glGenSamplers manpage}.
}
@defproc[(glGenSymbolsEXT (datatype Integer) (storagetype Integer) (range Integer) (components Natural)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGenTextures (n Integer)) U32Vector]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenTextures.xml"]{glGenTextures manpage}.
}
@defproc[(glGenTexturesEXT (n Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_object.txt"]{@racket[GL_EXT_texture_object]}.
}
@defproc[(glGenTransformFeedbacks (n Integer)) U32Vector]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenTransformFeedbacks.xml"]{glGenTransformFeedbacks manpage}.
}
@defproc[(glGenTransformFeedbacksNV (n Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback2.txt"]{@racket[GL_NV_transform_feedback2]}.
}
@defproc[(glGenVertexArrays (n Integer)) U32Vector]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenVertexArrays.xml"]{glGenVertexArrays manpage}.
}
@defproc[(glGenVertexArraysAPPLE (n Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_array_object.txt"]{@racket[GL_APPLE_vertex_array_object]}.

Alias of @racket[glGenVertexArray].
}
@defproc[(glGenVertexShadersEXT (range Natural)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGenerateMipmap (target Integer)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenerateMipmap.xml"]{glGenerateMipmap manpage}.
}
@defproc[(glGenerateMipmapEXT (target Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glGenerateMipmap].
}
@defproc[(glGenerateMultiTexMipmapEXT (texunit Integer) (target Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGenerateTextureMipmapEXT (texture Natural) (target Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetActiveAtomicCounterBufferiv (program Natural) (bufferIndex Natural) (pname Integer)) S32Vector]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveAtomicCounterBufferiv.xml"]{glGetActiveAtomicCounterBufferiv manpage}.
}
@defproc[(glGetActiveAttrib (program Natural) (index Natural) (bufSize Integer)) (values Integer Integer Integer Bytes)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveAttrib.xml"]{glGetActiveAttrib manpage}.
}
@defproc[(glGetActiveAttribARB (programObj Natural) (index Natural) (maxLength Integer)) (values Integer Integer Integer Bytes)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_shader.txt"]{@racket[GL_ARB_vertex_shader]}.

Alias of @racket[glGetActiveAttrib].
}
@defproc[(glGetActiveSubroutineName (program Natural) (shadertype Integer) (index Natural) (bufsize Integer)) (values Integer Bytes)]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveSubroutineName.xml"]{glGetActiveSubroutineName manpage}.
}
@defproc[(glGetActiveSubroutineUniformName (program Natural) (shadertype Integer) (index Natural) (bufsize Integer)) (values Integer Bytes)]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveSubroutineUniformName.xml"]{glGetActiveSubroutineUniformName manpage}.
}
@defproc[(glGetActiveSubroutineUniformiv (program Natural) (shadertype Integer) (index Natural) (pname Integer)) S32Vector]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveSubroutineUniform.xml"]{glGetActiveSubroutineUniform manpage}.
}
@defproc[(glGetActiveUniform (program Natural) (index Natural) (bufSize Integer)) (values Integer Integer Integer Bytes)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveUniform.xml"]{glGetActiveUniform manpage}.
}
@defproc[(glGetActiveUniformARB (programObj Natural) (index Natural) (maxLength Integer)) (values Integer Integer Integer Bytes)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glGetActiveUniform].
}
@defproc[(glGetActiveUniformBlockName (program Natural) (uniformBlockIndex Natural) (bufSize Integer)) (values Integer Bytes)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveUniformBlockName.xml"]{glGetActiveUniformBlockName manpage}.
}
@defproc[(glGetActiveUniformBlockiv (program Natural) (uniformBlockIndex Natural) (pname Integer)) S32Vector]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveUniformBlock.xml"]{glGetActiveUniformBlock manpage}.
}
@defproc[(glGetActiveUniformName (program Natural) (uniformIndex Natural) (bufSize Integer)) (values Integer Bytes)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveUniformName.xml"]{glGetActiveUniformName manpage}.
}
@defproc[(glGetActiveUniformsiv (program Natural) (uniformCount Integer) (uniformIndices U32Vector) (pname Integer)) S32Vector]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveUniformsiv.xml"]{glGetActiveUniformsiv manpage}.
}
@defproc[(glGetActiveVaryingNV (program Natural) (index Natural) (bufSize Integer)) (values Integer Integer Integer Bytes)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.
}
@defproc[(glGetArrayObjectfvATI (array Integer) (pname Integer)) Flonum]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glGetArrayObjectivATI (array Integer) (pname Integer)) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glGetAttachedObjectsARB (containerObj Natural) (maxCount Integer)) (values Integer U32Vector)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glGetAttachedShaders].
}
@defproc[(glGetAttachedShaders (program Natural) (maxCount Integer)) (values Integer U32Vector)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetAttachedShaders.xml"]{glGetAttachedShaders manpage}.
}
@defproc[(glGetAttribLocation (program Natural) (name (U Bytes String))) Integer]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetAttribLocation.xml"]{glGetAttribLocation manpage}.
}
@defproc[(glGetAttribLocationARB (programObj Natural) (name (U Bytes String))) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_shader.txt"]{@racket[GL_ARB_vertex_shader]}.

Alias of @racket[glGetAttribLocation].
}
@defproc[(glGetBooleanIndexedvEXT (target Integer) (index Natural) (data CPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/draw_buffers2.txt"]{@racket[GL_EXT_draw_buffers2]}.

Alias of @racket[glGetBooleani_v].
}
@defproc[(glGetBooleani_v (target Integer) (index Natural) (data CPointer)) Void]{
Version 3.0.
}
@defproc[(glGetBooleanv (pname Integer)) (Vectorof Boolean)]{
}
@defproc[(glGetBufferParameteri64v (target Integer) (pname Integer)) S64Vector]{
Version 3.2.
}
@defproc[(glGetBufferParameteriv (target Integer) (pname Integer)) S32Vector]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetBufferParameteriv.xml"]{glGetBufferParameteriv manpage}.
}
@defproc[(glGetBufferParameterivARB (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glGetBufferParameteriv].
}
@defproc[(glGetBufferParameterui64vNV (target Integer) (pname Integer)) U64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glGetBufferPointerv (target Integer) (pname Integer)) GLPointer]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetBufferPointerv.xml"]{glGetBufferPointerv manpage}.
}
@defproc[(glGetBufferPointervARB (target Integer) (pname Integer)) GLPointer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glGetBufferPointerv].
}
@defproc[(glGetBufferSubData (target Integer) (offset Integer) (size Integer) (data GLPointer)) Void]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetBufferSubData.xml"]{glGetBufferSubData manpage}.
}
@defproc[(glGetBufferSubDataARB (target Integer) (offset Integer) (size Integer) (data GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glGetBufferSubData].
}
@defproc[(glGetClipPlane (plane Integer)) F64Vector]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetClipPlane.xml"]{glGetClipPlane manpage}.
}
@defproc[(glGetColorTable (target Integer) (format Integer) (type Integer) (table GLPointer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetColorTable.xml"]{glGetColorTable manpage}.
}
@defproc[(glGetColorTableEXT (target Integer) (format Integer) (type Integer) (data GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/paletted_texture.txt"]{@racket[GL_EXT_paletted_texture]}.
}
@defproc[(glGetColorTableParameterfv (target Integer) (pname Integer)) F32Vector]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetColorTableParameter.xml"]{glGetColorTableParameter manpage}.
}
@defproc[(glGetColorTableParameterfvEXT (target Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/paletted_texture.txt"]{@racket[GL_EXT_paletted_texture]}.
}
@defproc[(glGetColorTableParameterfvSGI (target Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGI/color_table.txt"]{@racket[GL_SGI_color_table]}.
}
@defproc[(glGetColorTableParameteriv (target Integer) (pname Integer)) S32Vector]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetColorTableParameter.xml"]{glGetColorTableParameter manpage}.
}
@defproc[(glGetColorTableParameterivEXT (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/paletted_texture.txt"]{@racket[GL_EXT_paletted_texture]}.
}
@defproc[(glGetColorTableParameterivSGI (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGI/color_table.txt"]{@racket[GL_SGI_color_table]}.
}
@defproc[(glGetColorTableSGI (target Integer) (format Integer) (type Integer) (table GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGI/color_table.txt"]{@racket[GL_SGI_color_table]}.
}
@defproc[(glGetCombinerInputParameterfvNV (stage Integer) (portion Integer) (variable Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glGetCombinerInputParameterivNV (stage Integer) (portion Integer) (variable Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glGetCombinerOutputParameterfvNV (stage Integer) (portion Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glGetCombinerOutputParameterivNV (stage Integer) (portion Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glGetCombinerStageParameterfvNV (stage Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners2.txt"]{@racket[GL_NV_register_combiners2]}.
}
@defproc[(glGetCompressedMultiTexImageEXT (texunit Integer) (target Integer) (lod Integer) (img GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetCompressedTexImage (target Integer) (level Integer) (img GLPointer)) Void]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetCompressedTexImage.xml"]{glGetCompressedTexImage manpage}.
}
@defproc[(glGetCompressedTexImageARB (target Integer) (level Integer) (img GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_compression.txt"]{@racket[GL_ARB_texture_compression]}.

Alias of @racket[glGetCompressedTexImage].
}
@defproc[(glGetCompressedTextureImageEXT (texture Natural) (target Integer) (lod Integer) (img GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetConvolutionFilter (target Integer) (format Integer) (type Integer) (image GLPointer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetConvolutionFilter.xml"]{glGetConvolutionFilter manpage}.
}
@defproc[(glGetConvolutionFilterEXT (target Integer) (format Integer) (type Integer) (image GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.
}
@defproc[(glGetConvolutionParameterfv (target Integer) (pname Integer)) F32Vector]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetConvolutionParameter.xml"]{glGetConvolutionParameter manpage}.
}
@defproc[(glGetConvolutionParameterfvEXT (target Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.
}
@defproc[(glGetConvolutionParameteriv (target Integer) (pname Integer)) S32Vector]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetConvolutionParameter.xml"]{glGetConvolutionParameter manpage}.
}
@defproc[(glGetConvolutionParameterivEXT (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.
}
@defproc[(glGetDebugMessageLog (count Natural) (bufsize Integer) (sources S32Vector) (types S32Vector) (ids U32Vector) (severities S32Vector) (lengths S32Vector) (messageLog Bytes)) Natural]{
Version 4.3.
}
@defproc[(glGetDebugMessageLogAMD (count Natural) (bufsize Integer)) (values Natural S32Vector U32Vector U32Vector S32Vector Bytes)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/debug_output.txt"]{@racket[GL_AMD_debug_output]}.
}
@defproc[(glGetDebugMessageLogARB (count Natural) (bufsize Integer)) (values Natural S32Vector S32Vector U32Vector S32Vector S32Vector Bytes)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/debug_output.txt"]{@racket[GL_ARB_debug_output]}.
}
@defproc[(glGetDetailTexFuncSGIS (target Integer) (points F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/detail_texture.txt"]{@racket[GL_SGIS_detail_texture]}.
}
@defproc[(glGetDoubleIndexedvEXT (target Integer) (index Natural) (data F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetDoublei_v (target Integer) (index Natural) (data F64Vector)) Void]{
Version 4.1.
}
@defproc[(glGetDoublev (pname Integer)) F64Vector]{
}
@defproc[(glGetError) Integer]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetError.xml"]{glGetError manpage}.
}
@defproc[(glGetFenceivNV (fence Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fence.txt"]{@racket[GL_NV_fence]}.
}
@defproc[(glGetFinalCombinerInputParameterfvNV (variable Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glGetFinalCombinerInputParameterivNV (variable Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glGetFloatIndexedvEXT (target Integer) (index Natural) (data F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetFloati_v (target Integer) (index Natural) (data F32Vector)) Void]{
Version 4.1.
}
@defproc[(glGetFloatv (pname Integer)) F32Vector]{
}
@defproc[(glGetFogFuncSGIS (points F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/fog_function.txt"]{@racket[GL_SGIS_fog_function]}.
}
@defproc[(glGetFragDataIndex (program Natural) (name (U Bytes String))) Integer]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetFragDataIndex.xml"]{glGetFragDataIndex manpage}.
}
@defproc[(glGetFragDataLocation (program Natural) (name (U Bytes String))) Integer]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetFragDataLocation.xml"]{glGetFragDataLocation manpage}.
}
@defproc[(glGetFragDataLocationEXT (program Natural) (name (U Bytes String))) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glGetFragDataLocation].
}
@defproc[(glGetFragmentLightfvSGIX (light Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glGetFragmentLightivSGIX (light Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glGetFragmentMaterialfvSGIX (face Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glGetFragmentMaterialivSGIX (face Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glGetFramebufferAttachmentParameteriv (target Integer) (attachment Integer) (pname Integer)) S32Vector]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetFramebufferAttachmentParameter.xml"]{glGetFramebufferAttachmentParameter manpage}.
}
@defproc[(glGetFramebufferAttachmentParameterivEXT (target Integer) (attachment Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glGetFramebufferAttachmentParameteriv].
}
@defproc[(glGetFramebufferParameteriv (target Integer) (pname Integer)) S32Vector]{
Version 4.3.
}
@defproc[(glGetFramebufferParameterivEXT (framebuffer Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetGraphicsResetStatusARB) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetHandleARB (pname Integer)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.
}
@defproc[(glGetHistogram (target Integer) (reset Boolean) (format Integer) (type Integer) (the-values GLPointer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetHistogram.xml"]{glGetHistogram manpage}.
}
@defproc[(glGetHistogramEXT (target Integer) (reset Boolean) (format Integer) (type Integer) (the-values GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/histogram.txt"]{@racket[GL_EXT_histogram]}.
}
@defproc[(glGetHistogramParameterfv (target Integer) (pname Integer)) F32Vector]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetHistogramParameter.xml"]{glGetHistogramParameter manpage}.
}
@defproc[(glGetHistogramParameterfvEXT (target Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/histogram.txt"]{@racket[GL_EXT_histogram]}.
}
@defproc[(glGetHistogramParameteriv (target Integer) (pname Integer)) S32Vector]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetHistogramParameter.xml"]{glGetHistogramParameter manpage}.
}
@defproc[(glGetHistogramParameterivEXT (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/histogram.txt"]{@racket[GL_EXT_histogram]}.
}
@defproc[(glGetImageHandleNV (texture Natural) (level Integer) (layered Boolean) (layer Integer) (format Integer)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glGetImageTransformParameterfvHP (target Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/HP/image_transform.txt"]{@racket[GL_HP_image_transform]}.
}
@defproc[(glGetImageTransformParameterivHP (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/HP/image_transform.txt"]{@racket[GL_HP_image_transform]}.
}
@defproc[(glGetInfoLogARB (obj Natural) (maxLength Integer)) (values Integer Bytes)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.
}
@defproc[(glGetInstrumentsSGIX) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/instruments.txt"]{@racket[GL_SGIX_instruments]}.
}
@defproc[(glGetInteger64i_v (target Integer) (index Natural) (data S64Vector)) Void]{
Version 3.2.
}
@defproc[(glGetInteger64v (pname Integer)) S64Vector]{
Version 1.2.
}
@defproc[(glGetIntegerIndexedvEXT (target Integer) (index Natural) (data S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/draw_buffers2.txt"]{@racket[GL_EXT_draw_buffers2]}.

Alias of @racket[glGetIntegeri_v].
}
@defproc[(glGetIntegeri_v (target Integer) (index Natural) (data S32Vector)) Void]{
Version 3.0.
}
@defproc[(glGetIntegerui64i_vNV (value Integer) (index Natural) (result U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glGetIntegerui64vNV (value Integer) (result U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glGetIntegerv (pname Integer)) S32Vector]{
}
@defproc[(glGetInternalformati64v (target Integer) (internalformat Integer) (pname Integer) (bufSize Integer)) S64Vector]{
Version 4.3.
}
@defproc[(glGetInternalformativ (target Integer) (internalformat Integer) (pname Integer) (bufSize Integer)) S32Vector]{
Version 4.2.
}
@defproc[(glGetInvariantBooleanvEXT (id Natural) (value Integer) (data CPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetInvariantFloatvEXT (id Natural) (value Integer) (data F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetInvariantIntegervEXT (id Natural) (value Integer) (data S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetLightfv (light Integer) (pname Integer)) F32Vector]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetLight.xml"]{glGetLight manpage}.
}
@defproc[(glGetLightiv (light Integer) (pname Integer)) S32Vector]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetLight.xml"]{glGetLight manpage}.
}
@defproc[(glGetListParameterfvSGIX (list Natural) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/list_priority.txt"]{@racket[GL_SGIX_list_priority]}.
}
@defproc[(glGetListParameterivSGIX (list Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/list_priority.txt"]{@racket[GL_SGIX_list_priority]}.
}
@defproc[(glGetLocalConstantBooleanvEXT (id Natural) (value Integer) (data CPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetLocalConstantFloatvEXT (id Natural) (value Integer) (data F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetLocalConstantIntegervEXT (id Natural) (value Integer) (data S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetMapAttribParameterfvNV (target Integer) (index Natural) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glGetMapAttribParameterivNV (target Integer) (index Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glGetMapControlPointsNV (target Integer) (index Natural) (type Integer) (ustride Integer) (vstride Integer) (packed Boolean) (points GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glGetMapParameterfvNV (target Integer) (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glGetMapParameterivNV (target Integer) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glGetMapdv (target Integer) (query Integer) (v F64Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMap.xml"]{glGetMap manpage}.
}
@defproc[(glGetMapfv (target Integer) (query Integer) (v F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMap.xml"]{glGetMap manpage}.
}
@defproc[(glGetMapiv (target Integer) (query Integer) (v S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMap.xml"]{glGetMap manpage}.
}
@defproc[(glGetMaterialfv (face Integer) (pname Integer)) F32Vector]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMaterial.xml"]{glGetMaterial manpage}.
}
@defproc[(glGetMaterialiv (face Integer) (pname Integer)) S32Vector]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMaterial.xml"]{glGetMaterial manpage}.
}
@defproc[(glGetMinmax (target Integer) (reset Boolean) (format Integer) (type Integer) (the-values GLPointer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMinmax.xml"]{glGetMinmax manpage}.
}
@defproc[(glGetMinmaxEXT (target Integer) (reset Boolean) (format Integer) (type Integer) (the-values GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/histogram.txt"]{@racket[GL_EXT_histogram]}.
}
@defproc[(glGetMinmaxParameterfv (target Integer) (pname Integer)) F32Vector]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMinmaxParameter.xml"]{glGetMinmaxParameter manpage}.
}
@defproc[(glGetMinmaxParameterfvEXT (target Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/histogram.txt"]{@racket[GL_EXT_histogram]}.
}
@defproc[(glGetMinmaxParameteriv (target Integer) (pname Integer)) S32Vector]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMinmaxParameter.xml"]{glGetMinmaxParameter manpage}.
}
@defproc[(glGetMinmaxParameterivEXT (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/histogram.txt"]{@racket[GL_EXT_histogram]}.
}
@defproc[(glGetMultiTexEnvfvEXT (texunit Integer) (target Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexEnvivEXT (texunit Integer) (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexGendvEXT (texunit Integer) (coord Integer) (pname Integer)) F64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexGenfvEXT (texunit Integer) (coord Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexGenivEXT (texunit Integer) (coord Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexImageEXT (texunit Integer) (target Integer) (level Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexLevelParameterfvEXT (texunit Integer) (target Integer) (level Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexLevelParameterivEXT (texunit Integer) (target Integer) (level Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexParameterIivEXT (texunit Integer) (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexParameterIuivEXT (texunit Integer) (target Integer) (pname Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexParameterfvEXT (texunit Integer) (target Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexParameterivEXT (texunit Integer) (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultisamplefv (pname Integer) (index Natural)) F32Vector]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetMultisample.xml"]{glGetMultisample manpage}.
}
@defproc[(glGetMultisamplefvNV (pname Integer) (index Natural)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/explicit_multisample.txt"]{@racket[GL_NV_explicit_multisample]}.
}
@defproc[(glGetNamedBufferParameterivEXT (buffer Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedBufferParameterui64vNV (buffer Natural) (pname Integer)) U64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glGetNamedBufferPointervEXT (buffer Natural) (pname Integer)) GLPointer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedBufferSubDataEXT (buffer Natural) (offset Integer) (size Integer) (data GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedFramebufferAttachmentParameterivEXT (framebuffer Natural) (attachment Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedFramebufferParameterivEXT (framebuffer Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/framebuffer_no_attachments.txt"]{@racket[GL_ARB_framebuffer_no_attachments]}.
}
@defproc[(glGetNamedProgramLocalParameterIivEXT (program Natural) (target Integer) (index Natural)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedProgramLocalParameterIuivEXT (program Natural) (target Integer) (index Natural)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedProgramLocalParameterdvEXT (program Natural) (target Integer) (index Natural)) F64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedProgramLocalParameterfvEXT (program Natural) (target Integer) (index Natural)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedProgramStringEXT (program Natural) (target Integer) (pname Integer) (string GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedProgramivEXT (program Natural) (target Integer) (pname Integer)) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedRenderbufferParameterivEXT (renderbuffer Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedStringARB (namelen Integer) (name (U Bytes String)) (bufSize Integer)) (values Integer Bytes)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shading_language_include.txt"]{@racket[GL_ARB_shading_language_include]}.
}
@defproc[(glGetNamedStringivARB (namelen Integer) (name (U Bytes String)) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shading_language_include.txt"]{@racket[GL_ARB_shading_language_include]}.
}
@defproc[(glGetObjectBufferfvATI (buffer Natural) (pname Integer)) Flonum]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glGetObjectBufferivATI (buffer Natural) (pname Integer)) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glGetObjectLabel (identifier Integer) (name Natural) (bufSize Integer)) (values Integer Bytes)]{
Version 4.3.
}
@defproc[(glGetObjectParameterfvARB (obj Natural) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.
}
@defproc[(glGetObjectParameterivAPPLE (objectType Integer) (name Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/object_purgeable.txt"]{@racket[GL_APPLE_object_purgeable]}.
}
@defproc[(glGetObjectParameterivARB (obj Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.
}
@defproc[(glGetObjectPtrLabel (ptr GLPointer) (bufSize Integer)) (values Integer Bytes)]{
Version 4.3.
}
@defproc[(glGetOcclusionQueryivNV (id Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/occlusion_query.txt"]{@racket[GL_NV_occlusion_query]}.
}
@defproc[(glGetOcclusionQueryuivNV (id Natural) (pname Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/occlusion_query.txt"]{@racket[GL_NV_occlusion_query]}.
}
@defproc[(glGetPathColorGenfvNV (color Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathColorGenivNV (color Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathCommandsNV (path Natural) (commands Bytes)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathCoordsNV (path Natural) (coords F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathDashArrayNV (path Natural) (dashArray F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathLengthNV (path Natural) (startSegment Integer) (numSegments Integer)) Flonum]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathMetricRangeNV (metricQueryMask Natural) (firstPathName Natural) (numPaths Integer) (stride Integer) (metrics F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathMetricsNV (metricQueryMask Natural) (numPaths Integer) (pathNameType Integer) (paths GLPointer) (pathBase Natural) (stride Integer) (metrics F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathParameterfvNV (path Natural) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathParameterivNV (path Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathSpacingNV (pathListMode Integer) (numPaths Integer) (pathNameType Integer) (paths GLPointer) (pathBase Natural) (advanceScale Flonum) (kerningScale Flonum) (transformType Integer) (returnedSpacing F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathTexGenfvNV (texCoordSet Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathTexGenivNV (texCoordSet Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPerfMonitorCounterDataAMD (monitor Natural) (pname Integer) (dataSize Integer)) (values U32Vector Integer)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glGetPerfMonitorCounterInfoAMD (group Natural) (counter Natural) (pname Integer) (data GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glGetPerfMonitorCounterStringAMD (group Natural) (counter Natural) (bufSize Integer)) (values Integer Bytes)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glGetPerfMonitorCountersAMD (group Natural) (counterSize Integer)) (values Integer Integer U32Vector)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glGetPerfMonitorGroupStringAMD (group Natural) (bufSize Integer)) (values Integer Bytes)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glGetPerfMonitorGroupsAMD (groupsSize Integer)) (values Integer U32Vector)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glGetPixelMapfv (map Integer) (the-values F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetPixelMap.xml"]{glGetPixelMap manpage}.
}
@defproc[(glGetPixelMapuiv (map Integer) (the-values U32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetPixelMap.xml"]{glGetPixelMap manpage}.
}
@defproc[(glGetPixelMapusv (map Integer) (the-values U16Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetPixelMap.xml"]{glGetPixelMap manpage}.
}
@defproc[(glGetPixelTexGenParameterfvSGIS (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/pixel_texture.txt"]{@racket[GL_SGIS_pixel_texture]}.
}
@defproc[(glGetPixelTexGenParameterivSGIS (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/pixel_texture.txt"]{@racket[GL_SGIS_pixel_texture]}.
}
@defproc[(glGetPixelTransformParameterfvEXT (target Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/pixel_transform.txt"]{@racket[GL_EXT_pixel_transform]}.
}
@defproc[(glGetPixelTransformParameterivEXT (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/pixel_transform.txt"]{@racket[GL_EXT_pixel_transform]}.
}
@defproc[(glGetPointerIndexedvEXT (target Integer) (index Natural) (data CPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetPointerv (pname Integer)) GLPointer]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetPointerv.xml"]{glGetPointerv manpage}.
}
@defproc[(glGetPointervEXT (pname Integer)) GLPointer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.

Alias of @racket[glGetPointerv].
}
@defproc[(glGetPolygonStipple (mask Bytes)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetPolygonStipple.xml"]{glGetPolygonStipple manpage}.
}
@defproc[(glGetProgramBinary (program Natural) (bufSize Integer) (binary GLPointer)) (values Integer Integer)]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetProgramBinary.xml"]{glGetProgramBinary manpage}.
}
@defproc[(glGetProgramEnvParameterIivNV (target Integer) (index Natural)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glGetProgramEnvParameterIuivNV (target Integer) (index Natural)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glGetProgramEnvParameterdvARB (target Integer) (index Natural)) F64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glGetProgramEnvParameterfvARB (target Integer) (index Natural)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glGetProgramInfoLog (program Natural) (bufSize Integer)) (values Integer Bytes)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetProgramInfoLog.xml"]{glGetProgramInfoLog manpage}.
}
@defproc[(glGetProgramInterfaceiv (program Natural) (programInterface Integer) (pname Integer)) S32Vector]{
Version 4.3.
}
@defproc[(glGetProgramLocalParameterIivNV (target Integer) (index Natural)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glGetProgramLocalParameterIuivNV (target Integer) (index Natural)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glGetProgramLocalParameterdvARB (target Integer) (index Natural)) F64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glGetProgramLocalParameterfvARB (target Integer) (index Natural)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glGetProgramNamedParameterdvNV (id Natural) (len Integer) (name (U Bytes String))) F64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fragment_program.txt"]{@racket[GL_NV_fragment_program]}.
}
@defproc[(glGetProgramNamedParameterfvNV (id Natural) (len Integer) (name (U Bytes String))) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fragment_program.txt"]{@racket[GL_NV_fragment_program]}.
}
@defproc[(glGetProgramParameterdvNV (target Integer) (index Natural) (pname Integer)) F64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glGetProgramParameterfvNV (target Integer) (index Natural) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glGetProgramPipelineInfoLog (pipeline Natural) (bufSize Integer)) (values Integer Bytes)]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetProgramPipelineInfoLog.xml"]{glGetProgramPipelineInfoLog manpage}.
}
@defproc[(glGetProgramPipelineiv (pipeline Natural) (pname Integer)) S32Vector]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetProgramPipeline.xml"]{glGetProgramPipeline manpage}.
}
@defproc[(glGetProgramResourceIndex (program Natural) (programInterface Integer) (name (U Bytes String))) Natural]{
Version 4.3.
}
@defproc[(glGetProgramResourceLocation (program Natural) (programInterface Integer) (name (U Bytes String))) Integer]{
Version 4.3.
}
@defproc[(glGetProgramResourceLocationIndex (program Natural) (programInterface Integer) (name (U Bytes String))) Integer]{
Version 4.3.
}
@defproc[(glGetProgramResourceName (program Natural) (programInterface Integer) (index Natural) (bufSize Integer)) (values Integer Bytes)]{
Version 4.3.
}
@defproc[(glGetProgramResourceiv (program Natural) (programInterface Integer) (index Natural) (propCount Integer) (props S32Vector) (bufSize Integer)) (values Integer S32Vector)]{
Version 4.3.
}
@defproc[(glGetProgramStageiv (program Natural) (shadertype Integer) (pname Integer)) Integer]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetProgramStage.xml"]{glGetProgramStage manpage}.
}
@defproc[(glGetProgramStringARB (target Integer) (pname Integer) (string GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glGetProgramStringNV (id Natural) (pname Integer) (program Bytes)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glGetProgramSubroutineParameteruivNV (target Integer) (index Natural) (param U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program5.txt"]{@racket[GL_NV_gpu_program5]}.
}
@defproc[(glGetProgramiv (program Natural) (pname Integer)) Integer]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetProgram.xml"]{glGetProgram manpage}.
}
@defproc[(glGetProgramivARB (target Integer) (pname Integer)) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glGetProgramivNV (id Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glGetQueryIndexediv (target Integer) (index Natural) (pname Integer)) S32Vector]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetQueryIndexed.xml"]{glGetQueryIndexed manpage}.
}
@defproc[(glGetQueryObjecti64v (id Natural) (pname Integer)) S64Vector]{
Version 1.2.
}
@defproc[(glGetQueryObjecti64vEXT (id Natural) (pname Integer)) S64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/timer_query.txt"]{@racket[GL_EXT_timer_query]}.
}
@defproc[(glGetQueryObjectiv (id Natural) (pname Integer)) S32Vector]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetQueryObject.xml"]{glGetQueryObject manpage}.
}
@defproc[(glGetQueryObjectivARB (id Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/occlusion_query.txt"]{@racket[GL_ARB_occlusion_query]}.

Alias of @racket[glGetQueryObjectiv].
}
@defproc[(glGetQueryObjectui64v (id Natural) (pname Integer)) U64Vector]{
Version 1.2.
}
@defproc[(glGetQueryObjectui64vEXT (id Natural) (pname Integer)) U64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/timer_query.txt"]{@racket[GL_EXT_timer_query]}.
}
@defproc[(glGetQueryObjectuiv (id Natural) (pname Integer)) U32Vector]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetQueryObject.xml"]{glGetQueryObject manpage}.
}
@defproc[(glGetQueryObjectuivARB (id Natural) (pname Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/occlusion_query.txt"]{@racket[GL_ARB_occlusion_query]}.

Alias of @racket[glGetQueryObjectuiv].
}
@defproc[(glGetQueryiv (target Integer) (pname Integer)) S32Vector]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetQueryiv.xml"]{glGetQueryiv manpage}.
}
@defproc[(glGetQueryivARB (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/occlusion_query.txt"]{@racket[GL_ARB_occlusion_query]}.

Alias of @racket[glGetQueryiv].
}
@defproc[(glGetRenderbufferParameteriv (target Integer) (pname Integer)) S32Vector]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetRenderbufferParameter.xml"]{glGetRenderbufferParameter manpage}.
}
@defproc[(glGetRenderbufferParameterivEXT (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glGetRenderbufferParameteriv].
}
@defproc[(glGetSamplerParameterIiv (sampler Natural) (pname Integer)) S32Vector]{
Version 1.2.
}
@defproc[(glGetSamplerParameterIuiv (sampler Natural) (pname Integer)) U32Vector]{
Version 1.2.
}
@defproc[(glGetSamplerParameterfv (sampler Natural) (pname Integer)) F32Vector]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetSamplerParameter.xml"]{glGetSamplerParameter manpage}.
}
@defproc[(glGetSamplerParameteriv (sampler Natural) (pname Integer)) S32Vector]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetSamplerParameter.xml"]{glGetSamplerParameter manpage}.
}
@defproc[(glGetSeparableFilter (target Integer) (format Integer) (type Integer) (row GLPointer) (column GLPointer) (span GLPointer)) Void]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetSeparableFilter.xml"]{glGetSeparableFilter manpage}.
}
@defproc[(glGetSeparableFilterEXT (target Integer) (format Integer) (type Integer) (row GLPointer) (column GLPointer) (span GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.
}
@defproc[(glGetShaderInfoLog (shader Natural) (bufSize Integer)) (values Integer Bytes)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetShaderInfoLog.xml"]{glGetShaderInfoLog manpage}.
}
@defproc[(glGetShaderPrecisionFormat (shadertype Integer) (precisiontype Integer)) (values S32Vector Integer)]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetShaderPrecisionFormat.xml"]{glGetShaderPrecisionFormat manpage}.
}
@defproc[(glGetShaderSource (shader Natural) (bufSize Integer)) (values Integer Bytes)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetShaderSource.xml"]{glGetShaderSource manpage}.
}
@defproc[(glGetShaderSourceARB (obj Natural) (maxLength Integer)) (values Integer Bytes)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glGetShaderSource].
}
@defproc[(glGetShaderiv (shader Natural) (pname Integer)) Integer]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetShader.xml"]{glGetShader manpage}.
}
@defproc[(glGetSharpenTexFuncSGIS (target Integer) (points F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/sharpen_texture.txt"]{@racket[GL_SGIS_sharpen_texture]}.
}
@defproc[(glGetString (name Integer)) String]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetString.xml"]{glGetString manpage}.
}
@defproc[(glGetStringi (name Integer) (index Natural)) String]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetString.xml"]{glGetString manpage}.
}
@defproc[(glGetSubroutineIndex (program Natural) (shadertype Integer) (name (U Bytes String))) Natural]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetSubroutineIndex.xml"]{glGetSubroutineIndex manpage}.
}
@defproc[(glGetSubroutineUniformLocation (program Natural) (shadertype Integer) (name (U Bytes String))) Integer]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetSubroutineUniformLocation.xml"]{glGetSubroutineUniformLocation manpage}.
}
@defproc[(glGetSynciv (sync GLSync) (pname Integer) (bufSize Integer)) (values Integer S32Vector)]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetSync.xml"]{glGetSync manpage}.
}
@defproc[(glGetTexBumpParameterfvATI (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/envmap_bumpmap.txt"]{@racket[GL_ATI_envmap_bumpmap]}.
}
@defproc[(glGetTexBumpParameterivATI (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/envmap_bumpmap.txt"]{@racket[GL_ATI_envmap_bumpmap]}.
}
@defproc[(glGetTexEnvfv (target Integer) (pname Integer)) F32Vector]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetTexEnv.xml"]{glGetTexEnv manpage}.
}
@defproc[(glGetTexEnviv (target Integer) (pname Integer)) S32Vector]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetTexEnv.xml"]{glGetTexEnv manpage}.
}
@defproc[(glGetTexFilterFuncSGIS (target Integer) (filter Integer) (weights F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/texture_filter4.txt"]{@racket[GL_SGIS_texture_filter4]}.
}
@defproc[(glGetTexGendv (coord Integer) (pname Integer)) F64Vector]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetTexGen.xml"]{glGetTexGen manpage}.
}
@defproc[(glGetTexGenfv (coord Integer) (pname Integer)) F32Vector]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetTexGen.xml"]{glGetTexGen manpage}.
}
@defproc[(glGetTexGeniv (coord Integer) (pname Integer)) S32Vector]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetTexGen.xml"]{glGetTexGen manpage}.
}
@defproc[(glGetTexImage (target Integer) (level Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetTexImage.xml"]{glGetTexImage manpage}.
}
@defproc[(glGetTexLevelParameterfv (target Integer) (level Integer) (pname Integer)) F32Vector]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetTexLevelParameter.xml"]{glGetTexLevelParameter manpage}.
}
@defproc[(glGetTexLevelParameteriv (target Integer) (level Integer) (pname Integer)) S32Vector]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetTexLevelParameter.xml"]{glGetTexLevelParameter manpage}.
}
@defproc[(glGetTexParameterIiv (target Integer) (pname Integer)) S32Vector]{
Version 3.0.
}
@defproc[(glGetTexParameterIivEXT (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_integer.txt"]{@racket[GL_EXT_texture_integer]}.

Alias of @racket[glGetTexParameterIiv].
}
@defproc[(glGetTexParameterIuiv (target Integer) (pname Integer)) U32Vector]{
Version 3.0.
}
@defproc[(glGetTexParameterIuivEXT (target Integer) (pname Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_integer.txt"]{@racket[GL_EXT_texture_integer]}.

Alias of @racket[glGetTexParameterIuiv].
}
@defproc[(glGetTexParameterPointervAPPLE (target Integer) (pname Integer)) GLPointer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/texture_range.txt"]{@racket[GL_APPLE_texture_range]}.
}
@defproc[(glGetTexParameterfv (target Integer) (pname Integer)) F32Vector]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetTexParameter.xml"]{glGetTexParameter manpage}.
}
@defproc[(glGetTexParameteriv (target Integer) (pname Integer)) S32Vector]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetTexParameter.xml"]{glGetTexParameter manpage}.
}
@defproc[(glGetTextureHandleNV (texture Natural)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glGetTextureImageEXT (texture Natural) (target Integer) (level Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetTextureLevelParameterfvEXT (texture Natural) (target Integer) (level Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetTextureLevelParameterivEXT (texture Natural) (target Integer) (level Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetTextureParameterIivEXT (texture Natural) (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetTextureParameterIuivEXT (texture Natural) (target Integer) (pname Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetTextureParameterfvEXT (texture Natural) (target Integer) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetTextureParameterivEXT (texture Natural) (target Integer) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetTextureSamplerHandleNV (texture Natural) (sampler Natural)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glGetTrackMatrixivNV (target Integer) (address Natural) (pname Integer)) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glGetTransformFeedbackVarying (program Natural) (index Natural) (bufSize Integer)) (values Integer Integer Integer Bytes)]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetTransformFeedbackVarying.xml"]{glGetTransformFeedbackVarying manpage}.
}
@defproc[(glGetTransformFeedbackVaryingEXT (program Natural) (index Natural) (bufSize Integer)) (values Integer Integer Integer Bytes)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/transform_feedback.txt"]{@racket[GL_EXT_transform_feedback]}.

Alias of @racket[glGetTransformFeedbackVarying].
}
@defproc[(glGetTransformFeedbackVaryingNV (program Natural) (index Natural)) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.

Alias of @racket[glGetTransformFeedbackVarying].
}
@defproc[(glGetUniformBlockIndex (program Natural) (uniformBlockName (U Bytes String))) Natural]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniformBlockIndex.xml"]{glGetUniformBlockIndex manpage}.
}
@defproc[(glGetUniformBufferSizeEXT (program Natural) (location Integer)) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/bindable_uniform.txt"]{@racket[GL_EXT_bindable_uniform]}.
}
@defproc[(glGetUniformIndices (program Natural) (uniformCount Integer) (uniformNames (Vectorof (U Bytes String)))) U32Vector]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniformIndices.xml"]{glGetUniformIndices manpage}.
}
@defproc[(glGetUniformLocation (program Natural) (name (U Bytes String))) Integer]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniformLocation.xml"]{glGetUniformLocation manpage}.
}
@defproc[(glGetUniformLocationARB (programObj Natural) (name (U Bytes String))) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glGetUniformLocation].
}
@defproc[(glGetUniformOffsetEXT (program Natural) (location Integer)) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/bindable_uniform.txt"]{@racket[GL_EXT_bindable_uniform]}.
}
@defproc[(glGetUniformSubroutineuiv (shadertype Integer) (location Integer)) Natural]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniformSubroutine.xml"]{glGetUniformSubroutine manpage}.
}
@defproc[(glGetUniformdv (program Natural) (location Integer) (params F64Vector)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniform.xml"]{glGetUniform manpage}.
}
@defproc[(glGetUniformfv (program Natural) (location Integer) (params F32Vector)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniform.xml"]{glGetUniform manpage}.
}
@defproc[(glGetUniformfvARB (programObj Natural) (location Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glGetUniformfv].
}
@defproc[(glGetUniformi64vNV (program Natural) (location Integer) (params S64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glGetUniformiv (program Natural) (location Integer) (params S32Vector)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniform.xml"]{glGetUniform manpage}.
}
@defproc[(glGetUniformivARB (programObj Natural) (location Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glGetUniformiv].
}
@defproc[(glGetUniformui64vNV (program Natural) (location Integer) (params U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glGetUniformuiv (program Natural) (location Integer) (params U32Vector)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniform.xml"]{glGetUniform manpage}.
}
@defproc[(glGetUniformuivEXT (program Natural) (location Integer) (params U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glGetUniformuiv].
}
@defproc[(glGetVariantArrayObjectfvATI (id Natural) (pname Integer)) Flonum]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glGetVariantArrayObjectivATI (id Natural) (pname Integer)) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glGetVariantBooleanvEXT (id Natural) (value Integer) (data CPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetVariantFloatvEXT (id Natural) (value Integer) (data F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetVariantIntegervEXT (id Natural) (value Integer) (data S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetVariantPointervEXT (id Natural) (value Integer) (data CPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetVaryingLocationNV (program Natural) (name (U Bytes String))) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.
}
@defproc[(glGetVertexAttribArrayObjectfvATI (index Natural) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_attrib_array_object.txt"]{@racket[GL_ATI_vertex_attrib_array_object]}.
}
@defproc[(glGetVertexAttribArrayObjectivATI (index Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_attrib_array_object.txt"]{@racket[GL_ATI_vertex_attrib_array_object]}.
}
@defproc[(glGetVertexAttribIiv (index Natural) (pname Integer)) S32Vector]{
Version 3.0.
}
@defproc[(glGetVertexAttribIivEXT (index Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glGetVertexAttribIiv].
}
@defproc[(glGetVertexAttribIuiv (index Natural) (pname Integer)) U32Vector]{
Version 3.0.
}
@defproc[(glGetVertexAttribIuivEXT (index Natural) (pname Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glGetVertexAttribIuiv].
}
@defproc[(glGetVertexAttribLdv (index Natural) (pname Integer)) F64Vector]{
Version 4.1.
}
@defproc[(glGetVertexAttribLdvEXT (index Natural) (pname Integer)) F64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glGetVertexAttribLi64vNV (index Natural) (pname Integer)) S64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glGetVertexAttribLui64vNV (index Natural) (pname Integer)) U64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glGetVertexAttribPointerv (index Natural) (pname Integer)) GLPointer]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetVertexAttribPointerv.xml"]{glGetVertexAttribPointerv manpage}.
}
@defproc[(glGetVertexAttribPointervARB (index Natural) (pname Integer)) GLPointer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glGetVertexAttribPointerv].
}
@defproc[(glGetVertexAttribPointervNV (index Natural) (pname Integer)) GLPointer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glGetVertexAttribPointerv].
}
@defproc[(glGetVertexAttribdv (index Natural) (pname Integer)) F64Vector]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetVertexAttrib.xml"]{glGetVertexAttrib manpage}.
}
@defproc[(glGetVertexAttribdvARB (index Natural) (pname Integer)) F64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glGetVertexAttribdv].
}
@defproc[(glGetVertexAttribdvNV (index Natural) (pname Integer)) Real]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glGetVertexAttribdv].
}
@defproc[(glGetVertexAttribfv (index Natural) (pname Integer)) F32Vector]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetVertexAttrib.xml"]{glGetVertexAttrib manpage}.
}
@defproc[(glGetVertexAttribfvARB (index Natural) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glGetVertexAttribfv].
}
@defproc[(glGetVertexAttribfvNV (index Natural) (pname Integer)) Flonum]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glGetVertexAttribfv].
}
@defproc[(glGetVertexAttribiv (index Natural) (pname Integer)) S32Vector]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetVertexAttrib.xml"]{glGetVertexAttrib manpage}.
}
@defproc[(glGetVertexAttribivARB (index Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glGetVertexAttribiv].
}
@defproc[(glGetVertexAttribivNV (index Natural) (pname Integer)) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glGetVertexAttribiv].
}
@defproc[(glGetVideoCaptureStreamdvNV (video_capture_slot Natural) (stream Natural) (pname Integer)) F64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glGetVideoCaptureStreamfvNV (video_capture_slot Natural) (stream Natural) (pname Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glGetVideoCaptureStreamivNV (video_capture_slot Natural) (stream Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glGetVideoCaptureivNV (video_capture_slot Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glGetVideoi64vNV (video_slot Natural) (pname Integer)) S64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/present_video.txt"]{@racket[GL_NV_present_video]}.
}
@defproc[(glGetVideoivNV (video_slot Natural) (pname Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/present_video.txt"]{@racket[GL_NV_present_video]}.
}
@defproc[(glGetVideoui64vNV (video_slot Natural) (pname Integer)) U64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/present_video.txt"]{@racket[GL_NV_present_video]}.
}
@defproc[(glGetVideouivNV (video_slot Natural) (pname Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/present_video.txt"]{@racket[GL_NV_present_video]}.
}
@defproc[(glGetnColorTableARB (target Integer) (format Integer) (type Integer) (bufSize Integer) (table GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnCompressedTexImageARB (target Integer) (lod Integer) (bufSize Integer) (img GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnConvolutionFilterARB (target Integer) (format Integer) (type Integer) (bufSize Integer) (image GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnHistogramARB (target Integer) (reset Boolean) (format Integer) (type Integer) (bufSize Integer) (the-values GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnMapdvARB (target Integer) (query Integer) (bufSize Integer)) F64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnMapfvARB (target Integer) (query Integer) (bufSize Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnMapivARB (target Integer) (query Integer) (bufSize Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnMinmaxARB (target Integer) (reset Boolean) (format Integer) (type Integer) (bufSize Integer) (the-values GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnPixelMapfvARB (map Integer) (bufSize Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnPixelMapuivARB (map Integer) (bufSize Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnPixelMapusvARB (map Integer) (bufSize Integer)) U16Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnPolygonStippleARB (bufSize Integer)) Bytes]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnSeparableFilterARB (target Integer) (format Integer) (type Integer) (rowBufSize Integer) (row GLPointer) (columnBufSize Integer) (column GLPointer) (span GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnTexImageARB (target Integer) (level Integer) (format Integer) (type Integer) (bufSize Integer) (img GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnUniformdvARB (program Natural) (location Integer) (bufSize Integer)) F64Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnUniformfvARB (program Natural) (location Integer) (bufSize Integer)) F32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnUniformivARB (program Natural) (location Integer) (bufSize Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnUniformuivARB (program Natural) (location Integer) (bufSize Integer)) U32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGlobalAlphaFactorbSUN (factor Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
@defproc[(glGlobalAlphaFactordSUN (factor Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
@defproc[(glGlobalAlphaFactorfSUN (factor Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
@defproc[(glGlobalAlphaFactoriSUN (factor Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
@defproc[(glGlobalAlphaFactorsSUN (factor Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
@defproc[(glGlobalAlphaFactorubSUN (factor Byte)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
@defproc[(glGlobalAlphaFactoruiSUN (factor Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
@defproc[(glGlobalAlphaFactorusSUN (factor Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
