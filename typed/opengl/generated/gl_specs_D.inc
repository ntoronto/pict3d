#lang scribble/manual
@title{glD...}
@defproc[(glDebugMessageControl (source Integer) (type Integer) (severity Integer) (count Integer) (ids U32Vector) (enabled Boolean)) Void]{
Version 4.3.
}
@defproc[(glDebugMessageControlARB (source Integer) (type Integer) (severity Integer) (count Integer) (ids U32Vector) (enabled Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/debug_output.txt"]{@racket[GL_ARB_debug_output]}.
}
@defproc[(glDebugMessageEnableAMD (category Integer) (severity Integer) (count Integer) (ids U32Vector) (enabled Boolean)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/debug_output.txt"]{@racket[GL_AMD_debug_output]}.
}
@defproc[(glDebugMessageInsert (source Integer) (type Integer) (id Natural) (severity Integer) (length Integer) (buf (U Bytes String))) Void]{
Version 4.3.
}
@defproc[(glDebugMessageInsertAMD (category Integer) (severity Integer) (id Natural) (length Integer) (buf (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/debug_output.txt"]{@racket[GL_AMD_debug_output]}.
}
@defproc[(glDebugMessageInsertARB (source Integer) (type Integer) (id Natural) (severity Integer) (length Integer) (buf (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/debug_output.txt"]{@racket[GL_ARB_debug_output]}.
}
@defproc[(glDeformSGIX (mask Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/polynomial_ffd.txt"]{@racket[GL_SGIX_polynomial_ffd]}.
}
@defproc[(glDeformationMap3dSGIX (target Integer) (u1 Real) (u2 Real) (ustride Integer) (uorder Integer) (v1 Real) (v2 Real) (vstride Integer) (vorder Integer) (w1 Real) (w2 Real) (wstride Integer) (worder Integer) (points F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/polynomial_ffd.txt"]{@racket[GL_SGIX_polynomial_ffd]}.
}
@defproc[(glDeformationMap3fSGIX (target Integer) (u1 Flonum) (u2 Flonum) (ustride Integer) (uorder Integer) (v1 Flonum) (v2 Flonum) (vstride Integer) (vorder Integer) (w1 Flonum) (w2 Flonum) (wstride Integer) (worder Integer) (points F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/polynomial_ffd.txt"]{@racket[GL_SGIX_polynomial_ffd]}.
}
@defproc[(glDeleteAsyncMarkersSGIX (marker Natural) (range Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/async.txt"]{@racket[GL_SGIX_async]}.
}
@defproc[(glDeleteBuffers (n Integer) (buffers U32Vector)) Void]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDeleteBuffers.xml"]{glDeleteBuffers manpage}.
}
@defproc[(glDeleteBuffersARB (n Integer) (buffers U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glDeleteBuffers].
}
@defproc[(glDeleteFencesAPPLE (n Integer) (fences U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/fence.txt"]{@racket[GL_APPLE_fence]}.
}
@defproc[(glDeleteFencesNV (n Integer) (fences U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fence.txt"]{@racket[GL_NV_fence]}.
}
@defproc[(glDeleteFragmentShaderATI (id Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glDeleteFramebuffers (n Integer) (framebuffers U32Vector)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDeleteFramebuffers.xml"]{glDeleteFramebuffers manpage}.
}
@defproc[(glDeleteFramebuffersEXT (n Integer) (framebuffers U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glDeleteFramebuffers].
}
@defproc[(glDeleteLists (list Natural) (range Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glDeleteLists.xml"]{glDeleteLists manpage}.
}
@defproc[(glDeleteNamedStringARB (namelen Integer) (name (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shading_language_include.txt"]{@racket[GL_ARB_shading_language_include]}.
}
@defproc[(glDeleteNamesAMD (identifier Integer) (num Natural) (names U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/name_gen_delete.txt"]{@racket[GL_AMD_name_gen_delete]}.
}
@defproc[(glDeleteObjectARB (obj Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.
}
@defproc[(glDeleteOcclusionQueriesNV (n Integer) (ids U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/occlusion_query.txt"]{@racket[GL_NV_occlusion_query]}.
}
@defproc[(glDeletePathsNV (path Natural) (range Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glDeletePerfMonitorsAMD (n Integer) (monitors U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glDeleteProgram (program Natural)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDeleteProgram.xml"]{glDeleteProgram manpage}.
}
@defproc[(glDeleteProgramPipelines (n Integer) (pipelines U32Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDeleteProgramPipelines.xml"]{glDeleteProgramPipelines manpage}.
}
@defproc[(glDeleteProgramsARB (n Integer) (programs U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glDeleteProgramsNV (n Integer) (programs U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glDeleteProgramsARB].
}
@defproc[(glDeleteQueries (n Integer) (ids U32Vector)) Void]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDeleteQueries.xml"]{glDeleteQueries manpage}.
}
@defproc[(glDeleteQueriesARB (n Integer) (ids U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/occlusion_query.txt"]{@racket[GL_ARB_occlusion_query]}.

Alias of @racket[glDeleteQueries].
}
@defproc[(glDeleteRenderbuffers (n Integer) (renderbuffers U32Vector)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDeleteRenderbuffers.xml"]{glDeleteRenderbuffers manpage}.
}
@defproc[(glDeleteRenderbuffersEXT (n Integer) (renderbuffers U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glDeleteRenderbuffers].
}
@defproc[(glDeleteSamplers (count Integer) (samplers U32Vector)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDeleteSamplers.xml"]{glDeleteSamplers manpage}.
}
@defproc[(glDeleteShader (shader Natural)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDeleteShader.xml"]{glDeleteShader manpage}.
}
@defproc[(glDeleteSync (sync GLSync)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDeleteSync.xml"]{glDeleteSync manpage}.
}
@defproc[(glDeleteTextures (n Integer) (textures U32Vector)) Void]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDeleteTextures.xml"]{glDeleteTextures manpage}.
}
@defproc[(glDeleteTexturesEXT (n Integer) (textures U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_object.txt"]{@racket[GL_EXT_texture_object]}.
}
@defproc[(glDeleteTransformFeedbacks (n Integer) (ids U32Vector)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDeleteTransformFeedbacks.xml"]{glDeleteTransformFeedbacks manpage}.
}
@defproc[(glDeleteTransformFeedbacksNV (n Integer) (ids U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback2.txt"]{@racket[GL_NV_transform_feedback2]}.
}
@defproc[(glDeleteVertexArrays (n Integer) (arrays U32Vector)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDeleteVertexArrays.xml"]{glDeleteVertexArrays manpage}.
}
@defproc[(glDeleteVertexArraysAPPLE (n Integer) (arrays U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_array_object.txt"]{@racket[GL_APPLE_vertex_array_object]}.

Alias of @racket[glDeleteVertexArrays].
}
@defproc[(glDeleteVertexShaderEXT (id Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glDepthBoundsEXT (zmin Real) (zmax Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/depth_bounds_test.txt"]{@racket[GL_EXT_depth_bounds_test]}.
}
@defproc[(glDepthBoundsdNV (zmin Real) (zmax Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/depth_buffer_float.txt"]{@racket[GL_NV_depth_buffer_float]}.
}
@defproc[(glDepthFunc (func Integer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDepthFunc.xml"]{glDepthFunc manpage}.
}
@defproc[(glDepthMask (flag Boolean)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDepthMask.xml"]{glDepthMask manpage}.
}
@defproc[(glDepthRange (near Real) (far Real)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDepthRange.xml"]{glDepthRange manpage}.
}
@defproc[(glDepthRangeArrayv (first Natural) (count Integer) (v F64Vector)) Void]{
Version 4.1.
}
@defproc[(glDepthRangeIndexed (index Natural) (n Real) (f Real)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDepthRangeIndexed.xml"]{glDepthRangeIndexed manpage}.
}
@defproc[(glDepthRangedNV (zNear Real) (zFar Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/depth_buffer_float.txt"]{@racket[GL_NV_depth_buffer_float]}.
}
@defproc[(glDepthRangef (n Flonum) (f Flonum)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDepthRange.xml"]{glDepthRange manpage}.
}
@defproc[(glDetachObjectARB (containerObj Natural) (attachedObj Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glDetachShader].
}
@defproc[(glDetachShader (program Natural) (shader Natural)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDetachShader.xml"]{glDetachShader manpage}.
}
@defproc[(glDetailTexFuncSGIS (target Integer) (n Integer) (points F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/detail_texture.txt"]{@racket[GL_SGIS_detail_texture]}.
}
@defproc[(glDisable (cap Integer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDisable.xml"]{glDisable manpage}.
}
@defproc[(glDisableClientState (array Integer)) Void]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glDisableClientState.xml"]{glDisableClientState manpage}.
}
@defproc[(glDisableClientStateIndexedEXT (array Integer) (index Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glDisableIndexedEXT (target Integer) (index Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/draw_buffers2.txt"]{@racket[GL_EXT_draw_buffers2]}.

Alias of @racket[glDisablei].
}
@defproc[(glDisableVariantClientStateEXT (id Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glDisableVertexAttribAPPLE (index Natural) (pname Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_program_evaluators.txt"]{@racket[GL_APPLE_vertex_program_evaluators]}.
}
@defproc[(glDisableVertexAttribArray (index Natural)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDisableVertexAttribArray.xml"]{glDisableVertexAttribArray manpage}.
}
@defproc[(glDisableVertexAttribArrayARB (index Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glDisableVertexAttribArray].
}
@defproc[(glDisablei (target Integer) (index Natural)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDisable.xml"]{glDisable manpage}.
}
@defproc[(glDispatchCompute (num_groups_x Natural) (num_groups_y Natural) (num_groups_z Natural)) Void]{
Version 4.3.
}
@defproc[(glDispatchComputeIndirect (indirect Integer)) Void]{
Version 4.3.
}
@defproc[(glDrawArrays (mode Integer) (first Integer) (count Integer)) Void]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawArrays.xml"]{glDrawArrays manpage}.
}
@defproc[(glDrawArraysEXT (mode Integer) (first Integer) (count Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.

Alias of @racket[glDrawArrays].
}
@defproc[(glDrawArraysIndirect (mode Integer) (indirect GLPointer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawArraysIndirect.xml"]{glDrawArraysIndirect manpage}.
}
@defproc[(glDrawArraysInstanced (mode Integer) (first Integer) (count Integer) (instancecount Integer)) Void]{
Version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawArraysInstanced.xml"]{glDrawArraysInstanced manpage}.
}
@defproc[(glDrawArraysInstancedARB (mode Integer) (first Integer) (count Integer) (primcount Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/draw_instanced.txt"]{@racket[GL_ARB_draw_instanced]}.

Alias of @racket[glDrawArraysInstanced].
}
@defproc[(glDrawArraysInstancedBaseInstance (mode Integer) (first Integer) (count Integer) (instancecount Integer) (baseinstance Natural)) Void]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawArraysInstancedBaseInstance.xml"]{glDrawArraysInstancedBaseInstance manpage}.
}
@defproc[(glDrawArraysInstancedEXT (mode Integer) (start Integer) (count Integer) (primcount Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/draw_instanced.txt"]{@racket[GL_EXT_draw_instanced]}.

Alias of @racket[glDrawArraysInstancedARB].
}
@defproc[(glDrawBuffer (mode Integer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawBuffer.xml"]{glDrawBuffer manpage}.
}
@defproc[(glDrawBuffers (n Integer) (bufs S32Vector)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawBuffers.xml"]{glDrawBuffers manpage}.
}
@defproc[(glDrawBuffersARB (n Integer) (bufs S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/draw_buffers.txt"]{@racket[GL_ARB_draw_buffers]}.

Alias of @racket[glDrawBuffers].
}
@defproc[(glDrawBuffersATI (n Integer) (bufs S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/draw_buffers.txt"]{@racket[GL_ATI_draw_buffers]}.

Alias of @racket[glDrawBuffers].
}
@defproc[(glDrawElementArrayAPPLE (mode Integer) (first Integer) (count Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/element_array.txt"]{@racket[GL_APPLE_element_array]}.
}
@defproc[(glDrawElementArrayATI (mode Integer) (count Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/element_array.txt"]{@racket[GL_ATI_element_array]}.
}
@defproc[(glDrawElements (mode Integer) (count Integer) (type Integer) (indices GLPointer)) Void]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawElements.xml"]{glDrawElements manpage}.
}
@defproc[(glDrawElementsBaseVertex (mode Integer) (count Integer) (type Integer) (indices GLPointer) (basevertex Integer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawElementsBaseVertex.xml"]{glDrawElementsBaseVertex manpage}.
}
@defproc[(glDrawElementsIndirect (mode Integer) (type Integer) (indirect GLPointer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawElementsIndirect.xml"]{glDrawElementsIndirect manpage}.
}
@defproc[(glDrawElementsInstanced (mode Integer) (count Integer) (type Integer) (indices GLPointer) (instancecount Integer)) Void]{
Version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawElementsInstanced.xml"]{glDrawElementsInstanced manpage}.
}
@defproc[(glDrawElementsInstancedARB (mode Integer) (count Integer) (type Integer) (indices GLPointer) (primcount Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/draw_instanced.txt"]{@racket[GL_ARB_draw_instanced]}.

Alias of @racket[glDrawElementsInstanced].
}
@defproc[(glDrawElementsInstancedBaseInstance (mode Integer) (count Integer) (type Integer) (indices GLPointer) (instancecount Integer) (baseinstance Natural)) Void]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawElementsInstancedBaseInstance.xml"]{glDrawElementsInstancedBaseInstance manpage}.
}
@defproc[(glDrawElementsInstancedBaseVertex (mode Integer) (count Integer) (type Integer) (indices GLPointer) (instancecount Integer) (basevertex Integer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawElementsInstancedBaseVertex.xml"]{glDrawElementsInstancedBaseVertex manpage}.
}
@defproc[(glDrawElementsInstancedBaseVertexBaseInstance (mode Integer) (count Integer) (type Integer) (indices GLPointer) (instancecount Integer) (basevertex Integer) (baseinstance Natural)) Void]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawElementsInstancedBaseVertexBaseInstance.xml"]{glDrawElementsInstancedBaseVertexBaseInstance manpage}.
}
@defproc[(glDrawElementsInstancedEXT (mode Integer) (count Integer) (type Integer) (indices GLPointer) (primcount Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/draw_instanced.txt"]{@racket[GL_EXT_draw_instanced]}.

Alias of @racket[glDrawElementsInstancedARB].
}
@defproc[(glDrawMeshArraysSUN (mode Integer) (first Integer) (count Integer) (width Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/mesh_array.txt"]{@racket[GL_SUN_mesh_array]}.
}
@defproc[(glDrawPixels (width Integer) (height Integer) (format Integer) (type Integer) (pixels GLPointer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glDrawPixels.xml"]{glDrawPixels manpage}.
}
@defproc[(glDrawRangeElementArrayAPPLE (mode Integer) (start Natural) (end Natural) (first Integer) (count Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/element_array.txt"]{@racket[GL_APPLE_element_array]}.
}
@defproc[(glDrawRangeElementArrayATI (mode Integer) (start Natural) (end Natural) (count Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/element_array.txt"]{@racket[GL_ATI_element_array]}.
}
@defproc[(glDrawRangeElements (mode Integer) (start Natural) (end Natural) (count Integer) (type Integer) (indices GLPointer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawRangeElements.xml"]{glDrawRangeElements manpage}.
}
@defproc[(glDrawRangeElementsBaseVertex (mode Integer) (start Natural) (end Natural) (count Integer) (type Integer) (indices GLPointer) (basevertex Integer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawRangeElementsBaseVertex.xml"]{glDrawRangeElementsBaseVertex manpage}.
}
@defproc[(glDrawRangeElementsEXT (mode Integer) (start Natural) (end Natural) (count Integer) (type Integer) (indices GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/draw_range_elements.txt"]{@racket[GL_EXT_draw_range_elements]}.

Alias of @racket[glDrawRangeElements].
}
@defproc[(glDrawTransformFeedback (mode Integer) (id Natural)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawTransformFeedback.xml"]{glDrawTransformFeedback manpage}.
}
@defproc[(glDrawTransformFeedbackInstanced (mode Integer) (id Natural) (instancecount Integer)) Void]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawTransformFeedbackInstanced.xml"]{glDrawTransformFeedbackInstanced manpage}.
}
@defproc[(glDrawTransformFeedbackNV (mode Integer) (id Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback2.txt"]{@racket[GL_NV_transform_feedback2]}.
}
@defproc[(glDrawTransformFeedbackStream (mode Integer) (id Natural) (stream Natural)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawTransformFeedbackStream.xml"]{glDrawTransformFeedbackStream manpage}.
}
@defproc[(glDrawTransformFeedbackStreamInstanced (mode Integer) (id Natural) (stream Natural) (instancecount Integer)) Void]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glDrawTransformFeedbackStreamInstanced.xml"]{glDrawTransformFeedbackStreamInstanced manpage}.
}
