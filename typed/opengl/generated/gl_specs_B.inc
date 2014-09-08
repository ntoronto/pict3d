#lang scribble/manual
@title{glB...}
@defproc[(glBegin (mode Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glBegin.xml"]{glBegin manpage}.
}
@defproc[(glBeginConditionalRender (id Natural) (mode Integer)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBeginConditionalRender.xml"]{glBeginConditionalRender manpage}.
}
@defproc[(glBeginConditionalRenderNV (id Natural) (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/conditional_render.txt"]{@racket[GL_NV_conditional_render]}.

Alias of @racket[glBeginConditionalRender].
}
@defproc[(glBeginFragmentShaderATI) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glBeginOcclusionQueryNV (id Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/occlusion_query.txt"]{@racket[GL_NV_occlusion_query]}.
}
@defproc[(glBeginPerfMonitorAMD (monitor Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glBeginQuery (target Integer) (id Natural)) Void]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBeginQuery.xml"]{glBeginQuery manpage}.
}
@defproc[(glBeginQueryARB (target Integer) (id Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/occlusion_query.txt"]{@racket[GL_ARB_occlusion_query]}.

Alias of @racket[glBeginQuery].
}
@defproc[(glBeginQueryIndexed (target Integer) (index Natural) (id Natural)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBeginQueryIndexed.xml"]{glBeginQueryIndexed manpage}.
}
@defproc[(glBeginTransformFeedback (primitiveMode Integer)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBeginTransformFeedback.xml"]{glBeginTransformFeedback manpage}.
}
@defproc[(glBeginTransformFeedbackEXT (primitiveMode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/transform_feedback.txt"]{@racket[GL_EXT_transform_feedback]}.

Alias of @racket[glBeginTransformFeedback].
}
@defproc[(glBeginTransformFeedbackNV (primitiveMode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.

Alias of @racket[glBeginTransformFeedback].
}
@defproc[(glBeginVertexShaderEXT) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glBeginVideoCaptureNV (video_capture_slot Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glBindAttribLocation (program Natural) (index Natural) (name (U Bytes String))) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindAttribLocation.xml"]{glBindAttribLocation manpage}.
}
@defproc[(glBindAttribLocationARB (programObj Natural) (index Natural) (name (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_shader.txt"]{@racket[GL_ARB_vertex_shader]}.

Alias of @racket[glBindAttribLocation].
}
@defproc[(glBindBuffer (target Integer) (buffer Natural)) Void]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindBuffer.xml"]{glBindBuffer manpage}.
}
@defproc[(glBindBufferARB (target Integer) (buffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glBindBuffer].
}
@defproc[(glBindBufferBase (target Integer) (index Natural) (buffer Natural)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindBufferBase.xml"]{glBindBufferBase manpage}.
}
@defproc[(glBindBufferBaseEXT (target Integer) (index Natural) (buffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/transform_feedback.txt"]{@racket[GL_EXT_transform_feedback]}.

Alias of @racket[glBindBufferBase].
}
@defproc[(glBindBufferBaseNV (target Integer) (index Natural) (buffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.

Alias of @racket[glBindBufferBase].
}
@defproc[(glBindBufferOffsetEXT (target Integer) (index Natural) (buffer Natural) (offset Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/transform_feedback.txt"]{@racket[GL_EXT_transform_feedback]}.
}
@defproc[(glBindBufferOffsetNV (target Integer) (index Natural) (buffer Natural) (offset Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.

Alias of @racket[glBindBufferOffsetEXT].
}
@defproc[(glBindBufferRange (target Integer) (index Natural) (buffer Natural) (offset Integer) (size Integer)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindBufferRange.xml"]{glBindBufferRange manpage}.
}
@defproc[(glBindBufferRangeEXT (target Integer) (index Natural) (buffer Natural) (offset Integer) (size Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/transform_feedback.txt"]{@racket[GL_EXT_transform_feedback]}.

Alias of @racket[glBindBufferRange].
}
@defproc[(glBindBufferRangeNV (target Integer) (index Natural) (buffer Natural) (offset Integer) (size Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.

Alias of @racket[glBindBufferRange].
}
@defproc[(glBindFragDataLocation (program Natural) (color Natural) (name (U Bytes String))) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindFragDataLocation.xml"]{glBindFragDataLocation manpage}.
}
@defproc[(glBindFragDataLocationEXT (program Natural) (color Natural) (name (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glBindFragDataLocation].
}
@defproc[(glBindFragDataLocationIndexed (program Natural) (colorNumber Natural) (index Natural) (name (U Bytes String))) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindFragDataLocationIndexed.xml"]{glBindFragDataLocationIndexed manpage}.
}
@defproc[(glBindFragmentShaderATI (id Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glBindFramebuffer (target Integer) (framebuffer Natural)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindFramebuffer.xml"]{glBindFramebuffer manpage}.
}
@defproc[(glBindFramebufferEXT (target Integer) (framebuffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.
}
@defproc[(glBindImageTexture (unit Natural) (texture Natural) (level Integer) (layered Boolean) (layer Integer) (access Integer) (format Integer)) Void]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindImageTexture.xml"]{glBindImageTexture manpage}.
}
@defproc[(glBindImageTextureEXT (index Natural) (texture Natural) (level Integer) (layered Boolean) (layer Integer) (access Integer) (format Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/shader_image_load_store.txt"]{@racket[GL_EXT_shader_image_load_store]}.
}
@defproc[(glBindLightParameterEXT (light Integer) (value Integer)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glBindMaterialParameterEXT (face Integer) (value Integer)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glBindMultiTextureEXT (texunit Integer) (target Integer) (texture Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glBindParameterEXT (value Integer)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glBindProgramARB (target Integer) (program Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glBindProgramNV (target Integer) (id Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glBindProgramARB].
}
@defproc[(glBindProgramPipeline (pipeline Natural)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindProgramPipeline.xml"]{glBindProgramPipeline manpage}.
}
@defproc[(glBindRenderbuffer (target Integer) (renderbuffer Natural)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindRenderbuffer.xml"]{glBindRenderbuffer manpage}.
}
@defproc[(glBindRenderbufferEXT (target Integer) (renderbuffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.
}
@defproc[(glBindSampler (unit Natural) (sampler Natural)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindSampler.xml"]{glBindSampler manpage}.
}
@defproc[(glBindTexGenParameterEXT (unit Integer) (coord Integer) (value Integer)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glBindTexture (target Integer) (texture Natural)) Void]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindTexture.xml"]{glBindTexture manpage}.
}
@defproc[(glBindTextureEXT (target Integer) (texture Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_object.txt"]{@racket[GL_EXT_texture_object]}.

Alias of @racket[glBindTexture].
}
@defproc[(glBindTextureUnitParameterEXT (unit Integer) (value Integer)) Natural]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glBindTransformFeedback (target Integer) (id Natural)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindTransformFeedback.xml"]{glBindTransformFeedback manpage}.
}
@defproc[(glBindTransformFeedbackNV (target Integer) (id Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback2.txt"]{@racket[GL_NV_transform_feedback2]}.
}
@defproc[(glBindVertexArray (array Natural)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindVertexArray.xml"]{glBindVertexArray manpage}.
}
@defproc[(glBindVertexArrayAPPLE (array Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_array_object.txt"]{@racket[GL_APPLE_vertex_array_object]}.

Alias of @racket[glBindVertexArray].
}
@defproc[(glBindVertexBuffer (bindingindex Natural) (buffer Natural) (offset Integer) (stride Integer)) Void]{
Version 4.3.
}
@defproc[(glBindVertexShaderEXT (id Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glBindVideoCaptureStreamBufferNV (video_capture_slot Natural) (stream Natural) (frame_region Integer) (offset Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glBindVideoCaptureStreamTextureNV (video_capture_slot Natural) (stream Natural) (frame_region Integer) (target Integer) (texture Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glBinormal3bEXT (bx Fixnum) (by Fixnum) (bz Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3bvEXT (v S8Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3dEXT (bx Real) (by Real) (bz Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3dvEXT (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3fEXT (bx Flonum) (by Flonum) (bz Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3fvEXT (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3iEXT (bx Integer) (by Integer) (bz Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3ivEXT (v S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3sEXT (bx Fixnum) (by Fixnum) (bz Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3svEXT (v S16Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormalPointerEXT (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBitmap (width Integer) (height Integer) (xorig Flonum) (yorig Flonum) (xmove Flonum) (ymove Flonum) (bitmap (U Bytes String))) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glBitmap.xml"]{glBitmap manpage}.
}
@defproc[(glBlendColor (red Flonum) (green Flonum) (blue Flonum) (alpha Flonum)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendColor.xml"]{glBlendColor manpage}.
}
@defproc[(glBlendColorEXT (red Flonum) (green Flonum) (blue Flonum) (alpha Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/blend_color.txt"]{@racket[GL_EXT_blend_color]}.

Alias of @racket[glBlendColor].
}
@defproc[(glBlendEquation (mode Integer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendEquation.xml"]{glBlendEquation manpage}.
}
@defproc[(glBlendEquationEXT (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/blend_minmax.txt"]{@racket[GL_EXT_blend_minmax]}.

Alias of @racket[glBlendEquation].
}
@defproc[(glBlendEquationIndexedAMD (buf Natural) (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/draw_buffers_blend.txt"]{@racket[GL_AMD_draw_buffers_blend]}.
}
@defproc[(glBlendEquationSeparate (modeRGB Integer) (modeAlpha Integer)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendEquationSeparate.xml"]{glBlendEquationSeparate manpage}.
}
@defproc[(glBlendEquationSeparateEXT (modeRGB Integer) (modeAlpha Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/blend_equation_separate.txt"]{@racket[GL_EXT_blend_equation_separate]}.

Alias of @racket[glBlendEquationSeparate].
}
@defproc[(glBlendEquationSeparateIndexedAMD (buf Natural) (modeRGB Integer) (modeAlpha Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/draw_buffers_blend.txt"]{@racket[GL_AMD_draw_buffers_blend]}.
}
@defproc[(glBlendEquationSeparatei (buf Natural) (modeRGB Integer) (modeAlpha Integer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendEquationSeparate.xml"]{glBlendEquationSeparate manpage}.
}
@defproc[(glBlendEquationSeparateiARB (buf Natural) (modeRGB Integer) (modeAlpha Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/draw_buffers_blend.txt"]{@racket[GL_ARB_draw_buffers_blend]}.

Alias of @racket[glBlendEquationSeparatei].
}
@defproc[(glBlendEquationi (buf Natural) (mode Integer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendEquation.xml"]{glBlendEquation manpage}.
}
@defproc[(glBlendEquationiARB (buf Natural) (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/draw_buffers_blend.txt"]{@racket[GL_ARB_draw_buffers_blend]}.

Alias of @racket[glBlendEquationi].
}
@defproc[(glBlendFunc (sfactor Integer) (dfactor Integer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendFunc.xml"]{glBlendFunc manpage}.
}
@defproc[(glBlendFuncIndexedAMD (buf Natural) (src Integer) (dst Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/draw_buffers_blend.txt"]{@racket[GL_AMD_draw_buffers_blend]}.
}
@defproc[(glBlendFuncSeparate (sfactorRGB Integer) (dfactorRGB Integer) (sfactorAlpha Integer) (dfactorAlpha Integer)) Void]{
Version 1.4.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendFuncSeparate.xml"]{glBlendFuncSeparate manpage}.
}
@defproc[(glBlendFuncSeparateEXT (sfactorRGB Integer) (dfactorRGB Integer) (sfactorAlpha Integer) (dfactorAlpha Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/blend_func_separate.txt"]{@racket[GL_EXT_blend_func_separate]}.

Alias of @racket[glBlendFuncSeparate].
}
@defproc[(glBlendFuncSeparateINGR (sfactorRGB Integer) (dfactorRGB Integer) (sfactorAlpha Integer) (dfactorAlpha Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/INGR/blend_func_separate.txt"]{@racket[GL_INGR_blend_func_separate]}.

Alias of @racket[glBlendFuncSeparateEXT].
}
@defproc[(glBlendFuncSeparateIndexedAMD (buf Natural) (srcRGB Integer) (dstRGB Integer) (srcAlpha Integer) (dstAlpha Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/draw_buffers_blend.txt"]{@racket[GL_AMD_draw_buffers_blend]}.
}
@defproc[(glBlendFuncSeparatei (buf Natural) (srcRGB Integer) (dstRGB Integer) (srcAlpha Integer) (dstAlpha Integer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendFuncSeparate.xml"]{glBlendFuncSeparate manpage}.
}
@defproc[(glBlendFuncSeparateiARB (buf Natural) (srcRGB Integer) (dstRGB Integer) (srcAlpha Integer) (dstAlpha Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/draw_buffers_blend.txt"]{@racket[GL_ARB_draw_buffers_blend]}.

Alias of @racket[glBlendFuncSeparatei].
}
@defproc[(glBlendFunci (buf Natural) (src Integer) (dst Integer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendFunc.xml"]{glBlendFunc manpage}.
}
@defproc[(glBlendFunciARB (buf Natural) (src Integer) (dst Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/draw_buffers_blend.txt"]{@racket[GL_ARB_draw_buffers_blend]}.

Alias of @racket[glBlendFunci].
}
@defproc[(glBlitFramebuffer (srcX0 Integer) (srcY0 Integer) (srcX1 Integer) (srcY1 Integer) (dstX0 Integer) (dstY0 Integer) (dstX1 Integer) (dstY1 Integer) (mask Natural) (filter Integer)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlitFramebuffer.xml"]{glBlitFramebuffer manpage}.
}
@defproc[(glBlitFramebufferEXT (srcX0 Integer) (srcY0 Integer) (srcX1 Integer) (srcY1 Integer) (dstX0 Integer) (dstY0 Integer) (dstX1 Integer) (dstY1 Integer) (mask Natural) (filter Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_blit.txt"]{@racket[GL_EXT_framebuffer_blit]}.

Alias of @racket[glBlitFramebuffer].
}
@defproc[(glBufferAddressRangeNV (pname Integer) (index Natural) (address Natural) (length Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glBufferData (target Integer) (size Integer) (data GLPointer) (usage Integer)) Void]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBufferData.xml"]{glBufferData manpage}.
}
@defproc[(glBufferDataARB (target Integer) (size Integer) (data GLPointer) (usage Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glBufferData].
}
@defproc[(glBufferParameteriAPPLE (target Integer) (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/flush_buffer_range.txt"]{@racket[GL_APPLE_flush_buffer_range]}.
}
@defproc[(glBufferSubData (target Integer) (offset Integer) (size Integer) (data GLPointer)) Void]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBufferSubData.xml"]{glBufferSubData manpage}.
}
@defproc[(glBufferSubDataARB (target Integer) (offset Integer) (size Integer) (data GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glBufferSubData].
}
