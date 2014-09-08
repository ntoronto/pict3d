#lang scribble/manual
@title{glE...}
@defproc[(glEdgeFlag (flag Boolean)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glEdgeFlag.xml"]{glEdgeFlag manpage}.
}
@defproc[(glEdgeFlagFormatNV (stride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glEdgeFlagPointer (stride Integer) (pointer GLPointer)) Void]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glEdgeFlagPointer.xml"]{glEdgeFlagPointer manpage}.
}
@defproc[(glEdgeFlagPointerEXT (stride Integer) (count Integer) (pointer (Vectorof Boolean))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.
}
@defproc[(glEdgeFlagPointerListIBM (stride Integer) (pointer (Vectorof (Vectorof Boolean))) (ptrstride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt"]{@racket[GL_IBM_vertex_array_lists]}.
}
@defproc[(glEdgeFlagv (flag Boolean)) Void]{
Deprecated in version 3.1.
}
@defproc[(glElementPointerAPPLE (type Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/element_array.txt"]{@racket[GL_APPLE_element_array]}.
}
@defproc[(glElementPointerATI (type Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/element_array.txt"]{@racket[GL_ATI_element_array]}.
}
@defproc[(glEnable (cap Integer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glEnable.xml"]{glEnable manpage}.
}
@defproc[(glEnableClientState (array Integer)) Void]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glEnableClientState.xml"]{glEnableClientState manpage}.
}
@defproc[(glEnableClientStateIndexedEXT (array Integer) (index Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glEnableIndexedEXT (target Integer) (index Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/draw_buffers2.txt"]{@racket[GL_EXT_draw_buffers2]}.

Alias of @racket[glEnablei].
}
@defproc[(glEnableVariantClientStateEXT (id Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glEnableVertexAttribAPPLE (index Natural) (pname Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_program_evaluators.txt"]{@racket[GL_APPLE_vertex_program_evaluators]}.
}
@defproc[(glEnableVertexAttribArray (index Natural)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glEnableVertexAttribArray.xml"]{glEnableVertexAttribArray manpage}.
}
@defproc[(glEnableVertexAttribArrayARB (index Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glEnableVertexAttribArray].
}
@defproc[(glEnablei (target Integer) (index Natural)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glEnable.xml"]{glEnable manpage}.
}
@defproc[(glEnd) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glEnd.xml"]{glEnd manpage}.
}
@defproc[(glEndConditionalRender) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glEndConditionalRender.xml"]{glEndConditionalRender manpage}.
}
@defproc[(glEndConditionalRenderNV) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/conditional_render.txt"]{@racket[GL_NV_conditional_render]}.

Alias of @racket[glEndConditionalRender].
}
@defproc[(glEndFragmentShaderATI) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glEndList) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glEndList.xml"]{glEndList manpage}.
}
@defproc[(glEndOcclusionQueryNV) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/occlusion_query.txt"]{@racket[GL_NV_occlusion_query]}.
}
@defproc[(glEndPerfMonitorAMD (monitor Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glEndQuery (target Integer)) Void]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glEndQuery.xml"]{glEndQuery manpage}.
}
@defproc[(glEndQueryARB (target Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/occlusion_query.txt"]{@racket[GL_ARB_occlusion_query]}.

Alias of @racket[glEndQuery].
}
@defproc[(glEndQueryIndexed (target Integer) (index Natural)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glEndQueryIndexed.xml"]{glEndQueryIndexed manpage}.
}
@defproc[(glEndTransformFeedback) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glEndTransformFeedback.xml"]{glEndTransformFeedback manpage}.
}
@defproc[(glEndTransformFeedbackEXT) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/transform_feedback.txt"]{@racket[GL_EXT_transform_feedback]}.

Alias of @racket[glEndTransformFeedback].
}
@defproc[(glEndTransformFeedbackNV) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.

Alias of @racket[glEndTransformFeedback].
}
@defproc[(glEndVertexShaderEXT) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glEndVideoCaptureNV (video_capture_slot Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glEvalCoord1d (u Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml"]{glEvalCoord manpage}.
}
@defproc[(glEvalCoord1dv (u Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml"]{glEvalCoord manpage}.
}
@defproc[(glEvalCoord1f (u Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml"]{glEvalCoord manpage}.
}
@defproc[(glEvalCoord1fv (u Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml"]{glEvalCoord manpage}.
}
@defproc[(glEvalCoord2d (u Real) (v Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml"]{glEvalCoord manpage}.
}
@defproc[(glEvalCoord2dv (u F64Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml"]{glEvalCoord manpage}.
}
@defproc[(glEvalCoord2f (u Flonum) (v Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml"]{glEvalCoord manpage}.
}
@defproc[(glEvalCoord2fv (u F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml"]{glEvalCoord manpage}.
}
@defproc[(glEvalMapsNV (target Integer) (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glEvalMesh1 (mode Integer) (i1 Integer) (i2 Integer)) Void]{
Deprecated in version 3.1.
}
@defproc[(glEvalMesh2 (mode Integer) (i1 Integer) (i2 Integer) (j1 Integer) (j2 Integer)) Void]{
Deprecated in version 3.1.
}
@defproc[(glEvalPoint1 (i Integer)) Void]{
Deprecated in version 3.1.
}
@defproc[(glEvalPoint2 (i Integer) (j Integer)) Void]{
Deprecated in version 3.1.
}
@defproc[(glExecuteProgramNV (target Integer) (id Natural) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glExtractComponentEXT (res Natural) (src Natural) (num Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
