#lang scribble/manual
@title{glF...}
@defproc[(glFeedbackBuffer (size Integer) (type Integer)) F32Vector]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFeedbackBuffer.xml"]{glFeedbackBuffer manpage}.
}
@defproc[(glFenceSync (condition Integer) (flags Natural)) GLSync]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFenceSync.xml"]{glFenceSync manpage}.
}
@defproc[(glFinalCombinerInputNV (variable Integer) (input Integer) (mapping Integer) (componentUsage Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glFinish) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFinish.xml"]{glFinish manpage}.
}
@defproc[(glFinishAsyncSGIX) (values Integer Natural)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/async.txt"]{@racket[GL_SGIX_async]}.
}
@defproc[(glFinishFenceAPPLE (fence Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/fence.txt"]{@racket[GL_APPLE_fence]}.
}
@defproc[(glFinishFenceNV (fence Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fence.txt"]{@racket[GL_NV_fence]}.
}
@defproc[(glFinishObjectAPPLE (object Integer) (name Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/fence.txt"]{@racket[GL_APPLE_fence]}.
}
@defproc[(glFinishTextureSUNX) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUNX/constant_data.txt"]{@racket[GL_SUNX_constant_data]}.
}
@defproc[(glFlush) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFlush.xml"]{glFlush manpage}.
}
@defproc[(glFlushMappedBufferRange (target Integer) (offset Integer) (length Integer)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFlushMappedBufferRange.xml"]{glFlushMappedBufferRange manpage}.
}
@defproc[(glFlushMappedBufferRangeAPPLE (target Integer) (offset Integer) (size Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/flush_buffer_range.txt"]{@racket[GL_APPLE_flush_buffer_range]}.

Alias of @racket[glFlushMappedBufferRange].
}
@defproc[(glFlushMappedNamedBufferRangeEXT (buffer Natural) (offset Integer) (length Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glFlushPixelDataRangeNV (target Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/pixel_data_range.txt"]{@racket[GL_NV_pixel_data_range]}.
}
@defproc[(glFlushRasterSGIX) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/flush_raster.txt"]{@racket[GL_SGIX_flush_raster]}.
}
@defproc[(glFlushVertexArrayRangeAPPLE (length Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_array_range.txt"]{@racket[GL_APPLE_vertex_array_range]}.
}
@defproc[(glFlushVertexArrayRangeNV) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_array_range.txt"]{@racket[GL_NV_vertex_array_range]}.
}
@defproc[(glFogCoordFormatNV (type Integer) (stride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glFogCoordPointer (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFogCoordPointer.xml"]{glFogCoordPointer manpage}.
}
@defproc[(glFogCoordPointerEXT (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/fog_coord.txt"]{@racket[GL_EXT_fog_coord]}.

Alias of @racket[glFogCoordPointer].
}
@defproc[(glFogCoordPointerListIBM (type Integer) (stride Integer) (pointer (Vectorof GLPointer)) (ptrstride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt"]{@racket[GL_IBM_vertex_array_lists]}.
}
@defproc[(glFogCoordd (coord Real)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml"]{glFogCoord manpage}.
}
@defproc[(glFogCoorddEXT (coord Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/fog_coord.txt"]{@racket[GL_EXT_fog_coord]}.

Alias of @racket[glFogCoordd].
}
@defproc[(glFogCoorddv (coord Real)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml"]{glFogCoord manpage}.
}
@defproc[(glFogCoorddvEXT (coord Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/fog_coord.txt"]{@racket[GL_EXT_fog_coord]}.

Alias of @racket[glFogCoorddv].
}
@defproc[(glFogCoordf (coord Flonum)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml"]{glFogCoord manpage}.
}
@defproc[(glFogCoordfEXT (coord Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/fog_coord.txt"]{@racket[GL_EXT_fog_coord]}.

Alias of @racket[glFogCoordf].
}
@defproc[(glFogCoordfv (coord Flonum)) Void]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml"]{glFogCoord manpage}.
}
@defproc[(glFogCoordfvEXT (coord Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/fog_coord.txt"]{@racket[GL_EXT_fog_coord]}.

Alias of @racket[glFogCoordfv].
}
@defproc[(glFogCoordhNV (fog Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glFogCoordhvNV (fog Nonnegative-Fixnum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glFogFuncSGIS (n Integer) (points F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/fog_function.txt"]{@racket[GL_SGIS_fog_function]}.
}
@defproc[(glFogf (pname Integer) (param Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml"]{glFog manpage}.
}
@defproc[(glFogfv (pname Integer) (params F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml"]{glFog manpage}.
}
@defproc[(glFogi (pname Integer) (param Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml"]{glFog manpage}.
}
@defproc[(glFogiv (pname Integer) (params S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml"]{glFog manpage}.
}
@defproc[(glFragmentColorMaterialSGIX (face Integer) (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightModelfSGIX (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightModelfvSGIX (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightModeliSGIX (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightModelivSGIX (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightfSGIX (light Integer) (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightfvSGIX (light Integer) (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightiSGIX (light Integer) (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightivSGIX (light Integer) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentMaterialfSGIX (face Integer) (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentMaterialfvSGIX (face Integer) (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentMaterialiSGIX (face Integer) (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentMaterialivSGIX (face Integer) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFrameTerminatorGREMEDY) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/GREMEDY/frame_terminator.txt"]{@racket[GL_GREMEDY_frame_terminator]}.
}
@defproc[(glFrameZoomSGIX (factor Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/framezoom.txt"]{@racket[GL_SGIX_framezoom]}.
}
@defproc[(glFramebufferDrawBufferEXT (framebuffer Natural) (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glFramebufferDrawBuffersEXT (framebuffer Natural) (n Integer) (bufs (Vectorof Integer))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glFramebufferParameteri (target Integer) (pname Integer) (param Integer)) Void]{
Version 4.3.
}
@defproc[(glFramebufferReadBufferEXT (framebuffer Natural) (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glFramebufferRenderbuffer (target Integer) (attachment Integer) (renderbuffertarget Integer) (renderbuffer Natural)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFramebufferRenderbuffer.xml"]{glFramebufferRenderbuffer manpage}.
}
@defproc[(glFramebufferRenderbufferEXT (target Integer) (attachment Integer) (renderbuffertarget Integer) (renderbuffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glFramebufferRenderbuffer].
}
@defproc[(glFramebufferTexture (target Integer) (attachment Integer) (texture Natural) (level Integer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFramebufferTexture.xml"]{glFramebufferTexture manpage}.
}
@defproc[(glFramebufferTexture1D (target Integer) (attachment Integer) (textarget Integer) (texture Natural) (level Integer)) Void]{
Version 3.0.
}
@defproc[(glFramebufferTexture1DEXT (target Integer) (attachment Integer) (textarget Integer) (texture Natural) (level Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glFramebufferTexture1D].
}
@defproc[(glFramebufferTexture2D (target Integer) (attachment Integer) (textarget Integer) (texture Natural) (level Integer)) Void]{
Version 3.0.
}
@defproc[(glFramebufferTexture2DEXT (target Integer) (attachment Integer) (textarget Integer) (texture Natural) (level Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glFramebufferTexture2D].
}
@defproc[(glFramebufferTexture3D (target Integer) (attachment Integer) (textarget Integer) (texture Natural) (level Integer) (zoffset Integer)) Void]{
Version 3.0.
}
@defproc[(glFramebufferTexture3DEXT (target Integer) (attachment Integer) (textarget Integer) (texture Natural) (level Integer) (zoffset Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glFramebufferTexture3D].
}
@defproc[(glFramebufferTextureARB (target Integer) (attachment Integer) (texture Natural) (level Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/geometry_shader4.txt"]{@racket[GL_ARB_geometry_shader4]}.
}
@defproc[(glFramebufferTextureEXT (target Integer) (attachment Integer) (texture Natural) (level Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/geometry_program4.txt"]{@racket[GL_NV_geometry_program4]}.

Alias of @racket[glFramebufferTextureARB].
}
@defproc[(glFramebufferTextureFaceARB (target Integer) (attachment Integer) (texture Natural) (level Integer) (face Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/geometry_shader4.txt"]{@racket[GL_ARB_geometry_shader4]}.
}
@defproc[(glFramebufferTextureFaceEXT (target Integer) (attachment Integer) (texture Natural) (level Integer) (face Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/geometry_program4.txt"]{@racket[GL_NV_geometry_program4]}.

Alias of @racket[glFramebufferTextureFaceARB].
}
@defproc[(glFramebufferTextureLayer (target Integer) (attachment Integer) (texture Natural) (level Integer) (layer Integer)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFramebufferTextureLayer.xml"]{glFramebufferTextureLayer manpage}.
}
@defproc[(glFramebufferTextureLayerARB (target Integer) (attachment Integer) (texture Natural) (level Integer) (layer Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/geometry_shader4.txt"]{@racket[GL_ARB_geometry_shader4]}.

Alias of @racket[glFramebufferTextureLayer].
}
@defproc[(glFramebufferTextureLayerEXT (target Integer) (attachment Integer) (texture Natural) (level Integer) (layer Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/geometry_program4.txt"]{@racket[GL_NV_geometry_program4]}.

Alias of @racket[glFramebufferTextureLayer].
}
@defproc[(glFreeObjectBufferATI (buffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glFrontFace (mode Integer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFrontFace.xml"]{glFrontFace manpage}.
}
@defproc[(glFrustum (left Real) (right Real) (bottom Real) (top Real) (zNear Real) (zFar Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFrustum.xml"]{glFrustum manpage}.
}
