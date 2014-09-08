#lang scribble/manual
@title{glI...}
@defproc[(glIglooInterfaceSGIX (pname Integer) (params GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/igloo_interface.txt"]{@racket[GL_SGIX_igloo_interface]}.
}
@defproc[(glImageTransformParameterfHP (target Integer) (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/HP/image_transform.txt"]{@racket[GL_HP_image_transform]}.
}
@defproc[(glImageTransformParameterfvHP (target Integer) (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/HP/image_transform.txt"]{@racket[GL_HP_image_transform]}.
}
@defproc[(glImageTransformParameteriHP (target Integer) (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/HP/image_transform.txt"]{@racket[GL_HP_image_transform]}.
}
@defproc[(glImageTransformParameterivHP (target Integer) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/HP/image_transform.txt"]{@racket[GL_HP_image_transform]}.
}
@defproc[(glImportSyncEXT (external_sync_type Integer) (external_sync Integer) (flags Natural)) GLSync]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/x11_sync_object.txt"]{@racket[GL_EXT_x11_sync_object]}.
}
@defproc[(glIndexFormatNV (type Integer) (stride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glIndexFuncEXT (func Integer) (ref Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/index_func.txt"]{@racket[GL_EXT_index_func]}.
}
@defproc[(glIndexMask (mask Natural)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndexMask.xml"]{glIndexMask manpage}.
}
@defproc[(glIndexMaterialEXT (face Integer) (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/index_material.txt"]{@racket[GL_EXT_index_material]}.
}
@defproc[(glIndexPointer (type Integer) (stride Integer) (pointer GLPointer)) Void]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndexPointer.xml"]{glIndexPointer manpage}.
}
@defproc[(glIndexPointerEXT (type Integer) (stride Integer) (count Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.
}
@defproc[(glIndexPointerListIBM (type Integer) (stride Integer) (pointer (Vectorof GLPointer)) (ptrstride Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt"]{@racket[GL_IBM_vertex_array_lists]}.
}
@defproc[(glIndexd (c Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexdv (c Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexf (c Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexfv (c Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexi (c Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexiv (c Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexs (c Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexsv (c Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexub (c Byte)) Void]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexubv (c Byte)) Void]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glInitNames) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glInitNames.xml"]{glInitNames manpage}.
}
@defproc[(glInsertComponentEXT (res Natural) (src Natural) (num Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glInstrumentsBufferSGIX (size Integer)) S32Vector]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/instruments.txt"]{@racket[GL_SGIX_instruments]}.
}
@defproc[(glInterleavedArrays (format Integer) (stride Integer) (pointer GLPointer)) Void]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glInterleavedArrays.xml"]{glInterleavedArrays manpage}.
}
@defproc[(glInterpolatePathsNV (resultPath Natural) (pathA Natural) (pathB Natural) (weight Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glInvalidateBufferData (buffer Natural)) Void]{
Version 4.3.
}
@defproc[(glInvalidateBufferSubData (buffer Natural) (offset Integer) (length Integer)) Void]{
Version 4.3.
}
@defproc[(glInvalidateFramebuffer (target Integer) (numAttachments Integer) (attachments S32Vector)) Void]{
Version 4.3.
}
@defproc[(glInvalidateSubFramebuffer (target Integer) (numAttachments Integer) (attachments S32Vector) (x Integer) (y Integer) (width Integer) (height Integer)) Void]{
Version 4.3.
}
@defproc[(glInvalidateTexImage (texture Natural) (level Integer)) Void]{
Version 4.3.
}
@defproc[(glInvalidateTexSubImage (texture Natural) (level Integer) (xoffset Integer) (yoffset Integer) (zoffset Integer) (width Integer) (height Integer) (depth Integer)) Void]{
Version 4.3.
}
@defproc[(glIsAsyncMarkerSGIX (marker Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/async.txt"]{@racket[GL_SGIX_async]}.
}
@defproc[(glIsBuffer (buffer Natural)) Boolean]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsBuffer.xml"]{glIsBuffer manpage}.
}
@defproc[(glIsBufferARB (buffer Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glIsBuffer].
}
@defproc[(glIsBufferResidentNV (target Integer)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glIsEnabled (cap Integer)) Boolean]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsEnabled.xml"]{glIsEnabled manpage}.
}
@defproc[(glIsEnabledIndexedEXT (target Integer) (index Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/draw_buffers2.txt"]{@racket[GL_EXT_draw_buffers2]}.

Alias of @racket[glIsEnabledi].
}
@defproc[(glIsEnabledi (target Integer) (index Natural)) Boolean]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsEnabled.xml"]{glIsEnabled manpage}.
}
@defproc[(glIsFenceAPPLE (fence Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/fence.txt"]{@racket[GL_APPLE_fence]}.
}
@defproc[(glIsFenceNV (fence Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fence.txt"]{@racket[GL_NV_fence]}.
}
@defproc[(glIsFramebuffer (framebuffer Natural)) Boolean]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsFramebuffer.xml"]{glIsFramebuffer manpage}.
}
@defproc[(glIsFramebufferEXT (framebuffer Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glIsFramebuffer].
}
@defproc[(glIsImageHandleResidentNV (handle Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glIsList (list Natural)) Boolean]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIsList.xml"]{glIsList manpage}.
}
@defproc[(glIsNameAMD (identifier Integer) (name Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/name_gen_delete.txt"]{@racket[GL_AMD_name_gen_delete]}.
}
@defproc[(glIsNamedBufferResidentNV (buffer Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glIsNamedStringARB (namelen Integer) (name (U Bytes String))) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shading_language_include.txt"]{@racket[GL_ARB_shading_language_include]}.
}
@defproc[(glIsObjectBufferATI (buffer Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glIsOcclusionQueryNV (id Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/occlusion_query.txt"]{@racket[GL_NV_occlusion_query]}.
}
@defproc[(glIsPathNV (path Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glIsPointInFillPathNV (path Natural) (mask Natural) (x Flonum) (y Flonum)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glIsPointInStrokePathNV (path Natural) (x Flonum) (y Flonum)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glIsProgram (program Natural)) Boolean]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsProgram.xml"]{glIsProgram manpage}.
}
@defproc[(glIsProgramARB (program Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glIsProgram].
}
@defproc[(glIsProgramNV (id Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glIsProgram].
}
@defproc[(glIsProgramPipeline (pipeline Natural)) Boolean]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsProgramPipeline.xml"]{glIsProgramPipeline manpage}.
}
@defproc[(glIsQuery (id Natural)) Boolean]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsQuery.xml"]{glIsQuery manpage}.
}
@defproc[(glIsQueryARB (id Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/occlusion_query.txt"]{@racket[GL_ARB_occlusion_query]}.

Alias of @racket[glIsQuery].
}
@defproc[(glIsRenderbuffer (renderbuffer Natural)) Boolean]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsRenderbuffer.xml"]{glIsRenderbuffer manpage}.
}
@defproc[(glIsRenderbufferEXT (renderbuffer Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glIsRenderbuffer].
}
@defproc[(glIsSampler (sampler Natural)) Boolean]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsSampler.xml"]{glIsSampler manpage}.
}
@defproc[(glIsShader (shader Natural)) Boolean]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsShader.xml"]{glIsShader manpage}.
}
@defproc[(glIsSync (sync GLSync)) Boolean]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsSync.xml"]{glIsSync manpage}.
}
@defproc[(glIsTexture (texture Natural)) Boolean]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsTexture.xml"]{glIsTexture manpage}.
}
@defproc[(glIsTextureEXT (texture Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_object.txt"]{@racket[GL_EXT_texture_object]}.
}
@defproc[(glIsTextureHandleResidentNV (handle Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glIsTransformFeedback (id Natural)) Boolean]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsTransformFeedback.xml"]{glIsTransformFeedback manpage}.
}
@defproc[(glIsTransformFeedbackNV (id Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback2.txt"]{@racket[GL_NV_transform_feedback2]}.
}
@defproc[(glIsVariantEnabledEXT (id Natural) (cap Integer)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glIsVertexArray (array Natural)) Boolean]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsVertexArray.xml"]{glIsVertexArray manpage}.
}
@defproc[(glIsVertexArrayAPPLE (array Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_array_object.txt"]{@racket[GL_APPLE_vertex_array_object]}.

Alias of @racket[glIsVertexArray].
}
@defproc[(glIsVertexAttribEnabledAPPLE (index Natural) (pname Integer)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_program_evaluators.txt"]{@racket[GL_APPLE_vertex_program_evaluators]}.
}
