#lang scribble/manual
@title{glP...}
@defproc[(glPNTrianglesfATI (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/pn_triangles.txt"]{@racket[GL_ATI_pn_triangles]}.
}
@defproc[(glPNTrianglesiATI (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/pn_triangles.txt"]{@racket[GL_ATI_pn_triangles]}.
}
@defproc[(glPassTexCoordATI (dst Natural) (coord Natural) (swizzle Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glPassThrough (token Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPassThrough.xml"]{glPassThrough manpage}.
}
@defproc[(glPatchParameterfv (pname Integer) (the-values F32Vector)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPatchParameter.xml"]{glPatchParameter manpage}.
}
@defproc[(glPatchParameteri (pname Integer) (value Integer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPatchParameter.xml"]{glPatchParameter manpage}.
}
@defproc[(glPathColorGenNV (color Integer) (genMode Integer) (colorFormat Integer) (coeffs F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathCommandsNV (path Natural) (numCommands Integer) (commands (U Bytes String)) (numCoords Integer) (coordType Integer) (coords GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathCoordsNV (path Natural) (numCoords Integer) (coordType Integer) (coords GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathCoverDepthFuncNV (func Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathDashArrayNV (path Natural) (dashCount Integer) (dashArray F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathFogGenNV (genMode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathGlyphRangeNV (firstPathName Natural) (fontTarget Integer) (fontName GLPointer) (fontStyle Natural) (firstGlyph Natural) (numGlyphs Integer) (handleMissingGlyphs Integer) (pathParameterTemplate Natural) (emScale Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathGlyphsNV (firstPathName Natural) (fontTarget Integer) (fontName GLPointer) (fontStyle Natural) (numGlyphs Integer) (type Integer) (charcodes GLPointer) (handleMissingGlyphs Integer) (pathParameterTemplate Natural) (emScale Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathParameterfNV (path Natural) (pname Integer) (value Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathParameterfvNV (path Natural) (pname Integer) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathParameteriNV (path Natural) (pname Integer) (value Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathParameterivNV (path Natural) (pname Integer) (value S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathStencilDepthOffsetNV (factor Flonum) (units Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathStencilFuncNV (func Integer) (ref Integer) (mask Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathStringNV (path Natural) (format Integer) (length Integer) (pathString GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathSubCommandsNV (path Natural) (commandStart Integer) (commandsToDelete Integer) (numCommands Integer) (commands (U Bytes String)) (numCoords Integer) (coordType Integer) (coords GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathSubCoordsNV (path Natural) (coordStart Integer) (numCoords Integer) (coordType Integer) (coords GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathTexGenNV (texCoordSet Integer) (genMode Integer) (components Integer) (coeffs F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPauseTransformFeedback) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPauseTransformFeedback.xml"]{glPauseTransformFeedback manpage}.
}
@defproc[(glPauseTransformFeedbackNV) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback2.txt"]{@racket[GL_NV_transform_feedback2]}.
}
@defproc[(glPixelDataRangeNV (target Integer) (length Integer) (pointer GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/pixel_data_range.txt"]{@racket[GL_NV_pixel_data_range]}.
}
@defproc[(glPixelMapfv (map Integer) (mapsize Integer) (the-values F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPixelMap.xml"]{glPixelMap manpage}.
}
@defproc[(glPixelMapuiv (map Integer) (mapsize Integer) (the-values U32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPixelMap.xml"]{glPixelMap manpage}.
}
@defproc[(glPixelMapusv (map Integer) (mapsize Integer) (the-values U16Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPixelMap.xml"]{glPixelMap manpage}.
}
@defproc[(glPixelStoref (pname Integer) (param Flonum)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPixelStore.xml"]{glPixelStore manpage}.
}
@defproc[(glPixelStorei (pname Integer) (param Integer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPixelStore.xml"]{glPixelStore manpage}.
}
@defproc[(glPixelTexGenParameterfSGIS (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/pixel_texture.txt"]{@racket[GL_SGIS_pixel_texture]}.
}
@defproc[(glPixelTexGenParameterfvSGIS (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/pixel_texture.txt"]{@racket[GL_SGIS_pixel_texture]}.
}
@defproc[(glPixelTexGenParameteriSGIS (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/pixel_texture.txt"]{@racket[GL_SGIS_pixel_texture]}.
}
@defproc[(glPixelTexGenParameterivSGIS (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/pixel_texture.txt"]{@racket[GL_SGIS_pixel_texture]}.
}
@defproc[(glPixelTexGenSGIX (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/pixel_texture.txt"]{@racket[GL_SGIX_pixel_texture]}.
}
@defproc[(glPixelTransferf (pname Integer) (param Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPixelTransfer.xml"]{glPixelTransfer manpage}.
}
@defproc[(glPixelTransferi (pname Integer) (param Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPixelTransfer.xml"]{glPixelTransfer manpage}.
}
@defproc[(glPixelTransformParameterfEXT (target Integer) (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/pixel_transform.txt"]{@racket[GL_EXT_pixel_transform]}.
}
@defproc[(glPixelTransformParameterfvEXT (target Integer) (pname Integer) (params Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/pixel_transform.txt"]{@racket[GL_EXT_pixel_transform]}.
}
@defproc[(glPixelTransformParameteriEXT (target Integer) (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/pixel_transform.txt"]{@racket[GL_EXT_pixel_transform]}.
}
@defproc[(glPixelTransformParameterivEXT (target Integer) (pname Integer) (params Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/pixel_transform.txt"]{@racket[GL_EXT_pixel_transform]}.
}
@defproc[(glPixelZoom (xfactor Flonum) (yfactor Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPixelZoom.xml"]{glPixelZoom manpage}.
}
@defproc[(glPointAlongPathNV (path Natural) (startSegment Integer) (numSegments Integer) (distance Flonum)) (values Boolean Flonum Flonum Flonum Flonum)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPointParameterf (pname Integer) (param Flonum)) Void]{
Version 1.4.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPointParameter.xml"]{glPointParameter manpage}.
}
@defproc[(glPointParameterfARB (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/point_parameters.txt"]{@racket[GL_ARB_point_parameters]}.

Alias of @racket[glPointParameterf].
}
@defproc[(glPointParameterfEXT (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/point_parameters.txt"]{@racket[GL_EXT_point_parameters]}.

Alias of @racket[glPointParameterfARB].
}
@defproc[(glPointParameterfSGIS (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/point_parameters.txt"]{@racket[GL_SGIS_point_parameters]}.

Alias of @racket[glPointParameterfARB].
}
@defproc[(glPointParameterfv (pname Integer) (params F32Vector)) Void]{
Version 1.4.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPointParameter.xml"]{glPointParameter manpage}.
}
@defproc[(glPointParameterfvARB (pname Integer) (params Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/point_parameters.txt"]{@racket[GL_ARB_point_parameters]}.

Alias of @racket[glPointParameterfv].
}
@defproc[(glPointParameterfvEXT (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/point_parameters.txt"]{@racket[GL_EXT_point_parameters]}.

Alias of @racket[glPointParameterfvARB].
}
@defproc[(glPointParameterfvSGIS (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/point_parameters.txt"]{@racket[GL_SGIS_point_parameters]}.

Alias of @racket[glPointParameterfvARB].
}
@defproc[(glPointParameteri (pname Integer) (param Integer)) Void]{
Version 1.4.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPointParameter.xml"]{glPointParameter manpage}.
}
@defproc[(glPointParameteriNV (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/point_sprite.txt"]{@racket[GL_NV_point_sprite]}.

Alias of @racket[glPointParameteri].
}
@defproc[(glPointParameteriv (pname Integer) (params S32Vector)) Void]{
Version 1.4.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPointParameter.xml"]{glPointParameter manpage}.
}
@defproc[(glPointParameterivNV (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/point_sprite.txt"]{@racket[GL_NV_point_sprite]}.

Alias of @racket[glPointParameteriv].
}
@defproc[(glPointSize (size Flonum)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPointSize.xml"]{glPointSize manpage}.
}
@defproc[(glPollAsyncSGIX) (values Integer Natural)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/async.txt"]{@racket[GL_SGIX_async]}.
}
@defproc[(glPollInstrumentsSGIX) (values Integer Integer)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/instruments.txt"]{@racket[GL_SGIX_instruments]}.
}
@defproc[(glPolygonMode (face Integer) (mode Integer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPolygonMode.xml"]{glPolygonMode manpage}.
}
@defproc[(glPolygonOffset (factor Flonum) (units Flonum)) Void]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPolygonOffset.xml"]{glPolygonOffset manpage}.
}
@defproc[(glPolygonOffsetEXT (factor Flonum) (bias Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/polygon_offset.txt"]{@racket[GL_EXT_polygon_offset]}.
}
@defproc[(glPolygonStipple (mask (U Bytes String))) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPolygonStipple.xml"]{glPolygonStipple manpage}.
}
@defproc[(glPopAttrib) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPopAttrib.xml"]{glPopAttrib manpage}.
}
@defproc[(glPopClientAttrib) Void]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPopClientAttrib.xml"]{glPopClientAttrib manpage}.
}
@defproc[(glPopDebugGroup) Void]{
Version 4.3.
}
@defproc[(glPopMatrix) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPopMatrix.xml"]{glPopMatrix manpage}.
}
@defproc[(glPopName) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPopName.xml"]{glPopName manpage}.
}
@defproc[(glPresentFrameDualFillNV (video_slot Natural) (minPresentTime Natural) (beginPresentTimeId Natural) (presentDurationId Natural) (type Integer) (target0 Integer) (fill0 Natural) (target1 Integer) (fill1 Natural) (target2 Integer) (fill2 Natural) (target3 Integer) (fill3 Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/present_video.txt"]{@racket[GL_NV_present_video]}.
}
@defproc[(glPresentFrameKeyedNV (video_slot Natural) (minPresentTime Natural) (beginPresentTimeId Natural) (presentDurationId Natural) (type Integer) (target0 Integer) (fill0 Natural) (key0 Natural) (target1 Integer) (fill1 Natural) (key1 Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/present_video.txt"]{@racket[GL_NV_present_video]}.
}
@defproc[(glPrimitiveRestartIndex (index Natural)) Void]{
Version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPrimitiveRestartIndex.xml"]{glPrimitiveRestartIndex manpage}.
}
@defproc[(glPrimitiveRestartIndexNV (index Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/primitive_restart.txt"]{@racket[GL_NV_primitive_restart]}.
}
@defproc[(glPrimitiveRestartNV) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/primitive_restart.txt"]{@racket[GL_NV_primitive_restart]}.
}
@defproc[(glPrioritizeTextures (n Integer) (textures U32Vector) (priorities F32Vector)) Void]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPrioritizeTextures.xml"]{glPrioritizeTextures manpage}.
}
@defproc[(glPrioritizeTexturesEXT (n Integer) (textures U32Vector) (priorities F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_object.txt"]{@racket[GL_EXT_texture_object]}.

Alias of @racket[glPrioritizeTextures].
}
@defproc[(glProgramBinary (program Natural) (binaryFormat Integer) (binary GLPointer) (length Integer)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramBinary.xml"]{glProgramBinary manpage}.
}
@defproc[(glProgramBufferParametersIivNV (target Integer) (buffer Natural) (index Natural) (count Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/parameter_buffer_object.txt"]{@racket[GL_NV_parameter_buffer_object]}.
}
@defproc[(glProgramBufferParametersIuivNV (target Integer) (buffer Natural) (index Natural) (count Integer) (params U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/parameter_buffer_object.txt"]{@racket[GL_NV_parameter_buffer_object]}.
}
@defproc[(glProgramBufferParametersfvNV (target Integer) (buffer Natural) (index Natural) (count Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/parameter_buffer_object.txt"]{@racket[GL_NV_parameter_buffer_object]}.
}
@defproc[(glProgramEnvParameter4dARB (target Integer) (index Natural) (x Real) (y Real) (z Real) (w Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramEnvParameter4dvARB (target Integer) (index Natural) (params F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramEnvParameter4fARB (target Integer) (index Natural) (x Flonum) (y Flonum) (z Flonum) (w Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramEnvParameter4fvARB (target Integer) (index Natural) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramEnvParameterI4iNV (target Integer) (index Natural) (x Integer) (y Integer) (z Integer) (w Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramEnvParameterI4ivNV (target Integer) (index Natural) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramEnvParameterI4uiNV (target Integer) (index Natural) (x Natural) (y Natural) (z Natural) (w Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramEnvParameterI4uivNV (target Integer) (index Natural) (params U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramEnvParameters4fvEXT (target Integer) (index Natural) (count Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_program_parameters.txt"]{@racket[GL_EXT_gpu_program_parameters]}.
}
@defproc[(glProgramEnvParametersI4ivNV (target Integer) (index Natural) (count Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramEnvParametersI4uivNV (target Integer) (index Natural) (count Integer) (params U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramLocalParameter4dARB (target Integer) (index Natural) (x Real) (y Real) (z Real) (w Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramLocalParameter4dvARB (target Integer) (index Natural) (params F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramLocalParameter4fARB (target Integer) (index Natural) (x Flonum) (y Flonum) (z Flonum) (w Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramLocalParameter4fvARB (target Integer) (index Natural) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramLocalParameterI4iNV (target Integer) (index Natural) (x Integer) (y Integer) (z Integer) (w Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramLocalParameterI4ivNV (target Integer) (index Natural) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramLocalParameterI4uiNV (target Integer) (index Natural) (x Natural) (y Natural) (z Natural) (w Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramLocalParameterI4uivNV (target Integer) (index Natural) (params U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramLocalParameters4fvEXT (target Integer) (index Natural) (count Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_program_parameters.txt"]{@racket[GL_EXT_gpu_program_parameters]}.
}
@defproc[(glProgramLocalParametersI4ivNV (target Integer) (index Natural) (count Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramLocalParametersI4uivNV (target Integer) (index Natural) (count Integer) (params U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramNamedParameter4dNV (id Natural) (len Integer) (name (U Bytes String)) (x Real) (y Real) (z Real) (w Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fragment_program.txt"]{@racket[GL_NV_fragment_program]}.
}
@defproc[(glProgramNamedParameter4dvNV (id Natural) (len Integer) (name (U Bytes String)) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fragment_program.txt"]{@racket[GL_NV_fragment_program]}.
}
@defproc[(glProgramNamedParameter4fNV (id Natural) (len Integer) (name (U Bytes String)) (x Flonum) (y Flonum) (z Flonum) (w Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fragment_program.txt"]{@racket[GL_NV_fragment_program]}.
}
@defproc[(glProgramNamedParameter4fvNV (id Natural) (len Integer) (name (U Bytes String)) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fragment_program.txt"]{@racket[GL_NV_fragment_program]}.
}
@defproc[(glProgramParameter4dNV (target Integer) (index Natural) (x Real) (y Real) (z Real) (w Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glProgramParameter4dvNV (target Integer) (index Natural) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glProgramParameter4fNV (target Integer) (index Natural) (x Flonum) (y Flonum) (z Flonum) (w Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glProgramParameter4fvNV (target Integer) (index Natural) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glProgramParameteri (program Natural) (pname Integer) (value Integer)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramParameter.xml"]{glProgramParameter manpage}.
}
@defproc[(glProgramParameteriARB (program Natural) (pname Integer) (value Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/geometry_shader4.txt"]{@racket[GL_ARB_geometry_shader4]}.

Alias of @racket[glProgramParameteri].
}
@defproc[(glProgramParameteriEXT (program Natural) (pname Integer) (value Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/geometry_shader4.txt"]{@racket[GL_EXT_geometry_shader4]}.

Alias of @racket[glProgramParameteriARB].
}
@defproc[(glProgramParameters4dvNV (target Integer) (index Natural) (count Integer) (v F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glProgramParameters4fvNV (target Integer) (index Natural) (count Integer) (v F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glProgramStringARB (target Integer) (format Integer) (len Integer) (string GLPointer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramSubroutineParametersuivNV (target Integer) (count Integer) (params U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program5.txt"]{@racket[GL_NV_gpu_program5]}.
}
@defproc[(glProgramUniform1d (program Natural) (location Integer) (v0 Real)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1dEXT (program Natural) (location Integer) (x Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform1dv (program Natural) (location Integer) (count Integer) (value F64Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1dvEXT (program Natural) (location Integer) (count Integer) (value F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform1f (program Natural) (location Integer) (v0 Flonum)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1fEXT (program Natural) (location Integer) (v0 Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform1fv (program Natural) (location Integer) (count Integer) (value F32Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1fvEXT (program Natural) (location Integer) (count Integer) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform1i (program Natural) (location Integer) (v0 Integer)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1i64NV (program Natural) (location Integer) (x Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform1i64vNV (program Natural) (location Integer) (count Integer) (value S64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform1iEXT (program Natural) (location Integer) (v0 Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform1iv (program Natural) (location Integer) (count Integer) (value S32Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1ivEXT (program Natural) (location Integer) (count Integer) (value S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform1ui (program Natural) (location Integer) (v0 Natural)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1ui64NV (program Natural) (location Integer) (x Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform1ui64vNV (program Natural) (location Integer) (count Integer) (value U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform1uiEXT (program Natural) (location Integer) (v0 Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform1uiv (program Natural) (location Integer) (count Integer) (value U32Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1uivEXT (program Natural) (location Integer) (count Integer) (value U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2d (program Natural) (location Integer) (v0 Real) (v1 Real)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2dEXT (program Natural) (location Integer) (x Real) (y Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2dv (program Natural) (location Integer) (count Integer) (value F64Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2dvEXT (program Natural) (location Integer) (count Integer) (value F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2f (program Natural) (location Integer) (v0 Flonum) (v1 Flonum)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2fEXT (program Natural) (location Integer) (v0 Flonum) (v1 Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2fv (program Natural) (location Integer) (count Integer) (value F32Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2fvEXT (program Natural) (location Integer) (count Integer) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2i (program Natural) (location Integer) (v0 Integer) (v1 Integer)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2i64NV (program Natural) (location Integer) (x Integer) (y Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform2i64vNV (program Natural) (location Integer) (count Integer) (value S64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform2iEXT (program Natural) (location Integer) (v0 Integer) (v1 Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2iv (program Natural) (location Integer) (count Integer) (value S32Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2ivEXT (program Natural) (location Integer) (count Integer) (value S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2ui (program Natural) (location Integer) (v0 Natural) (v1 Natural)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2ui64NV (program Natural) (location Integer) (x Natural) (y Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform2ui64vNV (program Natural) (location Integer) (count Integer) (value U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform2uiEXT (program Natural) (location Integer) (v0 Natural) (v1 Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2uiv (program Natural) (location Integer) (count Integer) (value U32Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2uivEXT (program Natural) (location Integer) (count Integer) (value U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3d (program Natural) (location Integer) (v0 Real) (v1 Real) (v2 Real)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3dEXT (program Natural) (location Integer) (x Real) (y Real) (z Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3dv (program Natural) (location Integer) (count Integer) (value F64Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3dvEXT (program Natural) (location Integer) (count Integer) (value F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3f (program Natural) (location Integer) (v0 Flonum) (v1 Flonum) (v2 Flonum)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3fEXT (program Natural) (location Integer) (v0 Flonum) (v1 Flonum) (v2 Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3fv (program Natural) (location Integer) (count Integer) (value F32Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3fvEXT (program Natural) (location Integer) (count Integer) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3i (program Natural) (location Integer) (v0 Integer) (v1 Integer) (v2 Integer)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3i64NV (program Natural) (location Integer) (x Integer) (y Integer) (z Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform3i64vNV (program Natural) (location Integer) (count Integer) (value S64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform3iEXT (program Natural) (location Integer) (v0 Integer) (v1 Integer) (v2 Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3iv (program Natural) (location Integer) (count Integer) (value S32Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3ivEXT (program Natural) (location Integer) (count Integer) (value S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3ui (program Natural) (location Integer) (v0 Natural) (v1 Natural) (v2 Natural)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3ui64NV (program Natural) (location Integer) (x Natural) (y Natural) (z Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform3ui64vNV (program Natural) (location Integer) (count Integer) (value U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform3uiEXT (program Natural) (location Integer) (v0 Natural) (v1 Natural) (v2 Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3uiv (program Natural) (location Integer) (count Integer) (value U32Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3uivEXT (program Natural) (location Integer) (count Integer) (value U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4d (program Natural) (location Integer) (v0 Real) (v1 Real) (v2 Real) (v3 Real)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4dEXT (program Natural) (location Integer) (x Real) (y Real) (z Real) (w Real)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4dv (program Natural) (location Integer) (count Integer) (value F64Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4dvEXT (program Natural) (location Integer) (count Integer) (value F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4f (program Natural) (location Integer) (v0 Flonum) (v1 Flonum) (v2 Flonum) (v3 Flonum)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4fEXT (program Natural) (location Integer) (v0 Flonum) (v1 Flonum) (v2 Flonum) (v3 Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4fv (program Natural) (location Integer) (count Integer) (value F32Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4fvEXT (program Natural) (location Integer) (count Integer) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4i (program Natural) (location Integer) (v0 Integer) (v1 Integer) (v2 Integer) (v3 Integer)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4i64NV (program Natural) (location Integer) (x Integer) (y Integer) (z Integer) (w Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform4i64vNV (program Natural) (location Integer) (count Integer) (value S64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform4iEXT (program Natural) (location Integer) (v0 Integer) (v1 Integer) (v2 Integer) (v3 Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4iv (program Natural) (location Integer) (count Integer) (value S32Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4ivEXT (program Natural) (location Integer) (count Integer) (value S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4ui (program Natural) (location Integer) (v0 Natural) (v1 Natural) (v2 Natural) (v3 Natural)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4ui64NV (program Natural) (location Integer) (x Natural) (y Natural) (z Natural) (w Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform4ui64vNV (program Natural) (location Integer) (count Integer) (value U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform4uiEXT (program Natural) (location Integer) (v0 Natural) (v1 Natural) (v2 Natural) (v3 Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4uiv (program Natural) (location Integer) (count Integer) (value U32Vector)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4uivEXT (program Natural) (location Integer) (count Integer) (value U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformHandleui64NV (program Natural) (location Integer) (value Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glProgramUniformHandleui64vNV (program Natural) (location Integer) (count Integer) (the-values U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glProgramUniformMatrix2dv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix2dvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix2fv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix2fvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix2x3dv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix2x3dvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix2x3fv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix2x3fvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix2x4dv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix2x4dvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix2x4fv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix2x4fvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix3dv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix3dvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix3fv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix3fvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix3x2dv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix3x2dvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix3x2fv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix3x2fvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix3x4dv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix3x4dvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix3x4fv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix3x4fvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix4dv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix4dvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix4fv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix4fvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix4x2dv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix4x2dvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix4x2fv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix4x2fvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix4x3dv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix4x3dvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix4x3fv (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix4x3fvEXT (program Natural) (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformui64NV (program Natural) (location Integer) (value Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glProgramUniformui64vNV (program Natural) (location Integer) (count Integer) (value U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glProgramVertexLimitNV (target Integer) (limit Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/geometry_program4.txt"]{@racket[GL_NV_geometry_program4]}.
}
@defproc[(glProvokingVertex (mode Integer)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProvokingVertex.xml"]{glProvokingVertex manpage}.
}
@defproc[(glProvokingVertexEXT (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/provoking_vertex.txt"]{@racket[GL_EXT_provoking_vertex]}.
}
@defproc[(glPushAttrib (mask Natural)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPushAttrib.xml"]{glPushAttrib manpage}.
}
@defproc[(glPushClientAttrib (mask Natural)) Void]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPushClientAttrib.xml"]{glPushClientAttrib manpage}.
}
@defproc[(glPushClientAttribDefaultEXT (mask Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glPushDebugGroup (source Integer) (id Natural) (length Integer) (message (U Bytes String))) Void]{
Version 4.3.
}
@defproc[(glPushMatrix) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPushMatrix.xml"]{glPushMatrix manpage}.
}
@defproc[(glPushName (name Natural)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPushName.xml"]{glPushName manpage}.
}
