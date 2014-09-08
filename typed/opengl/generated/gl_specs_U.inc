#lang scribble/manual
@title{glU...}
@defproc[(glUniform1d (location Integer) (x Real)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1dv (location Integer) (count Integer) (value F64Vector)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1f (location Integer) (v0 Flonum)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1fARB (location Integer) (v0 Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform1f].
}
@defproc[(glUniform1fv (location Integer) (count Integer) (value F32Vector)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1fvARB (location Integer) (count Integer) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform1fv].
}
@defproc[(glUniform1i (location Integer) (v0 Integer)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1i64NV (location Integer) (x Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform1i64vNV (location Integer) (count Integer) (value S64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform1iARB (location Integer) (v0 Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform1i].
}
@defproc[(glUniform1iv (location Integer) (count Integer) (value S32Vector)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1ivARB (location Integer) (count Integer) (value S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform1iv].
}
@defproc[(glUniform1ui (location Integer) (v0 Natural)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1ui64NV (location Integer) (x Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform1ui64vNV (location Integer) (count Integer) (value U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform1uiEXT (location Integer) (v0 Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform1ui].
}
@defproc[(glUniform1uiv (location Integer) (count Integer) (value U32Vector)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1uivEXT (location Integer) (count Integer) (value U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform1uiv].
}
@defproc[(glUniform2d (location Integer) (x Real) (y Real)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2dv (location Integer) (count Integer) (value F64Vector)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2f (location Integer) (v0 Flonum) (v1 Flonum)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2fARB (location Integer) (v0 Flonum) (v1 Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform2f].
}
@defproc[(glUniform2fv (location Integer) (count Integer) (value F32Vector)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2fvARB (location Integer) (count Integer) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform2fv].
}
@defproc[(glUniform2i (location Integer) (v0 Integer) (v1 Integer)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2i64NV (location Integer) (x Integer) (y Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform2i64vNV (location Integer) (count Integer) (value S64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform2iARB (location Integer) (v0 Integer) (v1 Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform2i].
}
@defproc[(glUniform2iv (location Integer) (count Integer) (value S32Vector)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2ivARB (location Integer) (count Integer) (value S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform2iv].
}
@defproc[(glUniform2ui (location Integer) (v0 Natural) (v1 Natural)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2ui64NV (location Integer) (x Natural) (y Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform2ui64vNV (location Integer) (count Integer) (value U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform2uiEXT (location Integer) (v0 Natural) (v1 Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform2ui].
}
@defproc[(glUniform2uiv (location Integer) (count Integer) (value U32Vector)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2uivEXT (location Integer) (count Integer) (value U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform2uiv].
}
@defproc[(glUniform3d (location Integer) (x Real) (y Real) (z Real)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3dv (location Integer) (count Integer) (value F64Vector)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3f (location Integer) (v0 Flonum) (v1 Flonum) (v2 Flonum)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3fARB (location Integer) (v0 Flonum) (v1 Flonum) (v2 Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform3f].
}
@defproc[(glUniform3fv (location Integer) (count Integer) (value F32Vector)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3fvARB (location Integer) (count Integer) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform3fv].
}
@defproc[(glUniform3i (location Integer) (v0 Integer) (v1 Integer) (v2 Integer)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3i64NV (location Integer) (x Integer) (y Integer) (z Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform3i64vNV (location Integer) (count Integer) (value S64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform3iARB (location Integer) (v0 Integer) (v1 Integer) (v2 Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform3i].
}
@defproc[(glUniform3iv (location Integer) (count Integer) (value S32Vector)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3ivARB (location Integer) (count Integer) (value S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform3iv].
}
@defproc[(glUniform3ui (location Integer) (v0 Natural) (v1 Natural) (v2 Natural)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3ui64NV (location Integer) (x Natural) (y Natural) (z Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform3ui64vNV (location Integer) (count Integer) (value U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform3uiEXT (location Integer) (v0 Natural) (v1 Natural) (v2 Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform3ui].
}
@defproc[(glUniform3uiv (location Integer) (count Integer) (value U32Vector)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3uivEXT (location Integer) (count Integer) (value U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform3uiv].
}
@defproc[(glUniform4d (location Integer) (x Real) (y Real) (z Real) (w Real)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4dv (location Integer) (count Integer) (value F64Vector)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4f (location Integer) (v0 Flonum) (v1 Flonum) (v2 Flonum) (v3 Flonum)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4fARB (location Integer) (v0 Flonum) (v1 Flonum) (v2 Flonum) (v3 Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform4f].
}
@defproc[(glUniform4fv (location Integer) (count Integer) (value F32Vector)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4fvARB (location Integer) (count Integer) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform4fv].
}
@defproc[(glUniform4i (location Integer) (v0 Integer) (v1 Integer) (v2 Integer) (v3 Integer)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4i64NV (location Integer) (x Integer) (y Integer) (z Integer) (w Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform4i64vNV (location Integer) (count Integer) (value S64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform4iARB (location Integer) (v0 Integer) (v1 Integer) (v2 Integer) (v3 Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform4i].
}
@defproc[(glUniform4iv (location Integer) (count Integer) (value S32Vector)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4ivARB (location Integer) (count Integer) (value S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform4iv].
}
@defproc[(glUniform4ui (location Integer) (v0 Natural) (v1 Natural) (v2 Natural) (v3 Natural)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4ui64NV (location Integer) (x Natural) (y Natural) (z Natural) (w Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform4ui64vNV (location Integer) (count Integer) (value U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform4uiEXT (location Integer) (v0 Natural) (v1 Natural) (v2 Natural) (v3 Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform4ui].
}
@defproc[(glUniform4uiv (location Integer) (count Integer) (value U32Vector)) Void]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4uivEXT (location Integer) (count Integer) (value U32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform4uiv].
}
@defproc[(glUniformBlockBinding (program Natural) (uniformBlockIndex Natural) (uniformBlockBinding Natural)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniformBlockBinding.xml"]{glUniformBlockBinding manpage}.
}
@defproc[(glUniformBufferEXT (program Natural) (location Integer) (buffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/bindable_uniform.txt"]{@racket[GL_EXT_bindable_uniform]}.
}
@defproc[(glUniformHandleui64NV (location Integer) (value Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glUniformHandleui64vNV (location Integer) (count Integer) (value U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glUniformMatrix2dv (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 1.2.
}
@defproc[(glUniformMatrix2fv (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 2.0.
}
@defproc[(glUniformMatrix2fvARB (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniformMatrix2fv].
}
@defproc[(glUniformMatrix2x3dv (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 1.2.
}
@defproc[(glUniformMatrix2x3fv (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 2.1.
}
@defproc[(glUniformMatrix2x4dv (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 1.2.
}
@defproc[(glUniformMatrix2x4fv (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 2.1.
}
@defproc[(glUniformMatrix3dv (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 1.2.
}
@defproc[(glUniformMatrix3fv (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 2.0.
}
@defproc[(glUniformMatrix3fvARB (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniformMatrix3fv].
}
@defproc[(glUniformMatrix3x2dv (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 1.2.
}
@defproc[(glUniformMatrix3x2fv (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 2.1.
}
@defproc[(glUniformMatrix3x4dv (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 1.2.
}
@defproc[(glUniformMatrix3x4fv (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 2.1.
}
@defproc[(glUniformMatrix4dv (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 1.2.
}
@defproc[(glUniformMatrix4fv (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 2.0.
}
@defproc[(glUniformMatrix4fvARB (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniformMatrix4fv].
}
@defproc[(glUniformMatrix4x2dv (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 1.2.
}
@defproc[(glUniformMatrix4x2fv (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 2.1.
}
@defproc[(glUniformMatrix4x3dv (location Integer) (count Integer) (transpose Boolean) (value F64Vector)) Void]{
Version 1.2.
}
@defproc[(glUniformMatrix4x3fv (location Integer) (count Integer) (transpose Boolean) (value F32Vector)) Void]{
Version 2.1.
}
@defproc[(glUniformSubroutinesuiv (shadertype Integer) (count Integer) (indices U32Vector)) Void]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniformSubroutines.xml"]{glUniformSubroutines manpage}.
}
@defproc[(glUniformui64NV (location Integer) (value Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glUniformui64vNV (location Integer) (count Integer) (value U64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glUnlockArraysEXT) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/compiled_vertex_array.txt"]{@racket[GL_EXT_compiled_vertex_array]}.
}
@defproc[(glUnmapBuffer (target Integer)) Boolean]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUnmapBuffer.xml"]{glUnmapBuffer manpage}.
}
@defproc[(glUnmapBufferARB (target Integer)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glUnmapBuffer].
}
@defproc[(glUnmapNamedBufferEXT (buffer Natural)) Boolean]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glUnmapObjectBufferATI (buffer Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/map_object_buffer.txt"]{@racket[GL_ATI_map_object_buffer]}.
}
@defproc[(glUpdateObjectBufferATI (buffer Natural) (offset Natural) (size Integer) (pointer GLPointer) (preserve Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glUseProgram (program Natural)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUseProgram.xml"]{glUseProgram manpage}.
}
@defproc[(glUseProgramObjectARB (programObj Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUseProgram].
}
@defproc[(glUseProgramStages (pipeline Natural) (stages Natural) (program Natural)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUseProgramStages.xml"]{glUseProgramStages manpage}.
}
@defproc[(glUseShaderProgramEXT (type Integer) (program Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/separate_shader_objects.txt"]{@racket[GL_EXT_separate_shader_objects]}.
}
