#lang scribble/manual
@title{glA...}
@defproc[(glAccum (op Integer) (value Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glAccum.xml"]{glAccum manpage}.
}
@defproc[(glActiveProgramEXT (program Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/separate_shader_objects.txt"]{@racket[GL_EXT_separate_shader_objects]}.
}
@defproc[(glActiveShaderProgram (pipeline Natural) (program Natural)) Void]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glActiveShaderProgram.xml"]{glActiveShaderProgram manpage}.
}
@defproc[(glActiveStencilFaceEXT (face Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/stencil_two_side.txt"]{@racket[GL_EXT_stencil_two_side]}.
}
@defproc[(glActiveTexture (texture Integer)) Void]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glActiveTexture.xml"]{glActiveTexture manpage}.
}
@defproc[(glActiveTextureARB (texture Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glActiveTexture].
}
@defproc[(glActiveVaryingNV (program Natural) (name (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.
}
@defproc[(glAlphaFragmentOp1ATI (op Integer) (dst Natural) (dstMod Natural) (arg1 Natural) (arg1Rep Natural) (arg1Mod Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glAlphaFragmentOp2ATI (op Integer) (dst Natural) (dstMod Natural) (arg1 Natural) (arg1Rep Natural) (arg1Mod Natural) (arg2 Natural) (arg2Rep Natural) (arg2Mod Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glAlphaFragmentOp3ATI (op Integer) (dst Natural) (dstMod Natural) (arg1 Natural) (arg1Rep Natural) (arg1Mod Natural) (arg2 Natural) (arg2Rep Natural) (arg2Mod Natural) (arg3 Natural) (arg3Rep Natural) (arg3Mod Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glAlphaFunc (func Integer) (ref Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glAlphaFunc.xml"]{glAlphaFunc manpage}.
}
@defproc[(glApplyTextureEXT (mode Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/light_texture.txt"]{@racket[GL_EXT_light_texture]}.
}
@defproc[(glAreProgramsResidentNV (n Integer) (programs U32Vector)) (values Boolean (Vectorof Boolean))]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glAreTexturesResident (n Integer) (textures U32Vector)) (values Boolean (Vectorof Boolean))]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glAreTexturesResident.xml"]{glAreTexturesResident manpage}.
}
@defproc[(glAreTexturesResidentEXT (n Integer) (textures U32Vector)) (values Boolean (Vectorof Boolean))]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_object.txt"]{@racket[GL_EXT_texture_object]}.
}
@defproc[(glArrayElement (i Integer)) Void]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glArrayElement.xml"]{glArrayElement manpage}.
}
@defproc[(glArrayElementEXT (i Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.

Alias of @racket[glArrayElement].
}
@defproc[(glArrayObjectATI (array Integer) (size Integer) (type Integer) (stride Integer) (buffer Natural) (offset Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glAsyncMarkerSGIX (marker Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/async.txt"]{@racket[GL_SGIX_async]}.
}
@defproc[(glAttachObjectARB (containerObj Natural) (obj Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glAttachShader].
}
@defproc[(glAttachShader (program Natural) (shader Natural)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glAttachShader.xml"]{glAttachShader manpage}.
}
