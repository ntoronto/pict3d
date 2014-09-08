#lang scribble/manual
@title{glL...}
@defproc[(glLightEnviSGIX (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glLightModelf (pname Integer) (param Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glLightModel.xml"]{glLightModel manpage}.
}
@defproc[(glLightModelfv (pname Integer) (params F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glLightModel.xml"]{glLightModel manpage}.
}
@defproc[(glLightModeli (pname Integer) (param Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glLightModel.xml"]{glLightModel manpage}.
}
@defproc[(glLightModeliv (pname Integer) (params S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glLightModel.xml"]{glLightModel manpage}.
}
@defproc[(glLightf (light Integer) (pname Integer) (param Flonum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glLight.xml"]{glLight manpage}.
}
@defproc[(glLightfv (light Integer) (pname Integer) (params F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glLight.xml"]{glLight manpage}.
}
@defproc[(glLighti (light Integer) (pname Integer) (param Integer)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glLight.xml"]{glLight manpage}.
}
@defproc[(glLightiv (light Integer) (pname Integer) (params S32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glLight.xml"]{glLight manpage}.
}
@defproc[(glLineStipple (factor Integer) (pattern Nonnegative-Fixnum)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glLineStipple.xml"]{glLineStipple manpage}.
}
@defproc[(glLineWidth (width Flonum)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glLineWidth.xml"]{glLineWidth manpage}.
}
@defproc[(glLinkProgram (program Natural)) Void]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glLinkProgram.xml"]{glLinkProgram manpage}.
}
@defproc[(glLinkProgramARB (programObj Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glLinkProgram].
}
@defproc[(glListBase (base Natural)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glListBase.xml"]{glListBase manpage}.
}
@defproc[(glListParameterfSGIX (list Natural) (pname Integer) (param Flonum)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/list_priority.txt"]{@racket[GL_SGIX_list_priority]}.
}
@defproc[(glListParameterfvSGIX (list Natural) (pname Integer) (params F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/list_priority.txt"]{@racket[GL_SGIX_list_priority]}.
}
@defproc[(glListParameteriSGIX (list Natural) (pname Integer) (param Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/list_priority.txt"]{@racket[GL_SGIX_list_priority]}.
}
@defproc[(glListParameterivSGIX (list Natural) (pname Integer) (params S32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/list_priority.txt"]{@racket[GL_SGIX_list_priority]}.
}
@defproc[(glLoadIdentity) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glLoadIdentity.xml"]{glLoadIdentity manpage}.
}
@defproc[(glLoadIdentityDeformationMapSGIX (mask Natural)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/polynomial_ffd.txt"]{@racket[GL_SGIX_polynomial_ffd]}.
}
@defproc[(glLoadMatrixd (m F64Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glLoadMatrix.xml"]{glLoadMatrix manpage}.
}
@defproc[(glLoadMatrixf (m F32Vector)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glLoadMatrix.xml"]{glLoadMatrix manpage}.
}
@defproc[(glLoadName (name Natural)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glLoadName.xml"]{glLoadName manpage}.
}
@defproc[(glLoadProgramNV (target Integer) (id Natural) (len Integer) (program (U Bytes String))) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glLoadTransposeMatrixd (m F64Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glLoadTransposeMatrix.xml"]{glLoadTransposeMatrix manpage}.
}
@defproc[(glLoadTransposeMatrixdARB (m F64Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/transpose_matrix.txt"]{@racket[GL_ARB_transpose_matrix]}.

Alias of @racket[glLoadTransposeMatrixd].
}
@defproc[(glLoadTransposeMatrixf (m F32Vector)) Void]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glLoadTransposeMatrix.xml"]{glLoadTransposeMatrix manpage}.
}
@defproc[(glLoadTransposeMatrixfARB (m F32Vector)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/transpose_matrix.txt"]{@racket[GL_ARB_transpose_matrix]}.

Alias of @racket[glLoadTransposeMatrixf].
}
@defproc[(glLockArraysEXT (first Integer) (count Integer)) Void]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/compiled_vertex_array.txt"]{@racket[GL_EXT_compiled_vertex_array]}.
}
@defproc[(glLogicOp (opcode Integer)) Void]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glLogicOp.xml"]{glLogicOp manpage}.
}
