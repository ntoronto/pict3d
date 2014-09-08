#lang scribble/manual
@title{glO...}
@defproc[(glObjectLabel (identifier Integer) (name Natural) (length Integer) (label (U Bytes String))) Void]{
Version 4.3.
}
@defproc[(glObjectPtrLabel (ptr GLPointer) (length Integer) (label (U Bytes String))) Void]{
Version 4.3.
}
@defproc[(glObjectPurgeableAPPLE (objectType Integer) (name Natural) (option Integer)) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/object_purgeable.txt"]{@racket[GL_APPLE_object_purgeable]}.
}
@defproc[(glObjectUnpurgeableAPPLE (objectType Integer) (name Natural) (option Integer)) Integer]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/object_purgeable.txt"]{@racket[GL_APPLE_object_purgeable]}.
}
@defproc[(glOrtho (left Real) (right Real) (bottom Real) (top Real) (zNear Real) (zFar Real)) Void]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glOrtho.xml"]{glOrtho manpage}.
}
