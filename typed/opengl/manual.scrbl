#lang scribble/manual

@title{OpenGL}

@(defmodule opengl)

@(require (for-label racket/gui/base))
@(require (for-label ffi/vector))

@margin-note{This product is based on the published OpenGL® API, but is not an
implementation which is certified or licensed by Silicon Graphics, Inc.
under the OpenGL® API.}

An automatically-generated wrapper around the OpenGL library.  Being
automatically-generated means that it is quite complete.  It also
means that the wrapping may not be as nice as a manual wrapping.

Please note that these procedures are all very much @bold{unsafe};
in particular, if you call any of them while no OpenGL context
is active, you are almost certain to crash Racket.
An OpenGL context is typically establised by using the
@(xmethod canvas% with-gl-context) method.
If you are running from within DrRacket, I would recommend
to get used to saving your OpenGL program before running it.

This document contains, for reference, the calling conventions
for all the non-extension OpenGL procedures.
This information is also automatically generated.
This is only intended to easily look up type and number
of parameters and return values.
There is no information on what the various 
calls actually do; for that, please refer to the 
@hyperlink["http://www.opengl.org/documentation/"]{documentation of the OpenGL C API}.

The wrapper procedures automatically check for OpenGL errors after any call,
except between @racket[glBegin] and @racket[glEnd] where this is disallowed.
You don't have to call @racket[glGetError] yourself.

@include-section["generated/gl_specs_A.inc"]
@include-section["generated/gl_specs_B.inc"]
@include-section["generated/gl_specs_C.inc"]
@include-section["generated/gl_specs_D.inc"]
@include-section["generated/gl_specs_E.inc"]
@include-section["generated/gl_specs_F.inc"]
@include-section["generated/gl_specs_G.inc"]
@include-section["generated/gl_specs_H.inc"]
@include-section["generated/gl_specs_I.inc"]
@include-section["generated/gl_specs_L.inc"]
@include-section["generated/gl_specs_M.inc"]
@include-section["generated/gl_specs_N.inc"]
@include-section["generated/gl_specs_O.inc"]
@include-section["generated/gl_specs_P.inc"]
@include-section["generated/gl_specs_Q.inc"]
@include-section["generated/gl_specs_R.inc"]
@include-section["generated/gl_specs_S.inc"]
@include-section["generated/gl_specs_T.inc"]
@include-section["generated/gl_specs_U.inc"]
@include-section["generated/gl_specs_V.inc"]
@include-section["generated/gl_specs_W.inc"]

@section{Utility functions for homogenous vectors}

These functions are not part of the OpenGL API but are provided to make
working with @racket[glVertexPointer] and similar procedures easier.

@defproc[(gl-vector? (v any/c)) boolean?]{
  Returns @racket[#t] if @racket[v] belongs to one of
  the homogenous vector types which can be used with OpenGL, @racket[#f] otherwise.
  These vector types are:
  @racket[u8vector], @racket[s8vector], @racket[u16vector], @racket[s16vector],
  @racket[u32vector], @racket[s32vector], @racket[f32vector] and @racket[f64vector].
}

@defproc[(gl-pointer? (v any/c)) boolean?]{
  Returns @racket[#t] iff @racket[v] satisfies either 
  @racket[gl-vector?], @racket[cpointer?] or @racket[exact-nonnegative-integer?].
  This defines a very liberal "pointer-to-anything" type.
  Typically, OpenGL procedures which accept @racket[gl-pointer?] have additional requirements
  based on the context. In other words, the fact that the contract suggests that they accept such
  a wide range of values doesn't mean that in any particular context all values are legal.
}

@defproc[(gl-type? (v any/c)) boolean?]{
  Return @racket[#t] iff @racket[v] is one of the constants
  @racket[GL_UNSIGNED_BYTE], @racket[GL_BYTE], @racket[GL_UNSIGNED_SHORT], @racket[GL_SHORT], 
  @racket[GL_UNSIGNED_INT], @racket[GL_INT],
  @racket[GL_FLOAT], or @racket[GL_DOUBLE].
}

@defproc[(gl-vector->type (v gl-vector?)) gl-type?]{
  Determine the OpenGL type of @racket[v].
  This returns a numerical value such as @racket[GL_SHORT], @racket[GL_FLOAT], etc., which
  can be passed into @racket[glVertexPointer] and similar procedures.
}

@defproc[(gl-vector->cpointer (v gl-vector?)) cpointer?]{
  Get a C pointer to @racket[v].
}

@defproc[(gl-vector->length (v gl-vector?)) exact-nonnegative-integer?]{
  Get the length of @racket[v].
}

@defproc[(gl-vector->type/cpointer (v gl-vector?)) (values gl-type? cpointer?)]{
  Get the OpenGL type and C pointer of @racket[v].
  This is slightly more efficient than getting them each individually.
}

@defproc[(gl-vector->type/cpointer/length (v gl-vector?)) (values gl-type? cpointer? exact-nonnegative-integer?)]{
  Get the OpenGL type, C pointer and length of @racket[v].
  This is slightly more efficient than getting them each individually.
}

@defproc[(gl-vector-sizeof (v gl-vector?)) exact-nonnegative-integer?]{
  Get the length of @racket[v] in bytes.
}

@defproc[(gl-vector-alignof (v gl-vector?)) exact-nonnegative-integer?]{
  Get the alignment requirement of @racket[v].
}

@defproc[(gl-type->ctype (type gl-type?)) ctype?]{
  Get the C type associated with the given OpenGL type.
}

@defproc[(gl-type-sizeof (type gl-type?)) exact-nonnegative-integer?]{
  Get the length in bytes of values of type @racket[type].
}

@defproc[(gl-type-alignof (type gl-type?)) exact-nonnegative-integer?]{
  Get the alignment requirement of values of type @racket[type].
}

@section{Utility procedures for textures}

These procedures can be used to load 2D texture data.
Note that these, too, should only be called when an OpenGL context is active!

These procedures all load the alpha (transparancy) values as premultiplied alpha.
Since this is the only form of alpha blending which leads to correct results in all
cases@(cite "1"), no effort has been made to support other forms of alpha blending.

If your bitmaps contain transparent values, you should therefore enable alpha blending in OpenGL as follows.
@(racketblock
  (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)
  (glEnable GL_BLEND))

Note that some (older) OpenGL implementations may restrict textures to sizes which are powers of two.

@defproc[(bitmap->texture (bm (is-a?/c bitmap%))
                          (#:mipmap mipmap any/c #t)
                          (#:repeat repeat-mode (one-of/c 'none 'x 'y 'both) 'none))
                          exact-nonnegative->integer?]{
  Convert the bitmap into an OpenGL texture handle.
  As a side effect, the texture is bound to target @racket[GL_TEXTURE_2D].

  The parameter @racket[mipmap] (interpreted as a boolean) controls whether or not mipmapping is done.
  Mipmapping is a technique to avoid aliasing when an image is scaled down. If you are sure that your image
  will never be scaled down, you can save a small amount of memory and runtime by setting this parameter to @racket[#f].

  The @racket[repeat-mode] controls what happens if you use texture coordinates outside the range between 0 and 1.
  The parameter controls whether or not the image is repeated (tiled), and if it is repeated, 
  it defines along which of the specified axes (x, y, or both) the image is to be repeated.
}
  
@defproc[(load-texture (file (or/c path-string? input-port?))
                       (#:mipmap mipmap any/c #t)
                       (#:repeat repeat-mode (one-of/c 'none 'x 'y 'both) 'none))
                       exact-nonnegative->integer?]{
  Load a texture directly from a named file or input port.

  The parameters @racket[mipmap] and @racket[repeat-mode]
  have the same meaning as with @racket[bitmap->texture].
}

@section{Utility procedures for shaders}

These procedures can be used to load shaders written in GLSL (the GL Shading Language).
Note that these, too, should only be called when an OpenGL context is active!

@defproc[(load-shader (file (or/c path-string? input-port?))
                      (shader-type exact-nonnegative-integer?))
                       exact-nonnegative->integer?]{
  Load a shader written in GLSL from a named file or input port.

  The parameter @racket[shader-type] is typically either @racket[GL_VERTEX_SHADER]
  or @racket[GL_FRAGMENT_SHADER], although modern OpenGL versions define many
  more shader types. The returned shader object is already compiled and can be
  passed directly into @racket[create-program].
}


@defproc[(create-program (shader exact-nonnegative-integer?) ...) exact-nonnegative-integer?]{
  Create a complete GLSL program from combining several shaders.

  This procedure takes shaders as created by @racket[load-shader] and links them together
  into a single program. The resulting program is already linked and can be passed directly into @racket[glUseProgram].
}
 
@section{Additional utility procedures}

These procedures can be used to check the OpenGL version and supported extensions.
Note that these, too, should only be called when an OpenGL context is active!

@defproc[(gl-version) (listof exact-integer?)]{
Returns the OpenGL version, as a list of exact integers.
For example, version 3.1.2 would return a list (3 1 2).
}

@defproc[(gl-extensions) set-eq?]{
  Returns the supported OpenGL extensions, as a set of symbols.
}

@defproc[(gl-has-extension? (extension symbol?)) boolean?]{
  Checks whether the given extension is supported.
}

@defproc[(gl-version-at-least? (version (listof exact-integer?))) boolean?]{
  Checks if the OpenGL version is at least the given version.
}

@defproc[(GLsync? (v any/c)) boolean?]{
  Returns @racket[#t] if @racket[v] is an OpenGL sync object, 
  @racket[#f] otherwise.
}

@defproc[(set-gl-procedure-loader! (loader (-> string? (or/c cpointer? procedure? #f)))) any]{
  Registers a custom GL procedure loader.
  This is useful for exotic platforms which the default procedure loader does not know about.
  Note that the result of the loader is cached, so changing the loader halfway during your program
  will not work and will probably result in impressive crashes.
}

@defproc[(default-gl-procedure-loader (name string?)) (or/c cpointer? procedure? #f)]{
  This is the default loader. It knows how to load OpenGL procedures on Windows, MacOS X and Linux/Unix X11.
}


@(bibliography
 (bib-entry #:key "1"
  #:title "Composing Digital Images"
  #:author "Thomas Porter and Tom Duff"
  #:location "Computer Graphics Volume 18, Number 3 July 1984 pp 253-259"
  #:date "1984"
  #:url "http://keithp.com/~keithp/porterduff/"))
