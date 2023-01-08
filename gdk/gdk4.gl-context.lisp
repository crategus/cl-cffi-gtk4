;;; ----------------------------------------------------------------------------
;;; gdk.gl-context.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

;;;GdkGLContext
;;;GdkGLContext — OpenGL draw context

;;;Functions
;;;GdkDisplay *	gdk_gl_context_get_display ()
;;;GdkSurface *	gdk_gl_context_get_surface ()
;;;GdkGLContext *	gdk_gl_context_get_shared_context ()
;;;void	gdk_gl_context_get_version ()
;;;void	gdk_gl_context_set_required_version ()
;;;void	gdk_gl_context_get_required_version ()
;;;void	gdk_gl_context_set_debug_enabled ()
;;;gboolean	gdk_gl_context_get_debug_enabled ()
;;;void	gdk_gl_context_set_forward_compatible ()
;;;gboolean	gdk_gl_context_get_forward_compatible ()
;;;void	gdk_gl_context_set_use_es ()
;;;gboolean	gdk_gl_context_get_use_es ()
;;;gboolean	gdk_gl_context_is_legacy ()
;;;gboolean	gdk_gl_context_realize ()
;;;void	gdk_gl_context_make_current ()
;;;GdkGLContext *	gdk_gl_context_get_current ()
;;;void	gdk_gl_context_clear_current ()
;;;Properties
;;;GdkGLContext *	shared-context	Read / Write / Construct Only
;;;Types and Values
;;; 	GdkGLContext
;;;enum	GdkGLError
;;;Object Hierarchy
;;;    GObject
;;;    ╰── GdkDrawContext
;;;        ╰── GdkGLContext
;;;Includes
;;;#include <gdk/gdk.h>
;;;Description
;;;GdkGLContext is an object representing the platform-specific OpenGL draw context.

;;;GdkGLContexts are created for a GdkSurface using gdk_surface_create_gl_context(), and the context will match the the characteristics of the surface.

;;;A GdkGLContext is not tied to any particular normal framebuffer. For instance, it cannot draw to the GdkSurface back buffer. The GDK repaint system is in full control of the painting to that. Instead, you can create render buffers or textures and use gdk_cairo_draw_from_gl() in the draw function of your widget to draw them. Then GDK will handle the integration of your rendering with that of other widgets.

;;;Support for GdkGLContext is platform-specific, context creation can fail, returning NULL context.

;;;A GdkGLContext has to be made "current" in order to start using it, otherwise any OpenGL call will be ignored.

;;;Creating a new OpenGL context
;;;In order to create a new GdkGLContext instance you need a GdkSurface, which you typically get during the realize call of a widget.

;;;A GdkGLContext is not realized until either gdk_gl_context_make_current(), or until it is realized using gdk_gl_context_realize(). It is possible to specify details of the GL context like the OpenGL version to be used, or whether the GL context should have extra state validation enabled after calling gdk_surface_create_gl_context() by calling gdk_gl_context_realize(). If the realization fails you have the option to change the settings of the GdkGLContext and try again.

;;;Using a GdkGLContext
;;;You will need to make the GdkGLContext the current context before issuing OpenGL calls; the system sends OpenGL commands to whichever context is current. It is possible to have multiple contexts, so you always need to ensure that the one which you want to draw with is the current one before issuing commands:

;;;gdk_gl_context_make_current (context);
;;;You can now perform your drawing using OpenGL commands.

;;;You can check which GdkGLContext is the current one by using gdk_gl_context_get_current(); you can also unset any GdkGLContext that is currently set by calling gdk_gl_context_clear_current().

;;;Functions
;;;gdk_gl_context_get_display ()
;;;GdkDisplay *
;;;gdk_gl_context_get_display (GdkGLContext *context);
;;;Retrieves the GdkDisplay the context is created for

;;;Parameters
;;;context

;;;a GdkGLContext

;;;
;;;Returns
;;;a GdkDisplay or NULL.

;;;[nullable][transfer none]

;;;gdk_gl_context_get_surface ()
;;;GdkSurface *
;;;gdk_gl_context_get_surface (GdkGLContext *context);
;;;Retrieves the GdkSurface used by the context .

;;;Parameters
;;;context

;;;a GdkGLContext

;;;
;;;Returns
;;;a GdkSurface or NULL.

;;;[nullable][transfer none]

;;;gdk_gl_context_get_shared_context ()
;;;GdkGLContext *
;;;gdk_gl_context_get_shared_context (GdkGLContext *context);
;;;Retrieves the GdkGLContext that this context share data with.

;;;Parameters
;;;context

;;;a GdkGLContext

;;;
;;;Returns
;;;a GdkGLContext or NULL.

;;;[nullable][transfer none]

;;;gdk_gl_context_get_version ()
;;;void
;;;gdk_gl_context_get_version (GdkGLContext *context,
;;;                            int *major,
;;;                            int *minor);
;;;Retrieves the OpenGL version of the context .

;;;The context must be realized prior to calling this function.

;;;Parameters
;;;context

;;;a GdkGLContext

;;;
;;;major

;;;return location for the major version.

;;;[out]
;;;minor

;;;return location for the minor version.

;;;[out]
;;;gdk_gl_context_set_required_version ()
;;;void
;;;gdk_gl_context_set_required_version (GdkGLContext *context,
;;;                                     int major,
;;;                                     int minor);
;;;Sets the major and minor version of OpenGL to request.

;;;Setting major and minor to zero will use the default values.

;;;The GdkGLContext must not be realized or made current prior to calling this function.

;;;Parameters
;;;context

;;;a GdkGLContext

;;;
;;;major

;;;the major version to request

;;;
;;;minor

;;;the minor version to request

;;;
;;;gdk_gl_context_get_required_version ()
;;;void
;;;gdk_gl_context_get_required_version (GdkGLContext *context,
;;;                                     int *major,
;;;                                     int *minor);
;;;Retrieves the major and minor version requested by calling gdk_gl_context_set_required_version().

;;;Parameters
;;;context

;;;a GdkGLContext

;;;
;;;major

;;;return location for the major version to request.

;;;[out][nullable]
;;;minor

;;;return location for the minor version to request.

;;;[out][nullable]
;;;gdk_gl_context_set_debug_enabled ()
;;;void
;;;gdk_gl_context_set_debug_enabled (GdkGLContext *context,
;;;                                  gboolean enabled);
;;;Sets whether the GdkGLContext should perform extra validations and run time checking. This is useful during development, but has additional overhead.

;;;The GdkGLContext must not be realized or made current prior to calling this function.

;;;Parameters
;;;context

;;;a GdkGLContext

;;;
;;;enabled

;;;whether to enable debugging in the context

;;;
;;;gdk_gl_context_get_debug_enabled ()
;;;gboolean
;;;gdk_gl_context_get_debug_enabled (GdkGLContext *context);
;;;Retrieves the value set using gdk_gl_context_set_debug_enabled().

;;;Parameters
;;;context

;;;a GdkGLContext

;;;
;;;Returns
;;;TRUE if debugging is enabled

;;;gdk_gl_context_set_forward_compatible ()
;;;void
;;;gdk_gl_context_set_forward_compatible (GdkGLContext *context,
;;;                                       gboolean compatible);
;;;Sets whether the GdkGLContext should be forward compatible.

;;;Forward compatible contexts must not support OpenGL functionality that has been marked as deprecated in the requested version; non-forward compatible contexts, on the other hand, must support both deprecated and non deprecated functionality.

;;;The GdkGLContext must not be realized or made current prior to calling this function.

;;;Parameters
;;;context

;;;a GdkGLContext

;;;
;;;compatible

;;;whether the context should be forward compatible

;;;
;;;gdk_gl_context_get_forward_compatible ()
;;;gboolean
;;;gdk_gl_context_get_forward_compatible (GdkGLContext *context);
;;;Retrieves the value set using gdk_gl_context_set_forward_compatible().

;;;Parameters
;;;context

;;;a GdkGLContext

;;;
;;;Returns
;;;TRUE if the context should be forward compatible

;;;gdk_gl_context_set_use_es ()
;;;void
;;;gdk_gl_context_set_use_es (GdkGLContext *context,
;;;                           int use_es);
;;;Requests that GDK create an OpenGL ES context instead of an OpenGL one, if the platform and windowing system allows it.

;;;The context must not have been realized.

;;;By default, GDK will attempt to automatically detect whether the underlying GL implementation is OpenGL or OpenGL ES once the context is realized.

;;;You should check the return value of gdk_gl_context_get_use_es() after calling gdk_gl_context_realize() to decide whether to use the OpenGL or OpenGL ES API, extensions, or shaders.

;;;Parameters
;;;context

;;;a GdkGLContext:

;;;
;;;use_es

;;;whether the context should use OpenGL ES instead of OpenGL, or -1 to allow auto-detection

;;;
;;;gdk_gl_context_get_use_es ()
;;;gboolean
;;;gdk_gl_context_get_use_es (GdkGLContext *context);
;;;Checks whether the context is using an OpenGL or OpenGL ES profile.

;;;Parameters
;;;context

;;;a GdkGLContext

;;;
;;;Returns
;;;TRUE if the GdkGLContext is using an OpenGL ES profile

;;;gdk_gl_context_is_legacy ()
;;;gboolean
;;;gdk_gl_context_is_legacy (GdkGLContext *context);
;;;Whether the GdkGLContext is in legacy mode or not.

;;;The GdkGLContext must be realized before calling this function.

;;;When realizing a GL context, GDK will try to use the OpenGL 3.2 core profile; this profile removes all the OpenGL API that was deprecated prior to the 3.2 version of the specification. If the realization is successful, this function will return FALSE.

;;;If the underlying OpenGL implementation does not support core profiles, GDK will fall back to a pre-3.2 compatibility profile, and this function will return TRUE.

;;;You can use the value returned by this function to decide which kind of OpenGL API to use, or whether to do extension discovery, or what kind of shader programs to load.

;;;Parameters
;;;context

;;;a GdkGLContext

;;;
;;;Returns
;;;TRUE if the GL context is in legacy mode

;;;gdk_gl_context_realize ()
;;;gboolean
;;;gdk_gl_context_realize (GdkGLContext *context,
;;;                        GError **error);
;;;Realizes the given GdkGLContext.

;;;It is safe to call this function on a realized GdkGLContext.

;;;Parameters
;;;context

;;;a GdkGLContext

;;;
;;;error

;;;return location for a GError

;;;
;;;Returns
;;;TRUE if the context is realized

;;;gdk_gl_context_make_current ()
;;;void
;;;gdk_gl_context_make_current (GdkGLContext *context);
;;;Makes the context the current one.

;;;Parameters
;;;context

;;;a GdkGLContext

;;;
;;;gdk_gl_context_get_current ()
;;;GdkGLContext *
;;;gdk_gl_context_get_current (void);
;;;Retrieves the current GdkGLContext.

;;;Returns
;;;the current GdkGLContext, or NULL.

;;;[nullable][transfer none]

;;;gdk_gl_context_clear_current ()
;;;void
;;;gdk_gl_context_clear_current (void);
;;;Clears the current GdkGLContext.

;;;Any OpenGL call after this function returns will be ignored until gdk_gl_context_make_current() is called.

;;;Types and Values
;;;GdkGLContext
;;;typedef struct _GdkGLContext GdkGLContext;
;;;The GdkGLContext struct contains only private fields and should not be accessed directly.

;;;enum GdkGLError
;;;Error enumeration for GdkGLContext.

;;;Members
;;;GDK_GL_ERROR_NOT_AVAILABLE

;;;OpenGL support is not available

;;;
;;;GDK_GL_ERROR_UNSUPPORTED_FORMAT

;;;The requested visual format is not supported

;;;
;;;GDK_GL_ERROR_UNSUPPORTED_PROFILE

;;;The requested profile is not supported

;;;
;;;GDK_GL_ERROR_COMPILATION_FAILED

;;;The shader compilation failed

;;;
;;;GDK_GL_ERROR_LINK_FAILED

;;;The shader linking failed

;;;
;;;Property Details
;;;The “shared-context” property
;;;  “shared-context”           GdkGLContext *
;;;The GdkGLContext that this context is sharing data with, or NULL

;;;Owner: GdkGLContext

;;;Flags: Read / Write / Construct Only

;;; --- End of file gdk.gl-context.lisp ----------------------------------------
