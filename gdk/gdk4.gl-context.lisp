;;; ----------------------------------------------------------------------------
;;; gdk4.gl-context.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; GdkGLContext
;;;
;;;     OpenGL draw context
;;;
;;; Types and Values
;;;
;;;     GdkGLContext
;;;     GDKGLAPI
;;;     GdkGLError
;;;
;;; Accessors
;;;
;;;     gdk_gl_context_get_allowed_apis                    Since 4.6
;;;     gdk_gl_context_set_allowed_apis                    Since 4.6
;;;     gdk_gl_context_get_api                             Since 4.6
;;;     gdk_gl_context_get_shared_context                  Since 4.4 deprecated
;;;
;;; Functions
;;;
;;;     gdk_gl_context_get_display
;;;     gdk_gl_context_get_surface
;;;     gdk_gl_context_get_version
;;;     gdk_gl_context_set_required_version
;;;     gdk_gl_context_get_required_version
;;;     gdk_gl_context_set_debug_enabled
;;;     gdk_gl_context_get_debug_enabled
;;;     gdk_gl_context_set_forward_compatible
;;;     gdk_gl_context_get_forward_compatible
;;;     gdk_gl_context_set_use_es
;;;     gdk_gl_context_get_use_es
;;;     gdk_gl_context_is_legacy
;;;     gdk_gl_context_realize
;;;     gdk_gl_context_make_current
;;;     gdk_gl_context_get_current
;;;     gdk_gl_context_clear_current
;;;
;;;     gdk_gl_context_is_shared                           Since 4.4
;;;
;;; Properties
;;;
;;;     allowed-apis                                       Since 4.6
;;;     api                                                Since 4.6
;;;     shared-context                                     Since 4.4 deprecated
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDrawContext
;;;         ╰── GdkGLContext
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkGLError
;;;
;;; Error enumeration for GdkGLContext.
;;;
;;; GDK_GL_ERROR_NOT_AVAILABLE :
;;;     OpenGL support is not available
;;;
;;; GDK_GL_ERROR_UNSUPPORTED_FORMAT :
;;;     The requested visual format is not supported
;;;
;;; GDK_GL_ERROR_UNSUPPORTED_PROFILE :
;;;     The requested profile is not supported
;;;
;;; GDK_GL_ERROR_COMPILATION_FAILED :
;;;     The shader compilation failed
;;;
;;; GDK_GL_ERROR_LINK_FAILED :
;;;     The shader linking failed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkGLAPI
;;;
;;; The list of the different APIs that GdkGLContext can potentially support.
;;;
;;; Since 4.6
;;;
;;;
;;; GDK_GL_API_GL
;;;     The OpenGL API.
;;;
;;; GDK_GL_API_GLES
;;;     The OpenGL ES API.
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkGLAPI" gl-api
  (:export t
   :type-initializer "gdk_gl_api_get_type")
  :gl
  :gles)

;;; ----------------------------------------------------------------------------
;;; GdkGLContext
;;;
;;; typedef struct _GdkGLContext GdkGLContext;
;;;
;;; The GdkGLContext struct contains only private fields and should not be
;;; accessed directly.
;;;
;;; Description
;;;
;;; GdkGLContext is an object representing the platform-specific OpenGL draw
;;; context.
;;;
;;; GdkGLContexts are created for a GdkSurface using
;;; gdk_surface_create_gl_context(), and the context will match the the
;;; characteristics of the surface.
;;;
;;; A GdkGLContext is not tied to any particular normal framebuffer. For
;;; instance, it cannot draw to the GdkSurface back buffer. The GDK repaint
;;; system is in full control of the painting to that. Instead, you can create
;;; render buffers or textures and use gdk_cairo_draw_from_gl() in the draw
;;; function of your widget to draw them. Then GDK will handle the integration
;;; of your rendering with that of other widgets.
;;;
;;; Support for GdkGLContext is platform-specific, context creation can fail,
;;; returning NULL context.
;;;
;;; A GdkGLContext has to be made "current" in order to start using it,
;;; otherwise any OpenGL call will be ignored.
;;;
;;; Creating a new OpenGL context
;;;
;;; In order to create a new GdkGLContext instance you need a GdkSurface, which
;;; you typically get during the realize call of a widget.
;;;
;;; A GdkGLContext is not realized until either gdk_gl_context_make_current(),
;;; or until it is realized using gdk_gl_context_realize(). It is possible to
;;; specify details of the GL context like the OpenGL version to be used, or
;;; whether the GL context should have extra state validation enabled after
;;; calling gdk_surface_create_gl_context() by calling gdk_gl_context_realize().
;;; If the realization fails you have the option to change the settings of the
;;; GdkGLContext and try again.
;;;
;;; Using a GdkGLContext
;;;
;;; You will need to make the GdkGLContext the current context before issuing
;;; OpenGL calls; the system sends OpenGL commands to whichever context is
;;; current. It is possible to have multiple contexts, so you always need to
;;; ensure that the one which you want to draw with is the current one before
;;; issuing commands:
;;;
;;; gdk_gl_context_make_current (context);
;;;
;;; You can now perform your drawing using OpenGL commands.
;;;
;;; You can check which GdkGLContext is the current one by using
;;; gdk_gl_context_get_current(); you can also unset any GdkGLContext that is
;;; currently set by calling gdk_gl_context_clear_current().
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GdkGLContext" gl-context
  (:superclass draw-context
   :export t
   :interfaces nil
   :type-initializer "gdk_gl_context_get_type")
  ((allowed-apis
    gl-context-allowed-apis
    "allowed-apis" "GdkGLAPI" t t)
   (api
    gl-context-api
    "api" "gdkGLAPI" t nil)
   (shared-context
    gl-context-shared-context
    "shared-context" "GdkGLContext" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;     allowed-apis                                       Since 4.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;     api                                                Since 4.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “shared-context” property
;;;
;;;  “shared-context”           GdkGLContext *
;;;
;;; The GdkGLContext that this context is sharing data with, or NULL
;;;
;;; Owner: GdkGLContext
;;;
;;; Flags: Read / Write / Construct Only
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_shared_context ()                   Since 4.4 deprecated
;;;
;;; GdkGLContext *
;;; gdk_gl_context_get_shared_context (GdkGLContext *context);
;;;
;;; Retrieves the GdkGLContext that this context share data with.
;;;
;;; context :
;;;     a GdkGLContext
;;;
;;; Returns :
;;;     a GdkGLContext or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_display ()
;;;
;;; GdkDisplay *
;;; gdk_gl_context_get_display (GdkGLContext *context);
;;;
;;; Retrieves the GdkDisplay the context is created for
;;;
;;; context :
;;;     a GdkGLContext
;;;
;;; Returns :
;;;     a GdkDisplay or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_surface ()
;;;
;;; GdkSurface *
;;; gdk_gl_context_get_surface (GdkGLContext *context);
;;;
;;; Retrieves the GdkSurface used by the context .
;;;
;;; context :
;;;     a GdkGLContext
;;;
;;; Returns :
;;;     a GdkSurface or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_version ()
;;;
;;; void
;;; gdk_gl_context_get_version (GdkGLContext *context,
;;;                             int *major,
;;;                             int *minor);
;;;
;;; Retrieves the OpenGL version of the context .
;;;
;;; The context must be realized prior to calling this function.
;;;
;;; context :
;;;     a GdkGLContext
;;;
;;; major :
;;;     return location for the major version.
;;;
;;; minor :
;;;     return location for the minor version.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_required_version ()
;;;
;;; void
;;; gdk_gl_context_set_required_version (GdkGLContext *context,
;;;                                      int major,
;;;                                      int minor);
;;;
;;; Sets the major and minor version of OpenGL to request.
;;;
;;; Setting major and minor to zero will use the default values.
;;;
;;; The GdkGLContext must not be realized or made current prior to calling this
;;; function.
;;;
;;; context :
;;;     a GdkGLContext
;;;
;;; major :
;;;     the major version to request
;;;
;;; minor :
;;;     the minor version to request
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_required_version ()
;;;
;;; void
;;; gdk_gl_context_get_required_version (GdkGLContext *context,
;;;                                      int *major,
;;;                                      int *minor);
;;;
;;; Retrieves the major and minor version requested by calling
;;; gdk_gl_context_set_required_version().
;;;
;;; context :
;;;     a GdkGLContext
;;;
;;; major :
;;;     return location for the major version to request.
;;;
;;; minor :
;;;     return location for the minor version to request.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_debug_enabled ()
;;;
;;; void
;;; gdk_gl_context_set_debug_enabled (GdkGLContext *context,
;;;                                   gboolean enabled);
;;;
;;; Sets whether the GdkGLContext should perform extra validations and run time
;;; checking. This is useful during development, but has additional overhead.
;;;
;;; The GdkGLContext must not be realized or made current prior to calling this
;;; function.
;;;
;;; context :
;;;     a GdkGLContext
;;;
;;; enabled :
;;;     whether to enable debugging in the context
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_debug_enabled ()
;;;
;;; gboolean
;;; gdk_gl_context_get_debug_enabled (GdkGLContext *context);
;;;
;;; Retrieves the value set using gdk_gl_context_set_debug_enabled().
;;;
;;; context :
;;;     a GdkGLContext
;;;
;;; Returns :
;;;     TRUE if debugging is enabled
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_forward_compatible ()
;;;
;;; void
;;; gdk_gl_context_set_forward_compatible (GdkGLContext *context,
;;;                                        gboolean compatible);
;;;
;;; Sets whether the GdkGLContext should be forward compatible.
;;;
;;; Forward compatible contexts must not support OpenGL functionality that has
;;; been marked as deprecated in the requested version; non-forward compatible
;;; contexts, on the other hand, must support both deprecated and non deprecated
;;; functionality.
;;;
;;; The GdkGLContext must not be realized or made current prior to calling this
;;; function.
;;;
;;; context :
;;;     a GdkGLContext
;;;
;;; compatible :
;;;     whether the context should be forward compatible
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_forward_compatible ()
;;;
;;; gboolean
;;; gdk_gl_context_get_forward_compatible (GdkGLContext *context);
;;;
;;; Retrieves the value set using gdk_gl_context_set_forward_compatible().
;;;
;;; context :
;;;     a GdkGLContext
;;;
;;; Returns :
;;;     TRUE if the context should be forward compatible
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_use_es ()
;;;
;;; void
;;; gdk_gl_context_set_use_es (GdkGLContext *context,
;;;                            int use_es);
;;;
;;; Requests that GDK create an OpenGL ES context instead of an OpenGL one, if
;;; the platform and windowing system allows it.
;;;
;;; The context must not have been realized.
;;;
;;; By default, GDK will attempt to automatically detect whether the underlying
;;; GL implementation is OpenGL or OpenGL ES once the context is realized.
;;;
;;; You should check the return value of gdk_gl_context_get_use_es() after
;;; calling gdk_gl_context_realize() to decide whether to use the OpenGL or
;;; OpenGL ES API, extensions, or shaders.
;;;
;;; context :
;;;     a GdkGLContext:
;;;
;;; use_es :
;;;     whether the context should use OpenGL ES instead of OpenGL, or -1 to
;;;     allow auto-detection
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_use_es ()
;;;
;;; gboolean
;;; gdk_gl_context_get_use_es (GdkGLContext *context);
;;;
;;; Checks whether the context is using an OpenGL or OpenGL ES profile.
;;;
;;; context :
;;;     a GdkGLContext
;;;
;;; Returns :
;;;     TRUE if the GdkGLContext is using an OpenGL ES profile
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_is_legacy ()
;;;
;;; gboolean
;;; gdk_gl_context_is_legacy (GdkGLContext *context);
;;;
;;; Whether the GdkGLContext is in legacy mode or not.
;;;
;;; The GdkGLContext must be realized before calling this function.
;;;
;;; When realizing a GL context, GDK will try to use the OpenGL 3.2 core
;;; profile; this profile removes all the OpenGL API that was deprecated prior
;;; to the 3.2 version of the specification. If the realization is successful,
;;; this function will return FALSE.
;;;
;;; If the underlying OpenGL implementation does not support core profiles, GDK
;;; will fall back to a pre-3.2 compatibility profile, and this function will
;;; return TRUE.
;;;
;;; You can use the value returned by this function to decide which kind of
;;; OpenGL API to use, or whether to do extension discovery, or what kind of
;;; shader programs to load.
;;;
;;; context :
;;;     a GdkGLContext
;;;
;;; Returns :
;;;     TRUE if the GL context is in legacy mode
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_realize ()
;;;
;;; gboolean
;;; gdk_gl_context_realize (GdkGLContext *context,
;;;                         GError **error);
;;;
;;; Realizes the given GdkGLContext.
;;;
;;; It is safe to call this function on a realized GdkGLContext.
;;;
;;; context :
;;;     a GdkGLContext
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     TRUE if the context is realized
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_make_current ()
;;;
;;; void
;;; gdk_gl_context_make_current (GdkGLContext *context);
;;;
;;; Makes the context the current one.
;;;
;;; context :
;;;     a GdkGLContext
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_current ()
;;;
;;; GdkGLContext *
;;; gdk_gl_context_get_current (void);
;;;
;;; Retrieves the current GdkGLContext.
;;;
;;; Returns :
;;;     the current GdkGLContext, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_clear_current ()
;;;
;;; void
;;; gdk_gl_context_clear_current (void);
;;;
;;; Clears the current GdkGLContext.
;;;
;;; Any OpenGL call after this function returns will be ignored until
;;; gdk_gl_context_make_current() is called.
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk4.gl-context.lisp ---------------------------------------
