;;; ----------------------------------------------------------------------------
;;; gdk4.gl-context.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2024 Dieter Kaiser
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
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(gobject:define-gflags "GdkGLAPI" gl-api
  (:export t
   :type-initializer "gdk_gl_api_get_type")
  (:none 0)
  (:gl #.(ash 1 0))
  (:gles #.(ash 1 1)))

#+gtk-4-6
#+liber-documentation
(setf (liber:alias-for-symbol 'gl-api)
      "GFlags"
      (liber:symbol-documentation 'gl-api)
 "@version{#2023-8-3}
  @begin{declaration}
(gobject:define-gflags \"GdkGLAPI\" gl-api
  (:export t
   :type-initializer \"gdk_gl_api_get_type\")
  (:none 0)
  (:gl #.(ash 1 0))
  (:gles #.(ash 1 1)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No API.}
      @entry[:gl]{The OpenGL API.}
      @entry[:gles]{The OpenGL ES API.}
    @end{table}
  @end{values}
  @begin{short}
    The list of the different APIs that a @class{gdk:gl-context} object can
    potentially support.
  @end{short}
  Since 4.6
  @see-class{gdk:gl-context}")

;;; ----------------------------------------------------------------------------
;;; GdkGLContext
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkGLContext" gl-context
  (:superclass draw-context
   :export t
   :interfaces nil
   :type-initializer "gdk_gl_context_get_type")
  (#+gtk-4-6
   (allowed-apis
    gl-context-allowed-apis
    "allowed-apis" "GdkGLAPI" t t)
   #+gtk-4-6
   (api
    gl-context-api
    "api" "gdkGLAPI" t nil)
   (shared-context
    gl-context-shared-context
    "shared-context" "GdkGLContext" t t)))

#+liber-documentation
(setf (documentation 'gl-context 'type)
 "@version{#2023-8-3}
  @begin{short}
    The @class{gdk:gl-context} object is an object representing the platform
    specific OpenGL draw context.
  @end{short}
  A @class{gdk:gl-context} object is created for a @class{gdk:surface} object
  using the @fun{gdk:surface-create-gl-context} function, and the context will
  match the the characteristics of the surface.

  A @class{gdk:gl-context} object is not tied to any particular normal
  framebuffer. For instance, it cannot draw to the @class{gdk:surface} back
  buffer. The GDK repaint system is in full control of the painting to that.
  Instead, you can create render buffers or textures and use
  the @fun{gdk:cairo-draw-from-gl} function in the draw function of your widget
  to draw them. Then GDK will handle the integration of your rendering with
  that of other widgets.

  Support for the @class{gdk:gl-context} object is platform specific, context
  creation can fail, returning a @code{NULL} context.

  A @class{gdk:gl-context} object has to be made \"current\" in order to start
  using it, otherwise any OpenGL call will be ignored.

  @subheading{Creating a new OpenGL context}
  In order to create a new @class{gdk:gl-context} object you need a
  @class{gdk:surface} object, which you typically get during the realize call
  of a widget.

  A @class{gdk:gl-context} object is not realized until either the
  @fun{gdk:gl-context-make-current} function, or until it is realized using
  the @fun{gdk:gl-context-realize} function. It is possible to specify details
  of the GL context like the OpenGL version to be used, or whether the GL
  context should have extra state validation enabled after calling the
  @fun{gdk:surface-create-gl-context} function by calling the
  @fun{gdk:gl-context-realize} function. If the realization fails you have the
  option to change the settings of the @class{gdk:gl-context} object and try
  again.

  @subheading{Using a GdkGLContext}
  You will need to make the @class{gdk:gl-ontext} object the current context
  before issuing OpenGL calls. The system sends OpenGL commands to whichever
  context is current. It is possible to have multiple contexts, so you always
  need to ensure that the one which you want to draw with is the current one
  before issuing commands:
  @begin{pre}
(gdk:gl-context-make-current context)
  @end{pre}
  You can now perform your drawing using OpenGL commands.

  You can check which @class{gdk:gl-context} object is the current one by using
  the @fun{gdk:gl-context-current} function. You can also unset any
  @class{gdk:gl-context} object that is currently set by calling the
  @fun{gdk:gl-context-clear-current} function.
  @see-slot{gdk:gl-context-allowed-apis}
  @see-slot{gdk:gl-context-api}
  @see-slot{gdk:gl-context-shared-context}
  @see-class{gdk:draw-context}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gl-context-allowed-apis ------------------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "allowed-apis" 'gl-context) t)
 "The @code{allowed-apis} property of type @symbol{gdk:gl-api} (Read / Write)
  @br{}
  The allowed APIs. Since 4.6 @br{}
  Default value: @code{'(:gl :gles)}")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'gl-context-allowed-apis)
      "Accessor"
      (documentation 'gl-context-allowed-apis 'function)
 "@version{#2023-8-4}
  @syntax{(gdk:gl-context-allowed-apis object) => apis}
  @syntax{(setf (gdk:gl-context-allowed-apis object) apis)}
  @argument[object]{a @class{gdk:gl-context} object}
  @argument[apis]{a @symbol{gdk:gl-apis} value}
  @begin{short}
    Accessor of the @slot[gdk:gl-context]{allowed-apis} slot of the
    @class{gdk:gl-context} class.
  @end{short}
  The @fun{gdk:gl-context-allowed-apis} function gets the allowed APIs. The
  @setf{gdk:gl-context-allowed-apis} function sets the allowed APIs. When the
  @fun{gdk:gl-context-realize} function is called, only the allowed APIs will
  be tried. If you set this to @code{:none}, realizing will always fail.

  If you set it on a realized context, the property will not have any effect.
  It is only relevant during the @fun{gdk:gl-context-realize} function.

  By default, all APIs are allowed.

  Since 4.6
  @see-class{gdk:gl-context}
  @see-symbol{gdk:gl-api}
  @see-function{gdk:gl-context-realize}")

;;; --- gl-context-api ---------------------------------------------------------

#+ (and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "api" 'gl-context) t)
 "The @code{api} property of type @class{gdk:gl-api} (Read) @br{}
  The API currently in use. Since 4.6 @br{}
  Default value: @code{:none}")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'gl-context-api)
      "Accessor"
      (documentation 'gl-context-api 'function)
 "@version{#2023-8-4}
  @syntax{(gdk:gl-context-api object) => api}
  @argument[object]{a @class{gdk:gl-context} object}
  @argument[api]{a @symbol{gdk:gl-api} value}
  @begin{short}
    Accessor of the @slot[gdk:gl-context]{api} slot of the
    @class{gdk:gl-context} class.
  @end{short}
  The @fun{gdk:gl-context-api} function gets the API currently in use. If the
  renderer has not been realized yet, @code{:none} is returned.

  Since 4.6
  @see-class{gdk:gl-context}
  @see-symbol{gdk:gl-api}")

;;; --- gl-context-shared-context ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "shared-context" 'gl-context) t)
 "The @code{shared-context} property of type @class{gdk:gl-context}
  (Read / Write / Construct only) @br{}
  The GL context that this context is sharing data with, or @code{nil}.
  Deprecated since 4.4")

#+liber-documentation
(setf (liber:alias-for-function 'gl-context-shared-context)
      "Accessor"
      (documentation 'gl-context-shared-context 'function)
 "@version{#2023-8-4}
  @syntax{(gdk:gl-context-shared-context object) => context}
  @argument[object]{a @class{gdk:gl-context} object}
  @argument[context]{a @class{gdk:gl-context} object or @code{nil}}
  @begin{short}
    Accessor of the @slot[gdk:gl-context]{shared-context} slot of the
    @class{gdk:gl-context} class.
  @end{short}
  The @fun{gdk:gl-context-shared-context} function retrieves the GL context
  that this context share data with.

  Deprecated since 4.6
  @see-class{gdk:gl-context}")

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_display ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_gl_context_get_display" gl-context-display)
    (g:object display)
 #+liber-documentation
 "@version{#2023-8-3}
  @argument[context]{a @class{gdk:gl-context} object}
  @return{The @class{gdk:display} object or @code{nil}.}
  @begin{short}
    Retrieves the display the GL context is created for.
  @end{short}
  @see-class{gdk:gl-context}
  @see-class{gdk:display}"
  (context (g:object gl-context)))

(export 'gl-context-display)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_surface ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_gl_context_get_surface" gl-context-surface)
    (g:object surface)
 #+liber-documentation
 "@version{#2023-8-3}
  @argument[context]{a @class{gdk:gl-context} object}
  @return{The @class{gdk:surface} object or @code{nil}.}
  @begin{short}
    Retrieves the surface used by the GL context.
  @end{short}
  @see-class{gdk:gl-context}
  @see-class{gdk:surface}"
  (context (g:object gl-context)))

(export 'gl-context-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_version ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_gl_context_get_version" %gl-context-version) :void
  (context (g:object gl-context))
  (major (:pointer :int))
  (minor (:pointer :int)))

(defun gl-context-version (context)
 #+liber-documentation
 "@version{#2023-8-3}
  @argument[context]{a @class{gdk:gl-context} object}
  @begin{return}
    @arg{major} - an integer with the major version @br{}
    @arg{minor} - an integer with the minor version
  @end{return}
  @begin{short}
    Retrieves the OpenGL version of the GL context.
  @end{short}
  The GL context must be realized prior to calling this function.
  @see-class{gdk:gl-context}"
  (cffi:with-foreign-objects ((major :int) (minor :int))
    (%gl-context-version context major minor)
    (values (cffi:mem-ref major :int)
            (cffi:mem-ref minor :int))))

(export 'gl-context-version)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_required_version ()
;;; gdk_gl_context_get_required_version ()
;;; ----------------------------------------------------------------------------

(defun (setf gl-context-required-version) (value context)
  (destructuring-bind (major minor) value
    (cffi:foreign-funcall "gdk_gl_context_set_required_version"
                          (g:object gl-context) context
                          :int major
                          :int minor
                          :void)
    (values major minor)))

(cffi:defcfun ("gdk_gl_context_get_required_version"
               %gl-context-required-version) :void
  (context (g:object gl-context))
  (major (:pointer :int))
  (minor (:pointer :int)))

(defun gl-context-required-version (context)
 #+liber-documentation
 "@version{#2023-8-3}
  @syntax{(gdk:gl-context-required-version object) => major, minor}
  @syntax{(setf gdk:gl-context-required-version object) (list major minor))}
  @argument[context]{a @class{gdk:gl-context} object}
  @begin{return}
    @arg{major} - an integer with the major version to request @br{}
    @arg{minor} - an integer with the minor version to request
  @end{return}
  @begin{short}
    The @fun{gdk:gl-context-required-version} function retrieves the major and
    minor version of OpenGL to request.
  @end{short}
  The @setf{gdk:gl-context-required-version} function sets the major and minor
  version to request. Setting @arg{major} and @arg{minor} to zero will use the
  default values.

  The @class{gdk:gl-context} object must not be realized or made current prior
  to calling this function.
  @see-class{gdk:gl-context}"
  (cffi:with-foreign-objects ((major :int) (minor :int))
    (%gl-context-version context major minor)
    (values (cffi:mem-ref major :int)
            (cffi:mem-ref minor :int))))

(export 'gl-context-version)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_debug_enabled ()
;;; gdk_gl_context_get_debug_enabled ()
;;; ----------------------------------------------------------------------------

(defun (setf gl-context-debug-enabled) (enabled context)
  (cffi:foreign-funcall "gdk_gl_context_set_debug_enabled"
                        (g:object gl-context) context
                        :boolean enabled
                        :void)
  enabled)

(cffi:defcfun ("gdk_gl_context_get_debug_enabled"
               gl-context-debug-enabled) :boolean
 #+liber-documentation
 "@version{#2023-8-3}
  @syntax{(gdk:gl-context-debug-enabled object) => enabled}
  @syntax{(setf gdk:gl-context-debug-enabled object) enabled)}
  @argument[context]{a @class{gdk:gl-context} object}
  @argument[enabled]{a boolean whether debugging is enabled}
  @begin{short}
    The @fun{gdk:gl-context-debug-enabled} function retrieves whether debugging
    is enabled.
  @end{short}
  The @setf{gdk:gl-context-debug-enabled} function sets whether the GL context
  should perform extra validations and run time checking. This is useful during
  development, but has additional overhead.

  The @class{gdk:gl-context} object must not be realized or made current prior
  to calling this function.
  @see-class{gdk:gl-context}"
  (context (g:object gl-context)))

(export 'gl-context-debug-enabled)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_forward_compatible ()
;;; gdk_gl_context_get_forward_compatible ()
;;; ----------------------------------------------------------------------------

(defun (setf gl-context-forward-compatible) (setting context)
  (cffi:foreign-funcall "gdk_gl_context_set_forward_compatible"
                        (g:object gl-context) context
                        :boolean setting
                        :void)
  setting)

(cffi:defcfun ("gdk_gl_context_get_forward_compatible"
               gl-context-forward-compatible) :boolean
 #+liber-documentation
 "@version{#2023-8-3}
  @syntax{(gdk:gl-context-forward-compatible object) => setting}
  @syntax{(setf gdk:gl-context-forward-compatible object) setting)}
  @argument[context]{a @class{gdk:gl-context} object}
  @argument[setting]{a boolean whether @arg{context} is forward compatible}
  @begin{short}
    The @fun{gdk:gl-context-forward-compatible} function returns whether the
    GL context should be forward compatible.
  @end{short}
  The @setf{gdk:gl-context-forward-compatible} function sets whether the GL
  context should be forward compatible.

  Forward compatible GL contexts must not support OpenGL functionality that has
  been marked as deprecated in the requested version. Non-forward compatible
  GL contexts, on the other hand, must support both deprecated and non
  deprecated functionality.

  The @class{gdk:gl-context} object must not be realized or made current prior
  to calling this function.
  @see-class{gdk:gl-context}"
  (context (g:object gl-context)))

(export 'gl-context-forward-compatible)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_use_es ()
;;; gdk_gl_context_get_use_es ()
;;; ----------------------------------------------------------------------------

;; TODO: The return value is of type boolean, but an integer when setting.

(defun (setf gl-context-use-es) (value context)
  (cffi:foreign-funcall "gdk_gl_context_set_uses-es"
                        (g:object gl-context) context
                        :int value)
  value)

(cffi:defcfun ("gdk_gl_context_get_use_es" gl-context-use-es) :boolean
 #+liber-documentation
 "@version{#2023-8-3}
  @syntax{(gdk:gl-context-uses-es object) => setting}
  @syntax{(setf gdk:gl-context-use-es object) setting)}
  @argument[context]{a @class{gdk:gl-context} object}
  @argument[setting]{an integer whether the context uses OpenGL instead of
    OpenGL, or -1 to allow auto-detection}
  @begin{short}
    The @fun{gdk:gl-context-uses-es} function checks whether the context is
    using an OpenGL or OpenGL ES profile.
  @end{short}
  The @fun{gdk:gl-context-uses-es} function requests that GDK create an OpenGL
  ES context instead of an OpenGL one, if the platform and windowing system
  allows it. The context must not have been realized.

  By default, GDK will attempt to automatically detect whether the underlying
  GL implementation is OpenGL or OpenGL ES once the context is realized.

  You should check the return value of the @fun{gdk:gl-context-use-es} function
  after calling the @fun{gdk:gl-context-realize} function to decide whether to
  use the OpenGL or OpenGL ES API, extensions, or shaders.
  @see-class{gdk:gl-context}
  @see-function{gdk:gl-context-realize}"
  (context (g:object gl-context)))

(export 'gl-context-use-es)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_is_legacy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_gl_context_is_legacy" gl-context-is-legacy) :boolean
 #+liber-documentation
 "@version{#2023-8-3}
  @argument[context]{a @class{gdk:gl-context} object}
  @return{@em{True} if the GL context is in legacy mode.}
  @begin{short}
    Whether the GL context is in legacy mode or not.
  @end{short}
  The @class{gdk:gl-context} object must be realized before calling this
  function.

  When realizing a GL context, GDK will try to use the OpenGL 3.2 core profile.
  This profile removes all the OpenGL API that was deprecated prior to the 3.2
  version of the specification. If the realization is successful, this function
  will return @em{false}.

  If the underlying OpenGL implementation does not support core profiles, GDK
  will fall back to a pre-3.2 compatibility profile, and this function will
  return @em{true}.

  You can use the value returned by this function to decide which kind of
  OpenGL API to use, or whether to do extension discovery, or what kind of
  shader programs to load.
  @see-class{gdk:gl-context}"
  (context (g:object gl-context)))

(export 'gl-context-is-legacy)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_realize ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_gl_context_realize" %gl-context-realize) :boolean
  (context (g:object gl-context))
  (err :pointer))

(defun gl-context-realize (context)
 #+liber-documentation
 "@version{#2023-8-3}
  @argument[context]{a @class{gdk:gl-context} object}
  @return{@em{True} if the GL context is realized.}
  @begin{short}
    Realizes the given GL context.
  @end{short}
  It is safe to call this function on a realized GL context.
  @see-class{gdk:gl-context}"
  (glib:with-g-error (err)
    (%gl-context-realize context err)))

(export 'gl-context-realize)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_make_current ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_gl_context_make_current" gl-context-make-current) :void
 #+liber-documentation
 "@version{#2023-8-3}
  @argument[context]{a @class{gdk:gl-context} object}
  @begin{short}
    Makes the GL context the current one.
  @end{short}
  @see-class{gdk:gl-context}"
  (context (g:object gl-context)))

(export 'gl-context-make-current)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_current ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_gl_context_get_current" gl-context-current)
    (g:object gl-context)
 #+liber-documentation
 "@version{#2023-8-3}
  @return{The current @class{gdk:gl-context} object, or @code{nil}.}
  @begin{short}
    Retrieves the current GL context.
  @end{short}
  @see-class{gdk:gl-context}")

(export 'gl-context-current)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_clear_current ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_gl_context_clear_current" gl-context-clear-current) :void
 #+liber-documentation
 "@version{#2023-8-3}
  @begin{short}
    Clears the current GL context.
  @end{short}
  Any OpenGL call after this function returns will be ignored until the
  @fun{gdk:gl-context-make-current} function is called.
  @see-class{gdk:gl-context}
  @see-function{gdk:gl-context-make-current}")

(export 'gl-context-clear-current)

;;; --- End of file gdk4.gl-context.lisp ---------------------------------------
