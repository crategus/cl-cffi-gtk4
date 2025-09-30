;;; ----------------------------------------------------------------------------
;;; gtk4.gl-area.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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
;;; GtkGLArea
;;;
;;;     A widget for custom drawing with OpenGL
;;;
;;; Types and Values
;;;
;;;     GtkGLArea
;;;
;;; Accessors
;;;
;;;     gtk_gl_area_get_allowed_apis                        Since 4.12
;;;     gtk_gl_area_set_allowed_apis                        Since 4.12
;;;     gtk_gl_area_get_api                                 Since 4.12
;;;     gtk_gl_area_get_auto_render
;;;     gtk_gl_area_set_auto_render
;;;     gtk_gl_area_get_context
;;;     gtk_gl_area_get_has_depth_buffer
;;;     gtk_gl_area_set_has_depth_buffer
;;;     gtk_gl_area_get_has_stencil_buffer
;;;     gtk_gl_area_set_has_stencil_buffer
;;;     gtk_gl_area_get_use_es                              Deprecated 4.12
;;;     gtk_gl_area_set_use_es                              Deprecated 4.12
;;;
;;; Functions
;;;
;;;     gtk_gl_area_new
;;;     gtk_gl_area_make_current
;;;     gtk_gl_area_queue_render
;;;     gtk_gl_area_attach_buffers
;;;     gtk_gl_area_set_error
;;;     gtk_gl_area_get_error
;;;     gtk_gl_area_get_required_version
;;;     gtk_gl_area_set_required_version
;;;
;;; Properties
;;;
;;;     allowed-apis                                        Since 4.12
;;;     api                                                 Since 4.12
;;;     auto-render
;;;     context
;;;     has-depth-buffer
;;;     has-stencil-buffer
;;;     use-es                                              Deprecated 4.12
;;;
;;; Signals
;;;
;;;     create-context
;;;     render
;;;     resize
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkGLArea
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkGLArea
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkGLArea" gl-area
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_gl_area_get_type")
  (#+gtk-4-12
   (allowed-apis
    gl-area-allowed-apis
    "allowed-apis" "GdkGLAPI" t t)
   #+gtk-4-12
   (api
    gl-area-api
    "api" "GdkGLAPI" t nil)
   (auto-render
    gl-area-auto-render
    "auto-render" "gboolean" t t)
   (context
    gl-area-context
    "context" "GdkGlContext" t nil)
   (has-depth-buffer
    gl-area-has-depth-buffer
    "has-depth-buffer" "gboolean" t t)
   (has-stencil-buffer
    gl-area-has-stencil-buffer
    "has-stencil-buffer" "gboolean" t t)
   (use-es
    gl-area-use-es
    "use-es" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'gl-area 'type)
 "@version{2025-07-24}
  @begin{short}
    The @class{gtk:gl-area} widget is a widget that allows drawing with OpenGL.
  @end{short}

  @image[gl-area]{Figure: GtkGLArea}

  The @class{gtk:gl-area} widget sets up its own @class{gdk:gl-context} object
  for the window it creates, and creates a custom GL framebuffer that the widget
  will do GL rendering onto. It also ensures that this framebuffer is the
  default GL rendering target when rendering.

  In order to draw, you have to connect to the @sig[gtk:gl-area]{render} signal,
  or subclass the @class{gtk:gl-area} widget and override the @code{render()}
  virtual function.

  The @class{gtk:gl-area} widget ensures that the @class{gdk:gl-context} object
  is associated with the drawing area of the widget, and it is kept updated when
  the size and position of the drawing area changes.

  @subheading{Drawing with GtkGLArea}
  The simplest way to draw using OpenGL commands in a @class{gtk:gl-area} widget
  is to create a widget instance and connect to the @sig[gtk:gl-area]{render}
  signal.

  The @code{render()} function will be called when the @class{gtk:gl-area}
  widget is ready for you to draw its content:
  @begin{pre}
static gboolean
render (GtkGLArea *area, GdkGLContext *context)
{
  // inside this function it's safe to use GL; the given
  // GdkGLContext has been made current to the drawable
  // surface used by the `GtkGLArea` and the viewport has
  // already been set to be the size of the allocation

  // we can start by clearing the buffer
  glClearColor (0, 0, 0, 0);
  glClear (GL_COLOR_BUFFER_BIT);

  // draw your object
  // draw_an_object ();

  // we completed our drawing; the draw commands will be
  // flushed at the end of the signal emission chain, and
  // the buffers will be drawn on the window
  return TRUE;
@}

void setup_glarea (void)
{
  // create a GtkGLArea instance
  GtkWidget *gl_area = gtk_gl_area_new ();

  // connect to the \"render\" signal
  g_signal_connect (gl_area, \"render\", G_CALLBACK (render), NULL);
@}
  @end{pre}
  If you need to initialize OpenGL state, for example buffer objects or shaders,
  you should use the @sig[gtk:widget]{realize} signal. You can use the
  @sig[gtk:widget]{unrealize} signal to clean up. Since the
  @class{gdk:gl-context} object creation and initialization may fail, you will
  need to check for errors, using the @fun{gtk:gl-area-error} function. An
  example of how to safely initialize the GL state is:
  @begin{pre}
static void
on_realize (GtkGLarea *area)
{
  // We need to make the context current if we want to
  // call GL API
  gtk_gl_area_make_current (area);

  // If there were errors during the initialization or
  // when trying to make the context current, this
  // function will return a GError for you to catch
  if (gtk_gl_area_get_error (area) != NULL)
    return;

  // You can also use gtk_gl_area_set_error() in order
  // to show eventual initialization errors on the
  // GtkGLArea widget itself
  GError *internal_error = NULL;
  init_buffer_objects (&error);
  if (error != NULL)
    {
      gtk_gl_area_set_error (area, error);
      g_error_free (error);
      return;
    @}

  init_shaders (&error);
  if (error != NULL)
    {
      gtk_gl_area_set_error (area, error);
      g_error_free (error);
      return;
    @}
@}
  @end{pre}
  If you need to change the options for creating the @class{gdk:gl-context}
  object you should use the @sig[gtk:gl-area]{create-context} signal.
  @begin[Signal Details]{dictionary}
    @begin[gl-area::create-context]{signal}
      @begin{pre}
lambda (area)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[area]{The @class{gtk:gl-area} widget that emitted the signal.}
        @entry[Returns]{The newly created @class{gdk:gl-context} object. The
          @class{gtk:gl-area} widget will take ownership of the returned value.}
      @end{simple-table}
      The signal is emitted when the widget is being realized, and allows you to
      override how the GL context is created. This is useful when you want to
      reuse an existing GL context, or if you want to try creating different
      kinds of GL options. If context creation fails then the signal handler can
      use the @fun{gtk:gl-area-error} function to register a more detailed error
      of how the construction failed.
    @end{signal}
    @begin[gl-area::render]{signal}
      @begin{pre}
lambda (area context)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[area]{The @class{gtk:gl-area} widget that emitted the signal.}
        @entry[context]{The @class{gdk:gl-context} object used by @arg{area}.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event. @em{False} to propagate the event further.}
      @end{simple-table}
      The signal is emitted every time the contents of the @class{gtk:gl-area}
      widget should be redrawn. The context is bound to the area prior to
      emitting this function, and the buffers are painted to the window once the
      emission terminates.
    @end{signal}
    @begin[gl-area::resize]{signal}
      @begin{pre}
lambda (area width height)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[area]{The @class{gtk:gl-area} widget that emitted the signal.}
        @entry[width]{The integer for the width of the viewport.}
        @entry[height]{The integer for the height of the viewport.}
      @end{simple-table}
      The signal is emitted once when the widget is realized, and then each time
      the widget is changed while realized. This is useful in order to keep GL
      state up to date with the widget size, like for instance camera properties
      which may depend on the width/height ratio. The GL context for the area is
      guaranteed to be current when this signal is emitted. The default handler
      sets up the GL viewport.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:gl-area-new}
  @see-slot{gtk:gl-area-allowed-apis}
  @see-slot{gtk:gl-area-api}
  @see-slot{gtk:gl-area-auto-render}
  @see-slot{gtk:gl-area-context}
  @see-slot{gtk:gl-area-has-depth-buffer}
  @see-slot{gtk:gl-area-has-stencil-buffer}
  @see-slot{gtk:gl-area-use-es}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:gl-area-allowed-apis -----------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "allowed-apis" 'gl-area) t)
 "The @code{allowed-apis} property of type @class{gdk:gl-api} (Read / Write)
  @br{}
  The allowed APIs. Since 4.12 @br{}
  Default value: @code{'(:gl :gles)}")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'gl-area-allowed-apis)
      "Accessor"
      (documentation 'gl-area-allowed-apis 'function)
 "@version{2025-09-29}
  @syntax{(gtk:gl-area-allowed-apis object) => apis}
  @syntax{(setf (gtk:gl-area-allowed-apis object) apis)}
  @argument[object]{a @class{gtk:gl-area} widget}
  @argument[apis]{a @sym{gdk:gl-api} value}
  @begin{short}
    The accessor for the @slot[gtk:gl-area]{allowed-apis} slot of the
    @class{gtk:gl-area} class gets or sets the allowed APIs to create a context
    with.
  @end{short}
  You should check the @slot[gtk:gl-area]{api} property before drawing with
  either API. By default, all APIs are allowed.

  Since 4.12
  @see-class{gtk:gl-area}
  @see-symbol{gdk:gl-api}
  @see-function{gtk:gl-area-api}")

;;; --- gtk:gl-area-api --------------------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "api" 'gl-area) t)
 "The @code{api} property of type @class{gdk:gl-api} (Read) @br{}
  The API currently in use. Since 4.12 @br{}
  Default value: @code{'()}")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'gl-area-api)
      "Accessor"
      (documentation 'gl-area-api 'function)
 "@version{2025-09-29}
  @syntax{(gtk:gl-area-api object) => api}
  @argument[object]{a @class{gtk:gl-area} widget}
  @argument[api]{a @sym{gdk:gl-api} value}
  @begin{short}
    The accessor for the @slot[gtk:gl-area]{api} slot of the @class{gtk:gl-area}
    class returns the API is currently in use.
  @end{short}
  If the GL area has not been realized yet, @code{'()} is returned.

  Since 4.12
  @see-class{gtk:gl-area}
  @see-symbol{gdk:gl-api}
  @see-function{gtk:gl-area-api}")

;;; --- gtk:gl-area-auto-render ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "auto-render" 'gl-area) t)
 "The @code{auto-render} property of type @code{:boolean} (Read / Write) @br{}
  If set to @em{true} the @sig[gtk:gl-area]{render} signal will be emitted every
  time the widget draws. This is the default and is useful if drawing the widget
  is faster. If set to @em{false} the data from previous rendering is kept
  around and will be used for drawing the widget the next time, unless the
  window is resized. In order to force a rendering the
  @fun{gtk:gl-area-queue-render} function must be called. This mode is useful
  when the scene changes seldom, but takes a long time to redraw. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'gl-area-auto-render)
      "Accessor"
      (documentation 'gl-area-auto-render 'function)
 "@version{2025-09-29}
  @syntax{(gtk:gl-area-auto-render object) => setting}
  @syntax{(setf (gtk:gl-area-auto-render object) setting)}
  @argument[object]{a @class{gtk:gl-area} widget}
  @argument[setting]{a boolean whether the area is auto rendering}
  @begin{short}
    The accessor for the @slot[gtk:gl-area]{auto-render} slot of the
    @class{gtk:gl-area} class gets or sets whether the area is in auto render
    mode or not.
  @end{short}

  If @arg{setting} is @em{true} the @sig[gtk:gl-area]{render} signal will be
  emitted every time the widget draws. This is the default and is useful if
  drawing the widget is faster. If @arg{setting} is @em{false} the data from
  previous rendering is kept around and will be used for drawing the widget the
  next time, unless the window is resized. In order to force a rendering the
  @fun{gtk:gl-area-queue-render} function must be called. This mode is useful
  when the scene changes seldom, but takes a long time to redraw.
  @see-class{gtk:gl-area}
  @see-function{gtk:gl-area-queue-render}")

;;; --- gtk:gl-area-context ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "context" 'gl-area) t)
 "The @code{context} property of type @class{gdk:gl-context} (Read) @br{}
  The @class{gdk:gl-context} object used by the @fun{gtk:gl-area} widget. The
  @class{gtk:gl-area} widget is responsible for creating the
  @class{gdk:gl-context} instance. If you need to render with other kinds of
  buffers (stencil, depth, and so on), use render buffers.")

#+liber-documentation
(setf (liber:alias-for-function 'gl-area-context)
      "Accessor"
      (documentation 'gl-area-context 'function)
 "@version{2025-09-29}
  @syntax{(gtk:gl-area-context object) => context}
  @syntax{(setf (gtk:gl-area-context object) context)}
  @argument[object]{a @class{gtk:gl-area} widget}
  @argument[context]{a @class{gdk:gl-context} object}
  @begin{short}
    The accessor for the @slot[gtk:gl-area]{context} slot of the
    @class{gtk:gl-area} class returns the @class{gdk:gl-context} instance used
    by @arg{object}.
  @end{short}
  @see-class{gtk:gl-area}
  @see-class{gdk:gl-context}")

;;; --- gtk:gl-area-has-depth-buffer -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-depth-buffer" 'gl-area) t)
 "The @code{has-depth-buffer} property of type @code{:boolean} (Read / Write)
  @br{}
  If set to @em{true} the widget will allocate and enable a depth buffer for the
  target framebuffer. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'gl-area-has-depth-buffer)
      "Accessor"
      (documentation 'gl-area-has-depth-buffer 'function)
 "@version{2025-09-29}
  @syntax{(gtk:gl-area-has-depth-buffer object) => setting}
  @syntax{(setf (gtk:gl-area-has-depth-buffer object) setting)}
  @argument[object]{a @class{gtk:gl-area} widget}
  @argument[setting]{a boolean whether the widget will allocate and enable a
    depth buffer}
  @begin{short}
    The accessor for the @slot[gtk:gl-area]{has-depth-buffer} slot of the
    @class{gtk:gl-area} class gets or sets whether the area has a depth buffer.
  @end{short}

  If @arg{setting} is @em{true} the widget will allocate and enable a depth
  buffer for the target framebuffer. Otherwise there will be none.
  @see-class{gtk:gl-area}")

;;; --- gtk:gl-area-has-stencil-buffer -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-stencil-buffer" 'gl-area) t)
 "The @code{has-stencil-buffer} property of type @code{:boolean} (Read / Write)
  @br{}
  If set to @em{true} the widget will allocate and enable a stencil buffer for
  the target framebuffer. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'gl-area-has-stencil-buffer)
      "Accessor"
      (documentation 'gl-area-has-stencil-buffer 'function)
 "@version{2025-09-29}
  @syntax{(gtk:gl-area-has-stencil-buffer object) => setting}
  @syntax{(setf (gtk:gl-area-has-stencil-buffer object) setting)}
  @argument[object]{a @class{gtk:gl-area} widget}
  @argument[setting]{a boolean whether the widget will allocate and enable a
    stencil buffer}
  @begin{short}
    The accessor for the @slot[gtk:gl-area]{has-stencil-buffer} slot of the
    @class{gtk:gl-area} class gets or sets whether the area has a stencil
    buffer.
  @end{short}

  If @arg{setting} is @em{true} the widget will allocate and enable a stencil
  buffer for the target framebuffer. Otherwise there will be none.
  @see-class{gtk:gl-area}")

;;; --- gtk:gl-area-use-es -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-es" 'gl-area) t)
 "The @code{use-es} property of type @code{:boolean} (Read / Write) @br{}
  If set to @em{true} the widget will try to create a @class{gdk:gl-context}
  instance using OpenGL ES instead of OpenGL. Deprecated since 4.12 @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'gl-area-use-es)
      "Accessor"
      (documentation 'gl-area-use-es 'function)
 "@version{2025-09-29}
  @syntax{(gtk:gl-area-use-es object) => setting}
  @syntax{(setf (gtk:gl-area-use-es object) setting)}
  @argument[object]{a @class{gtk:gl-area} widget}
  @argument[setting]{a boolean whether the widget should create an OpenGL ES
    context}
  @begin{short}
    The accessor for the @slot[gtk:gl-area]{use-es} slot of the
    @class{gtk:gl-area} class gets or sets whether the area should create an
    OpenGL or an OpenGL ES context.
  @end{short}
  You should check the capabilities of the @class{gdk:gl-context} instance
  before drawing with either API.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.12. Use the @fun{gtk:gl-area-api}
    function.
  @end{dictionary}
  @see-class{gtk:gl-area}
  @see-class{gdk:gl-context}")

;;; ----------------------------------------------------------------------------
;;; gtk_gl_area_new
;;; ----------------------------------------------------------------------------

(declaim (inline gl-area-new))

(defun gl-area-new ()
 #+liber-documentation
 "@version{#2024-10-26}
  @return{The new @class{gtk:gl-area} widget.}
  @short{Creates a new @class{gtk:gl-area} widget.}
  @see-class{gtk:gl-area}"
  (make-instance 'gl-area))

(export 'gl-area-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gl_area_make_current
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gl_area_make_current" gl-area-make-current) :void
 #+liber-documentation
 "@version{#2023-10-21}
  @argument[area]{a @class{gtk:gl-area} object}
  @begin{short}
    Ensures that the @class{gdk:gl-context} object used by @arg{area} is
    associated with the the @class{gtk:gl-area} object.
  @end{short}
  This function is automatically called before emitting the @code{\"render\"}
  signal, and does not normally need to be called by application code.
  @see-class{gtk:gl-area}"
  (area (g:object gl-area)))

(export 'gl-area-make-current)

;;; ----------------------------------------------------------------------------
;;; gtk_gl_area_queue_render
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gl_area_queue_render" gl-area-queue-render) :void
 #+liber-documentation
 "@version{#2025-07-24}
  @argument[area]{a @class{gtk:gl-area} object}
  @begin{short}
    Marks the currently rendered data (if any) as invalid, and queues a redraw
    of the widget, ensuring that the @sig[gl-area]{render} signal is emitted
    during the draw.
  @end{short}
  This is only needed when the @fun{gtk:gl-area-auto-render} function has been
  called with a @em{false} value. The default behaviour is to emit the
  @sig[gl-area]{render} signal on each draw.
  @see-class{gtk:gl-area}
  @see-function{gtk:gl-area-auto-render}"
  (area (g:object gl-area)))

(export 'gl-area-queue-render)

;;; ----------------------------------------------------------------------------
;;; gtk_gl_area_attach_buffers
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gl_area_attach_buffers" gl-area-attach-buffers) :void
 #+liber-documentation
 "@version{#2024-10-26}
  @argument[area]{a @class{gtk:gl-area} object}
  @begin{short}
    Ensures that the area framebuffer object is made the current draw and read
    target, and that all the required buffers for the area are created and bound
    to the framebuffer.
  @end{short}
  This function is automatically called before emitting the @code{\"render\"}
  signal, and does not normally need to be called by application code.
  @see-class{gtk:gl-area}"
  (area (g:object gl-area)))

(export 'gl-area-attach-buffers)

;;; ----------------------------------------------------------------------------
;;; gtk_gl_area_set_error ()
;;;
;;; void
;;; gtk_gl_area_set_error (GtkGLArea *area,
;;;                        const GError *error);
;;;
;;; Sets an error on the area which will be shown instead of the GL rendering.
;;; This is useful in the “create-context” signal if GL context creation fails.
;;;
;;; area :
;;;     a GtkGLArea
;;;
;;; error :
;;;     a new GError, or NULL to unset the error.
;;; ----------------------------------------------------------------------------

#+nil
(defun (setf gl-area-error) (value area)
  (cffi:foreign-funcall "gtk_gl_area_set_error"
                        (g:object gl-area) area
                        (g:boxed glib:error) value
                        :void)
  value)

;;; ----------------------------------------------------------------------------
;;; gtk_gl_area_get_error ()
;;;
;;; GError *
;;; gtk_gl_area_get_error (GtkGLArea *area);
;;;
;;; Gets the current error set on the area .
;;;
;;; area :
;;;     a GtkGLArea
;;;
;;; Returns :
;;;     the GError or NULL.
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("gtk_gl_area_get_error" gl-area-error) (g:boxed glib:error)
  (area (g:object gl-area)))

#+nil
(export 'gl-area-error)

;;; ----------------------------------------------------------------------------
;;; gtk_gl_area_get_required_version
;;; gtk_gl_area_set_required_version
;;; ----------------------------------------------------------------------------

(defun (setf gl-area-required-version) (value area)
  (destructuring-bind (major minor) value
    (cffi:foreign-funcall "gtk_gl_area_set_required_version"
                          (g:object gl-area) area
                          :int major
                          :int minor)
    (values major minor)))

(cffi:defcfun ("gtk_gl_area_get_required_version" %gl-area-required-version)
    :void
  (area (g:object gl-area))
  (major (:pointer :int))
  (minor (:pointer :int)))

(defun gl-area-required-version (area)
 #+liber-documentation
 "@version{#2025-07-27}
  @syntax{(gtk:gl-area-required-version area) => major, minor}
  @syntax{(setf (gtk:gl-area-required-version area) '(major minor))}
  @argument[area]{a @class{gtk:gl-area} object}
  @argument[major]{an integer for the required major version}
  @argument[minor]{an integer for the required minor version}
  @begin{short}
    The @fun{gtk:gl-area-required-version} function retrieves the required
    version of OpenGL.
  @end{short}
  The @setf{gtk:gl-area-required-version} function sets the required version of
  OpenGL to be used when creating the context for the widget. This function must
  be called before the area has been realized.
  @see-class{gtk:gl-area}"
  (cffi:with-foreign-objects ((major :int) (minor :int))
    (%gl-area-required-version area major minor)
    (values (cffi:mem-ref major :int) (cffi:mem-ref minor :int))))

(export 'gl-area-required-version)

;;; --- End of file gtk4.gl-area.lisp ------------------------------------------
