;;; ----------------------------------------------------------------------------
;;; gsk4.renderer.lisp
;;;
;;; The documentation of this file is taken from the GSK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
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
;;; GskRenderer
;;;
;;;     Renders a scene
;;;
;;; Types and Values
;;;
;;;     GskRenderer
;;;     GskCairoRenderer
;;;     GskGLRenderer                                       Since 4.2
;;;     GskNGLRenderer                                      Since 4.2
;;;
;;; Accessors
;;;
;;;     gsk_renderer_get_surface
;;;
;;; Functions
;;;
;;;     gsk_renderer_new_for_surface
;;;
;;;     gsk_renderer_realize
;;;     gsk_renderer_unrealize
;;;     gsk_renderer_is_realized
;;;     gsk_renderer_render
;;;     gsk_renderer_render_texture
;;;
;;;     gsk_cairo_renderer_new
;;;     gsk_gl_renderer_new                                 Since 4.2
;;;     gsk_ngl_renderer_new                                Since 4.2
;;;
;;; Properties
;;;
;;;     realized
;;;     surface
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GskRenderer
;;; ----------------------------------------------------------------------------

(in-package :gsk)

;;; ----------------------------------------------------------------------------
;;; GskRenderer
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GskRenderer" renderer
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gsk_renderer_get_type")
  ((realized
    renderer-realized
    "realized" "gboolean" t nil)
   (surface
    renderer-surface
    "surface" "GdkSurface" t nil)))

#+liber-documentation
(setf (documentation 'renderer 'type)
 "@version{2024-11-7}
  @begin{short}
    The @class{gsk:renderer} class is a class that renders a scene graph
    defined by a tree of @class{gsk:render-node} instances.
  @end{short}
  Typically you will use a @class{gsk:renderer} instance to repeatedly call the
  @fun{gsk:renderer-render} function to update the contents of its associated
  @class{gdk:surface} object.

  It is necessary to realize a @class{gsk:renderer} instance using the
  @fun{gsk:renderer-realize} function before calling the
  @fun{gsk:renderer-render} function, in order to create the appropriate
  windowing system resources needed to render the scene.
  @see-constructor{gsk:renderer-new-for-surface}
  @see-slot{gsk:renderer-realized}
  @see-slot{gsk:renderer-surface}
  @see-class{gdk:surface}")

;;; ----------------------------------------------------------------------------
;;; Property Details
;;; ----------------------------------------------------------------------------

;;; --- gsk:renderer-realized --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "realized" 'renderer) t)
 "The @code{realized} property of type @code{:boolean} (Read) @br{}
  Whether the renderer has been associated with a surface or draw context. @br{}
  Default value : @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'renderer-realized)
      "Accessor"
      (documentation 'renderer-realized 'function)
 "@version{2024-11-7}
  @syntax{(gsk:renderer-realized object) => realized}
  @argument[object]{a @class{gsk:renderer} instance}
  @argument[realized]{a boolean whether the renderer has been associated with
    a surface or draw context}
  @begin{short}
    Accessor of the @slot[gsk:renderer]{realized} slot of the
    @class{gsk:renderer} class.
  @end{short}
  @see-class{gsk:renderer}")

;;; --- gsk:renderer-surface ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "surface" 'renderer) t)
 "The @code{surface} property of type @class{gdk:surface} (Read) @br{}
  The surface associated with the renderer.")

#+liber-documentation
(setf (liber:alias-for-function 'renderer-surface)
      "Accessor"
      (documentation 'renderer-surface 'function)
 "@version{2024-11-7}
  @syntax{(gsk:renderer-surface object) => surface}
  @argument[object]{a @class{gsk:renderer} instance}
  @argument[surface]{a @class{gdk:surface} object}
  @begin{short}
    Accessor of the @slot[gsk:renderer]{surface} slot of the
    @class{gsk:renderer} class.
  @end{short}
  The @fun{gsk:renderer-surface} function retrieves the @class{gdk:surface}
  object set using the @fun{gsk:renderer-realize} function. If the renderer has
  not been realized yet, @code{nil} will be returned.
  @see-class{gsk:renderer}
  @see-class{gdk:surface}")

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_new_for_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_new_for_surface" renderer-new-for-surface)
    (g:object renderer :already-referenced)
 #+liber-documentation
 "@version{2024-11-7}
  @argument[surface]{a @class{gdk:surface} object}
  @return{The new @class{gsk:renderer} instance.}
  @begin{short}
    Creates an appropriate @class{gsk:renderer} instance for the given
    @arg{surface}.
  @end{short}
  If the @code{GSK_RENDERER} environment variable is set, GSK will try that
  renderer first, before trying the backend-specific default. The ultimate
  fallback is the Cairo renderer.

  The renderer will be realized before it is returned.
  @see-class{gsk:renderer}
  @see-class{gdk:surface}"
  (surface (g:object gdk:surface)))

(export 'renderer-new-for-surface)

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_realize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_realize" %renderer-realize) :boolean
  (renderer (g:object renderer))
  (surface (g:object gdk:surface))
  (err :pointer))

(defun renderer-realize (renderer surface)
 #+liber-documentation
 "@version{#2024-11-7}
  @argument[renderer]{a @class{gsk:renderer} instance}
  @argument[surface]{a @class{gdk:surface} object}
  @begin{short}
    Creates the resources needed by the renderer to render the scene graph.
  @end{short}
  @see-class{gsk:renderer}
  @see-class{gdk:surface}"
  (glib:with-g-error (err)
    (%renderer-realize renderer surface err)))

(export 'renderer-realize)

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_unrealize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_unrealize" renderer-unrealize) :void
 #+liber-documentation
 "@version{#2024-11-7}
  @argument[renderer]{a @class{gsk:renderer} instance}
  @begin{short}
    Releases all the resources created by the @fun{gsk:renderer-realize}
    function.
  @end{short}
  @see-class{gsk:renderer}
  @see-function{gsk:renderer-realize}"
  (renderer (g:object renderer)))

(export 'renderer-unrealize)

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_is_realized
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_is_realized" renderer-is-realized) :boolean
 #+liber-documentation
 "@version{2024-11-7}
  @argument[renderer]{a @class{gsk:renderer} instance}
  @return{@em{True} if @arg{renderer} was realized, and @em{false} otherwise.}
  @short{Checks whether the renderer is realized or not.}
  @see-class{gsk:renderer}"
  (renderer (g:object renderer)))

(export 'renderer-is-realized)

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_render
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_render" %renderer-render) :void
  (renderer (g:object renderer))
  (root (g:object render-node))
  (region (:pointer (:struct cairo:region-t))))

(defun renderer-render (renderer root &optional region)
 #+liber-documentation
 "@version{#2024-11-7}
  @argument[renderer]{a @class{gsk:renderer} instance}
  @argument[root]{a @class{gsk:render-node} instance}
  @argument[region]{an optional @symbol{cairo:context-t} instance with the
    region that must be redrawn or the default @code{nil} value for the whole
    window}
  @begin{short}
    Renders the scene graph, described by a tree of @class{gsk:render-node}
    instances, ensuring that the given region gets redrawn.
  @end{short}
  Renderers must ensure that changes of the contents given by the root node as
  well as the area given by @arg{region} are redrawn. They are however free to
  not redraw any pixel outside of region if they can guarantee that it did not
  change.

  The renderer will acquire a reference on the @class{gsk:render-node} tree
  while the rendering is in progress.
  @see-class{gsk:renderer}
  @see-class{gsk:render-node}
  @see-symbol{cairo:region-t}"
  (let ((region (or region (cffi:null-pointer))))
    (%renderer-render renderer root region)))

(export 'renderer-render)

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_render_texture
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_render_texture" %renderer-render-texture)
    (g:object gdk:texture)
  (renderer (g:object renderer))
  (root (g:object render-node))
  (viewport (:pointer (:struct graphene:rect-t))))

(defun renderer-render-texture (renderer root &optional viewport)
 #+liber-documentation
 "@version{#2024-11-7}
  @argument[renderer]{a @class{gsk:renderer} instance}
  @argument[root]{a @class{gsk:render-node} instance}
  @argument[viewport]{an optional @symbol{graphene:rect-t} instance with the
    section to draw or the default @code{nil} value to use the bounds of
    @arg{root}}
  @return{The @class{gdk:texture} instance with the rendered contents of
    @arg{root}.}
  @begin{short}
    Renders the scene graph, described by a tree of @class{gsk:render-node}
    instances, to a @class{gdk:texture} instance.
  @end{short}
  The renderer will acquire a reference on the @class{gsk:render-node} tree
  while the rendering is in progress.

  If you want to apply any transformations to @arg{root}, you should put it
  into a transform node and pass that node instead.
  @see-class{gsk:renderer}
  @see-class{gsk:render-node}
  @see-class{gdk:texture}
  @see-symbol{graphene:rect-t}"
  (let ((viewport (or viewport (cffi:null-pointer))))
    (%renderer-render-texture renderer root viewport)))

(export 'renderer-render-texture)

;;; ----------------------------------------------------------------------------
;;; GskCairoRenderer
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GskCairoRenderer" cairo-renderer
  (:superclass renderer
   :export t
   :interfaces ()
   :type-initializer "gsk_cairo_renderer_get_type")
  nil)

#+liber-documentation
(setf (documentation 'cairo-renderer 'type)
 "@version{2024-11-7}
  @begin{short}
    The GSK renderer that is using Cairo.
  @end{short}
  Since the renderer is using Cairo, this renderer cannot support 3D
  transformations.
  @see-constructor{gsk:cairo-renderer-new}
  @see-class{gsk:renderer}")

;;; ----------------------------------------------------------------------------
;;; gsk_cairo_renderer_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cairo_renderer_new" cairo-renderer-new)
    (g:object renderer :already-referenced)
 #+liber-documentation
 "@version{2024-11-7}
  @return{The new Cairo @class{gsk:renderer} instance.}
  @begin{short}
    Creates a new @class{gsk:renderer} instance using Cairo.
  @end{short}
  The Cairo renderer is the fallback renderer drawing in ways similar to how
  GTK 3 drew its content. Its primary use is as comparison tool.

  The Cairo renderer is incomplete. It cannot render 3D transformed content and
  will instead render an error marker. Its usage should be avoided.
  @see-class{gsk:renderer}")

(export 'cairo-renderer-new)

;;; ----------------------------------------------------------------------------
;;; GskGLRenderer                                           Since 4.2
;;; ----------------------------------------------------------------------------

;; TODO: Implements undocumented GdkDmabufDownloader interface

#+gtk-4-2
(gobject:define-gobject "GskGLRenderer" gl-renderer
  (:superclass renderer
   :export t
   :interfaces ()
   :type-initializer "gsk_gl_renderer_get_type")
  nil)

#+(and gtk-4-2 liber-documentation)
(setf (documentation 'gl-renderer 'type)
 "@version{2024-11-7}
  @begin{short}
    The OpenGL based renderer.
  @end{short}
  See the @class{gsk:renderer} documentation.

  Since 4.2
  @see-constructor{gsk:gl-renderer-new}
  @see-class{gsk:renderer}")

;;; ----------------------------------------------------------------------------
;;; gsk_gl_renderer_new                                     Since 4.2
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_gl_renderer_new" gl-renderer-new)
    (g:object renderer :already-referenced)
 #+liber-documentation
 "@version{2024-11-7}
  @return{The new OpenGL @class{gsk:renderer} instance.}
  @begin{short}
    Creates a new @class{gsk:renderer} instance using OpenGL.
  @end{short}
  This is the default renderer used by GTK.
  @see-class{gsk:gl-renderer}
  @see-class{gsk:renderer}")

(export 'gl-renderer-new)

;;; ----------------------------------------------------------------------------
;;; GskNGLRenderer                                          Since 4.2
;;; ----------------------------------------------------------------------------

;; TODO: Implements undocumented GdkDmabufDownloader interface

#+gtk-4-2
(gobject:define-gobject "GskNglRenderer" ngl-renderer
  (:superclass renderer
   :export t
   :interfaces ()
   :type-initializer "gsk_ngl_renderer_get_type")
  nil)

#+(and gtk-4-2 liber-documentation)
(setf (documentation 'ngl-renderer 'type)
 "@version{2024-11-7}
  @begin{short}
    The new experimental OpenGL based renderer.
  @end{short}
  See the @class{gsk:renderer} documentation.

  Since 4.2
  @see-constructor{gsk:ngl-renderer-new}
  @see-class{gsk:renderer}")

;;; ----------------------------------------------------------------------------
;;; gsk_ngl_renderer_new                                    Since 4.2
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_ngl_renderer_new" ngl-renderer-new)
    (g:object renderer :already-referenced)
 #+liber-documentation
 "@version{2024-11-7}
  @return{The new experimental OpenGL @class{gsk:renderer} instance.}
  @begin{short}
    Creates an instance of the new experimental OpenGL renderer.
  @end{short}
  @see-class{gsk:ngl-renderer}
  @see-class{gsk:renderer}")

(export 'ngl-renderer-new)

;;; --- End of file gsk4.renderer.lisp -----------------------------------------
