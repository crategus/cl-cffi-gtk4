;;; ----------------------------------------------------------------------------
;;; gsk4.renderer.lisp
;;;
;;; The documentation in this file is taken from the GSK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2026 Dieter Kaiser
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
;;;     GskVulkanRenderer
;;;     GskGLRenderer                                       Since 4.2
;;;     GskNglRenderer                                      Since 4.2
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
;;;     gsk_renderer_realize_for_display                    Since 4.14
;;;     gsk_renderer_unrealize
;;;     gsk_renderer_is_realized
;;;     gsk_renderer_render
;;;     gsk_renderer_render_texture
;;;
;;;     gsk_cairo_renderer_new
;;;     gsk_vulkan_renderer_new
;;;     gsk_gl_renderer_new                                 Since 4.2
;;;     gsk_ngl_renderer_new                                Deprecated 4.18
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
 "@version{2026-02-08}
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
 "@version{2026-02-08}
  @syntax{(gsk:renderer-realized object) => realized}
  @argument[object]{a @class{gsk:renderer} instance}
  @argument[realized]{a boolean whether the renderer has been associated with
    a surface or draw context}
  @begin{short}
    The accessor for the @slot[gsk:renderer]{realized} slot of the
    @class{gsk:renderer} class gets whether the renderer has been associated
    with a surface or draw context.
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
 "@version{2026-02-11}
  @syntax{(gsk:renderer-surface object) => surface}
  @argument[object]{a @class{gsk:renderer} instance}
  @argument[surface]{a @class{gdk:surface} object}
  @begin{short}
    The accessor for the @slot[gsk:renderer]{surface} slot of the
    @class{gsk:renderer} class retrieves the @class{gdk:surface} object set
    using the @fun{gsk:renderer-realize} function.
  @end{short}
  If the renderer has not been realized yet, @code{nil} will be returned.
  @see-class{gsk:renderer}
  @see-class{gdk:surface}
  @see-function{gsk:renderer-realize}")

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_new_for_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_new_for_surface" renderer-new-for-surface)
    (g:object renderer :return)
 #+liber-documentation
 "@version{2026-02-08}
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
 "@version{2026-02-08}
  @argument[renderer]{a @class{gsk:renderer} instance}
  @argument[surface]{a @class{gdk:surface} object}
  @return{The boolean whether the renderer was successfully realized.}
  @begin{short}
    Creates the resources needed by the renderer to render the scene graph.
  @end{short}

  Since GTK 4.6, the surface may be @code{nil}, which allows using renderers
  without having to create a surface. Since GTK 4.14, it is recommended to use
  the @fun{gsk:renderer-realize-for-display} function for this case.

  Note that it is mandatory to call the @fun{gsk:renderer-unrealize} function
  before destroying the renderer.
  @see-class{gsk:renderer}
  @see-class{gdk:surface}
  @see-function{gsk:renderer-realize-for-display}
  @see-function{gsk:renderer-unrealize}"
  (let ((surface (or surface (cffi:null-pointer))))
    (glib:with-error (err)
      (%renderer-realize renderer surface err))))

(export 'renderer-realize)

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_realize_for_display                        Since 4.14
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gsk_renderer_realize_for_display" %renderer-realize-for-display)
    :boolean
  (renderer (g:object renderer))
  (display (g:object gdk:display))
  (err :pointer))

#+gtk-4-14
(defun renderer-realize-for-display (renderer display)
 #+liber-documentation
 "@version{2026-02-08}
  @argument[renderer]{a @class{gsk:renderer} instance}
  @argument[display]{a @class{gdk:display} object that the renderer will be
    used on}
  @return{The boolean whether the renderer was successfully realized.}
  @begin{short}
    Creates the resources needed by the renderer.
  @end{short}
  Note that it is mandatory to call the @fun{gsk:renderer-unrealize} function
  before destroying the renderer.

  Since 4.14
  @see-class{gsk:renderer}
  @see-class{gdk:display}
  @see-function{gsk:renderer-realize}
  @see-function{gsk:renderer-unrealize}"
  (glib:with-error (err)
    (%renderer-realize-for-display renderer display err)))

#+gtk-4-14
(export 'renderer-realize-for-display)

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_unrealize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_unrealize" renderer-unrealize) :void
 #+liber-documentation
 "@version{2026-02-08}
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
 "@version{2026-02-08}
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
  (node (g:object render-node))
  (region (:pointer (:struct cairo:region-t))))

(defun renderer-render (renderer node &optional region)
 #+liber-documentation
 "@version{#2026-02-11}
  @argument[renderer]{a @class{gsk:renderer} instance}
  @argument[node]{a @class{gsk:render-node} instance}
  @argument[region]{an optional @sym{cairo:region-t} instance for the region
    that must be redrawn or the default @code{nil} value for the whole window}
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
    (%renderer-render renderer node region)))

(export 'renderer-render)

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_render_texture
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_render_texture" %renderer-render-texture)
    (g:object gdk:texture)
  (renderer (g:object renderer))
  (node (g:object render-node))
  (viewport (:pointer (:struct graphene:rect-t))))

(defun renderer-render-texture (renderer node &optional viewport)
 #+liber-documentation
 "@version{#2026-02-09}
  @argument[renderer]{a @class{gsk:renderer} instance}
  @argument[node]{a @class{gsk:render-node} instance}
  @argument[viewport]{an optional @sym{graphene:rect-t} instance for the section
    to draw or the default @code{nil} value to use the bounds of @arg{node}}
  @begin{return}
    The @class{gdk:texture} instance for the rendered contents of @arg{node}.
  @end{return}
  @begin{short}
    Renders the scene graph, described by a tree of @class{gsk:render-node}
    instances, to a @class{gdk:texture} instance.
  @end{short}
  The renderer will acquire a reference on the @class{gsk:render-node} tree
  while the rendering is in progress.

  If you want to apply any transformations to @arg{node}, you should put it
  into a transform node and pass that node instead.
  @see-class{gsk:renderer}
  @see-class{gsk:render-node}
  @see-class{gdk:texture}
  @see-symbol{graphene:rect-t}"
  (let ((viewport (or viewport (cffi:null-pointer))))
    (%renderer-render-texture renderer node viewport)))

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
 "@version{2026-02-08}
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
    (g:object renderer :return)
 #+liber-documentation
 "@version{2026-02-08}
  @return{The new @class{gsk:cairo-renderer} instance.}
  @begin{short}
    Creates a new Cairo renderer.
  @end{short}
  The Cairo renderer is the fallback renderer drawing in ways similar to how
  GTK 3 drew its content. Its primary use is as comparison tool.

  The Cairo renderer is incomplete. It cannot render 3D transformed content and
  will instead render an error marker. Its usage should be avoided.
  @see-class{gsk:cairo-renderer}")

(export 'cairo-renderer-new)

;;; ----------------------------------------------------------------------------
;;; GskVulkanRenderer
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GskVulkanRenderer" vulkan-renderer
  (:superclass renderer
   :export t
   :interfaces ()
   :type-initializer "gsk_vulkan_renderer_get_type")
  nil)

#+liber-documentation
(setf (documentation 'vulkan-renderer 'type)
 "@version{2026-02-08}
  @begin{short}
    The GSK renderer that is using Vulkan.
  @end{short}
  This renderer will fail to realize if Vulkan is not supported.
  @see-constructor{gsk:vulkan-renderer-new}
  @see-class{gsk:renderer}")

;;; ----------------------------------------------------------------------------
;;; gsk_vulkan_renderer_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_vulkan_renderer_new" vulkan-renderer-new)
    (g:object renderer :return)
 #+liber-documentation
 "@version{2026-02-08}
  @return{The new @class{gsk:vulkan-renderer} instance.}
  @begin{short}
    Creates a new Vulkan renderer.
  @end{short}
  The Vulkan renderer is a renderer that uses the Vulkan library for rendering.
  This renderer will fail to realize when GTK was not compiled with Vulkan
  support.
  @see-class{gsk:vulkan-renderer}")

(export 'vulkan-renderer-new)

;;; ----------------------------------------------------------------------------
;;; GskGLRenderer                                           Since 4.2
;;; ----------------------------------------------------------------------------

#+gtk-4-2
(gobject:define-gobject "GskGLRenderer" gl-renderer
  (:superclass renderer
   :export t
   :interfaces ()
   :type-initializer "gsk_gl_renderer_get_type")
  nil)

#+(and gtk-4-2 liber-documentation)
(setf (documentation 'gl-renderer 'type)
 "@version{2026-02-08}
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
    (g:object renderer :return)
 #+liber-documentation
 "@version{2026-02-08}
  @return{The new @class{gsk:gl-renderer} instance.}
  @begin{short}
    Creates a new OpenGL renderer.
  @end{short}
  This is the default renderer used by GTK.
  @see-class{gsk:gl-renderer}")

(export 'gl-renderer-new)

;;; ----------------------------------------------------------------------------
;;; GskNGLRenderer                                          Since 4.2
;;; ----------------------------------------------------------------------------

#+gtk-4-2
(gobject:define-gobject "GskNglRenderer" ngl-renderer
  (:superclass renderer
   :export t
   :interfaces ()
   :type-initializer "gsk_ngl_renderer_get_type")
  nil)

#+(and gtk-4-2 liber-documentation)
(setf (documentation 'ngl-renderer 'type)
 "@version{2026-02-08}
  @begin{short}
    The new experimental OpenGL based renderer.
  @end{short}
  See the @class{gsk:gl-renderer} documentation.

  Since 4.2
  @see-constructor{gsk:ngl-renderer-new}
  @see-class{gsk:gl-renderer}")

;;; ----------------------------------------------------------------------------
;;; gsk_ngl_renderer_new                                    Deprecated 4.18
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_ngl_renderer_new" %ngl-renderer-new)
    (g:object renderer :return))

(defun ngl-renderer-new ()
 #+liber-documentation
 "@version{2026-02-08}
  @return{The new @class{gsk:ngl-renderer} instance.}
  @begin{short}
    Creates an instance of the OpenGL renderer.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.18. Use the @fun{gsk:gl-renderer-new}
    function.
  @end{dictionary}
  @see-class{gsk:ngl-renderer}
  @see-class{gsk:renderer}"
  #+(and gtk-4-18 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GSK:NGL-RENDERER-NEW is deprecated since 4.18."))
  (%ngl-renderer-new))

(export 'ngl-renderer-new)

;;; --- End of file gsk4.renderer.lisp -----------------------------------------
