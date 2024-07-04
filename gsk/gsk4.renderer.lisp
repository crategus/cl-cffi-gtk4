;;; ----------------------------------------------------------------------------
;;; gsk4.renderer.lisp
;;;
;;; The documentation of this file is taken from the GSK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;; GskRenderer
;;;
;;;     Renders a scene
;;;
;;; Types and Values
;;;
;;;     GskRenderer
;;;
;;; Functions
;;;
;;;     gsk_renderer_get_surface
;;;     gsk_renderer_realize
;;;     gsk_renderer_unrealize
;;;     gsk_renderer_is_realized
;;;     gsk_renderer_render
;;;     gsk_renderer_render_texture
;;;     gsk_renderer_new_for_surface
;;;     gsk_gl_renderer_new
;;;     gsk_cairo_renderer_new
;;;     gsk_vulkan_renderer_new
;;;     gsk_broadway_renderer_new
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

(gobject:define-g-object-class "GskRenderer" renderer
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
 "@version{#2023-10-22}
  @begin{short}
    The @class{gsk:renderer} class is a class that renders a scene graph
    defined via a tree of @class{gsk:render-node} instances.
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

;;; --- renderer-realized ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "realized" 'renderer) t)
 "The @code{realized} property of type @code{:boolean} (Read) @br{}
  Whether the renderer has been associated with a surface or draw context. @br{}
  Default value : @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'renderer-realized)
      "Accessor"
      (documentation 'renderer-realized 'function)
 "@version{#2023-10-22}
  @syntax{(gsk:renderer-realized object) => realized}
  @argument[object]{a @class{gsk:renderer} instance}
  @argument[realized]{a boolean whether the renderer has been associated with
    a surface or draw context}
  @begin{short}
    Accessor of the @slot[gsk:renderer]{realized} slot of the
    @class{gsk:renderer} class.
  @end{short}
  @see-class{gsk:renderer}")

;;; --- renderer-surface -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "surface" 'renderer) t)
 "The @code{surface} property of type @class{gdk:surface} (Read) @br{}
  The surface associated with renderer.")

#+liber-documentation
(setf (liber:alias-for-function 'renderer-surface)
      "Accessor"
      (documentation 'renderer-surface 'function)
 "@version{#2023-10-22}
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
;;; gsk_renderer_new_for_surface ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_new_for_surface" renderer-new-for-surface)
    (g:object renderer)
 #+liber-documentation
 "@version{#2023-10-22}
  @argument[surface]{a @class{gdk:surface} object}
  @return{A new @class{gsk:renderer} instance.}
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
;;; gsk_gl_renderer_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_gl_renderer_new" gl-renderer-new) (g:object renderer)
 #+liber-documentation
 "@version{#2023-10-23}
  @return{A new GL @class{gsk:renderer} instance.}
  @begin{short}
    Creates a new @class{gsk:renderer} instance using OpenGL.
  @end{short}
  This is the default renderer used by GTK.
  @see-class{gsk:renderer}")

(export 'gl-renderer-new)

;;; ----------------------------------------------------------------------------
;;; gsk_cairo_renderer_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cairo_renderer_new" cairo-renderer-new) (g:object renderer)
 #+liber-documentation
 "@version{#2023-10-23}
  @return{A new Cairo @class{gsk:renderer} instance.}
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
;;; gsk_renderer_realize ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_realize" %renderer-realize) :boolean
  (renderer (g:object renderer))
  (surface (g:object gdk:surface))
  (err :pointer))

(defun renderer-realize (renderer surface)
 #+liber-documentation
 "@version{#2023-10-22}
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
;;; gsk_renderer_unrealize ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_unrealize" renderer-unrealize) :void
 #+liber-documentation
 "@version{#2023-10-22}
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
;;; gsk_renderer_is_realized ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_is_realized" renderer-is-realized) :boolean
 #+liber-documentation
 "@version{#2023-10-22}
  @argument[renderer]{a @class{gsk:renderer} instance}
  @return{@em{True} if @arg{renderer} was realized, and @em{false} otherwise.}
  @short{Checks whether the renderer is realized or not.}
  @see-class{gsk:renderer}"
  (renderer (g:object renderer)))

(export 'renderer-is-realized)

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_render ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_render" renderer-render) :void
 #+liber-documentation
 "@version{#2023-10-22}
  @argument[renderer]{a @class{gsk:renderer} instance}
  @argument[root]{a @class{gsk:render-node} instance}
  @argument[region]{a @symbol{cairo:context-t} instance with the region that
    must be redrawn or @code{nil} for the whole window}
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
  (renderer (g:object renderer))
  (root (g:object render-node))
  (region (:pointer (:struct cairo:region-t))))

(export 'renderer-render)

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_render_texture ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_render_texture" renderer-render-texture)
    (g:object gdk:texture)
 #+liber-documentation
 "@version{#2023-10-22}
  @argument[renderer]{a @class{gsk:renderer} instance}
  @argument[root]{a @class{gsk:render-node} instance}
  @argument[viewport]{a @symbol{graphene:rect-t} instance with the section to
    draw or @code{nil} to use the bounds of @arg{root}}
  @return{A @class{gdk:texture} instance with the rendered contents of
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
  (renderer (g:object renderer))
  (root (g:object render-node))
  (viewport (:pointer (:struct graphene:rect-t))))

(export 'renderer-render-texture)

;;; ----------------------------------------------------------------------------
;;; gsk_vulkan_renderer_new ()
;;;
;;; GskRenderer *
;;; gsk_vulkan_renderer_new (void);
;;;
;;; Creates a new Vulkan renderer.
;;;
;;; The Vulkan renderer is a renderer that uses the Vulkan library for
;;; rendering.
;;;
;;; This function is only available when GTK was compiled with Vulkan support.
;;;
;;; Returns :
;;;     a new Vulkan renderer
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("gsk_vulkan_renderer_new" vulkan-renderer-new)
    (g:object renderer))

#+nil
(export 'vulkan-renderer-new)

;;; ----------------------------------------------------------------------------
;;; gsk_broadway_renderer_new ()
;;;
;;; GskRenderer *
;;; gsk_broadway_renderer_new (void);
;;;
;;; Creates a new Broadway renderer.
;;;
;;; The Broadway renderer is the default renderer for the broadway backend. It
;;; will only work with broadway surfaces, otherwise it will fail the call to
;;; gsk_renderer_realize().
;;;
;;; This function is only available when GTK was compiled with Broadway support.
;;;
;;; Returns :
;;;     a new Broadway renderer.
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("gsk_broadway_renderer_new" broadway-renderer-new)
    (g:object renderer))

#+nil
(export 'broadway-renderer-new)

;;; --- End of file gsk4.renderer.lisp -----------------------------------------
