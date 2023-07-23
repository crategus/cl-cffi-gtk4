;;; ----------------------------------------------------------------------------
;;; gsk.renderer.lisp
;;;
;;; The documentation of this file is taken from the GSK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;;
;;; GskRenderer is a class that renders a scene graph defined via a tree of
;;; GskRenderNode instances.
;;;
;;; Typically you will use a GskRenderer instance to repeatedly call
;;; gsk_renderer_render() to update the contents of its associated GdkSurface.
;;;
;;; It is necessary to realize a GskRenderer instance using
;;; gsk_renderer_realize() before calling gsk_renderer_render(), in order to
;;; create the appropriate windowing system resources needed to render the
;;; scene.
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

;;; ----------------------------------------------------------------------------
;;; Property Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “realized” property
;;;
;;;  “realized”                 gboolean
;;;
;;; The renderer has been associated with a surface.
;;;
;;; Owner: GskRenderer
;;;
;;; Flags: Read
;;;
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “surface” property
;;;
;;;  “surface”                  GdkSurface *
;;;
;;; The surface associated to the renderer.
;;;
;;; Owner: GskRenderer
;;;
;;; Flags: Read
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_get_surface ()
;;;
;;; GdkSurface *
;;; gsk_renderer_get_surface (GskRenderer *renderer);
;;;
;;; Retrieves the GdkSurface set using gsk_renderer_realize(). If the renderer
;;; has not been realized yet, NULL will be returned.
;;;
;;; renderer :
;;;     a GskRenderer
;;;
;;; Returns :
;;;     a GdkSurface.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_realize ()
;;;
;;; gboolean
;;; gsk_renderer_realize (GskRenderer *renderer,
;;;                       GdkSurface *surface,
;;;                       GError **error);
;;;
;;; Creates the resources needed by the renderer to render the scene graph.
;;;
;;; renderer :
;;;     a GskRenderer
;;;
;;; surface :
;;;     the GdkSurface renderer will be used on
;;;
;;; error :
;;;     return location for an error
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_realize" %renderer-realize) :boolean
  (renderer (g:object renderer))
  (surface (g:object gdk:surface))
  (err :pointer))

(defun renderer-realize (renderer surface)
  (glib:with-g-error (err)
    (%renderer-realize renderer surface err)))

(export 'renderer-realize)

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_unrealize ()
;;;
;;; void
;;; gsk_renderer_unrealize (GskRenderer *renderer);
;;;
;;; Releases all the resources created by gsk_renderer_realize().
;;;
;;; renderer :
;;;     a GskRenderer
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_unrealize" renderer-unrealize) :void
  (renderer (g:object renderer)))

(export 'renderer-unrealize)

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_is_realized ()
;;;
;;; gboolean
;;; gsk_renderer_is_realized (GskRenderer *renderer);
;;;
;;; Checks whether the renderer is realized or not.
;;;
;;; renderer :
;;;     a GskRenderer
;;;
;;; Returns :
;;;     TRUE if the GskRenderer was realized, and FALSE otherwise
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_is_realized" renderer-is-realized) :boolean
  (renderer (g:object renderer)))

(export 'renderer-is-realized)

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_render ()
;;;
;;; void
;;; gsk_renderer_render (GskRenderer *renderer,
;;;                      GskRenderNode *root,
;;;                      const cairo_region_t *region);
;;;
;;; Renders the scene graph, described by a tree of GskRenderNode instances,
;;; ensuring that the given region gets redrawn.
;;;
;;; Renderers must ensure that changes of the contents given by the root node as
;;; well as the area given by region are redrawn. They are however free to not
;;; redraw any pixel outside of region if they can guarantee that it didn't
;;; change.
;;;
;;; The renderer will acquire a reference on the GskRenderNode tree while the
;;; rendering is in progress.
;;;
;;; renderer :
;;;     a GskRenderer
;;;
;;; root :
;;;     a GskRenderNode
;;;
;;; region :
;;;     the cairo_region_t that must be redrawn or NULL for the whole window.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_render" renderer-render) :void
  (renderer (g:object renderer))
  (root (g:object render-node))
  (region (:pointer (:struct cairo:region-t))))

(export 'renderer-render)

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_render_texture ()
;;;
;;; GdkTexture *
;;; gsk_renderer_render_texture (GskRenderer *renderer,
;;;                              GskRenderNode *root,
;;;                              const graphene_rect_t *viewport);
;;;
;;; Renders the scene graph, described by a tree of GskRenderNode instances, to
;;; a GdkTexture.
;;;
;;; The renderer will acquire a reference on the GskRenderNode tree while the
;;; rendering is in progress.
;;;
;;; If you want to apply any transformations to root , you should put it into a
;;; transform node and pass that node instead.
;;;
;;; renderer :
;;;     a realized GskRenderer
;;;
;;; root :
;;;     a GskRenderNode
;;;
;;; viewport :
;;;     the section to draw or NULL to use root 's bounds.
;;;
;;; Returns :
;;;     a GdkTexture with the rendered contents of root .
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_render_texture" renderer-render-texture)
    (g:object gdk:texture)
  (renderer (g:object renderer))
  (root (g:object render-node))
  (viewport (:pointer (:struct graphene:rect-t))))

(export 'renderer-render-texture)

;;; ----------------------------------------------------------------------------
;;; gsk_renderer_new_for_surface ()
;;;
;;; GskRenderer *
;;; gsk_renderer_new_for_surface (GdkSurface *surface);
;;;
;;; Creates an appropriate GskRenderer instance for the given surface .
;;;
;;; If the GSK_RENDERER environment variable is set, GSK will try that renderer
;;; first, before trying the backend-specific default. The ultimate fallback is
;;; the cairo renderer.
;;;
;;; The renderer will be realized before it is returned.
;;;
;;; surface :
;;;     a GdkSurface
;;;
;;; Returns :
;;;     a GskRenderer.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_renderer_new_for_surface" renderer-new-for-surface)
    (g:object renderer)
  (surface (g:object gdk:surface)))

(export 'renderer-new-for-surface)

;;; ----------------------------------------------------------------------------
;;; gsk_gl_renderer_new ()
;;;
;;; GskRenderer *
;;; gsk_gl_renderer_new (void);
;;;
;;; Creates a new GskRenderer using OpenGL. This is the default renderer used
;;; by GTK.
;;;
;;; Returns :
;;;     a new GL renderer
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_gl_renderer_new" gl-renderer-new) (g:object renderer))

(export 'gl-renderer-new)

;;; ----------------------------------------------------------------------------
;;; gsk_cairo_renderer_new ()
;;;
;;; GskRenderer *
;;; gsk_cairo_renderer_new (void);
;;;
;;; Creates a new Cairo renderer.
;;;
;;; The Cairo renderer is the fallback renderer drawing in ways similar to how
;;; GTK 3 drew its content. Its primary use is as comparison tool.
;;;
;;; The Cairo renderer is incomplete. It cannot render 3D transformed content
;;; and will instead render an error marker. Its usage should be avoided.
;;;
;;; Returns :
;;;     a new Cairo renderer.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cairo_renderer_new" cairo-renderer-new) (g:object renderer))

(export 'cairo-renderer-new)

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

(cffi:defcfun ("gsk_vulkan_renderer_new" vulkan-renderer-new)
    (g:object renderer))

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

(cffi:defcfun ("gsk_broadway_renderer_new" broadway-renderer-new)
    (g:object renderer))

(export 'broadway-renderer-new)

;;; --- End of file gsk.renderer.lisp ------------------------------------------
