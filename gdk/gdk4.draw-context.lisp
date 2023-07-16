;;; ----------------------------------------------------------------------------
;;; gdk4.draw-context.lisp
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
;;; GdkDrawContext
;;;
;;;     Base class for draw contexts
;;;
;;; Types and Values
;;;
;;;     GdkDrawContext
;;;
;;; Accessors
;;;
;;;     gdk_draw_context_get_display
;;;     gdk_draw_context_get_surface
;;;
;;; Functions
;;;
;;;     gdk_draw_context_begin_frame
;;;     gdk_draw_context_end_frame
;;;     gdk_draw_context_is_in_frame
;;;     gdk_draw_context_get_frame_region
;;;
;;; Properties
;;;
;;;     display
;;;     surface
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDrawContext
;;;         ├── GdkGLContext
;;;         ╰── GdkVulkanContext
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDrawContext
;;;
;;; GdkDrawContext is the base object used by contexts implementing different
;;; rendering methods, such as GdkGLContext or GdkVulkanContext. It provides
;;; shared functionality between those contexts.
;;;
;;; You will always interact with one of those subclasses.
;;;
;;; A GdkDrawContext is always associated with a single toplevel surface.
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GdkDrawContext" draw-context
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_draw_context_get_type")
  ((display
    draw-context-display
    "display" "GdkDisplay" t nil)
   (surface
    draw-context-surface
    "surface" "GdkSurface" t nil)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; property display: Gdk.Display [ read, write, construct-only ]
;;;
;;; The GdkDisplay used to create the GdkDrawContext.
;;;
;;; Type:	GdkDisplay
;;; Getter method	gdk_draw_context_get_display()
;;;
;;; Flags
;;; Readable	yes
;;; Writable	yes
;;; Construct	no
;;; Construct only	yes
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_draw_context_get_display ()
;;;
;;; GdkDisplay *
;;; gdk_draw_context_get_display (GdkDrawContext *context);
;;;
;;; Retrieves the GdkDisplay the context is created for
;;;
;;; context :
;;;     a GdkDrawContext
;;;
;;; Returns :
;;;     a GdkDisplay or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; property surface: Gdk.Surface [ read, write, construct-only ]
;;;
;;; The GdkSurface the context is bound to.
;;;
;;; Type:	GdkSurface
;;; Getter method	gdk_draw_context_get_surface()
;;;
;;; Flags
;;; Readable	yes
;;; Writable	yes
;;; Construct	no
;;; Construct only	yes
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_draw_context_get_surface ()
;;;
;;; GdkSurface *
;;; gdk_draw_context_get_surface (GdkDrawContext *context);
;;;
;;; Retrieves the GdkSurface used by the context .
;;;
;;; context :
;;;     a GdkDrawContext
;;;
;;; Returns :
;;;     a GdkSurface or NULL.
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gdk_draw_context_begin_frame ()
;;;
;;; void
;;; gdk_draw_context_begin_frame (GdkDrawContext *context,
;;;                               const cairo_region_t *region);
;;;
;;; Indicates that you are beginning the process of redrawing region on the
;;; context 's surface.
;;;
;;; Calling this function begins a drawing operation using context on the
;;; surface that context was created from. The actual requirements and
;;; guarantees for the drawing operation vary for different implementations of
;;; drawing, so a GdkCairoContext and a GdkGLContext need to be treated
;;; differently.
;;;
;;; A call to this function is a requirement for drawing and must be followed by
;;; a call to gdk_draw_context_end_frame(), which will complete the drawing
;;; operation and ensure the contents become visible on screen.
;;;
;;; Note that the region passed to this function is the minimum region that
;;; needs to be drawn and depending on implementation, windowing system and
;;; hardware in use, it might be necessary to draw a larger region. Drawing
;;; implementation must use gdk_draw_context_get_frame_region() to query the
;;; region that must be drawn.
;;;
;;; When using GTK, the widget system automatically places calls to
;;; gdk_draw_context_begin_frame() and gdk_draw_context_end_frame() via the use
;;; of GskRenderers, so application code does not need to call these functions
;;; explicitly.
;;;
;;; context :
;;;     the context used to draw the frame
;;;
;;; region :
;;;     minimum region that should be drawn
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_draw_context_begin_frame" draw-context-begin-frame) :void
  (context (g:object draw-context))
  (region (:pointer (:struct cairo:region-t))))

(export 'draw-context-begin-frame)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_context_end_frame ()
;;;
;;; void
;;; gdk_draw_context_end_frame (GdkDrawContext *context);
;;;
;;; Ends a drawing operation started with gdk_draw_context_begin_frame() and
;;; makes the drawing available on screen. See that function for more details
;;; about drawing.
;;;
;;; When using a GdkGLContext, this function may call glFlush() implicitly
;;; before returning; it is not recommended to call glFlush() explicitly before
;;; calling this function.
;;;
;;; context :
;;;     a GdkDrawContext
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_draw_context_end_frame" draw-context-end-frame) :void
  (context (g:object draw-context)))

(export 'draw-context-end-frame)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_context_is_in_frame ()
;;;
;;; gboolean
;;; gdk_draw_context_is_in_frame (GdkDrawContext *context);
;;;
;;; Returns TRUE if context is in the process of drawing to its surface after a
;;; call to gdk_draw_context_begin_frame() and not yet having called
;;; gdk_draw_context_end_frame(). In this situation, drawing commands may be
;;; effecting the contents of a context 's surface.
;;;
;;; context :
;;;     a GdkDrawContext
;;;
;;; Returns :
;;;     TRUE if the context is between gdk_draw_context_begin_frame() and
;;;     gdk_draw_context_end_frame() calls.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_draw_context_is_in_frame" draw-context-is-in-frame) :boolean
  (context (g:object draw-context)))

(export 'draw-context-is-in-frame)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_context_get_frame_region ()
;;;
;;; const cairo_region_t *
;;; gdk_draw_context_get_frame_region (GdkDrawContext *context);
;;;
;;; Retrieves the region that is currently in the process of being repainted.
;;;
;;; After a call to gdk_draw_context_begin_frame() this function will return a
;;; union of the region passed to that function and the area of the surface that
;;; the context determined needs to be repainted.
;;;
;;; If context is not in between calls to gdk_draw_context_begin_frame() and
;;; gdk_draw_context_end_frame(), NULL will be returned.
;;;
;;; context :
;;;     a GdkDrawContext
;;;
;;; Returns :
;;;     a Cairo region or NULL if not drawing a frame.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_draw_context_get_frame_region" draw-context-frame-region)
    (:pointer (:struct cairo:region-t))
  (context (g:object draw-context)))

(export 'draw-context-frame-region)

;;; --- End of file gdk4.draw-context.lisp -------------------------------------
