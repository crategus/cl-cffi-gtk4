;;; ----------------------------------------------------------------------------
;;; gdk4.draw-context.lisp
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
;;;     gdk_draw_context_begin_frame                        Deprecated 4.16
;;;     gdk_draw_context_end_frame                          Deprecated 4.16
;;;     gdk_draw_context_is_in_frame                        Deprecated 4.16
;;;     gdk_draw_context_get_frame_region                   Deprecated 4.16
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
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkDrawContext" draw-context
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

#+liber-documentation
(setf (documentation 'draw-context 'type)
 "@version{2024-11-29}
  @begin{short}
    The @class{gdk:draw-context} object is the base object used by contexts
    implementing different rendering methods, such as the @class{gdk:gl-context}
    context.
  @end{short}
  It provides shared functionality between those contexts. You will always
  interact with one of those subclasses.

  A @class{gdk:draw-context} object is always associated with a single toplevel
  surface.
  @see-slot{gdk:draw-context-display}
  @see-slot{gdk:draw-context-surface}
  @see-class{gdk:gl-context}
  @see-class{gdk:cairo-context}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:draw-context-display -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'draw-context) t)
 "The @code{display} property of type @class{gdk:display}
  (Read / Write / Construct Only) @br{}
  The display used to create the draw context.")

#+liber-documentation
(setf (liber:alias-for-function 'draw-context-display)
      "Accessor"
      (documentation 'draw-context-display 'function)
 "@version{2024-11-29}
  @syntax{(gdk:draw-context-display object) => display}
  @argument[object]{a @class{gdk:draw-context} object}
  @argument[display]{a @class{gdk:display} object or @code{nil}}
  @begin{short}
    Accessor of the @code{display} slot of the @class{gdk:draw-context}
    class.
  @end{short}
  The @fun{gdk:draw-context-display} function retrieves the display the draw
  context is created for.
  @see-class{gdk:draw-context}
  @see-class{gdk:display}")

;;; --- gdk:draw-context-surface -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "surface" 'draw-context) t)
 "The @code{surface} property of type @class{gdk:surface}
  (Read / Write / Construct Only) @br{}
  The surface the draw context is bound to.")

#+liber-documentation
(setf (liber:alias-for-function 'draw-context-surface)
      "Accessor"
      (documentation 'draw-context-surface 'function)
 "@version{2024-11-29}
  @syntax{(gdk:draw-context-surface object) => surface}
  @argument[object]{a @class{gdk:draw-context} object}
  @argument[surface]{a @class{gdk:surface} object or @code{nil}}
  @begin{short}
    Accessor of the @code{surface} slot of the @class{gdk:draw-context}
    class.
  @end{short}
  The @fun{gdk:draw-context-surface} function retrieves the surface used by
  the draw context.
  @see-class{gdk:draw-context}
  @see-class{gdk:surface}")

;;; ----------------------------------------------------------------------------
;;; gdk_draw_context_begin_frame                            Deprecated 4.16
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_draw_context_begin_frame" %draw-context-begin-frame) :void
  (context (g:object draw-context))
  (region (:pointer (:struct cairo:region-t))))

(defun draw-context-begin-frame (context region)
 #+liber-documentation
 "@version{#2024-11-7}
  @argument[context]{a @class{gdk:draw-context} object used to draw the frame}
  @argument[region]{a @symbol{cairo:region-t} instance with the minimum region
    that should be drawn}
  @begin{short}
    Indicates that you are beginning the process of redrawing @arg{region} on
    the surface of @arg{context}.
  @end{short}
  Calling this function begins a drawing operation using the context on the
  surface that the context was created from. The actual requirements and
  guarantees for the drawing operation vary for different implementations of
  drawing, so a @class{gdk:cairo-context} and a @class{gdk:gl-context} object
  need to be treated differently.

  A call to this function is a requirement for drawing and must be followed by
  a call to the @fun{gdk:draw-context-end-frame} function, which will complete
  the drawing operation and ensure the contents become visible on screen.

  Note that the region passed to this function is the minimum region that needs
  to be drawn and depending on implementation, windowing system and hardware in
  use, it might be necessary to draw a larger region. Drawing implementation
  must use the @fun{gdk:draw-context-frame-region} function to query the region
  that must be drawn.

  When using GTK, the widget system automatically places calls to the
  @fun{gdk:draw-context-begin-frame} and @fun{gdk:draw-context-end-frame}
  functions via the use of @class{gsk:renderer} objects, so application code
  does not need to call these functions explicitly.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.16. Drawing directly to the surface is
    no longer recommended. Use the @class{gsk:render-node} and
    @class{gsk:renderer} API.
  @end{dictionary}
  @see-class{gdk:draw-context}
  @see-symbol{cairo:region-t}
  @see-class{gdk:cairo-context}
  @see-class{gdk:gl-context}
  @see-class{gsk:renderer}
  @see-function{gdk:draw-context-frame-region}
  @see-function{gdk:draw-context-end-frame}"
  #+(and gtk-4-16 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GDK:CONTEXT-BEGIN-FRAME is deprecated since 4.16"))
  (%draw-context-begin-frame context region))

(export 'draw-context-begin-frame)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_context_end_frame                              Deprecated 4.16
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_draw_context_end_frame" %draw-context-end-frame) :void
  (context (g:object draw-context)))

(defun draw-context-end-frame (context)
 #+liber-documentation
 "@version{#2024-11-7}
  @argument[context]{a @class{gdk:draw-context} object}
  @begin{short}
    Ends a drawing operation started with the @fun{gdk:draw-context-begin-frame}
    function and makes the drawing available on screen.
  @end{short}
  See that function for more details about drawing.

  When using a @class{gdk:gl-context} object, this function may call the
  @code{glFlush()} function implicitly before returning. It is not recommended
  to call the @code{glFlush()} function explicitly before calling this function.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.16. Drawing directly to the surface is
    no longer recommended. Use the @class{gsk:render-node} and
    @class{gsk:renderer} API.
  @end{dictionary}
  @see-class{gdk:draw-context}
  @see-function{gdk:draw-context-begin-frame}"
  #+(and gtk-4-16 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GDK:CONTEXT-END-FRAME is deprecated since 4.16"))
  (%draw-context-end-frame context))

(export 'draw-context-end-frame)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_context_is_in_frame                            Deprecated 4.16
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_draw_context_is_in_frame" %draw-context-is-in-frame)
    :boolean
  (context (g:object draw-context)))

(defun draw-context-is-in-frame (context)
 #+liber-documentation
 "@version{#2024-11-7}
  @argument[context]{a @class{gdk:draw-context} object}
  @return{@em{True} if the draw context is between the
  @fun{gdk:draw-context-begin-frame} and @fun{gdk:draw-context-end-frame}
  function calls.}
  @begin{short}
    Returns @em{true} if @arg{context} is in the process of drawing to its
    surface after a call to the @fun{gdk:draw-context-begin-frame} function and
    not yet having called the @fun{gdk:draw-context-end-frame} function.
  @end{short}
  In this situation, drawing commands may be effecting the contents of a
  surface of the context.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.16. Drawing directly to the surface is
    no longer recommended. Use the @class{gsk:render-node} and
    @class{gsk:renderer} API.
  @end{dictionary}
  @see-class{gdk:draw-context}
  @see-function{gdk:draw-context-begin-frame}
  @see-function{gdk:draw-context-end-frame}"
  #+(and gtk-4-16 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GDK:CONTEXT-IS-IN-FRAME is deprecated since 4.16"))
  (%draw-context-is-in-frame context))

(export 'draw-context-is-in-frame)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_context_get_frame_region                       Deprecated 4.16
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_draw_context_get_frame_region" %draw-context-frame-region)
    (:pointer (:struct cairo:region-t))
  (context (g:object draw-context)))

(defun draw-context-frame-region (context)
 #+liber-documentation
 "@version{2024-12-29}
  @argument[context]{a @class{gdk:draw-context} object}
  @return{The @symbol{cairo:region-t} instance or @code{cffi:null-pointer} if
    not drawing a frame.}
  @begin{short}
    Retrieves the region that is currently in the process of being repainted.
  @end{short}

  After a call to the @fun{gdk:draw-context-begin-frame} function this function
  will return a union of the region passed to that function and the area of the
  surface that the draw context determined needs to be repainted.

  If @arg{context} is not in between calls to the
  @fun{gdk:draw-context-begin-frame} and @fun{gdk:draw-context-end-frame}
  functions, @code{cffi:null-pointer} will be returned.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.16. Drawing directly to the surface is
    no longer recommended. Use the @class{gsk:render-node} and
    @class{gsk:renderer} API.
  @end{dictionary}
  @see-class{gdk:draw-context}
  @see-symbol{cairo:region-t}"
  #+(and gtk-4-16 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GDK:CONTEXT-FRAME-REGION is deprecated since 4.16"))
  (%draw-context-frame-region context))

(export 'draw-context-frame-region)

;;; --- End of file gdk4.draw-context.lisp -------------------------------------
