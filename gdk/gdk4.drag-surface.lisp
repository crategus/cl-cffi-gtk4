;;; ----------------------------------------------------------------------------
;;; gdk4.drag-surface.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GDK library,
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
;;; Types and Values
;;;
;;;     GdkDragSurface
;;;     GdkDragSurfaceSize
;;;
;;; Functions
;;;
;;;     gdk_drag_surface_present
;;;
;;;     gdk_drag_surface_size_set_size
;;;
;;; Signals
;;;
;;;     compute-size
;;;
;;; Hierarchy
;;;
;;;     GInterface
;;;     ╰── GdkDragSurface
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDragSurfaceSize
;;; ----------------------------------------------------------------------------

#+gtk-4-12
(cffi:defctype drag-surface-size :pointer)

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-symbol 'drag-surface-size)
      "CStruct"
      (liber:symbol-documentation 'drag-surface-size)
 "@version{#2025-07-31}
  @begin{short}
    The @sym{gdk:drag-surface-size} structure contains information that is
    useful to compute the size of a drag surface.
  @end{short}

  Since 4.12
  @see-class{gdk:drag-surface}")

#+gtk-4-12
(export 'drag-surface-size)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_surface_size_set_size
;;; ----------------------------------------------------------------------------

#+gtk-4-12
(cffi:defcfun ("gdk_drag_surface_size_set_size" drag-surface-size-set-size)
    :void
 #+liber-documentation
 "@version{#2025-07-31}
  @argument[size]{a @sym{gdk:drag-surface-size} instance}
  @argument[width]{an integer for the width}
  @argument[height]{an integer for the height}
  @begin{short}
    Sets the size the drag surface prefers to be resized to.
  @end{short}

  Since 4.12
  @see-symbol{gdk:drag-surface-size}"
  (size :pointer)
  (width :int)
  (height :int))

#+gtk-4-12
(export 'drag-surface-size-set-size)

;;; ----------------------------------------------------------------------------
;;; GdkDragSurface
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GdkDragSurface" drag-surface
  (:superclass g:object
   :export t
   :type-initializer "gdk_drag_surface_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'drag-surface)
      "Interface"
      (documentation 'drag-surface 'type)
 "@version{2025-08-04}
  @begin{short}
    The @class{gdk:drag-surface} interface is an interface for surfaces used
    during DND.
  @end{short}
  @begin[Signal Details]{dictionary}
    @begin[drag-surface::compute-size]{signal}
      @begin{pre}
lambda (surface size)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[surface]{The @class{gdk:drag-surface} object.}
        @entry[size]{The @sym{gdk:drag-surface-size} instance for the size of
          the drag surface.}
      @end{simple-table}
      Emitted when the size for the surface needs to be computed, when it is
      present. This signal will normally be emitted during the native surface
      layout cycle when the surface size needs to be recomputed. It is the
      responsibility of the drag surface user to handle this signal and compute
      the desired size of the surface, storing the computed size in the
      @sym{gdk:drag-surface-size} instance that is passed to the signal handler,
      using the @fun{gdk:drag-surface-size-set-size} function. Failing to set a
      size so will result in an arbitrary size being used as a result.
      Since 4.12
    @end{signal}
  @end{dictionary}
  @see-class{gdk:surface}
  @see-class{gdk:drag}
  @see-class{gdk:drop}")

;;; ----------------------------------------------------------------------------
;;; gdk_drag_surface_present
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_surface_present" drag-surface-present) :boolean
 #+liber-documentation
 "@version{#2025-08-04}
  @argument[surface]{a @class{gdk:drag-surface} object to show}
  @argument[width]{an integer for the unconstrained @arg{surface} width to
    layout}
  @argument[height]{an integer for the unconstrained @arg{surface} height to
    layout}
  @begin{return}
    @em{False} if the drag surface failed to be presented, otherwise @em{true}.
  @end{return}
  @short{Present the drag surface.}
  @see-class{gdk:drag-surface}"
  (surface (g:object drag-surface))
  (width :int)
  (height :int))

(export 'drag-surface-present)

;;; --- End of file gdk4.drag-surface.lisp -------------------------------------
