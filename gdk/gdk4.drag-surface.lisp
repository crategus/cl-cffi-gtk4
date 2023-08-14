;;; ----------------------------------------------------------------------------
;;; gdk4.drag-surface.lisp
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
;;; Types and Values
;;;
;;;     GdkDragSurface
;;;
;;; Functions
;;;
;;;     gdk_drag_surface_present
;;;
;;; Hierarchy
;;;
;;;     GInterface
;;;     ╰── GdkDragSurface
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDragSurface
;;; ----------------------------------------------------------------------------

;; FIXME: GdkDragSurface inherit from GdkSurface. We have changed the
;; implementation of define-g-interface to allow other prerequistes as
;; g:object. But when we define gdk:surface as prerequiste for gdk:drag-surface
;; the test gdk-cairo-context-cairo-create fails. Why?

(gobject:define-g-interface "GdkDragSurface" drag-surface
  (:superclass g:object
   :export t
   :type-initializer "gdk_drag_surface_get_type")
  nil)

#+liber-documentation
(setf (documentation 'drag-surface 'type)
 "@version{2023-7-30}
  @begin{short}
    The @sym{gdk:drag-surface} interface is an interface for surfaces used
    during DND.
  @end{short}
  @see-class{gdk:surface}
  @see-class{gdk:drag}
  @see-class{gdk:drop}")

;;; ----------------------------------------------------------------------------
;;; gdk_drag_surface_present ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_surface_present" drag-surface-present) :boolean
 #+liber-documentation
 "@version{#2023-7-30}
  @argument[surface]{a @class{gdk:drag-surface} object to show}
  @argument[width]{an integer with the unconstrained @arg{surface} width to
    layout}
  @argument[height]{an integer with the unconstrained @arg{surface} height to
    layout}
  @return{@em{False} if it failed to be presented, otherwise @em{true}.}
  @short{Present the drag surface.}
  @see-class{gdk:drag-surface}"
  (surface (g:object drag-surface))
  (width :int)
  (height :int))

(export 'drag-surface-present)

;;; --- End of file gdk4.drag-surface.lisp -------------------------------------
