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
