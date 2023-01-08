;;; ----------------------------------------------------------------------------
;;; gdk.cairo-context.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
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
;;; GdkCairoContext
;;;
;;;     Cairo draw context
;;;
;;; Types and Values
;;;
;;;     GdkCairoContext
;;;
;;; Functions
;;;
;;;     gdk_cairo_context_cairo_create
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkCairoContext
;;;
;;;
;;; GdkCairoContext is an object representing the platform-specific draw
;;; context.
;;;
;;; GdkCairoContexts are created for a GdkDisplay using
;;; gdk_surface_create_cairo_context(), and the context can then be used to
;;; draw on that GdkSurface.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkCairoContext" cairo-context
  (:superclass draw-context
   :export t
   :interfaces nil
   :type-initializer "gdk_cairo_context_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_context_cairo_create ()
;;;
;;; cairo_t *
;;; gdk_cairo_context_cairo_create (GdkCairoContext *self);
;;;
;;; Retrieves a Cairo context to be used to draw on the GdkSurface of context .
;;; A call to gdk_draw_context_begin_frame() with this context must have been
;;; done or this function will return NULL.
;;;
;;; The returned context is guaranteed to be valid until
;;; gdk_draw_context_end_frame() is called.
;;;
;;; self :
;;;     a GdkCairoContext that is currently drawing
;;;
;;; Returns :
;;;
;;;     a Cairo context to be used to draw the contents of the GdkSurface.
;;;     NULL is returned when context is not drawing.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_context_cairo_create" cairo-context-cairo-create)
    (:pointer (:struct cairo:context-t))
  (context (g:object cairo-context)))

(export 'cairo-context-cairo-create)

;;; --- End of file gdk.cairo-context.lisp -------------------------------------
