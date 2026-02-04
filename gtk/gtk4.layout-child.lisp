;;; ----------------------------------------------------------------------------
;;; gtk4.layout-child.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
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
;;; GtkLayoutChild
;;;
;;;     An object containing layout properties
;;;
;;; Types and Values
;;;
;;;     GtkLayoutChild
;;;
;;; Accessors
;;;
;;;     gtk_layout_child_get_layout_manager
;;;     gtk_layout_child_get_child_widget
;;;
;;; Properties
;;;
;;;     child-widget
;;;     layout-manager
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkLayoutChild
;;;         ├── GtkGridLayoutChild
;;;         ├── GtkOverlayLayoutChild
;;;         ├── GtkConstraintLayoutChild
;;;         ╰── GtkFixedLayoutChild
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkLayoutChild
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkLayoutChild" layout-child
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_layout_child_get_type")
  ((child-widget
    layout-child-child-widget
    "child-widget" "GtkWidget" t nil)
   (layout-manager
    layout-child-layout-manager
    "layout-manager" "GtkLayoutManager" t nil)))

#+liber-documentation
(setf (documentation 'layout-child 'type)
 "@version{2024-04-19}
  @begin{short}
    The @class{gtk:layout-child} class is the base class for objects that are
    meant to hold layout properties.
  @end{short}
  If a @class{gtk:layout-manager} object has per-child properties, like their
  packing type, or the horizontal and vertical span, or the icon name, then
  the layout manager should use a @class{gtk:layout-child} implementation to
  store those properties.

  A @class{gtk:layout-child} instance is only ever valid while a widget is part
  of a layout.
  @see-class{gtk:layout-child}
  @see-class{gtk:layout-manager}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:layout-child-child-widget ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child-widget" 'layout-child) t)
 "The @code{child-widget} property of type @class{gtk:widget}
 (Read / Write / Construct only) @br{}
 The widget that is associated to the @class{gtk:layout-child} object.")

#+liber-documentation
(setf (liber:alias-for-function 'layout-child-child-widget)
      "Accessor"
      (documentation 'layout-child-child-widget 'function)
 "@version{2024-04-19}
  @syntax{(gtk:layout-child-child-widget object) => child}
  @argument[object]{a @class{gtk:layout-child} object}
  @argument[child]{a @class{gtk:widget} object}
  @begin{short}
    Retrieves the @class{gtk:widget} child widget associated to the given
    @arg{object}.
  @end{short}
  @see-class{gtk:layout-child}
  @see-class{gtk:widget}")

;;; --- gtk:layout-child-layout-manager ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "layout-manager"
                                               'layout-child) t)
 "The @code{layout-manager} property of type @class{gtk:layout-manager}
 (Read / Write / Construct only) @br{}
 The layout manager that created the @class{gtk:layout-child} object.")

#+liber-documentation
(setf (liber:alias-for-function 'layout-child-layout-manager)
      "Accessor"
      (documentation 'layout-child-layout-manager 'function)
 "@version{2024-04-19}
  @syntax{(gtk:layout-child-layout-manager object) => manager}
  @argument[object]{a @class{gtk:layout-child} object}
  @argument[manager]{a @class{gtk:layout-manager} object}
  @begin{short}
    Retrieves the @class{gtk:layout-manager} object that created the given
    @arg{object}.
  @end{short}
  @see-class{gtk:layout-child}
  @see-class{gtk:layout-manager}")

;;; --- End of file gtk4.layout-child.lisp -------------------------------------
