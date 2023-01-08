;;; ----------------------------------------------------------------------------
;;; gtk.layout-child.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License. If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
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
;;;
;;; GtkLayoutChild is the base class for objects that are meant to hold layout
;;; properties. If a GtkLayoutManager has per-child properties, like their
;;; packing type, or the horizontal and vertical span, or the icon name, then
;;; the layout manager should use a GtkLayoutChild implementation to store
;;; those properties.
;;;
;;; A GtkLayoutChild instance is only ever valid while a widget is part of a
;;; layout.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkLayoutChild" layout-child
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

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “child-widget” property
;;;
;;;  “child-widget”             GtkWidget *
;;;
;;; The widget that is associated to the GtkLayoutChild instance.
;;;
;;; Owner: GtkLayoutChild
;;;
;;; Flags: Read / Write / Construct Only
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “layout-manager” property
;;;
;;;  “layout-manager”           GtkLayoutManager *
;;;
;;; The layout manager that created the GtkLayoutChild instance.
;;;
;;; Owner: GtkLayoutChild
;;;
;;; Flags: Read / Write / Construct Only
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gtk_layout_child_get_layout_manager ()
;;;
;;; GtkLayoutManager *
;;; gtk_layout_child_get_layout_manager (GtkLayoutChild *layout_child);
;;;
;;; Retrieves the GtkLayoutManager instance that created the given
;;; layout_child .
;;;
;;; layout_child :
;;;     a GtkLayoutChild
;;;
;;; Returns :
;;;     a GtkLayoutManager.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_layout_child_get_child_widget ()
;;;
;;; GtkWidget *
;;; gtk_layout_child_get_child_widget (GtkLayoutChild *layout_child);
;;;
;;; Retrieves the GtkWidget associated to the given layout_child .
;;;
;;; layout_child :
;;;     a GtkLayoutChild
;;;
;;; Returns :
;;;     a GtkWidget.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.layout-child.lisp --------------------------------------
