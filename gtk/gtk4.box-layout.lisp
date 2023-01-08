;;; ----------------------------------------------------------------------------
;;; gtk.box-layout.lisp
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
;;; GtkBoxLayout
;;;
;;;     Layout manager for placing all children in a single row or column
;;;
;;; Types and Values
;;;
;;;     GtkBoxLayout
;;;
;;; Accessors
;;;
;;;     gtk_box_layout_set_homogeneous
;;;     gtk_box_layout_get_homogeneous
;;;     gtk_box_layout_set_spacing
;;;     gtk_box_layout_get_spacing
;;;     gtk_box_layout_set_baseline_position
;;;     gtk_box_layout_get_baseline_position
;;;
;;; Functions
;;;
;;;     gtk_box_layout_new
;;;
;;; Properties
;;;
;;;     baseline-position
;;;     homogeneous
;;;     spacing
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkLayoutManager
;;;         ╰── GtkBoxLayout
;;;
;;; Implemented Interfaces
;;;
;;;     GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBoxLayout
;;;
;;; A GtkBoxLayout is a layout manager that arranges the children of any widget
;;; using it into a single row or column, depending on the value of its
;;; “orientation” property. Within the other dimension all children all
;;; allocated the same size. The GtkBoxLayout will respect the “halign” and
;;; “valign” properties of each child widget.
;;;
;;; If you want all children to be assigned the same size, you can use the
;;; “homogeneous” property.
;;;
;;; If you want to specify the amount of space placed between each child, you
;;; can use the “spacing” property.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkBoxLayout" box-layout
  (:superclass layout-manager
   :export t
   :interfaces ("GtkOrientable")
   :type-initializer "gtk_box_layout_get_type")
  ((baseline-position
    box-layout-baseline-position
    "baseline-position" "GtkBaselinePosition" t t)
   (homogeneous
    box-layout-homogeneous
    "homogeneous" "gboolean" t t)
   (spacing
    box-layout-spacing
    "spacing" "gint" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “baseline-position” property
;;;
;;;  “baseline-position”        GtkBaselinePosition
;;;
;;; The position of the allocated baseline within the extra space allocated to
;;; each child of the widget using a box layout manager.
;;;
;;; This property is only relevant for horizontal layouts containing at least
;;; one child with a baseline alignment.
;;;
;;; Owner: GtkBoxLayout
;;;
;;; Flags: Read / Write
;;;
;;; Default value: GTK_BASELINE_POSITION_CENTER
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “homogeneous” property
;;;
;;;  “homogeneous”              gboolean
;;;
;;; Whether the box layout should distribute the available space homogeneously
;;; among the children of the widget using it as a layout manager.
;;;
;;; Owner: GtkBoxLayout
;;;
;;; Flags: Read / Write
;;;
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “spacing” property
;;;
;;;  “spacing”                  int
;;;
;;; The space between each child of the widget using the box layout as its
;;; layout manager.
;;;
;;; Owner: GtkBoxLayout
;;;
;;; Flags: Read / Write
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 0
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gtk_box_layout_new ()
;;;
;;; GtkLayoutManager *
;;; gtk_box_layout_new (GtkOrientation orientation);
;;;
;;; Creates a new box layout.
;;;
;;; orientation :
;;;     the orientation for the new layout
;;;
;;; Returns :
;;;     a new box layout
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_box_layout_set_homogeneous ()
;;;
;;; void
;;; gtk_box_layout_set_homogeneous (GtkBoxLayout *box_layout,
;;;                                 gboolean homogeneous);
;;;
;;; Sets whether the box layout will allocate the same size to all children.
;;;
;;; box_layout :
;;;     a GtkBoxLayout
;;;
;;; homogeneous :
;;;     TRUE to set the box layout as homogeneous
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_box_layout_get_homogeneous ()
;;;
;;; gboolean
;;; gtk_box_layout_get_homogeneous (GtkBoxLayout *box_layout);
;;;
;;; Returns whether the layout is set to be homogeneous.
;;;
;;; box_layout :
;;;     a GtkBoxLayout
;;;
;;; Return :
;;;     TRUE if the layout is homogeneous
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_box_layout_set_spacing ()
;;;
;;; void
;;; gtk_box_layout_set_spacing (GtkBoxLayout *box_layout,
;;;                             guint spacing);
;;;
;;; Sets how much spacing to put between children.
;;;
;;; box_layout :
;;;     a GtkBoxLayout
;;;
;;; spacing :
;;;     the spacing to apply between children
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_box_layout_get_spacing ()
;;;
;;; guint
;;; gtk_box_layout_get_spacing (GtkBoxLayout *box_layout);
;;;
;;; Returns the space that box_layout puts between children.
;;;
;;; box_layout :
;;;     a GtkBoxLayout
;;;
;;; Returns :
;;;     the spacing of the layout
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_box_layout_set_baseline_position ()
;;;
;;; void
;;; gtk_box_layout_set_baseline_position (GtkBoxLayout *box_layout,
;;;                                       GtkBaselinePosition position);
;;;
;;; Sets the baseline position of a box layout.
;;;
;;; The baseline position affects only horizontal boxes with at least one
;;; baseline aligned child. If there is more vertical space available than
;;; requested, and the baseline is not allocated by the parent then the given
;;; position is used to allocate the baseline within the extra space available.
;;;
;;; box_layout :
;;;     a GtkBoxLayout
;;;
;;; position :
;;;     a GtkBaselinePosition
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_box_layout_get_baseline_position ()
;;;
;;; GtkBaselinePosition
;;; gtk_box_layout_get_baseline_position (GtkBoxLayout *box_layout);
;;;
;;; Gets the value set by gtk_box_layout_set_baseline_position().
;;;
;;; box_layout :
;;;     a GtkBoxLayout
;;;
;;; Returns :
;;;     the baseline position
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.box-layout.lisp ----------------------------------------
