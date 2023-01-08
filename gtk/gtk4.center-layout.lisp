;;; ----------------------------------------------------------------------------
;;; gtk.center-layout.lisp
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkCenterLayout
;;;
;;;     A centering layout
;;;
;;; Types and Values
;;;
;;;     GtkCenterLayout
;;;
;;; Functions
;;;
;;;     gtk_center_layout_new
;;;     gtk_center_layout_set_orientation
;;;     gtk_center_layout_get_orientation
;;;     gtk_center_layout_set_baseline_position
;;;     gtk_center_layout_get_baseline_position
;;;     gtk_center_layout_set_start_widget
;;;     gtk_center_layout_get_start_widget
;;;     gtk_center_layout_set_center_widget
;;;     gtk_center_layout_get_center_widget
;;;     gtk_center_layout_set_end_widget
;;;     gtk_center_layout_get_end_widget
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkLayoutManager
;;;         ╰── GtkCenterLayout
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCenterLayout
;;;
;;; A GtkCenterLayout is a layout manager that manages up to three children.
;;; The start widget is allocated at the start of the layout (left in LRT
;;; layouts and right in RTL ones), and the end widget at the end.
;;;
;;; The center widget is centered regarding the full width of the layout's.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCenterLayout" center-layout
  (:superclass layout-manager
   :export t
   :interfaces ()
   :type-initializer "gtk_center_layout_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_new ()
;;;
;;; GtkLayoutManager *
;;; gtk_center_layout_new (void);
;;;
;;; Creates a new GtkCenterLayout.
;;;
;;; Returns :
;;;     the newly created GtkCenterLayout
;;;-----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_set_orientation ()
;;;
;;; void
;;; gtk_center_layout_set_orientation (GtkCenterLayout *self,
;;;                                    GtkOrientation orientation);
;;;
;;; Sets the orientation of self .
;;;
;;; self :
;;;     a GtkCenterLayout
;;;
;;; orientation :
;;;     the new orientation
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_get_orientation ()
;;;
;;; GtkOrientation
;;; gtk_center_layout_get_orientation (GtkCenterLayout *self);
;;;
;;; Gets the current orienration of the layout manager.
;;;
;;; self :
;;;     a GtkCenterLayout
;;;
;;; Returns :
;;;     The current orientation of self
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_set_baseline_position ()
;;;
;;; void
;;; gtk_center_layout_set_baseline_position
;;;                                (GtkCenterLayout *self,
;;;                                 GtkBaselinePosition baseline_position);
;;;
;;; Sets the new baseline position of self
;;;
;;; self :
;;;     a GtkCenterLayout
;;;
;;; baseline_position :
;;;     the new baseline position
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_get_baseline_position ()
;;;
;;; GtkBaselinePosition
;;; gtk_center_layout_get_baseline_position (GtkCenterLayout *self);
;;;
;;; Returns the baseline position of the layout.
;;;
;;; self :
;;;     a GtkCenterLayout
;;;
;;; Returns :
;;;     The current baseline position of self .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_set_start_widget ()
;;;
;;; void
;;; gtk_center_layout_set_start_widget (GtkCenterLayout *self,
;;;                                     GtkWidget *widget);
;;;
;;; Sets the new start widget of self .
;;;
;;; To remove the existing start widget, pass NULL.
;;;
;;; self :
;;;     a GtkCenterLayout
;;;
;;; widget :
;;;     the new start widget.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_get_start_widget ()
;;;
;;; GtkWidget *
;;; gtk_center_layout_get_start_widget (GtkCenterLayout *self);
;;;
;;; Returns the start widget fo the layout.
;;;
;;; self :
;;;     a GtkCenterLayout
;;;
;;; Returns :
;;;     The current start widget of self .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_set_center_widget ()
;;;
;;; void
;;; gtk_center_layout_set_center_widget (GtkCenterLayout *self,
;;;                                      GtkWidget *widget);
;;;
;;; Sets the new center widget of self .
;;;
;;; To remove the existing center widget, pass NULL.
;;;
;;; self :
;;;     a GtkCenterLayout
;;;
;;; widget :
;;;     the new center widget.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_get_center_widget ()
;;;
;;; GtkWidget *
;;; gtk_center_layout_get_center_widget (GtkCenterLayout *self);
;;;
;;; Returns the center widget of the layout.
;;;
;;; self :
;;;     a GtkCenterLayout
;;;
;;; Returns :
;;;     the current center widget of self .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_set_end_widget ()
;;;
;;; void
;;; gtk_center_layout_set_end_widget (GtkCenterLayout *self,
;;;                                   GtkWidget *widget);
;;;
;;; Sets the new end widget of self .
;;;
;;; To remove the existing center widget, pass NULL.
;;;
;;; self :
;;;     a GtkCenterLayout
;;;
;;; widget :
;;;     the new end widget.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_get_end_widget ()
;;;
;;; GtkWidget *
;;; gtk_center_layout_get_end_widget (GtkCenterLayout *self);
;;;
;;; Returns the end widget of the layout.
;;;
;;; self :
;;;     a GtkCenterLayout
;;;
;;; Returns :
;;;     the current end widget of self .
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.center-layout.lisp -------------------------------------
