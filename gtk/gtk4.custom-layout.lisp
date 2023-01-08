;;; ----------------------------------------------------------------------------
;;; gtk.custom-layout.lisp
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
;;; GtkCustomLayout
;;;
;;;     A convenience layout manager
;;;
;;; Types and Values
;;;
;;;     GtkCustomLayout
;;;
;;; Functions
;;;
;;;     GtkCustomRequestModeFunc
;;;     GtkCustomMeasureFunc
;;;     GtkCustomAllocateFunc
;;;
;;;     gtk_custom_layout_new
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkLayoutManager
;;;         ╰── GtkCustomLayout
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCustomLayout
;;;
;;; GtkCustomLayout is a convenience type meant to be used as a transition
;;; mechanism between GtkWidgets implementing a layout policy, and
;;; GtkLayoutManager classes.
;;;
;;; A GtkCustomLayout uses closures matching to the old GtkWidget virtual
;;; functions for size negotiation, as a convenience API to ease the porting
;;; towards the corresponding GtkLayoutManager virtual functions.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCustomLayout" custom-layout
  (:superclass layout-manager
   :export t
   :interfaces ()
   :type-initializer "gtk_custom_layout_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GtkCustomRequestModeFunc ()
;;;
;;; GtkSizeRequestMode
;;; (*GtkCustomRequestModeFunc) (GtkWidget *widget);
;;;
;;; Queries a widget for its preferred size request mode.
;;;
;;; widget :
;;;     the widget to be queried
;;;
;;; Returns :
;;;     the size request mode
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkCustomMeasureFunc ()
;;;
;;; void
;;; (*GtkCustomMeasureFunc) (GtkWidget *widget,
;;;                          GtkOrientation orientation,
;;;                          int for_size,
;;;                          int *minimum,
;;;                          int *natural,
;;;                          int *minimum_baseline,
;;;                          int *natural_baseline);
;;;
;;; A function to be used by GtkCustomLayout to measure a widget.
;;;
;;; widget :
;;;     the widget to be measured
;;;
;;; orientation :
;;;     the direction to be measured
;;;
;;; for_size :
;;;     the size to be measured for
;;;
;;; minimum :
;;;     the measured minimum size of the widget.
;;;
;;; natural :
;;;     the measured natural size of the widget.
;;;
;;; minimum_baseline :
;;;     the measured minimum baseline of the widget.
;;;
;;; natural_baseline :
;;;     the measured natural baseline of the widget.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkCustomAllocateFunc ()
;;;
;;; void
;;; (*GtkCustomAllocateFunc) (GtkWidget *widget,
;;;                           int width,
;;;                           int height,
;;;                           int baseline);
;;;
;;; A function to be used by GtkCustomLayout to allocate a widget.
;;;
;;; widget :
;;;     the widget to allocate
;;;
;;; width :
;;;     the new width of the widget
;;;
;;; height :
;;;     the new height of the widget
;;;
;;; baseline :
;;;     the new baseline of the widget, or -1
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_custom_layout_new ()
;;;
;;; GtkLayoutManager *
;;; gtk_custom_layout_new (GtkCustomRequestModeFunc request_mode,
;;;                        GtkCustomMeasureFunc measure,
;;;                        GtkCustomAllocateFunc allocate);
;;;
;;; Creates a new legacy layout manager.
;;;
;;; Legacy layout managers map to the old GtkWidget size negotiation virtual 
;;; functions, and are meant to be used during the transition from layout 
;;; containers to layout manager delegates.
;;;
;;; request_mode :
;;;     a function to retrieve the GtkSizeRequestMode of the widget using the 
;;;     layout; the default request mode is GTK_SIZE_REQUEST_CONSTANT_SIZE.
;;;
;;; measure :
;;;     a function to measure the widget using the layout manager.
;;;
;;; allocate :
;;;     a function to allocate the children of the widget using the layout
;;;     manager.
;;;
;;; Returns :
;;;     the newly created GtkCustomLayout.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.custom-layout.lisp -------------------------------------
