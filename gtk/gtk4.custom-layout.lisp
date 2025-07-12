;;; ----------------------------------------------------------------------------
;;; gtk4.custom-layout.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
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
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkCustomLayout" custom-layout
  (:superclass layout-manager
   :export t
   :interfaces ()
   :type-initializer "gtk_custom_layout_get_type")
  nil)

#+liber-documentation
(setf (documentation 'custom-layout 'type)
 "@version{2024-04-23}
  @begin{short}
    The @class{gtk:custom-layout} class is a convenience type meant to be used
    as a transition mechanism between @class{gtk:widget} objects implementing a
    layout policy, and @class{gtk:layout-manager} classes.
  @end{short}

  A @class{gtk:custom-layout} object uses closures matching to the old
  @class{gtk:widget} virtual functions for size negotiation, as a convenience
  API to ease the porting towards the corresponding @class{gtk:layout-manager}
  virtual functions.
  @see-constructor{gtk:custom-layout-new}")

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

;;; --- End of file gtk4.custom-layout.lisp ------------------------------------
