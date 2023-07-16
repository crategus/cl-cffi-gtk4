;;; ----------------------------------------------------------------------------
;;; gtk4.center-layout.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GTK library.
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
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkCenterLayout" center-layout
  (:superclass layout-manager
   :export t
   :interfaces ()
   :type-initializer "gtk_center_layout_get_type")
  nil)

#+liber-documentation
(setf (documentation 'center-layout 'type)
 "@version{#2023-4-18}
  @begin{short}
    A @sym{gtk:center-layout} class is a layout manager that manages up to
    three children.
  @end{short}
  The start widget is allocated at the start of the layout (left in LRT layouts
  and right in RTL ones), and the end widget at the end. The center widget is
  centered regarding the full width of the layout.
  @see-class{gtk:layout-manager}")

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_new ()
;;;-----------------------------------------------------------------------------

(defun center-layout-new ()
 #+liber-documentation
 "@version{#2023-4-18}
  @return{The newly created @class{gtk:center-layout} object.}
  @short{Creates a new center layout manager.}
  @see-class{gtk:center-layout}"
  (make-instance 'center-layout))

(export 'center-layout-new)

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_set_orientation ()
;;; gtk_center_layout_get_orientation ()
;;; ----------------------------------------------------------------------------

(defun (setf center-layout-orientation) (orientation layout)
  (cffi:foreign-funcall "gdk_center_layout_set_orientation"
                        (g:object center-layout) layout
                        orientation orientation
                        :void)
  orientation)

(cffi:defcfun ("gdk_center_layout_get_orientation" center-layout-orientation)
    orientation
 #+liber-documentation
 "@version{#2023-4-19}
  @syntax[]{(gtk:center-layout-orientation layout) => orientation}
  @syntax[]{(setf (gtk:center-layout-orientation layout) orientation)}
  @argument[layout]{a @class{gtk:center-layout} object}
  @argument[orientation]{a @symbol{gtk:orientation} value}
  @begin{short}
    The @sym{gtk:center-layout-orientation} function gets the current
    orienration of the layout manager.
  @end{short}
  The @sym{(setf gtk:center-layout-orientation)} function sets the orientation.
  @see-class{gtk:center-layout}
  @see-symbol{gtk:orientation}"
  (layout (g:object center-layout)))

(export 'center-layout-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_set_baseline_position ()
;;; gtk_center_layout_get_baseline_position ()
;;; ----------------------------------------------------------------------------

(defun (setf center-layout-baseline-position) (position layout)
  (cffi:foreign-funcall "gdk_center_layout_set_baseline_position"
                        (g:object center-layout) layout
                        baseline-position position
                        :void)
  position)

(cffi:defcfun ("gdk_center_layout_get_baseline_position"
               center-layout-baseline-position) baseline-position
 #+liber-documentation
 "@version{#2023-4-19}
  @syntax[]{(gtk:center-layout-baseline-position layout) => position}
  @syntax[]{(setf (gtk:center-layout-baseline-position layout) position)}
  @argument[layout]{a @class{gtk:center-layout} object}
  @argument[position]{a @symbol{gtk:baseline-position} value}
  @begin{short}
    The @sym{gtk:center-layout-baseline-position} function gets the current
    baseline position of the layout manager.
  @end{short}
  The @sym{(setf gtk:center-layout-baseline-position)} function sets the
  baseline-position.
  @see-class{gtk:center-layout}
  @see-symbol{gtk:baseline-position}"
  (layout (g:object center-layout)))

(export 'center-layout-baseline-position)

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_set_start_widget ()
;;; gtk_center_layout_get_start_widget ()
;;; ----------------------------------------------------------------------------

(defun (setf center-layout-start-widget) (widget layout)
  (cffi:foreign-funcall "gdk_center_layout_set_start_widget"
                        (g:object center-layout) layout
                        (g:object widget) widget
                        :void)
  widget)

(export 'center-layout-start-widget)

(cffi:defcfun ("gdk_center_layout_get_start_widget" center-layout-start-widget)
    (g:object widget)
 #+liber-documentation
 "@version{#2023-4-19}
  @syntax[]{(gtk:center-layout-start-widget layout) => widget}
  @syntax[]{(setf (gtk:center-layout-start-widget layout) widget)}
  @argument[layout]{a @class{gtk:center-layout} object}
  @argument[widget]{a @class{gtk:widget} start widget}
  @begin{short}
    The @sym{gtk:center-layout-start-widget} function returns the start widget
     of the layout.
  @end{short}
  The @sym{(setf gtk:center-layout-start-widget)} function sets the start
  widget.
  @see-class{gtk:center-layout}
  @see-symbol{gtk:widget}"
  (layout (g:object center-layout)))

(export 'center-layout-start-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_set_center_widget ()
;;; gtk_center_layout_get_center_widget ()
;;; ----------------------------------------------------------------------------

(defun (setf center-layout-center-widget) (widget layout)
  (cffi:foreign-funcall "gdk_center_layout_set_center_widget"
                        (g:object center-layout) layout
                        (g:object widget) widget
                        :void)
  widget)

(cffi:defcfun ("gtk_center_layout_get_center_widget"
               center-layout-center-widget) (g:object widget)
 #+liber-documentation
 "@version{#2023-4-19}
  @syntax[]{(gtk:center-layout-center-widget layout) => widget}
  @syntax[]{(setf (gtk:center-layout-center-widget layout) widget)}
  @argument[layout]{a @class{gtk:center-layout} object}
  @argument[widget]{a @class{gtk:widget} center widget}
  @begin{short}
    The @sym{gtk:center-layout-center-widget} function returns the center
     widget of the layout.
  @end{short}
  The @sym{(setf gtk:center-layout-center-widget)} function sets the
  center widget.
  @see-class{gtk:center-layout}
  @see-symbol{gtk:widget}"
  (layout (g:object center-layout)))

(export 'center-layout-center-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_center_layout_set_end_widget ()
;;; gtk_center_layout_get_end_widget ()
;;; ----------------------------------------------------------------------------

(defun (setf center-layout-end-widget) (widget layout)
  (cffi:foreign-funcall "gdk_center_layout_set_end_widget"
                        (g:object center-layout) layout
                        (g:object widget) widget
                        :void)
  widget)

(cffi:defcfun ("gtk_center_layout_get_end_widget" center-layout-end-widget)
    (g:object widget)
 #+liber-documentation
 "@version{#2023-4-19}
  @syntax[]{(gtk:center-layout-end-widget layout) => widget}
  @syntax[]{(setf (gtk:center-layout-end-widget layout) widget)}
  @argument[layout]{a @class{gtk:center-layout} object}
  @argument[widget]{a @class{gtk:widget} center widget}
  @begin{short}
    The @sym{gtk:center-layout-end-widget} function returns the end widget of
    the layout.
  @end{short}
  The @sym{(setf gtk:center-layout-end-widget)} function sets the end widget.
  @see-class{gtk:center-layout}
  @see-symbol{gtk:widget}"
  (layout (g:object center-layout)))

(export 'center-layout-end-widget)

;;; --- End of file gtk4.center-layout.lisp ------------------------------------
