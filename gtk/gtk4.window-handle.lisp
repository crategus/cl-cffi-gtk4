;;; ----------------------------------------------------------------------------
;;; gtk4.window-handle.lisp
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
;;; GtkWindowHandle
;;;
;;;     A titlebar area widget
;;;
;;; Types and Values
;;;
;;;     GtkWindowHandle
;;;
;;; Accessors
;;;
;;;     gtk_window_handle_get_child
;;;     gtk_window_handle_set_child
;;;
;;; Functions
;;;
;;;     gtk_window_handle_new
;;;
;;; Properties
;;;
;;;     child
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkWindowHandle
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkWindowHandle
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkWindowHandle" window-handle
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_window_handle_get_type")
  ((child
    window-handle-child
    "child" "GtkWidget" t t)))

#+liber-documentation
(setf (documentation 'window-handle 'type)
 "@version{2026-01-31}
  @begin{short}
    The @class{gtk:window-handle} widget is a titlebar area widget.
  @end{short}
  When added into a window, it can be dragged to move the window, and handles
  right click, double click and middle click as expected of a titlebar.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:window-handle} implementation has a single CSS node with the
    name @code{windowhandle}.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    Until GTK 4.10, the @class{gtk:window-handle} implementation uses the
    @val[gtk:accessible-role]{:group} role. Starting from GTK 4.12, the
    implementation uses the @val[gtk:accessible-role]{:generic} role.
  @end{dictionary}
  @see-class{gtk:window}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:window-handle-child ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'window-handle) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'window-handle-child)
      "Accessor"
      (documentation 'window-handle-child 'function)
 "@version{2026-01-31}
  @syntax{(gtk:window-handle-child object) => child}
  @syntax{(setf (gtk:window-handle-child object) child)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    The accessor for the @slot[gtk:window-handle]{child} slot of the
    @class{gtk:window-handle} class gets or sets the child widget of the window
    handle.
  @end{short}
  @see-class{gtk:window}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_window_handle_new
;;; ----------------------------------------------------------------------------

(declaim (inline window-handle-new))

(defun window-handle-new ()
 #+liber-documentation
 "@version{2026-01-31}
  @return{The new @class{gtk:window-handle} widget.}
  @short{Creates new @class{gtk:window-handle} widget.}
  @see-class{gtk:window-handle}"
  (make-instance 'window-handle))

(export 'window-handle-new)

;;; --- End of file gtk4.window-handle.lisp ------------------------------------
