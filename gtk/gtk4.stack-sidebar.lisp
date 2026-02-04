;;; ----------------------------------------------------------------------------
;;; gtk4.stack-sidebar.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2026 Dieter Kaiser
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
;;; GtkStackSidebar
;;;
;;;     An automatic sidebar widget
;;;
;;; Types and Values
;;;
;;;     GtkStackSidebar
;;;
;;; Accessors
;;;
;;;     gtk_stack_sidebar_set_stack
;;;     gtk_stack_sidebar_get_stack
;;;
;;; Functions
;;;
;;;     gtk_stack_sidebar_new
;;;
;;; Properties
;;;
;;;     stack
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkStackSidebar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkStackSidebar
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkStackSidebar" stack-sidebar
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_stack_sidebar_get_type")
  ((stack
    stack-sidebar-stack
    "stack" "GtkStack" t t)))

#+liber-documentation
(setf (documentation 'stack-sidebar 'type)
 "@version{2025-04-23}
  @begin{short}
    The @class{gtk:stack-sidebar} widget enables you to quickly and easily
    provide a consistent sidebar  object for your user interface.
  @end{short}

  @image[stack-sidebar]{Figure: GtkStackSidebar}

  In order to use the @class{gtk:stack-sidebar} widget, you simply use the
  @class{gtk:stack} widget to organize your UI flow, and add the sidebar to
  your sidebar area. You can use the @fun{gtk:stack-sidebar-stack} function to
  connect the @class{gtk:stack-sidebar} widget to the @class{gtk:stack} widget.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:stack-sidebar} implementation has a single CSS node with name
    stacksidebar and @code{.sidebar} style class. When circumstances require it,
    the @class{gtk:stack-sidebar} widget adds the @code{.needs-attention} style
    class to the widgets representing the stack pages.
  @end{dictionary}
  @see-constructor{gtk:stack-sidebar-new}
  @see-slot{gtk:stack-sidebar-stack}
  @see-class{gtk:stack}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:stack-sidebar-stack ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "stack" 'stack-sidebar) t)
 "The @code{stack} property of type @class{gtk:stack} (Read / Write) @br{}
  The associated stack for this stack sidebar.")

#+liber-documentation
(setf (liber:alias-for-function 'stack-sidebar-stack)
      "Accessor"
      (documentation 'stack-sidebar-stack 'function)
 "@version{2025-07-30}
  @syntax{(gtk:stack-sidebar-stack object) => stack}
  @syntax{(setf (gtk:stack-sidebar-stack object) stack)}
  @argument[object]{a @class{gtk:stack-sidebar} widget}
  @argument[stack]{a @class{gtk:stack} widget}
  @begin{short}
    The accessor for the @slot[gtk:stack-sidebar]{stack} slot of the
    @class{gtk:stack-sidebar} class gets or sets the associated stack for the
    sidebar.
  @end{short}

  The stack sidebar will automatically update according to the order (packing)
  and items within the given stack.
  @see-class{gtk:stack-sidebar}
  @see-class{gtk:stack}")

;;; ----------------------------------------------------------------------------
;;; gtk_stack_sidebar_new
;;; ----------------------------------------------------------------------------

(declaim (inline stack-sidebar-new))

(defun stack-sidebar-new ()
 #+liber-documentation
 "@version{2025-04-23}
  @return{The new @class{gtk:stack-sidebar} widget.}
  @short{Creates a new stack sidebar.}
  @see-class{gtk:stack-sidebar}"
  (make-instance 'stack-sidebar))

(export 'stack-sidebar-new)

;;; --- End of file gtk4.stack-sidebar.lisp ------------------------------------
