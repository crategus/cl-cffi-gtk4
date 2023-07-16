;;; ----------------------------------------------------------------------------
;;; gtk4.window-handle.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;;
;;; GtkWindowHandle is a titlebar area widget. When added into a window, it can
;;; be dragged to move the window, and handles right click, double click and
;;; middle click as expected of a titlebar.
;;;
;;; CSS nodes
;;;
;;; GtkWindowHandle has a single CSS node with the name windowhandle.
;;;
;;; Accessibility
;;;
;;; GtkWindowHandle uses the GTK_ACCESSIBLE_ROLE_GROUP role.
;;;
;;; See Also
;;;
;;;     GtkWindow
;;;     GtkHeaderBar
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkWindowHandle" window-handle
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_window_handle_get_type")
  ((child
    window-handle-child
    "child" "GtkWidget" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “child” property
;;;
;;;  “child”                    GtkWidget *
;;;
;;; The child widget.
;;;
;;; Owner: GtkWindowHandle
;;;
;;; Flags: Read / Write
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gtk_window_handle_new ()
;;;
;;; GtkWidget *
;;; gtk_window_handle_new (void);
;;;
;;; Creates a new GtkWindowHandle.
;;;
;;; Returns :
;;;     a new GtkWindowHandle.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_handle_get_child ()
;;;
;;; GtkWidget *
;;; gtk_window_handle_get_child (GtkWindowHandle *self);
;;;
;;; Gets the child widget of self .
;;;
;;; self :
;;;     a GtkWindowHandle
;;;
;;; Returns :
;;;     the child widget of self .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_handle_set_child ()
;;;
;;; void
;;; gtk_window_handle_set_child (GtkWindowHandle *self,
;;;                              GtkWidget *child);
;;;
;;; Sets the child widget of self .
;;;
;;; self :
;;;     a GtkWindowHandle
;;;
;;; child :
;;;     the child widget.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.window-handle.lisp ------------------------------------
