;;; ----------------------------------------------------------------------------
;;; gtk.window-handle.lisp
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

(define-g-object-class "GtkWindowHandle" window-handle
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

;;; --- End of file gtk.window-handle.lisp -------------------------------------
