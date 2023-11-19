;;; ----------------------------------------------------------------------------
;;; gtk4.list-header.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GtkListHeader
;;;
;;; Functions
;;;
;;;     gtk_list_header_get_child
;;;     gtk_list_header_set_child
;;;     gtk_list_header_get_end
;;;     gtk_list_header_get_item
;;;     gtk_list_header_get_n_items
;;;     gtk_list_header_get_start
;;;
;;; Properties
;;;
;;;     child
;;;     end
;;;     item
;;;     n-items
;;;     start
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkListHeader
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkListHeader
;;;
;;; GtkListHeader is used by list widgets to represent the headers they display.
;;;
;;; GtkListHeader objects are managed just like GtkListItem objects via their
;;; factory, but provide a different set of properties suitable for managing the
;;; header instead of individual items.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Gtk.ListHeader:child
;;;
;;; Widget used for display.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Gtk.ListHeader:end
;;;
;;; The first position no longer part of this section.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Gtk.ListHeader:item
;;;
;;; The item at the start of the section.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Gtk.ListHeader:n-items
;;;
;;; Number of items in this section.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Gtk.ListHeader:start
;;;
;;; First position of items in this section.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_header_get_child
;;;
;;; Gets the child previously set via gtk_list_header_set_child() or NULL if
;;; none was set.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_header_set_child
;;;
;;; Sets the child to be used for this listitem.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_header_get_end
;;;
;;; Gets the end position in the model of the section that self is currently the
;;; header for.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_header_get_item
;;;
;;; Gets the model item at the start of the section. This is the item that
;;; occupies the list model at position GtkListHeader:start.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_header_get_n_items
;;;
;;; Gets the the number of items in the section.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_header_get_start
;;;
;;; Gets the start position in the model of the section that self is currently
;;; the header for.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.list-header.lisp --------------------------------------
