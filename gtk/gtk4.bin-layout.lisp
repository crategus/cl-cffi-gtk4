;;; ----------------------------------------------------------------------------
;;; gtk.bin-layout.lisp
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
;;; GtkBinLayout
;;;
;;;     A layout manager for bin-like widgets
;;;
;;; Types and Values
;;;
;;;     GtkBinLayout
;;;
;;; Functions
;;;
;;;      gtk_bin_layout_new
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkLayoutManager
;;;         ╰── GtkBinLayout
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBinLayout
;;;
;;; GtkBinLayout is a GtkLayoutManager subclass useful for create "bins" of
;;; widgets. GtkBinLayout will stack each child of a widget on top of each
;;; other, using the “hexpand”, “vexpand”, “halign”, and “valign” properties of
;;; each child to determine where they should be positioned.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkBinLayout" bin-layout
  (:superclass layout-manager
   :export t
   :interfaces ()
   :type-initializer "gtk_bin_layout_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_bin_layout_new ()
;;;
;;; GtkLayoutManager *
;;; gtk_bin_layout_new (void);
;;;
;;; Creates a new GtkBinLayout instance.
;;;
;;; Returns :
;;;     the newly created GtkBinLayout
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.bin-layout.lisp ----------------------------------------
