;;; ----------------------------------------------------------------------------
;;; gtk4.multi-sorter.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;; GtkMultiSorter
;;;
;;;     Combining multiple sorters
;;;
;;; Types and Values
;;;
;;;     GtkMultiSorter
;;;
;;; Functions
;;;
;;;     gtk_multi_sorter_new
;;;     gtk_multi_sorter_append
;;;     gtk_multi_sorter_remove
;;;
;;; Properties
;;;
;;;     item-type                                          Since 4.8
;;;     n-items                                            Since 4.8
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSorter
;;;         ╰── GtkMultiSorter
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;;     GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkMultiSorter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkMultiSorter" multi-sorter
  (:superclass sorter
   :export t
   :interfaces ("GListModel"
                "GtkBuildable")
   :type-initializer "gtk_multi_sorter_get_type")
  (#+gtk-4-8
   (item-type
    multi-sorter-item-type
    "item-type" "GType" t nil)
   #+gtk-4-8
   (n-items
    multi-sorter-n-items
    "n-items" "guint" t nil)))




;;;Description
;;;GtkMultiSorter combines multiple sorters by trying them in turn. If the first sorter compares two items as equal, the second is tried next, and so on.

;;;Functions
;;;gtk_multi_sorter_new ()
;;;GtkMultiSorter *
;;;gtk_multi_sorter_new (void);
;;;Creates a new multi sorter.

;;;This sorter compares items by trying each of the sorters in turn, until one returns non-zero. In particular, if no sorter has been added to it, it will always compare items as equal.

;;;Returns
;;;a new GtkMultiSorter

;;;gtk_multi_sorter_append ()
;;;void
;;;gtk_multi_sorter_append (GtkMultiSorter *self,
;;;                         GtkSorter *sorter);
;;;Add sorter to self to use for sorting at the end. self will consult all existing sorters before it will sort with the given sorter .

;;;Parameters
;;;self

;;;a GtkMultiSorter

;;;sorter

;;;a sorter to add.

;;;[transfer full]
;;;gtk_multi_sorter_remove ()
;;;void
;;;gtk_multi_sorter_remove (GtkMultiSorter *self,
;;;                         guint position);
;;;Removes the sorter at the given position from the list of sorter used by self .

;;;If position is larger than the number of sorters, nothing happens.

;;;Parameters
;;;self

;;;a GtkMultiSorter

;;;position

;;;position of sorter to remove


;;; --- End of file gtk4.multi-sorter.lisp -------------------------------------
