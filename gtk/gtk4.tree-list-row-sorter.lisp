;;; ----------------------------------------------------------------------------
;;; gtk4.tree-list-row-sorter.lisp
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
;;; GtkTreeListRowSorter
;;;
;;;     Sort trees by levels
;;;
;;; Types and Values
;;;
;;;     GtkTreeListRowSorter
;;;
;;; Functions
;;;
;;;     gtk_tree_list_row_sorter_new 
;;;     gtk_tree_list_row_sorter_get_sorter 
;;;     gtk_tree_list_row_sorter_set_sorter 
;;;
;;; Properties 
;;;
;;;     sorter
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSorter
;;;         ╰── GtkTreeListRowSorter
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTreeListRowSorter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkTreeListRowSorter" tree-list-row-sorter
  (:superclass sorter
   :export t
   :interfaces nil
   :type-initializer "gtk_tree_list_row_sorter_get_type")
  ((sorter
    tree-list-row-sorter-sorter
    "sorter" "GtkSorter" t t)))


;;;Property Details
;;;The “sorter” property
;;;  “sorter”                   GtkSorter *
;;;The underlying sorter

;;;Owner: GtkTreeListRowSorter

;;;Flags: Read / Write

;;;See Also
;;;GtkTreeListModel


;;;Description
;;;GtkTreeListRowSorter is a special-purpose sorter that will apply a given sorter to the levels in a tree, while respecting the tree structure.

;;;Here is an example for setting up a column view with a tree model and a GtkTreeListSorter:

;;;column_sorter = gtk_column_view_get_sorter (view);
;;;sorter = gtk_tree_list_row_sorter_new (g_object_ref (column_sorter));
;;;sort_model = gtk_sort_list_model_new (tree_model, sorter);
;;;selection = gtk_single_selection_new (sort_model);
;;;gtk_column_view_set_model (view, G_LIST_MODEL (selection));
;;;Functions
;;;gtk_tree_list_row_sorter_new ()
;;;GtkTreeListRowSorter *
;;;gtk_tree_list_row_sorter_new (GtkSorter *sorter);
;;;Create a special-purpose sorter that applies the sorting of sorter to the levels of a GtkTreeListModel.

;;;Note that this sorter relies on “passthrough” being FALSE as it can only sort GtkTreeListRows.

;;;Parameters
;;;sorter

;;;a GtkSorter, or NULL.

;;;[nullable][transfer full]
;;;Returns
;;;a new GtkTreeListRowSorter

;;;gtk_tree_list_row_sorter_get_sorter ()
;;;GtkSorter *
;;;gtk_tree_list_row_sorter_get_sorter (GtkTreeListRowSorter *self);
;;;Returns the sorter used by self .

;;;Parameters
;;;self

;;;a GtkTreeListRowSorter

;;;Returns
;;;the sorter used.

;;;[transfer none][nullable]

;;;gtk_tree_list_row_sorter_set_sorter ()
;;;void
;;;gtk_tree_list_row_sorter_set_sorter (GtkTreeListRowSorter *self,
;;;                                     GtkSorter *sorter);
;;;Sets the sorter to use for items with the same parent.

;;;This sorter will be passed the “item” of the tree list rows passed to self .

;;;Parameters
;;;self

;;;a GtkTreeListRowSorter

;;;sorter

;;;The sorter to use, or NULL.

;;;[nullable][transfer none]

;;; --- End of file gtk4.tree-list-row-sorter.lisp -----------------------------
