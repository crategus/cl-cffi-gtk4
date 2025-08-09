;;; ----------------------------------------------------------------------------
;;; gtk4.tree-list-row-sorter.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2025 Dieter Kaiser
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
;;; Accessors
;;;
;;;     gtk_tree_list_row_sorter_get_sorter
;;;     gtk_tree_list_row_sorter_set_sorter
;;;
;;; Functions
;;;
;;;     gtk_tree_list_row_sorter_new
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

(gobject:define-gobject "GtkTreeListRowSorter" tree-list-row-sorter
  (:superclass sorter
   :export t
   :interfaces nil
   :type-initializer "gtk_tree_list_row_sorter_get_type")
  ((sorter
    tree-list-row-sorter-sorter
    "sorter" "GtkSorter" t t)))

#+liber-documentation
(setf (documentation 'tree-list-row-sorter 'type)
 "@version{2025-04-16}
  @begin{short}
    The @class{gtk:tree-list-row-sorter} object is a special-purpose sorter that
    will apply a given sorter to the levels in a tree, while respecting the tree
    structure.
  @end{short}
  @begin[Examples]{dictionary}
  Here is an example for setting up a column view with a tree model and a
  @class{gtk:tree-list-row-sorter} object:
  @begin{pre}
column_sorter = gtk_column_view_get_sorter (view);
sorter = gtk_tree_list_row_sorter_new (g_object_ref (column_sorter));
sort_model = gtk_sort_list_model_new (tree_model, sorter);
selection = gtk_single_selection_new (sort_model);
gtk_column_view_set_model (view, G_LIST_MODEL (selection));
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:tree-list-row-sorter}
  @see-slot{gtk:tree-list-row-sorter-sorter}
  @see-class{gtk:tree-list-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:tree-list-row-sorter-sorter ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sorter"
                                               'tree-list-row-sorter) t)
 "The @code{sorter} property of type @class{gtk:sorter} (Read / Write) @br{}
  The underlying sorter.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-list-row-sorter-sorter)
      "Accessor"
      (documentation 'tree-list-row-sorter-sorter 'function)
 "@version{2025-04-16}
  @syntax{(gtk:tree-list-row-sorter-sorter object) => sorter}
  @syntax{(setf (gtk:tree-list-row-sorter-sorter object) sorter)}
  @argument[object]{a @class{gtk:tree-list-row-sorter} object}
  @argument[sorter]{a @class{gtk:sorter} object to use, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:tree-list-row-sorter]{sorter} slot of the
    @class{gtk:tree-list-row-sorter} class.
  @end{short}
  The @fun{gtk:tree-list-row-sorter-sorter} function returns the sorter used by
  @arg{object}. The @setf{gtk:tree-list-row-sorter-sorter} function sets the
  sorter to use for items with the same parent. This sorter will be passed the
  @slot[gtk:tree-list-row]{item} of the tree list rows passed to @arg{object}.
  @see-class{gtk:tree-list-row-sorter}
  @see-function{gtk:tree-list-row-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_list_row_sorter_new
;;; ----------------------------------------------------------------------------

(declaim (inline tree-list-row-sorter-new))

(defun tree-list-row-sorter-new (sorter)
 #+liber-documentation
 "@version{2025-04-16}
  @argument[sorter]{a @class{gtk:sorter} object}
  @return{The new @class{gtk:tree-list-row-sorter} object.}
  @begin{short}
    Create a special-purpose sorter that applies the sorting of sorter to the
    levels of a @class{gtk:tree-list-model} object.
  @end{short}
  Note that this sorter relies on the @slot[gtk:tree-list-model]{passthrough}
  property being @em{false} as it can only sort @class{gtk:tree-list-row}
  objects.
  @see-class{gtk:tree-list-sorter}
  @see-class{gtk:sorter}
  @see-class{gtk:tree-list-model}
  @see-class{gtk:tree-list-row}
  @see-function{gtk:tree-list-model-passthrough}"
  (make-instance 'tree-list-row-sorter
                 :sorter sorter))

(export 'tree-list-row-sorter-new)

;;; --- End of file gtk4.tree-list-row-sorter.lisp -----------------------------
