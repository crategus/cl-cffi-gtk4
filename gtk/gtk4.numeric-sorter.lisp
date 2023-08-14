;;; ----------------------------------------------------------------------------
;;; gtk4.numeric-sorter.lisp
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
;;; GtkNumericSorter
;;;
;;;     Sort by comparing numbers
;;;
;;; Types and Values
;;;
;;;     GtkNumericSorter
;;;
;;; Functions
;;;
;;;     gtk_numeric_sorter_new
;;;     gtk_numeric_sorter_get_expression
;;;     gtk_numeric_sorter_set_expression
;;;     gtk_numeric_sorter_get_sort_order
;;;     gtk_numeric_sorter_set_sort_order
;;;
;;; Properties
;;;
;;;     expression
;;;     sort-order
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSorter
;;;         ╰── GtkNumericSorter
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkNumericSorter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkNumericSorter" numeric-sorter
  (:superclass sorter
   :export t
   :interfaces ()
   :type-initializer "gtk_numeric_sorter_get_type")
  ((expression
    numeric-sorter-expression
    "expression" "GtkExpression" t t)
   (sort-order
    numeric-sorter-sort-order
    "sort-order" "GtkSortType" t t)))




;;;Property Details
;;;The “expression” property
;;;  “expression”               GtkExpression *
;;;The expression to evaluate on items to get a number to compare with

;;;[type GtkExpression]

;;;Owner: GtkNumericSorter

;;;Flags: Read / Write

;;;The “sort-order” property
;;;  “sort-order”               GtkSortType
;;;Whether the sorter will sort smaller numbers first

;;;Owner: GtkNumericSorter

;;;Flags: Read / Write

;;;Default value: GTK_SORT_ASCENDING

;;;See Also
;;;GtkExpression


;;;Description
;;;GtkNumericSorter is a GtkSorter that compares numbers.

;;;To obtain the numbers to compare, this sorter evaluates a GtkExpression.

;;;Functions
;;;gtk_numeric_sorter_new ()
;;;GtkNumericSorter *
;;;gtk_numeric_sorter_new (GtkExpression *expression);
;;;Creates a new numeric sorter using the given expression .

;;;Smaller numbers will be sorted first. You can call gtk_numeric_sorter_set_sort_order() to change this.

;;;Parameters
;;;expression

;;;The expression to evaluate.

;;;[transfer full][nullable]
;;;Returns
;;;a new GtkNumericSorter

;;;gtk_numeric_sorter_get_expression ()
;;;GtkExpression *
;;;gtk_numeric_sorter_get_expression (GtkNumericSorter *self);
;;;Gets the expression that is evaluated to obtain numbers from items.

;;;Parameters
;;;self

;;;a GtkNumericSorter

;;;Returns
;;;a GtkExpression, or NULL.

;;;[transfer none][nullable]

;;;gtk_numeric_sorter_set_expression ()
;;;void
;;;gtk_numeric_sorter_set_expression (GtkNumericSorter *self,
;;;                                   GtkExpression *expression);
;;;Sets the expression that is evaluated to obtain numbers from items.

;;;Unless an expression is set on self , the sorter will always compare items as invalid.

;;;The expression must have a return type that can be compared numerically, such as G_TYPE_INT or G_TYPE_DOUBLE.

;;;Parameters
;;;self

;;;a GtkNumericSorter

;;;expression

;;;a GtkExpression, or NULL.

;;;[nullable][transfer none]
;;;gtk_numeric_sorter_get_sort_order ()
;;;GtkSortType
;;;gtk_numeric_sorter_get_sort_order (GtkNumericSorter *self);
;;;Gets whether this sorter will sort smaller numbers first.

;;;Parameters
;;;self

;;;a GtkNumericSorter

;;;Returns
;;;the order of the numbers

;;;gtk_numeric_sorter_set_sort_order ()
;;;void
;;;gtk_numeric_sorter_set_sort_order (GtkNumericSorter *self,
;;;                                   GtkSortType sort_order);
;;;Sets whether to sort smaller numbers before larger ones.

;;;Parameters
;;;self

;;;a GtkNumericSorter

;;;sort_order

;;;whether to sort smaller numbers first


;;; --- End of file gtk4.numeric-sorter.lisp -----------------------------------
