;;; ----------------------------------------------------------------------------
;;; gtk4.numeric-sorter.lisp
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
;;; GtkNumericSorter
;;;
;;;     Sort by comparing numbers
;;;
;;; Types and Values
;;;
;;;     GtkNumericSorter
;;;
;;; Accessors
;;;
;;;     gtk_numeric_sorter_get_expression
;;;     gtk_numeric_sorter_set_expression
;;;     gtk_numeric_sorter_get_sort_order
;;;     gtk_numeric_sorter_set_sort_order
;;;
;;; Functions
;;;
;;;     gtk_numeric_sorter_new
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

(gobject:define-gobject "GtkNumericSorter" numeric-sorter
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

#+liber-documentation
(setf (documentation 'numeric-sorter 'type)
 "@version{2025-07-27}
  @begin{short}
    The @class{gtk:numeric-sorter} object is a @class{gtk:sorter} implementation
    that compares numbers.
  @end{short}
  To obtain the numbers to compare, this sorter evaluates a
  @class{gtk:expression} instance.
  @see-constructor{gtk:numeric-sorter-new}
  @see-slot{gtk:numeric-sorter-expression}
  @see-slot{gtk:numeric-sorter-sort-order}
  @see-class{gtk:sorter}
  @see-class{gtk:expression}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:numeric-sorter-expression ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expression" 'numeric-sorter) t)
 "The @code{expression} property of type @class{gtk:expression} (Read / Write)
  @br{}
  The expression to evaluate on items to get a number to compare with.")

#+liber-documentation
(setf (liber:alias-for-function 'numeric-sorter-expression)
      "Accessor"
      (documentation 'numeric-sorter-expression 'function)
 "@version{2025-09-29}
  @syntax{(gtk:numeric-sorter-expression object) => expression}
  @syntax{(setf (gtk:numeric-sorter-expression object) expression)}
  @argument[object]{a @class{gtk:numeric-sorter} object}
  @argument[expression]{a @class{gtk:expression} instance, or @code{nil}}
  @begin{short}
    The accessor for the @slot[gtk:numeric-sorter]{expression} slot of the
    @class{gtk:numeric-sorter} class gets or sets the expression that is
    evaluated to obtain numbers from items.
  @end{short}

  Unless an expression is set on @arg{object}, the sorter will always compare
  items as invalid. The expression must have a return type that can be compared
  numerically.
  @see-class{gtk:numeric-sorter}
  @see-class{gtk:expression}")

;;; --- gtk:numeric-sorter-sort-order ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sort-order" 'numeric-sorter) t)
 "The @code{sort-order} property of type @sym{gtk:sort-type} (Read / Write)
  @br{}
  Whether the sorter will sort smaller numbers first. @br{}
  Default value: @val[gtk:sort-type]{:ascending}")

#+liber-documentation
(setf (liber:alias-for-function 'numeric-sorter-sort-order)
      "Accessor"
      (documentation 'numeric-sorter-sort-order 'function)
 "@version{2025-09-29}
  @syntax{(gtk:numeric-sorter-sort-order object) => order}
  @syntax{(setf (gtk:numeric-sorter-sort-order object) order)}
  @argument[object]{a @class{gtk:numeric-sorter} object}
  @argument[order]{a @sym{gtk:sort-type} value}
  @begin{short}
    The accessor for the @slot[gtk:numeric-sorter]{sort-order} slot of the
    @class{gtk:numeric-sorter} class gets or sets whether this sorter will sort
    smaller numbers first.
  @end{short}
  @see-class{gtk:numeric-sorter}
  @see-symbol{gtk:sort-type}")

;;; ----------------------------------------------------------------------------
;;; gtk_numeric_sorter_new
;;; ----------------------------------------------------------------------------

(defun numeric-sorter-new (expression)
 #+liber-documentation
 "@version{2024-10-24}
  @argument[expression]{a @class{gtk:expression} instance to evalute}
  @return{The new @class{gtk:numeric-sorter} object.}
  @begin{short}
    Creates a new numeric sorter using the given @arg{expression}.
  @end{short}
  Smaller numbers will be sorted first. You can call the
  @fun{gtk:numeric-sorter-sort-order} function to change this.
  @see-class{gtk:numeric-sorter}
  @see-class{gtk:expression}"
  (make-instance 'numeric-sorter
                 :expression (or expression (cffi:null-pointer))))

(export 'numeric-sorter-new)

;;; --- End of file gtk4.numeric-sorter.lisp -----------------------------------
