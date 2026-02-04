;;; ----------------------------------------------------------------------------
;;; gtk4.bool-filter.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2026 Dieter Kaiser
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
;;; GtkBoolFilter
;;;
;;;     Filtering by boolean expressions
;;;
;;; Types and Values
;;;
;;;     GtkBoolFilter
;;;
;;; Accessors
;;;
;;;     gtk_bool_filter_get_expression
;;;     gtk_bool_filter_set_expression
;;;     gtk_bool_filter_get_invert
;;;     gtk_bool_filter_set_invert
;;;
;;; Functions
;;;
;;;     gtk_bool_filter_new
;;;
;;; Properties
;;;
;;;     expression
;;;     invert
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkFilter
;;;         ╰── GtkBoolFilter
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBoolFilter
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkBoolFilter" bool-filter
  (:superclass filter
   :export t
   :interfaces ()
   :type-initializer "gtk_bool_filter_get_type")
  ((expression
    bool-filter-expression
    "expression" "GtkExpression" t t)
   (invert
    bool-filter-invert
    "invert" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'bool-filter 'type)
 "@version{2025-03-13}
  @begin{short}
    The @class{gtk:bool-filter} object is a simple filter that takes a boolean
    @class{gtk:expression} instance to determine whether to include items.
  @end{short}
  @see-constructor{gtk:bool-filter-new}
  @see-slot{gtk:bool-filter-expression}
  @see-slot{gtk:bool-filter-invert}
  @see-class{gtk:filter}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:bool-filter-expression ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expression" 'bool-filter) t)
 "The @code{expression} property of type @class{gtk:expression} (Read / Write)
  @br{}
  The boolean expression to evaluate on the item.")

#+liber-documentation
(setf (liber:alias-for-function 'bool-filter-expression)
      "Accessor"
      (documentation 'bool-filter-expression 'function)
 "@version{2025-08-15}
  @syntax{(gtk:bool-filter-expression object) => expression}
  @syntax{(setf (gtk:bool-filter-expression object) expression)}
  @argument[object]{a @class{gtk:bool-filter} object}
  @argument[expression]{a @class{gtk:expression} instance}
  @begin{short}
    The accessor for the @slot[gtk:bool-filter]{expression} slot of the
    @class{gtk:bool-filter} class gets or sets the expression that the filter
    uses to evaluate if an item should be filtered.
  @end{short}
  The expression must have a @code{\"gboolean\"} value type.
  @see-class{gtk:bool-filter}
  @see-class{gtk:expression}")

;;; --- gtk:bool-filter-invert -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "invert" 'bool-filter) t)
 "The @code{invert} property of type @code{:boolean} (Read / Write) @br{}
  Whether the expression result should be inverted. @br{}
  Default value : @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'bool-filter-invert)
      "Accessor"
      (documentation 'bool-filter-invert 'function)
 "@version{2025-08-15}
  @syntax{(gtk:bool-filter-invert object) => invert}
  @syntax{(setf (gtk:bool-filter-invert object) invert)}
  @argument[object]{a @class{gtk:bool-filter} object}
  @argument[invert]{@em{true} to invert}
  @begin{short}
    The accessor for the @slot[gtk:bool-filter]{invert} slot of the
    @class{gtk:bool-filter} class gets or sets whether the filter inverts the
    expression.
  @end{short}.
  @begin[Notes]{dictionary}
    If no expression is set, this function returns the @code{cffi:null-pointer}
    value. To unset the expression for the bool filter, use the
    @code{cffi:null-pointer} value.
  @end{dictionary}
  @see-class{gtk:bool-filter}")

;;; ----------------------------------------------------------------------------
;;; gtk_bool_filter_new
;;; ----------------------------------------------------------------------------

(declaim (inline bool-filter-new))

(defun bool-filter-new (&optional expression)
 "@version{2024-09-28}
  @argument[expression]{a @class{gtk:expression} instance or @code{nil} for
    none}
  @return{The new @class{gtk:bool-filter} object.}
  @begin{short}
    Creates a new bool filter.
  @end{short}
  @see-class{gtk:bool-filter}"
  (make-instance 'bool-filter
                 :expression (or expression (cffi:null-pointer))))

(export 'bool-filter-new)

;;; --- End of file gtk4.bool-filter.lisp --------------------------------------
