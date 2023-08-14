;;; ----------------------------------------------------------------------------
;;; gtk4.bool-filter.lisp
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
;;; GtkBoolFilter
;;;
;;;     Filtering by boolean expressions
;;;
;;; Types and Values
;;;
;;;     GtkBoolFilter
;;;
;;; Functions
;;;
;;;     gtk_bool_filter_new
;;;     gtk_bool_filter_get_expression
;;;     gtk_bool_filter_set_expression
;;;     gtk_bool_filter_get_invert
;;;     gtk_bool_filter_set_invert
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

(gobject:define-g-object-class "GtkBoolFilter" bool-filter
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


;;;Property Details
;;;The “expression” property
;;;  “expression”               GtkExpression *
;;;The boolean expression to evaluate on item

;;;[type GtkExpression]

;;;Owner: GtkBoolFilter

;;;Flags: Read / Write

;;;The “invert” property
;;;  “invert”                   gboolean
;;;If the expression result should be inverted

;;;Owner: GtkBoolFilter

;;;Flags: Read / Write

;;;Default value: FALSE



;;;Includes
;;;#include <gtk/gtk.h>
;;;Description
;;;GtkBoolFilter is a simple filter that takes a boolean GtkExpression to determine whether to include items.

;;;Functions
;;;gtk_bool_filter_new ()
;;;GtkBoolFilter *
;;;gtk_bool_filter_new (GtkExpression *expression);
;;;Creates a new bool filter.

;;;Parameters
;;;expression

;;;The expression to evaluate or NULL for none.

;;;[transfer full][nullable]
;;;Returns
;;;a new GtkBoolFilter

;;;gtk_bool_filter_get_expression ()
;;;GtkExpression *
;;;gtk_bool_filter_get_expression (GtkBoolFilter *self);
;;;Gets the expression that the filter uses to evaluate if an item should be filtered.

;;;Parameters
;;;self

;;;a GtkBoolFilter

;;;Returns
;;;a GtkExpression.

;;;[transfer none]

;;;gtk_bool_filter_set_expression ()
;;;void
;;;gtk_bool_filter_set_expression (GtkBoolFilter *self,
;;;                                GtkExpression *expression);
;;;Sets the expression that the filter uses to check if items should be filtered. The expression must have a value type of G_TYPE_BOOLEAN.

;;;Parameters
;;;self

;;;a GtkBoolFilter

;;;expression

;;;a GtkExpression

;;;gtk_bool_filter_get_invert ()
;;;gboolean
;;;gtk_bool_filter_get_invert (GtkBoolFilter *self);
;;;Returns whether the filter inverts the expression.

;;;Parameters
;;;self

;;;a GtkBoolFilter

;;;Returns
;;;TRUE if the filter inverts

;;;gtk_bool_filter_set_invert ()
;;;void
;;;gtk_bool_filter_set_invert (GtkBoolFilter *self,
;;;                            gboolean invert);
;;;Sets whether the filter should invert the expression.

;;;Parameters
;;;self

;;;a GtkBoolFilter

;;;invert

;;;TRUE to invert


;;; --- End of file gtk4.bool-filter.lisp --------------------------------------
