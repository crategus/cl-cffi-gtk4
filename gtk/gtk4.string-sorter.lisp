;;; ----------------------------------------------------------------------------
;;; gtk4.string-sorter.lisp
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
;;; GtkStringSorter
;;;
;;;     Sort by comparing strings
;;;
;;; Types and Values
;;;
;;;     GtkStringSorter
;;;     GtkCollation                                       Since 4.10
;;;
;;; Functions
;;;
;;;     gtk_string_sorter_new
;;;     gtk_string_sorter_get_expression
;;;     gtk_string_sorter_set_expression
;;;     gtk_string_sorter_get_ignore_case
;;;     gtk_string_sorter_set_ignore_case
;;;
;;; Properties
;;;
;;;     collation                                          Since 4.10
;;;     expression
;;;     ignore-case
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSorter
;;;         ╰── GtkStringSorter
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCollation

;;;Declaration
;;;enum Gtk.Collation

;;;Description

;;;Describes how a GtkStringSorter turns strings into sort keys to compare them.

;;;Note that the result of sorting will in general depend on the current locale unless the mode is GTK_COLLATION_NONE.

;;;Available since: 4.10


;;;Members
;;;Name	Description
;;;GTK_COLLATION_NONE	
;;;Don’t do any collation.

;;;GTK_COLLATION_UNICODE	
;;;Use g_utf8_collate_key()

;;;GTK_COLLATION_FILENAME	
;;;Use g_utf8_collate_key_for_filename()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkStringSorter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkStringSorter" string-sorter
  (:superclass sorter
   :export t
   :interfaces ()
   :type-initializer "gtk_string_sorter_get_type")
  (#+gtk-4-10
   (collation
    string-sorter-collation
    "collation" "GtkCollation" t t)
   (expression
    string-sorter-expression
    "expression" "GtkExpression" t t)
   (ignore-case
    string-sorter-ignore-case
    "ignore-case" "gboolean" t t)))




;;;Property Details
;;;The “expression” property
;;;  “expression”               GtkExpression *
;;;The expression to evaluate on item to get a string to compare with

;;;[type GtkExpression]

;;;Owner: GtkStringSorter

;;;Flags: Read / Write

;;;The “ignore-case” property
;;;  “ignore-case”              gboolean
;;;If matching is case sensitive

;;;Owner: GtkStringSorter

;;;Flags: Read / Write

;;;Default value: TRUE

;;;See Also
;;;GtkExpression


;;;Description
;;;GtkStringSorter is a GtkSorter that compares strings. It does the comparison in a linguistically correct way using the current locale by normalizing Unicode strings and possibly case-folding them before performing the comparison.

;;;To obtain the strings to compare, this sorter evaluates a GtkExpression.

;;;Functions
;;;gtk_string_sorter_new ()
;;;GtkStringSorter *
;;;gtk_string_sorter_new (GtkExpression *expression);
;;;Creates a new string sorter that compares items using the given expression .

;;;Unless an expression is set on it, this sorter will always compare items as invalid.

;;;Parameters
;;;expression

;;;The expression to evaluate.

;;;[transfer full][nullable]
;;;Returns
;;;a new GtkStringSorter

;;;gtk_string_sorter_get_expression ()
;;;GtkExpression *
;;;gtk_string_sorter_get_expression (GtkStringSorter *self);
;;;Gets the expression that is evaluated to obtain strings from items.

;;;Parameters
;;;self

;;;a GtkStringSorter

;;;Returns
;;;a GtkExpression, or NULL.

;;;[transfer none][nullable]

;;;gtk_string_sorter_set_expression ()
;;;void
;;;gtk_string_sorter_set_expression (GtkStringSorter *self,
;;;                                  GtkExpression *expression);
;;;Sets the expression that is evaluated to obtain strings from items.

;;;The expression must have the type G_TYPE_STRING.

;;;Parameters
;;;self

;;;a GtkStringSorter

;;;expression

;;;a GtkExpression, or NULL.

;;;[nullable][transfer none]
;;;gtk_string_sorter_get_ignore_case ()
;;;gboolean
;;;gtk_string_sorter_get_ignore_case (GtkStringSorter *self);
;;;Gets whether the sorter ignores case differences.

;;;Parameters
;;;self

;;;a GtkStringSorter

;;;Returns
;;;TRUE if self is ignoring case differences

;;;gtk_string_sorter_set_ignore_case ()
;;;void
;;;gtk_string_sorter_set_ignore_case (GtkStringSorter *self,
;;;                                   gboolean ignore_case);
;;;Sets whether the sorter will ignore case differences.

;;;Parameters
;;;self

;;;a GtkStringSorter

;;;ignore_case

;;;TRUE to ignore case differences


;;; --- End of file gtk4.string-sorter.lisp ------------------------------------
