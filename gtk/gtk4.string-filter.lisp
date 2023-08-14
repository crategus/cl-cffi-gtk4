;;; ----------------------------------------------------------------------------
;;; gtk4.string-filter.lisp
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
;;; GtkStringFilter
;;;
;;;     Filtering by strings
;;;
;;; Types and Values
;;;
;;;     GtkStringFilter
;;;     GtkStringFilterMatchMode
;;;
;;; Functions
;;;
;;;     gtk_string_filter_new 
;;;     gtk_string_filter_get_search 
;;;     gtk_string_filter_set_search 
;;;     gtk_string_filter_get_expression 
;;;     gtk_string_filter_set_expression 
;;;     gtk_string_filter_get_ignore_case 
;;;     gtk_string_filter_set_ignore_case 
;;;     gtk_string_filter_get_match_mode 
;;;     gtk_string_filter_set_match_mode 
;;;
;;; Properties
;;;
;;;     expression
;;;     ignore-case
;;;     match-mode
;;;     search
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkFilter
;;;         ╰── GtkStringFilter
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkStringFilterMatchMode
;;;
;;; Specifies how search strings are matched inside text.
;;;
;;; GTK_STRING_FILTER_MATCH_MODE_EXACT
;;;     The search string and text must match exactly.
;;;
;;; GTK_STRING_FILTER_MATCH_MODE_SUBSTRING
;;;     The search string must be contained as a substring inside the text.
;;;
;;; GTK_STRING_FILTER_MATCH_MODE_PREFIX
;;;     The text must begin with the search string.
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; GtkStringFilter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkStringFilter" string-filter
  (:superclass filter
   :export t
   :interfaces ()
   :type-initializer "gtk_string_filter_get_type")
  ((expression
    string-filter-expression
    "expression" "GtkExpression" t t)
   (ignore-case
    string-filter-ignore-case
    "ignore-case" "gboolean" t t)
   (match-mode
    string-filter-match-mode
    "match-mode" "GtkStringFilterMatchMode" t t)
   (search
    string-filter-search
    "search" "gchararray" t t)))


;;;Property Details
;;;The “expression” property
;;;  “expression”               GtkExpression *
;;;The expression to evaluate on item to get a string to compare with

;;;[type GtkExpression]

;;;Owner: GtkStringFilter

;;;Flags: Read / Write

;;;The “ignore-case” property
;;;  “ignore-case”              gboolean
;;;If matching is case sensitive

;;;Owner: GtkStringFilter

;;;Flags: Read / Write

;;;Default value: TRUE

;;;The “match-mode” property
;;;  “match-mode”               GtkStringFilterMatchMode
;;;If exact matches are necessary or if substrings are allowed

;;;Owner: GtkStringFilter

;;;Flags: Read / Write

;;;Default value: GTK_STRING_FILTER_MATCH_MODE_SUBSTRING

;;;The “search” property
;;;  “search”                   char *
;;;The search term

;;;Owner: GtkStringFilter

;;;Flags: Read / Write

;;;Default value: NULL



;;;Includes
;;;#include <gtk/gtk.h>
;;;Description
;;;GtkStringFilter determines whether to include items by looking at strings and comparing them to a fixed search term. The strings are obtained from the items by evaluating a GtkExpression.

;;;GtkStringFilter has several different modes of comparison - it can match the whole string, just a prefix, or any substring.

;;;Functions
;;;gtk_string_filter_new ()
;;;GtkStringFilter *
;;;gtk_string_filter_new (GtkExpression *expression);
;;;Creates a new string filter.

;;;You will want to set up the filter by providing a string to search for and by providing a property to look up on the item.

;;;Parameters
;;;expression

;;;The expression to evaluate or NULL for none.

;;;[transfer full][nullable]
;;;Returns
;;;a new GtkStringFilter

;;;gtk_string_filter_get_search ()
;;;const char *
;;;gtk_string_filter_get_search (GtkStringFilter *self);
;;;Gets the search string set via gtk_string_filter_set_search().

;;;Parameters
;;;self

;;;a GtkStringFilter

;;;Returns
;;;The search string.

;;;[nullable][transfer none]

;;;gtk_string_filter_set_search ()
;;;void
;;;gtk_string_filter_set_search (GtkStringFilter *self,
;;;                              const char *search);
;;;Sets the string to search for.

;;;Parameters
;;;self

;;;a GtkStringFilter

;;;search

;;;The string to search for or NULL to clear the search.

;;;[transfer none][nullable]
;;;gtk_string_filter_get_expression ()
;;;GtkExpression *
;;;gtk_string_filter_get_expression (GtkStringFilter *self);
;;;Gets the expression that the string filter uses to obtain strings from items.

;;;Parameters
;;;self

;;;a GtkStringFilter

;;;Returns
;;;a GtkExpression.

;;;[transfer none]

;;;gtk_string_filter_set_expression ()
;;;void
;;;gtk_string_filter_set_expression (GtkStringFilter *self,
;;;                                  GtkExpression *expression);
;;;Sets the expression that the string filter uses to obtain strings from items. The expression must have a value type of G_TYPE_STRING.

;;;Parameters
;;;self

;;;a GtkStringFilter

;;;expression

;;;a GtkExpression

;;;gtk_string_filter_get_ignore_case ()
;;;gboolean
;;;gtk_string_filter_get_ignore_case (GtkStringFilter *self);
;;;Returns whether the filter ignores case differences.

;;;Parameters
;;;self

;;;a GtkStringFilter

;;;Returns
;;;TRUE if the filter ignores case

;;;gtk_string_filter_set_ignore_case ()
;;;void
;;;gtk_string_filter_set_ignore_case (GtkStringFilter *self,
;;;                                   gboolean ignore_case);
;;;Sets whether the filter ignores case differences.

;;;Parameters
;;;self

;;;a GtkStringFilter

;;;ignore_case

;;;TRUE to ignore case

;;;gtk_string_filter_get_match_mode ()
;;;GtkStringFilterMatchMode
;;;gtk_string_filter_get_match_mode (GtkStringFilter *self);
;;;Returns the match mode that the filter is using.

;;;Parameters
;;;self

;;;a GtkStringFilter

;;;Returns
;;;the match mode of the filter

;;;gtk_string_filter_set_match_mode ()
;;;void
;;;gtk_string_filter_set_match_mode (GtkStringFilter *self,
;;;                                  GtkStringFilterMatchMode mode);
;;;Sets the match mode for the filter.

;;;Parameters
;;;self

;;;a GtkStringFilter

;;;mode

;;;the new match mode


;;; --- End of file gtk4.string-filter.lisp ------------------------------------
