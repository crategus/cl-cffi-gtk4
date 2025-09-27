;;; ----------------------------------------------------------------------------
;;; gtk4.string-filter.lisp
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
;;; GtkStringFilterMatchMode
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkStringFilterMatchMode" string-filter-match-mode
  (:export t
   :type-initializer "gtk_string_filter_match_mode_get_type")
  (:exact 0)
  (:substring 1)
  (:prefix 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'string-filter-match-mode)
      "GEnum"
      (liber:symbol-documentation 'string-filter-match-mode)
 "@version{2025-07-22}
  @begin{declaration}
(gobject:define-genum \"GtkStringFilterMatchMode\" string-filter-match-mode
  (:export t
   :type-initializer \"gtk_string_filter_match_mode_get_type\")
  (:exact 0)
  (:substring 1)
  (:prefix 2))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:exact]{The search string and text must match exactly.}
      @entry[:substring]{The search string must be contained as a substring
        inside the text.}
      @entry[:prefix]{The text must begin with the search string.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Specifies how search strings are matched inside text.
  @end{short}
  @see-class{gtk:string-filter}")

;;; ----------------------------------------------------------------------------
;;; GtkStringFilter
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkStringFilter" string-filter
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

#+liber-documentation
(setf (documentation 'string-filter 'type)
 "@version{2025-07-26}
  @begin{short}
    The @class{gtk:string-filter} object determines whether to include items by
    looking at strings and comparing them to a fixed search term.
  @end{short}
  The strings are obtained from the items by evaluating a @class{gtk:expression}
  instance. The @class{gtk:string-filter} object has several different modes of
  comparison - it can match the whole string, just a prefix, or any substring.
  @see-constructor{gtk:string-filter-new}
  @see-slot{gtk:string-filter-expression}
  @see-slot{gtk:string-filter-ignore-case}
  @see-slot{gtk:string-filter-match-mode}
  @see-slot{gtk:string-filter-search}
  @see-class{gtk:filter}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:string-filter-expression -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expression" 'string-filter) t)
 "The @code{expression} property of type @class{gtk:expression} (Read / Write)
  @br{}
  The expression to evaluate on the item to get a string to compare with.")

#+liber-documentation
(setf (liber:alias-for-function 'string-filter-expression)
      "Accessor"
      (documentation 'string-filter-expression 'function)
 "@version{2025-09-25}
  @syntax{(gtk:string-filter-expression object) => expression}
  @syntax{(setf (gtk:string-filter-expression object) expression)}
  @argument[object]{a @class{gtk:string-filter} object}
  @argument[expression]{a @class{gtk:expression} instance}
  @begin{short}
    The accessor for the @slot[gtk:string-filter]{expression} slot of the
    @class{gtk:string-filter} class gets or sets the expression that the string
    filter uses to obtain strings from items.
  @end{short}
  The expression must have a @code{\"gchararray\"} value type.
  @see-class{gtk:string-filter}
  @see-class{gtk:expression}")

;;; --- gtk:string-filter-ignore-case ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ignore-case" 'string-filter) t)
 "The @code{ignore-case} property of type @code{:boolean} (Read / Write) @br{}
  Whether matching is case sensitive. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'string-filter-ignore-case)
      "Accessor"
      (documentation 'string-filter-ignore-case 'function)
 "@version{2025-09-25}
  @syntax{(gtk:string-filter-ignore-case object) => ignore}
  @syntax{(setf (gtk:stringt-filter-ignore-case object) ignore)}
  @argument[object]{a @class{gtk:string-filter} object}
  @argument[ignore]{@em{true} to ignore case}
  @begin{short}
    The accessor for the @slot[gtk:string-filter]{ignore-case} slot of the
    @class{gtk:string-filter} class gets or sets whether the filter ignores case
    differences.
  @end{short}
  @see-class{gtk:string-filter}")

;;; --- gtk:string-filter-match-mode -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "match-mode" 'string-filter) t)
 "The @code{match-mode} property of type @sym{gtk:string-filter-match-mode}
  (Read / Write) @br{}
  Whether exact matches are necessary or if substrings are allowed. @br{}
  Default value: @val[gtk:string-filter-match-mode]{:substring}")

#+liber-documentation
(setf (liber:alias-for-function 'string-filter-match-mode)
      "Accessor"
      (documentation 'string-filter-match-mode 'function)
 "@version{2025-09-25}
  @syntax{(gtk:string-filter-match-mode object) => mode}
  @syntax{(setf (gtk:stringt-filter-match-mode object) mode)}
  @argument[object]{a @class{gtk:string-filter} object}
  @argument[mode]{a @sym{gtk:string-filter-match-mode} value}
  @begin{short}
    The accessor for the @slot[gtk:string-filter]{match-mode} slot of the
    @class{gtk:string-filter} class gets or sets the match mode that the filter
    is using.
  @end{short}
  @see-class{gtk:string-filter}
  @see-symbol{gtk:string-filter-match-mode}")

;;; --- gtk:string-filter-search -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "search" 'string-filter) t)
 "The @code{search} property of type @code{:string} (Read / Write) @br{}
  The search term. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'string-filter-search)
      "Accessor"
      (documentation 'string-filter-search 'function)
 "@version{2025-09-25}
  @syntax{(gtk:string-filter-search object) => search}
  @syntax{(setf (gtk:stringt-filter-search object) search)}
  @argument[object]{a @class{gtk:string-filter} object}
  @argument[search]{a string to search for or @code{nil} to clear the search}
  @begin{short}
    The accessor for the @slot[gtk:string-filter]{search} slot of the
    @class{gtk:string-filter} class gets or sets the search string to search
    for.
  @end{short}
  @see-class{gtk:string-filter}")

;;; ----------------------------------------------------------------------------
;;; gtk_string_filter_new
;;; ----------------------------------------------------------------------------

(declaim (inline string-filter-new))

(defun string-filter-new (&optional expression)
 "@version{2025-03-13}
  @argument[expression]{a @class{gtk:expression} instance for the expression
    to evaluate or @code{nil} for none}
  @return{The new @class{gtk:string-filter} object.}
  @begin{short}
    Creates a new string filter.
  @end{short}
  You will want to set up the filter by providing a string to search for and
  by providing a property to look up on the item.
  @see-class{gtk:string-filter}
  @see-class{gtk:expression}"
  (make-instance 'string-filter
                 :expression (or expression (cffi:null-pointer))))

(export 'string-filter-new)

;;; --- End of file gtk4.string-filter.lisp ------------------------------------
