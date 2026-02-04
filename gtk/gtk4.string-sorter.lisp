;;; ----------------------------------------------------------------------------
;;; gtk4.string-sorter.lisp
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
;;; GtkStringSorter
;;;
;;;     Sort by comparing strings
;;;
;;; Types and Values
;;;
;;;     GtkStringSorter
;;;     GtkCollation                                        Since 4.10
;;;
;;; Accessors
;;;
;;;     gtk_string_sorter_get_collation                     Since 4.10
;;;     gtk_string_sorter_set_collation                     Since 4.10
;;;     gtk_string_sorter_get_expression
;;;     gtk_string_sorter_set_expression
;;;     gtk_string_sorter_get_ignore_case
;;;     gtk_string_sorter_set_ignore_case
;;;
;;; Functions
;;;
;;;     gtk_string_sorter_new
;;;
;;; Properties
;;;
;;;     collation                                           Since 4.10
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
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkCollation" collation
  (:export t
   :type-initializer "gtk_collation_get_type")
  (:none 0)
  (:unicode 1)
  (:filename 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'collation)
      "GEnum"
      (liber:symbol-documentation 'collation)
 "@version{2025-07-23}
  @begin{declaration}
(gobject:define-genum \"GtkCollation\" collation
  (:export t
   :type-initializer \"gtk_collation_get_type\")
  (:none 0)
  (:unicode 1)
  (:filename 2))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{Do not do any collation.}
      @entry[:unicode]{Use the @code{g_utf8_collate_key()} function.}
      @entry[:filename]{Use the @code{g_utf8_collate_key_for_filename()}
        function.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Describes how a @class{gtk:string-sorter} object turns strings into sort
    keys to compare them.
  @end{short}
  Note that the result of sorting will in general depend on the current locale
  unless the mode is @val[gtk:collation]{:none}.

  Since 4.10
  @see-class{gtk:string-sorter}")

;;; ----------------------------------------------------------------------------
;;; GtkStringSorter
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkStringSorter" string-sorter
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

#+liber-documentation
(setf (documentation 'string-sorter 'type)
 "@version{2025-07-27}
  @begin{short}
    The @class{gtk:string-sorter} object is a @class{gtk:sorter} implementation
    that compares strings.
  @end{short}
  It does the comparison in a linguistically correct way using the current
  locale by normalizing Unicode strings and possibly case-folding them before
  performing the comparison.

  To obtain the strings to compare, this sorter evaluates a
  @class{gtk:expression} instance.
  @see-constructor{gtk:string-sorter-new}
  @see-function{gtk:string-sorter-collation}
  @see-function{gtk:string-sorter-expression}
  @see-function{gtk:string-sorter-ignore-case}
  @see-class{gtk:sorter}
  @see-class{gtk:expression}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:string-sorter-collation --------------------------------------------

#+(and gtk-4-10 liber-documentation)
(setf (documentation (liber:slot-documentation "collation" 'string-sorter) t)
 "The @code{collation} property of type @sym{gtk:collation} (Read / Write) @br{}
  The collation method to use for sorting. The @val[gtk:collation]{:none} value
  is useful when the expression already returns collation keys, or strings that
  need to be compared byte-by-byte. The default value,
  @val[gtk:collation]{:unicode}, compares strings according to the Unicode
  collation algorithm. Since 4.10 @br{}
  Default value: @val[gtk:collation]{:unicode}")

#+(and gtk-4-10 liber-documentation)
(setf (liber:alias-for-function 'string-sorter-collation)
      "Accessor"
      (documentation 'string-sorter-collation 'function)
 "@version{2025-09-28}
  @syntax{(gtk:string-sorter-collation object) => collation}
  @syntax{(setf (gtk:string-sorter-collation object) collation)}
  @argument[object]{a @class{gtk:string-sorter} object}
  @argument[collation]{a @sym{gtk:collation} value}
  @begin{short}
    The accessor for the @slot[gtk:string-sorter]{collation} slot of the
    @class{gtk:string-sorter} class gets or sets the collation method to use for
    sorting.
  @end{short}

  Since 4.10
  @see-class{gtk:string-sorter}
  @see-symbol{gtk:collation}")

;;; --- gtk:string-sorter-expression -------------------------------------------

;; TODO: GtkExpression is not implemented as a Lisp object, therefore we have
;; to work with CFFI:NULL-POINTER values. Change the implementation of
;; GtkExpression?!

#+liber-documentation
(setf (documentation (liber:slot-documentation "expression" 'string-sorter) t)
 "The @code{expression} property of type @class{gtk:expression} (Read / Write)
  @br{}
  The expression to evaluate on items to get a string to compare with.")

#+liber-documentation
(setf (liber:alias-for-function 'string-sorter-expression)
      "Accessor"
      (documentation 'string-sorter-expression 'function)
 "@version{2025-09-28}
  @syntax{(gtk:string-sorter-expression object) => expression}
  @syntax{(setf (gtk:string-sorter-expression object) expression)}
  @argument[object]{a @class{gtk:string-sorter} object}
  @argument[expression]{a @class{gtk:expression} instance, or
    @code{(cffi:null-pointer)}}
  @begin{short}
    The accessor for the @slot[gtk:string-sorter]{expression} slot of the
    @class{gtk:string-sorter} class gets or sets the expression that is
    evaluated to obtain strings from items.
  @end{short}
  The expression must have the @code{\"gchararray\"} type.
  @see-class{gtk:string-sorter}
  @see-class{gtk:expression}")

;;; --- gtk:string-sorter-ignore-case ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ignore-case" 'string-sorter) t)
 "The @code{ignore-case} property of type @code{:boolean} (Read / Write) @br{}
  Whether matching is case sensitive. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'string-sorter-ignore-case)
      "Accessor"
      (documentation 'string-sorter-ignore-case 'function)
 "@version{2025-09-28}
  @syntax{(gtk:string-sorter-ignore-case object) => setting}
  @syntax{(setf (gtk:string-sorter-ignore-case object) setting)}
  @argument[object]{a @class{gtk:string-sorter} object}
  @argument[setting]{@em{true} to ignore case differences}
  @begin{short}
    The accessor for the @slot[gtk:string-sorter]{ignore-case} slot of the
    @class{gtk:string-sorter} class gets or sets whether the sorter ignores case
    differences.
  @end{short}
  @see-class{gtk:string-sorter}")

;;; ----------------------------------------------------------------------------
;;; gtk_string_sorter_new
;;; ----------------------------------------------------------------------------

(defun string-sorter-new (expression)
 #+liber-documentation
 "@version{2024-10-24}
  @argument[expression]{a @class{gtk:expression} instance to evaluate}
  @return{The new @class{gtk:string-sorter} object.}
  @begin{short}
    Creates a new string sorter that compares items using the given
    @arg{expression}.
  @end{short}
  Unless an expression is set on it, this sorter will always compare items as
  invalid.
  @see-class{gtk:string-sorter}
  @see-class{gtk:expression}"
  (make-instance 'string-sorter
                 :expression (or expression (cffi:null-pointer))))

(export 'string-sorter-new)

;;; --- End of file gtk4.string-sorter.lisp ------------------------------------
