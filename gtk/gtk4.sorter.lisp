;;; ----------------------------------------------------------------------------
;;; gtk4.sorter.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2024 Dieter Kaiser
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
;;; GtkSorter
;;;
;;;     Sorting items
;;;
;;; Types and Values
;;;
;;;     GtkSorter
;;;     GtkSorterOrder
;;;     GtkSorterChange
;;;
;;; Functions
;;;
;;;     gtk_sorter_compare
;;;     gtk_sorter_get_order
;;;     gtk_sorter_changed
;;;
;;;     gtk_ordering_from_cmpfunc                          Since 4.2
;;;
;;; Signals
;;;
;;;     changed
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSorter
;;;         ├── GtkCustomSorter
;;;         ├── GtkMultiSorter
;;;         ├── GtkNumericSorter
;;;         ├── GtkStringSorter
;;;         ╰── GtkTreeListRowSorter
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSorterOrder
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkSorterOrder" sorter-order
  (:export t
   :type-initializer "gtk_sorter_order_get_type")
  (:partial 0)
  (:none 1)
  (:total 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'sorter-order)
      "GEnum"
      (liber:symbol-documentation 'sorter-order)
 "@version{2023-9-4}
  @begin{declaration}
    @begin{pre}
(gobject:define-g-enum \"GtkSorterOrder\" sorter-order
  (:export t
   :type-initializer \"gtk_sorter_order_get_type\")
  (:partial 0)
  (:none 1)
  (:total 2))
    @end{pre}
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:partial]{A partial order. Any value of the @symbol{gtk:ordering}
        enumeration is possible.}
      @entry[:none]{No order, all elements are considered equal. The
        @fun{gtk:sorter-compare} function will only return the @code{:equal}
        value of the @symbol{gtk:ordering} enumeration.}
      @entry[:total]{A total order. The @fun{gtk:sorter-compare} function will
        only return the @code{:equal} value of the @symbol{gtk:ordering}
        enumeration if an item is compared with itself. Two different items will
        never cause this value to be returned.}
    @end{table}
  @end{values}
  @begin{short}
    Describes the type of order that a @class{gtk:sorter} object may describe.
  @end{short}
  @see-class{gtk:sorter}
  @see-symbol{gtk:ordering}
  @see-function{gtk:sorter-compare}")

;;; ----------------------------------------------------------------------------
;;; GtkSorterChange
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkSorterChange" sorter-change
  (:export t
   :type-initializer "gtk_sorter_change_get_type")
  (:different 0)
  (:inverted 1)
  (:less-strict 2)
  (:more-strict 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'sorter-change)
      "GEnum"
      (liber:symbol-documentation 'sorter-change)
 "@version{2023-9-4}
  @begin{declaration}
    @begin{pre}
(gobject:define-g-enum \"GtkSorterChange\" sorter-change
  (:export t
   :type-initializer \"gtk_sorter_change_get_type\")
  (:different 0)
  (:inverted 1)
  (:less-strict 2)
  (:more-strict 3))
    @end{pre}
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:different]{The sorter change cannot be described by any of the
        other enumeration values.}
      @entry[:inverted]{The sort order was inverted. Comparisons that returned
        the @code{:smaller} value now return the @code{:larger} value of the
        @symbol{gtk:ordering} enumeration and vice versa. Other comparisons
        return the same values as before.}
      @entry[:less-strict]{The sorter is less strict. Comparisons may now return
        the @code{:equal} value of the @symbol{gtk:ordering} enumeration that
        did not do so before.}
      @entry[:more-strict]{The sorter is more strict. Comparisons that did
        return the @code{:equal} value of the @symbol{gtk:ordering} enumeration
        may not do so anymore.}
    @end{table}
  @end{values}
  @begin{short}
    Describes changes in a sorter in more detail and allows users to optimize
    resorting.
  @end{short}
  @see-class{gtk:sorter}
  @see-symbol{gtk:ordering}")

;;; ----------------------------------------------------------------------------
;;; GtkSorter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkSorter" sorter
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_sorter_get_type")
  nil)

#+liber-documentation
(setf (documentation 'sorter 'type)
 "@version{2023-9-4}
  @begin{short}
    The @class{gtk:sorter} object is the way to describe sorting criteria.
  @end{short}
  Its primary user is the @class{gtk:sort-list-model} object.

  The model will use a sorter to determine the order in which its items should
  appear by calling the @fun{gtk:sorter-compare} function for pairs of items.

  Sorters may change their sorting behavior through their lifetime. In that
  case, they will emit the \"changed\" signal to notify that the sort order is
  no longer valid and should be updated by calling the @fun{gtk:sorter-compare}
  function again.

  GTK provides various pre-made sorter implementations for common sorting
  operations. The @class{gtk:column-view} widget has built-in support for
  sorting lists via the @slot[gtk:column-view]{sorter} property, where the user
  can change the sorting by clicking on list headers.

  Of course, in particular for large lists, it is also possible to subclass the
  @class{gtk:sorter} object and provide one's own sorter.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
lambda (sorter change)    :run-last
      @end{pre}
      This signal is emitted whenever the sorter changed. Users of the sorter
      should then update the sort order again via the @fun{gtk:sorter-compare}
      function. The @class{gtk:sort-list-model} object handles this signal
      automatically. Depending on the change parameter, it may be possible to
      update the sort order without a full resorting. Refer to the
      @symbol{gtk:sorter-change} documentation for details.
      @begin[code]{table}
        @entry[sorter]{The @class{gtk:sorter} object.}
        @entry[change]{How the sorter changed as a @symbol{gtk:sorter-change}
          value.}
      @end{table}
  @end{dictionary}
  @see-class{gtk:sort-list-model}")

;;; ----------------------------------------------------------------------------
;;; gtk_sorter_compare
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_sorter_compare" sorter-compare) ordering
 #+liber-documentation
 "@version{#2023-9-12}
  @argument[sorter]{a @class{gtk:sorter} object}
  @argument[item1]{a pointer to the first item to compare}
  @argument[item2]{a pointer to the second item to compare}
  @return{The @symbol{gtk:ordering} value with @code{:equal} if @arg{item1} =
    @arg{item2}, @code{:smaller} if @arg{item1} < @arg{item2}, @code{:larger}
    if @arg{item1} > @arg{item2}.}
  @begin{short}
    Compares two given items according to the sort order implemented by the
    sorter.
  @end{short}

  Sorters implement a partial order:
  @begin{itemize}
    @item{It is reflexive, i.e. a = a.}
    @item{It is antisymmetric, i.e. if a < b and b < a, then a = b.}
    @item{It is transitive, i.e. given any 3 items with a ≤ b and b ≤ c, then
      a ≤ c.}
  @end{itemize}
  The sorter may signal it conforms to additional constraints via the return
  value of the @fun{gtk:sorter-order} function.
  @see-class{gtk:sorter}
  @see-symbol{gtk:ordering}
  @see-function{gtk:sorter-order}"
  (sorter (g:object sorter))
  (item1 :pointer)
  (item2 :pointer))

(export 'sorter-compare)

;;; ----------------------------------------------------------------------------
;;; gtk_sorter_get_order
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_sorter_get_order" sorter-order) sorter-order
 #+liber-documentation
 "@version{#2023-9-12}
  @argument[sorter]{a @class{gtk:sorter} object}
  @return{The @symbol{gtk:sorter-order} value.}
  @begin{short}
    Gets the order that @arg{sorter} conforms to.
  @end{short}
  See the @symbol{gtk:sorter-order} enumeration for details of the possible
  return values. This function is intended to allow optimizations.
  @see-class{gtk:sorter}
  @see-symbol{gtk:sorter-order}"
  (sorter (g:object sorter)))

(export 'sorter-order)

;;; ----------------------------------------------------------------------------
;;; gtk_sorter_changed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_sorter_changed" sorter-changed) :void
 #+liber-documentation
 "@version{#2023-9-12}
  @argument[sorter]{a @class{gtk:sorter} object}
  @argument[change]{a @symbol{gtk:sorter-change} value}
  @begin{short}
    Emits the \"changed\" signal to notify all users of the sorter that it has
    changed.
  @end{short}
  Users of the sorter should then update the sort order via the
  @fun{gtk:sorter-compare} function.

  Depending on the @arg{change} parameter, it may be possible to update the sort
  order without a full resorting. Refer to the @symbol{gtk:sorter-change}
  documentation for details.

  This function is intended for implementors of @class{gtk:sorter} subclasses
  and should not be called from other functions.
  @see-class{gtk:sorter}
  @see-symbol{gtk:sorter-change}
  @see-function{gtk:sorter-compare}"
  (sorter (g:object sorter))
  (change sorter-change))

(export 'sorter-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_ordering_from_cmpfunc
;;; ----------------------------------------------------------------------------

#+gtk-4-2
(defun ordering-from-cmpfunc (result)
 #+liber-documentation
 "@version{2023-9-13}
  @argument[result]{An integer with the result of a comparison function}
  @return{The @symbol{gtk:ordering} value.}
  @begin{short}
    Converts the result of a @symbol{g:compare-data-func} function to a
    @symbol{gtk:ordering} value.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(mapcar #'gtk:ordering-from-cmpfunc '(-1 0 +1))
=> (:SMALLER :EQUAL :LARGER)
    @end{pre}
  @end{dictionary}
  Since 4.2
  @see-symbol{gtk:ordering}
  @see-symbol{g:compare-data-func}"
  (cffi:foreign-enum-keyword 'ordering result))

#+gtk-4-2
(export 'ordering-from-cmpfunc)

;;; --- End of file gtk4.sorter.lisp -------------------------------------------
