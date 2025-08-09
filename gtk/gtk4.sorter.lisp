;;; ----------------------------------------------------------------------------
;;; gtk4.sorter.lisp
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
;;;     gtk_ordering_from_cmpfunc                           Since 4.2
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

(gobject:define-genum "GtkSorterOrder" sorter-order
  (:export t
   :type-initializer "gtk_sorter_order_get_type")
  (:partial 0)
  (:none 1)
  (:total 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'sorter-order)
      "GEnum"
      (liber:symbol-documentation 'sorter-order)
 "@version{2025-07-23}
  @begin{declaration}
(gobject:define-genum \"GtkSorterOrder\" sorter-order
  (:export t
   :type-initializer \"gtk_sorter_order_get_type\")
  (:partial 0)
  (:none 1)
  (:total 2))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:partial]{A partial order, any value of the @sym{gtk:ordering}
        enumeration is possible.}
      @entry[:none]{No order, all elements are considered equal. The
        @fun{gtk:sorter-compare} function will only return the
        @val[gtk:ordering]{:equal} value of the @sym{gtk:ordering} enumeration.}
      @entry[:total]{A total order, the @fun{gtk:sorter-compare} function will
        only return the @val[gtk:ordering]{:equal} value of the
        @sym{gtk:ordering} enumeration if an item is compared with itself. Two
        different items will never cause this value to be returned.}
    @end{simple-table}
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

(gobject:define-genum "GtkSorterChange" sorter-change
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
 "@version{2025-07-27}
  @begin{declaration}
(gobject:define-genum \"GtkSorterChange\" sorter-change
  (:export t
   :type-initializer \"gtk_sorter_change_get_type\")
  (:different 0)
  (:inverted 1)
  (:less-strict 2)
  (:more-strict 3))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:different]{The sorter change cannot be described by any of the
        other enumeration values.}
      @entry[:inverted]{The sort order was inverted. Comparisons that returned
        the @val[gtk:ordering]{:smaller} value now return the
        @val[gtk:ordering]{:larger} value of the @sym{gtk:ordering} enumeration
        and vice versa. Other comparisons return the same values as before.}
      @entry[:less-strict]{The sorter is less strict. Comparisons may now return
        the @val[gtk:ordering]{:equal} value of the @sym{gtk:ordering}
        enumeration that did not do so before.}
      @entry[:more-strict]{The sorter is more strict. Comparisons that did
        return the @val[gtk:ordering]{:equal} value of the @sym{gtk:ordering}
        enumeration may not do so anymore.}
    @end{simple-table}
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

(gobject:define-gobject "GtkSorter" sorter
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_sorter_get_type")
  nil)

#+liber-documentation
(setf (documentation 'sorter 'type)
 "@version{2025-07-23}
  @begin{short}
    The @class{gtk:sorter} object is the way to describe sorting criteria.
  @end{short}
  Its primary user is the @class{gtk:sort-list-model} object. The model will
  use a sorter to determine the order in which its items should appear by
  calling the @fun{gtk:sorter-compare} function for pairs of items.

  Sorters may change their sorting behavior through their lifetime. In that
  case, they will emit the @sig[gtk:sorter]{changed} signal to notify that the
  sort order is no longer valid and should be updated by calling the
  @fun{gtk:sorter-compare} function again.

  GTK provides various pre-made sorter implementations for common sorting
  operations. The @class{gtk:column-view} widget has built-in support for
  sorting lists via the @slot[gtk:column-view]{sorter} property, where the user
  can change the sorting by clicking on list headers.

  Of course, in particular for large lists, it is also possible to subclass the
  @class{gtk:sorter} object and provide one's own sorter.
  @begin[Signal Details]{dictionary}
    @begin[sorter::changed]{signal}
      @begin{pre}
lambda (sorter change)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[sorter]{The @class{gtk:sorter} object.}
        @entry[change]{How the sorter changed as a @sym{gtk:sorter-change}
          value.}
      @end{simple-table}
      This signal is emitted whenever the sorter changed. Users of the sorter
      should then update the sort order again via the @fun{gtk:sorter-compare}
      function. The @class{gtk:sort-list-model} object handles this signal
      automatically. Depending on the change parameter, it may be possible to
      update the sort order without a full resorting. Refer to the
      @sym{gtk:sorter-change} documentation for details.
    @end{signal}
  @end{dictionary}
  @see-class{gtk:sort-list-model}")

;;; ----------------------------------------------------------------------------
;;; gtk_sorter_compare
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_sorter_compare" sorter-compare) ordering
 #+liber-documentation
 "@version{2025-07-27}
  @argument[sorter]{a @class{gtk:sorter} object}
  @argument[obj1]{a @class{g:object} instance for the first item to compare}
  @argument[obj2]{a @class{g:object} instance for the second item to compare}
  @begin{return}
    The @sym{gtk:ordering} value with the @val[gtk:ordering]{:equal} value if
    @arg{obj1} = @arg{obj2}, the @val[gtk:ordering]{:smaller} value if
    @arg{obj1} < @arg{obj2}, the @val[gtk:ordering]{:larger} value if
    @arg{obj1} > @arg{obj2}.
  @end{return}
  @begin{short}
    Compares two given objects according to the sort order implemented by the
    sorter.
  @end{short}

  Sorters implement a partial order:
  @begin{itemize}
    @item{It is reflexive, that is @code{a = a}.}
    @item{It is antisymmetric, that is if @code{a < b} and @code{b < a}, then
      @code{a = b}.}
    @item{It is transitive, that is given any 3 objects with @code{a ≤ b} and
      @code{b ≤ c}, then @code{a ≤ c}.}
  @end{itemize}
  The sorter may signal it conforms to additional constraints via the return
  value of the @fun{gtk:sorter-order} function.
  @see-class{gtk:sorter}
  @see-symbol{gtk:ordering}
  @see-function{gtk:sorter-order}"
  (sorter (g:object sorter))
  (obj1 g:object)
  (obj2 g:object))

(export 'sorter-compare)

;;; ----------------------------------------------------------------------------
;;; gtk_sorter_get_order
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_sorter_get_order" sorter-order) sorter-order
 #+liber-documentation
 "@version{2025-07-27}
  @argument[sorter]{a @class{gtk:sorter} object}
  @return{The @sym{gtk:sorter-order} value.}
  @begin{short}
    Gets the order that @arg{sorter} conforms to.
  @end{short}
  See the @sym{gtk:sorter-order} enumeration for details of the possible return
  values. This function is intended to allow optimizations.
  @see-class{gtk:sorter}
  @see-symbol{gtk:sorter-order}"
  (sorter (g:object sorter)))

(export 'sorter-order)

;;; ----------------------------------------------------------------------------
;;; gtk_sorter_changed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_sorter_changed" sorter-changed) :void
 #+liber-documentation
 "@version{2025-07-23}
  @argument[sorter]{a @class{gtk:sorter} object}
  @argument[change]{a @sym{gtk:sorter-change} value}
  @begin{short}
    Emits the @sig[gtk:sorter]{changed} signal to notify all users of the sorter
    that it has changed.
  @end{short}
  Users of the sorter should then update the sort order via the
  @fun{gtk:sorter-compare} function.

  Depending on the @arg{change} parameter, it may be possible to update the sort
  order without a full resorting. Refer to the @sym{gtk:sorter-change}
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
 "@version{2025-07-27}
  @argument[result]{an integer for the result of a comparison function}
  @return{The @sym{gtk:ordering} value.}
  @begin{short}
    Converts the result of a @sym{g:compare-data-func} function to a
    @sym{gtk:ordering} value.
  @end{short}

  Since 4.2
  @begin[Examples]{dictionary}
    @begin{pre}
(mapcar #'gtk:ordering-from-cmpfunc '(-1 0 +1))
=> (:SMALLER :EQUAL :LARGER)
    @end{pre}
  @end{dictionary}
  @see-symbol{gtk:ordering}
  @see-symbol{g:compare-data-func}"
  (cffi:foreign-enum-keyword 'ordering result))

#+gtk-4-2
(export 'ordering-from-cmpfunc)

;;; --- End of file gtk4.sorter.lisp -------------------------------------------
