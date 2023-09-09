;;; ----------------------------------------------------------------------------
;;; gtk4.sorter.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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
;;; enum GtkSorterOrder
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
  @begin{short}
    Describes the type of order that a @class{gtk:sorter} object may describe.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GtkSorterOrder\" sorter-order
  (:export t
   :type-initializer \"gtk_sorter_order_get_type\")
  (:partial 0)
  (:none 1)
  (:total 2))
  @end{pre}
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
  @see-class{gtk:sorter}
  @see-symbol{gtk:ordering}
  @see-function{gtk:sorter-compare}")

;;; ----------------------------------------------------------------------------
;;; enum GtkSorterChange
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
  @begin{short}
    Describes changes in a sorter in more detail and allows users to optimize
    resorting.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GtkSorterChange\" sorter-change
  (:export t
   :type-initializer \"gtk_sorter_change_get_type\")
  (:different 0)
  (:inverted 1)
  (:less-strict 2)
  (:more-strict 3))
  @end{pre}
  @begin[code]{table}
    @entry[:different]{The sorter change cannot be described by any of the
      other enumeration values.}
    @entry[:inverted]{The sort order was inverted. Comparisons that returned
      the @code{:smaller} value now return the @code{:larger} value of the
      @symbol{gtk:ordering} enumeration and vice versa. Other comparisons
      return the same values as before.}
    @entry[:less-strict]{The sorter is less strict. Comparisons may now return
      the @code{:equal} value of the @symbol{gtk:ordering} enumeration that did
      not do so before.}
    @entry[:more-strict]{The sorter is more strict. Comparisons that did return
      the @code{:equal} value of the @symbol{gtk:ordering} enumeration may not
      do so anymore.}
  @end{table}
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
  Its primary user is @class{gtk:sort-list-model} object.

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
;;; gtk_sorter_compare ()
;;;
;;; GtkOrdering
;;; gtk_sorter_compare (GtkSorter *self,
;;;                     gpointer item1,
;;;                     gpointer item2);
;;;
;;; Compares two given items according to the sort order implemented by the
;;; sorter.
;;;
;;; Sorters implement a partial order:
;;;
;;; It is reflexive, ie a = a
;;;
;;; It is antisymmetric, ie if a < b and b < a, then a = b
;;;
;;; It is transitive, ie given any 3 items with a ≤ b and b ≤ c, then a ≤ c
;;;
;;; The sorter may signal it conforms to additional constraints via the return
;;; value of gtk_sorter_get_order().
;;;
;;; self :
;;;     a GtkSorter
;;;
;;; item1 :
;;;     first item to compare.
;;;
;;; item2 :
;;;     second item to compare.
;;;
;;; Returns :
;;;     GTK_ORDERING_EQUAL if item1 == item2 ,
;;;     GTK_ORDERING_SMALLER if item1 < item2 ,
;;;     GTK_ORDERING_LARGER if item1 > item2
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_sorter_compare" sorter-compare) ordering
  (sorter (g:object sorter))
  (item1 :pointer)
  (item2 :pointer))

(export 'sorter-compare)

;;; ----------------------------------------------------------------------------
;;; gtk_sorter_get_order ()
;;;
;;; GtkSorterOrder
;;; gtk_sorter_get_order (GtkSorter *self);
;;;
;;; Gets the order that self conforms to. See GtkSorterOrder for details of the
;;; possible return values.
;;;
;;; This function is intended to allow optimizations.
;;;
;;; self :
;;;     a GtkSorter
;;;
;;; Returns :
;;;     The order
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_sorter_get_order" sorter-order) sorter-order
  (sorter (g:object sorter)))

(export 'sorter-oder)

;;; ----------------------------------------------------------------------------
;;; gtk_sorter_changed ()
;;;
;;; void
;;; gtk_sorter_changed (GtkSorter *self,
;;;                     GtkSorterChange change);
;;;
;;; Emits the “changed” signal to notify all users of the sorter that it has
;;; changed. Users of the sorter should then update the sort order via
;;; gtk_sorter_compare().
;;;
;;; Depending on the change parameter, it may be possible to update the sort
;;; order without a full resorting. Refer to the GtkSorterChange documentation
;;; for details.
;;;
;;; This function is intended for implementors of GtkSorter subclasses and
;;; should not be called from other functions.
;;;
;;; self :
;;;     a GtkSorter
;;;
;;; change :
;;;     How the sorter changed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_sorter_changed" sorter-changed) :void
  (sorter (g:object sorter))
  (change sorter-change))

(export 'sorter-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_ordering_from_cmpfunc
;;;
;;; Converts the result of a GCompareFunc like strcmp() to a GtkOrdering value.
;;;
;;; Since 4.2
;;; ----------------------------------------------------------------------------

#+gtk-4-2
(cffi:defcfun ("gtk_ordering_from_cmpfunc" ordering-from-cmpfunc) ordering
  (result :int))

#+gtk-4-2
(export 'ordering-from-cmpfunc)

;;; --- End of file gtk4.sorter.lisp -------------------------------------------
