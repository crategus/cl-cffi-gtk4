;;; ----------------------------------------------------------------------------
;;; gtk4.sorter.lisp
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
;;; GtkSorter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkSorter" sorter
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_sorter_get_type")
  nil)



;;;typedef struct _GtkSorter GtkSorter;
;;;enum GtkSorterOrder
;;;Describes the type of order that a GtkSorter may describe.

;;;Members
;;;GTK_SORTER_ORDER_PARTIAL

;;;A partial order. Any GtkOrdering is possible.

;;;GTK_SORTER_ORDER_NONE

;;;No order, all elements are considered equal. gtk_sorter_compare() will only return GTK_ORDERING_EQUAL.

;;;GTK_SORTER_ORDER_TOTAL

;;;A total order. gtk_sorter_compare() will only return GTK_ORDERING_EQUAL if an item is compared with itself. Two different items will never cause this value to be returned.

;;;enum GtkSorterChange
;;;Describes changes in a sorter in more detail and allows users to optimize resorting.

;;;Members
;;;GTK_SORTER_CHANGE_DIFFERENT

;;;The sorter change cannot be described by any of the other enumeration values

;;;GTK_SORTER_CHANGE_INVERTED

;;;The sort order was inverted. Comparisons that returned GTK_ORDERING_SMALLER now return GTK_ORDERING_LARGER and vice versa. Other comparisons return the same values as before.

;;;GTK_SORTER_CHANGE_LESS_STRICT

;;;The sorter is less strict: Comparisons may now return GTK_ORDERING_EQUAL that did not do so before.

;;;GTK_SORTER_CHANGE_MORE_STRICT

;;;The sorter is more strict: Comparisons that did return GTK_ORDERING_EQUAL may not do so anymore.

;;;Signal Details
;;;The “changed” signal
;;;void
;;;user_function (GtkSorter      *self,
;;;               GtkSorterChange change,
;;;               gpointer        user_data)
;;;This signal is emitted whenever the sorter changed. Users of the sorter should then update the sort order again via gtk_sorter_compare().

;;;GtkSortListModel handles this signal automatically.

;;;Depending on the change parameter, it may be possible to update the sort order without a full resorting. Refer to the GtkSorterChange documentation for details.

;;;Parameters
;;;self

;;;The GtkSorter

;;;change

;;;how the sorter changed

;;;user_data

;;;user data set when the signal handler was connected.

;;;Flags: Run Last

;;;See Also
;;;GtkSortListModel


;;;Description
;;;GtkSorter is the way to describe sorting criteria. Its primary user is GtkSortListModel.

;;;The model will use a sorter to determine the order in which its items should appear by calling gtk_sorter_compare() for pairs of items.

;;;Sorters may change their sorting behavior through their lifetime. In that case, they will emit the “changed” signal to notify that the sort order is no longer valid and should be updated by calling gtk_sorter_compare() again.

;;;GTK provides various pre-made sorter implementations for common sorting operations. GtkColumnView has built-in support for sorting lists via the “sorter” property, where the user can change the sorting by clicking on list headers.

;;;Of course, in particular for large lists, it is also possible to subclass GtkSorter and provide one's own sorter.

;;;Functions
;;;gtk_sorter_compare ()
;;;GtkOrdering
;;;gtk_sorter_compare (GtkSorter *self,
;;;                    gpointer item1,
;;;                    gpointer item2);
;;;Compares two given items according to the sort order implemented by the sorter.

;;;Sorters implement a partial order:

;;;It is reflexive, ie a = a

;;;It is antisymmetric, ie if a < b and b < a, then a = b

;;;It is transitive, ie given any 3 items with a ≤ b and b ≤ c, then a ≤ c

;;;The sorter may signal it conforms to additional constraints via the return value of gtk_sorter_get_order().

;;;Parameters
;;;self

;;;a GtkSorter

;;;item1

;;;first item to compare.

;;;[type GObject][transfer none]
;;;item2

;;;second item to compare.

;;;[type GObject][transfer none]
;;;Returns
;;;GTK_ORDERING_EQUAL if item1 == item2 , GTK_ORDERING_SMALLER if item1 < item2 , GTK_ORDERING_LARGER if item1 > item2

;;;gtk_sorter_get_order ()
;;;GtkSorterOrder
;;;gtk_sorter_get_order (GtkSorter *self);
;;;Gets the order that self conforms to. See GtkSorterOrder for details of the possible return values.

;;;This function is intended to allow optimizations.

;;;Parameters
;;;self

;;;a GtkSorter

;;;Returns
;;;The order

;;;gtk_sorter_changed ()
;;;void
;;;gtk_sorter_changed (GtkSorter *self,
;;;                    GtkSorterChange change);
;;;Emits the “changed” signal to notify all users of the sorter that it has changed. Users of the sorter should then update the sort order via gtk_sorter_compare().

;;;Depending on the change parameter, it may be possible to update the sort order without a full resorting. Refer to the GtkSorterChange documentation for details.

;;;This function is intended for implementors of GtkSorter subclasses and should not be called from other functions.

;;;Parameters
;;;self

;;;a GtkSorter

;;;change

;;;How the sorter changed


;;; --- End of file gtk4.sorter.lisp -------------------------------------------
