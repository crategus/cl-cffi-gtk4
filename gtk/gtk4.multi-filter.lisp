;;; ----------------------------------------------------------------------------
;;; gtk4.multi-filter.lisp
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
;;; GtkMultiFilter
;;;
;;;     Combining multiple filters
;;;
;;; Types and Values
;;;
;;;     GtkMultiFilter
;;;     GtkAnyFilter
;;;     GtkEveryFilter
;;;
;;; Functions
;;;
;;;     gtk_multi_filter_append
;;;     gtk_multi_filter_remove
;;;     gtk_any_filter_new
;;;     gtk_every_filter_new
;;;
;;; Properties
;;;
;;;     item-type                                          Since 4.8
;;;     n-items                                            Since 4.8
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkFilter
;;;         ╰── GtkMultiFilter
;;;             ├── GtkAnyFilter
;;;             ╰── GtkEveryFilter
;;;
;;; Implemented Interfaces
;;;
;;; GtkMultiFilter implements GListModel and GtkBuildable
;;; GtkAnyFilter implements GListModel and GtkBuildable
;;; GtkEveryFilter implements GListModel and GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkMultiFilter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkMultiFilter" multi-filter
  (:superclass filter
   :export t
   :interfaces ("GListModel"
                "GtkBuildable")
   :type-initializer "gtk_multi_filter_get_type")
  (#+gtk-4-8
   (item-type
    multi-filter-item-type
    "item-type" "GType" t nil)
   #+gtk-4-8
   (n-items
    multi-filter-n-items
    "n-items" "guint" t nil)))

;;; ----------------------------------------------------------------------------
;;; GtkAnyFilter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkAnyFilter" any-filter
  (:superclass multi-filter
   :export t
   :interfaces ("GListModel"
                "GtkBuildable")
   :type-initializer "gtk_any_filter_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GtkEveryFilter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkEveryFilter" every-filter
  (:superclass multi-filter
   :export t
   :interfaces ("GListModel"
                "GtkBuildable")
   :type-initializer "gtk_every_filter_get_type")
  nil)


;;;Description
;;;GtkMultiFilter is the base type that implements support for handling multiple filters.

;;;GtkAnyFilter is a subclass of GtkMultiFilter that matches an item when at least one of its filters matches.

;;;GtkEveryFilter is a subclass of GtkMultiFilter that matches an item when each of its filters matches.

;;;Functions
;;;gtk_multi_filter_append ()
;;;void
;;;gtk_multi_filter_append (GtkMultiFilter *self,
;;;                         GtkFilter *filter);
;;;Adds a filter to self to use for matching.

;;;Parameters
;;;self

;;;a GtkMultiFilter

;;;filter

;;;A new filter to use.

;;;[transfer full]
;;;gtk_multi_filter_remove ()
;;;void
;;;gtk_multi_filter_remove (GtkMultiFilter *self,
;;;                         guint position);
;;;Removes the filter at the given position from the list of filters used by self . If position is larger than the number of filters, nothing happens and the function returns.

;;;Parameters
;;;self

;;;a GtkMultiFilter

;;;position

;;;position of filter to remove

;;;gtk_any_filter_new ()
;;;GtkAnyFilter *
;;;gtk_any_filter_new (void);
;;;Creates a new empty "any" filter. Use gtk_multi_filter_append() to add filters to it.

;;;This filter matches an item if any of the filters added to it matches the item. In particular, this means that if no filter has been added to it, the filter matches no item.

;;;Returns
;;;a new GtkAnyFilter

;;;gtk_every_filter_new ()
;;;GtkEveryFilter *
;;;gtk_every_filter_new (void);
;;;Creates a new empty "every" filter. Use gtk_multi_filter_append() to add filters to it.

;;;This filter matches an item if each of the filters added to it matches the item. In particular, this means that if no filter has been added to it, the filter matches every item.

;;;Returns
;;;a new GtkEveryFilter


;;; --- End of file gtk4.multi-filter.lisp -------------------------------------
