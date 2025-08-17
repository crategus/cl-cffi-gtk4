;;; ----------------------------------------------------------------------------
;;; gtk4.multi-filter.lisp
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
;;;
;;;     gtk_any_filter_new
;;;     gtk_every_filter_new
;;;
;;; Properties
;;;
;;;     item-type                                           Since 4.8
;;;     n-items                                             Since 4.8
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

(gobject:define-gobject "GtkMultiFilter" multi-filter
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

#+liber-documentation
(setf (documentation 'multi-filter 'type)
 "@version{2024-09-28}
  @begin{short}
    The @class{gtk:multi-filter} class is the base type that implements support
    for handling multiple filters.
  @end{short}
  @see-slot{gtk:multi-filter-item-type}
  @see-slot{gtk:multi-filter-n-items}
  @see-class{gtk:filter}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:multi-filter-item-type ---------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type" 'multi-filter) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. Since 4.8")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'multi-filter-item-type)
      "Accessor"
      (documentation 'multi-filter-item-type 'function)
 "@version{2025-08-12}
  @syntax{(gtk:multi-filter-item-type object) => gtype}
  @argument[object]{a @class{gtk:multi-filter} object}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    The accessor for the @slot[gtk:multi-filter]{item-type} slot of the
    @class{gtk:multi-filter} class gets the type of items contained in the list
    model.
  @end{short}
  Items must be subclasses of the @class{g:object} class.

  Since 4.8
  @begin[Notes]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:multi-filter}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- gtk:multi-filter-n-items -----------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'multi-filter) t)
 "The @code{n-items} property of type @code{:uint} (Read) @br{}
  The number of items. Since 4.8 @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'multi-filter-n-items)
      "Accessor"
      (documentation 'multi-filter-n-items 'function)
 "@version{2025-08-12}
  @syntax{(gtk:multi-filter-n-items object) => n-items}
  @argument[object]{a @class{gtk:multi-filter} object}
  @argument[n-items]{an unsigned integer for the number of items contained in
    the model}
  @begin{short}
    The accessor for the @slot[gtk:multi-filter]{n-items} slot of the
    @class{gtk:multi-filter} class gets the number of items contained in the
    model.
  @end{short}

  Since 4.8
  @see-class{gtk:multi-filter}
  @see-function{g:list-model-n-items}")

;;; ----------------------------------------------------------------------------
;;; gtk_multi_filter_append
;;; ----------------------------------------------------------------------------

;; TODO: gtk_multi_filter_append takes the ownerhship of the FILTER argument.
;; We pass a reference of FILTER to the function. This avoids crashing the
;; testsuite. But it seems that sbcl does not free the used memory completly.
;; Is the memory management now completly correct?

(cffi:defcfun ("gtk_multi_filter_append" %multi-filter-append) :void
  (object (g:object multi-filter))
  (filter (g:object filter)))

(defun multi-filter-append (object filter)
 #+liber-documentation
 "@version{2024-10-01}
  @argument[object]{a @class{gtk:multi-filter} object}
  @argument[filter]{a @class{gtk:filter} object to use}
  @begin{short}
    Adds a filter to @arg{object} to use for matching.
  @end{short}
  @see-class{gtk:multi-filter}
  @see-class{gtk:filter}"
  (%multi-filter-append object (g:object-ref filter)))

(export 'multi-filter-append)

;;; ----------------------------------------------------------------------------
;;; gtk_multi_filter_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_multi_filter_remove" multi-filter-remove) :void
 #+liber-documentation
 "@version{2025-03-13}
  @argument[object]{a @class{gtk:multi-filter} object}
  @argument[pos]{an unsigned integer for the position of the filter to remove}
  @begin{short}
    Removes the filter at the given @arg{pos} from the list of filters used by
    @arg{object}.
  @end{short}
  If the @arg{pos} parameter is larger than the number of filters, nothing
  happens and the function returns.
  @see-class{gtk:multi-filter}"
  (object (g:object multi-filter))
  (pos :uint))

(export 'multi-filter-remove)

;;; ----------------------------------------------------------------------------
;;; GtkAnyFilter
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkAnyFilter" any-filter
  (:superclass multi-filter
   :export t
   :interfaces ("GListModel"
                "GtkBuildable")
   :type-initializer "gtk_any_filter_get_type")
  nil)

#+liber-documentation
(setf (documentation 'any-filter 'type)
 "@version{2024-09-28}
  @begin{short}
    The @class{gtk:any-filter} class is a subclass of the
    @class{gtk:multi-filter} class that matches an item when at least one of
    its filters matches.
  @end{short}
  @see-constructor{gtk:any-filter-new}
  @see-class{gtk:multi-filter}")

;;; ----------------------------------------------------------------------------
;;; gtk_any_filter_new
;;; ----------------------------------------------------------------------------

(declaim (inline any-filter-new))

(defun any-filter-new ()
 "@version{2024-09-28}
  @return{The new @class{gtk:any-filter} object.}
  @begin{short}
    Creates a new empty \"any\" filter.
  @end{short}
  Use the @fun{gtk:multi-filter-append} function to add filters to it. This
  filter matches an item if any of the filters added to it matches the item.
  In particular, this means that if no filter has been added to it, the filter
  matches no item.
  @see-class{gtk:any-filter}
  @see-function{gtk:multi-filter-append}"
  (make-instance 'any-filter))

(export 'any-filter-new)

;;; ----------------------------------------------------------------------------
;;; GtkEveryFilter
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkEveryFilter" every-filter
  (:superclass multi-filter
   :export t
   :interfaces ("GListModel"
                "GtkBuildable")
   :type-initializer "gtk_every_filter_get_type")
  nil)

#+liber-documentation
(setf (documentation 'every-filter 'type)
 "@version{2024-09-28}
  @begin{short}
    The @class{gtk:every-filter} class is a subclass of the
    @class{gtk:multi-filter} class that matches an item when each of its
    filters matches.
  @end{short}
  @see-constructor{gtk:every-filter-new}
  @see-class{gtk:multi-filter}")

;;; ----------------------------------------------------------------------------
;;; gtk_every_filter_new
;;; ----------------------------------------------------------------------------

(defun every-filter-new ()
 "@version{2024-09-28}
  @return{The new @class{gtk:every-filter} object.}
  @begin{short}
    Creates a new empty \"every\" filter.
  @end{short}
  Use the @fun{gtk:multi-filter-append} function to add filters to it. This
  filter matches an item if each of the filters added to it matches the item.
  In particular, this means that if no filter has been added to it, the filter
  matches every item.
  @see-class{gtk:every-filter}
  @see-function{gtk:multi-filter-append}"
  (make-instance 'every-filter))

(export 'every-filter-new)

;;; --- End of file gtk4.multi-filter.lisp -------------------------------------
