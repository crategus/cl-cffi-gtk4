;;; ----------------------------------------------------------------------------
;;; gtk4.filter-list-model.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
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
;;; GtkFilterListModel
;;;
;;;     A list model that filters its items
;;;
;;; Types and Values
;;;
;;;     GtkFilterListModel
;;;
;;; Accessors
;;;
;;;     gtk_filter_list_model_set_model
;;;     gtk_filter_list_model_get_model
;;;     gtk_filter_list_model_set_filter
;;;     gtk_filter_list_model_get_filter
;;;     gtk_filter_list_model_set_incremental
;;;     gtk_filter_list_model_get_incremental
;;;     gtk_filter_list_model_get_pending
;;;
;;; Functions
;;;
;;;     gtk_filter_list_model_new
;;;
;;; Properties
;;;
;;;     filter
;;;     incremental
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8
;;;     pending
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkFilterListModel
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFilterListModel
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkFilterListModel" filter-list-model
  (:superclass g:object
   :export t
   :interfaces ("GListModel"
                #+gtk-4-12
                "GtkSectionModel")
   :type-initializer "gtk_filter_list_model_get_type")
  ((filter
    filter-list-model-filter
    "filter" "GtkFilter" t t)
   (incremental
    filter-list-model-incremental
    "incremental" "gboolean" t t)
   #+gtk-4-8
   (item-type
    filter-list-model-item-type   ; only internal, not used
    "item-type" "GType" t nil)
   (model
    filter-list-model-model
    "model" "GListModel" t t)
   #+gtk-4-8
   (n-items
    filter-list-model-n-items
    "n-items" "guint" t nil)
   (pending
    filter-list-model-pending
    "pending" "guint" t nil)))

#+liber-documentation
(setf (documentation 'filter-list-model 'type)
 "@version{2023-9-28}
  @begin{short}
    The @class{gtk:filter-list-model} object is a list model that filters the
    elements of the underlying model according to a @class{gtk:filter} object.
  @end{short}
  It hides some elements from the other model according to criteria given by a
  @class{gtk:filter} object.

  The model can be set up to do incremental searching, so that filtering long
  lists does not block the UI. See the @fun{gtk:filter-list-model-incremental}
  function for details.
  @see-constructor{gtk:filter-list-model-new}
  @see-slot{gtk:filter-list-model-filter}
  @see-slot{gtk:filter-list-model-incremental}
  @see-slot{gtk:filter-list-model-item-type}
  @see-slot{gtk:filter-list-model-model}
  @see-slot{gtk:filter-list-model-n-items}
  @see-slot{gtk:filter-list-model-pending}
  @see-class{g:list-model}
  @see-class{gtk:filter}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:filter-list-model-filter -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "filter" 'filter-list-model) t)
 "The @code{filter} property of type @class{gtk:filter} (Read / Write) @br{}
  The filter for the model.")

#+liber-documentation
(setf (liber:alias-for-function 'filter-list-model-filter)
      "Accessor"
      (documentation 'filter-list-model-filter 'function)
 "@version{2023-9-28}
  @syntax{(gtk:filter-list-model-filter object) => filter}
  @syntax{(setf (gtk:filter-list-model-filter object) filter)}
  @argument[object]{a @class{gtk:filter-list-model} object}
  @argument[filter]{a @class{gtk:filter} object to use or @code{nil} to not
    filter items}
  @begin{short}
    Accessor of the @slot[gtk:filter-list-model]{filter} slot of the
    @class{gtk:filter-list-model} class.
  @end{short}
  The @fun{gtk:filter-list-model-filter} function gets the filter currently set.
  The @setf{gtk:filter-list-model-model} function sets the filter used to filter
  items.
  @see-class{gtk:filter-list-model}
  @see-class{gtk:filter}")

;;; --- gtk:filter-list-model-incremental --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "incremental"
                                               'filter-list-model) t)
 "The @code{incremental} property of type @code{:boolean} (Read / Write) @br{}
  Whether the model should filter items incrementally. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'filter-list-model-incremental)
      "Accessor"
      (documentation 'filter-list-model-incremental 'function)
 "@version{2023-9-28}
  @syntax{(gtk:filter-list-model-incremental object) => incremental}
  @syntax{(setf (gtk:filter-list-model-incremental object) incremental)}
  @argument[object]{a @class{gtk:filter-list-model} object}
  @argument[incremental]{@em{true} if incremental filtering is enabled}
  @begin{short}
    Accessor of the @slot[gtk:filter-list-model]{incremental} slot of the
    @class{gtk:filter-list-model} class.
  @end{short}
  The @fun{gtk:filter-list-model-incremental} function returns whether
  incremental filtering was enabled. The
  @setf{gtk:filter-list-model-incremental} function sets incremental filtering.

  When incremental filtering is enabled, the @class{gtk:filter-list-model}
  object will not run filters immediately, but will instead queue an idle
  handler that incrementally filters the items and adds them to the list. This
  of course means that items are not instantly added to the list, but only
  appear incrementally.

  When your filter blocks the UI while filtering, you might consider turning
  this on. Depending on your model and filters, this may become interesting
  around 10,000 to 100,000 items. By default, incremental filtering is disabled.

  See the @fun{gtk:filter-list-model-pending} function for progress information
  about an ongoing incremental filtering operation.
  @see-class{gtk:filter-list-model}
  @see-function{gtk:filter-list-model-pending}")

;;; --- gtk:filter-list-model-item-type ----------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type"
                                               'filter-list-model) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. Since 4.8")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'filter-list-model-item-type)
      "Accessor"
      (documentation 'filter-list-model-item-type 'function)
 "@version{2024-12-22}
  @syntax{(gtk:filter-list-model-item-type object) => gtype}
  @argument[object]{a @class{gtk:filter-list-model} object}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    Accessor of the @slot[gtk:filter-list-model]{item-type} slot of the
    @class{gtk:filter-list-model} class.
  @end{short}
  The type of items contained in the list model. Items must be subclasses of
  the @class{g:object} class.

  Since 4.8
  @begin[Notes]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:filter-list-model}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- gtk:filter-list-model-model --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'filter-list-model) t)
 "The @code{model} property of type @class{g:list-model} (Read / Write) @br{}
  The model being filtered.")

#+liber-documentation
(setf (liber:alias-for-function 'filter-list-model-model)
      "Accessor"
      (documentation 'filter-list-model-model 'function)
 "@version{2023-9-28}
  @syntax{(gtk:filter-list-model-model object) => model}
  @syntax{(setf (gtk:filter-list-model-model object) model)}
  @argument[object]{a @class{gtk:filter-list-model} object}
  @argument[model]{a @class{g:list-model} object that gets filtered}
  @begin{short}
    Accessor of the @slot[gtk:filter-list-model]{model} slot of the
    @class{gtk:filter-list-model} class.
  @end{short}
  The @fun{gtk:filter-list-model-model} function gets the model currently
  filtered or @code{nil} if none. The @setf{gtk:filter-list-model-model}
  function sets the model to be filtered.

  Note that GTK makes no effort to ensure that @arg{model} conforms to the item
  type of @arg{object}. It assumes that the caller knows what they are doing and
  have set up an appropriate filter to ensure that item types match.
  @see-class{gtk:filter-list-model}
  @see-class{g:list-model}")

;;; --- gtk:filter-list-model-n-items ------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'filter-list-model) t)
 "The @code{n-items} property of type @code{:uint} (Read / Write) @br{}
  The number of items. Since 4.8 @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'filter-list-model-n-items)
      "Accessor"
      (documentation 'filter-list-model-n-items 'function)
 "@version{2023-9-28}
  @syntax{(gtk:filter-list-model-n-items object) => n-items}
  @argument[object]{a @class{gtk:filter-list-model} object}
  @argument[n-items]{an unsigned integer with the number of items contained in
    the model}
  @begin{short}
    Accessor of the @slot[gtk:filter-list-model]{n-items} slot of the
    @class{gtk:filter-list-model} class.
  @end{short}
  @see-class{gtk:filter-list-model}
  @see-function{g:list-model-n-items}")

;;; --- gtk:filter-list-model-pending ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pending" 'filter-list-model) t)
 "The @code{pending} property of type @code{:uint} (Read) @br{}
  Number of items not yet filtered. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'filter-list-model-pending)
      "Accessor"
      (documentation 'filter-list-model-pending 'function)
 "@version{2023-9-28}
  @syntax{(gtk:filter-list-model-pending object) => pending}
  @argument[object]{a @class{gtk:filter-list-model} object}
  @argument[pending]{an unsigned integer with the number of items not yet
    filtered.}
  @begin{short}
    Accessor of the @slot[gtk:filter-list-model]{pending} slot of the
    @class{gtk:filter-list-model} class.
  @end{short}
  The @fun{gtk:filter-list-model-pending} function returns the number of items
  that have not been filtered yet.

  You can use this value to check if @arg{object} is busy filtering by comparing
  the return value to 0 or you can compute the percentage of the filter
  remaining by dividing the return value by the total number of items in the
  underlying model:
  @begin{pre}
(let ((percentage (/ (gtk:filter-list-model-pending object)
                     (gtk:filter-list-model-n-items object))))
  ... )
  @end{pre}
  If no filter operation is ongoing - in particular when the
  @slot[gtk:filter-list-model]{incremental} property is @em{false} - this
  function returns 0.
  @see-class{gtk:filter-list-model}
  @see-function{gtk:filter-list-model-incremental}")

;;; ----------------------------------------------------------------------------
;;; gtk_filter_list_model_new
;;; ----------------------------------------------------------------------------

(declaim (inline filter-list-model-new))

(defun filter-list-model-new (model filter)
 "@version{2023-9-28}
  @argument[model]{a @class{g:list-model} object to sort, or @code{nil}}
  @argument[filter]{a @class{gtk:filter} object or @code{nil} to not filter
    items}
  @begin{short}
    Creates a new @class{gtk:filter-list-model} object that will filter
    @arg{model} using the given @arg{filter}.
  @end{short}
  @see-class{gtk:filter-list-model}
  @see-class{gtk:filter}"
  (make-instance 'filter-list-model
                 :model model
                 :filter filter))

(export 'filter-list-model-new)

;;; --- End of file gtk4.filter-list-model -------------------------------------
