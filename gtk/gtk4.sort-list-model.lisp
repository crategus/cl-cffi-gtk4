;;; ----------------------------------------------------------------------------
;;; gtk4.sort-list-model.lisp
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
;;; GtkSortListModel
;;;
;;;     A list model that sorts its items
;;;
;;; Types and Values
;;;
;;;     GtkSortListModel
;;;
;;; Functions
;;;
;;;     gtk_sort_list_model_new
;;;
;;;     gtk_sort_list_model_set_sorter
;;;     gtk_sort_list_model_get_sorter
;;;     gtk_sort_list_model_set_model
;;;     gtk_sort_list_model_get_model
;;;     gtk_sort_list_model_set_incremental
;;;     gtk_sort_list_model_get_incremental
;;;     gtk_sort_list_model_get_pending
;;;     gtk_sort_list_model_get_section_sorter             Since 4.12
;;;     gtk_sort_list_model_set_section_sorter             Since 4.12
;;;
;;; Properties
;;;
;;;     incremental
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8
;;;     pending
;;;     section-sorter                                     Since 4.12
;;;     sorter
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSortListModel
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;;     GtkSectionModel                                    Since 4.12
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSortListModel
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkSortListModel" sort-list-model
  (:superclass g:object
   :export t
   :interfaces ("GListModel"
                #+gtk-4-12 "GtkSectionModel")
   :type-initializer "gtk_sort_list_model_get_type")
  ((incremental
    sort-list-model-incremental
    "incremental" "gboolean" t t)
   #+gtk-4-8
   (item-type
    %sort-list-model-item-type
    "item-type" "GType" t nil)
   (model
    sort-list-model-model
    "model" "GListModel" t t)
   #+gtk-4-8
   (n-items
    sort-list-model-n-items
    "n-items" "guint" t nil)
   (pending
    sort-list-model-pending
    "pending" "guint" t nil)
   #+gtk-4-12
   (section-sorter
    sort-list-model-section-sorter
    "section-sorter" "GtkSorter" t t)
   (sorter
    sort-list-model-sorter
    "sorter" "GtkSorter" t t)))

#+liber-documentation
(setf (documentation 'sort-list-model 'type)
 "@version{#2023-9-3}
  @begin{short}
    The @class{gtk:sort-list-model} object is a @class{g:list-model} object that
    sorts the elements of an underlying model according to a @class{gtk:sorter}
    object.
  @end{short}

  The model is a stable sort. If two items compare equal according to the
  sorter, the one that appears first in the original model will also appear
  first after sorting. Note that if you change the sorter, the previous order
  will have no influence on the new order. If you want that, consider using a
  @class{gtk:multi-sorter} object and appending the previous sorter to it.

  The model can be set up to do incremental sorting, so that sorting long lists
  does not block the UI. See the @fun{gtk:sort-list-model-incremental} function
  for details.

  The @class{gtk:sort-list-model} object is a generic model and because of that
  it cannot take advantage of any external knowledge when sorting. If you run
  into performance issues with the @class{gtk:sort-list-model} object, it is
  strongly recommended that you write your own sorting list model.

  The @class{gtk:sort-list-model} object allows sorting the items into
  sections.  It implements the @class{gtk:section-model} interface and when the
  @slot[gtk:sort-list-model]{section-sorter} is set, it will sort all items
  with that sorter and items comparing equal with it will be put into the same
  section. The @slot[gtk:sort-list-model]{sorter} property will then be used to
  sort items inside their sections.
  @see-constructor{gtk:sort-list-model-new}
  @see-slot{gtk:sort-list-model-incremental}
  @see-slot{gtk:sort-list-model-item-type}
  @see-slot{gtk:sort-list-model-model}
  @see-slot{gtk:sort-list-model-n-items}
  @see-slot{gtk:sort-list-model-pending}
  @see-slot{gtk:sort-list-model-section-sorter}
  @see-slot{gtk:sort-list-model-sorter}
  @see-class{g:list-model}
  @see-class{gtk:sorter}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:sort-list-model-incremental ----------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "incremental"
                                               'sort-list-model) t)
 "The @code{incremental} property of type @code{:boolean} (Read / Write) @br{}
  Whether the model should sort items incrementally. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'sort-list-model-incremental)
      "Accessor"
      (documentation 'sort-list-model-incremental 'function)
 "@version{#2023-9-3}
  @syntax{(gtk:sort-list-model-incremental object) => incremental}
  @syntax{(setf (gtk:sort-list-model-incremental object) incremental)}
  @argument[object]{a @class{gtk:sort-list-model} object}
  @argument[incremental]{a boolean whether incremental sorting is enabled}
  @begin{short}
    Accessor of the @slot[gtk:sort-list-model]{incremental} slot of the
    @class{gtk:sort-list-model} class.
  @end{short}
  The @fun{gtk:sort-list-model-incremental} function returns whether incremental
  sorting was enabled. The @setf{gtk:slice-list-model-model} function sets the
  sort model to do an incremental sort.

  When incremental sorting is enabled, the sort list model will not do a
  complete sort immediately, but will instead queue an idle handler that
  incrementally sorts the items towards their correct position. This of course
  means that items do not instantly appear in the right place. It also means
  that the total sorting time is a lot slower.

  When your filter blocks the UI while sorting, you might consider turning this
  on. Depending on your model and sorters, this may become interesting around
  10.000 to 100.000 items.

  By default, incremental sorting is disabled. See the
  @fun{gtk:sort-list-model-pending} function for progress information about an
  ongoing incremental sorting operation.
  @see-class{gtk:sort-list-model}
  @see-function{gtk:sort-list-model-pending}")

;;; --- gtk:sort-list-model-item-type ------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type" 'sort-list-model) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. See the @fun{g:list-model-item-type} function. Since 4.8")

#+gtk-4-8
(declaim (inline sort-list-model-item-type))

#+gtk-4-8
(defun sort-list-model-item-type (object)
  (g:list-model-item-type object))

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'sort-list-model-item-type)
      "Accessor"
      (documentation 'sort-list-model-item-type 'function)
 "@version{#2023-9-3}
  @syntax{(gtk:sort-list-model-item-type object) => gtype}
  @argument[object]{a @class{gtk:sort-list-model} object}
  @argument[gtype]{a @class{g:type-t} type}
  @begin{short}
    Accessor of the @slot[gtk:sort-list-model]{item-type} slot of the
    @class{gtk:sort-list-model} class.
  @end{short}
  The type of items contained in the list model. Items must be subclasses of
  the @class{g:object} class.
  @begin[Note]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:sort-list-model}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- gtk:sort-list-model-model ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'sort-list-model) t)
 "The @code{model} property of type @class{g:list-model} (Read / Write) @br{}
  The model being sorted.")

#+liber-documentation
(setf (liber:alias-for-function 'sort-list-model-model)
      "Accessor"
      (documentation 'sort-list-model-model 'function)
 "@version{#2023-9-3}
  @syntax{(gtk:sort-list-model-model object) => model}
  @syntax{(setf (gtk:sort-list-model-model object) model)}
  @argument[object]{a @class{gtk:sort-list-model} object}
  @argument[model]{a @class{g:list-model} object to be sorted}
  @begin{short}
    Accessor of the @slot[gtk:sort-list-model]{model} slot of the
    @class{gtk:sort-list-model} class.
  @end{short}
  The @fun{gtk:sort-list-model-model} function gets the model currently sorted
  or @code{nil} if none. The @setf{gtk:sort-list-model-model} function sets the
  model to be sorted. The item type of @arg{model} must conform to the item type
  of @arg{object}.
  @see-class{gtk:sort-list-model}
  @see-class{g:list-model}")

;;; --- gtk:sort-list-model-n-items --------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'sort-list-model) t)
 "The @code{n-items} property of type @code{:uint} (Read / Write) @br{}
  The number of items. See the @fun{g:list-model-n-items} function. Since 4.8
  @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'sort-list-model-n-items)
      "Accessor"
      (documentation 'sort-list-model-n-items 'function)
 "@version{#2023-9-3}
  @syntax{(gtk:sort-list-model-n-items object) => n-items}
  @argument[object]{a @class{gtk:sort-list-model} object}
  @argument[n-items]{an unsigned integer with the number of items contained in
    the model}
  @begin{short}
    Accessor of the @slot[gtk:sort-list-model]{n-items} slot of the
    @class{gtk:sort-list-model} class.
  @end{short}
  @see-class{g:sort-list-model}
  @see-function{g:list-model-n-items}")

;;; --- gtk:sort-list-model-pending --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pending" 'sort-list-model) t)
 "The @code{pending} property of type @code{:uint} (Read / Write) @br{}
  Estimate of unsorted items remaining. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'sort-list-model-pending)
      "Accessor"
      (documentation 'sort-list-model-pending 'function)
 "@version{#2023-9-3}
  @syntax{(gtk:sort-list-model-pending object) => pending}
  @argument[object]{a @class{gtk:sort-list-model} object}
  @argument[pending]{an unsigned integer with the estimate of unsorted items
    remaning}
  @begin{short}
    Accessor of the @slot[gtk:sort-list-model]{pending} slot of the
    @class{gtk:sort-list-model} class.
  @end{short}
  The @fun{gtk:sort-list-model-pending} estimates progress of an ongoing sorting
  operation.

  The estimate is the number of items that would still need to be sorted to
  finish the sorting operation if this was a linear algorithm. So this number is
  not related to how many items are already correctly sorted.

  If you want to estimate the progress, you can use code like this:
  @begin{pre}
pending = gtk_sort_list_model_get_pending (self);
model = gtk_sort_list_model_get_model (self);
progress = 1.0 - pending / (double) MAX (1, g_list_model_get_n_items (model));
  @end{pre}
  If no sort operation is ongoing - in particular when the
  @slot[gtk:sort-list-model]{incremental} property is @em{false} - this function
  returns 0.
  @see-class{g:sort-list-model}")

;;; --- gtk:sort-list-model-section-sorter -------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "section-sorter"
                                               'sort-list-model) t)
 "The @code{section-sorter} property of type @class{gtk:sorter} (Read / Write)
  @br{}
  The section sorter for this model, if one is set.")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'sort-list-model-section-sorter)
      "Accessor"
      (documentation 'sort-list-model-section-sorter 'function)
 "@version{#2023-9-3}
  @syntax{(gtk:sort-list-model-section-sorter object) => sorter}
  @syntax{(setf (gtk:sort-list-model-section-sorter object) sorter)}
  @argument[object]{a @class{gtk:sort-list-model} object}
  @argument[sorter]{a @class{gtk:sorter} object to sort @arg{object} with}
  @begin{short}
    Accessor of the @slot[gtk:sort-list-model]{section-sorter} slot of the
    @class{gtk:sort-list-model} class.
  @end{short}
  The @fun{gtk:sort-list-model-section-sorter} function gets the section sorter
  that is used to sort items of the list model into sections. The
  @setf{gtk:sort-list-model-section-sorter} function sets a new section sorter.
  @see-class{gtk:sort-list-model}
  @see-class{gtk:sorter}")

;;; --- gtk:sort-list-model-sorter ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sorter" 'sort-list-model) t)
 "The @code{sorter} property of type @class{gtk:sorter} (Read / Write) @br{}
  The sorter for this model.")

#+liber-documentation
(setf (liber:alias-for-function 'sort-list-model-sorter)
      "Accessor"
      (documentation 'sort-list-model-sorter 'function)
 "@version{#2023-9-3}
  @syntax{(gtk:sort-list-model-sorter object) => sorter}
  @syntax{(setf (gtk:sort-list-model-sorter object) sorter)}
  @argument[object]{a @class{gtk:sort-list-model} object}
  @argument[sorter]{a @class{gtk:sorter} object to sort @arg{object} with}
  @begin{short}
    Accessor of the @slot[gtk:sort-list-model]{sorter} slot of the
    @class{gtk:sort-list-model} class.
  @end{short}
  The @fun{gtk:sort-list-model-sorter} function gets the sorter that is used to
  sort @arg{object}. The @setf{gtk:sort-list-model-sorter} function sets a new
  sorter.
  @see-class{gtk:sort-list-model}
  @see-class{gtk:sorter}")

;;; ----------------------------------------------------------------------------
;;; gtk_sort_list_model_new
;;; ----------------------------------------------------------------------------

(declaim (inline sort-list-model-new))

(defun sort-list-model-new (model sorter)
 #+liber-documentation
 "@version{#2023-9-21}
  @argument[model]{a @class{g:list-model} object, or @code{nil}}
  @argument[sorter]{a @class{gtk:sorter} object to sort @arg{model} with,
    or @code{nil}}
  @return{The new @class{gtk:sort-list-model} object.}
  @begin{short}
    Creates a new sort list model that uses the sorter to sort @arg{model}.
  @end{short}
  @see-class{gtk:sort-list-model}"
  (make-instance 'sort-list-model
                 :model model
                 :sorter sorter))

(export 'sort-list-model-new)

;;; --- End of file gtk4.sort-list-model.lisp ----------------------------------
