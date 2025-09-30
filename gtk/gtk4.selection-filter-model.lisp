;;; ----------------------------------------------------------------------------
;;; gtk4.selection-filter-model.lisp
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
;;; GtkSelectionFilterModel
;;;
;;;     A list model that turns a selection in a model
;;;
;;; Types and Values
;;;
;;;     GtkSelectionFilterModel
;;;
;;; Accessors
;;;
;;;     gtk_selection_filter_model_set_model
;;;     gtk_selection_filter_model_get_model
;;;
;;; Functions
;;;
;;;     gtk_selection_filter_model_new
;;;
;;; Properties
;;;
;;;     item-type                                           Since 4.8
;;;     model
;;;     n-items                                             Since 4.8
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSelectionFilterModel
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSelectionFilterModel
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkSelectionFilterModel" selection-filter-model
  (:superclass g:object
   :export t
   :interfaces ("GListModel")
   :type-initializer "gtk_selection_filter_model_get_type")
  (#+gtk-4-8
   (item-type
    selection-filter-model-item-type
    "item-type" "GType" t nil)
   (model
    selection-filter-model-model
    "model" "GListModel" t t)
   #+gtk-4-8
   (n-items
    selection-filter-model-n-items
    "n-items" "guint" t nil)))

#+liber-documentation
(setf (documentation 'selection-filter-model 'type)
 "@version{2024-12-15}
  @begin{short}
    The @class{gtk:selection-filter-model} class is a list model that presents
    the selected items in a @class{gtk:selection-model} as its own list model.
  @end{short}
  @see-constructor{gtk:selection-filter-model-new}
  @see-class{gtk:selection-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:selection-filter-model-item-type -----------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type"
                                               'selection-filter-model) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. Since 4.8")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'selection-filter-model-item-type)
      "Accessor"
      (documentation 'selection-filter-model-item-type 'function)
 "@version{2025-08-17}
  @syntax{(gtk:selection-filter-model-item-type object) => gtype}
  @argument[object]{a @class{gtk:selection-filter-model} object}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    The accessor for the @slot[gtk:selection-filter-model]{item-type} slot of
    the @class{gtk:selection-filter-model} class returns the type of items
    contained in the list model.
  @end{short}
  Items must be subclasses of the @class{g:object} class.

  Since 4.8
  @begin[Notes]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:selection-filter-model}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- gtk:selection-filter-model-model ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model"
                                               'selection-filter-model) t)
 "The @code{model} property of type @class{g:list-model} (Read / Write) @br{}
  The model being filtered.")

#+liber-documentation
(setf (liber:alias-for-function 'selection-filter-model-model)
      "Accessor"
      (documentation 'selection-filter-model-model 'function)
 "@version{2025-09-28}
  @syntax{(gtk:selection-filter-model-model object) => model}
  @syntax{(setf (gtk:selection-filter-model-model object) model)}
  @argument[object]{a @class{gtk:selection-filter-model} object}
  @argument[model]{a @class{g:list-model} object to wrap}
  @begin{short}
    The accessor for the @slot[gtk:selection-filter-model]{model} slot of the
    @class{gtk:selection-filter-model} class gets or sets the model filtered.
  @end{short}
  Returns @code{nil} if none.

  Note that GTK makes no effort to ensure that @arg{model} conforms to the item
  type of @arg{object}. It assumes that the caller knows what they are doing and
  have set up an appropriate filter to ensure that item types match.
  @see-class{gtk:selection-filter-model}
  @see-class{g:list-model}")

;;; --- gtk:selection-filter-model-n-items -------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items"
                                               'selection-filter-model) t)
 "The @code{n-items} property of type @code{:uint} (Read) @br{}
  The number of items contained in the model. Since 4.8 @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'selection-filter-model-n-items)
      "Accessor"
      (documentation 'selection-filter-model-n-items 'function)
 "@version{2025-08-17}
  @syntax{(gtk:selection-filter-model-n-items object) => n-items}
  @argument[object]{a @class{gtk:selection-filter-model} object}
  @argument[n-items]{an unsigned integer for the number of items contained in
    the model}
  @begin{short}
    The accessor for the @slot[gtk:selection-filter-model]{n-items} slot of the
    @class{gtk:selection-filter-model} class returns the number of items
    contained in the model.
  @end{short}
  @see-class{gtk:selection-filter-model}
  @see-function{g:list-model-n-items}")

;;; ----------------------------------------------------------------------------
;;; gtk_selection_filter_model_new
;;; ----------------------------------------------------------------------------

(declaim (inline selection-filter-model-new))

(defun selection-filter-model-new (model)
 #+liber-documentation
 "@version{2024-12-15}
  @argument[model]{a @class{gtk:selection-model} object to filter, or
    @code{nil}}
  @return{The new @class{gtk:selection-filter-model} object.}
  @begin{short}
    Creates a new @class{gtk:selection-filter-model} object that will include
    the selected items from the underlying selection model.
  @end{short}
  @see-class{gtk:selection-filter-model}
  @see-class{gtk:selection-model}"
  (make-instance 'selection-filter-model
                 :model model))

(export 'selection-filter-model-new)

;;; --- End of file gtk4.selection-filter-model.lisp ---------------------------
