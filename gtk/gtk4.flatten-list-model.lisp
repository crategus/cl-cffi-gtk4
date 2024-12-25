;;; ----------------------------------------------------------------------------
;;; gtk4.flatten-list-model.lisp
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
;;; GtkFlattenListModel
;;;
;;;     A list model that flattens a list of lists
;;;
;;; Types and Values
;;;
;;;     GtkFlattenListModel
;;;
;;; Accessors
;;;
;;;     gtk_flatten_list_model_set_model
;;;     gtk_flatten_list_model_get_model
;;;
;;; Functions
;;;
;;;     gtk_flatten_list_model_new
;;;     gtk_flatten_list_model_get_model_for_item
;;;
;;; Properties
;;;
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkFlattenListModel
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFlattenListModel
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkFlattenListModel" flatten-list-model
  (:superclass g:object
   :export t
   :interfaces ("GListModel"
                #+gtk-4-12 "GtkSectionModel")
   :type-initializer "gtk_flatten_list_model_get_type")
  (#+gtk-4-8
   (item-type
    flatten-list-model-item-type
    "item-type" "GType" t nil)
   (model
    flatten-list-model-model
    "model" "GListModel" t t)
   #+gtk-4-8
   (n-items
    flatten-list-model-n-items
    "n-items" "guint" t nil)))

#+liber-documentation
(setf (documentation 'flatten-list-model 'type)
 "@version{2024-12-9}
  @begin{short}
    The @class{gtk:flatten-list-model} object is a list model that takes a list
    model containing list models and flattens it into a single model.
  @end{short}
  Another term for this is concatenation. The @class{gtk:flatten-list-model}
  object takes a list of lists and concatenates them into a single list.
  @see-constructor{gtk:flatten-list-model-new}
  @see-slot{gtk:flatten-list-model-item-type}
  @see-slot{gtk:flatten-list-model-model}
  @see-slot{gtk:flatten-list-model-n-items}
  @see-class{g:list-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:flatten-list-model-item-type ---------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type"
                                               'flatten-list-model) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. Since 4.8")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'flatten-list-model-item-type)
      "Accessor"
      (documentation 'flatten-list-model-item-type 'function)
 "@version{2024-12-22}
  @syntax{(gtk:flatten-list-model-item-type object) => gtype}
  @argument[object]{a @class{gtk:flatten-list-model} object}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    Accessor of the @slot[gtk:flatten-list-model]{item-type} slot of the
    @class{gtk:flatten-list-model} class.
  @end{short}
  The type of items contained in the list model. Items must be subclasses of
  the @class{g:object} class.

  Since 4.8
  @begin[Notes]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:flatten-list-model}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- gtk:flatten-list-model-model -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'flatten-list-model) t)
 "The @code{model} property of type @class{g:list-model} (Read / Write) @br{}
  The model being flattened.")

#+liber-documentation
(setf (liber:alias-for-function 'flatten-list-model-model)
      "Accessor"
      (documentation 'flatten-list-model-model 'function)
 "@version{2024-12-9}
  @syntax{(gtk:flatten-list-model-model object) => model}
  @syntax{(setf (gtk:flatten-list-model-model object) model)}
  @argument[object]{a @class{gtk:flatten-list-model} object}
  @argument[model]{a @class{g:list-model} object that gets flattened}
  @begin{short}
    Accessor of the @slot[gtk:flatten-list-model]{model} slot of the
    @class{gtk:flatten-list-model} class.
  @end{short}
  The @fun{gtk:flatten-list-model-model} function gets the model currently
  flattened or @code{nil} if none. The @setf{gtk:flatten-list-model-model}
  function sets the model to be flattened.
  @see-class{gtk:flatten-list-model}
  @see-class{g:list-model}")

;;; --- gtk:flatten-list-model-n-items -----------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'flatten-list-model) t)
 "The @code{n-items} property of type @code{:uint} (Read / Write) @br{}
  The number of items. Since 4.8 @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'flatten-list-model-n-items)
      "Accessor"
      (documentation 'flatten-list-model-n-items 'function)
 "@version{2024-12-9}
  @syntax{(gtk:flatten-list-model-n-items object) => n-items}
  @argument[object]{a @class{gtk:flatten-list-model} object}
  @argument[n-items]{an unsigned integer with the number of items contained in
    the model}
  @begin{short}
    Accessor of the @slot[gtk:flatten-list-model]{n-items} slot of the
    @class{gtk:flatten-list-model} class.
  @end{short}
  @see-class{gtk:flatten-list-model}
  @see-function{g:list-model-n-items}")

;;; ----------------------------------------------------------------------------
;;; gtk_flatten_list_model_new
;;; ----------------------------------------------------------------------------

(declaim (inline flatten-list-model-new))

(defun flatten-list-model-new (model)
 #+liber-documentation
 "@version{2024-12-9}
  @argument[model]{a @class{g:list-model} object to be flattened}
  @return{The new @class{gtk:flatten-list-model} object.}
  @begin{short}
    Creates a new @class{gtk:flatten-list-model} object that flattens
    @arg{model}.
  @end{short}
  @see-class{gtk:flatten-list-model}
  @see-class{g:list-model}"
  (make-instance 'flatten-list-model
                 :model model))

(export 'flatten-list-model-new)

;;; ----------------------------------------------------------------------------
;;; gtk_flatten_list_model_get_model_for_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flatten_list_model_get_model_for_item"
               flatten-list-model-model-for-item) (g:object g:list-model)
 #+liber-documentation
 "@version{2024-12-9}
  @argument[model]{a @class{gtk:flatten-list-model} object}
  @argument[position]{an unsigned integer with a position}
  @return{The @class{g:list-model} object containing the item at
    @arg{position}.}
  @begin{short}
    Returns the model containing the item at the given @arg{position}.
  @end{short}
  @see-class{gtk:flatten-list-model}
  @see-class{g:list-model}"
  (model (g:object flatten-list-model))
  (position :uint))

(export 'flatten-list-model-model-for-item)

;;; --- End of file gtk4.flatten-list-model.lisp -------------------------------
