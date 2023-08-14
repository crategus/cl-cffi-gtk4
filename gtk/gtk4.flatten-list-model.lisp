;;; ----------------------------------------------------------------------------
;;; gtk4.flatten-list-model.lisp
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
;;; GtkFlattenListModel
;;;
;;;     A list model that flattens a list of lists
;;;
;;; Types and Values
;;;
;;;     GtkFlattenListModel
;;;
;;; Functions
;;;
;;;     gtk_flatten_list_model_new
;;;     gtk_flatten_list_model_set_model
;;;     gtk_flatten_list_model_get_model
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

(gobject:define-g-object-class "GtkFlattenListModel" flatten-list-model
  (:superclass g:object
   :export t
   :interfaces ("GListModel")
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


;;;Property Details
;;;The “model” property
;;;  “model”                    GListModel *
;;;The model being flattened

;;;Owner: GtkFlattenListModel

;;;Flags: Read / Write

;;;See Also
;;;GListModel



;;;Description
;;;GtkFlattenListModel is a list model that takes a list model containing list models and flattens it into a single model.

;;;Another term for this is concatenation: GtkFlattenListModel takes a list of lists and concatenates them into a single list.

;;;Functions
;;;gtk_flatten_list_model_new ()
;;;GtkFlattenListModel *
;;;gtk_flatten_list_model_new (GListModel *model);
;;;Creates a new GtkFlattenListModel that flattens list .

;;;Parameters
;;;model

;;;the model to be flattened.

;;;[nullable][transfer full]
;;;Returns
;;;a new GtkFlattenListModel

;;;gtk_flatten_list_model_set_model ()
;;;void
;;;gtk_flatten_list_model_set_model (GtkFlattenListModel *self,
;;;                                  GListModel *model);
;;;Sets a new model to be flattened.

;;;Parameters
;;;self

;;;a GtkFlattenListModel

;;;model

;;;the new model or NULL.

;;;[nullable][transfer none]
;;;gtk_flatten_list_model_get_model ()
;;;GListModel *
;;;gtk_flatten_list_model_get_model (GtkFlattenListModel *self);
;;;Gets the model set via gtk_flatten_list_model_set_model().

;;;Parameters
;;;self

;;;a GtkFlattenListModel

;;;Returns
;;;The model flattened by self .

;;;[nullable][transfer none]

;;;gtk_flatten_list_model_get_model_for_item ()
;;;GListModel *
;;;gtk_flatten_list_model_get_model_for_item
;;;                               (GtkFlattenListModel *self,
;;;                                guint position);
;;;Returns the model containing the item at the given position.

;;;Parameters
;;;self

;;;a GtkFlattenListModel

;;;position

;;;a position

;;;Returns
;;;the model containing the item at position .

;;;[transfer none]


;;; --- End of file gtk4.flatten-list-model.lisp -------------------------------
