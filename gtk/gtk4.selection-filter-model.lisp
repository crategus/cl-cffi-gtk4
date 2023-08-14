;;; ----------------------------------------------------------------------------
;;; gtk4.selection-filter-model.lisp
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
;;; GtkSelectionFilterModel
;;;
;;;     A list model that turns a selection in a model
;;;
;;; Types and Values
;;;
;;;     GtkSelectionFilterModel
;;;
;;; Functions
;;;
;;;     gtk_selection_filter_model_new
;;;     gtk_selection_filter_model_set_model
;;;     gtk_selection_filter_model_get_model
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

(gobject:define-g-object-class "GtkSelectionFilterModel" selection-filter-model
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



;;;Property Details
;;;The “model” property
;;;  “model”                    GtkSelectionModel *
;;;The model being filtered

;;;Owner: GtkSelectionFilterModel

;;;Flags: Read / Write

;;;See Also
;;;GtkSelectionModel



;;;Description
;;;GtkSelectionFilterModel is a list model that presents the selected items in a GtkSelectionModel as its own list model.

;;;Functions
;;;gtk_selection_filter_model_new ()
;;;GtkSelectionFilterModel *
;;;gtk_selection_filter_model_new (GtkSelectionModel *model);
;;;Creates a new GtkSelectionFilterModel that will include the selected items from the underlying selection model.

;;;Parameters
;;;model

;;;the selection model to filter, or NULL.

;;;[allow-none][transfer none]
;;;Returns
;;;a new GtkSelectionFilterModel

;;;gtk_selection_filter_model_set_model ()
;;;void
;;;gtk_selection_filter_model_set_model (GtkSelectionFilterModel *self,
;;;                                      GtkSelectionModel *model);
;;;Sets the model to be filtered.

;;;Note that GTK makes no effort to ensure that model conforms to the item type of self . It assumes that the caller knows what they are doing and have set up an appropriate filter to ensure that item types match.

;;;Parameters
;;;self

;;;a GtkSelectionFilterModel

;;;model

;;;The model to be filtered.

;;;[allow-none]
;;;gtk_selection_filter_model_get_model ()
;;;GtkSelectionModel *
;;;gtk_selection_filter_model_get_model (GtkSelectionFilterModel *self);
;;;Gets the model currently filtered or NULL if none.

;;;Parameters
;;;self

;;;a GtkSelectionFilterModel

;;;Returns
;;;The model that gets filtered.

;;;[nullable][transfer none]


;;; --- End of file gtk4.selection-filter-model.lisp ---------------------------
