;;; ----------------------------------------------------------------------------
;;; gtk4.slice-list-model.lisp
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
;;; GtkSliceListModel
;;;
;;;     A list model that presents a slice out of a larger list
;;;
;;; Types and Values
;;;
;;;     GtkSliceListModel
;;;
;;; Functions
;;;
;;;     gtk_slice_list_model_new
;;;     gtk_slice_list_model_set_model
;;;     gtk_slice_list_model_get_model
;;;     gtk_slice_list_model_set_offset
;;;     gtk_slice_list_model_get_offset
;;;     gtk_slice_list_model_set_size
;;;     gtk_slice_list_model_get_size
;;;
;;; Properties
;;;
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8
;;;     offset
;;;     size
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSliceListModel
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSliceListModel
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkSliceListModel" slice-list-model
  (:superclass g:object
   :export t
   :interfaces ("GListModel")
   :type-initializer "gtk_slice_list_model_get_type")
  (#+gtk-4-8
   (item-type
    slice-list-model-item-type
    "item-type" "GType" t nil)
   (model
    slice-list-model-model
    "model" "GListModel" t t)
   #+gtk-4-8
   (n-items
    slice-list-model-n-items
    "n-items" "guint" t nil)
   (offset
    slice-list-model-offset
    "offset" "guint" t t)
   (size
    slice-list-model-size
    "size" "guint" t t)))



;;;Property Details
;;;The “model” property
;;;  “model”                    GListModel *
;;;Child model to take slice from

;;;Owner: GtkSliceListModel

;;;Flags: Read / Write

;;;The “offset” property
;;;  “offset”                   guint
;;;Offset of slice

;;;Owner: GtkSliceListModel

;;;Flags: Read / Write

;;;Default value: 0

;;;The “size” property
;;;  “size”                     guint
;;;Maximum size of slice

;;;Owner: GtkSliceListModel

;;;Flags: Read / Write

;;;Default value: 10

;;;See Also
;;;GListModel


;;;Description
;;;GtkSliceListModel is a list model that takes a list model and presents a slice of that model.

;;;This is useful when implementing paging by setting the size to the number of elements per page and updating the offset whenever a different page is opened.

;;;Functions
;;;gtk_slice_list_model_new ()
;;;GtkSliceListModel *
;;;gtk_slice_list_model_new (GListModel *model,
;;;                          guint offset,
;;;                          guint size);
;;;Creates a new slice model that presents the slice from offset to offset + size our of the given model .

;;;Parameters
;;;model

;;;The model to use, or NULL.

;;;[transfer full][allow-none]
;;;offset

;;;the offset of the slice

;;;size

;;;maximum size of the slice

;;;Returns
;;;A new GtkSliceListModel

;;;gtk_slice_list_model_set_model ()
;;;void
;;;gtk_slice_list_model_set_model (GtkSliceListModel *self,
;;;                                GListModel *model);
;;;Sets the model to show a slice of. The model's item type must conform to self 's item type.

;;;Parameters
;;;self

;;;a GtkSliceListModel

;;;model

;;;The model to be sliced.

;;;[allow-none]
;;;gtk_slice_list_model_get_model ()
;;;GListModel *
;;;gtk_slice_list_model_get_model (GtkSliceListModel *self);
;;;Gets the model that is currently being used or NULL if none.

;;;Parameters
;;;self

;;;a GtkSliceListModel

;;;Returns
;;;The model in use.

;;;[nullable][transfer none]

;;;gtk_slice_list_model_set_offset ()
;;;void
;;;gtk_slice_list_model_set_offset (GtkSliceListModel *self,
;;;                                 guint offset);
;;;Sets the offset into the original model for this slice.

;;;If the offset is too large for the sliced model, self will end up empty.

;;;Parameters
;;;self

;;;a GtkSliceListModel

;;;offset

;;;the new offset to use

;;;gtk_slice_list_model_get_offset ()
;;;guint
;;;gtk_slice_list_model_get_offset (GtkSliceListModel *self);
;;;Gets the offset set via gtk_slice_list_model_set_offset()

;;;Parameters
;;;self

;;;a GtkSliceListModel

;;;Returns
;;;The offset

;;;gtk_slice_list_model_set_size ()
;;;void
;;;gtk_slice_list_model_set_size (GtkSliceListModel *self,
;;;                               guint size);
;;;Sets the maximum size. self will never have more items than size .

;;;It can however have fewer items if the offset is too large or the model sliced from doesn't have enough items.

;;;Parameters
;;;self

;;;a GtkSliceListModel

;;;size

;;;the maximum size

;;;gtk_slice_list_model_get_size ()
;;;guint
;;;gtk_slice_list_model_get_size (GtkSliceListModel *self);
;;;Gets the size set via gtk_slice_list_model_set_size().

;;;Parameters
;;;self

;;;a GtkSliceListModel

;;;Returns
;;;The size


;;; --- End of file gtk4.slice-list-model.lisp ---------------------------------
