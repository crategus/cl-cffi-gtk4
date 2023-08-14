;;; ----------------------------------------------------------------------------
;;; gtk4.filter-list-model.lisp
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
;;; GtkFilterListModel
;;;
;;;     A list model that filters its items
;;;
;;; Types and Values
;;;
;;;     GtkFilterListModel
;;;
;;; Functions
;;;
;;;     gtk_filter_list_model_new
;;;     gtk_filter_list_model_set_model
;;;     gtk_filter_list_model_get_model
;;;     gtk_filter_list_model_set_filter
;;;     gtk_filter_list_model_get_filter
;;;     gtk_filter_list_model_set_incremental
;;;     gtk_filter_list_model_get_incremental
;;;     gtk_filter_list_model_get_pending
;;;
;;; Properties
;;;
;;;     filter
;;;     incremental
;;;     model
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

(gobject:define-g-object-class "GtkFilterListModel" filter-list-model
  (:superclass g:object
   :export t
   :interfaces ("GListModel")
   :type-initializer "gtk_filter_list_model_get_type")
  ((filter
    filter-list-model-filter
    "filter" "GtkFilter" t t)
   (incremental
    filter-list-model-incremental
    "incremental" "gboolean" t t)
   #+gtk-4-8
   (item-type
    filter-list-model-item-type
    "item-type" "GType" t nil)
   (model
    filter-list-model-model
    "model" "GListModel" t t)
   #+gkt-4-8
   (n-items
    filter-list-model-n-items
    "n-items" "guint" t nil)
   (pending
    filter-list-model-pending
    "pending" "guint" t nil)))


;;;Description
;;;GtkFilterListModel is a list model that filters a given other listmodel. It hides some elements from the other model according to criteria given by a GtkFilter.

;;;The model can be set up to do incremental searching, so that filtering long lists doesn't block the UI. See gtk_filter_list_model_set_incremental() for details.

;;;See Also
;;;GListModel, GtkFilter

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;;The “filter” property
;;;  “filter”                   GtkFilter *
;;;The filter for this model

;;;Owner: GtkFilterListModel

;;;Flags: Read / Write

;;;The “incremental” property
;;;  “incremental”              gboolean
;;;If the model should filter items incrementally

;;;Owner: GtkFilterListModel

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “model” property
;;;  “model”                    GListModel *
;;;The model being filtered

;;;Owner: GtkFilterListModel

;;;Flags: Read / Write

;;;The “pending” property
;;;  “pending”                  guint
;;;Number of items not yet filtered

;;;Owner: GtkFilterListModel

;;;Flags: Read

;;;Default value: 0




;;;Functions

;;;gtk_filter_list_model_new ()
;;;GtkFilterListModel *
;;;gtk_filter_list_model_new (GListModel *model,
;;;                           GtkFilter *filter);
;;;Creates a new GtkFilterListModel that will filter model using the given filter .

;;;Parameters
;;;model

;;;the model to sort, or NULL.

;;;[allow-none][transfer full]
;;;filter

;;;filter or NULL to not filter items.

;;;[allow-none][transfer full]
;;;Returns
;;;a new GtkFilterListModel

;;;gtk_filter_list_model_set_model ()
;;;void
;;;gtk_filter_list_model_set_model (GtkFilterListModel *self,
;;;                                 GListModel *model);
;;;Sets the model to be filtered.

;;;Note that GTK makes no effort to ensure that model conforms to the item type of self . It assumes that the caller knows what they are doing and have set up an appropriate filter to ensure that item types match.

;;;Parameters
;;;self

;;;a GtkFilterListModel

;;;model

;;;The model to be filtered.

;;;[allow-none]
;;;gtk_filter_list_model_get_model ()
;;;GListModel *
;;;gtk_filter_list_model_get_model (GtkFilterListModel *self);
;;;Gets the model currently filtered or NULL if none.

;;;Parameters
;;;self

;;;a GtkFilterListModel

;;;Returns
;;;The model that gets filtered.

;;;[nullable][transfer none]

;;;gtk_filter_list_model_set_filter ()
;;;void
;;;gtk_filter_list_model_set_filter (GtkFilterListModel *self,
;;;                                  GtkFilter *filter);
;;;Sets the filter used to filter items.

;;;Parameters
;;;self

;;;a GtkFilterListModel

;;;filter

;;;filter to use or NULL to not filter items.

;;;[allow-none][transfer none]
;;;gtk_filter_list_model_get_filter ()
;;;GtkFilter *
;;;gtk_filter_list_model_get_filter (GtkFilterListModel *self);
;;;Gets the GtkFilter currently set on self .

;;;Parameters
;;;self

;;;a GtkFilterListModel

;;;Returns
;;;The filter currently in use or NULL if the list isn't filtered.

;;;[nullable][transfer none]

;;;gtk_filter_list_model_set_incremental ()
;;;void
;;;gtk_filter_list_model_set_incremental (GtkFilterListModel *self,
;;;                                       gboolean incremental);
;;;When incremental filtering is enabled, the GtkFilterListModel will not run filters immediately, but will instead queue an idle handler that incrementally filters the items and adds them to the list. This of course means that items are not instantly added to the list, but only appear incrementally.

;;;When your filter blocks the UI while filtering, you might consider turning this on. Depending on your model and filters, this may become interesting around 10,000 to 100,000 items.

;;;By default, incremental filtering is disabled.

;;;See gtk_filter_list_model_get_pending() for progress information about an ongoing incremental filtering operation.

;;;Parameters
;;;self

;;;a GtkFilterListModel

;;;incremental

;;;TRUE to enable incremental filtering

;;;gtk_filter_list_model_get_incremental ()
;;;gboolean
;;;gtk_filter_list_model_get_incremental (GtkFilterListModel *self);
;;;Returns whether incremental filtering was enabled via gtk_filter_list_model_set_incremental().

;;;Parameters
;;;self

;;;a GtkFilterListModel

;;;Returns
;;;TRUE if incremental filtering is enabled

;;;gtk_filter_list_model_get_pending ()
;;;guint
;;;gtk_filter_list_model_get_pending (GtkFilterListModel *self);
;;;Returns the number of items that have not been filtered yet.

;;;You can use this value to check if self is busy filtering by comparing the return value to 0 or you can compute the percentage of the filter remaining by dividing the return value by the total number of items in the underlying model:

;;;pending = gtk_filter_list_model_get_pending (self);
;;;model = gtk_filter_list_model_get_model (self);
;;;percentage = pending / (double) g_list_model_get_n_items (model);
;;;If no filter operation is ongoing - in particular when “incremental” is FALSE - this function returns 0.

;;;Parameters
;;;self

;;;a GtkFilterListModel

;;;Returns
;;;The number of items not yet filtered


;;; --- End of file gtk4.filter-list-model -------------------------------------
