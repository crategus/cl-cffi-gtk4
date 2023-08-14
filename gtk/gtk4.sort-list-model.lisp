;;; ----------------------------------------------------------------------------
;;; gtk4.sort-list-model.lisp
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
;;;     gtk_sort_list_model_set_sorter
;;;     gtk_sort_list_model_get_sorter
;;;     gtk_sort_list_model_set_model
;;;     gtk_sort_list_model_get_model
;;;     gtk_sort_list_model_set_incremental
;;;     gtk_sort_list_model_get_incremental
;;;     gtk_sort_list_model_get_pending
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSortListModel
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkSortListModel" sort-list-model
  (:superclass g:object
   :export t
   :interfaces ("GListModel")
   :type-initializer "gtk_sort_list_model_get_type")
  ((incremental
    sort-list-model-incremental
    "incremental" "gboolean" t t)
   #+gtk-4-8
   (item-type
    sort-list-model-item-type
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


;;;Property Details
;;;The “incremental” property
;;;  “incremental”              gboolean
;;;If the model should sort items incrementally

;;;Owner: GtkSortListModel

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “model” property
;;;  “model”                    GListModel *
;;;The model being sorted

;;;Owner: GtkSortListModel

;;;Flags: Read / Write

;;;The “pending” property
;;;  “pending”                  guint
;;;Estimate of unsorted items remaining

;;;Owner: GtkSortListModel

;;;Flags: Read

;;;Default value: 0

;;;The “sorter” property
;;;  “sorter”                   GtkSorter *
;;;The sorter for this model

;;;Owner: GtkSortListModel

;;;Flags: Read / Write

;;;See Also
;;;GListModel, GtkSorter


;;;Description
;;;GtkSortListModel is a list model that takes a list model and sorts its elements according to a GtkSorter.

;;;The model can be set up to do incremental sorting, so that sorting long lists doesn't block the UI. See gtk_sort_list_model_set_incremental() for details.

;;;GtkSortListModel is a generic model and because of that it cannot take advantage of any external knowledge when sorting. If you run into performance issues with GtkSortListModel, it is strongly recommended that you write your own sorting list model.

;;;Functions
;;;gtk_sort_list_model_new ()
;;;GtkSortListModel *
;;;gtk_sort_list_model_new (GListModel *model,
;;;                         GtkSorter *sorter);
;;;Creates a new sort list model that uses the sorter to sort model .

;;;Parameters
;;;model

;;;the model to sort, or NULL.

;;;[allow-none][transfer full]
;;;sorter

;;;the GtkSorter to sort model with, or NULL.

;;;[allow-none][transfer full]
;;;Returns
;;;a new GtkSortListModel

;;;gtk_sort_list_model_set_sorter ()
;;;void
;;;gtk_sort_list_model_set_sorter (GtkSortListModel *self,
;;;                                GtkSorter *sorter);
;;;Sets a new sorter on self .

;;;Parameters
;;;self

;;;a GtkSortListModel

;;;sorter

;;;the GtkSorter to sort model with.

;;;[allow-none]
;;;gtk_sort_list_model_get_sorter ()
;;;GtkSorter *
;;;gtk_sort_list_model_get_sorter (GtkSortListModel *self);
;;;Gets the sorter that is used to sort self .

;;;Parameters
;;;self

;;;a GtkSortListModel

;;;Returns
;;;the sorter of self.

;;;[nullable][transfer none]

;;;gtk_sort_list_model_set_model ()
;;;void
;;;gtk_sort_list_model_set_model (GtkSortListModel *self,
;;;                               GListModel *model);
;;;Sets the model to be sorted. The model 's item type must conform to the item type of self .

;;;Parameters
;;;self

;;;a GtkSortListModel

;;;model

;;;The model to be sorted.

;;;[allow-none]
;;;gtk_sort_list_model_get_model ()
;;;GListModel *
;;;gtk_sort_list_model_get_model (GtkSortListModel *self);
;;;Gets the model currently sorted or NULL if none.

;;;Parameters
;;;self

;;;a GtkSortListModel

;;;Returns
;;;The model that gets sorted.

;;;[nullable][transfer none]

;;;gtk_sort_list_model_set_incremental ()
;;;void
;;;gtk_sort_list_model_set_incremental (GtkSortListModel *self,
;;;                                     gboolean incremental);
;;;Sets the sort model to do an incremental sort.

;;;When incremental sorting is enabled, the sortlistmodel will not do a complete sort immediately, but will instead queue an idle handler that incrementally sorts the items towards their correct position. This of course means that items do not instantly appear in the right place. It also means that the total sorting time is a lot slower.

;;;When your filter blocks the UI while sorting, you might consider turning this on. Depending on your model and sorters, this may become interesting around 10,000 to 100,000 items.

;;;By default, incremental sorting is disabled.

;;;See gtk_sort_list_model_get_pending() for progress information about an ongoing incremental sorting operation.

;;;Parameters
;;;self

;;;a GtkSortListModel

;;;incremental

;;;TRUE to sort incrementally

;;;gtk_sort_list_model_get_incremental ()
;;;gboolean
;;;gtk_sort_list_model_get_incremental (GtkSortListModel *self);
;;;Returns whether incremental sorting was enabled via gtk_sort_list_model_set_incremental().

;;;Parameters
;;;self

;;;a GtkSortListModel

;;;Returns
;;;TRUE if incremental sorting is enabled

;;;gtk_sort_list_model_get_pending ()
;;;guint
;;;gtk_sort_list_model_get_pending (GtkSortListModel *self);
;;;Estimates progress of an ongoing sorting operation

;;;The estimate is the number of items that would still need to be sorted to finish the sorting operation if this was a linear algorithm. So this number is not related to how many items are already correctly sorted.

;;;If you want to estimate the progress, you can use code like this:

;;;pending = gtk_sort_list_model_get_pending (self);
;;;model = gtk_sort_list_model_get_model (self);
;;;progress = 1.0 - pending / (double) MAX (1, g_list_model_get_n_items (model));
;;;If no sort operation is ongoing - in particular when “incremental” is FALSE - this function returns 0.

;;;Parameters
;;;self

;;;a GtkSortListModel

;;;Returns
;;;a progress estimate of remaining items to sort


;;; --- End of file gtk4.sort-list-model.lisp ----------------------------------
