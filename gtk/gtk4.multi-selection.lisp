;;; ----------------------------------------------------------------------------
;;; gtk4.multi-selection.lisp
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
;;; GtkMultiSelection
;;;
;;;     A selection model that allows selecting multiple items
;;;
;;; Types and Values
;;;
;;;     GtkMultiSelection
;;;
;;; Functions
;;;
;;;     gtk_multi_selection_new
;;;     gtk_multi_selection_get_model
;;;     gtk_multi_selection_set_model
;;;
;;; Properties
;;;
;;;     model
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkMultiSelection
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;;     GtkSelectionModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkMultiSelection
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkMultiSelection" multi-selection
  (:superclass g:object
   :export t
   :interfaces ("GtkSelectionModel")
   :type-initializer "gtk_multi_selection_get_type")
  (#+gtk-4-8
   (item-type
    multi-selection-item-type
    "item-type" "GType" t nil)
   (model
    multi-selection-model
    "model" "GListModel" t t)
   #+gtk-4-8
   (n-items
    multi-selection-n-items
    "n-items" "guint" t nil)))




;;;Property Details
;;;The “model” property
;;;  “model”                    GListModel *
;;;The list managed by this selection

;;;Owner: GtkMultiSelection

;;;Flags: Read / Write

;;;See Also
;;;GtkSelectionModel


;;;Includes
;;;#include <gtk/gtk.h>
;;;Description
;;;GtkMultiSelection is an implementation of the GtkSelectionModel interface that allows selecting multiple elements.

;;;Functions
;;;gtk_multi_selection_new ()
;;;GtkMultiSelection *
;;;gtk_multi_selection_new (GListModel *model);
;;;Creates a new selection to handle model .

;;;Parameters
;;;model

;;;the GListModel to manage, or NULL.

;;;[allow-none][transfer full]
;;;Returns
;;;a new GtkMultiSelection.

;;;[transfer full]

;;;gtk_multi_selection_get_model ()
;;;GListModel *
;;;gtk_multi_selection_get_model (GtkMultiSelection *self);
;;;Returns the underlying model of self .

;;;Parameters
;;;self

;;;a GtkMultiSelection

;;;Returns
;;;the underlying model.

;;;[transfer none]

;;;gtk_multi_selection_set_model ()
;;;void
;;;gtk_multi_selection_set_model (GtkMultiSelection *self,
;;;                               GListModel *model);
;;;Sets the model that self should wrap. If model is NULL, self will be empty.

;;;Parameters
;;;self

;;;a GtkMultiSelection

;;;model

;;;A GListModel to wrap.

;;;[allow-none]

;;; --- End of file gtk4.multi-selection.lisp ----------------------------------
