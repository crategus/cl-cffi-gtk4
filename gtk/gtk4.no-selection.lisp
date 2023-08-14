;;; ----------------------------------------------------------------------------
;;; gtk4.no-selection.lisp
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
;;; GtkNoSelection
;;;
;;;     A selection model that does not allow selecting anything
;;;
;;; Types and Values
;;;
;;;     GtkNoSelection
;;;
;;; Functions
;;;
;;;     gtk_no_selection_new
;;;     gtk_no_selection_get_model
;;;     gtk_no_selection_set_model
;;;
;;; Properties
;;;
;;;     model
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkNoSelection
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;;     GtkSelectionModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkNoSelection
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkNoSelection" no-selection
  (:superclass g:object
   :export t
   :interfaces ("GtkSelectionModel")
   :type-initializer "gtk_no_selection_get_type")
  (#+gtk-4-8
   (item-type
    no-selection-item-type
    "item-type" "GType" t nil)
   (model
    no-selection-model
    "model" "GListModel" t t)
   #+gtk-4-8
   (n-items
    no-selection-n-items
    "n-items" "guint" t nil)))

;;;GtkNoSelection is an implementation of the GtkSelectionModel interface that does not allow selecting anything.

;;;This model is meant to be used as a simple wrapper to GListModels when a GtkSelectionModel is required.



;;;typedef struct _GtkNoSelection GtkNoSelection;
;;;Property Details
;;;The “model” property
;;;  “model”                    GListModel *
;;;The model being managed

;;;Owner: GtkNoSelection

;;;Flags: Read / Write

;;;See Also
;;;GtkSelectionModel



;;; ----------------------------------------------------------------------------
;;; gtk_no_selection_new ()
;;;
;;; GtkNoSelection *
;;; gtk_no_selection_new (GListModel *model);
;;;
;;; Creates a new selection to handle model .
;;;
;;; model :
;;;     the GListModel to manage, or NULL.
;;;
;;; Returns :
;;;     a new GtkNoSelection.
;;; ----------------------------------------------------------------------------

(declaim (inline no-selection-new))

(defun no-selection-new (model)
  (make-instance 'no-selection
                 :model model))

(export 'no-selection-new)

;;; ----------------------------------------------------------------------------
;;;gtk_no_selection_get_model ()
;;;GListModel *
;;;gtk_no_selection_get_model (GtkNoSelection *self);
;;;Gets the model that self is wrapping.

;;;Parameters
;;;self

;;;a GtkNoSelection

;;;Returns
;;;The model being wrapped.

;;;[transfer none]

;;;gtk_no_selection_set_model ()
;;;void
;;;gtk_no_selection_set_model (GtkNoSelection *self,
;;;                            GListModel *model);
;;;Sets the model that self should wrap. If model is NULL, this model will be empty.

;;;Parameters
;;;self

;;;a GtkNoSelection

;;;model

;;;A GListModel to wrap.

;;; --- End of file gtk4.no-selection.lisp -------------------------------------
