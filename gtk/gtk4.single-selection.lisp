;;; ----------------------------------------------------------------------------
;;; gtk4.single-selection.lisp
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
;;; GtkSingleSelection
;;;
;;;     A selection model that allows selecting a single item
;;;
;;; Types and Values
;;;
;;;     GtkSingleSelection
;;;     GTK_INVALID_LIST_POSITION
;;;
;;; Functions
;;;
;;;     gtk_single_selection_new
;;;     gtk_single_selection_get_model
;;;     gtk_single_selection_set_model
;;;     gtk_single_selection_get_selected
;;;     gtk_single_selection_set_selected
;;;     gtk_single_selection_get_selected_item
;;;     gtk_single_selection_get_autoselect
;;;     gtk_single_selection_set_autoselect
;;;     gtk_single_selection_get_can_unselect
;;;     gtk_single_selection_set_can_unselect
;;;
;;; Properties
;;;
;;;     autoselect
;;;     can-unselect
;;;     model
;;;     selected
;;;     selected-item
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSingleSelection
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;;     GtkSelectionModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_INVALID_LIST_POSITION
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+gtk-invalid-list-position+)
      "Constant")

(defconstant +gtk-invalid-list-position+ 4294967295
 #+liber-documentation
 "@version{2023-8-13}
  @variable-value{4294967295}
  @begin{short}
    The value used to refer to a guaranteed invalid position in a 
    @class{g:list-model} object.
  @end{short}
  This value may be returned from some functions, others may accept it as input.
  Its interpretation may differ for different functions.

  Refer to each function's documentation for if this value is allowed and what 
  it does.
  @see-class{g:list-model}")

(export '+gtk-invalid-list-position+)

;;; ----------------------------------------------------------------------------
;;; GtkSingleSelection
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkSingleSelection" single-selection
  (:superclass g:object
   :export t
   :interfaces ("GtkSelectionModel")
   :type-initializer "gtk_single_selection_get_type")
  ((autoselect
    single-selection-autoselect
    "autoselect" "gboolean" t t)
   (can-unselect
    single-selection-can-unselect
    "can-unselect" "gboolean" t t)
   #+gtk-4-8
   (item-type
    single-selection-item-type
    "item-type" "GType" t nil)
   (model
    single-selection-model
    "model" "GListModel" t t)
   #+gtk-4-8
   (n-items
    single-selection-n-items
    "n-items" "guint" t nil)
   (selected
    single-selection-selected
    "selected" "guint" t t)
   (selected-item
    single-selection-selected-item
    "selected-item" "GObject" t nil)))


;;;Description
;;;GtkSingleSelection is an implementation of the GtkSelectionModel interface that allows selecting a single element. It is the default selection method used by list widgets in GTK.

;;;Note that the selection is *persistent* -- if the selected item is removed and re-added in the same “items-changed” emission, it stays selected. In particular, this means that changing the sort order of an underlying sort model will preserve the selection.



;;;Property Details

;;;The “autoselect” property
;;;  “autoselect”               gboolean
;;;If the selection will always select an item

;;;Owner: GtkSingleSelection

;;;Flags: Read / Write

;;;Default value: TRUE

;;;The “can-unselect” property
;;;  “can-unselect”             gboolean
;;;If unselecting the selected item is allowed

;;;Owner: GtkSingleSelection

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “model” property
;;;  “model”                    GListModel *
;;;The model being managed

;;;Owner: GtkSingleSelection

;;;Flags: Read / Write

;;;The “selected” property
;;;  “selected”                 guint
;;;Position of the selected item

;;;Owner: GtkSingleSelection

;;;Flags: Read / Write

;;;Default value: 4294967295

;;;The “selected-item” property
;;;  “selected-item”            GObject *
;;;The selected item

;;;Owner: GtkSingleSelection

;;;Flags: Read

;;;See Also
;;;GtkSelectionModel


;;; ----------------------------------------------------------------------------
;;; gtk_single_selection_new ()
;;;
;;; GtkSingleSelection *
;;; gtk_single_selection_new (GListModel *model);
;;;
;;; Creates a new selection to handle model .
;;;
;;; model :
;;;     the GListModel to manage, or NULL.
;;;
;;; Returns :
;;;     a new GtkSingleSelection.
;;; ----------------------------------------------------------------------------

(declaim (inline single-selection-new))

(defun single-selection-new (model)
  (make-instance 'single-selection
                 :model model))

(export 'single-selection-new)

;;; ----------------------------------------------------------------------------
;;; gtk_single_selection_get_model ()
;;;
;;; GListModel *
;;; gtk_single_selection_get_model (GtkSingleSelection *self);
;;;
;;; Gets the model that self is wrapping.

;;;Parameters
;;;self

;;;a GtkSingleSelection

;;;Returns
;;;The model being wrapped.

;;;[transfer none]

;;;gtk_single_selection_set_model ()
;;;void
;;;gtk_single_selection_set_model (GtkSingleSelection *self,
;;;                                GListModel *model);
;;;Sets the model that self should wrap. If model is NULL, self will be empty.

;;;Parameters
;;;self

;;;a GtkSingleSelection

;;;model

;;;A GListModel to wrap.

;;;[allow-none]
;;;gtk_single_selection_get_selected ()
;;;guint
;;;gtk_single_selection_get_selected (GtkSingleSelection *self);
;;;Gets the position of the selected item. If no item is selected, GTK_INVALID_LIST_POSITION is returned.

;;;Parameters
;;;self

;;;a GtkSingleSelection

;;;Returns
;;;The position of the selected item

;;;gtk_single_selection_set_selected ()
;;;void
;;;gtk_single_selection_set_selected (GtkSingleSelection *self,
;;;                                   guint position);
;;;Selects the item at the given position.

;;;If the list does not have an item at position or GTK_INVALID_LIST_POSITION is given, the behavior depends on the value of the “autoselect” property: If it is set, no change will occur and the old item will stay selected. If it is unset, the selection will be unset and no item will be selected.

;;;Parameters
;;;self

;;;a GtkSingleSelection

;;;position

;;;the item to select or GTK_INVALID_LIST_POSITION

;;;gtk_single_selection_get_selected_item ()
;;;gpointer
;;;gtk_single_selection_get_selected_item
;;;                               (GtkSingleSelection *self);
;;;Gets the selected item.

;;;If no item is selected, NULL is returned.

;;;Parameters
;;;self

;;;a GtkSingleSelection

;;;Returns
;;;The selected item.

;;;[transfer none]

;;;gtk_single_selection_get_autoselect ()
;;;gboolean
;;;gtk_single_selection_get_autoselect (GtkSingleSelection *self);
;;;Checks if autoselect has been enabled or disabled via gtk_single_selection_set_autoselect().

;;;Parameters
;;;self

;;;a GtkSingleSelection

;;;Returns
;;;TRUE if autoselect is enabled

;;;gtk_single_selection_set_autoselect ()
;;;void
;;;gtk_single_selection_set_autoselect (GtkSingleSelection *self,
;;;                                     gboolean autoselect);
;;;If autoselect is TRUE, self will enforce that an item is always selected. It will select a new item when the currently selected item is deleted and it will disallow unselecting the current item.

;;;Parameters
;;;self

;;;a GtkSingleSelection

;;;autoselect

;;;TRUE to always select an item

;;;gtk_single_selection_get_can_unselect ()
;;;gboolean
;;;gtk_single_selection_get_can_unselect (GtkSingleSelection *self);
;;;If TRUE, gtk_selection_model_unselect_item() is supported and allows unselecting the selected item.

;;;Parameters
;;;self

;;;a GtkSingleSelection

;;;Returns
;;;TRUE to support unselecting

;;;gtk_single_selection_set_can_unselect ()
;;;void
;;;gtk_single_selection_set_can_unselect (GtkSingleSelection *self,
;;;                                       gboolean can_unselect);
;;;If TRUE, unselecting the current item via gtk_selection_model_unselect_item() is supported.

;;;Note that setting “autoselect” will cause the unselecting to not work, so it practically makes no sense to set both at the same time the same time.

;;;Parameters
;;;self

;;;a GtkSingleSelection

;;;can_unselect

;;;TRUE to allow unselecting


;;; --- End of file gtk4.single-selection.lisp ---------------------------------
