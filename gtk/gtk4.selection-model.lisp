;;; ----------------------------------------------------------------------------
;;; gtk4.selection-model.lisp
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
;;; GtkSelectionModel
;;;
;;;     An extension of the list model interface that handles selections
;;;
;;; Types and Values
;;;
;;;     GtkSelectionModel
;;;
;;; Functions
;;;
;;;     gtk_selection_model_is_selected
;;;     gtk_selection_model_get_selection
;;;     gtk_selection_model_get_selection_in_range
;;;     gtk_selection_model_select_item
;;;     gtk_selection_model_unselect_item
;;;     gtk_selection_model_select_range
;;;     gtk_selection_model_unselect_range
;;;     gtk_selection_model_select_all
;;;     gtk_selection_model_unselect_all
;;;     gtk_selection_model_set_selection
;;;     gtk_selection_model_selection_changed
;;;
;;; Signals
;;;
;;;     selection-changed
;;;
;;; Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkSelectionModel
;;;
;;; Prerequisites
;;;
;;;     GtkSelectionModel requires GListModel and GObject.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSelectionModel
;;; ----------------------------------------------------------------------------

(gobject:define-g-interface "GtkSelectionModel" selection-model
  (:superclass g:list-model
   :export t
   :type-initializer "gtk_selection_model_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'selection-model)
      "Interface"
      (documentation 'selection-model 'type)
 "@version{2023-8-10}
  @begin{short}
    The @class{gtk:selection-model} interface is an interface that extends the
    @class{g:list-model} interface by adding support for selections.
  @end{short}
  This support is then used by widgets using list models to add the ability to
  select and unselect various items.

  GTK provides default implementations of the most common selection modes such
  as the @class{gtk:single-selection} implementation, so you will only need to
  implement this interface if you want detailed control about how selections
  should be handled.

  A @class{gtk:selection-model} object supports a single boolean per item
  indicating if an item is selected or not. This can be queried via the
  @fun{gtk:selection-model-is-selected} function. When the selected state of
  one or more items changes, the model will emit the \"selection-changed\"
  signal by calling the the @fun{gtk:selection-model-selection-changed}
  function. The positions given in that signal may have their selection state
  changed, though that is not a requirement. If new items added to the model
  via the \"items-changed\" signal are selected or not is up to the
  implementation.

  Note that items added via the \"items-changed\" signal may already be selected
  and no \"selection-changed\" signal will be emitted for them. So to track
  which items are selected, it is necessary to listen to both signals.

  Additionally, the interface can expose functionality to select and unselect
  items. If these functions are implemented, list widgets will allow users to
  select and unselect items. However, the @class{gtk:selection-model}
  implementations are free to only implement them partially or not at all. In
  that case the widgets will not support the unimplemented operations.

  When selecting or unselecting is supported by a model, the return values of
  the selection functions do not indicate if selection or unselection happened.
  They are only meant to indicate complete failure, like when this mode of
  selecting is not supported by the model.

  Selections may happen asynchronously, so the only reliable way to find out
  when an item was selected is to listen to the signals that indicate selection.
  @begin[Signal Details]{dictionary}
    @subheading{The \"selection-changed\" signal}
      @begin{pre}
lambda (model position n-items)    :run-last
      @end{pre}
      Emitted when the selection state of some of the items in model changes.
      Note that this signal does not specify the new selection state of the
      items, they need to be queried manually. It is also not necessary for a
      model to change the selection state of any of the items in the selection
      model, though it would be rather useless to emit such a signal.
      @begin[code]{table}
        @entry[model]{A @class{gtk:selection-model} object.}
        @entry[position]{An unsigned integer with the first item that may have
          changed.}
        @entry[n-items]{An unsigned integer with the number of items with
          changes.}
      @end{table}
  @end{dictionary}
  @see-class{g:list-model}
  @see-class{gtk:single-selection}")

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_is_selected ()
;;;
;;; gboolean
;;; gtk_selection_model_is_selected (GtkSelectionModel *model,
;;;                                  guint position);
;;;
;;; Checks if the given item is selected.
;;;
;;; model :
;;;     a GtkSelectionModel
;;;
;;; position :
;;;     the position of the item to query
;;;
;;; Returns :
;;;     TRUE if the item is selected
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_get_selection ()
;;;
;;; GtkBitset *
;;; gtk_selection_model_get_selection (GtkSelectionModel *model);
;;;
;;; Gets the set containing all currently selected items in the model.
;;;
;;; This function may be slow, so if you are only interested in single item,
;;; consider using gtk_selection_model_is_selected() or if you are only
;;; interested in a few consider gtk_selection_model_get_selection_in_range().
;;;
;;; model :
;;;     a GtkSelectionModel
;;;
;;; Returns :
;;;     a GtkBitset containing all the values currently selected in model . If
;;;     no items are selected, the bitset is empty. The bitset must not be
;;;     modified.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_get_selection_in_range ()
;;;
;;; GtkBitset *
;;; gtk_selection_model_get_selection_in_range
;;;                                (GtkSelectionModel *model,
;;;                                 guint position,
;;;                                 guint n_items);
;;;
;;; Gets a set containing a set where the values in the range [position,
;;; position + n_items) match the selected state of the items in that range.
;;; All values outside that range are undefined.
;;;
;;; This function is an optimization for gtk_selection_model_get_selection()
;;; when you are only interested in part of the model's selected state. A
;;; common use case is in response to the “selection-changed” signal.
;;;
;;; model :
;;;     a GtkSelectionModel
;;;
;;; position :
;;;     start of the queired range
;;;
;;; n_items :
;;;     number of items in the queried range
;;;
;;; Returns :
;;;     A GtkBitset that matches the selection state for the given state with
;;;     all other values being undefined. The bitset must not be modified.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_select_item ()
;;;
;;; gboolean
;;; gtk_selection_model_select_item (GtkSelectionModel *model,
;;;                                  guint position,
;;;                                  gboolean unselect_rest);
;;;
;;; Requests to select an item in the model.
;;;
;;; model :
;;;     a GtkSelectionModel
;;;
;;; position :
;;;     the position of the item to select
;;;
;;; unselect_rest :
;;;     whether previously selected items should be unselected
;;;
;;; Returns :
;;;     TRUE if this action was supported and no fallback should be tried. This
;;;     does not mean the item was selected.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_unselect_item ()
;;;
;;; gboolean
;;; gtk_selection_model_unselect_item (GtkSelectionModel *model,
;;;                                    guint position);
;;;
;;; Requests to unselect an item in the model.
;;;
;;; model :
;;;     a GtkSelectionModel
;;;
;;; position :
;;;     the position of the item to unselect
;;;
;;; Returns :
;;;     TRUE if this action was supported and no fallback should be tried. This
;;;     does not mean the item was unselected.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_select_range ()
;;;
;;; gboolean
;;; gtk_selection_model_select_range (GtkSelectionModel *model,
;;;                                   guint position,
;;;                                   guint n_items,
;;;                                   gboolean unselect_rest);
;;;
;;; Requests to select a range of items in the model.
;;;
;;; model :
;;;     a GtkSelectionModel
;;;
;;; position :
;;;     the first item to select
;;;
;;; n_items :
;;;     the number of items to select
;;;
;;; unselect_rest :
;;;     whether previously selected items should be unselected
;;;
;;; Returns :
;;;     TRUE if this action was supported and no fallback should be tried. This
;;;     does not mean the range was selected.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_unselect_range ()
;;;
;;; gboolean
;;; gtk_selection_model_unselect_range (GtkSelectionModel *model,
;;;                                     guint position,
;;;                                     guint n_items);
;;;
;;; Requests to unselect a range of items in the model.
;;;
;;; model :
;;;     a GtkSelectionModel
;;;
;;; position :
;;;     the first item to unselect
;;;
;;; n_items :
;;;     the number of items to unselect
;;;
;;; Returns :
;;;     TRUE if this action was supported and no fallback should be tried. This
;;;     does not mean the range was unselected.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_select_all ()
;;;
;;; gboolean
;;; gtk_selection_model_select_all (GtkSelectionModel *model);
;;;
;;; Requests to select all items in the model.
;;;
;;; model :
;;;     a GtkSelectionModel
;;;
;;; Returns :
;;;     TRUE if this action was supported and no fallback should be tried. This
;;;     does not mean that all items are now selected.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_unselect_all ()
;;;
;;; gboolean
;;; gtk_selection_model_unselect_all (GtkSelectionModel *model);
;;;
;;; Requests to unselect all items in the model.
;;;
;;; model :
;;;     a GtkSelectionModel
;;;
;;; Returns :
;;;     TRUE if this action was supported and no fallback should be tried. This
;;;     does not mean that all items are now unselected.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_set_selection ()
;;;
;;; gboolean
;;; gtk_selection_model_set_selection (GtkSelectionModel *model,
;;;                                    GtkBitset *selected,
;;;                                    GtkBitset *mask);
;;;
;;; This is the most advanced selection updating method that allows the most
;;; fine-grained control over selection changes. If you can, you should try the
;;; simpler versions, as implementations are more likely to implement support
;;; for those.
;;;
;;; Requests that the selection state of all positions set in mask be updated
;;; to the respective value in the selected bitmask.
;;;
;;; In pseudocode, it would look something like this:
;;;
;;; for (i = 0; i < n_items; i++)
;;;   {
;;;     // don't change values not in the mask
;;;     if (!gtk_bitset_contains (mask, i))
;;;       continue;
;;;
;;;     if (gtk_bitset_contains (selected, i))
;;;       select_item (i);
;;;     else
;;;       unselect_item (i);
;;;   }
;;;
;;; gtk_selection_model_selection_changed (model, first_changed_item,
;;;                                               n_changed_items);
;;;
;;; ask and selected must not be modified. They may refer to the same bitset,
;;; which would mean that every item in the set should be selected.
;;;
;;; model :
;;;     a GtkSelectionModel
;;;
;;; selected :
;;;     bitmask specifying if items should be selected or unselected
;;;
;;; mask :
;;;     bitmask specifying which items should be updated
;;;
;;; Returns :
;;;     TRUE if this action was supported and no fallback should be tried. This
;;;     does not mean that all items were updated according to the inputs.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_selection_changed ()
;;;
;;; void
;;; gtk_selection_model_selection_changed (GtkSelectionModel *model,
;;;                                        guint position,
;;;                                        guint n_items);
;;;
;;; Helper function for implementations of GtkSelectionModel. Call this when a
;;; the selection changes to emit the “selection-changed” signal.
;;;
;;; model :
;;;     a GtkSelectionModel
;;;
;;; position :
;;;     the first changed item
;;;
;;; n_items :
;;;     the number of changed items
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.selection-model.lisp ----------------------------------
