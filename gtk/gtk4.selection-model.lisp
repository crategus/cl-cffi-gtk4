;;; ----------------------------------------------------------------------------
;;; gtk4.selection-model.lisp
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
;;;     gtk_selection_model_set_selection
;;;     gtk_selection_model_get_selection_in_range
;;;     gtk_selection_model_select_item
;;;     gtk_selection_model_unselect_item
;;;     gtk_selection_model_select_range
;;;     gtk_selection_model_unselect_range
;;;     gtk_selection_model_select_all
;;;     gtk_selection_model_unselect_all
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

(gobject:define-ginterface "GtkSelectionModel" selection-model
  (:superclass g:list-model
   :export t
   :type-initializer "gtk_selection_model_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'selection-model)
      "Interface"
      (documentation 'selection-model 'type)
 "@version{2024-12-2}
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
  one or more items changes, the model will emit the
  @code{\"selection-changed\"} signal by calling the
  @fun{gtk:selection-model-selection-changed} function. The positions given in
  that signal may have their selection state changed, though that is not a
  requirement. If new items added to the model via the @code{\"items-changed\"}
  signal are selected or not is up to the implementation.

  Note that items added via the @code{\"items-changed\"} signal may already be
  selected and no @code{\"selection-changed\"} signal will be emitted for them.
  So to track which items are selected, it is necessary to listen to both
  signals.

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
      @begin[code]{table}
        @entry[model]{The @class{gtk:selection-model} object.}
        @entry[position]{The unsigned integer with the first item that may have
          changed.}
        @entry[n-items]{The unsigned integer with the number of items with
          changes.}
      @end{table}
      Emitted when the selection state of some of the items in model changes.
      Note that this signal does not specify the new selection state of the
      items, they need to be queried manually. It is also not necessary for a
      model to change the selection state of any of the items in the selection
      model, though it would be rather useless to emit such a signal.
  @end{dictionary}
  @see-class{g:list-model}
  @see-class{gtk:single-selection}")

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_is_selected
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_selection_model_is_selected" selection-model-is-selected)
    :boolean
 #+liber-documentation
 "@version{2024-12-2}
  @argument[model]{a @class{gtk:selection-model} object}
  @argument[position]{an unsigned integer with the position of the item to
    query}
  @return{@em{True} if the item is selected.}
  @begin{short}
    Checks if the given item is selected.
  @end{short}
  @see-class{gtk:selection-model}"
  (model (g:object selection-model))
  (position :uint))

(export 'selection-model-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_get_selection
;;; gtk_selection_model_set_selection
;;; ----------------------------------------------------------------------------

(defun (setf selection-model-selection) (selected model mask)
  (cffi:foreign-funcall "gtk_selection_model_set_selection"
                        (g:object selection-model) model
                        (g:boxed bitset) selected
                        (g:boxed bitset) mask
                        :boolean)
  selected)

(cffi:defcfun ("gtk_selection_model_get_selection" selection-model-selection)
    (g:boxed bitset :return)
 #+liber-documentation
 "@version{2024-12-2}
  @syntax{(gtk:selection-model-selection model) => selected}
  @syntax{(setf (gtk:selection-model-selection model mask) selected)}
  @argument[model]{a @class{gtk:selection-model} object}
  @argument[selected]{a @class{gtk:bitset} instance specifying the items which
    are selected or should be selected or unselected}
  @argument[mask]{a @class{gtk:bitset} instance specifying which items should
    be updated}
  @begin{short}
    The @fun{gtk:selection-model-selection} function gets the bitset containing
    all currently selected items in the model.
  @end{short}
  This function may be slow, so if you are only interested in single item,
  consider using the @fun{gtk:selection-model-is-selected} function or if you
  are only interested in a few consider the
  @fun{gtk:selection-model-selection-in-range} function.

  The @setf{gtk:selection-model-selection} function is the most advanced
  selection updating method that allows the most fine-grained control over
  selection changes. If you can, you should try the simpler versions, as
  implementations are more likely to implement support for those.

  Requests that the selection state of all positions set in @arg{mask} be
  updated to the respective value in the selected bitmask. In pseudocode, it
  would look something like this:
  @begin{pre}
for (i = 0; i < n_items; i++)
  {
    // don't change values not in the mask
    if (!gtk_bitset_contains (mask, i))
      continue;

    if (gtk_bitset_contains (selected, i))
       select_item (i);
    else
       unselect_item (i);
  @}

gtk_selection_model_selection_changed (model, first_changed_item,
                                              n_changed_items);
  @end{pre}
  The @arg{mask} and @arg{selected} parameters must not be modified. They may
  refer to the same bitset, which would mean that every item in the set should
  be selected.
  @see-class{gtk:selection-model}
  @see-function{gtk:selection-model-is-selected}
  @see-function{gtk:selection-model-selection-in-range}"
  (model (g:object selection-model)))

(export 'selection-model-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_get_selection_in_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_selection_model_get_selection_in_range"
               selection-model-selection-in-range) (g:boxed bitset :return)
 #+liber-documentation
 "@version{2024-12-2}
  @argument[model]{a @class{gtk:selection-model} object}
  @argument[position]{an unsigned integer with the start of the queried range}
  @argument[n-items]{an unsigned integer with the number of items in the queried
    range}
  @return{The @class{gtk:bitset} instance that matches the selection state for
    the given state with all other values being undefined. The bitset must not
    be modified.}
  @begin{short}
    Gets a bitset containing a set where the values in the range
    [@arg{position}, @arg{position} + @arg{n-items}) match the selected state of
    the items in that range.
  @end{short}
  All values outside that range are undefined.

  This function is an optimization for the @fun{gtk:selection-model-selection}
  function when you are only interested in part of the model's selected state.
  A common use case is in response to the @code{\"selection-changed\"} signal.
  @see-class{gtk:selection-model}
  @see-class{gtk:bitset}
  @see-function{gtk:selection-model-selection}"
  (model (g:object selection-model))
  (position :uint)
  (n-items :uint))

(export 'selection-model-selection-in-range)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_select_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_selection_model_select_item" selection-model-select-item)
    :boolean
 #+liber-documentation
 "@version{2024-12-2}
  @argument[model]{a @class{gtk:selection-model} object}
  @argument[position]{an unsigned integer with the position to select an item
    in the model}
  @argument[unselect-rest]{a boolean whether prviously selected items should
    be unselected}
  @return{@em{True} if this action was supported and no fallback should be
    tried. This does not mean the item was selected.}
  @begin{short}
    Requests to select an item in the model.
  @end{short}
  @see-class{gtk:selection-model}"
  (model (g:object selection-model))
  (position :uint)
  (unselect-rest :boolean))

(export 'selection-model-select-item)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_unselect_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_selection_model_unselect_item"
               selection-model-unselect-item) :boolean
 #+liber-documentation
 "@version{2024-12-2}
  @argument[model]{a @class{gtk:selection-model} object}
  @argument[position]{an unsigned integer with the position of the item to
    unselect}
  @return{@em{True} if this action was supported and no fallback should be
    tried. This does not mean the item was unselected.}
  @begin{short}
    Requests to unselect an item in the model.
  @end{short}
  @see-class{gtk:selection-model}"
  (model (g:object selection-model))
  (position :uint))

(export 'selection-model-unselect-item)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_select_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_selection_model_select_range" selection-model-select-range)
    :boolean
 #+liber-documentation
 "@version{2024-12-2}
  @argument[model]{a @class{gtk:selection-model} object}
  @argument[position]{an unsigned integer with the first item to select}
  @argument[n-items]{an unsigned integer with the number of items to select}
  @argument[unselect-rest]{a boolean whether previously selected items should be
    unselected}
  @return{@em{True} if this action was supported and no fallback should be
    tried. This does not mean the range was selected.}
  @begin{short}
    Requests to select a range of items in the model.
  @end{short}
  @see-class{gtk:selection-model}"
  (model (g:object selection-model))
  (position :uint)
  (n-items :uint)
  (unselect-rest :boolean))

(export 'selection-model-select-range)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_unselect_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_selection_model_unselect_range"
               selection-model-unselect-range) :boolean
 #+liber-documentation
 "@version{2024-12-2}
  @argument[model]{a @class{gtk:selection-model} object}
  @argument[position]{an unsigned integer with the first item to unselect}
  @argument[n-items]{an unsigned integer with the number of items to unselect}
  @return{@em{True} if this action was supported and no fallback should be
    tried. This does not mean the range was unselected.}
  @begin{short}
    Requests to unselect a range of items in the model.
  @end{short}
  @see-class{gtk:selection-model}"
  (model (g:object selection-model))
  (position :uint)
  (n-items :uint))

(export 'selection-model-unselect-range)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_select_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_selection_model_select_all" selection-model-select-all)
    :boolean
 #+liber-documentation
 "@version{2024-12-2}
  @argument[model]{a @class{gtk:selection-model} object}
  @return{@em{True} if this action was supported and no fallback should be
    tried. This does not mean that all items are now selected.}
  @begin{short}
    Requests to select all items in the model.
  @end{short}
  @see-class{gtk:selection-model}"
  (model (g:object selection-model)))

(export 'selection-model-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_unselect_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_selection_model_unselect_all" selection-model-unselect-all)
    :boolean
 #+liber-documentation
 "@version{2024-12-2}
  @argument[model]{a @class{gtk:selection-model} object}
  @return{@em{True} if this action was supported and no fallback should be
    tried. This does not mean that all items are now unselected.}
  @begin{short}
    Requests to unselect all items in the model.
  @end{short}
  @see-class{gtk:selection-model}"
  (model (g:object selection-model)))

(export 'selection-model-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_model_selection_changed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_selection_model_selection_changed"
               selection-model-selection-changed) :void
 #+liber-documentation
 "@version{2024-12-2}
  @argument[model]{a @class{gtk:selection-model} object}
  @argument[position]{an unsigned integer with the first changed item}
  @argument[n-items]{an unsigned integer with the number of changed items}
  @begin{short}
    Helper function for implementations of the @class{gtk:selection-model}
    class.
  @end{short}
  Call this when the selection changes to emit the @code{\"selection-changed\"}
  signal.
  @see-class{gtk:selection-model}"
  (model (g:object selection-model))
  (position :uint)
  (n-items :uint))

(export 'selection-model-selection-changed)

;;; --- End of file gtk4.selection-model.lisp ----------------------------------
