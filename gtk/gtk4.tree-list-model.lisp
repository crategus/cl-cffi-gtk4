;;; ----------------------------------------------------------------------------
;;; gtk4.tree-list-model.lisp
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
;;; GtkTreeListModel
;;;
;;;     A list model that can create child models on demand
;;;
;;; Types and Values
;;;
;;;     GtkTreeListModel
;;;     GtkTreeListRow
;;;
;;; Functions
;;;
;;;     GtkTreeListModelCreateModelFunc
;;;
;;;     gtk_tree_list_model_new 
;;;     gtk_tree_list_model_get_model 
;;;     gtk_tree_list_model_get_passthrough 
;;;     gtk_tree_list_model_set_autoexpand 
;;;     gtk_tree_list_model_get_autoexpand 
;;;     gtk_tree_list_model_get_child_row 
;;;     gtk_tree_list_model_get_row 
;;;
;;; Properties
;;;
;;;     autoexpand
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8
;;;     passthrough
;;;
;;;     children
;;;     depth
;;;     expandable
;;;     expanded
;;;     item
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ├── GtkTreeListModel
;;;     ╰── GtkTreeListRow
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTreeListRow
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkTreeListRow" tree-list-row
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_tree_list_row_get_type")
  ((children
    tree-list-row-children
    "children" "GListModel" t nil)
   (depth
    tree-list-row-depth
    "depth" "guint" t nil)
   (expandable
    tree-list-row-expandable
    "expandable" "gboolean" t nil)
   (expanded
    tree-list-row-expanded
    "expanded" "gboolean" t t)
   (item
    tree-list-row-item
    "item" "GObject" t nil)))
    
;;; ----------------------------------------------------------------------------
;;; GtkTreeListModel
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkTreeListModel" tree-list-model
  (:superclass g:object
   :export t
   :interfaces ("GListModel")
   :type-initializer "gtk_tree_list_model_get_type")
  ((autoexpand
    tree-list-model-autoexpand
    "autoexpand" "gboolean" t t)
   #+gtk-4-8
   (item-type
    tree-list-model-item-type
    "item-type" "GType" t nil)
   (model
    tree-list-model-model
    "model" "GListModel" t nil)
   #+gtk-4-8
   (n-items
    tree-list-model-n-items
    "n-items" "guint" t nil)
   (passthrough
    tree-list-model-passthrough
    "passthrough" "gboolean" t t)))

;;;Property Details
;;;The “autoexpand” property
;;;  “autoexpand”               gboolean
;;;If all rows should be expanded by default

;;;Owner: GtkTreeListModel

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “model” property
;;;  “model”                    GListModel *
;;;The root model displayed

;;;Owner: GtkTreeListModel

;;;Flags: Read

;;;The “passthrough” property
;;;  “passthrough”              gboolean
;;;If FALSE, the GListModel functions for this object return custom GtkTreeListRow objects. If TRUE, the values of the child models are pass through unmodified.

;;;Owner: GtkTreeListModel

;;;Flags: Read / Write / Construct Only

;;;Default value: FALSE

;;;The “children” property
;;;  “children”                 GListModel *
;;;The model holding the row's children.

;;;Owner: GtkTreeListRow

;;;Flags: Read

;;;The “depth” property
;;;  “depth”                    guint
;;;The depth in the tree of this row

;;;Owner: GtkTreeListRow

;;;Flags: Read

;;;Default value: 0

;;;The “expandable” property
;;;  “expandable”               gboolean
;;;If this row can ever be expanded

;;;Owner: GtkTreeListRow

;;;Flags: Read

;;;Default value: FALSE

;;;The “expanded” property
;;;  “expanded”                 gboolean
;;;If this row is currently expanded

;;;Owner: GtkTreeListRow

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “item” property
;;;  “item”                     GObject *
;;;The item held in this row

;;;Owner: GtkTreeListRow

;;;Flags: Read

;;;See Also
;;;GListModel


;;;Description
;;;GtkTreeListModel is a GListModel implementation that can expand rows by creating new child list models on demand.

;;;Functions
;;;GtkTreeListModelCreateModelFunc ()
;;;GListModel *
;;;(*GtkTreeListModelCreateModelFunc) (gpointer item,
;;;                                    gpointer user_data);
;;;Prototype of the function called to create new child models when gtk_tree_list_row_set_expanded() is called.

;;;This function can return NULL to indicate that item is guaranteed to be a leaf node and will never have children. If it does not have children but may get children later, it should return an empty model that is filled once children arrive.

;;;Parameters
;;;item

;;;The item that is being expanded.

;;;[type GObject]
;;;user_data

;;;User data passed when registering the function

;;;Returns
;;;The model tracking the children of item or NULL if item can never have children.

;;;[nullable][transfer full]

;;;gtk_tree_list_model_new ()
;;;GtkTreeListModel *
;;;gtk_tree_list_model_new (GListModel *root,
;;;                         gboolean passthrough,
;;;                         gboolean autoexpand,
;;;                         GtkTreeListModelCreateModelFunc create_func,
;;;                         gpointer user_data,
;;;                         GDestroyNotify user_destroy);
;;;Creates a new empty GtkTreeListModel displaying root with all rows collapsed.

;;;Parameters
;;;root

;;;The GListModel to use as root.

;;;[transfer full]
;;;passthrough

;;;TRUE to pass through items from the models

;;;autoexpand

;;;TRUE to set the autoexpand property and expand the root model

;;;create_func

;;;Function to call to create the GListModel for the children of an item

;;;user_data

;;;Data to pass to create_func .

;;;[closure]
;;;user_destroy

;;;Function to call to free user_data

;;;Returns
;;;a newly created GtkTreeListModel.

;;;gtk_tree_list_model_get_model ()
;;;GListModel *
;;;gtk_tree_list_model_get_model (GtkTreeListModel *self);
;;;Gets the root model that self was created with.

;;;Parameters
;;;self

;;;a GtkTreeListModel

;;;Returns
;;;the root model.

;;;[transfer none]

;;;gtk_tree_list_model_get_passthrough ()
;;;gboolean
;;;gtk_tree_list_model_get_passthrough (GtkTreeListModel *self);
;;;If this function returns FALSE, the GListModel functions for self return custom GtkTreeListRow objects. You need to call gtk_tree_list_row_get_item() on these objects to get the original item.

;;;If TRUE, the values of the child models are passed through in their original state. You then need to call gtk_tree_list_model_get_row() to get the custom GtkTreeListRows.

;;;Parameters
;;;self

;;;a GtkTreeListModel

;;;Returns
;;;TRUE if the model is passing through original row items

;;;gtk_tree_list_model_set_autoexpand ()
;;;void
;;;gtk_tree_list_model_set_autoexpand (GtkTreeListModel *self,
;;;                                    gboolean autoexpand);
;;;If set to TRUE, the model will recursively expand all rows that get added to the model. This can be either rows added by changes to the underlying models or via gtk_tree_list_row_set_expanded().

;;;Parameters
;;;self

;;;a GtkTreeListModel

;;;autoexpand

;;;TRUE to make the model autoexpand its rows

;;;gtk_tree_list_model_get_autoexpand ()
;;;gboolean
;;;gtk_tree_list_model_get_autoexpand (GtkTreeListModel *self);
;;;Gets whether the model is set to automatically expand new rows that get added. This can be either rows added by changes to the underlying models or via gtk_tree_list_row_set_expanded().

;;;Parameters
;;;self

;;;a GtkTreeListModel

;;;Returns
;;;TRUE if the model is set to autoexpand

;;;gtk_tree_list_model_get_child_row ()
;;;GtkTreeListRow *
;;;gtk_tree_list_model_get_child_row (GtkTreeListModel *self,
;;;                                   guint position);
;;;Gets the row item corresponding to the child at index position for self 's root model.

;;;If position is greater than the number of children in the root model, NULL is returned.

;;;Do not confuse this function with gtk_tree_list_model_get_row().

;;;Parameters
;;;self

;;;a GtkTreeListModel

;;;position

;;;position of the child to get

;;;Returns
;;;the child in position .

;;;[nullable][transfer full]

;;;gtk_tree_list_model_get_row ()
;;;GtkTreeListRow *
;;;gtk_tree_list_model_get_row (GtkTreeListModel *self,
;;;                             guint position);
;;;Gets the row object for the given row. If position is greater than the number of items in self , NULL is returned.

;;;The row object can be used to expand and collapse rows as well as to inspect its position in the tree. See its documentation for details.

;;;This row object is persistent and will refer to the current item as long as the row is present in self , independent of other rows being added or removed.

;;;If self is set to not be passthrough, this function is equivalent to calling g_list_model_get_item().

;;;Do not confuse this function with gtk_tree_list_model_get_child_row().

;;;Parameters
;;;self

;;;a GtkTreeListModel

;;;position

;;;the position of the row to fetch

;;;Returns
;;;The row item.

;;;[nullable][transfer full]


;;; --- End of file gtk4.tree-list-model.lisp ----------------------------------
