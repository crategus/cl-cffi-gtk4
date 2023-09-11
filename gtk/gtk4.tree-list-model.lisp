;;; ----------------------------------------------------------------------------
;;; gtk4.tree-list-model.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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
;;;     GtkTreeListRow
;;;
;;; Functions
;;;
;;;     gtk_tree_list_row_is_expandable
;;;
;;; Properties
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
;;;     ╰── GtkTreeListRow
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Types and Values
;;;
;;;     GtkTreeListModel
;;;
;;; Accessors
;;;
;;;     gtk_tree_list_model_get_autoexpand
;;;     gtk_tree_list_model_set_autoexpand
;;;     gtk_tree_list_model_get_item_type
;;;     gtk_tree_list_model_get_model
;;;     gtk_tree_list_model_get_n_items
;;;     gtk_tree_list_model_get_passthrough
;;;
;;; Functions
;;;
;;;     GtkTreeListModelCreateModelFunc
;;;
;;;     gtk_tree_list_model_new
;;;     gtk_tree_list_model_get_row
;;;     gtk_tree_list_model_get_child_row
;;;
;;; Properties
;;;
;;;     autoexpand
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8
;;;     passthrough
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTreeListModel
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

#+liber-documentation
(setf (documentation 'tree-list-row 'type)
 "@version{#2023-9-10}
  @begin{short}
    The @class{gtk:tree-list-row} object is used by the
    @class{gtk:tree-list-model} object to represent items.
  @end{short}
  It allows navigating the model as a tree and modify the state of rows.

  The @class{gtk:tree-list-row} instances are created by a
  @class{gtk:tree-list-model} object only when the
  @slot[gtk:tree-list-model]{passthrough} property is not set.

  There are various support objects that can make use of
  @class{gtk:tree-list-row} objects, such as the @class{gtk:tree-expander}
  widget that allows displaying an icon to expand or collapse a row or
  the @class{gtk:tree-list-row-sorter} object that makes it possible to sort
  trees properly.
  @see-slot{gtk:tree-list-row-children}
  @see-slot{gtk:tree-list-row-depth}
  @see-slot{gtk:tree-list-row-expandable}
  @see-slot{gtk:tree-list-row-expanded}
  @see-slot{gtk:tree-list-row-item}
  @see-class{gtk:tree-list-model}
  @see-class{g:list-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- tree-list-row-children -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "children" 'tree-list-row) t)
 "The @code{children} property of type @class{g:list-model} (Read) @br{}
  The model holding the children of the row.")


;;; --- tree-list-row-depth ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "depth" 'tree-list-row) t)
 "The @code{depth} property of type @code{:uint} (Read) @br{}
  The depth in the tree of this row. @br{}
  Default value: 0")


;;; --- tree-list-row-expandable -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expandable" 'tree-list-row) t)
 "The @code{expandable} property of type @code{:boolean} (Read) @br{}
  If this row can ever be expanded. @br{}
  Default value: @em{false}")


;;; --- tree-list-row-expanded -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expanded" 'tree-list-row) t)
 "The @code{expanded} property of type @code{:boolean} (Read / Write) @br{}
  If this row is currently expanded. @br{}
  Default value: @em{false}")


;;; --- tree-list-row-item -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expanded" 'tree-list-row) t)
 "The @code{item} property of type @class{g:object} (Read) @br{}
  The item held in this row.")


;;; ----------------------------------------------------------------------------
;;; gtk_tree_list_row_is_expandle
;;;
;;; gboolean
;;; gtk_tree_list_row_is_expandable (GtkTreeListRow* self)
;;;
;;; Checks if a row can be expanded.
;;;
;;; This does not mean that the row is actually expanded, this can be checked
;;; with gtk_tree_list_row_get_expanded().
;;;
;;; If a row is expandable never changes until the row is removed from its model
;;; at which point it will forever return FALSE.
;;;
;;; Return :
;;;     TRUE if the row is expandable.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_list_row_is_expandable" tree-list-row-is-expandable)
    :boolean
  (row (g:object tree-list-row)))

(export 'tree-list-row-is-expandable)

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

#+liber-documentation
(setf (documentation 'tree-list-model 'type)
 "@version{#2023-9-10}
  @begin{short}
    The @class{gtk:tree-list-model} object is a list model that can create
    child models on demand.
  @end{short}
  @see-constructor{gtk:tree-list-model-new}
  @see-slot{gtk:tree-list-model-autoexpand}
  @see-slot{gtk:tree-list-model-item-type}
  @see-slot{gtk:tree-list-model-model}
  @see-slot{gtk:tree-list-model-n-items}
  @see-slot{gtk:tree-list-model-passthrough}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- tree-list-model-autoexpand ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "autoexpand"
                                               'tree-list-model) t)
 "The @code{autoexpand} property of type @code{:boolean} (Read / Write) @br{}
  If all rows should be expanded by default. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-list-model-autoexpand)
      "Accessor"
      (documentation 'tree-list-model-autoexpand 'function)
 "@version{#2023-9-10}
  @syntax[]{(gtk:tree-list-model-autoexpand object) => autoexpand}
  @syntax[]{(setf (gtk:tree-list-model-autoexpand object) autoexpand)}
  @argument[object]{a @class{gtk:tree-list-model} object}
  @argument[autoexpand]{@em{true} if the model is set to autoexpand}
  @begin{short}
    Accessor of the @slot[gtk:tree-list-model]{autoexpand} slot of the
    @class{gtk:tree-list-model} class.
  @end{short}
  The @fun{gtk:tree-list-model-autoexpand} function gets whether the model is
  set to automatically expand new rows that get added.

  If set to @em{true} with the @sym{(setf gtk:tree-list-model-autoexpand}
  function, the model will recursively expand all rows that get added to the
  model.  This can be either rows added by changes to the underlying models or
  via the @fun{gtk:tree-list-row-expanded} function.
  @see-class{gtk:tree-list-model}
  @see-function{gtk:tree-list-row-expanded}")

;;; --- tree-list-model-item-type ----------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type" 'tree-list-model) t)
 "The @code{item-type} property of type @class{g:type-t}(Read) @br{}
  The type of items. Since 4.8")

#+gtk-4-8
(declaim (inline tree-list-model-item-type))

#+gtk-4-8
(defun tree-list-model-item-type (object)
  (g:list-model-item-type object))

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'tree-list-model-item-type)
      "Accessor"
      (documentation 'tree-list-model-item-type 'function)
 "@version{#2023-9-10}
  @syntax[]{(gtk:tree-list-model-item-type object) => gtype}
  @argument[object]{a @class{gtk:tree-list-model} object}
  @argument[gtype]{a @class{g:type-t} type}
  @begin{short}
    Accessor of the @slot[gtk:tree-list-model]{item-type} slot of the
    @class{gtk:tree-list-model} class.
  @end{short}
  The type of items contained in the list model. Items must be subclasses of
  the @class{g:object} class.
  @begin[Note]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:tree-list-model}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- tree-list-model-model --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'tree-list-model) t)
 "The @code{model} property of type @class{g:list-model} (Read) @br{}
  The root model displayed.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-list-model-model)
      "Accessor"
      (documentation 'tree-list-model-model 'function)
 "@version{#2023-9-10}
  @syntax[]{(gtk:tree-list-model-model object) => model}
  @argument[object]{a @class{gtk:tree-list-model} object}
  @argument[model]{a @class{gtk:list-model} object root model}
  @begin{short}
    Accessor of the @slot[gtk:tree-list-model]{model} slot of the
    @class{gtk:tree-list-model} class.
  @end{short}
  The @fun{gtk:tree-list-model-model} function gets the root model that
  @arg{object} was created with.
  @see-class{gtk:tree-list-model}
  @see-class{g:list-model}")

;;; --- tree-list-model-n-items ------------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'tree-list-model) t)
 "The @code{n-items} property of type @code{:uint}(Read / Write) @br{}
  The number of items. Since 4.8 @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'tree-list-model-n-items)
      "Accessor"
      (documentation 'tree-list-model-n-items 'function)
 "@version{#2023-8-16}
  @syntax[]{(gtk:tree-list-model-n-items object) => n-items}
  @argument[object]{a @class{gtk:tree-list-model} object}
  @argument[n-items]{an unsigned integer with the number of items contained in
    the model}
  @begin{short}
    Accessor of the @slot[gtk:tree-list-model]{n-items} slot of the
    @class{gtk:tree-list-model} class.
  @end{short}
  @see-class{gtk:tree-list-model}
  @see-function{g:list-model-n-items}")

;;; --- tree-list-model-passthrough --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "passthrough"
                                               'tree-list-model) t)
 "The @code{passthrough} property of type @code{:boolean}
  (Read / Write / Construct only) @br{}
  If @em{false}, the @class{g:list-model} functions for this object return
  custom @class{gtk:tree-list-row} objects. If @em{true}, the values of the
  child models are passed through unmodified.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-list-model-passthrough)
      "Accessor"
      (documentation 'tree-list-model-passthrough 'function)
 "@version{#2023-9-10}
  @syntax[]{(gtk:tree-list-model-passthrough object) => passthrough}
  @argument[object]{a @class{gtk:tree-list-model} object}
  @argument[passthrough]{@em{true} if the model is passing through original row
    items}
  @begin{short}
    Accessor of the @slot[gtk:tree-list-model]{passthrough} slot of the
    @class{gtk:tree-list-model} class.
  @end{short}
  If the @fun{gtk:tree-list-model-passthrough} function returns @em{false}, the
  @class{g:list-model} functions for @arg{object} return custom
  @class{gtk:tree-list-row} objects. You need to call the
  @fun{gtk:tree-list-row-item} function on these objects to get the original
  item.

  If @em{true}, the values of the child models are passed through in their
  original state. You then need to call the @fun{gtk:tree-list-model-row}
  function to get the custom @class{gtk:tree-list-row} objects.
  @see-class{gtk:tree-list-model}")

;;; ----------------------------------------------------------------------------
;;; GtkTreeListModelCreateModelFunc ()
;;;
;;; GListModel *
;;; (*GtkTreeListModelCreateModelFunc) (gpointer item,
;;;                                     gpointer user_data);
;;;
;;; Prototype of the function called to create new child models when
;;; gtk_tree_list_row_set_expanded() is called.
;;;
;;; This function can return NULL to indicate that item is guaranteed to be a
;;; leaf node and will never have children. If it does not have children but may
;;; get children later, it should return an empty model that is filled once
;;; children arrive.
;;;
;;; item :
;;;     The item that is being expanded.
;;;
;;; user_data :
;;;     User data passed when registering the function
;;;
;;; Returns :
;;;     The model tracking the children of item or NULL if item can never have
;;;     children.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_list_model_new ()
;;;
;;; GtkTreeListModel *
;;; gtk_tree_list_model_new (GListModel *root,
;;;                          gboolean passthrough,
;;;                          gboolean autoexpand,
;;;                          GtkTreeListModelCreateModelFunc create_func,
;;;                          gpointer user_data,
;;;                          GDestroyNotify user_destroy);
;;;
;;; Creates a new empty GtkTreeListModel displaying root with all rows
;;; collapsed.
;;;
;;; root :
;;;     The GListModel to use as root.
;;;
;;; passthrough :
;;;     TRUE to pass through items from the models
;;;
;;; autoexpand :
;;;     TRUE to set the autoexpand property and expand the root model
;;;
;;; create_func :
;;;     Function to call to create the GListModel for the children of an item
;;;
;;; user_data :
;;;     Data to pass to create_func .
;;;
;;; user_destroy :
;;;     Function to call to free user_data
;;;
;;; Returns :
;;;     a newly created GtkTreeListModel.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_list_model_get_row ()
;;;
;;; GtkTreeListRow *
;;; gtk_tree_list_model_get_row (GtkTreeListModel *self,
;;;                              guint position);
;;;
;;; Gets the row object for the given row. If position is greater than the
;;; number of items in self , NULL is returned.
;;;
;;; The row object can be used to expand and collapse rows as well as to inspect
;;; its position in the tree. See its documentation for details.
;;;
;;; This row object is persistent and will refer to the current item as long as
;;; the row is present in self , independent of other rows being added or
;;; removed.
;;;
;;; If self is set to not be passthrough, this function is equivalent to calling
;;; g_list_model_get_item().
;;;
;;; Do not confuse this function with gtk_tree_list_model_get_child_row().
;;;
;;; self :
;;;     a GtkTreeListModel
;;;
;;; position :
;;;     the position of the row to fetch
;;;
;;; Returns :
;;;     The row item.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_list_model_get_row" tree-list-model-row)
    (g:object tree-list-row)
  (model (g:object tree-list-model))
  (position :uint))

(export 'tree-list-model-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_list_model_get_child_row ()
;;;
;;; GtkTreeListRow *
;;; gtk_tree_list_model_get_child_row (GtkTreeListModel *self,
;;;                                    guint position);
;;;
;;; Gets the row item corresponding to the child at index position for self 's
;;; root model.
;;;
;;; If position is greater than the number of children in the root model, NULL
;;; is returned.
;;;
;;; Do not confuse this function with gtk_tree_list_model_get_row().
;;;
;;; self :
;;;     a GtkTreeListModel
;;;
;;; position :
;;;     position of the child to get
;;;
;;; Returns :
;;;     the child in position .
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_list_model_get_child_row" tree-list-model-child-row)
    (g:object tree-list-row)
  (model (g:object tree-list-model))
  (position :uint))

(export 'tree-list-model-child-row)

;;; --- End of file gtk4.tree-list-model.lisp ----------------------------------
