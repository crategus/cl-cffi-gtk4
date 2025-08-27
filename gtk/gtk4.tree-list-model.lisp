;;; ----------------------------------------------------------------------------
;;; gtk4.tree-list-model.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2025 Dieter Kaiser
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
;;; Accessors
;;;
;;;     gtk_tree_list_row_get_children
;;;     gtk_tree_list_row_get_depth
;;;     gtk_tree_list_row_get_expanded
;;;     gtk_tree_list_row_set_expanded
;;;     gtk_tree_list_row_get_item
;;;
;;; Functions
;;;
;;;     gtk_tree_list_row_get_child_row
;;;     gtk_tree_list_row_get_parent
;;;     gtk_tree_list_row_get_position
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
;;;     item-type                                           Since 4.8
;;;     model
;;;     n-items                                             Since 4.8
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

(gobject:define-gobject "GtkTreeListRow" tree-list-row
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
 "@version{2024-10-17}
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

;;; --- gtk:tree-list-row-children ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "children" 'tree-list-row) t)
 "The @code{children} property of type @class{g:list-model} (Read) @br{}
  The model holding the children of the row.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-list-row-children)
      "Accessor"
      (documentation 'tree-list-row-children 'function)
 "@version{2025-07-25}
  @syntax{(gtk:tree-list-row-children object) => children}
  @argument[object]{a @class{gtk:tree-list-row} object}
  @argument[children]{a @class{g:list-model} object containing the children}
  @begin{short}
    Accessor of the @slot[gtk:tree-list-row]{children} slot of the
    @class{gtk:tree-list-row} class.
  @end{short}
  If the row is expanded, the @fun{gtk:tree-list-row-children} function gets
  the model holding the children of @arg{model}. This is the model created by
  the @sym{gtk:tree-list-model-create-model-func} callback function and
  contains the original items, no matter what value the
  @slot[gtk:tree-list-model]{passthrough} property is set to.
  @see-class{gtk:tree-list-row}
  @see-class{g:list-model}
  @see-function{gtk:tree-list-model-passthrough}")

;;; --- gtk:tree-list-row-depth ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "depth" 'tree-list-row) t)
 "The @code{depth} property of type @code{:uint} (Read) @br{}
  The depth in the tree of this row. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'tree-list-row-depth)
      "Accessor"
      (documentation 'tree-list-row-depth 'function)
 "@version{2025-04-16}
  @syntax{(gtk:tree-list-row-depth object) => depth}
  @argument[object]{a @class{gtk:tree-list-row} object}
  @argument[depth]{an unsigned integer for the depth of the row}
  @begin{short}
    Accessor of the @slot[gtk:tree-list-row]{depth} slot of the
    @class{gtk:tree-list-row} class.
  @end{short}
  The @fun{gtk:tree-list-row-depth} function gets the depth of the row. Rows
  that correspond to items in the root model have a depth of zero, rows
  corresponding to items of models of direct children of the root model have a
  depth of 1 and so on. The depth of a row never changes until the row is
  removed from its model at which point it will forever return 0.
  @see-class{gtk:tree-list-row}")

;;; --- gtk:tree-list-row-expandable -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expandable" 'tree-list-row) t)
 "The @code{expandable} property of type @code{:boolean} (Read) @br{}
  Whether the row can ever be expanded. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-list-row-expandable)
      "Accessor"
      (documentation 'tree-list-row-expandable 'function)
 "@version{2024-10-17}
  @syntax{(gtk:tree-list-row-expandable object) => expandable}
  @argument[object]{a @class{gtk:tree-list-row} object}
  @argument[expandable]{a boolean whether the row can ever be expanded}
  @begin{short}
    Accessor of the @slot[gtk:tree-list-row]{expandable} slot of the
    @class{gtk:tree-list-row} class.
  @end{short}
  The @fun{gtk:tree-list-row-expandable} function gets whether the row can
  ever be expanded.
  @see-class{gtk:tree-list-row}")

;;; --- gtk:tree-list-row-expanded ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expanded" 'tree-list-row) t)
 "The @code{expanded} property of type @code{:boolean} (Read / Write) @br{}
  Whether the row is currently expanded. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-list-row-expanded)
      "Accessor"
      (documentation 'tree-list-row-expanded 'function)
 "@version{2024-10-17}
  @syntax{(gtk:tree-list-row-expanded object) => expanded}
  @argument[object]{a @class{gtk:tree-list-row} object}
  @argument[expanded]{a boolean whether the row is currently expanded}
  @begin{short}
    Accessor of the @slot[gtk:tree-list-row]{expanded} slot of the
    @class{gtk:tree-list-row} class.
  @end{short}
  The @fun{gtk:tree-list-row-expanded} function gets if a row is currently
  expanded.
  @see-class{gtk:tree-list-row}")

;;; --- gtk:tree-list-row-item -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "item" 'tree-list-row) t)
 "The @code{item} property of type @class{g:object} (Read) @br{}
  The item held in this row.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-list-row-item)
      "Accessor"
      (documentation 'tree-list-row-item 'function)
 "@version{2025-04-16}
  @syntax{(gtk:tree-list-row-item object) => item}
  @argument[object]{a @class{gtk:tree-list-row} object}
  @argument[item]{a @class{g:object} instance for the item held in this
    list row}
  @begin{short}
    Accessor of the @slot[gtk:tree-list-row]{item} slot of the
    @class{gtk:tree-list-row} class.
  @end{short}
  The @fun{gtk:tree-list-row-item} function gets the item corresponding to this
  list row.
  @see-class{gtk:tree-list-row}")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_list_row_get_child_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_list_row_get_child_row" tree-list-row-child-row)
    (g:object tree-list-row :return)
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[listrow]{a @class{gtk:tree-list-row} object}
  @argument[pos]{an unsigned integer for the position of the child to get}
  @return{The @class{gtk:tree-list-row} object for the child in @arg{pos}.}
  @begin{short}
    If @arg{listrow} is not expanded or @arg{pos} is greater than the number of
    children, @code{nil} is returned.
  @end{short}
  @see-class{gtk:tree-list-row}"
  (listrow (g:object tree-list-row))
  (pos :uint))

(export 'tree-list-row-child-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_list_row_get_parent
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_list_row_get_parent" tree-list-row-parent)
    (g:object tree-list-row :return)
 #+liber-documentation
 "@version{2024-10-29}
  @argument[listrow]{a @class{gtk:tree-list-row} object}
  @return{The parent @class{gtk:tree-list-row} object.}
  @begin{short}
    Gets the list row representing the parent for @arg{listrow}.
  @end{short}
  That is the list row that would need to be collapsed to make this list row
  disappear. If @arg{listrow} is a list row corresponding to the root model,
  @code{nil} is returned. The value returned by this function never changes
  until the list row is removed from its model at which point it will forever
  return @code{nil}.
  @see-class{gtk:tree-list-row}"
  (listrow (g:object tree-list-row)))

(export 'tree-list-row-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_list_row_get_position
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_list_row_get_position" tree-list-row-position) :uint
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[listrow]{a @class{gtk:tree-list-row} object}
  @return{The unsigned integer for the position in the model.}
  @begin{short}
    Returns the position in the model that @arg{listrow} occupies at the moment.
  @end{short}
  @see-class{gtk:tree-list-row}"
  (listrow (g:object tree-list-row)))

(export 'tree-list-row-position)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_list_row_is_expandable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_list_row_is_expandable" tree-list-row-is-expandable)
    :boolean
 #+liber-documentation
 "@version{#2024-10-29}
  @argument[listrow]{a @class{gtk:tree-list-row} object}
  @return{@em{True} if the row is expandable.}
  @begin{short}
    Checks if a row can be expanded.
  @end{short}
  This does not mean that the row is actually expanded, this can be checked
  with the @fun{gtk:tree-list-row-expanded} function. If a row is expandable
  never changes until the row is removed from its model at which point it will
  forever return @em{false}.
  @see-class{gtk:tree-list-row}
  @see-function{gtk:tree-list-row-expanded}"
  (listrow (g:object tree-list-row)))

(export 'tree-list-row-is-expandable)

;;; ----------------------------------------------------------------------------
;;; GtkTreeListModel
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkTreeListModel" tree-list-model
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
 "@version{2025-04-16}
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

;;; --- gtk:tree-list-model-autoexpand -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "autoexpand" 'tree-list-model) t)
 "The @code{autoexpand} property of type @code{:boolean} (Read / Write) @br{}
  Whether all rows should be expanded by default. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-list-model-autoexpand)
      "Accessor"
      (documentation 'tree-list-model-autoexpand 'function)
 "@version{2024-10-17}
  @syntax{(gtk:tree-list-model-autoexpand object) => autoexpand}
  @syntax{(setf (gtk:tree-list-model-autoexpand object) autoexpand)}
  @argument[object]{a @class{gtk:tree-list-model} object}
  @argument[autoexpand]{@em{true} if the model is set to autoexpand}
  @begin{short}
    Accessor of the @slot[gtk:tree-list-model]{autoexpand} slot of the
    @class{gtk:tree-list-model} class.
  @end{short}
  The @fun{gtk:tree-list-model-autoexpand} function gets whether the model is
  set to automatically expand new rows that get added.

  If set to @em{true} with the @setf{gtk:tree-list-model-autoexpand} function,
  the model will recursively expand all rows that get added to the model.  This
  can be either rows added by changes to the underlying models or via the
  @fun{gtk:tree-list-row-expanded} function.
  @see-class{gtk:tree-list-model}
  @see-function{gtk:tree-list-row-expanded}")

;;; --- gtk:tree-list-model-item-type ------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type" 'tree-list-model) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. Since 4.8")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'tree-list-model-item-type)
      "Accessor"
      (documentation 'tree-list-model-item-type 'function)
 "@version{2025-08-17}
  @syntax{(gtk:tree-list-model-item-type object) => gtype}
  @argument[object]{a @class{gtk:tree-list-model} object}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    The accessor for the @slot[gtk:tree-list-model]{item-type} slot of the
    @class{gtk:tree-list-model} class returns the type of items contained in
    the list model.
  @end{short}
  Items must be subclasses of the @class{g:object} class.

  Since 4.8
  @begin[Notes]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:tree-list-model}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- gtk:tree-list-model-model ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'tree-list-model) t)
 "The @code{model} property of type @class{g:list-model} (Read) @br{}
  The root model displayed.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-list-model-model)
      "Accessor"
      (documentation 'tree-list-model-model 'function)
 "@version{2024-10-17}
  @syntax{(gtk:tree-list-model-model object) => model}
  @argument[object]{a @class{gtk:tree-list-model} object}
  @argument[model]{a @class{g:list-model} object root model}
  @begin{short}
    Accessor of the @slot[gtk:tree-list-model]{model} slot of the
    @class{gtk:tree-list-model} class.
  @end{short}
  The @fun{gtk:tree-list-model-model} function gets the root model that
  @arg{object} was created with.
  @see-class{gtk:tree-list-model}
  @see-class{g:list-model}")

;;; --- gtk:tree-list-model-n-items --------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'tree-list-model) t)
 "The @code{n-items} property of type @code{:uint}(Read) @br{}
  The number of items contained in the model. Since 4.8 @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'tree-list-model-n-items)
      "Accessor"
      (documentation 'tree-list-model-n-items 'function)
 "@version{2025-08-17}
  @syntax{(gtk:tree-list-model-n-items object) => n-items}
  @argument[object]{a @class{gtk:tree-list-model} object}
  @argument[n-items]{an unsigned integer for the number of items contained in
    the model}
  @begin{short}
    The accessor for the @slot[gtk:tree-list-model]{n-items} slot of the
    @class{gtk:tree-list-model} class returns the number of items contained in
    the model.
  @end{short}
  @see-class{gtk:tree-list-model}
  @see-function{g:list-model-n-items}")

;;; --- gtk:tree-list-model-passthrough ----------------------------------------

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
 "@version{2024-10-17}
  @syntax{(gtk:tree-list-model-passthrough object) => passthrough}
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
;;; GtkTreeListModelCreateModelFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback tree-list-model-create-model-func (g:object g:list-model)
    ((item :pointer)
     (data :pointer))
  (let ((object (cffi:convert-from-foreign item 'g:object))
        (func (glib:get-stable-pointer-value data)))
    (funcall func object)))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-list-model-create-model-func)
      "Callback"
      (liber:symbol-documentation 'tree-list-model-create-model-func)
 "@version{2025-06-20}
  @syntax{lambda (object) => result}
  @argument[object]{a @class{g:object} instance for the item that is being
    expanded}
  @argument[result]{a @class{g:list-model} object tracking the children of
    @arg{object} or @code{nil} if @arg{object} can never have children}
  @begin{short}
    Prototype of the function called to create new child models when the
    @fun{gtk:tree-list-row-expanded} function is called.
  @end{short}
  This function can return @code{nil} to indicate that @arg{object} is
  guaranteed to be a leaf node and will never have children. If it does not have
  children but may get children later, it should return an empty model that is
  filled once children arrive.
  @see-class{gtk:tree-list-model}
  @see-class{g:list-model}
  @see-function{gtk:tree-list-model-new}
  @see-function{gtk:tree-list-row-expanded}")

(export 'tree-list-model-create-model-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_list_model_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_list_model_new" %tree-list-model-new)
    (g:object tree-list-model :return)
  (root (g:object g:list-model))
  (passthrough :boolean)
  (autoexpand :boolean)
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun tree-list-model-new (root passthrough autoexpand func)
 #+liber-documentation
 "@version{2025-07-25}
  @argument[root]{a @class{g:list-model} object to use as root}
  @argument[passthrough]{@em{true} to pass through items from the models}
  @argument[autoexpand]{@em{true} to set the
    @slot[gtk:tree-list-model]{autoexpand} property and expand the root model}
  @argument[func]{a @sym{gtk:tree-list-model-create-model-func} callback
  function to create the @class{g:list-model} object for the children of an
    item}
  @begin{short}
    Creates a new empty @class{gtk:tree-list-model} object displaying @arg{root}
    with all rows collapsed.
  @end{short}
  @see-class{gtk:tree-list-model}
  @see-class{g:list-model}
  @see-symbol{gtk:tree-list-model-create-model-func}
  @see-function{gtk:tree-list-model-autoexpand}"
  (%tree-list-model-new root
                        passthrough
                        autoexpand
                        (cffi:callback tree-list-model-create-model-func)
                        (glib:allocate-stable-pointer func)
                        (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'tree-list-model-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_list_model_get_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_list_model_get_row" tree-list-model-row)
    (g:object tree-list-row)
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[model]{a @class{gtk:tree-list-model} object}
  @argument[position]{an unsigned integer for the position of the row to fetch}
  @return{The @class{gtk:tree-list-row} object for the row item.}
  @begin{short}
    Gets the row object for the given row.
  @end{short}
  If @arg{position} is greater than the number of items in @arg{model},
  @code{nil} is returned. The row object can be used to expand and collapse rows
  as well as to inspect its position in the tree. See its documentation for
  details.

  This row object is persistent and will refer to the current item as long as
  the row is present in @arg{model}, independent of other rows being added or
  removed. If @arg{model} is set to not be passthrough, this function is
  equivalent to calling the @fun{g:list-model-item} function. Do not confuse
  this function with the @fun{gtk:tree-list-model-child-row} function.
  @see-class{gtk:tree-list-model}
  @see-class{gtk:tree-list-row}
  @see-function{g:list-model-item}
  @see-function{gtk:tree-list-model-child-row}"
  (model (g:object tree-list-model))
  (position :uint))

(export 'tree-list-model-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_list_model_get_child_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_list_model_get_child_row" tree-list-model-child-row)
    (g:object tree-list-row)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[model]{a @class{gtk:tree-list-model} object}
  @argument[position]{an unsigned integer for the position of the child to get}
  @begin{return}
    The @class{gtk:tree-list-row} object with the child in @arg{position}.
  @end{return}
  @begin{short}
    Gets the row item corresponding to the child at index @arg{position} for
    @arg{model}'s root model.
  @end{short}
  If @arg{position} is greater than the number of children in the root model,
  @code{nil} is returned. Do not confuse this function with the
  @fun{gtk:tree-list-model-row} function.
  @see-class{gtk:tree-list-model}
  @see-class{gtk:tree-list-row}
  @see-function{gtk:tree-list-model-row}"
  (model (g:object tree-list-model))
  (position :uint))

(export 'tree-list-model-child-row)

;;; --- End of file gtk4.tree-list-model.lisp ----------------------------------
