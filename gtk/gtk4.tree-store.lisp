;;; ----------------------------------------------------------------------------
;;; gtk4.tree-store.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkTreeStore
;;;
;;;     A tree-like data structure that can be used with the GtkTreeView
;;;
;;; Types and Values
;;;
;;;     GtkTreeStore
;;;
;;; Functions
;;;
;;;     gtk_tree_store_new
;;;     gtk_tree_store_newv
;;;     gtk_tree_store_set_column_types
;;;     gtk_tree_store_set_value
;;;     gtk_tree_store_set
;;;     gtk_tree_store_set_valist
;;;     gtk_tree_store_set_valuesv
;;;     gtk_tree_store_remove
;;;     gtk_tree_store_insert
;;;     gtk_tree_store_insert_before
;;;     gtk_tree_store_insert_after
;;;     gtk_tree_store_insert_with_values
;;;     gtk_tree_store_insert_with_valuesv
;;;     gtk_tree_store_prepend
;;;     gtk_tree_store_append
;;;     gtk_tree_store_is_ancestor
;;;     gtk_tree_store_iter_depth
;;;     gtk_tree_store_clear
;;;     gtk_tree_store_iter_is_valid
;;;     gtk_tree_store_reorder
;;;     gtk_tree_store_swap
;;;     gtk_tree_store_move_before
;;;     gtk_tree_store_move_after
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTreeStore
;;;
;;; Implemented Interfaces
;;;
;;;     GtkBuildable
;;;     GtkTreeDragDest
;;;     GtkTreeDragSource
;;;     GtkTreeModel
;;;     GtkTreeSortable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTreeStore
;;; ----------------------------------------------------------------------------

;; TODO: Implement the missing interfaces

(define-g-object-class "GtkTreeStore" tree-store
  (:superclass g:object
   :export t
   :interfaces ("GtkBuildable"
;                "GtkTreeDragDest"
;                "GtkTreeDragSource"
                "GtkTreeModel"
;                "GtkTreeSortable"
                )
   :type-initializer "gtk_tree_store_get_type")
  nil)

#+liber-documentation
(setf (documentation 'tree-store 'type)
 "@version{#2021-3-3}
  @begin{short}
    The @sym{gtk:tree-store} object is a list model for use with a
    @class{gtk:tree-view} widget.
  @end{short}
  It implements the @class{gtk:tree-model} interface, and consequentialy, can
  use all of the methods available there. It also implements the
  @class{gtk:tree-sortable} interface so it can be sorted by the tree view.
  Finally, it also implements the tree drag and drop interfaces.
  @begin[GtkTreeStore as GtkBuildable]{dictionary}
    The @sym{gtk:tree-store} implementation of the @class{gtk:buildable}
    interface allows to specify the model columns with a @code{<columns>}
    element that may contain multiple @code{<column>} elements, each specifying
    one model column. The \"type\" attribute specifies the data type for
    the column.

    @b{Example:} A UI Definition fragment for a tree store
    @begin{pre}
<object class=\"GtkTreeStore\">
  <columns>
    <column type=\"gchararray\"/>
    <column type=\"gchararray\"/>
    <column type=\"gint\"/>
  </columns>
</object>
    @end{pre}
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-sortable}")

;;; ----------------------------------------------------------------------------

(defmethod initialize-instance :after ((store tree-store)
                                       &rest initargs
                                       &key (column-types
                                             nil
                                             column-types-p)
                                       &allow-other-keys)
  (declare (ignore initargs))
  (when column-types-p
    (tree-store-set-column-types store column-types)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_new
;;; ----------------------------------------------------------------------------

(declaim (inline tree-store-new))

(defun tree-store-new (&rest column-types)
 #+liber-documentation
 "@version{2022-11-24}
  @argument[column-types]{all @class{g:type-t} types for the columns, from first
    to last}
  @return{A new @class{gtk:tree-store} object.}
  @begin{short}
    Creates a new tree store as with columns of the types passed in.
  @end{short}
  Note that only types derived from standard GType fundamental types are
  supported.
  @begin[Example]{dictionary}
    The following example creates a new @class{gtk:tree-store} object with
    three columns, of type \"gint\", \"gchararray\", and \"GdkPixbuf\"
    respectively.
    @begin{pre}
(gtk:tree-store-new \"gint\" \"gchararray\" \"GdkPixbuf\")
    @end{pre}
  @end{dictionary}
  @see-class{g:type-t}
  @see-class{gtk:tree-store}"
  (make-instance 'tree-store
                 :column-types column-types))

(export 'tree-store-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_newv ()
;;;
;;; GtkTreeStore * gtk_tree_store_newv (gint n_columns, GType *types);
;;;
;;; Non vararg creation function. Used primarily by language bindings.
;;;
;;; n_columns :
;;;     number of columns in the tree store
;;;
;;; types :
;;;     an array of GType types for the columns, from first to last
;;;
;;; Returns :
;;;     a new GtkTreeStore Rename to: gtk_tree_store_new
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_set_column_types
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_set_column_types"
          %tree-store-set-column-types) :void
  (store (g:object tree-store))
  (n-columns :int)
  (types :pointer))

(defun tree-store-set-column-types (store types)
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:tree-store} object}
  @argument[types]{a list @class{g:type-t} types, one for each column}
  @begin{short}
    This function is meant primarily for GObjects that inherit from the
    @class{gtk:tree-store} class, and should only be used when constructing a
    new @class{gtk:tree-store} object.
  @end{short}
  It will not function after a row has been added, or a method on a
  @class{gtk:tree-model} object is called.
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-model}
  @see-class{g:type-t}"
  (let ((n (length types)))
    (with-foreign-object (types-ar 'g:type-t n)
      (iter (for i from 0 below n)
            (for gtype in types)
            (setf (cffi:mem-aref types-ar 'g:type-t i) gtype))
      (%tree-store-set-column-types store n types-ar))))

(export 'tree-store-set-column-types)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_set_value
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_set_value" %tree-store-set-value) :void
  (store (g:object tree-store))
  (iter (g:boxed tree-iter))
  (column :int)
  (value :pointer))

(defun tree-store-set-value (store iter column value)
 #+liber-documentation
 "@version{#2021-3-3}
  @argument[store]{a @class{gtk:tree-store} object}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator for the row being
    modified}
  @argument[column]{an integer with the column number to modify}
  @argument[value]{new value for the cell}
  @begin{short}
    Sets the data in the cell specified by @arg{iter} and @arg{column}.
  @end{short}
  The type of value must be convertible to the type of the column.
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-store-set}"
  (with-foreign-object (gvalue '(:struct g:value))
    (set-g-value gvalue
                 value
                 (tree-model-column-type store column)
                 :zero-g-value t)
    (%tree-store-set-value store iter column gvalue)
    (gobject:value-unset gvalue)
    (values)))

(export 'tree-store-set-value)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_set
;;; ----------------------------------------------------------------------------

(defun tree-store-set (store iter &rest values)
 #+liber-documentation
 "@version{2022-11-24}
  @argument[store]{a @class{gtk:tree-store} object}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator for the row being
    modified}
  @argument[values]{the values to set}
  @return{The @class{gtk:tree-iter} iterator for the row being modified.}
  @begin{short}
    Sets the values of one or more cells in the row referenced by @arg{iter}.
  @end{short}
  The variable argument list should contain the values to be set.
  @begin[Example]{dictionary}
    @begin{pre}
(let ((model (gtk:tree-store-new \"gchararray\" \"gchararray\" \"guint\")))
  ;; First Book
  (let ((iter (gtk:tree-store-append model nil))) ; Toplevel iterator
    ;; Set the toplevel row
    (gtk:tree-store-set model
                        iter
                        \"The Art of Computer Programming\"
                        \"Donald E. Knuth\"
                        2011)
    ;; Append and set three child rows
    (gtk:tree-store-set model
                        (gtk:tree-store-append model iter) ; Child iterator
                        \"Volume 1: Fundamental Algorithms\"
                        \"\"
                        1997)
  ... ))
    @end{pre}
  @end{dictionary}
  @begin[Note]{dictionary}
    The Lisp implemenation does not support pairs of a column index and a value,
    but a list of values. Therefore, it is not possible to set individual
    columns. See the @fun{gtk:tree-store-set-value} function for setting the
    value of single columns.
  @end{dictionary}
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-store-set-value}"
  (let ((n (length values)))
    (with-foreign-objects ((value-ar '(:struct g:value) n)
                           (columns-ar :int n))
      (loop for i from 0 below n
            for value in values
            for gtype = (tree-model-column-type store i)
            do (setf (cffi:mem-aref columns-ar :int i) i)
               (set-g-value (cffi:mem-aptr value-ar '(:struct g:value) i)
                            value
                            gtype
                            :zero-g-value t))
      (%tree-store-set-valuesv store iter columns-ar value-ar n)
      (loop for i from 0 below n
            do (gobject:value-unset (cffi:mem-aptr value-ar
                                                   '(:struct g:value) i)))
      iter)))

(export 'tree-store-set)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_set_valist ()
;;;
;;; void gtk_tree_store_set_valist (GtkTreeStore *tree_store,
;;;                                 GtkTreeIter *iter,
;;;                                 va_list var_args);
;;;
;;; See gtk_tree_store_set(); this version takes a va_list for use by language
;;; bindings.
;;;
;;; tree_store :
;;;     A GtkTreeStore
;;;
;;; iter :
;;;     A valid GtkTreeIter for the row being modified
;;;
;;; var_args :
;;;     va_list of column/value pairs
;;; ----------------------------------------------------------------------------

;; Implementation not needed

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_set_valuesv                             not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_set_valuesv" %tree-store-set-valuesv) :void
 #+liber-documentation
 "@version{#2013-7-4}
  @argument[tree-store]{a @class{gtk:tree-store} object}
  @argument[iter]{a valid @class{gtk:tree-iter} for the row being modified}
  @argument[columns]{an array of column numbers}
  @argument[values]{an array of @symbol{g:value}'s}
  @argument[n-values]{the length of the columns and values arrays}
  @begin{short}
    A variant of the @fun{gtk:tree-store-set} function which takes the columns
    and values as two arrays. This function is mainly intended
    for language bindings or in case the number of columns to change is not
    known until run-time.
  @end{short}
  @see-function{gtk:tree-store-set}"
  (store (g:object tree-store))
  (iter (g:boxed tree-iter))
  (columns :pointer)
  (values :pointer)
  (n-values :int))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_remove
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_remove" tree-store-remove) :boolean
 #+liber-documentation
 "@version{#2021-3-5}
  @argument[store]{a @class{gtk:tree-store} object}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator}
  @return{@em{True} if @arg{iter} is still valid, @code{nil} if not.}
  @begin{short}
    Removes the given row from the tree store.
  @end{short}
  After being removed, @arg{iter} is set to the next valid row at that level, or
  invalidated if it previously pointed to the last one in the tree store.
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-store-insert}"
  (store (g:object tree-store))
  (iter (g:boxed tree-iter)))

(export 'tree-store-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_insert
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_insert" %tree-store-insert) :void
  (store (g:object tree-store))
  (iter (g:boxed tree-iter))
  (parent (g:boxed tree-iter))
  (position :int))

(defun tree-store-insert (store parent position)
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:tree-store} object}
  @argument[parent]{a valid @class{gtk:tree-iter} iterator, or @code{nil}}
  @argument[position]{an integer with the position to insert the new row}
  @return{A @class{gtk:tree-iter} iterator to the new row.}
  @begin{short}
    Creates a new row at @arg{position}.
  @end{short}
  If @arg{parent} is non-@code{nil}, then the row will be made a child of
  @arg{parent}. Otherwise, the row will be created at the toplevel. If
  @arg{position} is larger than the number of rows at that level, then the new
  row will be inserted to the end of the list. The returned iterator point to
  this new row. The row will be empty after this function is called. To fill in
  values, you need to call the @fun{gtk:tree-store-set} or
  @fun{gtk:tree-store-set-value} functions.
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-store-remove}
  @see-function{gtk:tree-store-set}
  @see-function{gtk:tree-store-set-value}"
  (let ((iter (make-tree-iter)))
    (%tree-store-insert store iter parent position)
    iter))

(export 'tree-store-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_insert_before
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_insert_before" %tree-store-insert-before) :void
  (store (g:object tree-store))
  (iter (g:boxed tree-iter))
  (parent (g:boxed tree-iter))
  (sibling (g:boxed tree-iter)))

(defun tree-store-insert-before (store parent sibling)
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:tree-store} object}
  @argument[parent]{a valid @class{gtk:tree-iter} iterator, or @code{nil}}
  @argument[sibling]{a valid @class{gtk:tree-iter} iterator, or @code{nil}}
  @begin{short}
    Inserts a new row before sibling.
  @end{short}
  If @arg{sibling} is @code{nil}, then the row will be appended to
  @arg{parent}'s children. If @arg{parent} and @arg{sibling} are @arg{nil}, then
  the row will be appended to the toplevel. If both @arg{sibling} and
  @arg{parent} are set, then @arg{parent} must be the parent of @arg{sibling}.
  When @arg{sibling} is set, @arg{parent} is optional.

  The returned iterator point to this new row. The row will be empty after this
  function is called. To fill in values, you need to call the
  @fun{gtk:tree-store-set} or @fun{gtk:tree-store-set-value} functions.
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-store-set}
  @see-function{gtk:tree-store-set-value}"
  (let ((iter (make-tree-iter)))
    (%tree-store-insert-before store iter parent sibling)
    iter))

(export 'tree-store-insert-before)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_insert_after
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_insert_after" %tree-store-insert-after) :void
  (store (g:object tree-store))
  (iter (g:boxed tree-iter))
  (parent (g:boxed tree-iter))
  (sibling (g:boxed tree-iter)))

(defun tree-store-insert-after (store parent sibling)
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:tree-store} object}
  @argument[parent]{a valid @class{gtk:tree-iter} iterator, or @code{nil}}
  @argument[sibling]{a valid @class{gtk:tree-iter} iterator, or @code{nil}}
  @begin{short}
    Inserts a new row after sibling.
  @end{short}
  If @arg{sibling} is @code{nil}, then the row will be prepended to
  @arg{parent}'s children. If @arg{parent} and @arg{sibling} are @code{nil},
  then the row will be prepended to the toplevel. If both @arg{sibling} and
  @arg{parent} are set, then @arg{parent} must be the parent of @arg{sibling}.
  When @arg{sibling} is set, @arg{parent} is optional.

  The returned iterator point to this new row. The row will be empty after this
  function is called. To fill in values, you need to call the
  @fun{gtk:tree-store-set} or @fun{gtk:tree-store-set-value} functions.
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-store-set}
  @see-function{gtk:tree-store-set-value}"
  (let ((iter (make-tree-iter)))
    (%tree-store-insert-after store iter parent sibling)
    iter))

(export 'tree-store-insert-after)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_insert_with_values
;;; ----------------------------------------------------------------------------

(defun tree-store-insert-with-values (store parent position &rest values)
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:tree-store} object}
  @argument[parent]{a valid @class{gtk:tree-iter} iterator, or @code{nil}}
  @argument[position]{an integer with the position to insert the new row, or -1
    to append after existing rows}
  @argument[values]{pairs of column number and value}
  @return{A @class{gtk:tree-iterator} iterator.}
  @begin{short}
    Creates a new row at @arg{position}.
  @end{short}

  The returned iterator point to this new row. If @arg{position} is -1, or
  larger than the number of rows on the list, then the new row will be appended
  to the list. The row will be filled with the values given to this function.

  Calling this function has the same effect as calling
  @begin{pre}
(let ((iter (gtk:tree-store-insert store iter position)))
  (gtk:tree-store-set store iter values)
  .. )
  @end{pre}
  with the different that the former will only emit a \"row-inserted\" signal,
  while the latter will emit \"row-inserted\", \"row-changed\" and if the tree
  store is sorted, \"rows-reordered\". Since emitting the \"rows-reordered\"
  signal repeatedly can affect the performance of the program, the
  @sym{gtk:tree-store-insert-with-values} function should generally be preferred
  when inserting rows in a sorted tree store.
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-store-insert}
  @see-function{gtk:tree-store-set}"
  (let ((n (length values))
        (iter (make-tree-iter)))
    (with-foreign-objects ((v-ar '(:struct g:value) n)
                           (columns-ar :int n))
      (iter (for i from 0 below n)
            (for value in values)
            (for gtype = (tree-model-column-type store i))
            (setf (cffi:mem-aref columns-ar :int i) i)
            (set-g-value (cffi:mem-aptr v-ar '(:struct g:value) i)
                         value
                         gtype
                         :zero-g-value t))
      (%tree-store-insert-with-valuesv store
                                       iter
                                       parent
                                       position
                                       columns-ar
                                       v-ar
                                       n)
      (iter (for i from 0 below n)
            (gobject:value-unset (cffi:mem-aptr v-ar '(:struct g:value) i)))
      iter)))

(export 'tree-store-insert-with-values)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_insert_with_valuesv
;;; ----------------------------------------------------------------------------

;; This function is for internal use and not exported.

(defcfun ("gtk_tree_store_insert_with_valuesv"
          %tree-store-insert-with-valuesv) :void
 #+liber-documentation
 "@version{#2013-3-27}
  @argument[tree_store]{A GtkTreeStore}
  @argument[iter]{An unset GtkTreeIter to set the new row, or NULL.}
  @argument[parent]{A valid GtkTreeIter, or NULL.}
  @argument[position]{position to insert the new row}
  @argument[columns]{an array of column numbers}
  @argument[values]{an array of GValues}
  @argument[n_values]{the length of the columns and values arrays}
  @begin{short}
    A variant of gtk_tree_store_insert_with_values() which takes the columns and
    values as two arrays, instead of varargs. This function is mainly intended
    for language bindings.
  @end{short}
  @see-class{gtk:tree-store}"
  (store (g:object tree-store))
  (iter (g:boxed tree-iter))
  (parent (g:boxed tree-iter))
  (position :int)
  (columns :pointer)
  (values :pointer)
  (n-values :int))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_prepend
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_prepend" %tree-store-prepend) :void
  (tree-store (g:object tree-store))
  (iter (g:boxed tree-iter))
  (parent (g:boxed tree-iter)))

(defun tree-store-prepend (store parent)
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:tree-store} object}
  @argument[parent]{a valid @class{gtk:tree-iter} iterator, or @code{nil}}
  @return{A @class{gtk:tree-iterator} iterator.}
  @begin{short}
    Prepends a new row to @arg{store}.
  @end{short}
  If @arg{parent} is non-@code{nil}, then it will prepend the new row before
  the first child of parent, otherwise it will prepend a row to the toplevel.
  The returned iterator point to this new row. The row will be empty after this
  function is called. To fill in values, you need to call the
  @fun{gtk:tree-store-set} or @fun{gtk:tree-store-set-value} functions.
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-store-set}
  @see-function{gtk:tree-store-set-value}"
  (let ((iter (make-tree-iter)))
    (%tree-store-prepend store iter parent)
    iter))

(export 'tree-store-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_append
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_append" %tree-store-append) :void
  (store (g:object tree-store))
  (iter (g:boxed tree-iter))
  (parent (g:boxed tree-iter)))

(defun tree-store-append (store parent)
 #+liber-documentation
 "@version{2022-11-24}
  @argument[store]{a @class{gtk:tree-store} object}
  @argument[parent]{a valid @class{gtk:tree-iter} iterator, or @code{nil}}
  @return{The @class{gtk:tree-iter} iterator of the appended row.}
  @begin{short}
    Appends a new row to the tree store.
  @end{short}
  If @arg{parent} is non-@code{nil}, then it will append the new row after the
  last child of @arg{parent}, otherwise it will append a row to the toplevel.
  The row will be empty after this function is called. To fill in values, you
  need to call the @fun{gtk:tree-store-set} or @fun{gtk:tree-store-set-value}
  functions.
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-store-set}
  @see-function{gtk:tree-store-set-value}"
  (let ((iter (make-tree-iter)))
    (%tree-store-append store iter parent)
    iter))

(export 'tree-store-append)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_is_ancestor
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_is_ancestor" tree-store-is-ancestor) :boolean
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:tree-store} object}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator}
  @argument[descendant]{a valid @class{gtk:tree-iter} iterator}
  @return{@em{True}, if @arg{iter} is an ancestor of @arg{descendant}.}
  @begin{short}
    Returns @em{true} if @arg{iter} is an ancestor of @arg{descendant}.
  @end{short}
  That is, @arg{iter} is the parent, or grandparent or great-grandparent,
  of @arg{descendant}.
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}"
  (store (g:object tree-store))
  (iter (g:boxed tree-iter))
  (descendant (g:boxed tree-iter)))

(export 'tree-store-is-ancestor)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_iter_depth
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_iter_depth" tree-store-iter-depth) :int
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:tree-store} object}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator}
  @return{The depth of @arg{iter}.}
  @begin{short}
    Returns the depth of @arg{iter}.
  @end{short}
  This will be 0 for anything on the root level, 1 for anything down a level,
  etc.
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}"
  (store (g:object tree-store))
  (iter (g:boxed tree-iter)))

(export 'tree-store-iter-depth)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_clear
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_clear" tree-store-clear) :void
 #+liber-documentation
 "@version{#2021-3-5}
  @argument[store]{a @class{gtk:tree-store} object}
  @short{Removes all rows from the tree store.}
  @see-class{gtk:tree-store}"
  (store (g:object tree-store)))

(export 'tree-store-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_iter_is_valid
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_iter_is_valid" tree-store-iter-is-valid) :boolean
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:tree-store} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator}
  @return{@em{True} if @arg{iter} is valid, @code{nil} if @arg{iter} is
    invalid.}
  @begin{short}
    Checks if the given @arg{iter} is a valid iterator for this
    @class{gtk:tree-store} object.
  @end{short}
  @begin[Warning]{dictionary}
    This function is slow. Only use it for debugging and/or testing purposes.
  @end{dictionary}
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}"
  (store (g:object tree-store))
  (iter (g:boxed tree-iter)))

(export 'tree-store-iter-is-valid)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_reorder
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_reorder" %tree-store-reorder) :void
  (store (g:object list-store))
  (parent (g:boxed tree-iter))
  (order :pointer))

(defun tree-store-reorder (store parent order)
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:list-store} object}
  @argument[parent]{a @class{gtk:tree-iter} iterator}
  @argument[order]{a list of integer mapping the new position of each child row
    to its old position before the re-ordering}
  @begin{short}
    Reorders the children of @arg{parent} in @arg{store} to follow the order
    indicated by @arg{order}.
  @end{short}
  Note that this function only works with unsorted stores.
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}"
  (let ((n (length order)))
    (with-foreign-object (order-ar :int n)
      (iter (for i from 0 below n)
            (for j in order)
            (setf (cffi:mem-aref order-ar :int i) j))
      (%tree-store-reorder store parent order-ar))))

(export 'tree-store-reorder)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_swap
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_swap" tree-store-swap) :void
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:tree-store} object}
  @argument[a]{a @class{gtk:tree-iter} iterator}
  @argument[b]{another @class{gtk:tree-iter} iterator}
  @begin{short}
    Swaps @arg{a} and @arg{b} in the same level of @arg{store}.
  @end{short}
  Note that this function only works with unsorted stores.
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}"
  (store (g:object tree-store))
  (a (g:boxed tree-iter))
  (b (g:boxed tree-iter)))

(export 'tree-store-swap)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_move_before
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_move_before" tree-store-move-before) :void
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:tree-store}}
  @argument[iter]{a @class{gtk:tree-iter} iterator}
  @argument[position]{a @class{gtk:tree-iter} iterator or @code{nil}}
  @begin{short}
    Moves @arg{iter} in @arg{store} to the position before @arg{position}.
  @end{short}
  @arg{iter} and @arg{position} should be in the same level. Note that this
  function only works with unsorted stores. If @arg{position} is @code{nil},
  @arg{iter} will be moved to the end of the level.
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-store-move-after}"
  (store (g:object tree-store))
  (iter (g:boxed tree-iter))
  (position (g:boxed tree-iter)))

(export 'tree-store-move-before)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_move_after
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_move_after" tree-store-move-after) :void
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:tree-store} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator}
  @argument[position]{a @class{gtk:tree-iter} iterator}
  @begin{short}
    Moves @arg{iter} in @arg{store} to the position after @arg{position}.
  @end{short}
  @arg{iter} and @arg{position} should be in the same level. Note that this
  function only works with unsorted stores. If @arg{position} is @code{nil},
  @arg{iter} will be moved to the start of the level.
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-store-move-before}"
  (store (g:object tree-store))
  (iter (g:boxed tree-iter))
  (position (g:boxed tree-iter)))

(export 'tree-store-move-after)

;;; --- End of file gtk4.tree-store.lisp ---------------------------------------
