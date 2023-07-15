;;; ----------------------------------------------------------------------------
;;; gtk4.list-store.lisp
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
;;; GtkListStore
;;;
;;;     A list-like data structure that can be used with the GtkTreeView
;;;
;;; Types and Values
;;;
;;;     GtkListStore
;;;
;;; Functions
;;;
;;;     gtk_list_store_new
;;;     gtk_list_store_newv
;;;     gtk_list_store_set_column_types
;;;     gtk_list_store_set
;;;     gtk_list_store_set_valist
;;;     gtk_list_store_set_value
;;;     gtk_list_store_set_valuesv
;;;     gtk_list_store_remove
;;;     gtk_list_store_insert
;;;     gtk_list_store_insert_before
;;;     gtk_list_store_insert_after
;;;     gtk_list_store_insert_with_values
;;;     gtk_list_store_insert_with_valuesv
;;;     gtk_list_store_prepend
;;;     gtk_list_store_append
;;;     gtk_list_store_clear
;;;     gtk_list_store_iter_is_valid
;;;     gtk_list_store_reorder
;;;     gtk_list_store_swap
;;;     gtk_list_store_move_before
;;;     gtk_list_store_move_after
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkListStore
;;;
;;; Implemented Interfaces
;;;
;;;     GtkTreeModel
;;;     GtkTreeDragSource
;;;     GtkTreeDragDest
;;;     GtkTreeSortable
;;;     GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkListStore
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkListStore" list-store
  (:superclass g:object
   :export t
   :interfaces ("GtkBuildable"
                "GtkTreeModel"
                "GtkTreeDragSource"
                "GtkTreeDragDest"
                "GtkTreeSortable")
   :type-initializer "gtk_list_store_get_type")
  nil)

#+liber-documentation
(setf (documentation 'list-store 'type)
 "@version{#2022-9-10}
  @begin{short}
    The @sym{gtk:list-store} object is a list model for use with a
    @class{gtk:tree-view} widget.
  @end{short}
  It implements the @class{gtk:tree-model} interface, and consequentialy, can
  use all of the methods available there. It also implements the
  @class{gtk:tree-sortable} interface so it can be sorted by the tree view.
  Finally, it also implements the tree drag and drop interfaces.

  The @sym{gtk:list-store} object can accept most GObject types as a column
  type, though it cannot accept all custom types. Internally, it will keep a
  copy of data passed in, such as a string or a boxed pointer. Columns that
  accept GObjects are handled a little differently. The @sym{gtk:list-store}
  object will keep a reference to the object instead of copying the value. As a
  result, if the object is modified, it is up to the application writer to call
  the function @fun{gtk:tree-model-row-changed} to emit the \"row-changed\"
  signal. This most commonly affects lists with @class{gdk-texture} objects
  stored.

  @subheading{Performance Considerations}
    Internally, the @sym{gtk:list-store} object was implemented with a linked
    list with a tail pointer prior to GTK 2.6. As a result, it was fast at
    data insertion and deletion, and not fast at random data access. The
    @sym{gtk:list-store} object sets the @code{:iters-persist} flag of the
    @symbol{gtk:tree-model-flags} flags, which means that @class{gtk:tree-iter}
    iterators can be cached while the row exists. Thus, if access to a
    particular row is needed often and your code is expected to run on older
    versions of GTK, it is worth keeping the iterator around.

  @subheading{Atomic Operations}
    It is important to note that only the method
    @fun{gtk:list-store-insert-with-values} is atomic, in the sense that the
    row is being appended to the store and the values filled in in a single
    operation with regard to the @class{gtk:tree-model} interface signaling. In
    contrast, using e.g. the functions @fun{gtk:list-store-append} and then
    @fun{gtk:list-store-set} will first create a row, which triggers the
    \"row-inserted\" signal on the @sym{gtk:list-store} object. The row,
    however, is still empty, and any signal handler connecting to
    \"row-inserted\" on this particular store should be prepared for the
    situation that the row might be empty. This is especially important if you
    are wrapping the @sym{gtk:list-store} object inside a
    @class{gtk:tree-model-filter} object and are using a callback function
    @symbol{gtk:tree-model-filter-visible-func}. Using any of the non-atomic
    operations to append rows to the @sym{gtk:list-store} object will cause the
    callback function @symbol{gtk:tree-model-filter-visible-func} to be visited
    with an empty row first. The function must be prepared for that.
  @begin[Example]{dictionary}
    Creating a simple list store.
    @begin{pre}
(defun create-and-fill-model ()
  (let ((list-data '(\"Name1\" \"Name2\" \"Name3\" \"Name4\" \"Name5\"))
        ;; Create a new list store with three columns
        (list-store (make-instance 'gtk:list-store
                                   :column-types
                                   '(\"gint\" \"gchararray\" \"gboolean\"))))
    ;; Fill in some data
    (loop for data in list-data
          for i from 0 do
          ;; Add a new row to the model
          (gtk:list-store-set list-store
                              (gtk:list-store-append list-store)
                              i
                              data
                              nil))
    ;; Modify a particular row
    (let ((path (gtk:tree-path-new-from-string \"2\")))
      (gtk:list-store-set-value list-store
                                (gtk:tree-model-iter list-store path)
                                2
                                t))
    ;; Return the new list store
    list-store))
    @end{pre}
  @end{dictionary}
  @begin[GtkListStore as GtkBuildable]{dictionary}
    The @sym{gtk:list-store} implementation of the @class{gtk:buildable}
    interface allows to specify the model columns with a @code{<columns>}
    element that may contain multiple @code{<column>} elements, each specifying
    one model column. The @code{type} attribute specifies the data type for the
    column.

    Additionally, it is possible to specify content for the list store in the
    UI definition, with the @code{<data>} element. It can contain multiple
    @code{<row>} elements, each specifying to content for one row of the list
    model. Inside a @code{<row>}, the @code{<col>} elements specify the content
    for individual cells.

    Note that it is probably more common to define your models in the code, and
    one might consider it a layering violation to specify the content of a list
    store in a UI definition, data, not presentation, and common wisdom is to
    separate the two, as far as possible.

    @b{Example:} A UI Definition fragment for a list store
    @begin{pre}
   <object class=\"GtkListStore\">
     <columns>
       <column type=\"gchararray\"/>
       <column type=\"gchararray\"/>
       <column type=\"gint\"/>
     </columns>
     <data>
       <row>
         <col id=\"0\">John</col>
         <col id=\"1\">Doe</col>
         <col id=\"2\">25</col>
       </row>
       <row>
         <col id=\"0\">Johan</col>
         <col id=\"1\">Dahlin</col>
         <col id=\"2\">50</col>
       </row>
     </data>
   </object>
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:list-store-new}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-sortable}
  @see-class{gtk:tree-model-filter}
  @see-class{gtk:buildable}
  @see-class{gdk-texture}
  @see-symbol{gtk:tree-model-flags}")

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_new ()
;;; ----------------------------------------------------------------------------

(defmethod initialize-instance :after
    ((store list-store)
     &rest initargs
     &key (column-types nil column-types-p)
     &allow-other-keys)
  (declare (ignore initargs))
  (when column-types-p
    (apply #'list-store-set-column-types store column-types)))

;;; ----------------------------------------------------------------------------

(declaim (inline list-store-new))

(defun list-store-new (&rest column-types)
 #+liber-documentation
 "@version{#2021-4-21}
  @argument[column-types]{all @class{g:type-t} types for the columns, from first
    to last}
  @return{A new @class{gtk:list-store} object.}
  @begin{short}
    Creates a new list store as with each of the types passed in.
  @end{short}
  Note that only types derived from standard GType fundamental types are
  supported.
  @begin[Example]{dictionary}
    The following example creates a new @class{gtk:list-store} object with
    three columnes, of type \"gint\", \"gchararray\" and \"GdkPixbuf\".
    @begin{pre}
(gtk:list-store-new \"gint\" \"gchararray\" \"GdkPixbuf\")
    @end{pre}
    Note that in the Lisp binding a second implementation is
    @begin{pre}
(make-instance 'gtk:list-store
               :column-types '(\"gint\" \"gchararray\" \"GdkPixbuf\"))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{g:type-t}"
  (make-instance 'list-store
                 :column-types column-types))

(export 'list-store-new)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_newv ()
;;;
;;; GtkListStore *
;;; gtk_list_store_newv (int n_columns,
;;;                      GType *types);
;;;
;;; Non-vararg creation function. Used primarily by language bindings.
;;;
;;; n_columns :
;;;     number of columns in the list store
;;;
;;; types :
;;;     an array of GType types for the columns, from first to last.
;;;
;;; Returns :
;;;     a new GtkListStore.
;;; ----------------------------------------------------------------------------

;; Implementation not needed

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set_column_types ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_set_column_types" %list-store-set-column-types)
    :void
  (store (g:object list-store))
  (n-columns :int)
  (types :pointer))

(defun list-store-set-column-types (store &rest column-types)
 #+liber-documentation
 "@version{#2021-4-21}
  @argument[store]{a @class{gtk:list-store} object}
  @argument[column-types]{the @class{g:type-t} types of the columns}
  @begin{short}
    This function is meant primarily for GObjects that inherit from the
    @class{gtk:list-store} class, and should only be used when constructing a
    new @class{gtk:list-store} object.
  @end{short}
  It will not function after a row has been added, or a method on a
  @class{gtk:tree-model} object is called.
  @begin[Example]{dictionary}
    Create a list store and set the column types:
    @begin{pre}
(let ((store (gtk:list-store-new)))
  (gtk:list-store-set-column-types store \"gint\" \"gchararray\" \"GdkPixbuf\")
  ... )
    @end{pre}
    This is equivalent to:
    @begin{pre}
(let ((store (gtk:list-store-new \"gint\" \"gchararray\" \"GdkPixbuf\")))
   ... )
    @end{pre}
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{g:type-t}
  @see-class{gtk:tree-model}
  @see-function{gtk:list-store-new}"
  (let ((n (length column-types)))
    (cffi:with-foreign-object (types-ar 'g:type-t n)
      (loop for i from 0 below n
            for gtype in column-types
         do (setf (cffi:mem-aref types-ar 'g:type-t i) gtype))
      (%list-store-set-column-types store n types-ar))))

(export 'list-store-set-column-types)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set ()
;;; ----------------------------------------------------------------------------

;; The Lisp implementation does not support pairs of an index and a value.
;; Consider to change the implemenation.

(defun list-store-set (store iter &rest values)
 #+liber-documentation
 "@version{#2022-9-10}
  @argument[store]{a @class{gtk:list-store} object}
  @argument[iter]{a @class{gtk:tree-iter} row iterator}
  @argument[values]{the values to set}
  @return{The @class{gtk:tree-iter} iterator for the row being modified.}
  @begin{short}
    Sets the values of one or more cells in the row referenced by @arg{iter}.
  @end{short}
  The variable argument list should contain the values to be set.
  @begin[Example]{dictionary}
    @begin{pre}
(let ((model (gtk:list-store-new \"gchararray\" \"gchararray\" \"guint\")))
  ;; Append a row and fill in some data
  (gtk:list-store-set model
                      (gtk:list-store-append model)
                      \"Hans\" \"Müller\" 1961)
   ... )
    @end{pre}
  @end{dictionary}
  @begin[Note]{dictionary}
    The Lisp implemenation does not support pairs of a column index and a
    value, but a list of values. Therefore, it is not possible to set the values
    of individual columns. See the @fun{gtk:list-store-set-value} function for
    setting the value of single columns.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-set-value}"
  (let ((n (length values)))
    (cffi:with-foreign-objects ((value-ar '(:struct g:value) n)
                                (columns-ar :int n))
      (loop for i from 0 below n
            for value in values
            for gtype = (tree-model-column-type store i)
         do (setf (cffi:mem-aref columns-ar :int i) i)
            (gobject:set-g-value (cffi:mem-aptr value-ar '(:struct g:value) i)
                                 value
                                 gtype
                                 :zero-gvalue t))
      (%list-store-set-valuesv store iter columns-ar value-ar n)
      (loop for i from 0 below n
         do (gobject:value-unset (cffi:mem-aptr value-ar '(:struct g:value) i)))
      iter)))

(export 'list-store-set)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set_valist ()
;;;
;;; void
;;; gtk_list_store_set_valist (GtkListStore *list_store,
;;;                            GtkTreeIter *iter,
;;;                            va_list var_args);
;;;
;;; See gtk_list_store_set(); this version takes a va_list for use by language
;;; bindings.
;;;
;;; list_store :
;;;     A GtkListStore
;;;
;;; iter :
;;;     A valid GtkTreeIter for the row being modified
;;;
;;; var_args :
;;;     va_list of column/value pairs
;;; ----------------------------------------------------------------------------

;; Implementation not needed

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set_value ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_set_value" %list-store-set-value) :void
  (store (g:object list-store))
  (iter (g:boxed tree-iter))
  (column :int)
  (value :pointer))

(defun list-store-set-value (store iter column value)
 #+liber-documentation
 "@version{#2021-3-3}
  @argument[store]{a @class{gtk:list-store} object}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator for the row being
    modified}
  @argument[column]{an integer with the column number to modify}
  @argument[value]{new value for the cell}
  @begin{short}
    Sets the data in the cell specified by @arg{iter} and @arg{column}.
  @end{short}
  The type of @arg{value} must be convertible to the type of the @arg{column}.
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-set}"
  (cffi:with-foreign-object (gvalue '(:struct g:value))
    (gobject:set-g-value gvalue
                         value
                         (tree-model-column-type store column)
                         :zero-gvalue t)
    (%list-store-set-value store iter column gvalue)
    (gobject:value-unset gvalue)
    (values)))

(export 'list-store-set-value)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set_valuesv ()
;;;
;;; void
;;; gtk_list_store_set_valuesv (GtkListStore *list_store,
;;;                             GtkTreeIter *iter,
;;;                             int *columns,
;;;                             GValue *values,
;;;                             int n_values);
;;;
;;; A variant of gtk_list_store_set_valist() which takes the columns and values
;;; as two arrays, instead of varargs. This function is mainly intended for
;;; language-bindings and in case the number of columns to change is not known
;;; until run-time.
;;;
;;; list_store :
;;;     A GtkListStore
;;;
;;; iter :
;;;     A valid GtkTreeIter for the row being modified
;;;
;;; columns :
;;;     an array of column numbers.
;;;
;;; values :
;;;     an array of GValues.
;;;
;;; n_values :
;;;     the length of the columns and values arrays
;;; ----------------------------------------------------------------------------

;; Only for internal use. Not exported.

(defcfun ("gtk_list_store_set_valuesv" %list-store-set-valuesv) :void
  (store (g:object list-store))
  (iter (g:boxed tree-iter))
  (columns :pointer)
  (values :pointer)
  (n-values :int))

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_remove" list-store-remove) :boolean
 #+liber-documentation
 "@version{#2021-3-5}
  @argument[store]{a @class{gtk:list-store} object}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator}
  @return{@em{True} if @arg{iter} is valid, @code{nil} if not.}
  @begin{short}
    Removes the given row from the list store.
  @end{short}
  After being removed, @arg{iter} is set to be the next valid row, or
  invalidated if it pointed to the last row in the list store.
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-insert}"
  (store (g:object list-store))
  (iter (g:boxed tree-iter)))

(export 'list-store-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_insert" %list-store-insert) :void
  (store (g:object list-store))
  (iter (g:boxed tree-iter))
  (position :int))

(defun list-store-insert (store position)
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:list-store} object}
  @argument[position]{an integer with the position to insert the new row}
  @return{A @class{gtk:tree-iter} iterator of the new row.}
  @begin{short}
    Creates a new row at @arg{position}.
  @end{short}
  The returned iterator will point to this new row. If @arg{position} is larger
  than the number of rows on the list, then the new row will be appended to the
  list. The row will be empty after this function is called. To fill in values,
  you need to call the functions @fun{gtk:list-store-set} or
  @fun{gtk:list-store-set-value}.
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-set}
  @see-function{gtk:list-store-set-value}"
  (let ((iter (make-tree-iter)))
    (%list-store-insert store iter position)
    iter))

(export 'list-store-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_before ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_insert_before" %list-store-insert-before) :void
  (store (g:object list-store))
  (iter (g:boxed tree-iter))
  (sibling (g:boxed tree-iter)))

(defun list-store-insert-before (store sibling)
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:list-store} object}
  @argument[sibling]{a valid @class{gtk:tree-iter} iterator, or @code{nil}}
  @return{A @class{gtk:tree-iter} iterator to the new row.}
  @begin{short}
    Inserts a new row before @arg{sibling}.
  @end{short}
  If @arg{sibling} is @code{nil}, then the row will be appended to the end of
  the list. The returned iterator will point to this new row. The row will be
  empty after this function is called. To fill in values, you need to call the
  functions @fun{gtk:list-store-set} or @fun{gtk:list-store-set-value}.
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-set}
  @see-function{gtk:list-store-set-value}"
  (let ((iter (make-tree-iter)))
    (%list-store-insert-before store iter sibling)
    iter))

(export 'list-store-insert-before)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_after ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_insert_after" %list-store-insert-after) :void
  (store (g:object list-store))
  (iter (g:boxed tree-iter))
  (sibling (g:boxed tree-iter)))

(defun list-store-insert-after (store sibling)
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:list-store} object}
  @argument[sibling]{a valid @class{gtk:tree-iter}, or @code{nil}}
  @return{A @class{gtk:tree-iter} iterator to the new row.}
  @begin{short}
    Inserts a new row after @arg{sibling}.
  @end{short}
  If @arg{sibling} is @code{nil}, then the row will be prepended to the
  beginning of the list. The returned iterator will point to this new row. The
  row will be empty after this function is called. To fill in values, you need
  to call the functions @fun{gtk:list-store-set} or
  @fun{gtk:list-store-set-value}.
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-set}
  @see-function{gtk:list-store-set-value}"
  (let ((iter (make-tree-iter)))
    (%list-store-insert-after store iter sibling)
    iter))

(export 'list-store-insert-after)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_with_values ()
;;; ----------------------------------------------------------------------------

(defun list-store-insert-with-values (store position &rest values)
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:list-store} object}
  @argument[position]{an integer with the position to insert the new row,
    or -1 to append after existing rows}
  @argument[values]{values to store in @arg{store}}
  @return{A @class{gtk:tree-iter} iterator to the new row.}
  @begin{short}
    Creates a new row at @arg{position}.
  @end{short}
  The returned iterator will point to this new row. If @arg{position} is -1, or
  larger than the number of rows in the list, then the new row will be appended
  to the list. The row will be filled with the values given to this function.

  Calling the function @sym{gtk:list-store-insert-with-values} has the same
  effect as calling
  @begin{pre}
 (let ((iter (gtk:list-store-insert list-store position)))
   (gtk:list-store-set list-store iter  ...)
 )
  @end{pre}
  with the difference that the former will only emit a \"row-inserted\" signal,
  while the latter will emit \"row-inserted\", \"row-changed\" and, if the list
  store is sorted, \"rows-reordered\" signals. Since emitting the
  \"rows-reordered\" signal repeatedly can affect the performance of the
  program, the function @sym{gtk:list-store-insert-with-values} should generally
  be preferred when inserting rows in a sorted list store.
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-insert}
  @see-function{gtk:list-store-set}"
  (let ((n (length values))
        (iter (make-tree-iter)))
    (cffi:with-foreign-objects ((value-ar '(:struct g:value) n)
                                (columns-ar :int n))
      (iter (for i from 0 below n)
            (for value in values)
            (for gtype = (tree-model-column-type store i))
            (setf (cffi:mem-aref columns-ar :int i) i)
            (gobject:set-g-value (cffi:mem-aptr value-ar '(:struct g:value) i)
                                 value
                                 gtype
                                 :zero-gvalue t))
      (%list-store-insert-with-valuesv store
                                       iter
                                       position
                                       columns-ar
                                       value-ar
                                       n)
      (iter (for i from 0 below n)
            (gobject:value-unset (cffi:mem-aptr value-ar '(:struct g:value) i)))
      iter)))

(export 'list-store-insert-with-values)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_with_valuesv ()
;;;
;;; void
;;; gtk_list_store_insert_with_valuesv (GtkListStore *list_store,
;;;                                     GtkTreeIter *iter,
;;;                                     int position,
;;;                                     int *columns,
;;;                                     GValue *values,
;;;                                     int n_values);
;;;
;;; A variant of gtk_list_store_insert_with_values() which takes the columns and
;;; values as two arrays, instead of varargs. This function is mainly intended
;;; for language-bindings.
;;;
;;; list_store :
;;;     A GtkListStore
;;;
;;; iter :
;;;     An unset GtkTreeIter to set to the new row, or NULL.
;;;
;;; position :
;;;     position to insert the new row, or -1 for last
;;;
;;; columns :
;;;     an array of column numbers.
;;;
;;; values :
;;;     an array of GValues.
;;;
;;; n_values :
;;;     the length of the columns and values arrays
;;; ----------------------------------------------------------------------------

;; Only for internal use. Not exported.

(defcfun ("gtk_list_store_insert_with_valuesv"
          %list-store-insert-with-valuesv) :void
  (store (g:object list-store))
  (iter (g:boxed tree-iter))
  (position :int)
  (columns :pointer)
  (values :pointer)
  (n-values :int))

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_prepend ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_prepend" %list-store-prepend) :void
  (store (g:object list-store))
  (iter (g:boxed tree-iter)))

(defun list-store-prepend (store)
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:list-store} object}
  @return{A @class{gtk:tree-iter} iterator to the prepended row.}
  @begin{short}
    Prepends a new row to @arg{store}.
  @end{short}
  The returned iterator will point to this new row. The row will be empty after
  this function is called. To fill in values, you need to call the functions
  @fun{gtk:list-store-set} or @fun{gtk:list-store-set-value}.
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-set}
  @see-function{gtk:list-store-set-value}"
  (let ((iter (make-tree-iter)))
    (%list-store-prepend store iter)
    iter))

(export 'list-store-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_append ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_append" %list-store-append) :void
  (store (g:object list-store))
  (iter (g:boxed tree-iter)))

(defun list-store-append (store)
 #+liber-documentation
 "@version{#2022-9-10}
  @argument[store]{a @class{gtk:list-store} object}
  @return{A @class{gtk:tree-iter} iterator to the appended row.}
  @begin{short}
    Appends a new row to the list store.
  @end{short}
  The returned iterator will point to the new row. The row will be empty after
  this function is called. To fill in values, you need to call the
  @fun{gtk:list-store-set} or @fun{gtk:list-store-set-value} functions.
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-set}
  @see-function{gtk:list-store-set-value}"
  (let ((iter (make-tree-iter)))
    (%list-store-append store iter)
    iter))

(export 'list-store-append)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_clear ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_clear" list-store-clear) :void
 #+liber-documentation
 "@version{#2021-3-5}
  @argument[store]{a @class{gtk:list-store} object}
  @short{Removes all rows from the list store.}
  @see-class{gtk:list-store}"
  (store (g:object list-store)))

(export 'list-store-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_iter_is_valid ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_iter_is_valid" list-store-iter-is-valid) :boolean
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:list-store} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator}
  @return{@em{True} if @arg{iter} is valid, @code{nil} if @arg{iter} is
    invalid.}
  @begin{short}
    Checks if the given @arg{iter} is a valid iterator for this
    @class{gtk:list-store}.
  @end{short}
  @begin[Warning]{dictionary}
    This function is slow. Only use it for debugging and/or testing purposes.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}"
  (store (g:object list-store))
  (iter (g:boxed tree-iter)))

(export 'list-store-iter-is-valid)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_reorder ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_reorder" %list-store-reorder) :void
  (store (g:object list-store))
  (order :pointer))

(defun list-store-reorder (store order)
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:list-store} object}
  @argument[order]{a list of integer mapping the new position of each row
    to its old position before the re-ordering}
  @begin{short}
    Reorders @arg{store} to follow the order indicated by @arg{order}.
  @end{short}
  Note that this function only works with unsorted stores.
  @see-class{gtk:list-store}"
  (let ((n (length order)))
    (cffi:with-foreign-object (order-ar :int n)
      (iter (for i from 0 below n)
            (for j in order)
            (setf (cffi:mem-aref order-ar :int i) j))
      (%list-store-reorder store order-ar))))

(export 'list-store-reorder)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_swap ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_swap" list-store-swap) :void
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:list-store} object}
  @argument[a]{a @class{gtk:tree-iter} iterator}
  @argument[b]{a @class{gtk:tree-iter} iterator}
  @begin{short}
    Swaps @arg{a} and @arg{b} in @arg{store}.
  @end{short}
  Note that this function only works with unsorted stores.
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}"
  (store (g:object list-store))
  (a (g:boxed tree-iter))
  (b (g:boxed tree-iter)))

(export 'list-store-swap)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_move_before ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_move_before" list-store-move-before) :void
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:list-store} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator}
  @argument[position]{a @class{gtk:tree-iter} iterator, or @code{nil}}
  @begin{short}
    Moves @arg{iter} in @arg{store} to the position before @arg{position}.
  @end{short}
  Note that this function only works with unsorted stores. If @arg{position} is
  @code{nil}, @arg{iter} will be moved to the end of the list.
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-move-after}"
  (store (g:object list-store))
  (iter (g:boxed tree-iter))
  (position (g:boxed tree-iter)))

(export 'list-store-move-before)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_move_after ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_move_after" list-store-move-after) :void
 #+liber-documentation
 "@version{#2021-3-11}
  @argument[store]{a @class{gtk:list-store} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator}
  @argument[position]{a @class{gtk:tree-iter} iterator or @code{nil}}
  @begin{short}
    Moves @arg{iter} in @arg{store} to the position after @arg{position}.
  @end{short}
  Note that this function only works with unsorted stores. If @arg{position} is
  @code{nil}, @arg{iter} will be moved to the start of the list.
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-move-before}"
  (store (g:object list-store))
  (iter (g:boxed tree-iter))
  (position (g:boxed tree-iter)))

(export 'list-store-move-after)

;;; --- End of file gtk4.list-store-lisp ---------------------------------------
