;;; ----------------------------------------------------------------------------
;;; gtk4.list-store.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.12 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;;     gtk_list_store_newv                                 not needed
;;;     gtk_list_store_set_column_types
;;;     gtk_list_store_set
;;;     gtk_list_store_set_valist
;;;     gtk_list_store_set_value
;;;     gtk_list_store_set_valuesv                          not needed
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
;;; GtkListStore
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkListStore" list-store
  (:superclass g:object
   :export t
   :interfaces ("GtkBuildable"
                "GtkTreeModel"
                "GtkTreeDragSource"
                "GtkTreeDragDest"
                "GtkTreeSortable")
   :type-initializer "gtk_list_store_get_type")
  nil)

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj list-store) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:LIST-STORE is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'list-store 'type)
 "@version{2025-07-23}
  @begin{short}
    The @class{gtk:list-store} object is a list model for use with a
    @class{gtk:tree-view} widget.
  @end{short}
  It implements the @class{gtk:tree-model} interface, and consequentialy, can
  use all of the methods available there. It also implements the
  @class{gtk:tree-sortable} interface so it can be sorted by the tree view.
  Finally, it also implements the tree drag and drop interfaces.

  The @class{gtk:list-store} object can accept most GObject types as a column
  type, though it cannot accept all custom types. Internally, it will keep a
  copy of data passed in, such as a string or a boxed pointer. Columns that
  accept GObjects are handled a little differently. The @class{gtk:list-store}
  object will keep a reference to the object instead of copying the value. As a
  result, if the object is modified, it is up to the application writer to call
  the @fun{gtk:tree-model-row-changed} function to emit the
  @sig[gtk:tree-model]{row-changed} signal. This most commonly affects lists
  with @class{gdk:texture} objects stored.

  @subheading{Performance Considerations}
    Internally, the @class{gtk:list-store} object was implemented with a linked
    list with a tail pointer prior to GTK 2.6. As a result, it was fast at
    data insertion and deletion, and not fast at random data access. The
    @class{gtk:list-store} object sets the
    @val[gtk:tree-model-flags]{:iters-persist} flag of the
    @sym{gtk:tree-model-flags} flags, which means that @class{gtk:tree-iter}
    iterators can be cached while the row exists. Thus, if access to a
    particular row is needed often and your code is expected to run on older
    versions of GTK, it is worth keeping the iterator around.

  @subheading{Atomic Operations}
    It is important to note that only the
    @fun{gtk:list-store-insert-with-values} method is atomic, in the sense that
    the row is being appended to the store and the values filled in in a single
    operation with regard to the @class{gtk:tree-model} interface signaling. In
    contrast, using, for example, the @fun{gtk:list-store-append} function and
    then the @fun{gtk:list-store-set} function will first create a row, which
    triggers the @sig[gtk:tree-model]{row-inserted} signal on the
    @class{gtk:list-store} object. The row, however, is still empty, and any
    signal handler connecting to the @sig[gtk:tree-model]{row-inserted} signal
    on this particular store should be prepared for the situation that the row
    might be empty. This is especially important if you are wrapping the
    @class{gtk:list-store} object inside a @class{gtk:tree-model-filter} object
    and are using a @sym{gtk:tree-model-filter-visible-func} callback function.
    Using any of the non-atomic operations to append rows to the
    @class{gtk:list-store} object will cause the
    @sym{gtk:tree-model-filter-visible-func} callback function to be visited
    with an empty row first. The function must be prepared for that.
  @begin[Examples]{dictionary}
    Creating a simple list store.
    @begin{pre}
(defun create-and-fill-model ()
  (let ((listdata '(\"Name1\" \"Name2\" \"Name3\" \"Name4\" \"Name5\"))
        ;; Create new list store with three columns
        (store (make-instance 'gtk:list-store
                              :column-types
                              '(\"gint\" \"gchararray\" \"gboolean\"))))
    ;; Fill in some data
    (iter (for data in listdata)
          (for i from 0)
          ;; Add a new row to the model
          (gtk:list-store-set store
                              (gtk:list-store-append store)
                              i
                              data
                              nil))
    ;; Modify particular row
    (let ((path (gtk:tree-path-new-from-string \"2\")))
      (gtk:list-store-set-value store
                                (gtk:tree-model-iter store path)
                                2
                                t))
    ;; Return new list store
    store))
    @end{pre}
  @end{dictionary}
  @begin[GtkListStore as GtkBuildable]{dictionary}
    The @class{gtk:list-store} implementation of the @class{gtk:buildable}
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
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-constructor{gtk:list-store-new}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-sortable}
  @see-class{gtk:tree-model-filter}
  @see-class{gtk:buildable}
  @see-class{gdk:texture}
  @see-symbol{gtk:tree-model-flags}")

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_new
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
 "@version{2024-04-07}
  @argument[column-types]{@class{g:type-t} type IDs for the columns, from first
    to last}
  @return{The new @class{gtk:list-store} object.}
  @begin{short}
    Creates a new list store as with each of the types passed in.
  @end{short}
  Note that only types derived from standard @class{g:type-t} fundamental type
  IDs are supported.
  @begin[Examples]{dictionary}
    The following example creates a new @class{gtk:list-store} object with
    three columnes, of type @code{\"gint\"}, @code{\"gchararray\"} and
    @code{\"GdkPixbuf\"}.
    @begin{pre}
(gtk:list-store-new \"gint\" \"gchararray\" \"GdkPixbuf\")
    @end{pre}
    Note that in the Lisp binding a second implementation is
    @begin{pre}
(make-instance 'gtk:list-store
               :column-types '(\"gint\" \"gchararray\" \"GdkPixbuf\"))
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{g:type-t}
  @see-class{g:list-store}"
  (make-instance 'list-store
                 :column-types column-types))

(export 'list-store-new)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_newv                                     not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set_column_types                         not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_store_set_column_types" %list-store-set-column-types)
    :void
  (store (g:object list-store))
  (n-columns :int)
  (types :pointer))

(defun list-store-set-column-types (store &rest column-types)
 #+liber-documentation
 "@version{#2024-04-07}
  @argument[store]{a @class{gtk:list-store} object}
  @argument[column-types]{@class{g:type-t} type IDs of the columns}
  @begin{short}
    This function is meant primarily for GObjects that inherit from the
    @class{gtk:list-store} class, and should only be used when constructing a
    new @class{gtk:list-store} object.
  @end{short}
  It will not function after a row has been added, or a method on a
  @class{gtk:tree-model} object is called.
  @begin[Examples]{dictionary}
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
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{g:type-t}
  @see-class{gtk:tree-model}
  @see-function{gtk:list-store-new}"
  (let ((n (length column-types)))
    (cffi:with-foreign-object (types-ar 'g:type-t n)
      (iter (for i from 0 below n)
            (for gtype in column-types)
            (setf (cffi:mem-aref types-ar 'g:type-t i) gtype))
      (%list-store-set-column-types store n types-ar))))

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set_valist                               not used
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set
;;; ----------------------------------------------------------------------------

;; The Lisp implementation does not support pairs of an index and a value.
;; Consider to change the implementation.

(defun list-store-set (model iter &rest values)
 #+liber-documentation
 "@version{2024-04-07}
  @argument[model]{a @class{gtk:list-store} object}
  @argument[iter]{a @class{gtk:tree-iter} row iterator}
  @argument[values]{values to set}
  @return{The @class{gtk:tree-iter} iterator for the row being modified.}
  @begin{short}
    Sets the values of one or more cells in the row referenced by @arg{iter}.
  @end{short}
  The variable argument list should contain the values to be set.
  @begin[Examples]{dictionary}
    @begin{pre}
(let ((model (gtk:list-store-new \"gchararray\" \"gchararray\" \"guint\")))
  ;; Append a row and fill in some data
  (gtk:list-store-set model
                      (gtk:list-store-append model)
                      \"Hans\" \"Müller\" 1961)
   ... )
    @end{pre}
  @end{dictionary}
  @begin[Notes]{dictionary}
    The Lisp implementation does not support pairs of a column index and a
    value, but a list of values. Therefore, it is not possible to set the values
    of individual columns. See the @fun{gtk:list-store-set-value} function for
    setting the value of single columns.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-set-value}"
  (let ((n (length values)))
    (cffi:with-foreign-objects ((value-ar '(:struct g:value) n)
                                (columns-ar :int n))
      (iter (for i from 0 below n)
            (for value in values)
            (for gtype = (tree-model-column-type model i))
            (setf (cffi:mem-aref columns-ar :int i) i)
            (gobject:set-gvalue (cffi:mem-aptr value-ar '(:struct g:value) i)
                                value
                                gtype))
      (%list-store-set-valuesv model iter columns-ar value-ar n)
      (iter (for i from 0 below n)
            (gobject:value-unset (cffi:mem-aptr value-ar '(:struct g:value) i)))
      iter)))

(export 'list-store-set)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set_valuesv                              not exported
;;; ----------------------------------------------------------------------------

;; Only for internal use. Not exported.

(cffi:defcfun ("gtk_list_store_set_valuesv" %list-store-set-valuesv) :void
  (store (g:object list-store))
  (iter (g:boxed tree-iter))
  (columns :pointer)
  (values :pointer)
  (n-values :int))

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_store_set_value" %list-store-set-value) :void
  (model (g:object list-store))
  (iter (g:boxed tree-iter))
  (column :int)
  (value :pointer))

(defun list-store-set-value (model iter column value)
 #+liber-documentation
 "@version{2025-07-23}
  @argument[model]{a @class{gtk:list-store} object}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator for the row being
    modified}
  @argument[column]{an integer for the column number to modify}
  @argument[value]{a new value for the cell}
  @begin{short}
    Sets the data in the cell specified by @arg{iter} and @arg{column}.
  @end{short}
  The type of @arg{value} must be convertible to the type of @arg{column}.
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-set}"
  (cffi:with-foreign-object (gvalue '(:struct g:value))
    (gobject:set-gvalue gvalue
                        value
                        (tree-model-column-type model column))
    (%list-store-set-value model iter column gvalue)
    (gobject:value-unset gvalue)
    (values)))

(export 'list-store-set-value)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_store_remove" list-store-remove) :boolean
 #+liber-documentation
 "@version{#2024-04-07}
  @argument[model]{a @class{gtk:list-store} object}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator}
  @return{@em{True} if @arg{iter} is valid, @code{nil} if not.}
  @begin{short}
    Removes the given row from the list store.
  @end{short}
  After being removed, @arg{iter} is set to be the next valid row, or
  invalidated if it pointed to the last row in the list store.
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-insert}"
  (model (g:object list-store))
  (iter (g:boxed tree-iter)))

(export 'list-store-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_store_insert" %list-store-insert) :void
  (model (g:object list-store))
  (iter (g:boxed tree-iter))
  (pos :int))

(defun list-store-insert (model pos)
 #+liber-documentation
 "@version{2025-07-23}
  @argument[model]{a @class{gtk:list-store} object}
  @argument[pos]{an integer for the position to insert the new row}
  @return{The @class{gtk:tree-iter} iterator for the new row.}
  @begin{short}
    Creates a new row at @arg{pos}.
  @end{short}
  The returned iterator will point to this new row. If the @arg{pos} argument
  is larger than the number of rows on the list, then the new row will be
  appended to the list. The row will be empty after this function is called. To
  fill in values, you need to call the @fun{gtk:list-store-set} or
  @fun{gtk:list-store-set-value} functions.
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-set}
  @see-function{gtk:list-store-set-value}"
  (let ((iter (make-tree-iter)))
    (%list-store-insert model iter pos)
    iter))

(export 'list-store-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_before
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_store_insert_before" %list-store-insert-before) :void
  (model (g:object list-store))
  (iter (g:boxed tree-iter))
  (sibling (g:boxed tree-iter)))

(defun list-store-insert-before (model sibling)
 #+liber-documentation
 "@version{2025-07-23}
  @argument[model]{a @class{gtk:list-store} object}
  @argument[sibling]{a valid @class{gtk:tree-iter} iterator, or @code{nil}}
  @return{The @class{gtk:tree-iter} iterator for the new row.}
  @begin{short}
    Inserts a new row before @arg{sibling}.
  @end{short}
  If @arg{sibling} is @code{nil}, then the row will be appended to the end of
  the list. The returned iterator will point to this new row. The row will be
  empty after this function is called. To fill in values, you need to call the
  @fun{gtk:list-store-set} or @fun{gtk:list-store-set-value} functions.
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-set}
  @see-function{gtk:list-store-set-value}"
  (let ((iter (make-tree-iter)))
    (%list-store-insert-before model iter sibling)
    iter))

(export 'list-store-insert-before)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_after
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_store_insert_after" %list-store-insert-after) :void
  (model (g:object list-store))
  (iter (g:boxed tree-iter))
  (sibling (g:boxed tree-iter)))

(defun list-store-insert-after (model sibling)
 #+liber-documentation
 "@version{2025-07-23}
  @argument[model]{a @class{gtk:list-store} object}
  @argument[sibling]{a valid @class{gtk:tree-iter}, or @code{nil}}
  @return{The @class{gtk:tree-iter} iterator for the new row.}
  @begin{short}
    Inserts a new row after @arg{sibling}.
  @end{short}
  If @arg{sibling} is @code{nil}, then the row will be prepended to the
  beginning of the list. The returned iterator will point to this new row. The
  row will be empty after this function is called. To fill in values, you need
  to call the @fun{gtk:list-store-set} or @fun{gtk:list-store-set-value}
  functions.
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-set}
  @see-function{gtk:list-store-set-value}"
  (let ((iter (make-tree-iter)))
    (%list-store-insert-after model iter sibling)
    iter))

(export 'list-store-insert-after)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_with_valuesv                      not exported
;;; ----------------------------------------------------------------------------

;; Only for internal use. Not exported.

(cffi:defcfun ("gtk_list_store_insert_with_valuesv"
               %list-store-insert-with-valuesv) :void
  (model (g:object list-store))
  (iter (g:boxed tree-iter))
  (pos :int)
  (columns :pointer)
  (args :pointer)
  (n-values :int))

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_with_values
;;; ----------------------------------------------------------------------------

(defun list-store-insert-with-values (model pos &rest values)
 #+liber-documentation
 "@version{2025-07-23}
  @argument[model]{a @class{gtk:list-store} object}
  @argument[pos]{an integer for the position to insert the new row,
    or -1 to append after existing rows}
  @argument[values]{values to store in @arg{model}}
  @return{The @class{gtk:tree-iter} iterator for the new row.}
  @begin{short}
    Creates a new row at @arg{pos}.
  @end{short}
  The returned iterator will point to this new row. If @arg{pos} is -1, or
  larger than the number of rows in the list, then the new row will be appended
  to the list. The row will be filled with the values given to this function.

  Calling the @fun{gtk:list-store-insert-with-values} function has the same
  effect as calling
  @begin{pre}
 (let ((iter (gtk:list-store-insert model pos)))
   (gtk:list-store-set model iter  ...)
 )
  @end{pre}
  with the difference that the former will only emit a
  @sig[gtk:tree-model]{row-inserted} signal, while the latter will emit
  @sig[gtk:tree-model]{row-inserted}, @sig[gtk:tree-model]{row-changed} and, if
  the list store is sorted, @sig[gtk:tree-model]{rows-reordered} signals. Since
  emitting the @sig[gtk:tree-model]{rows-reordered} signal repeatedly can affect
  the performance of the program, the @fun{gtk:list-store-insert-with-values}
  function should generally be preferred when inserting rows in a sorted list
  store.
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
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
            (for gtype = (tree-model-column-type model i))
            (setf (cffi:mem-aref columns-ar :int i) i)
            (gobject:set-gvalue (cffi:mem-aptr value-ar '(:struct g:value) i)
                                value
                                gtype))
      (%list-store-insert-with-valuesv model
                                       iter
                                       pos
                                       columns-ar
                                       value-ar
                                       n)
      (iter (for i from 0 below n)
            (gobject:value-unset (cffi:mem-aptr value-ar '(:struct g:value) i)))
      iter)))

(export 'list-store-insert-with-values)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_prepend
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_store_prepend" %list-store-prepend) :void
  (model (g:object list-store))
  (iter (g:boxed tree-iter)))

(defun list-store-prepend (model)
 #+liber-documentation
 "@version{#2025-07-23}
  @argument[model]{a @class{gtk:list-store} object}
  @return{The @class{gtk:tree-iter} iterator for the prepended row.}
  @begin{short}
    Prepends a new row to @arg{model}.
  @end{short}
  The returned iterator will point to this new row. The row will be empty after
  this function is called. To fill in values, you need to call the
  @fun{gtk:list-store-set} or @fun{gtk:list-store-set-value} functions.
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-set}
  @see-function{gtk:list-store-set-value}"
  (let ((iter (make-tree-iter)))
    (%list-store-prepend model iter)
    iter))

(export 'list-store-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_append
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_store_append" %list-store-append) :void
  (model (g:object list-store))
  (iter (g:boxed tree-iter)))

(defun list-store-append (model)
 #+liber-documentation
 "@version{2025-07-23}
  @argument[model]{a @class{gtk:list-store} object}
  @return{The @class{gtk:tree-iter} iterator for the appended row.}
  @begin{short}
    Appends a new row to the list store.
  @end{short}
  The returned iterator will point to the new row. The row will be empty after
  this function is called. To fill in values, you need to call the
  @fun{gtk:list-store-set} or @fun{gtk:list-store-set-value} functions.
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-set}
  @see-function{gtk:list-store-set-value}"
  (let ((iter (make-tree-iter)))
    (%list-store-append model iter)
    iter))

(export 'list-store-append)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_clear
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_store_clear" list-store-clear) :void
 #+liber-documentation
 "@version{#2024-04-07}
  @argument[model]{a @class{gtk:list-store} object}
  @short{Removes all rows from the list store.}
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}"
  (model (g:object list-store)))

(export 'list-store-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_iter_is_valid
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_store_iter_is_valid" list-store-iter-is-valid) :boolean
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[model]{a @class{gtk:list-store} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator}
  @begin{return}
    @em{True} if @arg{iter} is valid, @code{nil} if @arg{iter} is invalid.
  @end{return}
  @begin{short}
    Checks if the given @arg{iter} is a valid iterator for this
    @class{gtk:list-store}.
  @end{short}
  @begin[Warning]{dictionary}
    This function is slow. Only use it for debugging and/or testing purposes.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}"
  (model (g:object list-store))
  (iter (g:boxed tree-iter)))

(export 'list-store-iter-is-valid)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_reorder
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_store_reorder" %list-store-reorder) :void
  (model (g:object list-store))
  (order :pointer))

(defun list-store-reorder (model order)
 #+liber-documentation
 "@version{#2024-04-07}
  @argument[model]{a @class{gtk:list-store} object}
  @argument[order]{a list of integer mapping the new position of each row
    to its old position before the re-ordering}
  @begin{short}
    Reorders @arg{model} to follow the order indicated by @arg{order}.
  @end{short}
  Note that this function only works with unsorted stores.
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}"
  (let ((n (length order)))
    (cffi:with-foreign-object (order-ar :int n)
      (iter (for i from 0 below n)
            (for j in order)
            (setf (cffi:mem-aref order-ar :int i) j))
      (%list-store-reorder model order-ar))))

(export 'list-store-reorder)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_swap
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_store_swap" list-store-swap) :void
 #+liber-documentation
 "@version{#2024-04-07}
  @argument[model]{a @class{gtk:list-store} object}
  @argument[a]{a @class{gtk:tree-iter} iterator}
  @argument[b]{a @class{gtk:tree-iter} iterator}
  @begin{short}
    Swaps @arg{a} and @arg{b} in @arg{model}.
  @end{short}
  Note that this function only works with unsorted stores.
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}"
  (model (g:object list-store))
  (a (g:boxed tree-iter))
  (b (g:boxed tree-iter)))

(export 'list-store-swap)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_move_before
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_store_move_before" list-store-move-before) :void
 #+liber-documentation
 "@version{#2024-04-07}
  @argument[model]{a @class{gtk:list-store} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator}
  @argument[pos]{a @class{gtk:tree-iter} iterator, or @code{nil}}
  @begin{short}
    Moves @arg{iter} in @arg{model} to the position before @arg{pos}.
  @end{short}
  Note that this function only works with unsorted stores. If the @arg{pos}
  argument is @code{nil}, @arg{iter} will be moved to the end of the list.
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-move-after}"
  (model (g:object list-store))
  (iter (g:boxed tree-iter))
  (pos (g:boxed tree-iter)))

(export 'list-store-move-before)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_move_after
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_store_move_after" list-store-move-after) :void
 #+liber-documentation
 "@version{#2024-04-07}
  @argument[model]{a @class{gtk:list-store} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator}
  @argument[pos]{a @class{gtk:tree-iter} iterator or @code{nil}}
  @begin{short}
    Moves @arg{iter} in @arg{model} to the position after @arg{pos}.
  @end{short}
  Note that this function only works with unsorted stores. If the @arg{pos}
  argument is @code{nil}, @arg{iter} will be moved to the start of the list.
  @begin[Warning]{dictionary}
    The @class{gtk:list-store} implementation is deprecated since 4.10.
    Use the @class{g:list-store} object instead.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-iter}
  @see-function{gtk:list-store-move-before}"
  (model (g:object list-store))
  (iter (g:boxed tree-iter))
  (pos (g:boxed tree-iter)))

(export 'list-store-move-after)

;;; --- End of file gtk4.list-store-lisp ---------------------------------------
