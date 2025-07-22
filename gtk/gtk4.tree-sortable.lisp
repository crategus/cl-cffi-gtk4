;;; ----------------------------------------------------------------------------
;;; gtk4.tree-sortable.lisp
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
;;; GtkTreeSortable
;;;
;;;     The interface for sortable models used by GtkTreeView
;;;
;;; Types and Values
;;;
;;;     GtkTreeSortable
;;;     GtkTreeSortableIface
;;;
;;;     GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID
;;;     GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID
;;;
;;; Functions
;;;
;;;     GtkTreeIterCompareFunc
;;;
;;;     gtk_tree_sortable_sort_column_changed
;;;     gtk_tree_sortable_get_sort_column_id
;;;     gtk_tree_sortable_set_sort_column_id
;;;     gtk_tree_sortable_set_sort_func
;;;     gtk_tree_sortable_set_default_sort_func
;;;     gtk_tree_sortable_has_default_sort_func
;;;
;;; Signals
;;;
;;;     sort-column-changed
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkTreeSortable
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+tree-sortable-default-sort-column-id+)
      "Constant")

(defconstant +tree-sortable-default-sort-column-id+ -1
 #+liber-documentation
 "@version{2024-05-09}
  @variable-value{-1}
  @begin{short}
    The default sort column ID can be used to make a @class{gtk:tree-sortable}
    object use the default sort function.
  @end{short}
  See also the @fun{gtk:tree-sortable-sort-column-id} function.
  @see-class{gtk:tree-sortable}
  @see-function{gtk:tree-sortable-sort-column-id}")

(export '+tree-sortable-default-sort-column-id+)

;;; ----------------------------------------------------------------------------
;;; GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+tree-sortable-unsorted-sort-column-id+)
      "Constant")

(defconstant +tree-sortable-unsorted-sort-column-id+ -2
 #+liber-documentation
 "@version{2024-05-09}
  @variable-value{-2}
  @begin{short}
    The unsorted sort column ID can be used to make a @class{gtk:tree-sortable}
    object use no sorting.
  @end{short}
  See also the @fun{gtk:tree-sortable-sort-column-id} function.
  @see-class{gtk:tree-sortable}
  @see-function{gtk:tree-sortable-sort-column-id}")

(export '+tree-sortable-unsorted-sort-column-id+)

;;; ----------------------------------------------------------------------------
;;; GtkTreeSortable
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkTreeSortable" tree-sortable
  (:export t
   :type-initializer "gtk_tree_sortable_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'tree-sortable)
      "Interface"
      (documentation 'tree-sortable 'type)
 "@version{2024-05-09}
  @begin{short}
    The @class{gtk:tree-sortable} interface is an interface to be implemented by
    tree models which support sorting.
  @end{short}
  The @class{gtk:tree-view} widget uses the methods provided by this interface
  to sort the model.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-sortable} implementation is deprecated since 4.10.
    There is no replacement for this interface. You should use the
    @class{gtk:sort-list-model} implementation to wrap your list model instead.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[tree-sortable::sort-column-changed]{signal}
      @begin{pre}
 lambda (sortable)    : Run Last
      @end{pre}
      @begin[code]{simple-table}
        @entry[sortable]{The @class{gtk:tree-sortable} object on which the
        signal is emitted.}
      @end{simple-table}
      The signal is emitted when the sort column or sort order of @arg{sortable}
      is changed. The signal is emitted before the contents of @arg{sortable}
      are resorted.
    @end{signal}
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-model-sort}")

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj tree-sortable) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:TREE-SORTABLE is deprecated since 4.10")))

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeSortableIface
;;;
;;; struct GtkTreeSortableIface {
;;;   GTypeInterface g_iface;
;;;
;;;   /* signals */
;;;   void     (* sort_column_changed)  (GtkTreeSortable        *sortable);
;;;
;;;   /* virtual table */
;;;   gboolean (* get_sort_column_id)   (GtkTreeSortable        *sortable,
;;;                                      gint                   *sort_column_id,
;;;                                      GtkSortType            *order);
;;;   void     (* set_sort_column_id)   (GtkTreeSortable        *sortable,
;;;                                      gint                    sort_column_id,
;;;                                      GtkSortType             order);
;;;   void     (* set_sort_func)        (GtkTreeSortable        *sortable,
;;;                                      gint                    sort_column_id,
;;;                                      GtkTreeIterCompareFunc  sort_func,
;;;                                      gpointer                user_data,
;;;                                      GDestroyNotify          destroy);
;;;   void     (* set_default_sort_func) (GtkTreeSortable        *sortable,
;;;                                       GtkTreeIterCompareFunc  sort_func,
;;;                                       gpointer                user_data,
;;;                                       GDestroyNotify          destroy);
;;;   gboolean (* has_default_sort_func) (GtkTreeSortable        *sortable);
;;; };
;;; ----------------------------------------------------------------------------

(gobject:define-vtable ("GtkTreeSortable" tree-sortable)
  (:skip parent-instance (:pointer (:struct gobject:type-interface)))
  ;; signal
  (:skip sort-columns-changed :pointer)
  ;; methods
  (get-sort-column-id (:boolean (sortable (g:object tree-sortable))
                                (sort-column-id (:pointer :int))
                                (order (:pointer sort-type)))
    :impl-call ((sortable)
                (multiple-value-bind (sorted-p r-sort-column-id r-order)
                    (tree-sortable-get-sort-column-id-impl sortable)
                  (unless (cffi:null-pointer-p sort-column-id)
                    (setf (cffi:mem-ref sort-column-id :int) r-sort-column-id))
                  (unless (cffi:null-pointer-p order)
                    (setf (cffi:mem-ref order 'sort-type) r-order))
                  sorted-p)))
  (set-sort-column-id (:void (sortable (g:object tree-sortable))
                             (sort-column-id :int)
                             (order sort-type)))
  (set-sort-func (:void (sortable (g:object tree-sortable))
                        (sort-column-id :int)
                        (func :pointer)
                        (data :pointer)
                        (destroy-notify :pointer)))
  (set-default-sort-func (:void (sortable (g:object tree-sortable))
                                (func :pointer)
                                (data :pointer)
                                (destroy-notify :pointer)))
  (has-default-sort-func (:boolean (sortable (g:object tree-sortable)))))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_sortable_sort_column_changed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_sortable_sort_column_changed"
               tree-sortable-sort-column-changed) :void
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[sortable]{a @class{gtk:tree-sortable} object}
  @begin{short}
    Emits a @sig[gtk:tree-sortable]{sort-column-changed} signal on
    @arg{sortable}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-sortable} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-sortable}"
  (sortable (g:object tree-sortable)))

(export 'tree-sortable-sort-column-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_sortable_get_sort_column_id
;;; ----------------------------------------------------------------------------

(defun (setf tree-sortable-sort-column-id) (value sortable)
  (destructuring-bind (column order)
      (if (listp value)
          value
          (list value :ascending))
    (cffi:foreign-funcall "gtk_tree_sortable_set_sort_column_id"
                          (g:object tree-sortable) sortable
                          :int column
                          sort-type order
                          :boolean)
    value))

(cffi:defcfun ("gtk_tree_sortable_get_sort_column_id"
               %tree-sortable-sort-column-id) :boolean
  (sortable (g:object tree-sortable))
  (column (:pointer :int))
  (order (:pointer sort-type)))

(defun tree-sortable-sort-column-id (sortable)
 #+liber-documentation
 "@version{#2025-07-22}
  @syntax{(gtk:tree-sortable-sort-column-id sortable) => column, order}
  @syntax{(setf (gtk:tree-sortable-sort-column-id sortable) column)}
  @syntax{(setf (gtk:tree-sortable-sort-column-id sortable) '(column order))}
  @argument[sortable]{a @class{gtk:tree-sortable} object}
  @argument[column]{an integer for the sort column ID}
  @argument[order]{a value of the @sym{gtk:sort-type} enumeration}
  @begin{short}
    The @fun{gtk:tree-sortable-sort-column-id} function returns the current
    sort column ID and the sort order.
  @end{short}
  The @setf{gtk:tree-sortable-sort-column-id} function sets the sort column ID
  and the sort order. If no sort order is given, the sort order is set to the
  default value @val[gtk:sort-type]{:ascending}.

  The sortable will resort itself to reflect this change, after emitting a
  @sig[gtk:tree-sortable]{sort-column-changed} signal. The @code{column}
  argument may either be a regular column ID, or one of the following special
  values: @var{gtk:+tree-sortable-default-sort-column-id+} or
  @var{gtk:+tree-sortable-unsorted-sort-column-id+}.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-sortable} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-sortable}
  @see-symbol{gtk:sort-type}"
  (cffi:with-foreign-objects ((column :int) (order 'sort-type))
    (%tree-sortable-sort-column-id sortable column order)
    (values (cffi:mem-ref column :int)
            (cffi:mem-ref order 'sort-type))))

(export 'tree-sortable-sort-column-id)

;;; ----------------------------------------------------------------------------
;;; GtkTreeIterCompareFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback tree-iter-compare-func :int
    ((model (g:object tree-model))
     (iter1 (g:boxed tree-iter))
     (iter2 (g:boxed tree-iter))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (funcall func model iter1 iter2)))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-iter-compare-func)
      "Callback"
      (liber:symbol-documentation 'tree-iter-compare-func)
 "@version{#2025-07-22}
  @syntax{lambda (model iter1 iter2) => result}
  @argument[model]{a @class{gtk:tree-model} object the comparison is within}
  @argument[iter1]{a @class{gtk:tree-iter} iterator in @arg{model}}
  @argument[iter2]{another @class{gtk:tree-iter} iterator in @arg{model}}
  @argument[result]{a negative integer, zero or a positive integer depending on
    whether @arg{iter1} sorts before, with or after @arg{iter2}}
  @begin{short}
    The @sym{gtk:tree-iter-compare-func} callback function should return a
    negative integer, zero, or a positive integer if @arg{iter1} sorts before
    @arg{iter2}, @arg{iter1} sorts with @arg{iter2}, or @arg{iter1} sorts after
    @arg{iter2} respectively.
  @end{short}
  If two iterators compare as equal, their order in the sorted model is
  undefined. To ensure that the @class{gtk:tree-sortable} object behaves as
  expected, the @sym{gtk:tree-iter-compare-func} callback function must define
  a partial order on the model, that is, it must be reflexive, antisymmetric
  and transitive.
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}
  @see-class{gtk:tree-sortable}
  @see-function{gtk:tree-sortable-set-sort-func}")

(export 'tree-iter-compare-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_sortable_set_sort_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_sortable_set_sort_func" %tree-sortable-set-sort-func)
    :void
  (sortable (g:object tree-sortable))
  (column :int)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun tree-sortable-set-sort-func (sortable column func)
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[sortable]{a @class{gtk:tree-sortable} object}
  @argument[column]{an integer for the sort column ID to set the function for}
  @argument[func]{a @sym{gtk:tree-iter-compare-func} callback function}
  @begin{short}
    Sets the comparison function used when sorting to be @arg{func}.
  @end{short}
  If the current sort column ID of @arg{sortable} is the same as
  @arg{column}, then the model will sort using this function.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-sortable} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-sortable}
  @see-symbol{gtk:tree-iter-compare-func}"
  (%tree-sortable-set-sort-func
          sortable
          column
          (cffi:callback tree-iter-compare-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'tree-sortable-set-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_sortable_set_default_sort_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_sortable_set_default_sort_func"
               %tree-sortable-set-default-sort-func) :void
  (sortable (g:object tree-sortable))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun tree-sortable-set-default-sort-func (sortable func)
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[sortable]{a @class{gtk:tree-sortable} object}
  @argument[func]{a @sym{gtk:tree-iter-compare-func} callback function}
  @begin{short}
    Sets the default comparison callback function used when sorting to be
    @arg{func}.
  @end{short}
  If the current sort column ID of @arg{sortable} is
  @var{gtk:+tree-sortable-default-sort-column-id+}, then the model will sort
  using this function.

  If @arg{func} is @code{nil}, then there will be no default comparison
  function. This means that once the model has been sorted, it cannot go back
  to the default state. In this case, when the current sort column ID of
  @arg{sortable} is @var{gtk:+tree-sortable-default-sort-column-id+},
  the model will be unsorted.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-sortable} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-sortable}
  @see-symbol{gtk:tree-iter-compare-func}"
  (if func
      (%tree-sortable-set-default-sort-func
              sortable
              (cffi:callback tree-iter-compare-func)
              (glib:allocate-stable-pointer func)
              (cffi:callback glib:stable-pointer-destroy-notify))
      (%tree-sortable-set-default-sort-func
              sortable
              (cffi:null-pointer)
              (cffi:null-pointer)
              (cffi:null-pointer))))

(export 'tree-sortable-set-default-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_sortable_has_default_sort_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_sortable_has_default_sort_func"
               tree-sortable-has-default-sort-func) :boolean
 #+liber-documentation
 "@version{#2024-05-09}
  @argument[sortable]{a @class{gtk:tree-sortable} object}
  @return{@em{True}, if the model has a default sort function.}
  @begin{short}
    Returns @em{true} if the model has a default sort function.
  @end{short}
  This is used primarily by @class{gtk:tree-view-column} objects in order to
  determine if a model can go back to the default state, or not.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-sortable} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-sortable}
  @see-class{gtk:tree-view-column}"
  (sortable (g:object tree-sortable)))

(export 'tree-sortable-has-default-sort-func)

;;; --- End of file gtk4.tree-sortable.lisp ------------------------------------
