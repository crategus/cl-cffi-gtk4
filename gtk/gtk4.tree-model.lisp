;;; ----------------------------------------------------------------------------
;;; gtk4.tree-model.lisp
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
;;; GtkTreeModel
;;;
;;;     The tree interface used by GtkTreeView
;;;
;;; Types and Values
;;;
;;;     GtkTreeModelFlags
;;;     GtkTreeIter
;;;     GtkTreePath
;;;     GtkTreeRowReference
;;;     GtkTreeModel
;;;
;;; Functions
;;;
;;;     GtkTreeModelForeachFunc
;;;
;;;     gtk_tree_path_new
;;;     gtk_tree_path_new_first
;;;     gtk_tree_path_new_from_indices
;;;     gtk_tree_path_new_from_indicesv                     not needed
;;;     gtk_tree_path_new_from_string
;;;     gtk_tree_path_append_index
;;;     gtk_tree_path_prepend_index
;;;     gtk_tree_path_get_depth
;;;     gtk_tree_path_get_indices
;;;     gtk_tree_path_get_indices_with_depth                not needed
;;;     gtk_tree_path_free                                  not needed
;;;     gtk_tree_path_copy
;;;     gtk_tree_path_compare
;;;     gtk_tree_path_next
;;;     gtk_tree_path_prev
;;;     gtk_tree_path_up
;;;     gtk_tree_path_down
;;;     gtk_tree_path_is_ancestor
;;;     gtk_tree_path_is_descendant
;;;     gtk_tree_path_to_string
;;;
;;;     gtk_tree_row_reference_new
;;;     gtk_tree_row_reference_new_proxy                    not implemented
;;;     gtk_tree_row_reference_get_model
;;;     gtk_tree_row_reference_get_path
;;;     gtk_tree_row_reference_valid
;;;     gtk_tree_row_reference_free                         not needed
;;;     gtk_tree_row_reference_copy
;;;     gtk_tree_row_reference_inserted                     not implemented
;;;     gtk_tree_row_reference_deleted                      not implemented
;;;     gtk_tree_row_reference_reordered                    not implemented
;;;
;;;     gtk_tree_iter_copy                                  not implemented
;;;     gtk_tree_iter_free                                  not needed
;;;
;;;     gtk_tree_model_get_flags
;;;     gtk_tree_model_get_n_columns
;;;     gtk_tree_model_get_column_type
;;;     gtk_tree_model_get_iter
;;;     gtk_tree_model_get_iter_from_string
;;;     gtk_tree_model_get_iter_first
;;;     gtk_tree_model_get_path
;;;     gtk_tree_model_get_value
;;;     gtk_tree_model_iter_next
;;;     gtk_tree_model_iter_previous
;;;     gtk_tree_model_iter_children
;;;     gtk_tree_model_iter_has_child
;;;     gtk_tree_model_iter_n_children
;;;     gtk_tree_model_iter_nth_child
;;;     gtk_tree_model_iter_parent
;;;     gtk_tree_model_get_string_from_iter
;;;     gtk_tree_model_ref_node
;;;     gtk_tree_model_unref_node
;;;     gtk_tree_model_get
;;;     gtk_tree_model_get_valist
;;;     gtk_tree_model_foreach
;;;     gtk_tree_model_row_changed
;;;     gtk_tree_model_row_inserted
;;;     gtk_tree_model_row_has_child_toggled
;;;     gtk_tree_model_row_deleted
;;;     gtk_tree_model_rows_reordered
;;;     gtk_tree_model_rows_reordered_with_length
;;;
;;; Signals
;;;
;;;     row-changed
;;;     row-deleted
;;;     row-has-child-toggled
;;;     row-inserted
;;;     rows-reordered
;;;
;;; Hierarchy
;;;
;;;     GBoxed
;;;     ├── GtkTreeIter
;;;     ╰── GtkTreePath
;;;
;;;     GInterface
;;;     ╰── GtkTreeModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTreeModelFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkTreeModelFlags" tree-model-flags
  (:export t
   :type-initializer "gtk_tree_model_flags_get_type")
  (:iters-persist 1)
  (:list-only 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-model-flags)
      "GFlags"
      (liber:symbol-documentation 'tree-model-flags)
 "@version{2025-2-25}
  @begin{declaration}
(gobject:define-gflags \"GtkTreeModelFlags\" tree-model-flags
  (:export t
   :type-initializer \"gtk_tree_model_flags_get_type\")
  (:iters-persist 1)
  (:list-only 2))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:iters-persist]{Iterators survive all signals emitted by the
        model.}
      @entry[:list-only]{The model is a list only, and never has children.}
    @end{table}
  @end{values}
  @begin{short}
    These flags indicate various properties of a @class{gtk:tree-model} object.
  @end{short}
  They are returned by the @fun{gtk:tree-model-flags} function, and must be
  static for the lifetime of the model. A more complete description of the
  @code{:iters-persist} value can be found in the overview of this section.
  @see-class{gtk:tree-model}
  @see-function{gtk:tree-model-flags}")

;;; ----------------------------------------------------------------------------
;;; GtkTreeIter
;;; ----------------------------------------------------------------------------

;; TODO: Change the implementation to an opaque boxed type

(cffi:define-foreign-type pointer-as-integer-foreign-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser pointer-as-integer))

(defmethod cffi:translate-to-foreign (value
                                      (type pointer-as-integer-foreign-type))
  (cffi:make-pointer value))

(defmethod cffi:translate-from-foreign (value
                                        (type pointer-as-integer-foreign-type))
  (cffi:pointer-address value))

;;; ----------------------------------------------------------------------------

(glib:define-gboxed-cstruct tree-iter "GtkTreeIter"
  (:export nil
   :type-initializer "gtk_tree_iter_get_type")
  (stamp :int :initform 0)
  (user-data pointer-as-integer :initform 0)
  (user-data-2 pointer-as-integer :initform 0)
  (user-data-3 pointer-as-integer :initform 0))

#+liber-documentation
(setf (liber:alias-for-class 'tree-iter)
      "GBoxed"
      (documentation 'tree-iter 'type)
 "@version{2025-2-25}
  @begin{declaration}
(glib:define-gboxed-cstruct tree-iter \"GtkTreeIter\"
  (:export t
   :type-initializer \"gtk_tree_iter_get_type\")
  (stamp :int :initform 0)
  (user-data pointer-as-integer :initform 0)
  (user-data-2 pointer-as-integer :initform 0)
  (user-data-3 pointer-as-integer :initform 0))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[stamp]{The unique stamp to catch invalid iterators.}
      @entry[user-data]{Model specific data.}
      @entry[user-data-2]{Model specific data.}
      @entry[user-data-3]{Model specific data.}
    @end{table}
  @end{values}
  @begin{short}
    The @class{gtk:tree-iter} structure is the primary structure for accessing
    a @class{gtk:tree-model} object. Models are expected to put a unique integer
    in the @arg{stamp} member, and put model specific data in the three
    @arg{user-data} members.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-iter} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-path}")

(export 'tree-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_iter_copy
;;;
;;; Creates a dynamically allocated tree iterator as a copy of iter.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_iter_free
;;;
;;; Frees an iterator that has been allocated by gtk_tree_iter_copy().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkTreePath
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque tree-path "GtkTreePath"
  :export t
  :type-initializer "gtk_tree_path_get_type"
  :alloc (%tree-path-new))

#+liber-documentation
(setf (liber:alias-for-class 'tree-path)
      "GBoxed"
      (documentation 'tree-path 'type)
 "@version{2025-1-11}
  @begin{declaration}
(glib:define-gboxed-opaque tree-path \"GtkTreePath\"
  :export t
  :type-initializer \"gtk_tree_path_get_type\"
  :alloc (%tree-path-new))
  @end{declaration}
  @begin{short}
    An opaque structure representing a path to a row in a model.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-constructor{gtk:tree-path-new}
  @see-constructor{gtk:tree-path-new-first}
  @see-constructor{gtk:tree-path-new-from-indices}
  @see-constructor{gtk:tree-path-new-from-string}
  @see-constructor{gtk:tree-path-copy}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}")

(export 'tree-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_new" %tree-path-new) :pointer)

(cffi:defcfun ("gtk_tree_path_new" tree-path-new) (g:boxed tree-path :return)
 #+liber-documentation
 "@version{2025-1-11}
  @return{The newly created @class{gtk:tree-path} instance.}
  @short{Creates a new tree path.}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}")

(export 'tree-path-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new_first
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_new_first" tree-path-new-first)
    (g:boxed tree-path :return)
 #+liber-documentation
 "@version{2025-1-11}
  @return{The new @class{gtk:tree-path} instance.}
  @short{Creates a new tree path.}
  The string representation of this tree path is \"0\".
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}")

(export 'tree-path-new-first)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new_from_indices
;;; ----------------------------------------------------------------------------

(defun tree-path-new-from-indices (&rest indices)
 #+liber-documentation
 "@version{2025-2-25}
  @argument[indices]{integers for the indices}
  @return{The newly created @class{gtk:tree-path} instance.}
  @begin{short}
    Creates a new tree path from the given @arg{indices}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}"
  (tree-path-new-from-string
      (string-right-trim ":" (format nil "~{~D:~}" indices))))

(export 'tree-path-new-from-indices)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new_from_indicesv                         not needed
;;;
;;; Creates a new path with the given indices array of length .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new_from_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_new_from_string" tree-path-new-from-string)
    (g:boxed tree-path :return)
 #+liber-documentation
 "@version{2025-2-25}
  @argument[pathstr]{a string representation of a tree path}
  @return{The newly created @class{gtk:tree-path} instance, or @code{nil}.}
  @begin{short}
    Creates a tree path initialized from @arg{pathstr}.
  @end{short}
  The @arg{pathstr} argument is expected to be a colon separated list of
  numbers. For example, the string \"10:4:0\" would create a tree path of depth
  3 pointing to the 11th child of the root node, the 5th child of that 11th
  child, and the 1st child of that 5th child. If an invalid path string is
  passed in, @code{nil} is returned.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-path-new}
  @see-function{gtk:tree-path-new-from-indices}
  @see-function{gtk:tree-path-to-string}"
  (pathstr :string))

(export 'tree-path-new-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_copy" tree-path-copy) (g:boxed tree-path :return)
 #+liber-documentation
 "@version{2025-1-11}
  @argument[path]{a @class{gtk:tree-path} instance}
  @return{The new @class{gtk:tree-path} instance.}
  @short{Creates a new tree path as a copy of @arg{path}.}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}"
  (path (g:boxed tree-path)))

(export 'tree-path-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_append_index
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_append_index" %tree-path-append-index) :void
  (path (g:boxed tree-path))
  (index :int))

;; We dot not modify the argument, but return the new value.

(defun tree-path-append-index (path index)
 #+liber-documentation
 "@version{2025-1-11}
  @argument[path]{a @class{gtk:tree-path} instance}
  @argument[index]{an integer for the index}
  @return{The @class{gtk:tree-path} instance.}
  @short{Appends a new index to the tree path.}
  As a result, the depth of @arg{path} is increased.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}"
  (let ((path (tree-path-copy path)))
    (%tree-path-append-index path index)
    path))

(export 'tree-path-append-index)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_prepend_index
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_prepend_index" %tree-path-prepend-index) :void
  (path (g:boxed tree-path))
  (index :int))

;; We dot not modify the argument, but return the new value.

(defun tree-path-prepend-index (path index)
 #+liber-documentation
 "@version{2025-1-11}
  @argument[path]{a @class{gtk:tree-path} instance}
  @argument[index]{an integer for the index}
  @return{The @class{gtk:tree-path} instance.}
  @short{Prepends a new index to the tree path.}
  As a result, the depth of @arg{path} is increased.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}"
  (let ((path (tree-path-copy path)))
    (%tree-path-prepend-index path index)
    path))

(export 'tree-path-prepend-index)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_get_depth
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_get_depth" tree-path-depth) :int
 #+liber-documentation
 "@version{2025-1-11}
  @argument[path]{a @class{gtk:tree-path} instance}
  @return{The integer with the depth of @arg{path}.}
  @short{Returns the current depth of the tree path.}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}"
  (path (g:boxed tree-path)))

(export 'tree-path-depth)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_get_indices
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_get_indices" %tree-path-indices) (:pointer :int)
  (path (g:boxed tree-path)))

(defun tree-path-indices (path)
 #+liber-documentation
 "@version{2025-2-25}
  @argument[path]{a @class{gtk:tree-path} instance}
  @return{The list of integers with the current indices, or @code{nil}.}
  @short{Returns the current indices of the tree path.}
  This is a list of integers, each representing a node in a model. The length of
  the list can be obtained with the @fun{gtk:tree-path-depth} function.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-path-depth}"
  (let ((n (tree-path-depth path))
        (indices (%tree-path-indices path)))
    (iter (for i from 0 below n)
          (collect (cffi:mem-aref indices :int i)))))

(export 'tree-path-indices)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_get_indices_with_depth                    not needed
;;;
;;; Returns the current indices of path.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_free                                      not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_compare
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_compare" tree-path-compare ) :int
 #+liber-documentation
 "@version{2025-2-25}
  @argument[path1]{a @class{gtk:tree-path} instance}
  @argument[path2]{a @class{gtk:tree-path} instance to compare with}
  @begin{return}
    The integer with the relative position of @arg{path1} and @arg{path2}.
  @end{return}
  @begin{short}
    Compares two paths.
  @end{short}
  If @arg{path1} appears before @arg{path2} in a model, then -1 is returned. If
  @arg{path2} appears before @arg{path1}, then 1 is returned. If the two nodes
  are equal, then 0 is returned.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}"
  (path1 (g:boxed tree-path))
  (path2 (g:boxed tree-path)))

(export 'tree-path-compare)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_next
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_next" %tree-path-next) :void
  (path (g:boxed tree-path)))

(defun tree-path-next (path)
 #+liber-documentation
 "@version{2025-1-11}
  @argument[path]{a @class{gtk:tree-path} instance}
  @return{The new @class{gtk:tree-path} instance.}
  @short{Moves @arg{path} to point to the next node at the current depth.}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-path-prev}"
  (let ((path (tree-path-copy path)))
    (%tree-path-next path)
    path))

(export 'tree-path-next)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_prev
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_prev" %tree-path-prev) :boolean
  (path (g:boxed tree-path)))

(defun tree-path-prev (path)
 #+liber-documentation
 "@version{2025-1-11}
  @argument[path]{a @class{gtk:tree-path} instance}
  @begin{return}
    The @class{gtk:tree-path} instance to point to the previous node, if it
    exists, otherwise @code{nil}.
  @end{return}
  @begin{short}
    Moves the tree path to point to the previous node at the current depth,
    if it exists.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-path-next}"
  (let ((path (tree-path-copy path)))
    (when (%tree-path-prev path)
      path)))

(export 'tree-path-prev)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_up
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_up" %tree-path-up) :boolean
  (path (g:boxed tree-path)))

(defun tree-path-up (path)
 #+liber-documentation
 "@version{2025-1-11}
  @argument[path]{a @class{gtk:tree-path} instance}
  @begin{return}
    The @class{gtk:tree-path} instance to point to the parent node, if it
    has a parent, otherwise @code{nil}.
  @end{return}
  @begin{short}
    Moves the tree path to point to its parent node, if it has a parent.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-path-down}"
  (let ((path (tree-path-copy path)))
    (when (%tree-path-up path)
      path)))

(export 'tree-path-up)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_down
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_down" %tree-path-down) :void
  (path (g:boxed tree-path)))

(defun tree-path-down (path)
 #+liber-documentation
 "@version{2025-1-11}
  @argument[path]{a @class{gtk:tree-path} instance}
  @begin{short}
    Moves @arg{path} to point to the first child of the current tree path.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-path-up}"
  (let ((path (tree-path-copy path)))
    (%tree-path-down path)
    path))

(export 'tree-path-down)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_is_ancestor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_is_ancestor" tree-path-is-ancestor) :boolean
 #+liber-documentation
 "@version{2025-1-11}
  @argument[path]{a @class{gtk:tree-path} instance}
  @argument[descendant]{another @class{gtk:tree-path} instance}
  @return{@em{True} if @arg{descendant} is contained inside @arg{path}.}
  @begin{short}
    Returns @em{true} if @arg{descendant} is a descendant of @arg{path}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-path-is-descendant}"
  (path (g:boxed tree-path))
  (descendant (g:boxed tree-path)))

(export 'tree-path-is-ancestor)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_is_descendant
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_is_descendant" tree-path-is-descendant) :boolean
 #+liber-documentation
 "@version{2025-1-11}
  @argument[path]{a @class{gtk:tree-path} instance}
  @argument[ancestor]{another @class{gtk:tree-path} instance}
  @return{@em{True} if @arg{ancestor} contains @arg{path} somewhere below it.}
  @begin{short}
    Returns @em{true} if @arg{path} is a descendant of @arg{ancestor}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-path-is-ancestor}"
  (path (g:boxed tree-path))
  (ancestor (g:boxed tree-path)))

(export 'tree-path-is-descendant)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_path_to_string" tree-path-to-string) :string
 #+liber-documentation
 "@version{2025-1-11}
  @argument[path]{a @class{gtk:tree-path} instance}
  @return{The string with the representation of the tree path.}
  @short{Generates a string representation of the tree path.}
  This string is a ':' separated list of numbers. For example, \"4:10:0:3\"
  would be an acceptable return value for this string.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-path} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-path-new-from-string}"
  (path (g:boxed tree-path)))

(export 'tree-path-to-string)

;;; ----------------------------------------------------------------------------
;;; GtkTreeRowReference
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque tree-row-reference "GtkTreeRowReference"
  :export t
  :type-initializer "gtk_tree_row_reference_get_type"
  :alloc (error "GtkTreeRowReference cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'tree-row-reference)
      "GBoxed"
      (documentation 'tree-row-reference 'type)
 "@version{2025-1-11}
  @begin{declaration}
(glib:define-gboxed-opaque gtk:tree-row-reference \"GtkTreeRowReference\"
  :export t
  :type-initializer \"gtk_tree_row_reference_get_type\"
  :alloc (error \"GtkTreeRowReference cannot be created from the Lisp side.\"))
  @end{declaration}
  @begin{short}
    The @class{gtk:tree-row-reference} instance tracks model changes so that it
    always refers to the same row, a @class{gtk:tree-path} instance refers to a
    position, not a fixed row.
  @end{short}
  Create a new @class{gtk:tree-row-reference} instance with the
  @fun{gtk:tree-row-reference-new} function.

  @begin[Warning]{dictionary}
    The @class{gtk:tree-row-reference} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-constructor{gtk:tree-row-reference-new}
  @see-constructor{gtk:tree-row-reference-copy}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-row-reference-new}")

(export 'tree-row-reference)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_row_reference_new" tree-row-reference-new)
    (g:boxed tree-row-reference :return)
 #+liber-documentation
 "@version{2025-1-11}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[path]{a valid @class{gtk:tree-path} instance to monitor}
  @begin{return}
    The newly allocated @class{gtk:tree-row-reference} instance, or @code{nil}.
  @end{return}
  @begin{short}
    Creates a row reference based on @arg{path}.
  @end{short}
  This reference will keep pointing to the node pointed to by @arg{path}, so
  long as it exists. Any changes that occur on @arg{model} are propagated, and
  the row reference is updated appropriately. If @arg{path} is not a valid path
  in @arg{model}, then @code{nil} is returned.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-row-reference} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-row-reference}"
  (model (g:object tree-model))
  (path (g:boxed tree-path)))

(export 'tree-row-reference-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_new_proxy
;;;
;;; You do not need to use this function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_row_reference_copy" tree-row-reference-copy)
    (g:boxed tree-row-reference :return)
 #+liber-documentation
 "@version{2025-1-11}
  @argument[reference]{a @class{gtk:tree-row-reference} instance}
  @return{The new @class{gtk:tree-row-reference} instance.}
  @short{Copies a tree row reference.}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-row-reference} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-row-reference}"
  (reference (g:boxed tree-row-reference)))

(export 'tree-row-reference-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_get_model
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_row_reference_get_model" tree-row-reference-model)
    (g:object tree-model)
 #+liber-documentation
 "@version{2025-1-11}
  @argument[reference]{a @class{gtk:tree-row-reference} instance}
  @return{The @class{gtk:tree-model} object.}
  @short{Returns the model that the row reference is monitoring.}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-row-reference} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-row-reference}
  @see-class{gtk:tree-model}"
  (reference (g:boxed tree-row-reference)))

(export 'tree-row-reference-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_get_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_row_reference_get_path" tree-row-reference-path)
    (g:boxed tree-path :return)
 #+liber-documentation
 "@version{2025-1-11}
  @argument[reference]{a @class{gtk:tree-row-reference} instance}
  @return{The current @class{gtk:tree-path} instance, or @code{nil}.}
  @begin{short}
    Returns a path that the row reference currently points to, or @code{nil} if
    the path pointed to is no longer valid.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-row-reference} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-row-reference}
  @see-class{gtk:tree-path}"
  (reference (g:boxed tree-path)))

(export 'tree-row-reference-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_valid
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_row_reference_valid" tree-row-reference-valid) :boolean
 #+liber-documentation
 "@version{2025-1-11}
  @argument[reference]{a @class{gtk:tree-row-reference}, or @code{nil}}
  @return{@em{True} if @arg{reference} points to a valid path.}
  @begin{short}
    Returns @em{true} if @arg{reference} is non-@code{nil} and refers to a
    current valid path.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-row-reference} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-row-reference}"
  (reference (g:boxed tree-row-reference)))

(export 'tree-row-reference-valid)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_free                             not needed
;;;
;;; Free's reference. reference may be NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_inserted
;;;
;;; Lets a set of row reference created by gtk_tree_row_reference_new_proxy()
;;; know that the model emitted the "row-inserted" signal.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_deleted
;;;
;;; Lets a set of row reference created by gtk_tree_row_reference_new_proxy()
;;; know that the model emitted the "row-deleted" signal.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_reordered
;;;
;;; Lets a set of row reference created by gtk_tree_row_reference_new_proxy()
;;; know that the model emitted the "rows-reordered" signal.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkTreeModel
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkTreeModel" tree-model
  (:export t
   :type-initializer "gtk_tree_model_get_type")
  nil)

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj tree-model) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:TREE-MODEL is deprecated since 4.10")))

#+liber-documentation
(setf (liber:alias-for-class 'tree-model)
      "Interface"
      (documentation 'tree-model 'type)
 "@version{2025-3-3}
  @begin{short}
    The @class{gtk:tree-model} interface defines a generic tree interface for
    use by the @class{gtk:tree-view} widget.
  @end{short}
  It is an abstract interface, and is designed to be usable with any appropriate
  data structure. The programmer just has to implement this interface on their
  own data type for it to be viewable by a @class{gtk:tree-view} widget.

  The model is represented as a hierarchical tree of strongly typed, columned
  data. In other words, the model can be seen as a tree where every node has
  different values depending on which column is being queried. The type of
  data found in a column is determined by using the GType system, that is
  @code{\"gint\"}, @code{\"GtkButton\"}, @code{\"gpointer\"}, and so on. The
  types are homogeneous per column across all nodes. It is important to note
  that this interface only provides a way of examining a model and observing
  changes. The implementation of each individual model decides how and if
  changes are made.

  In order to make life simpler for programmers who do not need to write their
  own specialized model, two generic models are provided - the
  @class{gtk:tree-store} and the @class{gtk:list-store} classes. To use these,
  the developer simply pushes data into these models as necessary. These models
  provide the data structure as well as all appropriate tree interfaces. As a
  result, implementing drag and drop, sorting, and storing data is trivial. For
  the vast majority of trees and lists, these two models are sufficient.

  Models are accessed on a node/column level of granularity. One can query for
  the value of a model at a certain node and a certain column on that node.
  There are two structures used to reference a particular node in a model.
  They are the @class{gtk:tree-path} and the @class{gtk:tree-iter} structures.
  Most of the interface consists of operations on a @class{gtk:tree-iter}
  iterator.

  A tree path is essentially a potential node. It is a location on a model that
  may or may not actually correspond to a node on a specific model. The
  @class{gtk:tree-path} structure can be converted into either an array of
  unsigned integers or a string. The string form is a list of numbers separated
  by a colon. Each number refers to the offset at that level. Thus, the path
  @code{'0'} refers to the root node and the path @code{'2:4'} refers to the
  fifth child of the third node.

  By contrast, a @class{gtk:tree-iter} iterator is a reference to a specific
  node on a specific model. It is a generic structure with an integer and three
  generic pointers. These are filled in by the model in a model-specific way.
  One can convert a path to an iterator by calling the
  @fun{gtk:tree-model-iter} function. These iterators are the primary way of
  accessing a model and are similar to the iterators used by the
  @class{gtk:text-buffer} class. They are generally statically allocated on the
  stack and only used for a short time. The model interface defines a set of
  operations using them for navigating the model.

  It is expected that models fill in the iterator with private data. For
  example, the @class{gtk:list-store} model, which is internally a simple
  linked list, stores a list node in one of the pointers. The
  @class{gtk:tree-model-sort} class stores an array and an offset in two of the
  pointers. Additionally, there is an integer field. This field is generally
  filled with a unique stamp per model. This stamp is for catching errors
  resulting from using invalid iterators with a model.

  The life cycle of an iterator can be a little confusing at first. Iterators
  are expected to always be valid for as long as the model is unchanged (and
  does not emit a signal). The model is considered to own all outstanding
  iterators and nothing needs to be done to free them from the user's point of
  view. Additionally, some models guarantee that an iterator is valid for as
  long as the node it refers to is valid (most notably the
  @class{gtk:tree-store} and @class{gtk:list-store} models). Although generally
  uninteresting, as one always has to allow for the case where iterators do not
  persist beyond a signal, some very important performance enhancements were
  made in the sort model. As a result, the @code{:iters-persist} flag was added
  to indicate this behavior.
  @begin[Examples]{dictionary}
    To show some common operation of a model, some examples are provided. The
    first example shows three ways of getting the iterator at the location
    @code{'3:2:5'}. While the first method shown is easier, the second is much
    more common, as you often get paths from callbacks.
    @begin{pre}
;; Three ways of getting the iter pointing to the location
(let (path iter parent)
  ;; Get the iterator from a string
  (setf iter (gtk:tree-model-iter-from-string model \"3:2:5\"))

  ;; Get the iterator from a path
  (setf path (gtk:tree-path-new-from-string \"3:2:5\"))
  (setf iter (gtk:tree-model-iter model path))

  ;; Walk the tree to find the iterator
  (setf parent (gtk:tree-model-iter-nth-child model nil 3))
  (setf parent (gtk:tree-model-iter-nth-child model parent 2))
  (setf iter (gtk:tree-model-iter-nth-child model parent 5))
  ... )
    @end{pre}
    The second example shows a quick way of iterating through a list and
    getting a value from each row.
  @begin{pre}
(do* ((model (gtk:tree-view-model view))             ; get the model
      (iter (gtk:tree-model-iter-first model)        ; get first iter
            (gtk:tree-model-iter-next model iter)))  ; get next iter
     ((not iter))                                    ; until iter is nil
     ;; Get a value and do something with the data
     (let ((value (gtk:tree-model-value model iter col-yearborn)))
       (gtk:list-store-set-value model iter
                                       col-yearborn
                                       (1+ value))))
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"row-changed\" signal}
      @begin{pre}
lambda (model path iter)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[model]{The @class{gtk:tree-model} object on which the signal
          is emitted.}
        @entry[path]{The @class{gtk:tree-path} instance identifying the changed
          row.}
        @entry[iter]{The valid @class{gtk:tree-iter} iterator pointing to the
          changed row.}
      @end{table}
      The signal is emitted when a row in the model has changed.
    @subheading{The \"row-deleted\" signal}
      @begin{pre}
lambda (model path)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[model]{The @class{gtk:tree-model} object on which the signal
          is emitted.}
        @entry[path]{The @class{gtk:tree-path} instance identifying the row.}
      @end{table}
      The signal is emitted when a row has been deleted. Note that no iterator
      is passed to the signal handler, since the row is already deleted. This
      should be called by models after a row has been removed. The location
      pointed to by path should be the location that the row previously was at.
      It may not be a valid location anymore.
    @subheading{The \"row-has-child-toggled\" signal}
      @begin{pre}
lambda (model path iter)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[model]{The @class{gtk:tree-model} object on which the signal
          is emitted.}
        @entry[path]{The @class{gtk:tree-path} instance identifying the row.}
        @entry[iter]{The valid @class{gtk:tree-iter} iterator pointing to the
          row.}
      @end{table}
      The signal is emitted when a row has gotten the first child row or lost
      its last child row.
    @subheading{The \"row-inserted\" signal}
      @begin{pre}
lambda (model path iter)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[model]{The @class{gtk:tree-model} object on which the signal is
          emitted.}
        @entry[path]{The @class{gtk:tree-path} instance identifying the new
          row.}
        @entry[iter]{The valid @class{gtk:tree-iter} iterator pointing to the
          new row.}
      @end{table}
      The signal is emitted when a new row has been inserted in the model. Note
      that the row may still be empty at this point, since it is a common
      pattern to first insert an empty row, and then fill it with the desired
      values.
    @subheading{The \"rows-reordered\" signal}
      @begin{pre}
lambda (model path iter order)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[model]{The @class{gtk:tree-model} object on which the signal
          is emitted.}
        @entry[path]{The @class{gtk:tree-path} instance identifying the tree
          node whose children have been reordered.}
        @entry[iter]{The valid @class{gtk:tree-iter} iterator pointing to the
         node whose children have been reordered.}
        @entry[order]{The array of integers mapping the current position of
          each child to its old position before the re-ordering, that is
          @code{@arg{order}[newpos] = oldpos}.}
      @end{table}
      The signal is emitted when the children of a node in the
      @class{gtk:tree-model} object have been reordered. Note that the signal
      is not emitted when rows are reordered by drag and drop, since this is
      implemented by removing and then reinserting the row.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-sortable}")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_flags
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_get_flags" tree-model-flags) tree-model-flags
 #+liber-documentation
 "@version{2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @begin{return}
    The @symbol{gtk:tree-model-flags} value for the flags supported by this
    interface.
  @end{return}
  @begin{short}
    Returns a set of flags supported by this interface.
  @end{short}
  The flags are a bitwise combination of @symbol{gtk:tree-model-flags} flags.
  The flags supported should not change during the lifetime of the model.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-symbol{gtk:tree-model-flags}"
  (model (g:object tree-model)))

(export 'tree-model-flags)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_n_columns
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_get_n_columns" tree-model-n-columns) :int
 #+liber-documentation
 "@version{2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @return{The integer with the number of columns.}
  @begin{short}
    Returns the number of columns supported by @arg{model}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-function{gtk:tree-model-column-type}"
  (model (g:object tree-model)))

(export 'tree-model-n-columns)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_column_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_get_column_type" tree-model-column-type) g:type-t
 #+liber-documentation
 "@version{2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[index]{an integer for the column index}
  @return{The @class{g:type-t} type ID of the column.}
  @begin{short}
    Returns the type of the column.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-function{gtk:tree-model-n-columns}"
  (model (g:object tree-model))
  (index :int))

(export 'tree-model-column-type)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_iter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_get_iter" %tree-model-iter) :boolean
  (model (g:object tree-model))
  (iter (g:boxed tree-iter))
  (path (g:boxed tree-path)))

(defun tree-model-iter (model path)
 #+liber-documentation
 "@version{2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[path]{a @class{gtk:tree-path} instance}
  @begin{return}
    The @class{gtk:tree-iter} iterator or @code{nil}, if the iterator is
    not set.
  @end{return}
  @begin{short}
    Returns a valid iterator pointing to @arg{path}.
  @end{short}
  If @arg{path} does not exist, @code{nil} is returned.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}
  @see-class{gtk:tree-path}"
  (let ((iter (make-tree-iter)))
    (when (%tree-model-iter model iter path)
      iter)))

(export 'tree-model-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_iter_from_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_get_iter_from_string"
               %tree-model-iter-from-string) :boolean
  (model (g:object tree-model))
  (iter (g:boxed tree-iter))
  (pathstr :string))

(defun tree-model-iter-from-string (model pathstr)
 #+liber-documentation
 "@version{2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[pathstr]{a string representation of a @class{gtk:tree-path} object}
  @return{The @class{gtk:tree-iter} iterator.}
  @begin{short}
    Returns a valid iterator pointing to @arg{pathstr}, if it exists.
  @end{short}
  Otherwise, @code{nil} is returned.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}
  @see-class{gtk:tree-path}"
  (let ((iter (make-tree-iter)))
    (when (%tree-model-iter-from-string model iter pathstr)
      iter)))

(export 'tree-model-iter-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_iter_first
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_get_iter_first" %tree-model-iter-first) :boolean
  (model (g:object tree-model))
  (iter (g:boxed tree-iter)))

(defun tree-model-iter-first (model)
 #+liber-documentation
 "@version{2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @return{The @class{gtk:tree-iter} iterator.}
  @begin{short}
    Returns the first iterator in the tree, the one at the path @code{\"0\"}.
  @end{short}
  Returns @code{nil} if the tree is empty.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-model-iter-next}"
  (let ((iter (make-tree-iter)))
    (when (%tree-model-iter-first model iter)
      iter)))

(export 'tree-model-iter-first)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_get_path" tree-model-path)
    (g:boxed tree-path :return)
 #+liber-documentation
 "@version{2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator}
  @return{The newly created @class{gtk:tree-path} instance.}
  @begin{short}
    Returns a tree path referenced by the given iterator.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}
  @see-class{gtk:tree-path}"
  (model (g:object tree-model))
  (iter (g:boxed tree-iter)))

(export 'tree-model-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_get_value" %tree-model-value) :void
  (model (g:object tree-model))
  (iter (g:boxed tree-iter))
  (column :int)
  (value (:pointer (:struct g:value))))

(defun tree-model-value (model iter column)
 #+liber-documentation
 "@version{2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator}
  @argument[column]{an integer for the column to lookup the value at}
  @return{The value at @arg{column}.}
  @begin{short}
    Returns the value at @arg{column}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}"
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value)
    (%tree-model-value model iter column value)
    (prog1
      (g:value-get value)
      (g:value-unset value))))

(export 'tree-model-value)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_next
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_iter_next" %tree-model-iter-next) :boolean
  (model (g:object tree-model))
  (iter (g:boxed tree-iter)))

(defun tree-model-iter-next (model iter)
 #+liber-documentation
 "@version{#2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator}
  @return{The @class{gtk:tree-iter} iterator.}
  @begin{short}
    Returns the iterator to the node following @arg{iter} at the current level.
  @end{short}
  If there is no next iterator, @code{nil} is returned.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-model-iter-first}
  @see-function{gtk:tree-model-iter-previous}"
  (let ((next (copy-tree-iter iter)))
    (when (%tree-model-iter-next model next)
      next)))

(export 'tree-model-iter-next)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_previous
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_iter_previous" %tree-model-iter-previous)
    :boolean
  (model (g:object tree-model))
  (iter (g:boxed tree-iter)))

(defun tree-model-iter-previous (model iter)
 #+liber-documentation
 "@version{#2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator}
  @return{The @class{gtk:tree-iter} iterator.}
  @begin{short}
    Returns the iterator to the previous node at the current level.
  @end{short}
  If there is no previous iterator, @code{nil} is returned.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-function{gtk:tree-model-iter-next}"
  (let ((prev (copy-tree-iter iter)))
    (when (%tree-model-iter-previous model prev)
      prev)))

(export 'tree-model-iter-previous)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_children
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_iter_children" %tree-model-iter-children)
    :boolean
  (model (g:object tree-model))
  (iter (g:boxed tree-iter))
  (parent (g:boxed tree-iter)))

(defun tree-model-iter-children (model parent)
 #+liber-documentation
 "@version{#2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[parent]{a @class{gtk:tree-iter} iterator, or @code{nil}}
  @return{The @class{gtk:tree-iter} iterator.}
  @begin{short}
    Returns the iterator to the first child of @arg{parent}.
  @end{short}
  If @arg{parent} has no children, @code{nil} is returned. The @arg{parent}
  iterator will remain a valid node after this function has been called.

  If @arg{parent} is @code{nil} returns the first node. This is equivalent to:
  @begin{pre}
(gtk:tree-model-iter-first model)
  @end{pre}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-model-iter-parent}
  @see-function{gtk:tree-model-iter-n-children}
  @see-function{gtk:tree-model-iter-nth-child}"
  (let ((child (make-tree-iter)))
    (when (%tree-model-iter-children model child parent)
      child)))

(export 'tree-model-iter-children)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_has_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_iter_has_child" tree-model-iter-has-child)
    :boolean
 #+liber-documentation
 "@version{#2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator to test for children}
  @return{@em{True} if @arg{iter} has children.}
  @begin{short}
    Returns @em{true} if @arg{iter} has children, @code{nil} otherwise.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}"
  (model (g:object tree-model))
  (iter (g:boxed tree-iter)))

(export 'tree-model-iter-has-child)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_n_children
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_iter_n_children" tree-model-iter-n-children) :int
 #+liber-documentation
 "@version{#2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator, or @code{nil}}
  @return{The integer with the number of children of @arg{iter}.}
  @begin{short}
    Returns the number of children that @arg{iter} has.
  @end{short}
  As a special case, if @arg{iter} is @code{nil}, then the number of toplevel
  nodes is returned.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}"
  (model (g:object tree-model))
  (iter (g:boxed tree-iter)))

(export 'tree-model-iter-n-children)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_nth_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_iter_nth_child" %tree-model-iter-nth-child)
    :boolean
  (model (g:object tree-model))
  (iter (g:boxed tree-iter))
  (parent (g:boxed tree-iter))
  (index :int))

(defun tree-model-iter-nth-child (model parent index)
 #+liber-documentation
 "@version{#2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[parent]{a @class{gtk:tree-iter} iterator to get the child from, or
    @code{nil}}
  @argument[index]{an integer for the index of the desired child}
  @return{The @class{gtk:tree-iter} iterator to the nth child.}
  @begin{short}
    Returns the iterator to the child of @arg{parent}, using the given index.
  @end{short}
  The first index is 0. If @arg{index} is too big, or @arg{parent} has no
  children, @code{nil} is returned. The @arg{parent} iterator will remain a
  valid node after this function has been called. As a special case, if
  @arg{parent} is @code{nil}, then the nth root node is returned.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}"
  (let ((child (make-tree-iter)))
    (when (%tree-model-iter-nth-child model child parent index)
      child)))

(export 'tree-model-iter-nth-child)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_parent
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_iter_parent" %tree-model-iter-parent) :boolean
  (model g:object)
  (iter (g:boxed tree-iter))
  (child (g:boxed tree-iter)))

(defun tree-model-iter-parent (model child)
 #+liber-documentation
 "@version{#2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[child]{a @class{gtk:tree-iter} iterator}
  @return{The @class{gtk:tree-iter} iterator to the parent.}
  @begin{short}
    Returns the iterator to the parent of @arg{child}.
  @end{short}
  If @arg{child} is at the toplevel, and does not have a parent, then
  @code{nil} is returned. The @arg{child} iterator will remain a valid node
  after this function has been called.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-model-iter-children}"
  (let ((parent (make-tree-iter)))
    (when (%tree-model-iter-parent model parent child)
      parent)))

(export 'tree-model-iter-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_string_from_iter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_get_string_from_iter"
               tree-model-string-from-iter) (:string :free-from-foreign t)
 #+liber-documentation
 "@version{#2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[iter]{a @class{gtk:tree-iter} instance}
  @return{The string representation of @arg{iter}.}
  @begin{short}
    Generates a string representation of the iterator.
  @end{short}
  This string is a ':' separated list of numbers. For example, \"4:10:0:3\"
  would be an acceptable return value for this string.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}"
  (model (g:object tree-model))
  (iter (g:boxed tree-iter)))

(export 'tree-model-string-from-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_ref_node                                not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_ref_node" tree-model-ref-node) :void
 #+liber-documentation
 "@version{#2021-3-4}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[iter]{the @class{gtk:tree-iter} iterator}
  @begin{short}
    Lets the tree ref the node.
  @end{short}

  This is an optional method for models to implement. To be more specific,
  models may ignore this call as it exists primarily for performance reasons.

  This function is primarily meant as a way for views to let caching models
  know when nodes are being displayed, and hence, whether or not to cache that
  node. Being displayed means a node is in an expanded branch, regardless of
  whether the node is currently visible in the viewport. For example, a
  file-system based model would not want to keep the entire file-hierarchy in
  memory, just the sections that are currently being displayed by every
  current view.

  A model should be expected to be able to get an iter independent of its
  reffed state.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}"
  (model (g:object tree-model))
  (iter (g:boxed tree-iter)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_unref_node                              not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_unref_node" tree-model-unref-node) :void
 #+liber-documentation
 "@version{#2020-6-28}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[iter]{the @class{gtk:tree-iter} iterator}
  @begin{short}
    Lets the tree unref the node.
  @end{short}

  This is an optional method for models to implement. To be more specific,
  models may ignore this call as it exists primarily for performance reasons.
  For more information on what this means, see the
  @fun{gtk:tree-model-ref-node} function.

  Please note that nodes that are deleted are not unreffed.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-model-ref-node}"
  (model (g:object tree-model))
  (iter (g:boxed tree-iter)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get
;;; ----------------------------------------------------------------------------

;; TODO: Consider to return all values of the row. This would be more consistent
;; to the function gtk:tree-model-set.

(defun tree-model-get (model iter &rest colums)
 #+liber-documentation
 "@version{#2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator to a row}
  @argument[columns]{a list of integers for the column numbers}
  @return{The list of values for the columns.}
  @begin{short}
    Gets the value of one or more cells in the row referenced by @arg{iter}.
  @end{short}
  The variable argument list should contain integer column numbers.
  For example, to get a value from columns 1 and 3, you would write:
  @begin{pre}
(gtk:tree-model-get model iter 1 3)
  @end{pre}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-model-value}"
  (let ((result nil))
    (dolist (column colums)
      (setf result
            (cons (tree-model-value model iter column) result)))
    (reverse result)))

(export 'tree-model-get)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_valist ()
;;;
;;; void
;;; gtk_tree_model_get_valist (GtkTreeModel *tree_model,
;;;                            GtkTreeIter *iter,
;;;                            va_list var_args);
;;;
;;; See gtk_tree_model_get(), this version takes a va_list for language
;;; bindings
;;; to use.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     a row in tree_model
;;;
;;; var_args :
;;;     va_list of column/return location pairs
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkTreeModelForeachFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback tree-model-foreach-func :boolean
    ((model g:object)
     (path (g:boxed tree-path))
     (iter (g:boxed tree-iter))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func model path iter)
      (stop-tree-model-iteration () :report "Return T" t)
      (skip-tree-model-current () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-model-foreach-func)
      "Callback"
      (liber:symbol-documentation 'tree-model-foreach-func)
 "@version{#2025-3-3}
  @syntax{lambda (model path iter) => result}
  @argument[model]{a @class{gtk:tree-model} object being iterated}
  @argument[path]{a current @class{gtk:tree-path} instance}
  @argument[iter]{a current @class{gtk:tree-iter} iterator}
  @argument[result]{@em{true} to stop iterating, @em{false} to continue}
  @begin{short}
    Type of the callback function passed to the @fun{gtk:tree-model-foreach}
    function to iterate over the rows in a tree model.
  @end{short}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-model-foreach}")

(export 'tree-model-foreach-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_foreach
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_foreach" %tree-model-foreach) :void
  (model g:object)
  (func :pointer)
  (data :pointer))

(defun tree-model-foreach (model func)
 #+liber-documentation
 "@version{#2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[func]{a @symbol{gtk:tree-model-foreach-func} callback function to
    be called on each row}
  @begin{short}
    Calls @arg{func} on each node in @arg{model} in a depth-first fashion.
  @end{short}
  If @arg{func} returns @em{true}, then the tree ceases to be walked, and the
  @fun{gtk:tree-model-foreach} function returns.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-symbol{gtk:tree-model-foreach-func}"
  (glib:with-stable-pointer (ptr func)
    (%tree-model-foreach model
                         (cffi:callback tree-model-foreach-func)
                         ptr)))

(export 'tree-model-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_row_changed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_row_changed" tree-model-row-changed) :void
 #+liber-documentation
 "@version{#2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[path]{a @class{gtk:tree-path} instance pointing to the changed row}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator pointing to the
    changed row}
  @begin{short}
    Emits the @code{\"row-changed\"} signal on @arg{model}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-iter}"
  (model (g:object tree-model))
  (path (g:boxed tree-path))
  (iter (g:boxed tree-iter)))

(export 'tree-model-row-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_row_inserted
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_row_inserted" tree-model-row-inserted) :void
 #+liber-documentation
 "@version{#2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[path]{a @class{gtk:tree-path} instance pointing to the inserted row}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator pointing to the
    inserted row}
  @begin{short}
    Emits the @code{\"row-inserted\"} signal on @arg{model}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-iter}"
  (model (g:object tree-model))
  (path (g:boxed tree-path))
  (iter (g:boxed tree-iter)))

(export 'tree-model-row-inserted)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_row_has_child_toggled
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_row_has_child_toggled"
               tree-model-row-has-child-toggled) :void
 #+liber-documentation
 "@version{#2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[path]{a @class{gtk:tree-path} instance pointing to the changed row}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator pointing to the
    changed row}
  @begin{short}
    Emits the @code{\"row-has-child-toggled\"} signal on @arg{model}.
  @end{short}
  This should be called by models after the child state of a node changes.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-iter}"
  (model (g:object tree-model))
  (path (g:boxed tree-path))
  (iter (g:boxed tree-iter)))

(export 'tree-model-row-has-child-toggled)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_row_deleted
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_row_deleted" tree-model-row-deleted) :void
 #+liber-documentation
 "@version{#2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[path]{a @class{gtk:tree-path} instance pointing to the previous
    location of the deleted row}
  @begin{short}
    Emits the @code{\"row-deleted\"} signal on @arg{model}.
  @end{short}
  This should be called by models after a row has been removed. The location
  pointed to by path should be the location that the row previously was at. It
  may not be a valid location anymore.

  Nodes that are deleted are not unreffed, this means that any outstanding
  references on the deleted node should not be released.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-path}"
  (model (g:object tree-model))
  (path (g:boxed tree-path)))

(export 'tree-model-row-deleted)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_rows_reordered
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_rows_reordered_with_length"
               %tree-model-rows-reordered) :void
  (model (g:object tree-model))
  (path (g:boxed tree-path))
  (iter (g:boxed tree-iter))
  (order (:pointer :int))
  (length :int))

(defun tree-model-rows-reordered (model path iter order)
 #+liber-documentation
 "@version{#2025-3-3}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[path]{a @class{gtk:tree-path} instance pointing to the tree node
    whose children have been reordered}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator pointing to the node
    whose children have been reordered, or @code{nil} if the depth of path is 0}
  @argument[order]{a list of integers mapping the current position of each
    child to its old position before the re-ordering}
  @begin{short}
    Emits the @code{\"rows-reordered\"} signal on @arg{model}.
  @end{short}
  This should be called by models when their rows have been reordered.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-iter}"
  (cffi:with-foreign-object (order-ar :int (length order))
    ;; TODO: Check this implementation. Is this correct?
    (dolist (i order)
      (cffi:mem-aref order-ar :int i))
    (%tree-model-rows-reordered model path iter order-ar (length order))))

(export 'tree-model-rows-reordered)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_rows_reordered_with_length ()
;;;
;;; void
;;; gtk_tree_model_rows_reordered_with_length
;;;                                (GtkTreeModel *tree_model,
;;;                                 GtkTreePath *path,
;;;                                 GtkTreeIter *iter,
;;;                                 gint *new_order,
;;;                                 gint length);
;;;
;;; Emits the "rows-reordered" signal on tree_model .
;;;
;;; This should be called by models when their rows have been reordered.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; path :
;;;     a GtkTreePath pointing to the tree node whose children have been
;;;     reordered
;;;
;;; iter :
;;;     a valid GtkTreeIter pointing to the node whose children have been
;;;     reordered, or NULL if the depth of path is 0.
;;;
;;; new_order :
;;;     an array of integers mapping the current position of each child to its
;;;     old position before the re-ordering, that is
;;;     new_order [newpos] = oldpos.
;;;
;;; length :
;;;     length of new_order array
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.tree-model.lisp ---------------------------------------
