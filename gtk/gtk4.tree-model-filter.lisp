;;; ----------------------------------------------------------------------------
;;; gtk4.tree-model-filter.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; GtkTreeModelFilter
;;;
;;;     A GtkTreeModel which hides parts of an underlying tree model
;;;
;;; Types and Values
;;;
;;;     GtkTreeModelFilter
;;;
;;; Functions
;;;
;;;     GtkTreeModelFilterVisibleFunc
;;;     GtkTreeModelFilterModifyFunc
;;;
;;;     gtk_tree_model_filter_new
;;;     gtk_tree_model_filter_set_visible_func
;;;     gtk_tree_model_filter_set_modify_func
;;;     gtk_tree_model_filter_set_visible_column
;;;     gtk_tree_model_filter_get_model
;;;     gtk_tree_model_filter_convert_child_iter_to_iter
;;;     gtk_tree_model_filter_convert_iter_to_child_iter
;;;     gtk_tree_model_filter_convert_child_path_to_path
;;;     gtk_tree_model_filter_convert_path_to_child_path
;;;     gtk_tree_model_filter_refilter
;;;     gtk_tree_model_filter_clear_cache
;;;
;;; Properties
;;;
;;;     child-model
;;;     virtual-root
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTreeModelFilter
;;;
;;; Implemented Interfaces
;;;
;;;     GtkTreeModel
;;;     GtkTreeDragSource
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTreeModelFilter
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkTreeModelFilter" tree-model-filter
  (:superclass g:object
   :export t
   :interfaces ("GtkTreeModel"
                "GtkTreeDragSource")
   :type-initializer "gtk_tree_model_filter_get_type")
  ((child-model
    tree-model-filter-child-model
    "child-model" "GtkTreeModel" t t)
   (virtual-root
    tree-model-filter-virtual-root
    "virtual-root" "GtkTreePath" t t)))

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj tree-model-filter) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:TREE-MODEL-FILTER is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'tree-model-filter 'type)
 "@version{2024-4-29}
  @begin{short}
    The @class{gtk:tree-model-filter} object is a tree model which wraps
    another tree model.
  @end{short}
  It can do the following things:
  @begin{itemize}
    @begin{item}
      Filter specific rows, based on data from a \"visible column\", a column
      storing booleans indicating whether the row should be filtered or not,
      or based on the return value of a \"visible function\", which gets a
      model, iterator and returns a boolean indicating whether the row should
      be filtered or not.
    @end{item}
    @begin{item}
      Modify the \"appearance\" of the model, using a modify function. This is
      extremely powerful and allows for just changing some values and also for
      creating a completely different model based on the given child model.
    @end{item}
    @begin{item}
      Set a different root node, also known as a \"virtual root\". You can pass
      in a @class{gtk:tree-path} instance indicating the root node for the
      filter at construction time.
    @end{item}
  @end{itemize}
  The basic API is similar to the @class{gtk:tree-model-sort} class. For an
  example on its usage, see the @class{gtk:tree-model-sort} documentation.

  When using the @class{gtk:tree-model-filter} object, it is important to
  realize that the @class{gtk:tree-model-filter} object maintains an internal
  cache of all nodes which are visible in its clients. The cache is likely to
  be a subtree of the tree exposed by the child model. The
  @class{gtk:tree-model-filter} object will not cache the entire child model
  when unnecessary to not compromise the caching mechanism that is exposed by
  the reference counting scheme. If the child model implements reference
  counting, unnecessary signals may not be emitted because of reference
  counting, see the @class{gtk:tree-model} documentation. Note that, for
  example; the @class{gtk:tree-store} object does not implement reference
  counting and will always emit all signals, even when the receiving node is
  not visible.

  Because of this, limitations for possible visible functions do apply. In
  general, visible functions should only use data or properties from the node
  for which the visibility state must be determined, its siblings or its
  parents. Usually, having a dependency on the state of any child node is not
  possible, unless references are taken on these explicitly. When no such
  reference exists, no signals may be received for these child nodes. See
  reference counting in the @class{gtk:tree-model} documentation.

  Determining the visibility state of a given node based on the state of its
  child nodes is a frequently occurring use case. Therefore, the
  @class{gtk:tree-model-filter} object explicitly supports this. For example,
  when a node does not have any children, you might not want the node to be
  visible. As soon as the first row is added to the node's child level, or the
  last row removed, the node's visibility should be updated.

  This introduces a dependency from the node on its child nodes. In order to
  accommodate this, the @class{gtk:tree-model-filter} object must make sure the
  necesary signals are received from the child model. This is achieved by
  building, for all nodes which are exposed as visible nodes to
  the @class{gtk:tree-model-filter} objects clients, the child level (if any)
  and take a reference on the first node in this level. Furthermore, for every
  @code{\"row-inserted\"}, @code{\"row-changed\"} or @code{\"row-deleted\"}
  signal, also these which were not handled because the node was not cached,
  the @class{gtk:tree-model-filter} object will check if the visibility state
  of any parent node has changed.

  Beware, however, that this explicit support is limited to these two cases.
  For example, if you want a node to be visible only if two nodes in a child's
  child level (2 levels deeper) are visible, you are on your own. In this
  case, either rely on the @class{gtk:tree-store} object to emit all signals
  because it does not implement reference counting, or for models that do
  implement reference counting, obtain references on these child levels
  yourself.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model-filter} implementation is deprecated since 4.10.
    Use the @class{gtk:filter-list-model} implementation instead.
  @end{dictionary}
  @see-constructor{gtk:tree-model-filter-new}
  @see-slot{gtk:tree-model-filter-child-model}
  @see-slot{gtk:tree-model-filter-virtual-root}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-model-sort}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:tree-model-filter-child-model --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child-model"
                                               'tree-model-filter) t)
 "The @code{child-model} property of type @class{gtk:tree-model}
  (Read / Write / Construct) @br{}
  The model for the filter model to filter.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-model-filter-child-model)
      "Accessor"
      (documentation 'tree-model-filter-child-model 'function)
 "@version{2024-4-29}
  @syntax{(gtk:tree-model-filter-child-model object) => child}
  @argument[object]{a @class{gtk:tree-model-filter} object}
  @argument[child]{a @class{gtk:tree-model} object}
  @begin{short}
    Accessor of the @slot[gtk:tree-model-filter]{child-model} slot of the
    @class{gtk:tree-model-filter} class.
  @end{short}
  The model for the filter model to filter.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model-filter} implementation is deprecated since 4.10.
    Use the @class{gtk:filter-list-model} implementation instead.
  @end{dictionary}
  @see-class{gtk:tree-model-filter}
  @see-class{gtk:tree-model}")

;;; --- gtk:tree-model-filter-virtual-root -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "virtual-root"
                                               'tree-model-filter) t)
 "The @code{virtual-root} property of type @class{gtk:tree-path}
  (Read / Write / Construct) @br{}
  The virtual root, relative to the child model, for this filter model.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-model-filter-virtual-root)
      "Accessor"
      (documentation 'tree-model-filter-virtual-root 'function)
 "@version{2024-4-29}
  @syntax{(gtk:tree-model-filter-virtual-root object) => root}
  @argument[object]{a @class{gtk:tree-model-filter} object}
  @argument[root]{a @class{gtk:tree-path} instance}
  @begin{short}
    Accessor of the @slot[gtk:tree-model-filter]{virtual-root} slot of the
    @class{gtk:tree-model-filter} class.
  @end{short}
  The virtual root, relative to the child model, for this filter model.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model-filter} implementation is deprecated since 4.10.
    Use the @class{gtk:filter-list-model} implementation instead.
  @end{dictionary}
  @see-class{gtk:tree-model-filter}
  @see-class{gtk:tree-path}")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_filter_new" tree-model-filter-new)
    (g:object tree-model)
 #+liber-documentation
 "@version{2024-4-29}
  @argument[child]{a @class{gtk:tree-model} object}
  @argument[root]{a @class{gtk:tree-path} instance or @code{nil}}
  @return{The new @class{gtk:tree-model} object.}
  @begin{short}
    Creates a new @class{gtk:tree-model} object, with @arg{child} as the child
    model and @arg{root} as the virtual root.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model-filter} implementation is deprecated since 4.10.
    Use the @class{gtk:filter-list-model} implementation instead.
  @end{dictionary}
  @see-class{gtk:tree-model-filter}
  @see-class{gtk:tree-path}"
  (child (g:object tree-model))
  (root (g:boxed tree-path)))

(export 'tree-model-filter-new)

;;; ----------------------------------------------------------------------------
;;; GtkTreeModelFilterVisibleFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback tree-model-filter-visible-func :boolean
    ((model (g:object tree-model))
     (iter (g:boxed tree-iter))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func model iter)
      (return-true () :report "Return T" t)
      (return-false () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-model-filter-visible-func)
      "Callback"
      (liber:symbol-documentation 'tree-model-filter-visible-func)
 "@version{#2024-5-3}
  @syntax{lambda (model iter) => result}
  @argument[model]{a child model of the @class{gtk:tree-model-filter} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator pointing to the row in model
    whose visibility is determined}
  @argument[result]{a boolean whether the row indicated by @arg{iter} is
    visible}
  @begin{short}
    A callback function which decides whether the row indicated by @arg{iter}
    is visible.
  @end{short}
  @see-class{gtk:tree-model-filter}
  @see-class{gtk:tree-iter}
  @see-symbol{gtk:tree-model-filter-set-visible-func}")

(export 'tree-model-filter-visible-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_set_visible_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_filter_set_visible_func"
               %tree-model-filter-set-visible-func) :void
  (filter (g:object tree-model-filter))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun tree-model-filter-set-visible-func (filter func)
 #+liber-documentation
 "@version{#2024-4-29}
  @argument[filter]{a @class{gtk:tree-model-filter} object}
  @argument[func]{a @symbol{gtk:tree-model-filter-visible-func} callback
    function}
  @begin{short}
    Sets the visible function used when filtering the filter model to be
    @arg{func}.
  @end{short}
  The function should return @em{true} if the given row should be visible and
  @em{false} otherwise.

  If the condition calculated by the function changes over time, for example,
  because it depends on some global parameters, you must call the
  @fun{gtk:tree-model-filter-refilter} function to keep the visibility
  information of the model uptodate.

  Note that @arg{func} is called whenever a row is inserted, when it may still
  be empty. The visible function should therefore take special care of empty
  rows.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model-filter} implementation is deprecated since 4.10.
    Use the @class{gtk:filter-list-model} implementation instead.
  @end{dictionary}
  @see-class{gtk:tree-model-filter}
  @see-symbol{gtk:tree-model-filter-visible-func}
  @see-function{gtk:tree-model-filter-refilter}"
  (%tree-model-filter-set-visible-func
          filter
          (cffi:callback tree-model-filter-visible-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'tree-model-filter-set-visible-func)

;;; ----------------------------------------------------------------------------
;;; GtkTreeModelFilterModifyFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback tree-model-filter-modify-func :void
    ((model (g:object tree-model))
     (iter (g:boxed tree-iter))
     (value (:pointer (:struct g:value)))
     (column :int)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (funcall func model iter value column)))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-model-filter-modify-func)
      "Callback"
      (liber:symbol-documentation 'tree-model-filter-modify-func)
 "@version{#2024-5-3}
  @syntax{lambda (model iter value column)}
  @argument[model]{a @class{gtk:tree-model-filter} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator pointing to the row whose
    display values are determined}
  @argument[value]{a @symbol{g:value} instance which is already initialized for
    with the correct type for the column @arg{column}}
  @argument[column]{an integer with the column whose display value is
    determined}
  @begin{short}
    A callback function which calculates display values from raw values in the
    model.
  @end{short}
  It must fill @arg{value} with the display value for the column @arg{column}
  in the row indicated by @arg{iter}.

  Since this function is called for each data access, it is not a particularly
  efficient operation.
  @see-class{gtk:tree-model-filter}
  @see-class{gtk:tree-iter}
  @see-symbol{g:value}
  @see-symbol{gtk:tree-model-filter-set-modify-func}")

(export 'tree-model-filter-modify-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_set_modify_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_filter_set_modify_func"
               %tree-model-filter-set-modify-func) :void
  (filter (g:object tree-model-filter))
  (n-columns :int)
  (types :pointer)
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun tree-model-filter-set-modify-func (filter gtypes func)
 #+liber-documentation
 "@version{#2024-4-29}
  @argument[filter]{a @class{gtk:tree-model-filter} object}
  @argument[gtypes]{a list of @class{g:type-t} type IDs of the columns}
  @argument[func]{a @symbol{gtk:tree-model-filter-modify-func} callback
    function}
  @begin{short}
    With @arg{types} parameters, you give a list of column types for this model,
    which will be exposed to the parent model/view.
  @end{short}
  The @arg{func} parameter specifies the modify function. The modify function
  will get called for each data access, the goal of the modify function is to
  return the data which should be displayed at the location specified using the
  parameters of the modify function.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model-filter} implementation is deprecated since 4.10.
    Use the @class{gtk:filter-list-model} implementation instead.
  @end{dictionary}
  @see-class{gtk:tree-model-filter}
  @see-symbol{gtk:tree-model-filter-modify-func}"
  (let ((n (length gtypes)))
    (cffi:with-foreign-object (types-ar 'g:type-t n)
      (iter (for i from 0 below n)
            (for gtype in gtypes)
            (setf (cffi:mem-aref types-ar 'g:type-t i) gtype))
      (%tree-model-filter-set-modify-func
              filter
              n
              types-ar
              (cffi:callback tree-model-filter-modify-func)
              (glib:allocate-stable-pointer func)
              (cffi:callback glib:stable-pointer-destroy-notify)))))

(export 'tree-model-filter-set-modify-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_set_visible_column
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_filter_set_visible_column"
               tree-model-filter-set-visible-column) :void
 #+liber-documentation
 "@version{#2024-4-29}
  @argument[filter]{a @class{gtk:tree-model-filter} object}
  @argument[column]{an integer which is the column containing the visible
    information}
  @begin{short}
    Sets @arg{column} of the child model to be the column where @arg{filter}
    should look for visibility information.
  @end{short}
  The column should be of type @code{\"gboolean\"}, where @em{true} means that
  a row is visible, and @em{false} if not.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model-filter} implementation is deprecated since 4.10.
    Use the @class{gtk:filter-list-model} implementation instead.
  @end{dictionary}
  @see-class{gtk:tree-model-filter}"
  (filter (g:object tree-model-filter))
  (column :int))

(export 'tree-model-filter-set-visible-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_get_model
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_filter_get_model" tree-model-filter-model)
    (g:object tree-model)
 #+liber-documentation
 "@version{#2024-4-29}
  @argument[filter]{a @class{gtk:tree-model-filter} object}
  @return{The @class{gtk:tree-model} object for the child model.}
  @begin{short}
    Returns the child model of the filter model.
  @end{short}
  @begin[Notes]{dictionary}
    This function duplicates the @fun{gtk:tree-model-filter-child-model}
    function.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model-filter} implementation is deprecated since 4.10.
    Use the @class{gtk:filter-list-model} implementation instead.
  @end{dictionary}
  @see-class{gtk:tree-model-filter}
  @see-class{gtk:tree-model}
  @see-function{gtk:tree-model-filter-child-model}"
  (filter (g:object tree-model-filter)))

(export 'tree-model-filter-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_convert_child_iter_to_iter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_filter_convert_child_iter_to_iter"
               %tree-model-filter-convert-child-iter-to-iter) :boolean
  (filter (g:object tree-model-filter))
  (filter-iter (g:boxed tree-iter))
  (child-iter (g:boxed tree-iter)))

(defun tree-model-filter-convert-child-iter-to-iter (filter iter)
 #+liber-documentation
 "@version{#2024-4-29}
  @argument[filter]{a @class{gtk:tree-model-filter} object}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator pointing to a row on
    the child model}
  @begin{return}
    The @class{gtk:tree-iter} iterator in @arg{filter} if @arg{iter} is a valid
    iterator pointing to a visible row in the child model.
  @end{return}
  @begin{short}
    Returns an interator to point to the row in @arg{filter} that corresponds
    to the row pointed at by @arg{iter}.
  @end{short}
  If the iterator was not set, @code{nil} is returned.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model-filter} implementation is deprecated since 4.10.
    Use the @class{gtk:filter-list-model} implementation instead.
  @end{dictionary}
  @see-class{gtk:tree-model-filter}
  @see-class{gtk:tree-iter}"
  (let ((filter-iter (make-instance 'tree-iter)))
    (when (%tree-model-filter-convert-child-iter-to-iter filter
                                                         filter-iter
                                                         iter)
      filter-iter)))

(export 'tree-model-filter-convert-child-iter-to-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_convert_iter_to_child_iter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_filter_convert_iter_to_child_iter"
               %tree-model-filter-convert-iter-to-child-iter) :void
  (filter (g:object tree-model-filter))
  (child-iter (g:boxed tree-iter))
  (filter-iter (g:boxed tree-iter)))

(defun tree-model-filter-convert-iter-to-child-iter (filter iter)
 #+liber-documentation
 "@version{#2024-4-29}
  @argument[filter]{a @class{gtk:tree-model-filter} object}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator pointing to a row on
    @arg{filter}}
  @begin{return}
    The @class{gtk:tree-iter} iterator.
  @end{return}
  @begin{short}
    Returns the iterator to point to the row pointed to by @arg{iter}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model-filter} implementation is deprecated since 4.10.
    Use the @class{gtk:filter-list-model} implementation instead.
  @end{dictionary}
  @see-class{gtk:tree-model-filter}
  @see-class{gtk:tree-iter}"
  (let ((child-iter (make-instance 'tree-iter)))
    (%tree-model-filter-convert-iter-to-child-iter filter
                                                   child-iter
                                                   iter)
    child-iter))

(export 'tree-model-filter-convert-iter-to-child-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_convert_child_path_to_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_filter_convert_child_path_to_path"
               tree-model-filter-convert-child-path-to-path)
    (g:boxed tree-path :return)
 #+liber-documentation
 "@version{#2024-4-29}
  @argument[filter]{a @class{gtk:tree-model-filter} object}
  @argument[path]{a @class{gtk:tree-path} instance to convert}
  @return{The newly allocated @class{gtk:tree-path} instance, or @code{nil}.}
  @begin{short}
    Converts @arg{path} to a path relative to filter.
  @end{short}
  That is, @arg{path} points to a path in the child model. The returned path
  will point to the same row in the filtered model. If @arg{path} is not a
  valid path on the child model or points to a row which is not visible in
  @arg{filter}, then @code{nil} is returned.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model-filter} implementation is deprecated since 4.10.
    Use the @class{gtk:filter-list-model} implementation instead.
  @end{dictionary}
  @see-class{gtk:tree-model-filter}
  @see-class{gtk:tree-path}"
  (filter (g:object tree-model-sort))
  (path (g:boxed tree-path)))

(export 'tree-model-filter-convert-child-path-to-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_convert_path_to_child_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_filter_convert_path_to_child_path"
               tree-model-filter-convert-path-to-child-path)
    (g:boxed tree-path :return)
 #+liber-documentation
 "@version{#2024-4-29}
  @argument[filter]{a @class{gtk:tree-model-filter} object}
  @argument[path]{a @class{gtk:tree-path} instance to convert}
  @return{The newly allocated @class{gtk:tree-path} instance, or @code{nil}.}
  @begin{short}
    Converts @arg{path} to a path on the child model of @arg{filter}.
  @end{short}
  That is, @arg{path} points to a location in @arg{filter}. The returned path
  will point to the same location in the model not being filtered. If
  @arg{path} does not point to a location in the child model, @code{nil} is
  returned.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model-filter} implementation is deprecated since 4.10.
    Use the @class{gtk:filter-list-model} implementation instead.
  @end{dictionary}
  @see-class{gtk:tree-model-filter}
  @see-class{gtk:tree-path}"
  (filter (g:object tree-model-sort))
  (path (g:boxed tree-path)))

(export 'tree-model-filter-convert-path-to-child-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_refilter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_filter_refilter" tree-model-filter-refilter)
    :void
 #+liber-documentation
 "@version{#2024-4-29}
  @argument[filter]{a @class{gtk:tree-model-filter} object}
  @begin{short}
    Emits the @code{\"row_changed\"} signal for each row in the child model,
    which causes the filter to re-evaluate whether a row is visible or not.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model-filter} implementation is deprecated since 4.10.
    Use the @class{gtk:filter-list-model} implementation instead.
  @end{dictionary}
  @see-class{gtk:tree-model-filter}"
  (filter (g:object tree-model-filter)))

(export 'tree-model-filter-refilter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_clear_cache                       not exported
;;; ----------------------------------------------------------------------------

;; The corresponding gtk:tree-model-ref-node and gtk:tree-model-unref-node
;; functions are not exported.

(cffi:defcfun ("gtk_tree_model_filter_clear_cache"
               tree-model-filter-clear-cache) :void
 #+liber-documentation
 "@version{#2024-4-29}
  @argument[filter]{a @class{gtk:tree-model-filter} object}
  @begin{short}
    This function clears the filter of any cached iterators that have not been
    reffed with the @fun{gtk:tree-model-ref-node} function.
  @end{short}
  This function should almost never be called.

  This might be useful if the child model being filtered is static, does not
  change often, and there has been a lot of unreffed access to nodes. As a side
  effect of this function, all unreffed iterators will be invalid.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-model-filter} implementation is deprecated since 4.10.
    Use the @class{gtk:filter-list-model} implementation instead.
  @end{dictionary}
  @see-class{gtk:tree-model-filter}
  @see-function{gtk:tree-model-ref-node}"
  (filter (g:object tree-model-filter)))

;;; --- End of file gtk4.tree-model-filter.lisp --------------------------------
