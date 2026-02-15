;;; ----------------------------------------------------------------------------
;;; gtk4.tree-selection.lisp
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
;;; GtkTreeSelection
;;;
;;;     The selection object for GtkTreeView
;;;
;;; Types and Values
;;;
;;;     GtkTreeSelection
;;;
;;; Accessors
;;;
;;;     gtk_tree_selection_get_mode
;;;     gtk_tree_selection_set_mode
;;;
;;; Functions
;;;
;;;     GtkTreeSelectionFunc
;;;     GtkTreeSelectionForeachFunc
;;;
;;;     gtk_tree_selection_set_select_function
;;;     gtk_tree_selection_get_tree_view
;;;     gtk_tree_selection_get_selected
;;;     gtk_tree_selection_selected_foreach
;;;     gtk_tree_selection_get_selected_rows
;;;     gtk_tree_selection_count_selected_rows
;;;     gtk_tree_selection_select_path
;;;     gtk_tree_selection_unselect_path
;;;     gtk_tree_selection_path_is_selected
;;;     gtk_tree_selection_select_iter
;;;     gtk_tree_selection_unselect_iter
;;;     gtk_tree_selection_iter_is_selected
;;;     gtk_tree_selection_select_all
;;;     gtk_tree_selection_unselect_all
;;;     gtk_tree_selection_select_range
;;;     gtk_tree_selection_unselect_range
;;;
;;; Properties
;;;
;;;     mode
;;;
;;; Signals
;;;
;;;     changed
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTreeSelection
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTreeSelection
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkTreeSelection" tree-selection
  (:superclass g:object
    :export t
    :interfaces ()
    :type-initializer "gtk_tree_selection_get_type")
  ((mode
    tree-selection-mode
    "mode" "GtkSelectionMode" t t)))

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj tree-selection) &key)
  (when gtk-init:*warn-deprecated*
    (warn "GTK:TREE-SELECTION is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'tree-selection 'type)
 "@version{2025-07-22}
  @begin{short}
    The @class{gtk:tree-selection} object is a helper object to manage the
    selection for a @class{gtk:tree-view} widget.
  @end{short}
  The @class{gtk:tree-selection} object is automatically created when a new
  @class{gtk:tree-view} widget is created, and cannot exist independentally of
  this widget. The primary reason the @class{gtk:tree-selection} objects exists
  is for cleanliness of code and API. That is, there is no conceptual reason
  all these functions could not be methods on the @class{gtk:tree-view} widget
  instead of a separate function.

  The @class{gtk:tree-selection} object is gotten from a @class{gtk:tree-view}
  widget by calling the @fun{gtk:tree-view-selection} function. It can be
  manipulated to check the selection status of the tree view, as well as select
  and deselect individual rows. Selection is done completely tree view side. As
  a result, multiple tree views of the same model can have completely different
  selections. Additionally, you cannot change the selection of a row on the
  model that is not currently displayed by the tree view without expanding its
  parents first.

  One of the important things to remember when monitoring the selection of a
  tree view is that the @sig[gtk:tree-selection]{changed} signal is mostly a
  hint. That is, it may only emit one signal when a range of rows is selected.
  Additionally, it may on occasion emit a @sig[gtk:tree-selection]{changed}
  signal.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[tree-selection::changed]{signal}
      @begin{pre}
lambda (selection)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[selection]{The @class{gtk:tree-selection} object that received
        the signal.}
      @end{simple-table}
      Emitted whenever the selection has (possibly) changed. Please note that
      this signal is mostly a hint. It may only be emitted once when a range of
      rows are selected, and it may occasionally be emitted when nothing has
      happened.
    @end{signal}
  @end{dictionary}
  @see-slot{gtk:tree-selection-mode}
  @see-class{gtk:tree-view}
  @see-function{gtk:tree-view-selection}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mode" 'tree-selection) t)
 "The @code{mode} property of type @sym{gtk:selection-mode} (Read / Write) @br{}
  The selection mode. @br{}
  Default value: @val[gtk:selection-mode]{:single}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-selection-mode)
      "Accessor"
      (documentation 'tree-selection-mode 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-selection-mode object) => mode}
  @syntax{(setf (gtk:tree-selection-mode object) mode)}
  @argument[object]{a @class{gtk:tree-selection} object}
  @argument[mode]{a @sym{gtk:selection-mode} value}
  @begin{short}
    The accessor for the @slot[gtk:tree-selection]{mode} slot of the
    @class{gtk:tree-selection} class gets or sets the selection mode of the
    selection.
  @end{short}
  If the previous type was @val[gtk:selection-mode]{:multiple}, then the anchor
  is kept selected, if it was previously selected.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-symbol{gtk:selection-mode}")

;;; ----------------------------------------------------------------------------
;;; GtkTreeSelectionFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback tree-selection-func :boolean
    ((selection (g:object tree-selection))
     (model (g:object tree-model))
     (path (g:boxed tree-path))
     (selected :boolean)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func selection model path selected)
      (return-true () :report "Return T" t)
      (return-false () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-selection-func)
      "Callback"
      (liber:symbol-documentation 'tree-selection-func)
 "@version{#2025-12-08}
  @syntax{lambda (selection model path selected) => result}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[model]{a @class{gtk:tree-model} object being viewed}
  @argument[path]{a @class{gtk:tree-path} instance of the row in question}
  @argument[selected]{@em{true}, if the path is currently selected}
  @argument[result]{@em{true}, if the selection state of the row can be toggled}
  @begin{short}
    A callback function used by the @fun{gtk:tree-selection-set-select-function}
    function to filter whether or not a row may be selected.
  @end{short}
  It is called whenever the selection state of a row might change. A return
  value of @em{true} indicates to selection that it is okay to change the
  selection.
  @begin[Warning]{dictionary}
    This callback function is deprecated since 4.20. There is no replacement.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-selection-set-select-function}")

(export 'tree-selection-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_set_select_function
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_set_select_function"
               %tree-selection-set-select-function) :void
  (selection (g:object tree-selection))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun tree-selection-set-select-function (selection func)
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[func]{a @sym{gtk:tree-selection-func} selection function, may
    be @code{nil}}
  @begin{short}
    Sets the selection function.
  @end{short}
  If set, this function is called before any node is selected or unselected,
  giving some control over which nodes are selected. The select function
  should return @em{true} if the state of the node may be toggled, and
  @em{false} if the state of the node should be left unchanged.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-symbol{gtk:tree-selection-func}"
  (if func
      (%tree-selection-set-select-function
              selection
              (cffi:callback tree-selection-func)
              (glib:allocate-stable-pointer func)
              (cffi:callback glib:stable-pointer-destroy-notify))
      (%tree-selection-set-select-function
              selection
              (cffi:null-pointer)
              (cffi:null-pointer)
              (cffi:null-pointer))))

(export 'tree-selection-set-select-function)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_tree_view
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_get_tree_view" tree-selection-tree-view)
    (g:object tree-view)
 #+liber-documentation
 "@version{2024-11-05}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @return{The @class{gtk:tree-view} object.}
  @begin{short}
    Returns the tree view associated with @arg{selection}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-view}"
  (selection (g:object tree-selection)))

(export 'tree-selection-tree-view)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_selected
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_get_selected" %tree-selection-selected)
    :boolean
  (selection (g:object tree-selection))
  (model :pointer)
  (iter (g:boxed tree-iter)))

(defun tree-selection-selected (selection)
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @begin{return}
    The @class{gtk:tree-iter} iterator of the selected node, or @code{nil}
    if there is no selected node.
  @end{return}
  @begin{short}
    Returns the iterator to the currently selected node if the selection mode
    is set to the values @val[gtk:selection-mode]{:single} or
    @val[gtk:selection-mode]{:browse} of the @sym{gtk:selection-mode}
    enumeration.
  @end{short}
  This function will not work if you use the selection mode
  @val[gtk:selection-mode]{:multiple}.
  @begin[Notes]{dictionary}
    As a convenience the C implementation also gets the current model of the
    tree view wiget associated with the selection. Use the
    @fun{gtk:tree-selection-tree-view} and @fun{gtk:tree-view-model} functions
    instead to get the model.
  @end{dictionary}
  @begin[Examples]{dictionary}
    @begin{pre}
(let* ((model (gtk:tree-view-model view))
       (selection (gtk:tree-view-selection view))
       ;; This will only work in single or browse selection mode
       (iter (gtk:tree-selection-selected selection)))
  (if iter
      ;; A row is selected
      ...
      ;; No row is selected
      ...
  ... )
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-iter}
  @see-symbol{gtk:selection-mode}
  @see-function{gtk:tree-view-model}
  @see-function{gtk:tree-selection-tree-view}"
  (let ((iter (make-instance 'tree-iter)))
    (when (%tree-selection-selected selection (cffi:null-pointer) iter)
      iter)))

(export 'tree-selection-selected)

;;; ----------------------------------------------------------------------------
;;; GtkTreeSelectionForeachFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback tree-selection-foreach-func :void
    ((model (g:object tree-model))
     (path (g:boxed tree-path))
     (iter (g:boxed tree-iter))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (funcall func model path iter)))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-selection-foreach-func)
      "Callback"
      (liber:symbol-documentation 'tree-selection-foreach-func)
 "@version{#2025-12-08}
  @syntax{lambda (model path iter)}
  @argument[model]{a @class{gtk:tree-model} object being viewed}
  @argument[path]{a @class{gtk:tree-path} instance of a selected row}
  @argument[iter]{a @class{gtk:tree-iter} instance pointing to a selected row}
  @begin{short}
    A callback function used by the @fun{gtk:tree-selection-selected-foreach}
    function to map all selected rows.
  @end{short}
  It will be called on every selected row in the view.
  @begin[Warning]{dictionary}
    This callback function is deprecated since 4.20. There is no replacement.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-selection-selected-foreach}")

(export 'tree-selection-foreach-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_selected_foreach
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_selected_foreach"
               %tree-selection-selected-foreach) :void
  (selection (g:object tree-selection))
  (func :pointer)
  (data :pointer))

(defun tree-selection-selected-foreach (selection func)
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[func]{a @sym{gtk:tree-selection-foreach-func} callback function
    to call for each selected node}
  @begin{short}
    Calls a function for each selected node.
  @end{short}
  Note that you cannot modify the tree view or selection from within this
  function. As a result, the @fun{gtk:tree-selection-selected-rows} function
  might be more useful.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-symbol{gtk:tree-selection-foreach-func}
  @see-function{gtk:tree-selection-selected-rows}"
  (glib:with-stable-pointer (ptr func)
    (%tree-selection-selected-foreach
            selection
            (cffi:callback tree-selection-foreach-func)
            ptr)))

(export 'tree-selection-selected-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_selected_rows
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_get_selected_rows"
               %tree-selection-selected-rows)
    (g:list-t (g:boxed tree-path :return))
  (selection (g:object tree-selection))
  (model :pointer))

(defun tree-selection-selected-rows (selection)
 #+liber-documentation
 "@version{2025-04-15}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @begin{return}
    The list containing a @class{gtk:tree-path} instance for each selected row.
  @end{return}
  @begin{short}
    Creates a list of path of all selected rows.
  @end{short}
  Additionally, if you are planning on modifying the model after calling this
  function, you may want to convert the returned list into a list of
  @class{gtk:tree-row-reference} objects. To do this, you can use the
  @fun{gtk:tree-row-reference-new} function.
  @begin[Notes]{dictionary}
    As a convenience the C implementation also gets the current model of the
    tree view wiget associated with the selection. Use the
    @fun{gtk:tree-selection-tree-view} and @fun{gtk:tree-view-model} functions
    instead to get the model.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-row-reference}
  @see-function{gtk:tree-row-reference-new}
  @see-function{gtk:tree-selection-tree-view}
  @see-function{gtk:tree-view-model}"
  (%tree-selection-selected-rows selection (cffi:null-pointer)))

(export 'tree-selection-selected-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_count_selected_rows
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_count_selected_rows"
               tree-selection-count-selected-rows) :int
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @return{The integer for the number of rows selected.}
  @short{Returns the number of rows that have been selected in the tree.}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-function{gtk:tree-selection-selected-rows}"
  (selection (g:object tree-selection)))

(export 'tree-selection-count-selected-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_select_path" tree-selection-select-path)
    :void
 #+liber-documentation
 "@version{#2024-11-05}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[path]{a @class{gtk:tree-path} instance to be selected}
  @short{Select the row at @arg{path}.}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-path}"
  (selection (g:object tree-selection))
  (path (g:boxed tree-path)))

(export 'tree-selection-select-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_unselect_path" tree-selection-unselect-path)
    :void
 #+liber-documentation
 "@version{#2024-11-05}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[path]{a @class{gtk:tree-path} instance to be unselected}
  @short{Unselects the row at @arg{path}.}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-selection-select-path}"
  (selection (g:object tree-selection))
  (path (g:boxed tree-path)))

(export 'tree-selection-unselect-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_path_is_selected
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_path_is_selected"
               tree-selection-path-is-selected) :boolean
 #+liber-documentation
 "@version{#2024-11-05}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[path]{a @class{gtk:tree-path} instance to check selection on}
  @return{@em{True} if @arg{path} is selected.}
  @begin{short}
    Returns @em{true} if the row pointed to by @arg{path} is currently selected.
  @end{short}
  If @arg{path} does not point to a valid location, @em{false} is returned.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-selection-iter-is-selected}"
  (selection (g:object tree-selection))
  (path (g:boxed tree-path)))

(export 'tree-selection-path-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_iter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_select_iter" tree-selection-select-iter)
    :void
 #+liber-documentation
 "@version{#2024-11-05}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator to be selected}
  @short{Selects the specified iterator.}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-selection-unselect-iter}"
  (selection (g:object tree-selection))
  (iter (g:boxed tree-iter)))

(export 'tree-selection-select-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_iter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_unselect_iter" tree-selection-unselect-iter)
    :void
 #+liber-documentation
 "@version{#2024-11-05}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator to be unselected}
  @short{Unselects the specified iterator.}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-selection-select-iter}"
  (selection (g:object tree-selection))
  (iter (g:boxed tree-iter)))

(export 'tree-selection-unselect-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_iter_is_selected
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_iter_is_selected"
               tree-selection-iter-is-selected) :boolean
 #+liber-documentation
 "@version{#2024-11-05}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator}
  @return{@em{True}, if @arg{iter} is selected.}
  @begin{short}
    Returns @em{true} if the row at @arg{iter} is currently selected.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-iter}"
  (selection (g:object tree-selection))
  (iter (g:boxed tree-iter)))

(export 'tree-selection-iter-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_select_all" tree-selection-select-all) :void
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @begin{short}
    Selects all the nodes.
  @end{short}
  The @arg{selection} argument must be set to the
  @val[gtk:selection-mode]{:multiple} mode.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-function{gtk:tree-selection-unselect-all}"
  (selection (g:object tree-selection)))

(export 'tree-selection-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_unselect_all" tree-selection-unselect-all)
    :void
 #+liber-documentation
 "@version{#2024-11-05}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @short{Unselects all the nodes.}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-function{gtk:tree-selection-select-all}"
  (selection (g:object tree-selection)))

(export 'tree-selection-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_select_range" tree-selection-select-range)
    :void
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[start]{an initial @class{gtk:tree-path} node for the range}
  @argument[end]{a final @class{gtk:tree-path} node for the range}
  @begin{short}
    Selects a range of nodes, determined by @arg{start} and @arg{end} inclusive.
  @end{short}
  The @arg{selection} argument must be set to the
  @val[gtk:selection-mode]{:multiple} mode.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-path}"
  (selection (g:object tree-selection))
  (start (g:boxed tree-path))
  (end (g:boxed tree-path)))

(export 'tree-selection-select-range)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_selection_unselect_range"
               tree-selection-unselect-range) :void
 #+liber-documentation
 "@version{#2024-11-05}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[start]{an initial @class{gtk:tree-path} node of the range}
  @argument[end]{a initial @class{gtk:tree-path} node of the range}
  @begin{short}
    Unselects a range of nodes, determined by @arg{start} and @arg{end}
    inclusive.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-selection} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-path}"
  (selection (g:object tree-selection))
  (start (g:boxed tree-path))
  (end (g:boxed tree-path)))

(export 'tree-selection-unselect-range)

;;; --- End of file gtk4.tree-selection.lisp -----------------------------------
