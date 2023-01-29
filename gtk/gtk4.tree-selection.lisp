;;; ----------------------------------------------------------------------------
;;; gtk4.tree-selection.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
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
;;;     gtk_tree_selection_get_select_function
;;;     gtk_tree_selection_get_user_data
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

(define-g-object-class "GtkTreeSelection" tree-selection
  (:superclass g:object
    :export t
    :interfaces ()
    :type-initializer "gtk_tree_selection_get_type")
  ((mode
    tree-selection-mode
    "mode" "GtkSelectionMode" t t)))

#+liber-documentation
(setf (documentation 'tree-selection 'type)
 "@version{#2021-2-27}
  @begin{short}
    The @sym{gtk:tree-selection} object is a helper object to manage the
    selection for a @class{gtk:tree-view} widget.
  @end{short}
  The @sym{gtk:tree-selection} object is automatically created when a new
  @class{gtk:tree-view} widget is created, and cannot exist independentally of
  this widget. The primary reason the @sym{gtk:tree-selection} objects exists
  is for cleanliness of code and API. That is, there is no conceptual reason
  all these functions could not be methods on the @class{gtk:tree-view} widget
  instead of a separate function.

  The @sym{gtk:tree-selection} object is gotten from a @class{gtk:tree-view}
  widget by calling the @fun{gtk:tree-view-selection} function. It can be
  manipulated to check the selection status of the tree view, as well as select
  and deselect individual rows. Selection is done completely tree view side. As
  a result, multiple tree views of the same model can have completely different
  selections. Additionally, you cannot change the selection of a row on the
  model that is not currently displayed by the tree view without expanding its
  parents first.

  One of the important things to remember when monitoring the selection of a
  tree view is that the \"changed\" signal is mostly a hint. That is, it may
  only emit one signal when a range of rows is selected. Additionally, it may
  on occasion emit a \"changed\" signal.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
lambda (selection)    :run-first
      @end{pre}
      Emitted whenever the selection has (possibly) changed. Please note that
      this signal is mostly a hint. It may only be emitted once when a range of
      rows are selected, and it may occasionally be emitted when nothing has
      happened.
      @begin[code]{table}
        @entry[selection]{The @sym{gtk:tree-selection} object which received
        the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:tree-selection-mode}
  @see-class{gtk:tree-view}
  @see-function{gtk:tree-view-selection}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mode" 'tree-selection) t)
 "The @code{mode} property of type @symbol{gtk:selection-mode} (Read / Write)
  @br{}
  Selection mode. @br{}
  Default value: @code{:single}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-selection-mode)
      "Accessor"
      (documentation 'tree-selection-mode 'function)
 "@version{#2022-1-23}
  @syntax[]{(gtk:tree-selection-mode object) => mode}
  @syntax[]{(setf (gtk:tree-selection-mode object) mode)}
  @argument[object]{a @class{gtk:tree-selection} object}
  @argument[mode]{a value of the @symbol{gtk:selection-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:tree-selection]{mode} slot of the
    @class{gtk:tree-selection} class.
  @end{short}

  The @sym{gtk:tree-selection-mode} function gets the current selection mode of
  the selection. The @sym{(setf gtk:tree-selection-mode)} function sets the
  selection mode. If the previous type was @code{:multiple}, then the anchor is
  kept selected, if it was previously selected.
  @see-class{gtk:tree-selection}
  @see-symbol{gtk:selection-mode}")

;;; ----------------------------------------------------------------------------
;;; GtkTreeSelectionFunc
;;; ----------------------------------------------------------------------------

(defcallback tree-selection-func :boolean
    ((selection (g:object tree-selection))
     (model (g:object tree-model))
     (path (g:boxed tree-path))
     (selected :boolean)
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn selection model path selected)
      (return-true () t)
      (return-false () nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-selection-func)
      "Callback"
      (liber:symbol-documentation 'tree-selection-func)
 "@version{#2022-1-23}
  @begin{short}
    A callback function used by the @fun{gtk:tree-selection-set-select-function}
    function to filter whether or not a row may be selected.
  @end{short}
  It is called whenever the selection state of a row might change. A return
  value of @em{true} indicates to selection that it is okay to change the
  selection.
  @begin{pre}
lambda (selection model path selected)
  @end{pre}
  @begin[code]{table}
    @entry[selection]{A @class{gtk:tree-selection} object.}
    @entry[mode]{A @class{gtk:tree-model} being viewed.}
    @entry[path]{The @class{gtk:tree-path} instance of the row in question.}
    @entry[selected]{@em{True}, if the path is currently selected.}
    @entry[Returns]{@em{True}, if the selection state of the row can be
      toggled.}
  @end{table}
  @see-class{gtk:tree-selection}
  @see-function{gtk:tree-selection-set-select-function}")

(export 'tree-selection-func)

;;; ----------------------------------------------------------------------------
;;; GtkTreeSelectionForeachFunc
;;; ----------------------------------------------------------------------------

(defcallback tree-selection-foreach-func :void
    ((model (g:object tree-model))
     (path (g:boxed tree-path))
     (iter (g:boxed tree-iter))
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (funcall fn model path iter)))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-selection-foreach-func)
      "Callback"
      (liber:symbol-documentation 'tree-selection-foreach-func)
 "@version{#2022-1-23}
  @begin{short}
    A callback function used by the @fun{gtk:tree-selection-selected-foreach}
    function to map all selected rows.
  @end{short}
  It will be called on every selected row in the view.
  @begin{pre}
lambda (model path iter)
  @end{pre}
  @begin[code]{table}
    @entry[model]{The @class{gtk:tree-model} object being viewed.}
    @entry[path]{The @class{gtk:tree-path} instance of a selected row.}
    @entry[iter]{A @class{gtk:tree-iter} instance pointing to a selected row.}
  @end{table}
  @see-class{gtk:tree-selection}
  @see-function{gtk:tree-selection-selected-foreach}")

(export 'tree-selection-foreach-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_set_select_function
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_set_select_function"
          %tree-selection-set-select-function) :void
  (selection (g:object tree-selection))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun tree-selection-set-select-function (selection func)
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[func]{a @symbol{gtk:tree-selection-func} selection function, may
    be @code{nil}}
  @begin{short}
    Sets the selection function.
  @end{short}
  If set, this function is called before any node is selected or unselected,
  giving some control over which nodes are selected. The select function
  should return @em{true} if the state of the node may be toggled, and
  @em{false} if the state of the node should be left unchanged.
  @see-class{gtk:tree-selection}
  @see-symbol{gtk:tree-selection-func}"
  (%tree-selection-set-select-function
      selection
      (callback tree-selection-func)
      (glib:allocate-stable-pointer func)
      (callback glib:stable-pointer-destroy-notify)))

(export 'tree-selection-set-select-function)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_select_function
;;; ----------------------------------------------------------------------------

;; The Lisp select-function is stored in user-data

(defun tree-selection-get-select-function (selection)
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @return{The @symbol{gtk:tree-selection-func} callback function.}
  @short{Returns the current selection function.}
  @see-class{gtk:tree-selection}
  @see-symbol{gtk:tree-selection-func}"
  (let ((ptr (%tree-selection-get-user-data selection)))
    (unless (null-pointer-p ptr)
      (get-stable-pointer-value ptr))))

(export 'tree-selection-get-select-function)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_user_data ()
;;;
;;; gpointer gtk_tree_selection_get_user_data (GtkTreeSelection *selection);
;;;
;;; Returns the user data for the selection function.
;;;
;;; selection :
;;;     A GtkTreeSelection.
;;;
;;; Returns :
;;;     The user data.
;;; ----------------------------------------------------------------------------

;; The function is not exported, because we do not store user-data, but a
;; pointer to the select function.

(defcfun ("gtk_tree_selection_get_user_data"
          %tree-selection-get-user-data) :pointer
  (selection (g:object tree-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_tree_view -> tree-selection-tree-view
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_get_tree_view" tree-selection-tree-view)
    (g:object tree-view)
 #+liber-documentation
 "@version{#2021-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @return{A @class{gtk:tree-view} object.}
  @begin{short}
    Returns the tree view associated with @arg{selection}.
  @end{short}
  @see-class{gtk:tree-selection}"
  (selection (g:object tree-selection)))

(export 'tree-selection-tree-view)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_selected -> tree-selection-selected
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_get_selected" %tree-selection-selected)
    :boolean
  (selection (g:object tree-selection))
  (model :pointer)
  (iter (g:boxed tree-iter)))

(defun tree-selection-selected (selection)
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @begin{return}
    The @class{gtk:tree-iter} iterator of the selected node, or @code{nil}
    if there is no selected node.
  @end{return}
  @begin{short}
    Returns the iterator to the currently selected node if the selection mode
    is set to the values @code{:single} or @code{:browse} of the
    @symbol{gtk:selection-mode} enumeration.
  @end{short}
  This function will not work if you use the selection mode @code{:multiple}.
  @begin[Note]{dictionary}
    As a convenience the C implementation also gets the current model of the
    tree view wiget associated with the selection. Use the
    @fun{gtk:tree-selection-tree-view} and @fun{gtk:tree-view-model} functions
    instead to get the model.
  @end{dictionary}
  @begin[Example]{dictionary}
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
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-iter}
  @see-symbol{gtk:selection-mode}
  @see-function{gtk:tree-view-model}
  @see-function{gtk:tree-selection-tree-view}"
  (let ((iter (make-instance 'tree-iter)))
    (when (%tree-selection-selected selection (null-pointer) iter)
      iter)))

(export 'tree-selection-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_selected_foreach
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_selected_foreach"
          %tree-selection-selected-foreach) :void
  (selection (g:object tree-selection))
  (func :pointer)
  (data :pointer))

(defun tree-selection-selected-foreach (selection func)
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[func]{a @symbol{gtk:tree-selection-foreach-func} callback function
    to call for each selected node}
  @begin{short}
    Calls a function for each selected node.
  @end{short}
  Note that you cannot modify the tree view or selection from within this
  function. As a result, the @fun{gtk:tree-selection-selected-rows} function
  might be more useful.
  @see-class{gtk:tree-selection}
  @see-symbol{gtk:tree-selection-foreach-func}
  @see-function{gtk:tree-selection-selected-rows}"
  (with-stable-pointer (ptr func)
    (%tree-selection-selected-foreach
                                      selection
                                      (callback tree-selection-foreach-func)
                                      ptr)))

(export 'tree-selection-selected-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_selected_rows -> tree-selection-selected-rows
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_get_selected_rows"
          %tree-selection-selected-rows) (g:list-t (g:boxed tree-path :return))
  (selection (g:object tree-selection))
  (model :pointer))

(defun tree-selection-selected-rows (selection)
 #+liber-documentation
 "@version{2023-1-28}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @return{A list containing a @class{gtk:tree-path} instance for each selected
    row.}
  @begin{short}
    Creates a list of path of all selected rows.
  @end{short}
  Additionally, if you are planning on modifying the model after calling this
  function, you may want to convert the returned list into a list of
  @class{gtk:tree-row-reference} objects. To do this, you can use the
  @fun{gtk:tree-row-reference-new} function.
  @begin[Note]{dictionary}
    As a convenience the C implementation also gets the current model of the
    tree view wiget associated with the selection. Use the
    @fun{gtk:tree-selection-tree-view} and @fun{gtk:tree-view-model} functions
    instead to get the model.
  @end{dictionary}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-row-reference}
  @see-function{gtk:tree-row-reference-new}"
  (%tree-selection-selected-rows selection (cffi:null-pointer)))

(export 'tree-selection-selected-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_count_selected_rows
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_count_selected_rows"
           tree-selection-count-selected-rows) :int
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @return{An integer with the number of rows selected.}
  @short{Returns the number of rows that have been selected in the tree.}
  @see-class{gtk:tree-selection}
  @see-function{gtk:tree-selection-selected-rows}"
  (selection (g:object tree-selection)))

(export 'tree-selection-count-selected-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_path
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_select_path" tree-selection-select-path) :void
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[path]{a @class{gtk:tree-path} instance to be selected}
  @short{Select the row at @arg{path}.}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-selection-select-iter}
  @see-function{gtk:tree-selection-select-all}
  @see-function{gtk:tree-selection-unselect-path}"
  (selection (g:object tree-selection))
  (path (g:boxed tree-path)))

(export 'tree-selection-select-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_path
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_unselect_path" tree-selection-unselect-path)
    :void
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[path]{a @class{gtk:tree-path} instance to be unselected}
  @short{Unselects the row at @arg{path}.}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-selection-select-path}"
  (selection (g:object tree-selection))
  (path (g:boxed tree-path)))

(export 'tree-selection-unselect-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_path_is_selected
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_path_is_selected"
           tree-selection-path-is-selected) :boolean
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[path]{a @class{gtk:tree-path} instance to check selection on}
  @return{@em{True} if @arg{path} is selected.}
  @begin{short}
    Returns @em{true} if the row pointed to by @arg{path} is currently selected.
  @end{short}
  If @arg{path} does not point to a valid location, @em{false} is returned.
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-selection-iter-is-selected}"
  (selection (g:object tree-selection))
  (path (g:boxed tree-path)))

(export 'tree-selection-path-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_iter
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_select_iter" tree-selection-select-iter) :void
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator to be selected}
  @short{Selects the specified iterator.}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-selection-select-path}
  @see-function{gtk:tree-selection-select-all}
  @see-function{gtk:tree-selection-unselect-iter}"
  (selection (g:object tree-selection))
  (iter (g:boxed tree-iter)))

(export 'tree-selection-select-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_iter
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_unselect_iter" tree-selection-unselect-iter)
    :void
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator to be unselected}
  @short{Unselects the specified iterator.}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-selection-select-iter}"
  (selection (g:object tree-selection))
  (iter (g:boxed tree-iter)))

(export 'tree-selection-unselect-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_iter_is_selected
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_iter_is_selected"
           tree-selection-iter-is-selected) :boolean
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator}
  @return{@em{True}, if @arg{iter} is selected.}
  @begin{short}
    Returns @em{true} if the row at @arg{iter} is currently selected.
  @end{short}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-selection-path-is-selected}"
  (selection (g:object tree-selection))
  (iter (g:boxed tree-iter)))

(export 'tree-selection-iter-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_all
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_select_all" tree-selection-select-all) :void
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @begin{short}
    Selects all the nodes.
  @end{short}
  The @arg{selection} argument must be set to @code{:multiple} mode.
  @see-class{gtk:tree-selection}
  @see-function{gtk:tree-selection-unselect-all}"
  (selection (g:object tree-selection)))

(export 'tree-selection-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_all
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_unselect_all" tree-selection-unselect-all)
    :void
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @short{Unselects all the nodes.}
  @see-class{gtk:tree-selection}
  @see-function{gtk:tree-selection-select-all}"
  (selection (g:object tree-selection)))

(export 'tree-selection-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_range
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_select_range" tree-selection-select-range)
    :void
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[start]{an initial @class{gtk:tree-path} node of the range}
  @argument[end]{a final @class{gtk:tree-path} node of the range}
  @begin{short}
    Selects a range of nodes, determined by @arg{start} and @arg{end} inclusive.
  @end{short}
  The @arg{selection} argument must be set to @code{:multiple} mode.
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-path}"
  (selection (g:object tree-selection))
  (start (g:boxed tree-path))
  (end (g:boxed tree-path)))

(export 'tree-selection-select-range)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_range
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_unselect_range" tree-selection-unselect-range)
    :void
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[selection]{a @class{gtk:tree-selection} object}
  @argument[start]{an initial @class{gtk:tree-path} node of the range}
  @argument[end]{a initial @class{gtk:tree-path} node of the range}
  @begin{short}
    Unselects a range of nodes, determined by @arg{start} and @arg{end}
    inclusive.
  @end{short}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-path}"
  (selection (g:object tree-selection))
  (start (g:boxed tree-path))
  (end (g:boxed tree-path)))

(export 'tree-selection-unselect-range)

;;; --- End of file gtk4.tree-selection.lisp -----------------------------------
