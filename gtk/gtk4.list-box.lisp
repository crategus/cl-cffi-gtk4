;;; ----------------------------------------------------------------------------
;;; gtk4.list-box.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2023 Dieter Kaiser
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
;;; GtkListBox
;;;
;;;     A list container
;;;
;;; Types and Values
;;;
;;;     GtkListBox
;;;     GtkListBoxRow
;;;
;;; Accessors
;;;
;;;     gtk_list_box_get_activate_on_single_click
;;;     gtk_list_box_set_activate_on_single_click
;;;     gtk_list_box_get_selection_mode
;;;     gtk_list_box_set_selection_mode
;;;     gtk_list_box_get_show_separators
;;;     gtk_list_box_set_show_separators
;;;
;;;     gtk_list_box_row_get_activatable
;;;     gtk_list_box_row_set_activatable
;;;     gtk_list_box_row_get_child
;;;     gtk_list_box_row_set_child
;;;     gtk_list_box_row_get_selectable
;;;     gtk_list_box_row_set_selectable
;;;
;;; Functions
;;;
;;;     GtkListBoxFilterFunc
;;;     GtkListBoxSortFunc
;;;     GtkListBoxUpdateHeaderFunc
;;;
;;;     gtk_list_box_new
;;;     gtk_list_box_prepend
;;;     gtk_list_box_append
;;;     gtk_list_box_insert
;;;     gtk_list_box_remove
;;;     gtk_list_box_select_row
;;;     gtk_list_box_unselect_row
;;;     gtk_list_box_select_all
;;;     gtk_list_box_unselect_all
;;;     gtk_list_box_get_selected_row
;;;
;;;     GtkListBoxForeachFunc
;;;
;;;     gtk_list_box_selected_foreach
;;;     gtk_list_box_get_selected_rows
;;;     gtk_list_box_get_adjustment
;;;     gtk_list_box_set_adjustment
;;;     gtk_list_box_set_placeholder
;;;     gtk_list_box_get_row_at_index
;;;     gtk_list_box_get_row_at_y
;;;     gtk_list_box_invalidate_filter
;;;     gtk_list_box_invalidate_headers
;;;     gtk_list_box_invalidate_sort
;;;     gtk_list_box_set_filter_func
;;;     gtk_list_box_set_header_func
;;;     gtk_list_box_set_sort_func
;;;     gtk_list_box_drag_highlight_row
;;;     gtk_list_box_drag_unhighlight_row
;;;
;;;     GtkListBoxCreateWidgetFunc
;;;
;;;     gtk_list_box_bind_model
;;;
;;;     gtk_list_box_row_new
;;;     gtk_list_box_row_changed
;;;     gtk_list_box_row_is_selected
;;;     gtk_list_box_row_get_header
;;;     gtk_list_box_row_set_header
;;;     gtk_list_box_row_get_index
;;;
;;; Properties
;;;
;;;     accept-unpaired-release
;;;     activate-on-single-click
;;;     selection-mode
;;;     show-separators
;;;
;;;     activatable
;;;     child
;;;     selectable
;;;
;;; Signals
;;;
;;;     activate-cursor-row
;;;     move-cursor
;;;     row-activated
;;;     row-selected
;;;     select-all
;;;     selected-rows-changed
;;;     toggle-cursor-row
;;;     unselect-all
;;;
;;;     activate
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ├── GtkListBox
;;;;            ╰── GtkListBoxRow
;;;
;;; Implemented Interfaces (GtkListBox)
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;
;;; Implemented Interfaces (GtkListBoxRow)
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkActionable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkListBoxRow
;;; ----------------------------------------------------------------------------

;; TODO: Implement missing interfaces

(define-g-object-class "GtkListBoxRow" list-box-row
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkActionable"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_list_box_row_get_type")
  ((activatable
    list-box-row-activatable
    "activatable" "gboolean" t t)
   (child
    list-box-row-child
    "child" "GtkWidget" t t)
   (selectable
    list-box-row-selectable
    "selectable" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'list-box-row 'type)
 "@version{#2021-11-30}
  @begin{short}
    The @sym{gtk:list-box-row} widget is a child widget for the
    @class{gtk:list-box} widget.
  @end{short}
  The @sym{gtk:list-box-row} widget can be marked as activatable or selectable.
  If a row is activatable, the \"row-activated\" signal will be emitted for it
  when the user tries to activate it. If it is selectable, the row will be
  marked as selected when the user tries to select it.
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (row)    :action
      @end{pre}
      This is a keybinding signal, which will cause this row to be activated.
      If you want to be notified when the user activates a row (by key or not),
      use the \"row-activated\" signal on the parent @class{gtk:list-box}
      widget of the row.
      @begin[code]{table}
        @entry[row]{The @sym{gtk:list-box-row} widget on which the signal is
          emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:list-box-row-activatable}
  @see-slot{gtk:list-box-row-child}
  @see-slot{gtk:list-box-row-selectable}
  @see-contstructor{gtk:list-box-row-new}
  @see-class{gtk:list-box}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- list-box-row-activatable -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "activatable"
                                               'list-box-row) t)
 "The @code{activatable} property of type @code{:boolean} (Read / Write) @br{}
  Determines whether the \"row-activated\" signal will be emitted for this row.
  @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'list-box-row-activatable)
      "Accessor"
      (documentation 'list-box-row-activatable 'function)
 "@version{#2021-11-15}
  @syntax[]{(gtk:list-box-row-activatable object) => activatable}
  @syntax[]{(setf (gtk:list-box-row-activatable object) activatable)}
  @argument[object]{a @class{gtk:list-box-row} widget}
  @argument[activatable]{@em{true} to mark the row as activatable}
  @begin{short}
    Accessor of the @slot[gtk:list-box-row]{activatable} slot of the
    @class{gtk:list-box-row} class.
  @end{short}
  The @sym{gtk:list-box-row-activatable} function gets the value of the
  @slot[gtk:list-box-row]{activatable} property for this row. The
  @sym{(setf gtk:list-box-row-activatable)} function sets the property.
  @see-class{gtk:list-box-row}")

;;; --- list-box-row-child -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'list-box-row) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'list-box-row-child)
      "Accessor"
      (documentation 'list-box-row-child 'function)
 "@version{#2022-1-8}
  @syntax[]{(gtk:list-box-row-child object) => child}
  @syntax[]{(setf (gtk:list-box-row-child object) child)}
  @argument[object]{a @class{gtk:list-box-row} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:list-box-row]{child} slot of the
    @class{gtk:list-box-row} class.
  @end{short}
  Gets or sets the child widget of the list box row.
  @see-class{gtk:list-box-row}")

;;; --- list-box-row-selectable --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selectable"
                                               'list-box-row) t)
 "The @code{selectable} property of type @code{:boolean} (Read / Write) @br{}
  Determines whether this row can be selected. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'list-box-row-selectable)
      "Accessor"
      (documentation 'list-box-row-selectable 'function)
 "@version{#2021-11-15}
  @syntax[]{(gtk:list-box-row-selectable object) => selectable}
  @syntax[]{(setf (gtk:list-box-row-selectable object) selectable)}
  @argument[object]{a @class{gtk:list-box-row} widget}
  @argument[selectable]{@em{true} to mark the row as selectable}
  @begin{short}
    Accessor of the @slot[gtk:list-box-row]{selectable} slot of the
    @class{gtk:list-box} class.
  @end{short}
  The @sym{gtk:list-box-row-selectable} function gets the value of the
  @slot[gtk:list-box-row]{selectable} property for this row. The
  @sym{(setf gtk:list-box-row-selectable)} function sets the property.
  @see-class{gtk:list-box-row}")

;;; ----------------------------------------------------------------------------
;;; GtkListBox
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkListBox" list-box
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_list_box_get_type")
  ((accept-unpaired-release
    list-box-accept-unpaired-release
    "accept-unpaired-release" "gboolean" t t)
   (activate-on-single-click
    list-box-activate-on-single-click
    "activate-on-single-click" "gboolean" t t)
   (selection-mode
    list-box-selection-mode
    "selection-mode" "GtkSelectionMode" t t)
   (show-separators
    list-box-show-separators
    "show-separators" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'list-box 'type)
 "@version{#2022-9-7}
  @begin{short}
    The @sym{gtk:list-box} widget is a vertical list box that contains
    @class{gtk:list-box-row} children.
  @end{short}
  These rows can by dynamically sorted and filtered, and headers can be added
  dynamically depending on the row content. It also allows keyboard and mouse
  navigation and selection like a typical list.

  @image[list-box]{Figure: GtkListBox}

  Using the @sym{gtk:list-box} widget is often an alternative to the
  @class{gtk:tree-view} widget, especially when the list contents has a more
  complicated layout than what is allowed by a @class{gtk:cell-renderer}
  object, or when the contents is interactive, i.e. has a button in it.

  Although a @sym{gtk:list-box} widget must have only @class{gtk:list-box-row}
  children you can add any kind of widget to it via the
  @fun{gtk:list-box-prepend}, @fun{gtk:list-box-append}, and
  @fun{gtk:list-box-insert} functions, and a @class{gtk:list-box-row} widget
  will automatically be inserted between the list and the widget.

  The @class{gtk:list-box-row} widget can be marked as activatable or
  selectable. If a row is activatable, the \"row-activated\" signal will be
  emitted for it when the user tries to activate it. If it is selectable, the
  row will be marked as selected when the user tries to select it.
  @begin[CSS Nodes]{dictionary}
    @begin{pre}
list
 ╰── row[.activatable]
    @end{pre}
    The @sym{gtk:list-box} implementation uses a single CSS node named
    @code{list}. Each @class{gtk:list-box-row} widget uses a single CSS node
    named @code{row}. The row nodes get the @code{.activatable} style class
    added when appropriate.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-cursor-row\" signal}
      @begin{pre}
lambda (listbox)    :action
      @end{pre}
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk:list-box} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
lambda (listbox step count)    :action
      @end{pre}
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk:list-box} widget on which the signal is
          emitted.}
        @entry[step]{A value of the @symbol{gtk:movement-step} enumeration.}
        @entry[count]{An integer.}
      @end{table}
    @subheading{The \"row-activated\" signal}
      @begin{pre}
lambda (listbox row)    :run-last
      @end{pre}
      The signal is emitted when a row has been activated by the user.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk:list-box} widget on which the signal is
          emitted.}
        @entry[row]{The activated @class{gtk:list-box-row} widget.}
      @end{table}
    @subheading{The \"row-selected\" signal}
      @begin{pre}
lambda (listbox row)    :run-last
      @end{pre}
      The signal is emitted when a new row is selected, or when the selection
      is cleared. When the list box is using the @code{:multiple} selection
      mode, this signal will not give you the full picture of selection changes,
      and you should use the \"selected-rows-changed\" signal instead.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk:list-box} widget on which the signal is
          emitted.}
        @entry[row]{The selected @class{gtk:list-box-row} widget.}
      @end{table}
    @subheading{The \"select-all\" signal}
      @begin{pre}
lambda (listbox)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to select all
      children of the list box, if the selection mode permits it. The default
      bindings for this signal is the @kbd{Ctrl-a} key.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk:list-box} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"selected-rows-changed\" signal}
      @begin{pre}
lambda (listbox)    :run-first
      @end{pre}
      The signal is emitted when the set of selected rows changes.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk:list-box} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"toggle-cursor-row\" signal}
      @begin{pre}
lambda (listbox)    :action
      @end{pre}
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk:list-box} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"unselect-all\" signal}
      @begin{pre}
lambda (listbox)    :action
      @end{pre}
        The signal is a keybinding signal which gets emitted to unselect all
        children of the list box, if the selection mode permits it. The default
        bindings for this signal is the @kbd{Ctrl-Shift-a} key.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk:list-box} widget on which the signal is
          emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:list-box-accept-unpaired-release}
  @see-slot{gtk:list-box-activate-on-single-click}
  @see-slot{gtk:list-box-selection-mode}
  @see-slot{gtk:list-box-show-separators}
  @see-constructor{gtk:list-box-new}
  @see-class{gtk:list-box-row}
  @see-class{gtk:tree-view}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- list-box-accept-unpaired-release -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accept-unpaired-release"
                                               'list-box) t)
 "The @code{accept-unpaired-release} property of type @code{:boolean}
  (Read / Write) @br{}
  Accept unpaired release. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'list-box-accept-unpaired-release)
      "Accessor"
      (documentation 'list-box-accept-unpaired-release 'function)
 "@version{#2022-2-3}
  @syntax[]{(gtk:list-box-accept-unpaired-release object) => setting}
  @syntax[]{(setf (gtk:list-box-accept-unpaired-release object) setting)}
  @argument[object]{a @class{gtk:list-box} widget}
  @argument[setting]{a boolean whether to accept unpaired release}
  @begin{short}
    Accessor of the @slot[gtk:list-box]{accept-unpaired-release} slot of the
    @class{gtk:list-box} class.
  @end{short}
  @see-class{gtk:list-box}")

;;; --- list-box-activate-on-single-click ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "activate-on-single-click"
                                               'list-box) t)
 "The @code{activate-on-single-click} property of type @code{:boolean}
  (Read / Write) @br{}
  Activate row on a single click. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'list-box-activate-on-single-click)
      "Accessor"
      (documentation 'list-box-activate-on-single-click 'function)
 "@version{#2021-12-15}
  @syntax[]{(gtk:list-box-activate-on-single-click object) => setting}
  @syntax[]{(setf (gtk:list-box-activate-on-single-click object) setting)}
  @argument[object]{a @class{gtk:list-box} widget}
  @argument[setting]{a boolean whether to activate the row on a single click}
  @begin{short}
    Accessor of the @slot[gtk:list-box]{activate-on-single-click} slot of the
    @class{gtk:list-box} class.
  @end{short}
  The @sym{gtk:list-box-activate-on-single-click} function returns whether rows
  activate on single clicks. The
  @sym{(setf gtk:list-box-activate-on-single-click)} function sets whether rows
  activate on single clicks.

  If the @arg{setting} argument is @em{true}, rows will be activated when you
  click on them, otherwise you need to double-click.
  @see-class{gtk:list-box}")

;;; --- list-box-selection-mode --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selection-mode" 'list-box) t)
 "The @code{selection-mode} property of type @symbol{gtk:selection-mode}
  (Read / Write) @br{}
  The selection mode. @br{}
  Default value: @code{:single}")

#+liber-documentation
(setf (liber:alias-for-function 'list-box-selection-mode)
      "Accessor"
      (documentation 'list-box-selection-mode 'function)
 "@version{#2021-11-15}
  @syntax[]{(gtk:list-box-selection-mode object) => mode}
  @syntax[]{(setf (gtk:list-box-selection-mode object) mode)}
  @argument[object]{a @class{gtk:list-box} widget}
  @argument[mode]{a value of the @symbol{gtk:selection-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:list-box]{selection-mode} slot of the
    @class{gtk:list-box} class.
  @end{short}
  The @sym{gtk:list-box-selection-mode} function gets the selection mode of the
  list box. The @sym{(setf gtk:list-box-selection-mode)} function sets the
  selection mode. See the @symbol{gtk:selection-mode} enumeration for details.
  @see-class{gtk:list-box}
  @see-symbol{gtk:selection-mode}")

;;; --- list-box-show-separators -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-separators" 'list-box) t)
 "The @code{show-separators} property of type @code{:boolean} (Read / Write)
  @br{}
  Show separators between rows. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'list-box-show-separators)
      "Accessor"
      (documentation 'list-box-show-separators 'function)
 "@version{#2022-2-3}
  @syntax[]{(gtk:list-box-show-separators object) => setting}
  @syntax[]{(setf (gtk:list-box-show-separators object) setting)}
  @argument[object]{a @class{gtk:list-box} widget}
  @argument[setting]{a boolean whether to show separators between row}
  @begin{short}
    Accessor of the @slot[gtk:list-box]{show-separators} slot of the
    @class{gtk:list-box} class.
  @end{short}
  The @sym{gtk:list-box-show-separators} function returns whether the list box
  should show separators between rows. The
  @sym{(setf gtk:list-box-show-separators)} function sets whether the list box
  should show separators.
  @see-class{gtk:list-box}")

;;; ----------------------------------------------------------------------------
;;; GtkListBoxFilterFunc
;;; ----------------------------------------------------------------------------

(defcallback list-row-filter-func :boolean
    ((row (g:object list-box-row))
     (data :pointer))
  (let ((ptr (get-stable-pointer-value data)))
    (funcall ptr row)))

#+liber-documentation
(setf (liber:alias-for-symbol 'list-box-filter-func)
      "Callback"
      (liber:symbol-documentation 'list-box-filter-func)
 "@version{#2021-11-15}
  @begin{short}
    Will be called whenever the row changes or is added and lets you control
    if the row should be visible or not.
  @end{short}
  @begin{pre}
 lambda (row)
  @end{pre}
  @begin[code]{table}
    @entry[row]{A @class{gtk:list-box-row} widget that may be filtered.}
    @entry[Returns]{@em{True} if the row should be visible, @em{false}
      otherwise.}
  @end{table}
  @see-class{gtk:list-box-row}
  @see-function{gtk:list-box-set-filter-func}")

(export 'list-box-filter-func)

;;; ----------------------------------------------------------------------------
;;; GtkListBoxSortFunc
;;; ----------------------------------------------------------------------------

(defcallback list-box-sort-func :int
    ((row1 (g:object list-box-row))
     (row2 (g:object list-box-row))
     (data :pointer))
  (let ((ptr (get-stable-pointer-value data)))
    (funcall ptr row1 row2)))

#+liber-documentation
(setf (liber:alias-for-symbol 'list-box-sort-func)
      "Callback"
      (liber:symbol-documentation 'list-box-sort-func)
 "@version{#2021-11-15}
  @begin{short}
    The type of the callback function that compares two rows to determine which
    should be first.
  @end{short}
  @begin{pre}
 lambda (row1 row2)
  @end{pre}
  @begin[code]{table}
    @entry[row1]{A @class{gtk:list-box-row} widget with the first row.}
    @entry[row2]{A @class{gtk:list-box-row} widget with the second row.}
    @entry[Return]{An integer which is < 0 if @arg{row1} should be before
      @arg{row2}, 0 if they are equal and > 0 otherwise.}
  @end{table}
  @see-class{gtk:list-box-row}
  @see-function{gtk:list-box-set-sort-func}")

(export 'list-box-sort-func)

;;; ----------------------------------------------------------------------------
;;; GtkListBoxUpdateHeaderFunc
;;; ----------------------------------------------------------------------------

(defcallback list-box-update-header-func :void
    ((row (g:object list-box-row))
     (before (g:object list-box-row))
     (data :pointer))
  (let ((ptr (get-stable-pointer-value data)))
    (funcall ptr row before)))

#+liber-documentation
(setf (liber:alias-for-symbol 'list-box-update-header-func)
      "Callback"
      (liber:symbol-documentation 'list-box-update-header-func)
 "@version{#2021-11-15}
  @begin{short}
    Whenever @arg{row} changes or which row is before @arg{row} changes this is
    called, which lets you update the header on @arg{row}.
  @end{short}
  You may remove or set a new one via the @fun{gtk:list-box-set-header-func}
  function or just change the state of the current header widget.
  @begin{pre}
 lambda (row before)
  @end{pre}
  @begin[code]{table}
    @entry[row]{A @class{gtk:list-box-row} widget with the row to update.}
    @entry[before]{A @class{gtk:list-box-row} widget before @arg{row}, or
      @code{nil} if it is the first row.}
  @end{table}
  @see-class{gtk:list-box-row}
  @see-function{gtk:list-box-set-header-func}")

(export 'list-box-update-header-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_new
;;; ----------------------------------------------------------------------------

(declaim (inline list-box-new))

(defun list-box-new ()
 #+liber-documentation
 "@version{#2021-11-15}
  @return{A new @class{gtk:list-box} widget.}
  @begin{short}
    Creates a new list box.
  @end{short}
  @see-class{gtk:list-box}"
  (make-instance 'list-box))

(export 'list-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_prepend
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_prepend" list-box-prepend) :void
 #+liber-documentation
 "@version{#2022-9-7}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[child]{a @class{gtk:widget} child widget to add}
  @begin{short}
    Prepend a child widget to the list box.
  @end{short}
  If a sort function is set, the child widget will actually be inserted at the
  calculated position.
  @see-class{gtk:list-box}
  @see-class{gtk:widget}
  @see-function{gtk:list-box-insert}"
  (listbox (g:object list-box))
  (child (g:object widget)))

(export 'list-box-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_append
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_append" list-box-append) :void
 #+liber-documentation
 "@version{#2022-9-7}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[child]{a @class{gtk:widget} child widget to append}
  @begin{short}
    Append a child widget to the list box.
  @end{short}
  If a sort function is set, the widget will actually be inserted at the
  calculated position.
  @see-class{gtk:list-box}
  @see-class{gtk:widget}"
  (listbox (g:object list-box))
  (child (g:object widget)))

(export 'list-box-append)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_insert
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_insert" list-box-insert) :void
 #+liber-documentation
 "@version{#2022-9-7}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[child]{a @class{gtk:widget} child widget to add}
  @argument[position]{an integer with the position to insert the child widget
    in}
  @begin{short}
    Insert the child widget into the list box at the given position.
  @end{short}
  If a sort function is set, the child widget will actually be inserted at the
  calculated position.

  If the position is -1, or larger than the total number of items in the list
  box, then the child widget will be appended to the end.
  @see-class{gtk:list-box}
  @see-class{gtk:widget}
  @see-function{gtk:list-box-prepend}"
  (listbox (g:object list-box))
  (child (g:object widget))
  (position :int))

(export 'list-box-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_remove
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_remove" list-box-remove) :void
 #+liber-documentation
 "@version{#2022-2-3}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[child]{a @class{gtk:widget} child widget to remove}
  @short{Removes a child widget from the list box.}
  @see-class{gtk:list-box}
  @see-class{gtk:widget}"
  (box (g:object list-box))
  (child (g:object widget)))

(export 'list-box-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_select_row
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_select_row" list-box-select-row) :void
 #+liber-documentation
 "@version{#2021-11-30}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[row]{a @class{gtk:list-box-row} widget}
  @begin{short}
    Make @arg{row} the currently selected row.
  @end{short}
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}"
  (listbox (g:object list-box))
  (row (g:object list-box-row)))

(export 'list-box-select-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_unselect_row
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_unselect_row" list-box-unselect-row) :void
 #+liber-documentation
 "@version{#2021-11-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[row]{a @class{gtk:list-box-row} widget}
  @begin{short}
    Unselects a single row of the list box, if the selection mode allows it.
  @end{short}
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}"
  (listbox (g:object list-box))
  (row (g:object list-box-row)))

(export 'list-box-unselect-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_select_all
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_select_all" list-box-select-all) :void
 #+liber-documentation
 "@version{#2021-11-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @begin{short}
    Select all children of the list box, if the selection mode allows it.
  @end{short}
  @see-class{gtk:list-box}"
  (listbox (g:object list-box)))

(export 'list-box-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_unselect_all
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_unselect_all" list-box-unselect-all) :void
 "@version{#2021-11-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @begin{short}
    Unselect all children of the list box, if the selection mode allows it.
  @end{short}
  @see-class{gtk:list-box}"
  (listbox (g:object list-box)))

(export 'list-box-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_selected_row -> list-box-selected-row
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_get_selected_row" list-box-selected-row)
    (g:object list-box-row)
 "@version{#2021-11-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @return{The selected @class{gtk:list-box-row} widget.}
  @begin{short}
    Gets the selected row.
  @end{short}
  Note that the list box may allow multiple selection, in which case you should
  use the @fun{gtk:list-box-selected-foreach} function to find all selected
  rows.
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}
  @see-function{gtk:list-box-selected-foreach}"
  (listbox (g:object list-box)))

(export 'list-box-selected-row)

;;; ----------------------------------------------------------------------------
;;; GtkListBoxForeachFunc
;;; ----------------------------------------------------------------------------

(defcallback list-box-foreach-func :void
    ((listbox (g:object list-box))
     (row (g:object list-box-row))
     (data :pointer))
  (restart-case
    (let ((ptr (get-stable-pointer-value data)))
      (funcall ptr listbox row))
    (return () :report "Error in GtkListBoxForeachFunc callback." nil)))

#+liber-documentation
(setf (liber:alias-for-symbol 'list-box-foreach-func)
      "Callback"
      (liber:symbol-documentation 'list-box-foreach-func)
 "@version{#2021-12-15}
  @begin{short}
    A callback function used by the @fun{gtk:list-box-selected-foreach}
    function.
  @end{short}
  It will be called on every selected child widget of the list box.
  @begin{pre}
 lambda (listbox row)
  @end{pre}
  @begin[code]{table}
    @entry[listbox]{A @class{gtk:list-box} widget.}
    @entry[row]{A @class{gtk:list-box-row} widget.}
  @end{table}
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}
  @see-function{gtk:list-box-selected-foreach}")

(export 'list-box-foreach-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_selected_foreach
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_selected_foreach" %list-box-selected-foreach) :void
  (listbox (g:object list-box))
  (func :pointer)
  (data :pointer))

(defun list-box-selected-foreach (listbox func)
 #+liber-documentation
 "@version{#2021-11-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[func]{a @symbol{gtk:list-box-foreach-func} callback function}
  @begin{short}
    Calls a function for each selected child.
  @end{short}
  Note that the selection cannot be modified from within this function.
  @see-class{gtk:list-box}
  @see-symbol{gtk:list-box-foreach-func}"
  (with-stable-pointer (ptr func)
    (%list-box-selected-foreach listbox
                                (cffi:callback list-box-foreach-func)
                                ptr)))

(export 'list-box-selected-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_selected_rows -> list-box-selected-rows
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_get_selected_rows" list-box-selected-rows)
    (g:list-t (g:object list-box-row))
 #+liber-documentation
 "@version{#2021-12-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @return{A list containing the @class{gtk:list-box-row} widget for each
    selected row.}
  @begin{short}
    Creates a list of all selected rows in the list box.
  @end{short}
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}"
  (listbox (g:object list-box)))

(export 'list-box-selected-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_adjustment
;;; gtk_list_box_set_adjustment -> list-box-adjustment
;;; ----------------------------------------------------------------------------

(defun (setf list-box-adjustment) (adjustment listbox)
  (cffi:foreign-funcall "gtk_list_box_set_adjustment"
                        (g:object list-box) listbox
                        (g:object adjustment) adjustment
                        :void)
  adjustment)

(defcfun ("gtk_list_box_get_adjustment" list-box-adjustment)
    (g:object adjustment)
 #+liber-documentation
 "@version{#2021-11-15}
  @syntax[]{(gtk:list-box-adjustment listbox) => adjustment}
  @syntax[]{(setf (gtk:list-box-adjustment listbox) adjustment)}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[adjustment]{a @class{gtk:adjustment} object}
  @begin{short}
    Accessor of the adjustment the list box uses for vertical scrolling.
  @end{short}

  The @sym{gtk:list-box-adjustment} function gets the adjustment (if any) that
  the list box uses for vertical scrolling. The
  @sym{(setf gtk:list-box-adjustment)} function sets the adjustment.

  For instance, this is used to get the page size for @kbd{PageUp/Down} key
  handling. In the normal case when the list box is packed inside a
  @class{gtk:scrolled-window} widget the adjustment from that will be picked up
  automatically, so there is no need to manually do that.
  @see-class{gtk:list-box}
  @see-class{gtk:adjustment}
  @see-class{gtk:scrolled-window}"
  (listbox (g:object list-box)))

(export 'list-box-adjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_placeholder
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_set_placeholder" list-box-set-placeholder) :void
 #+liber-documentation
 "@version{#2021-12-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[placeholder]{a @class{gtk:widget} object}
  @begin{short}
    Sets the placeholder that is shown in the list box when it does not display
    any visible children.
  @end{short}
  @see-class{gtk:list-box}
  @see-class{gtk:widget}"
  (listbox (g:object list-box))
  (placeholder (g:object widget)))

(export 'list-box-set-placeholder)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_row_at_index -> list-box-row-at-index
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_get_row_at_index" list-box-row-at-index)
    (g:object list-box-row)
 #+liber-documentation
 "@version{#2021-11-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[index]{an integer with the index of the row}
  @return{The @class{gtk:list-box-row} widget at @arg{index}.}
  @begin{short}
    Gets the n-th child in the list box (not counting headers).
  @end{short}
  If the @arg{index} argument is negative or larger than the number of items in
  the list box, @code{nil} is returned.
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}"
  (listbox (g:object list-box))
  (index :int))

(export 'list-box-row-at-index)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_row_at_y -> list-box-row-at-y
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_get_row_at_y" list-box-row-at-y)
    (g:object list-box-row)
 #+liber-documentation
 "@version{#2021-12-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[y]{an integer with the position of the row}
  @return{The @class{gtk:list-box-row} widget for the given @arg{y} coordinate.}
  @begin{short}
    Gets the row at the y position in the list box.
  @end{short}
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}"
  (listbox (g:object list-box))
  (y :int))

(export 'list-box-row-at-y)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_invalidate_filter
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_invalidate_filter" list-box-invalidate-filter) :void
 #+liber-documentation
 "@version{#2021-11-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @begin{short}
    Update the filtering for all rows.
  @end{short}
  Call this when the result of the filter function on the list box is changed
  due to an external factor. For instance, this would be used if the filter
  function just looked for a specific search string and the entry with the
  search string has changed.
  @see-class{gtk:list-box}
  @see-function{gtk:list-box-set-filter-func}"
  (listbox (g:object list-box)))

(export 'list-box-invalidate-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_invalidate_headers
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_invalidate_headers" list-box-invalidate-headers)
    :void
 #+liber-documentation
 "@version{#2021-11-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @begin{short}
    Update the separators for all rows.
  @end{short}
  Call this when the result of the header function on the list box is changed
  due to an external factor.
  @see-class{gtk:list-box}
  @see-function{gtk:list-box-set-header-func}"
  (listbox (g:object list-box)))

(export 'list-box-invalidate-headers)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_invalidate_sort
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_invalidate_sort" list-box-invalidate-sort) :void
 #+liber-documentation
 "@version{#2021-11-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @begin{short}
    Update the sorting for all rows.
  @end{short}
  Call this when the result of the sort function on the list box is changed due
  to an external factor.
  @see-class{gtk:list-box}
  @see-function{gtk:list-box-set-sort-func}"
  (listbox (g:object list-box)))

(export 'list-box-invalidate-sort)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_filter_func
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_set_filter_func" %list-box-set-filter-func) :void
  (listbox (g:object list-box))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun list-box-set-filter-func (listbox func)
 #+liber-documentation
 "@version{#2021-11-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[func]{a @symbol{gtk:list-box-filter-func} callback function that
    lets you filter which rows to show}
  @begin{short}
    By setting a filter function on the list box one can decide dynamically
    which of the rows to show.
  @end{short}
  For instance, to implement a search function on a list that filters the
  original list to only show the matching rows.

  The @arg{func} function will be called for each row after the call, and it
  will continue to be called each time a row changes via the
  @fun{gtk:list-box-row-changed} function, or when the
  @fun{gtk:list-box-invalidate-filter} function is called.

  Note that using a filter function is incompatible with using a model,
  see the @fun{gtk:list-box-bind-model} function.
  @see-class{gtk:list-box}
  @see-function{gtk:list-box-row-changed}
  @see-function{gtk:list-box-invalidate-filter}
  @see-function{gtk:list-box-bind-model}"
  (%list-box-set-filter-func listbox
          (cffi:callback list-box-filter-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'list-box-set-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_header_func
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_set_header_func" %list-box-set-header-func) :void
  (listbox (g:object list-box))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun list-box-set-header-func (listbox func)
 #+liber-documentation
 "@version{#2021-11-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[func]{a @symbol{gtk:list-box-update-header-func} callback function
    that lets you add row headers}
  @begin{short}
    By setting a header function on the list box one can dynamically add headers
    in front of rows, depending on the contents of the row and its position in
    the list box.
  @end{short}
  For instance, one could use it to add headers in front of the first item of a
  new kind, in a list sorted by the kind.

  The @arg{func} callback function can look at the current header widget using
  the @fun{gtk:list-box-row-header} function and either update the state of the
  widget as needed, or set a new one using the
  @sym{(setf gtk:list-box-row-header)} function. If no header is needed, set
  the header to @code{nil}.

  Note that you may get many calls of the @arg{func} callback function to this
  for a particular row when e.g. changing things that do not affect the header.
  In this case it is important for performance to not blindly replace an
  existing header with an identical one.

  The @arg{func} callback function will be called for each row after the call,
  and it will continue to be called each time a row changes via the
  @fun{gtk:list-box-row-changed} function, and when the row before changes
  either by the @fun{gtk:list-box-row-changed} function on the previous row, or
  when the previous row becomes a different row. It is also called for all rows
  when the @fun{gtk:list-box-invalidate-headers} function is called.
  @see-class{gtk:list-box}
  @see-symbol{gtk:list-box-update-header-func}
  @see-function{gtk:list-box-row-header}
  @see-function{gtk:list-box-row-changed}
  @see-function{gtk:list-box-invalidate-headers}"
  (%list-box-set-header-func listbox
          (cffi:callback list-box-update-header-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'list-box-set-header-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_sort_func
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_set_sort_func" %list-box-set-sort-func) :void
  (listbox (g:object list-box))
  (func :pointer)
  (user-data :pointer)
  (destroy :pointer))

(defun list-box-set-sort-func (listbox func)
 #+liber-documentation
 "@version{#2021-12-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[func]{a @symbol{gtk:list-box-sort-func} callback function for the
    sort function}
  @begin{short}
    By setting a sort function on the list box one can dynamically reorder the
    rows of the list, based on the contents of the rows.
  @end{short}

  The @arg{func} callback function will be called for each row after the call,
  and will continue to be called each time a row changes via the
  @fun{gtk:list-box-row-changed} function, and when the
  @fun{gtk:list-box-invalidate-sort} function is called.

  Note that using a sort function is incompatible with using a model. See the
  @fun{gtk:list-box-bind-model} function.
  @see-class{gtk:list-box}
  @see-symbol{gtk:list-box-sort-func}
  @see-function{gtk:list-box-row-changed}
  @see-function{gtk:list-box-invalidate-sort}
  @see-function{gtk:list-box-bind-model}"
  (%list-box-set-sort-func listbox
          (cffi:callback list-box-sort-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'list-box-set-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_drag_highlight_row
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_drag_highlight_row" list-box-drag-highlight-row)
    :void
 #+liber-documentation
 "@version{#2021-11-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[row]{a @class{gtk:list-box-row} widget}
  @begin{short}
    This is a helper function for implementing drag and drop onto a list box.
  @end{short}
  The passed in row will be highlighted via the @fun{gtk:drag-highlight}
  function, and any previously highlighted row will be unhighlighted. The row
  will also be unhighlighted when the widget gets a drag leave event.
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}
  @see-function{gtk:drag-highlight}"
  (listbox (g:object list-box))
  (row (g:object list-box-row)))

(export 'list-box-drag-highlight-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_drag_unhighlight_row
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_drag_unhighlight_row" list-box-drag-unhighlight-row)
    :void
 #+liber-documentation
 "@version{#2021-11-15}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @begin{short}
    If a row has previously been highlighted via the
    @fun{gtk:list-box-drag-highlight-row} function it will have the highlight
    removed.
  @end{short}
  @see-class{gtk:list-box}
  @see-function{gtk:list-box-drag-highlight-row}"
  (listbox (g:object list-box)))

(export 'list-box-drag-unhighlight-row)

;;; ----------------------------------------------------------------------------
;;; GtkListBoxCreateWidgetFunc ()
;;; ----------------------------------------------------------------------------

;; TODO: Check this. Perhaps it is better to pass a GObject for item.

(defcallback list-box-create-widget-func (g:object widget)
    ((item :pointer)
     (data :pointer))
  (let ((ptr (get-stable-pointer-value data)))
    (funcall ptr item)))

#+liber-documentation
(setf (liber:alias-for-symbol 'list-box-create-widget-func)
      "Callback"
      (liber:symbol-documentation 'list-box-create-widget-func)
 "@version{#2022-9-8}
  @begin{short}
    Called for list boxes that are bound to a @class{g-list-model} object with
    the @fun{gtk:list-box-bind-model} function for each item that gets added to
    the model.
  @end{short}
  If the widget returned is not a @class{gtk:list-box-row} widget, then the
  widget will be inserted as the child of an intermediate
  @class{gtk:list-box-row} widget.
  @begin{pre}
lambda (item)
  @end{pre}
  @begin[code]{table}
    @entry[item]{A pointer to the item from the model for which to create a
       widget for.}
    @entry[Returns]{A @class{gtk:widget} object that represents @arg{item}.}
  @end{table}
  @see-class{gtk:list-box}
  @see-class{g-list-model}
  @see-function{gtk:list-box-bind-model}")

(export 'list-box-create-widget-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_bind_model
;;; ----------------------------------------------------------------------------

;; TODO: Check the implementation for value of NIL for MODEL or FUNC.

(defcfun ("gtk_list_box_bind_model" %list-box-bind-model) :void
  (listbox (g:object list-box))
  (model (g:object g-list-model))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun list-box-bind-model (listbox model func)
 #+liber-documentation
 "@version{#2022-9-7}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[model]{a @class{g-list-model} object to be bound to @arg{listbox}}
  @argument[func]{a @symbol{gtk:list-box-create-widget-func} callback function
    that creates widgets for items or @code{nil} in case you also passed
    @code{nil} as @arg{model}}
  @begin{short}
    Binds a model to the list box.
  @end{short}
  If the list box was already bound to a model, that previous binding is
  destroyed.

  The contents of the list box are cleared and then filled with widgets that
  represent items from the model. The list box is updated whenever the model
  changes. If the @arg{model} argument is @code{nil}, the list box is left
  empty.

  It is undefined to add or remove widgets directly, for example, with the
  @fun{gtk:list-box-insert} function, while the list box is bound to a model.

  Note that using a model is incompatible with the filtering and sorting
  functionality in the list box. When using a model, filtering and sorting
  should be implemented by the model.
  @see-class{gtk:list-box}
  @see-class{g-list-model}
  @see-symbol{gtk:list-box-create-widget-func}
  @see-function{gtk:list-box-insert}"
  (%list-box-bind-model listbox
          model
          (cffi:callback list-box-create-widget-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'list-box-bind-model)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_new
;;; ----------------------------------------------------------------------------

(declaim (inline list-box-row-new))

(defun list-box-row-new ()
 #+liber-documentation
 "@version{#2021-11-15}
  @return{A new @class{gtk:list-box-row} widget.}
  @begin{short}
    Creates a new list box row, to be used as a child widget of a list box.
  @end{short}
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}"
  (make-instance 'list-box-row))

(export 'list-box-row-new)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_changed
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_row_changed" list-box-row-changed) :void
 #+liber-documentation
 "@version{#2021-11-15}
  @argument[row]{a @class{gtk:list-box-row} widget}
  @begin{short}
    Marks @arg{row} as changed, causing any state that depends on this to be
    updated.
  @end{short}
  This affects sorting, filtering and headers.

  Note that calls to this method must be in sync with the data used for the row
  functions. For instance, if the list box is mirroring some external data set,
  and *two* rows changed in the external data set then when you call the
  @sym{gtk:list-box-row-changed} function on the first row the sort function
  must only read the new data for the first of the two changed rows, otherwise
  the resorting of the rows will be wrong.

  This generally means that if you do not fully control the data model you have
  to duplicate the data that affects the list box row functions into the row
  widgets themselves. Another alternative is to call the
  @fun{gtk:list-box-row-invalidate-sort} function on any model change, but that is
  more expensive.
  @see-class{gtk:list-box-row}
  @see-class{gtk:list-box-row}
  @see-function{gtk:list-box-row-invalidate-sort}"
  (row (g:object list-box-row)))

(export 'list-box-row-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_is_selected
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_row_is_selected" list-box-row-is-selected) :boolean
 #+liber-documentation
 "@version{#2021-7-13}
  @argument[row]{a @class{gtk:list-box-row} widget}
  @begin{short}
    Returns a boolean whether the child is currently selected in its list box
    container.
  @end{short}
  @see-class{gtk:list-box-row}
  @see-class{gtk:list-box-row}"
  (row (g:object list-box-row)))

(export 'list-box-row-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_get_header
;;; gtk_list_box_row_set_header -> list-box-row-header
;;; ----------------------------------------------------------------------------

(defun (setf list-box-row-header) (header row)
  (cffi:foreign-funcall "gtk_list_box_row_set_header"
                        (g:object list-box-row) row
                        (g:object widget) header
                        :void)
  header)

(defcfun ("gtk_list_box_row_get_header" list-box-row-header)
    (g:object widget)
 #+liber-documentation
 "@version{#2021-11-15}
  @syntax[]{(gtk:list-box-row-header row) => header}
  @syntax[]{(setf (gtk:list-box-row-header row) header)}
  @argument[row]{a @class{gtk:list-box-row} widget}
  @argument[header]{a @class{gtk:widget} object}
  @begin{short}
    Accessor of the header widget of a list box row.
  @end{short}

  The @sym{gtk:list-box-row-header} function returns the current header of the
  list box row. This can be used in a @symbol{gtk:list-box-update-header-func}
  callback function to see if there is a header set already, and if so to update
  the state of it.

  The @sym{(setf gtk:list-box-row-header)} function sets the current header of
  the list box row. This is only allowed to be called from a
  @symbol{gtk:list-box-update-header-func} callback function. It will replace
  any existing header in the row, and be shown in front of the row in the list
  box.
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}
  @see-class{gtk:widget}
  @see-symbol{gtk:list-box-update-header-func}"
  (row (g:object list-box-row)))

(export 'list-box-row-header)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_get_index -> list-box-row-index
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_row_get_index" list-box-row-index) :int
 #+liber-documentation
 "@version{#2021-11-30}
  @argument[row]{a @class{gtk:list-box-row} widget}
  @return{An integer with the index of the row in the list box, or -1 if the
    row is not in the list box.}
  @begin{short}
    Gets the current index of the row in its list box.
  @end{short}
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}"
  (row (g:object list-box-row)))

(export 'list-box-row-index)

;;; --- End of file gtk4.list-box.lisp -----------------------------------------
