;;; ----------------------------------------------------------------------------
;;; gtk4.list-box.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2024 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GtkListBoxRow
;;;
;;; Accessors
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
;;;     gtk_list_box_row_new
;;;     gtk_list_box_row_changed
;;;     gtk_list_box_row_is_selected
;;;     gtk_list_box_row_get_header
;;;     gtk_list_box_row_set_header
;;;     gtk_list_box_row_get_index
;;;
;;; Properties
;;;
;;;     activatable
;;;     child
;;;     selectable
;;;
;;; Signals
;;;
;;;     activate
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkActionable
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Types and Values
;;;
;;;     GtkListBox
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
;;;     gtk_list_box_remove_all                            Since 4.12
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
;;; Properties
;;;
;;;     accept-unpaired-release
;;;     activate-on-single-click
;;;     selection-mode
;;;     show-separators
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
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ├── GtkListBox
;;;             ╰── GtkListBoxRow
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkListBoxRow
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkListBoxRow" list-box-row
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
 "@version{2024-4-13}
  @begin{short}
    The @class{gtk:list-box-row} widget is a child widget for the
    @class{gtk:list-box} widget.
  @end{short}
  The @class{gtk:list-box-row} widget can be marked as activatable or
  selectable. If a row is activatable, the @code{\"row-activated\"} signal will
  be emitted for it when the user tries to activate it. If it is selectable,
  the row will be marked as selected when the user tries to select it.
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (row)    :action
      @end{pre}
      This is a keybinding signal, which will cause this row to be activated.
      If you want to be notified when the user activates a row (by key or not),
      use the @code{\"row-activated\"} signal on the parent @class{gtk:list-box}
      widget of the row.
      @begin[code]{table}
        @entry[row]{The @class{gtk:list-box-row} widget on which the signal is
          emitted.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:list-box-row-new}
  @see-slot{gtk:list-box-row-activatable}
  @see-slot{gtk:list-box-row-child}
  @see-slot{gtk:list-box-row-selectable}
  @see-class{gtk:list-box}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:list-box-row-activatable -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "activatable"
                                               'list-box-row) t)
 "The @code{activatable} property of type @code{:boolean} (Read / Write) @br{}
  Determines whether the @code{\"row-activated\"} signal will be emitted for
  this row. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'list-box-row-activatable)
      "Accessor"
      (documentation 'list-box-row-activatable 'function)
 "@version{2024-4-13}
  @syntax[]{(gtk:list-box-row-activatable object) => activatable}
  @syntax[]{(setf (gtk:list-box-row-activatable object) activatable)}
  @argument[object]{a @class{gtk:list-box-row} widget}
  @argument[activatable]{@em{true} to mark the row as activatable}
  @begin{short}
    Accessor of the @slot[gtk:list-box-row]{activatable} slot of the
    @class{gtk:list-box-row} class.
  @end{short}
  The @fun{gtk:list-box-row-activatable} function gets the value of the
  @slot[gtk:list-box-row]{activatable} property for this row. The
  @setf{gtk:list-box-row-activatable} function sets the property.
  @see-class{gtk:list-box-row}")

;;; --- gtk:list-box-row-child -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'list-box-row) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'list-box-row-child)
      "Accessor"
      (documentation 'list-box-row-child 'function)
 "@version{2024-4-13}
  @syntax[]{(gtk:list-box-row-child object) => child}
  @syntax[]{(setf (gtk:list-box-row-child object) child)}
  @argument[object]{a @class{gtk:list-box-row} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:list-box-row]{child} slot of the
    @class{gtk:list-box-row} class.
  @end{short}
  Gets or sets the child widget of the list box row.
  @see-class{gtk:list-box-row}
  @see-class{gtk:widget}")

;;; --- gtk:list-box-row-selectable --------------------------------------------

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
 "@version{2024-4-13}
  @syntax[]{(gtk:list-box-row-selectable object) => selectable}
  @syntax[]{(setf (gtk:list-box-row-selectable object) selectable)}
  @argument[object]{a @class{gtk:list-box-row} widget}
  @argument[selectable]{@em{true} to mark the row as selectable}
  @begin{short}
    Accessor of the @slot[gtk:list-box-row]{selectable} slot of the
    @class{gtk:list-box} class.
  @end{short}
  The @fun{gtk:list-box-row-selectable} function gets the value of the
  @slot[gtk:list-box-row]{selectable} property for this row. The
  @setf{gtk:list-box-row-selectable} function sets the property.
  @see-class{gtk:list-box-row}")

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_new
;;; ----------------------------------------------------------------------------

(declaim (inline list-box-row-new))

(defun list-box-row-new ()
 #+liber-documentation
 "@version{2024-4-13}
  @return{The new @class{gtk:list-box-row} widget.}
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

(cffi:defcfun ("gtk_list_box_row_changed" list-box-row-changed) :void
 #+liber-documentation
 "@version{#2024-4-13}
  @argument[row]{a @class{gtk:list-box-row} widget}
  @begin{short}
    Marks the list box row as changed, causing any state that depends on this
    to be updated.
  @end{short}
  This affects sorting, filtering and headers.

  Note that calls to this method must be in sync with the data used for the row
  functions. For instance, if the list box is mirroring some external data set,
  and *two* rows changed in the external data set then when you call the
  @fun{gtk:list-box-row-changed} function on the first row the sort function
  must only read the new data for the first of the two changed rows, otherwise
  the resorting of the rows will be wrong.

  This generally means that if you do not fully control the data model you have
  to duplicate the data that affects the list box row functions into the row
  widgets themselves. Another alternative is to call the
  @fun{gtk:list-box-invalidate-sort} function on any model change, but that is
  more expensive.
  @see-class{gtk:list-box-row}
  @see-class{gtk:list-box-row}
  @see-function{gtk:list-box-invalidate-sort}"
  (row (g:object list-box-row)))

(export 'list-box-row-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_is_selected
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_row_is_selected" list-box-row-is-selected) :boolean
 #+liber-documentation
 "@version{#2024-4-13}
  @argument[row]{a @class{gtk:list-box-row} widget}
  @return{@em{True} if @arg{row} is selected.}
  @begin{short}
    Returns a boolean whether the row is currently selected in its list box.
  @end{short}
  @see-class{gtk:list-box-row}"
  (row (g:object list-box-row)))

(export 'list-box-row-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_get_header
;;; gtk_list_box_row_set_header
;;; ----------------------------------------------------------------------------

(defun (setf list-box-row-header) (header row)
  (cffi:foreign-funcall "gtk_list_box_row_set_header"
                        (g:object list-box-row) row
                        (g:object widget) header
                        :void)
  header)

(cffi:defcfun ("gtk_list_box_row_get_header" list-box-row-header)
    (g:object widget)
 #+liber-documentation
 "@version{#2024-4-13}
  @syntax[]{(gtk:list-box-row-header row) => header}
  @syntax[]{(setf (gtk:list-box-row-header row) header)}
  @argument[row]{a @class{gtk:list-box-row} widget}
  @argument[header]{a @class{gtk:widget} object}
  @begin{short}
    The @fun{gtk:list-box-row-header} function returns the current header of
    the list box row.
  @end{short}
  This can be used in a @symbol{gtk:list-box-update-header-func} callback
  function to see if there is a header set already, and if so to update the
  state of it.

  The @setf{gtk:list-box-row-header} function sets the current header. This is
  only allowed to be called from a @symbol{gtk:list-box-update-header-func}
  callback function. It will replace any existing header in the row, and be
  shown in front of the row in the list box.
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}
  @see-class{gtk:widget}
  @see-symbol{gtk:list-box-update-header-func}"
  (row (g:object list-box-row)))

(export 'list-box-row-header)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_get_index
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_row_get_index" list-box-row-index) :int
 #+liber-documentation
 "@version{#2024-4-13}
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

;;; ----------------------------------------------------------------------------
;;; GtkListBox
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkListBox" list-box
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
 "@version{2024-4-12}
  @begin{short}
    The @class{gtk:list-box} widget is a vertical list box that contains
    @class{gtk:list-box-row} children.
  @end{short}
  These rows can by dynamically sorted and filtered, and headers can be added
  dynamically depending on the row content. It also allows keyboard and mouse
  navigation and selection like a typical list.

  @image[list-box]{Figure: GtkListBox}

  Using the @class{gtk:list-box} widget is often an alternative to the
  @class{gtk:tree-view} widget, especially when the list contents has a more
  complicated layout than what is allowed by a @class{gtk:cell-renderer}
  object, or when the contents is interactive, i.e. has a button in it.

  Although a @class{gtk:list-box} widget must have only @class{gtk:list-box-row}
  children you can add any kind of widget to it via the
  @fun{gtk:list-box-prepend}, @fun{gtk:list-box-append}, and
  @fun{gtk:list-box-insert} functions, and a @class{gtk:list-box-row} widget
  will automatically be inserted between the list and the widget.

  The @class{gtk:list-box-row} widget can be marked as activatable or
  selectable. If a row is activatable, the @code{\"row-activated\"} signal will
  be emitted for it when the user tries to activate it. If it is selectable,
  the row will be marked as selected when the user tries to select it.
  @begin[CSS Nodes]{dictionary}
    @begin{pre}
list
 ╰── row[.activatable]
    @end{pre}
    The @class{gtk:list-box} implementation uses a single CSS node named
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
        @entry[listbox]{The @class{gtk:list-box} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
lambda (listbox step count)    :action
      @end{pre}
      @begin[code]{table}
        @entry[listbox]{The @class{gtk:list-box} widget on which the signal is
          emitted.}
        @entry[step]{The value of the @symbol{gtk:movement-step} enumeration.}
        @entry[count]{The integer with the number of steps.}
      @end{table}
    @subheading{The \"row-activated\" signal}
      @begin{pre}
lambda (listbox row)    :run-last
      @end{pre}
      The signal is emitted when a row has been activated by the user.
      @begin[code]{table}
        @entry[listbox]{The @class{gtk:list-box} widget on which the signal is
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
      and you should use the @code{\"selected-rows-changed\"} signal instead.
      @begin[code]{table}
        @entry[listbox]{The @class{gtk:list-box} widget on which the signal is
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
        @entry[listbox]{The @class{gtk:list-box} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"selected-rows-changed\" signal}
      @begin{pre}
lambda (listbox)    :run-first
      @end{pre}
      The signal is emitted when the set of selected rows changes.
      @begin[code]{table}
        @entry[listbox]{The @class{gtk:list-box} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"toggle-cursor-row\" signal}
      @begin{pre}
lambda (listbox)    :action
      @end{pre}
      @begin[code]{table}
        @entry[listbox]{The @class{gtk:list-box} widget on which the signal is
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
        @entry[listbox]{The @class{gtk:list-box} widget on which the signal is
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

;;; --- gtk:list-box-accept-unpaired-release -----------------------------------

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
 "@version{2024-4-12}
  @syntax[]{(gtk:list-box-accept-unpaired-release object) => setting}
  @syntax[]{(setf (gtk:list-box-accept-unpaired-release object) setting)}
  @argument[object]{a @class{gtk:list-box} widget}
  @argument[setting]{a boolean whether to accept unpaired release}
  @begin{short}
    Accessor of the @slot[gtk:list-box]{accept-unpaired-release} slot of the
    @class{gtk:list-box} class.
  @end{short}
  @see-class{gtk:list-box}")

;;; --- gtk:list-box-activate-on-single-click ----------------------------------

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
 "@version{2024-4-12}
  @syntax[]{(gtk:list-box-activate-on-single-click object) => setting}
  @syntax[]{(setf (gtk:list-box-activate-on-single-click object) setting)}
  @argument[object]{a @class{gtk:list-box} widget}
  @argument[setting]{a boolean whether to activate the row on a single click}
  @begin{short}
    Accessor of the @slot[gtk:list-box]{activate-on-single-click} slot of the
    @class{gtk:list-box} class.
  @end{short}
  The @fun{gtk:list-box-activate-on-single-click} function returns whether rows
  activate on single clicks. The
  @setf{gtk:list-box-activate-on-single-click} function sets whether rows
  activate on single clicks.

  If the @arg{setting} argument is @em{true}, rows will be activated when you
  click on them, otherwise you need to double-click.
  @see-class{gtk:list-box}")

;;; --- gtk:list-box-selection-mode --------------------------------------------

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
 "@version{2024-4-12}
  @syntax[]{(gtk:list-box-selection-mode object) => mode}
  @syntax[]{(setf (gtk:list-box-selection-mode object) mode)}
  @argument[object]{a @class{gtk:list-box} widget}
  @argument[mode]{a value of the @symbol{gtk:selection-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:list-box]{selection-mode} slot of the
    @class{gtk:list-box} class.
  @end{short}
  The @fun{gtk:list-box-selection-mode} function gets the selection mode of the
  list box. The @setf{gtk:list-box-selection-mode} function sets the selection
  mode. See the @symbol{gtk:selection-mode} enumeration for details.
  @see-class{gtk:list-box}
  @see-symbol{gtk:selection-mode}")

;;; --- gtk:list-box-show-separators -------------------------------------------

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
 "@version{2024-4-12}
  @syntax[]{(gtk:list-box-show-separators object) => setting}
  @syntax[]{(setf (gtk:list-box-show-separators object) setting)}
  @argument[object]{a @class{gtk:list-box} widget}
  @argument[setting]{a boolean whether to show separators between row}
  @begin{short}
    Accessor of the @slot[gtk:list-box]{show-separators} slot of the
    @class{gtk:list-box} class.
  @end{short}
  The @fun{gtk:list-box-show-separators} function returns whether the list box
  should show separators between rows. The @setf{gtk:list-box-show-separators}
  function sets whether the list box should show separators.
  @see-class{gtk:list-box}")

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_new
;;; ----------------------------------------------------------------------------

(declaim (inline list-box-new))

(defun list-box-new ()
 #+liber-documentation
 "@version{2024-4-13}
  @return{The new @class{gtk:list-box} widget.}
  @short{Creates a new list box.}
  @see-class{gtk:list-box}"
  (make-instance 'list-box))

(export 'list-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_prepend
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_prepend" list-box-prepend) :void
 #+liber-documentation
 "@version{#2024-4-13}
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

(cffi:defcfun ("gtk_list_box_append" list-box-append) :void
 #+liber-documentation
 "@version{#2024-4-13}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[child]{a @class{gtk:widget} child widget to append}
  @begin{short}
    Append a child widget to the list box.
  @end{short}
  If a sort function is set, the widget will actually be inserted at the
  calculated position.
  @see-class{gtk:list-box}
  @see-class{gtk:widget}
  @see-function{gtk:list-box-insert}"
  (listbox (g:object list-box))
  (child (g:object widget)))

(export 'list-box-append)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_insert
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_insert" list-box-insert) :void
 #+liber-documentation
 "@version{#2024-4-13}
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
  @see-function{gtk:list-box-append}
  @see-function{gtk:list-box-prepend}"
  (listbox (g:object list-box))
  (child (g:object widget))
  (position :int))

(export 'list-box-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_remove" list-box-remove) :void
 #+liber-documentation
 "@version{#2024-4-13}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[child]{a @class{gtk:widget} child widget to remove}
  @short{Removes a child widget from the list box.}
  @see-class{gtk:list-box}
  @see-class{gtk:widget}"
  (listbox (g:object list-box))
  (child (g:object widget)))

(export 'list-box-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_remove_all
;;; ----------------------------------------------------------------------------

#+gtk-4-12
(cffi:defcfun ("gtk_list_box_remove_all" list-box-remove-all) :void
 #+liber-documentation
 "@version{#2024-4-13}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @short{Removes all child widgets from the list box.}

  Since 4.12
  @see-class{gtk:list-box}"
  (listbox (g:object list-box)))

#+gtk-4-12
(export 'list-box-remove-all)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_select_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_select_row" list-box-select-row) :void
 #+liber-documentation
 "@version{#2024-4-13}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[row]{a @class{gtk:list-box-row} widget}
  @short{Make the list box row the currently selected row.}
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}"
  (listbox (g:object list-box))
  (row (g:object list-box-row)))

(export 'list-box-select-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_unselect_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_unselect_row" list-box-unselect-row) :void
 #+liber-documentation
 "@version{#2024-4-13}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[row]{a @class{gtk:list-box-row} widget}
  @begin{short}
    Unselects a single row of the list box, if the selection mode allows it.
  @end{short}
  See the @fun{gtk:list-box-selection-mode} function and the
  @symbol{gtk:selection-mode} enumeration for more information about selection
  modes of the list box.
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}
  @see-symbol{gtk:selection-mode}
  @see-function{gtk:list-box-selection-mode}"
  (listbox (g:object list-box))
  (row (g:object list-box-row)))

(export 'list-box-unselect-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_select_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_select_all" list-box-select-all) :void
 #+liber-documentation
 "@version{#2024-4-13}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @begin{short}
    Select all children of the list box, if the selection mode allows it.
  @end{short}
  See the @fun{gtk:list-box-selection-mode} function and the
  @symbol{gtk:selection-mode} enumeration for more information about selection
  modes of the list box.
  @see-class{gtk:list-box}
  @see-symbol{gtk:selection-mode}
  @see-function{gtk:list-box-selection-mode}
  @see-function{gtk:list-box-unselect-all}"
  (listbox (g:object list-box)))

(export 'list-box-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_unselect_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_unselect_all" list-box-unselect-all) :void
 "@version{#2024-4-13}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @begin{short}
    Unselect all children of the list box, if the selection mode allows it.
  @end{short}
  See the @fun{gtk:list-box-selection-mode} function and the
  @symbol{gtk:selection-mode} enumeration for more information about selection
  modes of the list box.
  @see-class{gtk:list-box}
  @see-symbol{gtk:selection-mode}
  @see-function{gtk:list-box-selection-mode}
  @see-function{gtk:list-box-select-all}"
  (listbox (g:object list-box)))

(export 'list-box-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_selected_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_get_selected_row" list-box-selected-row)
    (g:object list-box-row)
 "@version{#2024-4-13}
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

(cffi:defcallback list-box-foreach-func :void
    ((listbox (g:object list-box))
     (row (g:object list-box-row))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func listbox row)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'list-box-foreach-func)
      "Callback"
      (liber:symbol-documentation 'list-box-foreach-func)
 "@version{#2024-4-13}
  @syntax{lambda (listbox row)}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[row]{a @class{gtk:list-box-row} widget}
  @begin{short}
    A callback function used by the @fun{gtk:list-box-selected-foreach}
    function.
  @end{short}
  It will be called on every selected child widget of the list box.
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}
  @see-function{gtk:list-box-selected-foreach}")

(export 'list-box-foreach-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_selected_foreach
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_selected_foreach" %list-box-selected-foreach) :void
  (listbox (g:object list-box))
  (func :pointer)
  (data :pointer))

(defun list-box-selected-foreach (listbox func)
 #+liber-documentation
 "@version{#2024-4-13}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[func]{a @symbol{gtk:list-box-foreach-func} callback function}
  @begin{short}
    Calls a function for each selected child.
  @end{short}
  Note that the selection cannot be modified from within this function.
  @see-class{gtk:list-box}
  @see-symbol{gtk:list-box-foreach-func}"
  (glib:with-stable-pointer (ptr func)
    (%list-box-selected-foreach listbox
                                (cffi:callback list-box-foreach-func)
                                ptr)))

(export 'list-box-selected-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_selected_rows
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_get_selected_rows" list-box-selected-rows)
    (g:list-t (g:object list-box-row))
 #+liber-documentation
 "@version{#2024-4-13}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @return{The list containing the @class{gtk:list-box-row} widget for each
    selected row.}
  @short{Creates a list of all selected rows in the list box.}
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}"
  (listbox (g:object list-box)))

(export 'list-box-selected-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_adjustment
;;; gtk_list_box_set_adjustment
;;; ----------------------------------------------------------------------------

(defun (setf list-box-adjustment) (adjustment listbox)
  (cffi:foreign-funcall "gtk_list_box_set_adjustment"
                        (g:object list-box) listbox
                        (g:object adjustment) adjustment
                        :void)
  adjustment)

(cffi:defcfun ("gtk_list_box_get_adjustment" list-box-adjustment)
    (g:object adjustment)
 #+liber-documentation
 "@version{#2024-4-13}
  @syntax[]{(gtk:list-box-adjustment listbox) => adjustment}
  @syntax[]{(setf (gtk:list-box-adjustment listbox) adjustment)}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[adjustment]{a @class{gtk:adjustment} object}
  @begin{short}
    The @fun{gtk:list-box-adjustment} function gets the adjustment (if any)
    that the list box uses for vertical scrolling.
  @end{short}
  The @setf{gtk:list-box-adjustment} function sets the adjustment.

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

(cffi:defcfun ("gtk_list_box_set_placeholder" list-box-set-placeholder) :void
 #+liber-documentation
 "@version{#2024-4-13}
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
;;; gtk_list_box_get_row_at_index
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_get_row_at_index" list-box-row-at-index)
    (g:object list-box-row)
 #+liber-documentation
 "@version{#2024-4-13}
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
;;; gtk_list_box_get_row_at_y
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_get_row_at_y" list-box-row-at-y)
    (g:object list-box-row)
 #+liber-documentation
 "@version{#2024-4-13}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[y]{an integer with the position of the row}
  @return{The @class{gtk:list-box-row} widget for the given @arg{y} coordinate.}
  @short{Gets the row at the y position in the list box.}
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}"
  (listbox (g:object list-box))
  (y :int))

(export 'list-box-row-at-y)

;;; ----------------------------------------------------------------------------
;;; GtkListBoxFilterFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback list-row-filter-func :boolean
    ((row (g:object list-box-row))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func row)
      (return-true () :report "Return T" t)
      (return-false () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'list-box-filter-func)
      "Callback"
      (liber:symbol-documentation 'list-box-filter-func)
 "@version{#2024-4-13}
  @syntax{lambda (row) => result}
  @argument[row]{a @class{gtk:list-box-row} widget that may be filtered}
  @argument[result]{@em{true} if the row should be visible, @em{false}
    otherwise}
  @begin{short}
    Will be called whenever the row changes or is added and lets you control
    if the row should be visible or not.
  @end{short}
  @see-class{gtk:list-box-row}
  @see-function{gtk:list-box-set-filter-func}")

(export 'list-box-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_filter_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_set_filter_func" %list-box-set-filter-func) :void
  (listbox (g:object list-box))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun list-box-set-filter-func (listbox func)
 #+liber-documentation
 "@version{#2024-4-13}
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
;;; gtk_list_box_invalidate_filter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_invalidate_filter" list-box-invalidate-filter)
    :void
 #+liber-documentation
 "@version{#2024-4-13}
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
;;; GtkListBoxUpdateHeaderFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback list-box-update-header-func :void
    ((row (g:object list-box-row))
     (before (g:object list-box-row))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func row before)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'list-box-update-header-func)
      "Callback"
      (liber:symbol-documentation 'list-box-update-header-func)
 "@version{#2024-4-13}
  @syntax{lambda (row before)}
  @argument[row]{a @class{gtk:list-box-row} widget with the row to update}
  @argument[before]{a @class{gtk:list-box-row} widget before @arg{row}, or
    @code{nil} if it is the first row}
  @begin{short}
    Whenever @arg{row} changes or which row is before @arg{row} changes this is
    called, which lets you update the header on @arg{row}.
  @end{short}
  You may remove or set a new one via the @fun{gtk:list-box-set-header-func}
  function or just change the state of the current header widget.
  @see-class{gtk:list-box-row}
  @see-function{gtk:list-box-set-header-func}")

(export 'list-box-update-header-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_header_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_set_header_func" %list-box-set-header-func) :void
  (listbox (g:object list-box))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun list-box-set-header-func (listbox func)
 #+liber-documentation
 "@version{#2024-4-13}
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
  widget as needed, or set a new one using the @setf{gtk:list-box-row-header}
  function. If no header is needed, set the header to @code{nil}.

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
;;; gtk_list_box_invalidate_headers
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_invalidate_headers" list-box-invalidate-headers)
    :void
 #+liber-documentation
 "@version{#2024-4-13}
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
;;; GtkListBoxSortFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback list-box-sort-func :int
    ((row1 (g:object list-box-row))
     (row2 (g:object list-box-row))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func row1 row2)
      (return<0 () :report "Return -1" -1)
      (return=0 () :report "Return  0" 0)
      (return>0 () :report "Return  1" 1))))

#+liber-documentation
(setf (liber:alias-for-symbol 'list-box-sort-func)
      "Callback"
      (liber:symbol-documentation 'list-box-sort-func)
 "@version{#2024-4-13}
  @syntax{lambda (row1 row2) => result}
  @argument[row1]{a @class{gtk:list-box-row} widget with the first row}
  @argument[row2]{a @class{gtk:list-box-row} widget with the second row}
  @argument[result]{an integer which is < 0 if @arg{row1} should be before
    @arg{row2}, 0 if they are equal and > 0 otherwise}
  @begin{short}
    The type of the callback function that compares two rows to determine which
    should be first.
  @end{short}
  @see-class{gtk:list-box-row}
  @see-function{gtk:list-box-set-sort-func}")

(export 'list-box-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_sort_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_set_sort_func" %list-box-set-sort-func) :void
  (listbox (g:object list-box))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun list-box-set-sort-func (listbox func)
 #+liber-documentation
 "@version{#2024-4-13}
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
;;; gtk_list_box_invalidate_sort
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_invalidate_sort" list-box-invalidate-sort) :void
 #+liber-documentation
 "@version{#2024-4-13}
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
;;; gtk_list_box_drag_highlight_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_drag_highlight_row" list-box-drag-highlight-row)
    :void
 #+liber-documentation
 "@version{#2024-4-13}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[row]{a @class{gtk:list-box-row} widget}
  @begin{short}
    Add a drag highlight to a row.
  @end{short}
  This is a helper function for implementing DnD onto a @class{gtk:list-box}
  widget. The passed in row will be highlighted by setting the
  @code{:drop-active} state from the @symbol{gtk:state-flags} flags and any
  previously highlighted row will be unhighlighted.

  The row will also be unhighlighted when the widget gets a drag leave event.
  @see-class{gtk:list-box}
  @see-class{gtk:list-box-row}
  @see-symbol{gtk:state-flags}"
  (listbox (g:object list-box))
  (row (g:object list-box-row)))

(export 'list-box-drag-highlight-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_drag_unhighlight_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_drag_unhighlight_row"
               list-box-drag-unhighlight-row) :void
 #+liber-documentation
 "@version{#2024-4-13}
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
;;; GtkListBoxCreateWidgetFunc
;;; ----------------------------------------------------------------------------

;; TODO: Check this?! We convert the pointer to a GObject. We need an example.

(cffi:defcallback list-box-create-widget-func (g:object widget)
    ((item :pointer)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data))
        (object (cffi:convert-from-foreign item 'g:object)))
    (funcall func object)))

#+liber-documentation
(setf (liber:alias-for-symbol 'list-box-create-widget-func)
      "Callback"
      (liber:symbol-documentation 'list-box-create-widget-func)
 "@version{#2024-4-13}
  @syntax{lambda (item) => result}
  @argument[object]{a @class{g:object} object for the item from the model for
    which to create a widget for}
  @argument[result]{a @class{gtk:widget} object that represents @arg{item}}
  @begin{short}
    Called for list boxes that are bound to a @class{g:list-model} object with
    the @fun{gtk:list-box-bind-model} function for each item that gets added to
    the model.
  @end{short}
  If the widget returned is not a @class{gtk:list-box-row} widget, then the
  widget will be inserted as the child of an intermediate
  @class{gtk:list-box-row} widget.
  @see-class{gtk:list-box}
  @see-class{g:list-model}
  @see-function{gtk:list-box-bind-model}")

(export 'list-box-create-widget-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_bind_model
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_list_box_bind_model" %list-box-bind-model) :void
  (listbox (g:object list-box))
  (model (g:object g:list-model))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun list-box-bind-model (listbox model func)
 #+liber-documentation
 "@version{#2024-4-13}
  @argument[listbox]{a @class{gtk:list-box} widget}
  @argument[model]{a @class{g:list-model} object to be bound to @arg{listbox}}
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
  @see-class{g:list-model}
  @see-symbol{gtk:list-box-create-widget-func}
  @see-function{gtk:list-box-insert}"
  (if model
      (%list-box-bind-model listbox
                            model
                            (cffi:callback list-box-create-widget-func)
                            (glib:allocate-stable-pointer func)
                            (cffi:callback glib:stable-pointer-destroy-notify))
      (%list-box-bind-model listbox
                            nil
                            (cffi:null-pointer)
                            (cffi:null-pointer)
                            (cffi:null-pointer))))

(export 'list-box-bind-model)

;;; --- End of file gtk4.list-box.lisp -----------------------------------------
