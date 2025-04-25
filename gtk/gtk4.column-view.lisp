;;; ----------------------------------------------------------------------------
;;; gtk4.column-view.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2025 Dieter Kaiser
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
;;; GtkColumnView
;;;
;;;     A widget for displaying lists in multiple columns
;;;
;;; Types and Values
;;;
;;;     GtkColumnView
;;;
;;; Accessors
;;;
;;;     gtk_column_view_get_columns
;;;     gtk_column_view_get_enable_rubberband
;;;     gtk_column_view_set_enable_rubberband
;;;     gtk_column_view_get_header_factory                  Since 4.12
;;;     gtk_column_view_set_header_factory                  Since 4.12
;;;     gtk_column_view_get_model
;;;     gtk_column_view_set_model
;;;     gtk_column_view_get_reorderable
;;;     gtk_column_view_set_reorderable
;;;     gtk_column_view_get_row_factory                     Since 4.12
;;;     gtk_column_view_set_row_factory                     Since 4.12
;;;     gtk_column_view_get_show_column_separators
;;;     gtk_column_view_set_show_column_separators
;;;     gtk_column_view_get_show_row_separators
;;;     gtk_column_view_set_show_row_separators
;;;     gtk_column_view_set_single_click_activate
;;;     gtk_column_view_get_single_click_activate
;;;     gtk_column_view_get_sorter
;;;     gtk_column_view_get_tab_behavior                    Since 4.12
;;;     gtk_column_view_set_tab_behavior                    Since 4.12
;;;
;;; Functions
;;;
;;;     gtk_column_view_new
;;;     gtk_column_view_append_column
;;;     gtk_column_view_insert_column
;;;     gtk_column_view_remove_column
;;;     gtk_column_view_sort_by_column
;;;
;;; Properties
;;;
;;;     columns
;;;     enable-rubberband
;;;     header-factory                                      Since 4.12
;;;     model
;;;     reorderable
;;;     row-factory                                         Since 4.12
;;;     show-column-separators
;;;     show-row-separators
;;;     single-click-activate
;;;     sorter
;;;     tab-behavior                                        Since 4.12
;;;
;;; Signals
;;;
;;;     activate
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkColumnView
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkScrollable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkColumnView
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkColumnView" column-view
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkScrollable")
   :type-initializer "gtk_column_view_get_type")
  ((columns
    column-view-columns
    "columns" "GListModel" t nil)
   (enable-rubberband
    column-view-enable-rubberband
    "enable-rubberband" "gboolean" t t)
   #+gtk-4-12
   (header-factory
    column-view-header-factory
    "header-factory" "GtkListItemFactory" t t)
   (model
    column-view-model
    "model" "GtkSelectionModel" t t)
   (reorderable
    column-view-reorderable
    "reorderable" "gboolean" t t)
   #+gtk-4-12
   (row-factory
    column-view-row-factory
    "row-factory" "GtkListItemFactory" t t)
   (show-column-separators
    column-view-show-column-separators
    "show-column-separators" "gboolean" t t)
   (show-row-separators
    column-view-show-row-separators
    "show-row-separators" "gboolean" t t)
   (single-click-activate
    column-view-single-click-activate
    "single-click-activate" "gboolean" t t)
   (sorter
    column-view-sorter
    "sorter" "GtkSorter" t nil)
   #+gtk-4-12
   (tab-behavior
    column-view-tab-behavior
    "tab-behavior" "GtkListTabBehavior" t t)))

#+liber-documentation
(setf (documentation 'column-view 'type)
 "@version{2025-4-13}
  @begin{short}
    The @class{gtk:column-view} widget is a widget to present a view into a
    large dynamic list of items using multiple columns with headers.
  @end{short}
  The @class{gtk:column-view} widget uses the factories of its columns to
  generate a cell widget for each column, for each visible item and displays
  them together as the row for this item. The
  @slot[gtk:column-view]{show-row-separators} and
  @slot[gtk:column-view]{show-column-separators} properties offer a simple way
  to display separators between the rows or columns.

  The @class{gtk:column-view} widget allows the user to select items according
  to the selection characteristics of the model. For models that allow multiple
  selected items, it is possible to turn on rubberband selection, using the
  @slot[gtk:column-view]{enable-rubberband} property.

  The column view supports sorting that can be customized by the user by
  clicking on column headers. To set this up, the @class{gtk:sorter} object
  returned by the @fun{gtk:column-view-sorter} function must be attached to a
  sort model for the data that the view is showing, and the columns must have
  sorters attached to them by calling @setf{gtk:column-view-column-sorter}
  function. The initial sort order can be set with the
  @fun{gtk:column-view-sort-by-column} function.

  The column view also supports interactive resizing and reordering of columns,
  via Drag-and-Drop of the column headers. This can be enabled or disabled with
  the @slot[gtk:column-view]{reorderable} and @slot[gtk:column-view]{resizable}
  properties.

  To learn more about the list widget framework, see the list widget overview.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:column-view} implementation uses a single CSS node named
    @code{columnview}. It may carry the @code{.column-separators} style class,
    when @slot[gtk:column-view]{show-column-separators} property is set. Header
    widgets appear below a node with name @code{header}. The rows are contained
    in a @class{gtk:list-view} widget, so there is a @code{listview} node with
    the same structure as for a standalone @class{gtk:list-view} widget. If the
    @slot[gtk:column-view]{show-row-separators} property is set, it will be
    passed on to the list view, causing its CSS node to carry the
    @code{.separators} style class. For rubberband selection, a node with name
    @code{rubberband} is used. The main @code{columnview} node may also carry
    @code{.rich-list}, @code{.navigation-sidebar} or @code{.data-table} style
    classes to select the  style of list presentation.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:column-view} implementation uses the @code{:tree-grid} role
    of the @symbol{gtk:accessible-role} enumeration, header title widgets are
    using the @code{:column-header} role. The row widgets are using the
    @code{:row} role, and individual cells are using the @code{:grid-cell} role.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (columnview position)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[columnview]{The @class{gtk:column-view} widget.}
        @entry[position]{The unsigned integer with the position of the item to
          activate.}
      @end{table}
      The signal is emitted when a row has been activated by the user. This
      allows for a convenient way to handle activation in a columnview. See
      the @fun{gtk:list-item-activatable} function for details on how to use
      this signal.
  @end{dictionary}
  @see-constructor{gtk:column-view-new}
  @see-slot{gtk:column-view-columns}
  @see-slot{gtk:column-view-enable-rubberband}
  @see-slot{gtk:column-view-header-factory}
  @see-slot{gtk:column-view-model}
  @see-slot{gtk:column-view-reorderable}
  @see-slot{gtk:column-view-row-factory}
  @see-slot{gtk:column-view-show-column-separators}
  @see-slot{gtk:column-view-show-row-separators}
  @see-slot{gtk:column-view-single-click-activate}
  @see-slot{gtk:column-view-sorter}
  @see-slot{gtk:column-view-tab-behavior}
  @see-class{gtk:column-view-column}
  @see-class{gtk:tree-view}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:column-view-columns ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "columns" 'column-view) t)
 "The @code{columns} property of type @class{g:list-model} (Read) @br{}
  The list of columns.")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-columns)
      "Accessor"
      (documentation 'column-view-columns 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-columns object) => columns}
  @argument[object]{a @class{gtk:column-view} object}
  @argument[columns]{a @class{g:list-model} object for the list managing the
    columns}
  @begin{short}
    Accessor of the @slot[gtk:column-view]{columns} slot of the
    @class{gtk:column-view} class.
  @end{short}
  The @fun{gtk:column-view-columns} function gets the list of columns in this
  column view. This list is constant over the lifetime of @arg{object} and can
  be used to monitor changes to the columns of @arg{object} by connecting to the
  @code{\"items-changed\"} signal.
  @see-class{gtk:column-view}")

;;; --- gtk:column-view-enable-rubberband --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-rubberband"
                                               'column-view) t)
 "The @code{enable-rubberband} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to allow rubberband selection. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-enable-rubberband)
      "Accessor"
      (documentation 'column-view-enable-rubberband 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-enable-rubberband object) => setting}
  @syntax{(setf (gtk:column-view-enable-rubberband object) setting)}
  @argument[object]{a @class{gtk:column-view} object}
  @argument[setting]{@em{true} if rubberband selection is enabled}
  @begin{short}
    Accessor of the @slot[gtk:column-view]{enable-rubberband} slot of the
    @class{gtk:column-view} class.
  @end{short}
  The @fun{gtk:column-view-enable-rubberband} function returns whether rows can
  be selected by dragging with the mouse. The
  @setf{gtk:column-view-enable-rubberband} function sets the property.
  @see-class{gtk:column-view}")

;;; --- gtk:column-view-header-factory -----------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "header-factory" 'column-view) t)
 "The @code{header-factory} property of type @class{gtk:list-item-factory}
  (Read / Write) @br{}
  The factory for creating header widgets. Since 4.12")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'column-view-header-factory)
      "Accessor"
      (documentation 'column-view-header-factory 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-header-factory object) => factory}
  @syntax{(setf (gtk:column-view-header-factory object) factory)}
  @argument[object]{a @class{gtk:column-view} object}
  @argument[factory]{a @class{gtk:list-item-factory} object to use}
  @begin{short}
    Accessor of the @slot[gtk:column-view]{header-factory} slot of the
    @class{gtk:column-view} class.
  @end{short}
  The @fun{gtk:column-view-header-factory} function gets the factory to use for
  populating the @class{gtk:list-header} objects used in section headers. The
  @setf{gtk:column-view-header-factory} function sets the factory. If this
  factory is set to @code{nil}, the list will not show section headers.

  Since 4.12
  @see-class{gtk:column-view}
  @see-class{gtk:list-item-factory}
  @see-class{gtk:list-header}")

;;; --- gtk:column-view-model --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'column-view) t)
 "The @code{model} property of type @class{gtk:selection-model} (Read / Write)
  @br{}
  The model for the items displayed.")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-model)
      "Accessor"
      (documentation 'column-view-model 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-model object) => model}
  @syntax{(setf (gtk:column-view-model object) model)}
  @argument[object]{a @class{gtk:column-view} object}
  @argument[model]{a @class{gtk:selection-model} object to use, or @code{nil}
    for none}
  @begin{short}
    Accessor of the @slot[gtk:column-view]{model} slot of the
    @class{gtk:column-view} class.
  @end{short}
  The @fun{gtk:column-view-model} function gets the model that is currently used
  to read the items displayed. The @setf{gtk:column-view-model} function sets
  the model to use. This must be a @class{gtk:selection-model} object.
  @see-class{gtk:column-view}
  @see-class{gtk:selection-model}")

;;; --- gtk:column-view-reorderable --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "reorderable" 'column-view) t)
 "The @code{reorderable} property of type @code{:boolean} (Read / Write) @br{}
  Whether columns are reorderable. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-reorderable)
      "Accessor"
      (documentation 'column-view-reorderable 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-reorderable object) => reorderable}
  @syntax{(setf (gtk:column-view-reorderable object) reorderable)}
  @argument[object]{a @class{gtk:column-view} object}
  @argument[reorderable]{@em{true} if columns are reorderable}
  @begin{short}
    Accessor of the @slot[gtk:column-view]{reorderable} slot of the
    @class{gtk:column-view} class.
  @end{short}
  The @fun{gtk:column-view-reorderable} function returns whether columns are
  reorderable. The @setf{gtk:column-view-reorderable} function sets the
  property.
  @see-class{gtk:column-view}")

;;; --- gtk:column-view-row-factory --------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "row-factory" 'column-view) t)
 "The @code{row-factory} property of type @class{gtk:list-item-factory}
  (Read / Write) @br{}
  The factory used for configuring rows. Since 4.12")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'column-view-row-factory)
      "Accessor"
      (documentation 'column-view-row-factory 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-row-factory object) => factory}
  @syntax{(setf (gtk:column-view-row-factory object) factory)}
  @argument[object]{a @class{gtk:column-view} object}
  @argument[factory]{a @class{gtk:list-item-factory} object to use}
  @begin{short}
    Accessor of the @slot[gtk:column-view]{row-factory} slot of the
    @class{gtk:column-view} class.
  @end{short}
  The @fun{gtk:column-view-row-factory} function gets the factory that is
  used for configuring rows. The @setf{gtk:column-view-row-factory} function
  sets the factory. The factory must be for configuring
  @class{gtk:column-view-row} objects.

  If this factory is not set - which is the default - then the defaults will be
  used. This factory is not used to set the widgets displayed in the individual
  cells. For that see the @fun{gtk:column-view-column-factory} function and
  the @class{gtk:column-view-cell} object.

  Since 4.12
  @see-class{gtk:column-view}
  @see-class{gtk:list-item-factory}
  @see-class{gtk:column-view-row}
  @see-class{gtk:column-view-cell}")

;;; --- gtk:column-view-show-column-separators ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-column-separators"
                                               'column-view) t)
 "The @code{show-column-separators} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to show separators between columns. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-show-column-separators)
      "Accessor"
      (documentation 'column-view-show-column-separators 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-show-column-separators object) => setting}
  @syntax{(setf (gtk:column-view-show-column-separators object) setting)}
  @argument[object]{a @class{gtk:column-view} object}
  @argument[setting]{@em{true} to show column separators}
  @begin{short}
    Accessor of the @slot[gtk:column-view]{show-column-separators} slot of the
    @class{gtk:column-view} class.
  @end{short}
  The @fun{gtk:column-view-show-column-separators} function returns whether the
  list should show separators between columns. The
  @setf{gtk:column-view-show-column-separators} function sets the property.
  @see-class{gtk:column-view}")

;;; --- gtk:column-view-show-row-separators ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-row-separators"
                                               'column-view) t)
 "The @code{show-row-separators} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to show separators between rows. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-show-row-separators)
      "Accessor"
      (documentation 'column-view-show-row-separators 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-show-row-separators object) => setting}
  @syntax{(setf (gtk:column-view-show-row-separators object) setting)}
  @argument[object]{a @class{gtk:column-view} object}
  @argument[setting]{@em{true} to show column separators}
  @begin{short}
    Accessor of the @slot[gtk:column-view]{show-row-separators} slot of the
    @class{gtk:column-view} class.
  @end{short}
  The @fun{gtk:column-view-show-row-separators} function returns whether the
  list should show separators between rows. The
  @setf{gtk:column-view-show-row-separators} function sets the property.
  @see-class{gtk:column-view}")

;;; --- gtk:column-view-single-click-activate ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "single-click-activate"
                                               'column-view) t)
 "The @code{single-click-activate} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to activate rows on single click and select them on hover. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-single-click-activate)
      "Accessor"
      (documentation 'column-view-single-click-activate 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-single-click-activate object) => setting}
  @syntax{(setf (gtk:column-view-single-click-activate object) setting)}
  @argument[object]{a @class{gtk:column-view} object}
  @argument[setting]{@em{true} if rows are activated on single click}
  @begin{short}
    Accessor of the @slot[gtk:column-view]{single-click-activate} slot of the
    @class{gtk:column-view} class.
  @end{short}
  The @fun{gtk:column-view-single-click-activate} function returns whether rows
  will be activated on single click and selected on hover. The
  @setf{gtk:column-view-single-click-activate} function sets the property.
  @see-class{gtk:column-view}")

;;; --- gtk:column-view-sorter -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sorter" 'column-view) t)
 "The @code{sorter} property of type @class{gtk:sorter} (Read) @br{}
  The sorter with the sorting choices of the user.")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-sorter)
      "Accessor"
      (documentation 'column-view-sorter 'function)
 "@version{2025-4-25}
  @syntax{(gtk:column-view-sorter object) => sorter}
  @argument[object]{a @class{gtk:column-view} object}
  @argument[sorter]{a @class{gtk:sorter} object}
  @begin{short}
    Accessor of the @slot[gtk:column-view]{sorter} slot of the
    @class{gtk:column-view} class.
  @end{short}
  The @fun{gtk:column-view-sorter} function returns a special sorter that
  reflects the users sorting choices in the column view. To allow users to
  customizable sorting by clicking on column headers, this sorter needs to be
  set on the sort model underneath the model that is displayed by the view. See
  the @fun{gtk:column-view-column-sorter} function for setting up per-column
  sorting.
  @begin[Examples]{dictionary}
    Set the special sorter on the sort model underneath the model that is
    displayed by the view:
    @begin{pre}
(let* (;; Get the special sorter from the column view
       (sorter (gtk:column-view-sorter view))
       ;; Create a new selection model from a new sort list model
       (model (gtk:sort-list-model-new store sorter))
       (selection (gtk:no-selection-new model)))
  ;; Set the selection as model for the column view
  (setf (gtk:column-view-model view) selection)
  ... )
    @end{pre}
  @end{dictionary}
  @see-class{gtk:column-view}
  @see-class{gtk:selection-model}
  @see-function{gtk:column-view-column-sorter}")

;;; --- gtk:column-view-tab-behavior -------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "tab-behavior" 'column-view) t)
 "The @code{tab-behavior} property of type @symbol{gtk:list-tab-behavior}
  (Read / Write) @br{}
  The behavior of the @kbd{Tab} and @kbd{Shift+Tab} keys. @br{}
  Default value: @code{:all}")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'column-view-tab-behavior)
      "Accessor"
      (documentation 'column-view-tab-behavior 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-tab-behavior object) => setting}
  @syntax{(setf (gtk:column-view-tab-behavior object) setting)}
  @argument[object]{a @class{gtk:column-view} object}
  @argument[setting]{a @symbol{gtk:list-tab-behavior} value}
  @begin{short}
    Accessor of the @slot[gtk:column-view]{tab-behavior} slot of the
    @class{gtk:column-view} class.
  @end{short}
  The @fun{gtk:column-view-tab-behavior} function gets the behavior of the
  @kbd{Tab} and @kbd{Shift+Tab} keys. The @setf{gtk:column-view-tab-behavior}
  function sets the tab behavior.
  @see-class{gtk:column-view}
  @see-symbol{gtk:list-tab-behavior}")

;;; ----------------------------------------------------------------------------
;;; gtk_column_view_new
;;; ----------------------------------------------------------------------------

(declaim (inline column-view-new))

(defun column-view-new (&optional model)
 #+liber-documentation
 "@version{2025-4-13}
  @argument[model]{a @class{gtk:selection-model} object to use, or the default
    @code{nil} value}
  @return{The new @class{gtk:column-view} widget.}
  @begin{short}
    Creates a new @class{gtk:column-view} widget.
  @end{short}
  You most likely want to call the @fun{gtk:column-view-append-column} function
  to add columns next.
  @see-class{gtk:column-view}
  @see-class{gtk:selection-model}
  @see-function{gtk:column-view-append-column}"
  (make-instance 'column-view
                 :model model))

(export 'column-view-new)

;;; ----------------------------------------------------------------------------
;;; gtk_column_view_append_column
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_column_view_append_column" column-view-append-column) :void
 #+liber-documentation
 "@version{2025-4-25}
  @argument[columnview]{a @class{gtk:column-view} widget}
  @argument[column]{a @class{gtk:column-view-column} object that has not been
    added to @arg{columnview} yet}
  @begin{short}
    Appends the column to the end of the columns in @arg{columnview}.
  @end{short}
  @see-class{gtk:column-view}
  @see-class{gtk:column-view-column}"
  (columnview (g:object column-view))
  (column (g:object column-view-column)))

(export 'column-view-append-column)

;;; ----------------------------------------------------------------------------
;;; gtk_column_view_insert_column
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_column_view_insert_column" column-view-insert-column) :void
 #+liber-documentation
 "@version{2025-4-13}
  @argument[columnview]{a @class{gtk:column-view} widget}
  @argument[pos]{an unsigned integer for the position to insert
    @arg{column} at}
  @argument[column]{a @class{gtk:column-view-column} object to insert}
  @begin{short}
    Inserts a column at the given @arg{pos} in the columns of @arg{columview}.
  @end{short}
  If @arg{column} is already a column of @arg{columnview}, it will be
  repositioned.
  @see-class{gtk:column-view}
  @see-class{gtk:column-view-column}"
  (columnview (g:object column-view))
  (pos :uint)
  (column (g:object column-view-column)))

(export 'column-view-insert-column)

;;; ----------------------------------------------------------------------------
;;; gtk_column_view_remove_column
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_column_view_remove_column" column-view-remove-column) :void
 #+liber-documentation
 "@version{2025-4-13}
  @argument[columnview]{a @class{gtk:column-view} widget}
  @argument[column]{a @class{gtk:column-view-column} object to remove}
  @begin{short}
    Removes the column from the list of columns of @arg{columnview}.
  @end{short}
  @see-class{gtk:column-view}
  @see-class{gtk:column-view-column}"
  (columnview (g:object column-view))
  (column (g:object column-view-column)))

(export 'column-view-remove-column)

;;; ----------------------------------------------------------------------------
;;; gtk_column_view_sort_by_column
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_column_view_sort_by_column" column-view-sort-by-column)
    :void
 #+liber-documentation
 "@version{#2025-4-13}
  @argument[columnview]{a @class{gtk:column-view} widget}
  @argument[column]{a @class{gtk:column-view-column} object to sort by,
    or @code{nil}}
  @argument[direction]{a @symbol{gtk:sort-type} value for the direction to sort
    in}
  @begin{short}
    Sets the sorting of the column view.
  @end{short}
  This function should be used to set up the initial sorting. At runtime, users
  can change the sorting of a column view by clicking on the list headers.

  This call only has an effect if the sorter returned by the
  @fun{gtk:column-view-sorter} function is set on a sort model, and the
  @setf{gtk:column-view-column-sorter} function has been called on column to
  associate a sorter with the column.

  If @arg{column} is @code{nil}, the column view will be unsorted.
  @see-class{gtk:column-view}
  @see-class{gtk:column-view-column}
  @see-symbol{gtk:sort-type}
  @see-function{gtk:column-view-sorter}
  @see-function{gtk:column-view-column-sorter}"
  (columnview (g:object column-view))
  (column (g:object column-view-column))
  (direction sort-type))

(export 'column-view-sort-by-column)

;;; --- End of file gtk4.column-view.lisp --------------------------------------
