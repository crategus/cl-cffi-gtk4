;;; ----------------------------------------------------------------------------
;;; gtk4.column-view.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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
;;;     gtk_column_view_get_header_factory                 Since 4.12
;;;     gtk_column_view_set_header_factory                 Since 4.12
;;;     gtk_column_view_get_model
;;;     gtk_column_view_set_model
;;;     gtk_column_view_get_reorderable
;;;     gtk_column_view_set_reorderable
;;;     gtk_column_view_get_row_factory                    Since 4.12
;;;     gtk_column_view_set_row_factory                    Since 4.12
;;;     gtk_column_view_get_show_column_separators
;;;     gtk_column_view_set_show_column_separators
;;;     gtk_column_view_get_show_row_separators
;;;     gtk_column_view_set_show_row_separators
;;;     gtk_column_view_set_single_click_activate
;;;     gtk_column_view_get_single_click_activate
;;;     gtk_column_view_get_sorter
;;;     gtk_column_view_get_tab_behavior                   Since 4.12
;;;     gtk_column_view_set_tab_behavior                   since 4.12
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
;;;     header-factory                                     Since 4.12
;;;     model
;;;     reorderable
;;;     row-factory                                        Since 4.12
;;;     show-column-separators
;;;     show-row-separators
;;;     single-click-activate
;;;     sorter
;;;     tab-behavior                                       Since 4.12
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

(gobject:define-g-object-class "GtkColumnView" column-view
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
   (single-click-activate
    column-view-single-click-activate
    "single-click-activte" "gboolean" t t)
   (sorter
    column-view-sorter
    "sorter" "GtkSorter" t nil)
   #+gtk-4-12
   (tab-behavior
    column-view-tab-behavior
    "tab-behavior" "GtkListTabBehavior" t t)))

#+liber-documentation
(setf (documentation 'column-view 'type)
 "@version{#2023-9-9}
  @begin{short}
    The @class{gtk:column-view} widget is a widget to present a view into a
    large dynamic list of items using multiple columns with headers.
  @end{short}
  The @class{gtk:column-view} widget uses the factories of its columns to
  generate a cell widget for each column, for each visible item and displays
  them together as the row for this item. The
  @slot[gtk:view-column]{show-row-separators} and
  @slot[gtk:view-column]{show-column-separators} properties offer a simple way
  to display separators between the rows or columns.

  The @class{gtk:column-view} widget allows the user to select items according
  to the selection characteristics of the model. For models that allow multiple
  selected items, it is possible to turn on rubberband selection, using the
  @slot[gtk:column-view]{enable-rubberband} property.

  The column view supports sorting that can be customized by the user by
  clicking on column headers. To set this up, the @class{gtk:sorter} object
  returned by the @fun{gtk:column-view-sorter} function must be attached to a
  sort model for the data that the view is showing, and the columns must have
  sorters attached to them by calling @sym{(setf gtk:column-view-column-sorter)}
  function. The initial sort order can be set with the
  @fun{gtk:column-view-sort-by-column} function.

  The column view also supports interactive resizing and reordering of columns,
  via Drag-and-Drop of the column headers. This can be enabled or disabled with
  the @slot[gtk:column-view]{reorderable} and @slot[gtk:column-view]{resizable}
  properties.

  To learn more about the list widget framework, see the overview.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:column-view} implementation uses a single CSS node named
    @code{columnview}. It may carry the @code{.column-separators} style class,
    when @slot[gtk:column-view]{show-column-separators} property is set. Header
    widets appear below a node with name @code{header}. The rows are contained
    in a @class{gtk:list-view} widget, so there is a @code{listview} node with
    the same structure as for a standalone @class{gtk:list-view} widget. If the
    @slot[gtk:column-view]{show-row-separators} property is set, it will be
    passed on to the list view, causing its CSS node to carry the
    @code{.separators} style class. For rubberband selection, a node with name
    @code{rubberband} is used.

    The main @code{columnview} node may also carry style classes to select the
    style of list presentation: @code{.rich-list}, @code{.navigation-sidebar}
    or @code{.data-table}.
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
      The signal is emitted when a row has been activated by the user, usually
      via activating the @code{GtkListBase|list.activate-item} action. This
      allows for a convenient way to handle activation in a columnview. See
      the @fun{gtk:list-item-activatable} function for details on how to use
      this signal.
      @begin[code]{table}
        @entry[columnview]{The @class{gtk:column-view} widget.}
        @entry[position]{An unsigned integer with the position of the item to
          activate.}
      @end{table}
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

;;;The “columns” property
;;;  “columns”                  GListModel *
;;;The list of columns

;;;Owner: GtkColumnView

;;;Flags: Read

;;;The “enable-rubberband” property
;;;  “enable-rubberband”        gboolean
;;;Allow rubberband selection

;;;Owner: GtkColumnView

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “model” property
;;;  “model”                    GtkSelectionModel *
;;;Model for the items displayed

;;;Owner: GtkColumnView

;;;Flags: Read / Write

;;;The “reorderable” property
;;;  “reorderable”              gboolean
;;;Whether columns are reorderable

;;;Owner: GtkColumnView

;;;Flags: Read / Write

;;;Default value: TRUE

;;;The “show-column-separators” property
;;;  “show-column-separators”   gboolean
;;;Show separators between columns

;;;Owner: GtkColumnView

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “show-row-separators” property
;;;  “show-row-separators”      gboolean
;;;Show separators between rows

;;;Owner: GtkColumnView

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “single-click-activate” property
;;;  “single-click-activate”    gboolean
;;;Activate rows on single click and select them on hover

;;;Owner: GtkColumnView

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “sorter” property
;;;  “sorter”                   GtkSorter *
;;;Sorter with the sorting choices of the user

;;;Owner: GtkColumnView

;;;Flags: Read





;;;Functions
;;;gtk_column_view_new ()
;;;GtkWidget *
;;;gtk_column_view_new (GtkSelectionModel *model);
;;;Creates a new GtkColumnView.

;;;You most likely want to call gtk_column_view_append_column() to add columns next.

;;;Parameters
;;;model

;;;the list model to use, or NULL.

;;;[allow-none][transfer full]
;;;Returns
;;;a new GtkColumnView

;;;gtk_column_view_append_column ()
;;;void
;;;gtk_column_view_append_column (GtkColumnView *self,
;;;                               GtkColumnViewColumn *column);
;;;Appends the column to the end of the columns in self .

;;;Parameters
;;;self

;;;a GtkColumnView

;;;column

;;;a GtkColumnViewColumn that hasn't been added to a GtkColumnView yet

;;;gtk_column_view_insert_column ()
;;;void
;;;gtk_column_view_insert_column (GtkColumnView *self,
;;;                               guint position,
;;;                               GtkColumnViewColumn *column);
;;;Inserts a column at the given position in the columns of self .

;;;If column is already a column of self , it will be repositioned.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;position

;;;the position to insert column at

;;;column

;;;the GtkColumnViewColumn to insert

;;;gtk_column_view_remove_column ()
;;;void
;;;gtk_column_view_remove_column (GtkColumnView *self,
;;;                               GtkColumnViewColumn *column);
;;;Removes the column from the list of columns of self .

;;;Parameters
;;;self

;;;a GtkColumnView

;;;column

;;;a GtkColumnViewColumn that's part of self

;;;gtk_column_view_get_columns ()
;;;GListModel *
;;;gtk_column_view_get_columns (GtkColumnView *self);
;;;Gets the list of columns in this column view. This list is constant over the lifetime of self and can be used to monitor changes to the columns of self by connecting to the “items-changed” signal.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;Returns
;;;The list managing the columns.

;;;[transfer none]

;;;gtk_column_view_get_model ()
;;;GtkSelectionModel *
;;;gtk_column_view_get_model (GtkColumnView *self);
;;;Gets the model that's currently used to read the items displayed.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;Returns
;;;The model in use.

;;;[nullable][transfer none]

;;;gtk_column_view_set_model ()
;;;void
;;;gtk_column_view_set_model (GtkColumnView *self,
;;;                           GtkSelectionModel *model);
;;;Sets the GtkSelectionModel to use.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;model

;;;the model to use or NULL for none.

;;;[allow-none][transfer none]
;;;gtk_column_view_get_sorter ()
;;;GtkSorter *
;;;gtk_column_view_get_sorter (GtkColumnView *self);
;;;Returns a special sorter that reflects the users sorting choices in the column view.

;;;To allow users to customizable sorting by clicking on column headers, this sorter needs to be set on the sort model underneath the model that is displayed by the view.

;;;See gtk_column_view_column_set_sorter() for setting up per-column sorting.

;;;Here is an example:

;;;columnview[.column-separators][.rich-list][.navigation-sidebar][.data-table]
;;;├── header
;;;│   ├── <column header>
;;;┊   ┊
;;;│   ╰── <column header>
;;;│
;;;├── listview
;;;│
;;;┊
;;;╰── [rubberband]
;;;Parameters
;;;self

;;;a GtkColumnView

;;;Returns
;;;the GtkSorter of self .

;;;[nullable][transfer none]

;;;gtk_column_view_get_show_row_separators ()
;;;gboolean
;;;gtk_column_view_get_show_row_separators
;;;                               (GtkColumnView *self);
;;;Returns whether the list should show separators between rows.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;Returns
;;;TRUE if the list shows separators

;;;gtk_column_view_set_show_row_separators ()
;;;void
;;;gtk_column_view_set_show_row_separators
;;;                               (GtkColumnView *self,
;;;                                gboolean show_row_separators);
;;;Sets whether the list should show separators between rows.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;show_row_separators

;;;TRUE to show row separators

;;;gtk_column_view_get_show_column_separators ()
;;;gboolean
;;;gtk_column_view_get_show_column_separators
;;;                               (GtkColumnView *self);
;;;Returns whether the list should show separators between columns.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;Returns
;;;TRUE if the list shows column separators

;;;gtk_column_view_set_show_column_separators ()
;;;void
;;;gtk_column_view_set_show_column_separators
;;;                               (GtkColumnView *self,
;;;                                gboolean show_column_separators);
;;;Sets whether the list should show separators between columns.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;show_column_separators

;;;TRUE to show column separators

;;;gtk_column_view_sort_by_column ()
;;;void
;;;gtk_column_view_sort_by_column (GtkColumnView *self,
;;;                                GtkColumnViewColumn *column,
;;;                                GtkSortType direction);
;;;Sets the sorting of the view.

;;;This function should be used to set up the initial sorting. At runtime, users can change the sorting of a column view by clicking on the list headers.

;;;This call only has an effect if the sorter returned by gtk_column_view_get_sorter() is set on a sort model, and gtk_column_view_column_set_sorter() has been called on column to associate a sorter with the column.

;;;If column is NULL, the view will be unsorted.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;column

;;;the GtkColumnViewColumn to sort by, or NULL.

;;;[allow-none]
;;;direction

;;;the direction to sort in

;;;gtk_column_view_set_single_click_activate ()
;;;void
;;;gtk_column_view_set_single_click_activate
;;;                               (GtkColumnView *self,
;;;                                gboolean single_click_activate);
;;;Sets whether rows should be activated on single click and selected on hover.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;single_click_activate

;;;TRUE to activate items on single click

;;;gtk_column_view_get_single_click_activate ()
;;;gboolean
;;;gtk_column_view_get_single_click_activate
;;;                               (GtkColumnView *self);
;;;Returns whether rows will be activated on single click and selected on hover.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;Returns
;;;TRUE if rows are activated on single click

;;;gtk_column_view_set_reorderable ()
;;;void
;;;gtk_column_view_set_reorderable (GtkColumnView *self,
;;;                                 gboolean reorderable);
;;;Sets whether columns should be reorderable by dragging.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;reorderable

;;;whether columns should be reorderable

;;;gtk_column_view_get_reorderable ()
;;;gboolean
;;;gtk_column_view_get_reorderable (GtkColumnView *self);
;;;Returns whether columns are reorderable.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;Returns
;;;TRUE if columns are reorderable

;;;gtk_column_view_set_enable_rubberband ()
;;;void
;;;gtk_column_view_set_enable_rubberband (GtkColumnView *self,
;;;                                       gboolean enable_rubberband);
;;;Sets whether selections can be changed by dragging with the mouse.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;enable_rubberband

;;;TRUE to enable rubberband selection

;;;gtk_column_view_get_enable_rubberband ()
;;;gboolean
;;;gtk_column_view_get_enable_rubberband (GtkColumnView *self);
;;;Returns whether rows can be selected by dragging with the mouse.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;Returns
;;;TRUE if rubberband selection is enabled


;;; --- End of file gtk4.column-view.lisp --------------------------------------
