;;; ----------------------------------------------------------------------------
;;; gtk4.tree-view.lisp
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
;;; GtkTreeView
;;;
;;;     A widget for displaying both trees and lists
;;;
;;; Types and Values
;;;
;;;     GtkTreeView
;;;     GtkTreeViewDropPosition
;;;     GtkTreeViewGridLines
;;;
;;; Accessors
;;;
;;;     gtk_tree_view_get_activate_on_single_click
;;;     gtk_tree_view_set_activate_on_single_click
;;;     gtk_tree_view_get_enable_search
;;;     gtk_tree_view_set_enable_search
;;;     gtk_tree_view_get_enable_tree_lines
;;;     gtk_tree_view_set_enable_tree_lines
;;;     gtk_tree_view_get_expander_column
;;;     gtk_tree_view_set_expander_column
;;;     gtk_tree_view_get_fixed_height_mode
;;;     gtk_tree_view_set_fixed_height_mode
;;;     gtk_tree_view_get_headers_clickable
;;;     gtk_tree_view_set_headers_clickable
;;;     gtk_tree_view_get_headers_visible
;;;     gtk_tree_view_set_headers_visible
;;;     gtk_tree_view_get_hover_expand
;;;     gtk_tree_view_set_hover_expand
;;;     gtk_tree_view_get_hover_selection
;;;     gtk_tree_view_set_hover_selection
;;;     gtk_tree_view_get_level_indentation
;;;     gtk_tree_view_set_level_indentation
;;;     gtk_tree_view_get_model
;;;     gtk_tree_view_set_model
;;;     gtk_tree_view_get_reorderable
;;;     gtk_tree_view_set_reorderable
;;;     gtk_tree_view_get_rubber_banding
;;;     gtk_tree_view_set_rubber_banding
;;;     gtk_tree_view_get_search_column
;;;     gtk_tree_view_set_search_column
;;;     gtk_tree_view_get_show_expanders
;;;     gtk_tree_view_set_show_expanders
;;;     gtk_tree_view_get_tooltip_column
;;;     gtk_tree_view_set_tooltip_column
;;;
;;; Functions
;;;
;;;     gtk_tree_view_new
;;;     gtk_tree_view_new_with_model
;;;     gtk_tree_view_get_selection
;;;     gtk_tree_view_columns_autosize
;;;     gtk_tree_view_append_column
;;;     gtk_tree_view_remove_column
;;;     gtk_tree_view_insert_column
;;;     gtk_tree_view_insert_column_with_attributes
;;;     gtk_tree_view_insert_column_with_data_func
;;;     gtk_tree_view_get_n_columns
;;;     gtk_tree_view_get_column
;;;     gtk_tree_view_get_columns
;;;     gtk_tree_view_move_column_after
;;;
;;;     GtkTreeViewColumnDropFunc
;;;     gtk_tree_view_set_column_drag_function
;;;
;;;     gtk_tree_view_scroll_to_point
;;;     gtk_tree_view_scroll_to_cell
;;;     gtk_tree_view_set_cursor
;;;     gtk_tree_view_set_cursor_on_cell
;;;     gtk_tree_view_get_cursor
;;;     gtk_tree_view_row_activated
;;;     gtk_tree_view_expand_all
;;;     gtk_tree_view_collapse_all
;;;     gtk_tree_view_expand_to_path
;;;     gtk_tree_view_expand_row
;;;     gtk_tree_view_collapse_row
;;;
;;;     GtkTreeViewMappingFunc
;;;     gtk_tree_view_map_expanded_rows
;;;
;;;     gtk_tree_view_row_expanded
;;;     gtk_tree_view_get_path_at_pos
;;;     gtk_tree_view_is_blank_at_pos
;;;     gtk_tree_view_get_cell_area
;;;     gtk_tree_view_get_background_area
;;;     gtk_tree_view_get_visible_rect
;;;     gtk_tree_view_get_visible_range
;;;
;;;     gtk_tree_view_convert_bin_window_to_tree_coords
;;;     gtk_tree_view_convert_bin_window_to_widget_coords
;;;     gtk_tree_view_convert_tree_to_bin_window_coords
;;;     gtk_tree_view_convert_tree_to_widget_coords
;;;     gtk_tree_view_convert_widget_to_bin_window_coords
;;;     gtk_tree_view_convert_widget_to_tree_coords
;;;
;;;     gtk_tree_view_enable_model_drag_dest
;;;     gtk_tree_view_enable_model_drag_source
;;;     gtk_tree_view_unset_rows_drag_source
;;;     gtk_tree_view_unset_rows_drag_dest
;;;     gtk_tree_view_set_drag_dest_row
;;;     gtk_tree_view_get_drag_dest_row
;;;     gtk_tree_view_get_dest_row_at_pos
;;;     gtk_tree_view_create_row_drag_icon
;;;
;;;     GtkTreeViewSearchEqualFunc
;;;     gtk_tree_view_get_search_equal_func
;;;     gtk_tree_view_set_search_equal_func
;;;
;;;     gtk_tree_view_get_search_entry
;;;     gtk_tree_view_set_search_entry
;;;
;;;     GtkTreeViewRowSeparatorFunc
;;;     gtk_tree_view_get_row_separator_func                not needed
;;;     gtk_tree_view_set_row_separator_func
;;;
;;;     gtk_tree_view_is_rubber_banding_active
;;;     gtk_tree_view_get_grid_lines
;;;     gtk_tree_view_set_grid_lines
;;;     gtk_tree_view_set_tooltip_row
;;;     gtk_tree_view_set_tooltip_cell
;;;     gtk_tree_view_get_tooltip_context
;;;
;;; Properties
;;;
;;;     activate-on-single-click
;;;     enable-grid-lines
;;;     enable-search
;;;     enable-tree-lines
;;;     expander-column
;;;     fixed-height-mode
;;;     headers-clickable
;;;     headers-visible
;;;     hover-expand
;;;     hover-selection
;;;     level-indentation
;;;     model
;;;     reorderable
;;;     rubber-banding
;;;     search-column
;;;     show-expanders
;;;     tooltip-column
;;;
;;; Signals
;;;
;;;     columns-changed
;;;     cursor-changed
;;;     expand-collapse-cursor-row
;;;     move-cursor
;;;     row-activated
;;;     row-collapsed
;;;     row-expanded
;;;     select-all
;;;     select-cursor-parent
;;;     select-cursor-row
;;;     start-interactive-search
;;;     test-collapse-row
;;;     test-expand-row
;;;     toggle-cursor-row
;;;     unselect-all
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkTreeView
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
;;; GtkTreeViewDropPosition
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkTreeViewDropPosition" tree-view-drop-position
  (:export t
   :type-initializer "gtk_tree_view_drop_position_get_type")
  (:before 0)
  (:after 1)
  (:into-or-before 2)
  (:into-or-after 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-view-drop-position)
      "GEnum"
      (liber:symbol-documentation 'tree-view-drop-position)
 "@version{2024-4-27}
  @begin{declaration}
(gobject:define-genum \"GtkTreeViewDropPosition\" tree-view-drop-position
  (:export t
   :type-initializer \"gtk_tree_view_drop_position_get_type\")
  (:before 0)
  (:after 1)
  (:into-or-before 2)
  (:into-or-after 3))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:before]{Dropped row is inserted before.}
      @entry[:after]{Dropped row is inserted after.}
      @entry[:into-or-before]{Dropped row becomes a child or is inserted
        before.}
      @entry[:into-or-after]{Dropped row becomes a child or is inserted after.}
    @end{table}
  @end{values}
  @begin{short}
    An enumeration for determining where a dropped row goes in a tree view.
  @end{short}
  @see-class{gtk:tree-view}")

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewGridLines
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkTreeViewGridLines" tree-view-grid-lines
  (:export t
   :type-initializer "gtk_tree_view_grid_lines_get_type")
  (:none 0)
  (:horizontal 1)
  (:vertical 2)
  (:both 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-view-grid-lines)
      "GEnum"
      (liber:symbol-documentation 'tree-view-grid-lines)
 "@version{2024-4-27}
  @begin{declaration}
(gobject:define-genum \"GtkTreeViewGridLines\" tree-view-grid-lines
  (:export t
   :type-initializer \"gtk_tree_view_grid_lines_get_type\")
  (:none 0)
  (:horizontal 1)
  (:vertical 2)
  (:both 3))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No grid lines.}
      @entry[:horizontal]{Horizontal grid lines.}
      @entry[:vertical]{Vertical grid lines.}
      @entry[:both]{Horizontal and vertical grid lines.}
    @end{table}
  @end{values}
  @begin{short}
    Used to indicate which grid lines to draw in a tree view.
  @end{short}
  @see-class{gtk:tree-view}")

;;; ----------------------------------------------------------------------------
;;; GtkTreeView
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkTreeView" tree-view
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_tree_view_get_type")
  ((activate-on-single-click
    tree-view-activate-on-single-click
    "activate-on-single-click" "gboolean" t t)
   (enable-grid-lines
    tree-view-enable-grid-lines
    "enable-grid-lines" "GtkTreeViewGridLines" t t)
   (enable-search
    tree-view-enable-search
    "enable-search" "gboolean" t t)
   (enable-tree-lines
    tree-view-enable-tree-lines
    "enable-tree-lines" "gboolean" t t)
   (expander-column
    tree-view-expander-column
    "expander-column" "GtkTreeViewColumn" t t)
   (fixed-height-mode
    tree-view-fixed-height-mode
    "fixed-height-mode" "gboolean" t t)
   (headers-clickable
    tree-view-headers-clickable
    "headers-clickable" "gboolean" t t)
   (headers-visible
    tree-view-headers-visible
    "headers-visible" "gboolean" t t)
   (hover-expand
    tree-view-hover-expand
    "hover-expand" "gboolean" t t)
   (hover-selection
    tree-view-hover-selection
    "hover-selection" "gboolean" t t)
   (level-indentation
    tree-view-level-indentation
    "level-indentation" "gint" t t)
   (model
    tree-view-model
    "model" "GtkTreeModel" t t)
   (reorderable
    tree-view-reorderable
    "reorderable" "gboolean" t t)
   (rubber-banding
    tree-view-rubber-banding
    "rubber-banding" "gboolean" t t)
   (search-column
    tree-view-search-column
    "search-column" "gint" t t)
   (show-expanders
    tree-view-show-expanders
    "show-expanders" "gboolean" t t)
   (tooltip-column
    tree-view-tooltip-column
    "tooltip-column" "gint" t t)))

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj tree-view) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:TREE-VIEW is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'tree-view 'type)
 "@version{2025-3-30}
  @begin{short}
    Widget that displays any object that implements the @class{gtk:tree-model}
    interface.
  @end{short}

  @image[tree-view]{Figure: GtkTreeView}

  Please refer to the tree widget conceptual overview for an overview of all
  the objects and data types related to the tree widget and how they work
  together.

  Several different coordinate systems are exposed in the @class{gtk:tree-view}
  API. These are:

  @image[tree-view-coordinates]{Figure: Tree view coordinates}

  Coordinate systems in the @class{gtk:tree-view} API:
  @begin[em]{table}
     @entry[Widget coordinates]{Coordinates relative to the widget.}
    @entry[Bin window coordinates]{Coordinates relative to the window that
      the @class{gtk:tree-view} widget renders to.}
    @entry[Tree coordinates]{Coordinates relative to the entire scrollable area
      of the @class{gtk:tree-view} widget. These coordinates start at
      @code{(0, 0)} for row @code{0} of the tree.}
  @end{table}
  Several functions are available for converting between the different
  coordinate systems. The most common translations are between widget and bin
  window coordinates and between bin window and tree coordinates. For the
  former you can use the
  @fun{gtk:tree-view-convert-widget-to-bin-window-coords} function (and vice
  versa), for the latter the
  @fun{gtk:tree-view-convert-bin-window-to-tree-coords} function (and vice
  versa).
  @begin[GtkTreeView as GtkBuildable]{dictionary}
    The @class{gtk:tree-view} implementation of the @class{gtk:buildable}
    interface accepts @class{gtk:tree-view-column} objects as @code{<child>}
    elements and exposes the internal @class{gtk:tree-selection} object in UI
    definitions.

    @b{Example:} A UI definition fragment with the @class{gtk:tree-view} widget
    @begin{pre}
<object class=\"GtkTreeView\" id=\"treeview\">
  <property name=\"model\">liststore1</property>
  <child>
    <object class=\"GtkTreeViewColumn\" id=\"test-column\">
      <property name=\"title\">Test</property>
      <child>
        <object class=\"GtkCellRendererText\" id=\"test-renderer\"/>
        <attributes>
          <attribute name=\"text\">1</attribute>
        </attributes>
      </child>
    </object>
  </child>
  <child internal-child=\"selection\">
    <object class=\"GtkTreeSelection\" id=\"selection\">
      <signal name=\"changed\" handler=\"on_treeview_selection_changed\"/>
    </object>
  </child>
</object>
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
treeview.view
├── header
│   ├── <column header>
┊   ┊
│   ╰── <column header>
│
╰── [rubberband]
    @end{pre}
    The @class{gtk:tree-view} implementation has a main CSS node with name
    @code{treeview} and @code{.view} style class. It has a subnode with name
    @code{header}, which is the parent for all the column header widgets' CSS
    nodes. For rubberband selection, a subnode with name @code{rubberband} is
    used.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Use the
    @class{gtk:list-view} implementation for lists, and the
    @class{gtk:column-view} implementation for tabular lists.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"columns-changed\" signal}
      @begin{pre}
lambda (view)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @class{gtk:tree-view} widget on which the signal is
          emitted.}
      @end{table}
      The number of columns of the tree view has changed.
    @subheading{The \"cursor-changed\" signal}
      @begin{pre}
lambda (view)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @class{gtk:tree-view} widget on which the signal is
          emitted.}
      @end{table}
      The position of the cursor (focused cell) has changed.
    @subheading{The \"expand-collapse-cursor-row\" signal}
      @begin{pre}
lambda (view arg1 arg2)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @class{gtk:tree-view} widget on which the signal is
          emitted.}
        @entry[arg1]{a boolean without description}
        @entry[arg2]{a boolean without description}
        @entry[Returns]{a boolean without description}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
lambda (view step direction)    :action
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @class{gtk:tree-view} widget on which the signal is
          emitted.}
        @entry[step]{The granularity of the move, as a value of the
          @symbol{gtk:movement-step} enumeration. The @code{:logical-positions},
          @code{:visual-positions}, @code{:display-lines}, @code{:pages} and
          @code{:buffer-ends} values are supported. The
          @code{:logical-positions} and @code{:visual-positions} values are
          treated identically.}
        @entry[direction]{The integer with the direction to move: +1 to move
          forwards, -1 to move backwards. The resulting movement is undefined
          for all other values.}
        @entry[Returns]{@em{True} if @arg{step} is supported, @em{false}
          otherwise.}
      @end{table}
      Keybinding signal which gets emitted when the user presses one of the
      cursor keys. Applications should not connect to it, but may emit it with
      the @fun{g:signal-emit} function if they need to control the cursor
      programmatically. In contrast to the @fun{gtk:tree-view-get-cursor} and
      @fun{gtk:tree-view-set-cursor-on-cell} functions when moving horizontally
      @code{\"move-cursor\"} does not reset the current selection.
    @subheading{The \"row-activated\" signal}
      @begin{pre}
lambda (view path column)    :action
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @class{gtk:tree-view} widget on which the signal is
          emitted.}
        @entry[path]{The @class{gtk:tree-path} instance for the activated row.}
        @entry[column]{The @class{gtk:tree-view-column} object in which the
          activation occurred.}
      @end{table}
      The signal is emitted when the @fun{gtk:tree-view-row-activated} function
      is called or the user double clicks a tree view row. It is also emitted
      when a non-editable row is selected and one of the @kbd{Space},
      @kbd{Shift+Space}, @kbd{Return} or @kbd{Enter} keys is pressed. For
      selection handling refer to the tree widget conceptual overview as well
      as the @class{gtk:tree-selection} API documentation.
    @subheading{The \"row-collapsed\" signal}
      @begin{pre}
lambda (view iter path)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @class{gtk:tree-view} widget on which the signal is
          emitted.}
        @entry[iter]{The @class{gtk:tree-iter} iterator of the collapsed row.}
        @entry[path]{The @class{gtk:tree-path} instance that points to the row.}
      @end{table}
      The given row has been collapsed (child nodes are hidden).
    @subheading{The \"row-expanded\" signal}
      @begin{pre}
lambda (view iter path)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @class{gtk:tree-view} widget on which the signal is
          emitted.}
        @entry[iter]{The @class{gtk:tree-iter} iterator of the expanded row.}
        @entry[path]{The @class{gtk:tree-path} instance that points to the row.}
      @end{table}
      The given row has been expanded (child nodes are shown).
    @subheading{The \"select-all\" signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @class{gtk:tree-view} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"select-cursor-parent\" signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @class{gtk:tree-view} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"select-cursor-row\" signal}
      @begin{pre}
lambda (view arg)    :action
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @class{gtk:tree-view} widget on which the signal is
          emitted.}
        @entry[arg]{a boolean without description}
        @entry[Returns]{a boolean without description}
      @end{table}
    @subheading{The \"start-interactive-search\" signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @class{gtk:tree-view} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"test-collapse-row\" signal}
      @begin{pre}
lambda (view iter path)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @class{gtk:tree-view} widget on which the signal is
          emitted.}
        @entry[iter]{The @class{gtk:tree-iter} iterator of the row to
          collapsed.}
        @entry[path]{The @class{gtk:tree-path} instance that points to the row.}
        @entry[Returns]{@em{False} to allow collapsing, @em{true} to reject.}
      @end{table}
      The given row is about to be collapsed (hide its children nodes). Use
      this signal if you need to control the collapsibility of individual rows.
    @subheading{The \"test-expand-row\" signal}
      @begin{pre}
lambda (view iter path)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @class{gtk:tree-view} widget on which the signal is
          emitted.}
        @entry[iter]{The @class{gtk:tree-iter} iterator of the row to expand.}
        @entry[path]{The @class{gtk:tree-path} instance that points to the row.}
        @entry[Returns]{@em{False} to allow expansion, @em{true} to reject.}
      @end{table}
      The given row is about to be expanded (show its children nodes). Use this
      signal if you need to control the expandability of individual rows.
    @subheading{The \"toggle-cursor-row\" signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @class{gtk:tree-view} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"unselect-all\" signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @class{gtk:tree-view} widget on which the signal is
          emitted.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:tree-view-new}
  @see-constructor{gtk:tree-view-new-with-model}
  @see-slot{gtk:tree-view-activate-on-single-click}
  @see-slot{gtk:tree-view-enable-grid-lines}
  @see-slot{gtk:tree-view-enable-search}
  @see-slot{gtk:tree-view-enable-tree-lines}
  @see-slot{gtk:tree-view-expander-column}
  @see-slot{gtk:tree-view-fixed-height-mode}
  @see-slot{gtk:tree-view-headers-clickable}
  @see-slot{gtk:tree-view-headers-visible}
  @see-slot{gtk:tree-view-hover-expand}
  @see-slot{gtk:tree-view-hover-selection}
  @see-slot{gtk:tree-view-level-indentation}
  @see-slot{gtk:tree-view-model}
  @see-slot{gtk:tree-view-reorderable}
  @see-slot{gtk:tree-view-rubber-banding}
  @see-slot{gtk:tree-view-search-column}
  @see-slot{gtk:tree-view-show-expanders}
  @see-slot{gtk:tree-view-tooltip-column}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:tree-selection}
  @see-class{gtk:tree-model}
  @see-class{gtk:list-view}
  @see-class{gtk:column-view}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:tree-view-activate-on-single-click ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "activate-on-single-click"
                                               'tree-view) t)
 "The @code{activate-on-single-click} property of type @code{:boolean}
  (Read / Write) @br{}
  Specifies whether the @code{\"row-activated\"} signal will be emitted after a
  single click. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-activate-on-single-click)
      "Accessor"
      (documentation 'tree-view-activate-on-single-click 'function)
 "@version{2024-3-10}
  @syntax{(gtk:tree-view-activate-on-single-click object) => setting}
  @syntax{(setf (gtk:tree-view-activate-on-single-click object) setting)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[setting]{a boolean that is @em{true} to emit the
    @code{\"row-activated\"} signal on a single click}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{activate-on-single-click} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  Cause the @code{\"row-activated\"} signal to be emitted on a single click
  instead of a double click.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}")

;;; --- gtk:tree-view-enable-grid-lines ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-grid-lines"
                                               'tree-view) t)
 "The @code{enable-grid-lines} property of type
  @symbol{gtk:tree-view-grid-lines} (Read / Write) @br{}
  Whether grid lines should be drawn in the tree view. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-enable-grid-lines)
      "Accessor"
      (documentation 'tree-view-enable-grid-lines 'function)
 "@version{2024-3-9}
  @syntax{(gtk:tree-view-enable-grid-lines object) => setting}
  @syntax{(setf (gtk:tree-view-enable-grid-lines object) setting)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[setting]{a boolean whether grid lines should be drawn}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{enable-grid-lines} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  Whether grid lines should be drawn in the tree view.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}")

;;; --- gtk:tree-view-enable-search --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-search" 'tree-view) t)
 "The @code{enable-search} property of type @code{:boolean} (Read / Write) @br{}
  Whether the tree view allows user to search through columns interactively.
  @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-enable-search)
      "Accessor"
      (documentation 'tree-view-enable-search 'function)
 "@version{2024-3-10}
  @syntax{(gtk:tree-view-enable-search object) => enable}
  @syntax{(setf (gtk:tree-view-enable-search object) enable)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[enable]{@em{true}, if the user can search interactively}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{enable-search} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  If the @slot[gtk:tree-view]{enable-search} property is set, then the user can
  type in text to search through the tree view interactively, this is sometimes
  called \"typeahead find\". Note that even if the property is @em{false}, the
  user can still initiate a search using the \"start-interactive-search\" key
  binding.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}")

;;; --- gtk:tree-view-enable-tree-lines ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-tree-lines"
                                               'tree-view) t)
 "The @code{enable-tree-lines} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether tree lines should be drawn in the tree view. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-enable-tree-lines)
      "Accessor"
      (documentation 'tree-view-enable-tree-lines 'function)
 "@version{2024-3-9}
  @syntax{(gtk:tree-view-enable-tree-lines object) => enable}
  @syntax{(setf (gtk:tree-view-enable-tree-lines object) enable)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[enable]{@em{true}, to enable tree line drawing, @em{false}
    otherwise}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{enable-tree-lines} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  The @fun{gtk:tree-view-enable-tree-lines} function returns whether or not
  tree lines are drawn in the tree view. The
  @setf{gtk:tree-view-enable-tree-lines} function sets whether to draw lines.
  This does not have any visible effects for lists.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}")

;;; --- gtk:tree-view-expander-column  -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expander-column" 'tree-view) t)
 "The @code{expander-column} property of type @class{gtk:tree-view-column}
  (Read / Write) @br{}
  The column for the expander column.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-expander-column)
      "Accessor"
      (documentation 'tree-view-expander-column 'function)
 "@version{2024-3-9}
  @syntax{(gtk:tree-view-expander-column object) => column}
  @syntax{(setf (gtk:tree-view-expander-column object) column)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[column]{a @class{gtk:tree-view-column} object with the column to
    draw the expander arrow at, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{expander-column} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  The @fun{gtk:tree-view-expander-column} function returns the column that is
  the current expander column. The @setf{gtk:tree-view-expander-column} function
  sets the column to draw the expander arrow at. It must be in the tree view. If
  the @arg{column} argument is @code{nil}, then the expander arrow is always at
  the first visible column.

  If you do not want expander arrow to appear in your tree, set the expander
  column to a hidden column.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-view-column}")

;;; --- gtk:tree-view-fixed-height-mode ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "fixed-height-mode"
                                               'tree-view) t)
 "The @code{fixed-height-mode} property of type @code{:boolean} (Read / Write)
  @br{}
  Setting this property to @em{true} speeds up the tree view by assuming that
  all rows have the same height. Only enable this option if all rows are the
  same height. Please see the @fun{gtk:tree-view-fixed-height-mode} function
  for more information on this option. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-fixed-height-mode)
      "Accessor"
      (documentation 'tree-view-fixed-height-mode 'function)
 "@version{2024-3-10}
  @syntax{(gtk:tree-view-fixed-height-mode object) => enable}
  @syntax{(setf (gtk:tree-view-fixed-height-mode object) enable)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[enable]{@em{true} to enable fixed height mode}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{fixed-height-mode} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  The @fun{gtk:tree-view-fixed-height-mode} function returns whether fixed
  height mode is turned on for the tree view. The
  @setf{gtk:tree-view-fixed-height-mode} function enables or disables the fixed
  height mode.

  Fixed height mode speeds up the tree view by assuming that all rows have the
  same height. Only enable this option if all rows are the same height and all
  columns have the @code{:fixed} value of the
  @symbol{gtk:tree-view-column-sizing} enumeration.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-symbol{gtk:tree-view-column-sizing}")

;;;  --- gtk:tree-view-headers-clickable ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "headers-clickable"
                                               'tree-view) t)
 "The @code{headers-clickable} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether column headers respond to click events. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-headers-clickable)
      "Accessor"
      (documentation 'tree-view-headers-clickable 'function)
 "@version{2024-3-9}
  @syntax{(gtk:tree-view-headers-clickable object) => setting}
  @syntax{(setf (gtk:tree-view-headers-clickable object) setting)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[setting]{@em{true} if the columns are clickable}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{headers-clickable} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  The @fun{gtk:tree-view-headers-clickable} function returns whether all header
  columns are clickable. The @setf{gtk:tree-view-headers-clickable} function
  allow the column title buttons to be clicked.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}")

;;; --- gtk:tree-view-headers-visible ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "headers-visible" 'tree-view) t)
 "The @code{headers-visible} property of type @code{:boolean} (Read / Write)
  @br{}
  Wether to show the column header buttons. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-headers-visible)
      "Accessor"
      (documentation 'tree-view-headers-visible 'function)
 "@version{2024-3-9}
  @syntax{(gtk:tree-view-headers-visible object) => visible}
  @syntax{(setf (gtk:tree-view-headers-visible object) visible)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[visible]{@em{true} if the headers are visible}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{headers-visible} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  The @fun{gtk:tree-view-headers-visible} function returns @em{true} if the
  headers on the tree view are visible. The @setf{gtk:tree-view-headers-visible}
  function sets the visibility state of the headers.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}")

;;; --- gtk:tree-view-hover-expand ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hover-expand" 'tree-view) t)
 "The @code{hover-expand} property of type @code{:boolean} (Read / Write) @br{}
  Enables or disables the hover expansion mode of the tree view. Hover
  expansion makes rows expand or collapse if the pointer moves over them. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-hover-expand)
      "Accessor"
      (documentation 'tree-view-hover-expand 'function)
 "@version{2024-3-9}
  @syntax{(gtk:tree-view-hover-expand object) => expand}
  @syntax{(setf (gtk:tree-view-hover-expand object) expand)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[expand]{@em{true} to enable hover selection mode}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{hover-expand} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  The @fun{gtk:tree-view-hover-expand} function returns whether hover expansion
  mode is turned on for the tree view. The @setf{gtk:tree-view-hover-expand}
  function enables or disables the hover expansion mode. Hover expansion makes
  rows expand or collapse if the pointer moves over them.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}")

;;; --- gtk:tree-view-hover-selection ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hover-selection" 'tree-view) t)
 "The @code{hover-selection} property of type @code{:boolean} (Read / Write)
  @br{}
  Enables or disables the hover selection mode of the tree view. Hover
  selection makes the selected row follow the pointer. Currently, this works
  only for the @code{:single} and @code{:browse} selection modes. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-hover-selection)
      "Accessor"
      (documentation 'tree-view-hover-selection 'function)
 "@version{2024-3-10}
  @syntax{(gtk:tree-view-hover-selection object) => setting}
  @syntax{(setf (gtk:tree-view-hover-selection object) setting)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[setting]{@em{true} to enable hover selection mode}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{hover-selection} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  The @fun{gtk:tree-view-hover-selection} function returns whether hover
  selection mode is turned on for the tree view. The
  @setf{gtk:tree-view-hover-selection} function enables or disables the hover
  selection mode. Hover selection makes the selected row follow the pointer.
  Currently, this works only for the @code{:single} and @code{:browse} selection
  modes.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-symbol{gtk:selection-mode}")

;;; --- gtk:tree-view-level-indentation ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "level-indentation"
                                               'tree-view) t)
 "The @code{level-indentation} property of type @code{:int} (Read / Write) @br{}
  The extra indentation for each level. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-level-indentation)
      "Accessor"
      (documentation 'tree-view-level-indentation 'function)
 "@version{2024-3-9}
  @syntax{(gtk:tree-view-level-indentation object) => indentation}
  @syntax{(setf (gtk:tree-view-level-indentation object) indentation)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[indentation]{an integer with the amount, in pixels, of extra
    indentation}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{level-indentation} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  The @fun{gtk:tree-view-level-indentation} function returns the amount, in
  pixels, of extra indentation for child levels in the tree view in addition to
  the default indentation. The @setf{gtk:tree-view-level-indentation} function
  sets the amount of extra indentation.

  The value should be specified in pixels, a value of 0 disables this feature
  and in this case only the default indentation will be used. This does not
  have any visible effects for lists.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}")

;;; --- gtk:tree-view-model ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'tree-view) t)
 "The @code{model} property of type @class{gtk:tree-model} (Read / Write) @br{}
  The model for the tree view.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-model)
      "Accessor"
      (documentation 'tree-view-model 'function)
 "@version{2024-3-9}
  @syntax{(gtk:tree-view-model object) => model}
  @syntax{(setf (gtk:tree-view-model object) model)}
  @argument[objet]{a @class{gtk:tree-view} widget}
  @argument[model]{a @class{gtk:tree-model} object}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{model} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  The @fun{gtk:tree-view-model} function returns the model the tree view is
  based on. Returns @code{nil} if the model is unset. The
  @setf{gtk:tree-view-model} function sets the model. If the tree view already
  has a model set, it will remove it before setting the new model. If the
  @arg{model} argument is @code{nil}, then it will unset the old model.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-model}")

;;; --- gtk:tree-view-reorderable ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "reorderable" 'tree-view) t)
 "The @code{reorderable} property of type @code{:boolean} (Read / Write) @br{}
  The tree view is reorderable. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-reorderable)
      "Accessor"
      (documentation 'tree-view-reorderable 'function)
 "@version{2024-3-10}
  @syntax{(gtk:tree-view-reorderable object) => reorderable}
  @syntax{(setf (gtk:tree-view-reorderable object) rorderable)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[reorderable]{@em{true}, if the tree view can be reordered}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{reorderable} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  This function is a convenience function to allow you to reorder models that
  support the @class{gtk:tree-drag-source} and the @class{gtk:tree-drag-dest}
  interfaces.

  Both the @class{gtk:tree-store} and the @class{gtk:list-store} classes support
  these. If the @arg{reorderable} argument is @em{true}, then the user can
  reorder the model by dragging and dropping rows. The developer can listen to
  these changes by connecting to the model's @code{\"row-inserted\"} and
  @code{\"row-deleted\"} signals. The reordering is implemented by setting up
  the tree view as a drag source and destination. Therefore, drag and drop can
  not be used in a reorderable view for any other purpose.

  This function does not give you any degree of control over the order - any
  reordering is allowed. If more control is needed, you should probably handle
  drag and drop manually.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-drag-source}
  @see-class{gtk:tree-drag-dest}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-store}")

;;; --- gtk:tree-view-rubber-banding -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "rubber-banding" 'tree-view) t)
 "The @code{rubber-banding} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to enable selection of multiple items by dragging the mouse
  pointer. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-rubber-banding)
      "Accessor"
      (documentation 'tree-view-rubber-banding 'function)
 "@version{2024-3-10}
  @syntax{(gtk:tree-view-rubber-banding object) => enable}
  @syntax{(setf (gtk:tree-view-rubber-banding object) enable)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[enable]{@em{true}, to enable rubber banding}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{rubber-banding} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  The @fun{gtk:tree-view-rubber-banding} function returns whether rubber
  banding is turned on for the tree view. The
  @setf{gtk:tree-view-rubber-banding} function enables or disables rubber
  banding. If the selection mode is @code{:multiple}, rubber banding will allow
  the user to select multiple rows by dragging the mouse.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-symbol{gtk:selection-mode}")

;;; --- gtk:tree-view-search-column --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "search-column" 'tree-view) t)
 "The @code{search-column} property of type @code{:int} (Read / Write) @br{}
  Model column to search through during interactive search. @br{}
  The allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-search-column)
      "Accessor"
      (documentation 'tree-view-search-column 'function)
 "@version{2024-3-10}
  @syntax{(gtk:tree-view-search-column object) => column}
  @syntax{(setf (gtk:tree-view-search-column object) column)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[column]{an integer with the column of the model to search in, or
    -1 to disable searching}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{search-column} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  The @fun{gtk:tree-view-search-column} function gets the column searched on by
  the interactive search code. The @setf{gtk:tree-view-search-column} function
  sets @arg{column} as the column where the interactive search code should
  search in for the current model.

  If the search column is set, users can use the \"start-interactive-search\"
  key binding to bring up search popup. The @slot[gtk:tree-view]{enable-search}
  property controls whether simply typing text will also start an interactive
  search.

  Note that the @arg{column} argument refers to a column of the current model.
  The search column is reset to -1 when the model is changed.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-function{gtk:tree-view-enable-search}")

;;; --- gtk:tree-view-show-expanders -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-expanders" 'tree-view) t)
 "The @code{show-expanders} property of type @code{:boolean} (Read / Write)
  @br{}
  @em{True} if the view has expanders. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-show-expanders)
      "Accessor"
      (documentation 'tree-view-show-expanders 'function)
 "@version{2024-3-9}
  @syntax{(gtk:tree-view-show-expanders object) => enabled}
  @syntax{(setf (gtk:tree-view-show-expanders object) enabled)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[enabled]{@em{true} to enable expander drawing, @em{false} otherwise}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{show-expanders} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  The @fun{gtk:tree-view-show-expanders} function returns whether or not
  expanders are drawn in the tree view. The @setf{gtk:tree-view-show-expanders}
  function sets whether to draw and enable expanders and indent child rows in
  the tree view. When disabled there will be no expanders visible in tree views
  and there will be no way to expand and collapse rows by default. Also note
  that hiding the expanders will disable the default indentation. You can set a
  custom indentation in this case using the
  @fun{gtk:tree-view-level-indentation} function. This does not have any visible
  effects for lists.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-function{gtk:tree-view-level-indentation}")

;;; --- gtk:tree-view-tooltip-column -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tooltip-column" 'tree-view) t)
 "The @code{tooltip-column} property of type @code{:int} (Read / Write)@br{}
  The column in the model containing the tooltip texts for the rows. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-tooltip-column)
      "Accessor"
      (documentation 'tree-view-tooltip-column 'function)
 "@version{2024-3-10}
  @syntax{(gtk:tree-view-tooltip-column object) => column}
  @syntax{(setf (gtk:tree-view-tooltip-column object) column)}
  @argument[object]{a @class{gtk:tree-view} widget}
  @argument[column]{an integer which is a valid column number for tree
    view's model}
  @begin{short}
    Accessor of the @slot[gtk:tree-view]{tooltip-column} slot of the
    @class{gtk:tree-view} class.
  @end{short}
  The @fun{gtk:tree-view-tooltip-column} function returns the column of the
  tree view's model which is being used for displaying tooltips on the tree
  view's rows.

  If you only plan to have simple (text-only) tooltips on full rows, you can
  use this function to have the @class{gtk:tree-view} widget handle these
  automatically for you. The @arg{column} argument should be set to the column
  in the tree view's model containing the tooltip texts, or -1 to disable this
  feature.

  When enabled, the @slot[gtk:widget]{has-tooltip} property will be set to
  @em{true} and the tree view will connect a @code{\"query-tooltip\"} signal
  handler.

  Note that the signal handler sets the text with the
  @fun{gtk:tooltip-set-markup} function, so &, <, etc have to be escaped in the
  text.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-function{gtk:tooltip-set-markup}
  @see-function{gtk:widget-has-tooltip}")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_new
;;; ----------------------------------------------------------------------------

(declaim (inline tree-view-new))

(defun tree-view-new ()
 #+liber-documentation
 "@version{2024-3-10}
  @return{The newly created @class{gtk:tree-view} widget.}
  @begin{short}
    Creates a new tree view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-function{gtk:tree-view-new-with-model}"
  (make-instance 'tree-view))

(export 'tree-view-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_new_with_model
;;; ----------------------------------------------------------------------------

(declaim (inline tree-view-new-with-model))

(defun tree-view-new-with-model (model)
 #+liber-documentation
 "@version{2024-3-10}
  @argument[model]{a @class{gtk:tree-model} object}
  @return{The newly created @class{gtk:tree-view} widget.}
  @begin{short}
    Creates a new tree view with the model initialized to @arg{model}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-model}
  @see-function{gtk:tree-view-new}"
  (make-instance 'tree-view
                 :model model))

(export 'tree-view-new-with-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_selection
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_get_selection" tree-view-selection)
    (g:object tree-selection)
 #+liber-documentation
 "@version{2024-3-9}
  @argument[view]{a @class{gtk:tree-view} widget}
  @return{The @class{gtk:tree-selection} object.}
  @begin{short}
    Gets the tree selection associated with the tree view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-selection}"
  (view (g:object tree-view)))

(export 'tree-view-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_columns_autosize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_columns_autosize" tree-view-columns-autosize)
    :void
 #+liber-documentation
 "@version{#2024-3-9}
  @argument[view]{a @class{gtk:tree-view} widget}
  @begin{short}
    Resizes all columns to their optimal width.
  @end{short}
  Only works after the tree view has been realized.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}"
  (view (g:object tree-view)))

(export 'tree-view-columns-autosize)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_append_column
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_append_column" tree-view-append-column) :int
 #+liber-documentation
 "@version{2024-3-9}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[column]{a @class{gtk:tree-view-column} object to add}
  @return{The integer with the number of columns in @arg{view} after appending.}
  @begin{short}
    Appends @arg{column} to the list of columns in the tree view.
  @end{short}
  If @arg{view} has fixed height mode enabled, then @arg{column} must have
  its @slot[gtk:tree-view-column]{sizing} property set to be the @code{:fixed}
  value.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-view-column}
  @see-symbol{gtk:tree-view-column-sizing}"
  (view (g:object tree-view))
  (column (g:object tree-view-column)))

(export 'tree-view-append-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_remove_column
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_remove_column" tree-view-remove-column) :int
 #+liber-documentation
 "@version{2024-3-9}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[column]{a @class{gtk:tree-view-column} object to remove}
  @return{The integer with the number of columns in @arg{view} after removing.}
  @begin{short}
    Removes a column from the tree view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-view-column}"
  (view (g:object tree-view))
  (column (g:object tree-view-column)))

(export 'tree-view-remove-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_insert_column
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_insert_column" tree-view-insert-column) :int
 #+liber-documentation
 "@version{2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[column]{a @class{gtk:tree-view-column} object to be inserted}
  @argument[pos]{an integer with the position to insert @arg{column} in}
  @return{The integer with the number of columns in @arg{view} after insertion.}
  @begin{short}
    This inserts the column into the tree view at the given position.
  @end{short}
  If the @arg{pos} argument is -1, then the column is inserted at the end. If
  @arg{view} has the @slot[gtk:tree-view]{fixed-height-mode} property enabled,
  then @arg{column} must have its @slot[gtk:tree-view-column]{sizing} property
  set to be the @code{:fixed} value.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-fixed-height-mode}
  @see-function{gtk:tree-view-column-sizing}"
  (view (g:object tree-view))
  (column (g:object tree-view-column))
  (pos :int))

(export 'tree-view-insert-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_insert_column_with_attributes
;;; ----------------------------------------------------------------------------

(defun tree-view-insert-column-with-attributes (view
                                                pos
                                                title
                                                renderer &rest attributes)
 #+liber-documentation
 "@version{2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[pos]{an integer with the position to insert the new column in}
  @argument[title]{a string with the title to set the header to}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[attributes]{pairs of attributes}
  @return{The integer with the number of columns in @arg{view} after insertion.}
  @begin{short}
    Creates a new @class{gtk:tree-view-column} object and inserts it into the
    tree view at @arg{pos}.
  @end{short}
  If the @arg{pos} argument is -1, then the newly created column is inserted at
  the end. The column is initialized with the attributes given. If @arg{view}
  has the @slot[gtk:tree-view]{fixed-height-mode} property enabled, then the
  new column will have its @slot[gtk:tree-view-column]{sizing} property set to
  be the @code{:fixed} value.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:cell-renderer}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-fixed-height-mode}
  @see-function{gtk:tree-view-column-sizing}"
  (let ((column (tree-view-column-new)))
    (when (tree-view-fixed-height-mode view)
      (setf (tree-view-column-sizing column) :fixed))
    (setf (tree-view-column-title column) title)
    (tree-view-column-pack-start column renderer :expand t)
    (apply #'tree-view-column-set-attributes column renderer attributes)
    (tree-view-insert-column view column pos)))

(export 'tree-view-insert-column-with-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_insert_column_with_data_func
;;; ----------------------------------------------------------------------------

(defun tree-view-insert-column-with-data-func (view pos title renderer func)
 #+liber-documentation
 "@version{2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[pos]{an integer with the position to insert the new column in}
  @argument[title]{an string with the title to set the header to}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[func]{a @symbol{gtk:tree-cell-data-func} callback function to set
    attributes of cell renderer}
  @return{The integer with the number of columns in @arg{view} after insertion.}
  @begin{short}
    Convenience function that inserts a new column into the tree view with the
    given cell renderer and a @symbol{gtk:tree-cell-data-func} callback function
    to set cell renderer attributes (normally using data from the model).
  @end{short}
  See also the @fun{gtk:tree-view-column-set-cell-data-func} and
  @fun{gtk:tree-view-column-pack-start} functions. If @arg{view} has the
  @slot[gtk:tree-view]{fixed-height-mode} property enabled, then the new column
  will have its @slot[gtk:tree-view-column]{sizing} property set to be the
  @code{:fixed} value.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:cell-renderer}
  @see-symbol{gtk:tree-cell-data-func}
  @see-function{gtk:tree-view-column-set-cell-data-func}
  @see-function{gtk:tree-view-column-pack-start}
  @see-function{gtk:tree-view-fixed-height-mode}
  @see-function{gtk:tree-view-column-sizing}"
  (let ((column (tree-view-column-new)))
    (when (tree-view-fixed-height-mode view)
      (setf (tree-view-column-sizing column) :fixed))
    (setf (tree-view-column-title column) title)
    (tree-view-column-pack-start column renderer :expand t)
    (tree-view-column-set-cell-data-func column renderer func)
    (tree-view-insert-column view column pos)))

(export 'tree-view-insert-column-with-data-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_n_columns
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_get_n_columns" tree-view-n-columns) :int
 #+liber-documentation
 "@version{2024-3-9}
  @argument[view]{a @class{gtk:tree-view} widget}
  @return{The integer with the number of columns in the tree view.}
  @begin{short}
    Queries the number of columns in the given tree view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}"
  (view (g:object tree-view)))

(export 'tree-view-n-columns)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_column
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_get_column" tree-view-column)
    (g:object tree-view-column)
 #+liber-documentation
 "@version{2024-3-9}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[pos]{an integer with the position of the column, counting from 0}
  @begin{return}
    The @class{gtk:tree-view-column} object, or @code{nil} if the position is
    outside the range of columns.
  @end{return}
  @begin{short}
    Gets the tree view column at the given position in the tree view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-view-column}"
  (view (g:object tree-view))
  (pos :int))

(export 'tree-view-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_columns
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_get_columns" tree-view-columns)
    (g:list-t (g:object tree-view-column))
 #+liber-documentation
 "@version{2024-3-9}
  @argument[view]{a @class{gtk:tree-view} widget}
  @return{The list of @class{gtk:tree-view-column} objects.}
  @begin{short}
    Returns a list of all the tree view columns currently in the tree view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-view-column}"
  (view (g:object tree-view)))

(export 'tree-view-columns)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_move_column_after
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_move_column_after" tree-view-move-column-after)
    :void
 #+liber-documentation
 "@version{2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[column]{a @class{gtk:tree-view-column} object to be moved}
  @argument[base]{a @class{gtk:tree-view-column} object to be moved relative to,
    or @code{nil}}
  @begin{short}
    Moves @arg{column} to be after to @arg{base}.
  @end{short}
  If the @arg{base} argument is @code{nil}, then @arg{column} is placed in the
  first position.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-view-column}"
  (view (g:object tree-view))
  (column (g:object tree-view-column))
  (base (g:object tree-view-column)))

(export 'tree-view-move-column-after)

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewColumnDropFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback tree-view-column-drop-func :boolean
    ((view (g:object tree-view))
     (column (g:object tree-view-column))
     (prev (g:object tree-view-column))
     (next (g:object tree-view-column))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func view column prev next)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-view-column-drop-func)
      "Callback"
      (liber:symbol-documentation 'tree-view-column-drop-func)
 "@version{#2024-5-4}
  @syntax{lambda (view column prev next) => result}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[column]{a @class{gtk:tree-view-column} object being dragged}
  @argument[prev]{a @class{gtk:tree-view-column} object on one side of
    @arg{column}}
  @argument[next]{a @class{gtk:tree-view-column} object on the other side of
    @arg{column}}
  @argument[result]{@em{true}, if @arg{column} can be dropped in this spot}
  @begin{short}
    Callback function type for determining whether @arg{column} can be dropped
    in a particular spot as determined by @arg{prev} and @arg{next}.
  @end{short}
  In left to right locales, @arg{prev} is on the left of the potential drop
  spot, and @arg{next} is on the right. In right to left mode, this is reversed.
  This callback function should return @em{true} if the spot is a valid drop
  spot. Please note that returning @em{true} does not actually indicate that
  the column drop was made, but is meant only to indicate a possible drop spot
  to the user.
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-view-column}")

(export 'tree-view-column-drop-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_column_drag_function
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_set_column_drag_function"
               %tree-view-set-column-drag-function) :void
  (view (g:object tree-view))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun tree-view-set-column-drag-function (view func)
 #+liber-documentation
 "@version{2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[func]{a @symbol{gtk:tree-view-column-drop-func} callback function
    to determine which columns are reorderable, or @code{nil}}
  @begin{short}
    Sets a user @symbol{gtk:tree-view-column-drop-func} callback function for
    determining where a column may be dropped when dragged.
  @end{short}
  This function is called on every column pair in turn at the beginning of a
  column drag to determine where a drop can take place. The arguments passed to
  @arg{func} are: a @class{gtk:tree-view} widget, a @class{gtk:tree-view-column}
  object being dragged, and the two @class{gtk:tree-view-column} objects
  determining the drop spot. If either of the @class{gtk:tree-view-column}
  arguments for the drop spot are @code{nil}, then they indicate an edge. If
  @arg{func} is set to be @code{nil}, then @arg{view} reverts to the default
  behavior of allowing all columns to be dropped everywhere.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-view-column}"
  (if func
      (%tree-view-set-column-drag-function
              view
              (cffi:callback tree-view-column-drop-func)
              (glib:allocate-stable-pointer func)
              (cffi:callback glib:stable-pointer-destroy-notify))
      (%tree-view-set-column-drag-function
              view
              (cffi:null-pointer)
              (cffi:null-pointer)
              (cffi:null-pointer))))

(export 'tree-view-set-column-drag-function)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_scroll_to_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_scroll_to_point" tree-view-scroll-to-point) :void
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[tx]{an integer with the x coordinate of new top-left pixel
    of visible area, or -1}
  @argument[ty]{an integer with the y coordinate of new top-left pixel
    of visible area, or -1}
  @begin{short}
    Scrolls the tree view such that the top-left corner of the visible area is
    @arg{tx}, @arg{ty}, where @arg{tx} and @arg{ty} are specified in tree
    coordinates.
  @end{short}
  The tree view must be realized before this function is called. If it is
  not, you probably want to be using the @fun{gtk:tree-view-scroll-to-cell}
  function.

  If either @arg{tx} or @arg{ty} are -1, then that direction is not scrolled.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-function{gtk:tree-view-scroll-to-cell}"
  (view (g:object tree-view))
  (tx :int)
  (ty :int))

(export 'tree-view-scroll-to-point)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_scroll_to_cell
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_scroll_to_cell" %tree-view-scroll-to-cell) :void
  (view (g:object tree-view))
  (path (g:boxed tree-path))
  (column (g:object tree-view-column))
  (use-align :boolean)
  (row-align :float)
  (col-align :float))

(defun tree-view-scroll-to-cell (view path column
                                      &optional
                                      (row-align 0.5 row-align-supplied-p)
                                      (col-align 0.5 col-align-supplied-p))
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance of the row to move to, or
    @code{nil}}
  @argument[column]{a @class{gtk:tree-view-column} object to move horizontally
    to, or @code{nil}}
  @argument[row-align]{an optional float with the vertical alignment of the row
    specified by path, the default is 0.5}
  @argument[col-align]{an optional float with the horizontal alignment of the
    column specified by column, the default is 0.5}
  @begin{short}
    Moves the alignments of the tree view to the position specified by
    @arg{column} and @arg{path}.
  @end{short}
  If the @arg{column} argument is @code{nil}, then no horizontal scrolling
  occurs. Likewise, if the @arg{path} argument is @code{nil} no vertical
  scrolling occurs. At a minimum, one of @arg{column} or @arg{path} need to be
  non-@code{nil}. The @arg{row-align} argument determines where the row is
  placed, and the @arg{col-align} argument determines where @arg{column} is
  placed. Both are expected to be between 0.0 and 1.0. The 0.0 value means
  left/top alignment, the 1.0 value means right/bottom alignment, and the
  0.5 value means center.

  If the cell is currently visible on the screen, nothing is done.

  This function only works if the model is set, and @arg{path} is a valid row
  on the model. If the model changes before the tree view is realized, the
  centered path will be modified to reflect this change.
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-view-column}"
  (%tree-view-scroll-to-cell view
                             path
                             column
                             (or row-align-supplied-p col-align-supplied-p)
                             row-align
                             col-align))

(export 'tree-view-scroll-to-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_cursor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_set_cursor" %tree-view-set-cursor) :void
  (view (g:object tree-view))
  (path (g:boxed tree-path))
  (focus (g:object tree-view-column))
  (start :boolean))

(defun tree-view-set-cursor (view path &key focus start)
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance}
  @argument[focus]{a @class{gtk:tree-view-column} object, or @code{nil}}
  @argument[start]{@em{true} if the specified cell should start being
    edited}
  @begin{short}
    Sets the current keyboard focus to be at @arg{path}, and selects it.
  @end{short}
  This is useful when you want to focus the user's attention on a particular
  row. If the @arg{focus} argument is not @code{nil}, then focus is given to
  the column specified by it. Additionally, if the @arg{focus} argument is
  specified, and the @arg{start} argument is @em{true}, then editing should be
  started in the specified cell. This function is often followed by the
  @fun{gtk:widget-grab-focus} function in order to give keyboard focus to the
  widget. Please note that editing can only happen when the tree view is
  realized.

  If the @arg{path} argument is invalid for the model, the current cursor
  (if any) will be unset and the function will return without failing.
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:widget-grab-focus}"
  (%tree-view-set-cursor view path focus start))

(export 'tree-view-set-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_cursor_on_cell
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_set_cursor_on_cell" %tree-view-set-cursor-on-cell)
    :void
  (view (g:object tree-view))
  (path (g:boxed tree-path))
  (focus (g:object tree-view-column))
  (cell (g:object cell-renderer))
  (start :boolean))

(defun tree-view-set-cursor-on-cell (view path &key focus cell start)
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance}
  @argument[focus]{a @class{gtk:tree-view-column}, or @code{nil}}
  @argument[cell]{a @class{gtk:cell-renderer}, or @code{nil}}
  @argument[start]{@em{true} if the specified cell should start being edited}
  @begin{short}
    Sets the current keyboard focus to be at @arg{path}, and selects it.
  @end{short}
  This is useful when you want to focus the user's attention on a particular
  row. If the @arg{focus} argument is not @code{nil}, then focus is given to
  the column specified by it. If the @arg{focus} and @arg{cell} arguments are
  not @code{nil}, and the @arg{focus} argument contains 2 or more editable or
  activatable cells, then focus is given to the cell specified by @arg{cell}.
  Additionally, if the @arg{focus} argument is specified, and the @arg{start}
  argument is @em{true}, then editing should be started in the specified cell.
  This function is often followed by the @fun{gtk:widget-grab-focus} function
  in order to give keyboard focus to the widget. Please note that editing can
  only happen when the tree view is realized.

  If the @arg{path} argument is invalid for the model, the current cursor
  (if any) will be unset and the function will return without failing.
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:widget-grab-focus}"
  (%tree-view-set-cursor-on-cell view path focus cell start))

(export 'tree-view-set-cursor-on-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_cursor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_get_cursor" %tree-view-get-cursor) :void
  (view (g:object tree-view))
  (path :pointer)
  (focus :pointer))

(defun tree-view-get-cursor (view)
 #+liber-documentation
 "@version{2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @begin{return}
    @arg{path} -- a @class{gtk:tree-path} instance with the cursor path,
      or @code{nil} @br{}
    @arg{focus} -- a @class{gtk:tree-view-column} object with the focus column,
      or @code{nil}
  @end{return}
  @begin{short}
    Returns @arg{path} and @arg{focus} with the current path and focus column.
  @end{short}
  If the cursor is not currently set, then @arg{path} will be @code{nil}. If no
  column currently has focus, then @arg{focus} will be @code{nil}.
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-view-column}"
  (cffi:with-foreign-objects ((path :pointer) (focus :pointer))
    (%tree-view-get-cursor view path focus)
    (values (cffi:mem-ref path '(g:boxed tree-path :return))
            (cffi:mem-ref focus '(g:object tree-view-column)))))

(export 'tree-view-get-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_row_activated
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_row_activated" tree-view-row-activated) :void
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance to be activated}
  @argument[column]{a @class{gtk:tree-view-column} object to be activated}
  @begin{short}
    Activates the cell determined by @arg{path} and @arg{column}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:tree-path}"
  (view (g:object tree-view))
  (path (g:boxed tree-path))
  (column (g:object tree-view-column)))

(export 'tree-view-row-activated)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_expand_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_expand_all" tree-view-expand-all) :void
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @begin{short}
    Recursively expands all nodes in the tree view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}"
  (view (g:object tree-view)))

(export 'tree-view-expand-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_collapse_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_collapse_all" tree-view-collapse-all) :void
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @begin{short}
    Recursively collapses all visible, expanded nodes in the tree view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}"
  (view (g:object tree-view)))

(export 'tree-view-collapse-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_expand_to_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_expand_to_path" tree-view-expand-to-path) :void
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance to a row}
  @begin{short}
    Expands the row at @arg{path}.
  @end{short}
  This will also expand all parent rows of @arg{path} as necessary.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}"
  (view (g:object tree-path))
  (path (g:boxed tree-path)))

(export 'tree-view-expand-to-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_expand_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_expand_row" tree-view-expand-row) :boolean
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance to a row}
  @argument[all]{a boolean whether to recursively expand, or just expand
    immediate children}
  @return{@em{True} if the row existed and had children.}
  @begin{short}
    Opens the row so its children are visible.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}"
  (view (g:object tree-view))
  (path (g:boxed tree-path))
  (all :boolean))

(export 'tree-view-expand-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_collapse_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_collapse_row" tree-view-collapse-row) :boolean
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance to a row in the tree view}
  @return{@em{True} if the row was collapsed.}
  @begin{short}
    Collapses a row (hides its child rows, if they exist).
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}"
  (view (g:object tree-view))
  (path (g:boxed tree-path)))

(export 'tree-view-collapse-row)

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewMappingFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback tree-view-mapping-func :void
    ((view (g:object tree-view))
     (path (g:boxed tree-path))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (funcall func view path)))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-view-mapping-func)
      "Callback"
      (liber:symbol-documentation 'tree-view-mapping-func)
 "@version{#2024-5-4}
  @syntax{lambda (view path)}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance that is expanded}
  @begin{short}
    Callback function used for the @fun{gtk:tree-view-map-expanded-rows}
    function.
  @end{short}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}
  @see-function{gtk:tree-view-map-expanded-rows}")

(export 'tree-view-mapping-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_map_expanded_rows
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_map_expanded_rows" %tree-view-map-expanded-rows)
    :void
  (view (g:object tree-view))
  (func :pointer)
  (data :pointer))

(defun tree-view-map-expanded-rows (view func)
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[func]{a @symbol{gtk:tree-view-mapping-func} callback function to be
    called}
  @begin{short}
    Calls @arg{func} on all expanded rows.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-symbol{gtk:tree-view-mapping-func}"
  (glib:with-stable-pointer (ptr func)
    (%tree-view-map-expanded-rows view
                                  (cffi:callback tree-view-mapping-func)
                                  ptr)))

(export 'tree-view-map-expanded-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_row_expanded
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_row_expanded" tree-view-row-expanded) :boolean
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance to test expansion state}
  @return{@em{True} if @arg{path} is expanded.}
  @begin{short}
    Returns @em{true} if the node pointed to by @arg{path} is expanded in the
    tree view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}"
  (view (g:object tree-view))
  (path (g:boxed tree-path)))

(export 'tree-view-row-expanded)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_path_at_pos
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_get_path_at_pos" %tree-view-path-at-pos) :boolean
  (view (g:object tree-view))
  (x :int)
  (y :int)
  (path :pointer)
  (column :pointer)
  (xcell :pointer)
  (ycell :pointer))

(defun tree-view-path-at-pos (view x y)
 #+liber-documentation
 "@version{#2024-5-16}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[x]{an integer with the x position to be identified (relative to
    the bin window)}
  @argument[y]{an integer with the y position to be identified (relative to
    the bin window)}
  @begin{return}
    @arg{path} -- a @class{gtk:tree-path} instance, or @code{nil} @br{}
    @arg{column} -- a @class{gtk:tree-view-column} object, or @code{nil} @br{}
    @arg{xcell} -- an integer with the x coordinate relative to the cell @br{}
    @arg{ycell} -- an integer with the y coordinate relative to the cell
  @end{return}
  @begin{short}
    Finds the path at the point @code{(x,y)}, relative to bin window
    coordinates.
  @end{short}
  That is, @arg{x} and @arg{y} are relative to an events coordinates. Widget
  relative coordinates must be converted using the
  @fun{gtk:tree-view-convert-widget-to-bin-window-coords} function. It is
  primarily for things like popup menus. If the @arg{path} argument is
  non-@code{nil}, then it will be filled with the @class{gtk:tree-path} instance
  at that point. If the @arg{column} argument is non-@code{nil}, then it will be
  filled with the column at that point. The @arg{xcell} and @arg{ycell} values
  return the coordinates relative to the cell background, for example, the
  @code{background} argument passed to the @fun{gtk:cell-renderer-snapshot}
  function. This function is only meaningful if @arg{view} is realized.
  Therefore this function will always return @em{false} if @arg{view} is not
  realized or does not have a model.

  For converting widget coordinates, for example, the ones you get from the
  @code{\"query-tooltip\"} signal, please see the
  @fun{gtk:tree-view-convert-widget-to-bin-window-coords} function.
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:cell-renderer-snapshot}
  @see-function{gtk:tree-view-bin-window}
  @see-function{gtk:tree-view-convert-widget-to-bin-window-coords}"
  (cffi:with-foreign-objects ((path :pointer)
                              (column :pointer)
                              (xcell :int)
                              (ycell :int))
    (when (%tree-view-path-at-pos view
                                  x y
                                  path
                                  column
                                  xcell ycell)
      (values (cffi:mem-ref path '(g:boxed tree-path :return))
              (cffi:mem-ref column '(g:object tree-view-column))
              (cffi:mem-ref xcell :int)
              (cffi:mem-ref ycell :int)))))

(export 'tree-view-path-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_is_blank_at_pos
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_is_blank_at_pos" %tree-view-is-blank-at-pos)
    :boolean
  (view (g:object tree-view))
  (x :int)
  (y :int)
  (path :pointer)
  (column :pointer)
  (xcell :pointer)
  (ycell :pointer))

(defun tree-view-is-blank-at-pos (view x y)
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[x]{an integer with the x position to be identified (relative to
    the bin window)}
  @argument[y]{an integer with the y position to be identified (relative to
    the bin window)}
  @begin{return}
    @arg{path} -- a @class{gtk:tree-path} instance, or @code{nil} @br{}
    @arg{column} -- a @class{gtk:tree-view-column} object, or @code{nil} @br{}
    @arg{xcell} -- an integer with the x coordinate relative to the cell,
      or @code{nil} @br{}
    @arg{ycell} -- an integer where the y coordinate relative to the cell,
      or @code{nil}
  @end{return}
  @begin{short}
    Determine whether the point @code{(x,y)} in the tree view is blank, that
    is no cell content nor an expander arrow is drawn at the location.
  @end{short}
  If so, the location can be considered as the background. You might wish to
  take special action on clicks on the background, such as clearing a current
  selection, having a custom context menu or starting rubber banding.

  The @arg{x} and @arg{y} coordinates that are provided must be relative to the
  bin window coordinates. Widget relative coordinates must be converted using
  the @fun{gtk:tree-view-convert-widget-to-bin-window-coords} function.

  For converting widget coordinates, for example, the ones you get from the
  @code{\"query-tooltip\"} signal, please see the
  @fun{gtk:tree-view-convert-widget-to-bin-window-coords} function.

  The @arg{path}, @arg{column}, @arg{xcell} and @arg{ycell} arguments will
  be returned likewise as for the @fun{gtk:tree-view-path-at-pos} function.
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-path-at-pos}
  @see-function{gtk:tree-view-convert-widget-to-bin-window-coords}"
  (cffi:with-foreign-objects ((path :pointer)
                              (column :pointer)
                              (xcell :int)
                              (ycell :int))
    (when (%tree-view-is-blank-at-pos view
                                      x y
                                      path
                                      column
                                      xcell ycell)
      (values (cffi:mem-ref path '(g:boxed tree-path :return))
              (cffi:mem-ref column 'g:object)
              (cffi:mem-ref xcell :int)
              (cffi:mem-ref ycell :int)))))

(export 'tree-view-is-blank-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_cell_area
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_get_cell_area" %tree-view-cell-area) :void
  (view (g:object tree-view))
  (path (g:boxed tree-path))
  (column (g:object tree-view-column))
  (rectangle (g:boxed gdk:rectangle)))

(defun tree-view-cell-area (view path column)
 #+liber-documentation
 "@version{#2024-5-16}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance for the row, or @code{nil}
    to get only horizontal coordinates}
  @argument[column]{a @class{gtk:tree-view-column} object for the column, or
    @code{nil} to get only vertical coordinates}
  @return{The @class{gdk:rectangle} instance with the cell rectangle.}
  @begin{short}
    Returns the bounding rectangle in bin window coordinates for the cell at
    the row specified by @arg{path} and the column specified by @arg{column}.
  @end{short}
  If the @arg{path} argument is @code{nil}, or points to a path not currently
  displayed, the @arg{y} and @arg{height} fields of the rectangle will be
  filled with 0. If the @arg{column} argument is @code{nil}, the @arg{x} and
  @arg{width} fields will be filled with 0. The sum of all cell rectangles does
  not cover the entire tree. There are extra pixels in between rows, for
  example. The returned rectangle is equivalent to the @arg{area} argument
  passed to the @fun{gtk:cell-renderer-snapshot} function. This function is
  only valid if the tree view is realized.
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-view-column}
  @see-class{gdk:rectangle}
  @see-function{gtk:cell-renderer-snapshot}"
  (let ((rect (gdk:rectangle-new)))
    (%tree-view-cell-area view path column rect)
    rect))

(export 'tree-view-cell-area)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_background_area
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_get_background_area" %tree-view-background-area)
    :void
  (view (g:object tree-view))
  (path (g:boxed tree-path))
  (column (g:object tree-view-column))
  (rectangle (g:boxed gdk:rectangle)))

(defun tree-view-background-area (view path column)
 #+liber-documentation
 "@version{#2025-3-30}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance for the row, or @code{nil}
    to get only horizontal coordinates}
  @argument[column]{a @class{gtk:tree-view-column} object for the column, or
    @code{nil} to get only vertical coordiantes}
  @begin{return}
    The @class{gdk:rectangle} instance with the cell background rectangle.
  @end{return}
  @begin{short}
    Returns the bounding rectangle in the bin window coordinates for the cell
    at the row specified by @arg{path} and the column specified by @arg{column}.
  @end{short}
  If the @arg{path} argument is @code{nil}, or points to a node not found in
  the tree, the @arg{y} and @arg{height} fields of the rectangle will be filled
  with 0. If the @arg{column} argument is @code{nil}, the @arg{x} and
  @arg{width} fields will be filled with 0. The returned rectangle is equivalent
  to the @code{background} argument passed to the
  @fun{gtk:cell-renderer-snapshot} function. These background areas tile to
  cover the entire bin window. Contrast with the @code{area} argument, returned
  by the @fun{gtk:tree-view-cell-area} function, which returns only the cell
  itself, excluding surrounding borders and the tree expander area.
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-view-column}
  @see-class{gdk:rectangle}
  @see-function{gtk:cell-renderer-snapshot}
  @see-function{gtk:tree-view-cell-area}"
  (let ((rect (gdk:rectangle-new)))
    (%tree-view-background-area view path column rect)
    rect))

(export 'tree-view-background-area)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_visible_rect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_get_visible_rect" %tree-view-visible-rect) :void
  (view (g:object tree-view))
  (rectangle (g:boxed gdk:rectangle)))

(defun tree-view-visible-rect (view)
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @return{The @class{gdk:rectangle} instance.}
  @begin{short}
    Returns the rectangle with the currently visible region of the tree view
    buffer, in tree view coordinates.
  @end{short}
  Convert to the bin window coordinates with the
  @fun{gtk:tree-view-convert-tree-to-bin-window-coords} function. Tree
  coordinates start at @code{(0,0)} for the first row of the tree view, and
  cover the entire scrollable area of the tree view.
  @see-class{gtk:tree-view}
  @see-class{gdk:rectangle}
  @see-function{gtk:tree-view-convert-tree-to-bin-window-coords}"
  (let ((rect (gdk:rectangle-new)))
    (%tree-view-visible-rect view rect)
    rect))

(export 'tree-view-visible-rect)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_visible_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_get_visible_range" %tree-view-visible-range)
    :boolean
  (view (g:object tree-view))
  (start :pointer)
  (end :pointer))

(defun tree-view-visible-range (view)
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @begin{return}
    @code{start} -- a @class{gtk:tree-path} instance with the start of region,
      or @code{nil} @br{}
    @code{end} -- a @class{gtk:tree-path} instance with the end of region,
      or @code{nil}
  @end{return}
  @begin{short}
    Returns @arg{start} and @arg{end} to be the first and last visible path.
  @end{short}
  Note that there may be invisible paths in between.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}"
  (cffi:with-foreign-objects ((start :pointer) (end :pointer))
    (when (%tree-view-visible-range view start end)
      (values (cffi:mem-ref start '(g:boxed tree-path :return))
              (cffi:mem-ref end '(g:boxed tree-path :return))))))

(export 'tree-view-visible-range)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_bin_window_to_tree_coords
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_convert_bin_window_to_tree_coords"
               %tree-view-convert-bin-window-to-tree-coords) :void
  (view (g:object tree-view))
  (x :int)
  (y :int)
  (tx :pointer)
  (ty :pointer))

(defun tree-view-convert-bin-window-to-tree-coords (view x y)
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[x]{an integer with the x coordinate relative to bin window}
  @argument[y]{an integer with the y coordinate relative to bin window}
  @begin{return}
    @arg{tx} -- an integer with the tree x coordinate @br{}
    @arg{ty} -- an integer with the tree y coordinate
  @end{return}
  @begin{short}
    Converts bin window coordinates to coordinates for the tree view (the full
    scrollable area of the tree view).
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}"
  (cffi:with-foreign-objects ((tx :int) (ty :int))
    (%tree-view-convert-bin-window-to-tree-coords view x y tx ty)
    (values (cffi:mem-ref tx :int)
            (cffi:mem-ref ty :int))))

(export 'tree-view-convert-bin-window-to-tree-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_bin_window_to_widget_coords
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_convert_bin_window_to_widget_coords"
               %tree-view-convert-bin-window-to-widget-coords) :void
  (view (g:object tree-view))
  (x :int)
  (y :int)
  (wx :pointer)
  (wy :pointer))

(defun tree-view-convert-bin-window-to-widget-coords (view x y)
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[x]{an integer with the bin window x coordinate}
  @argument[y]{an integer with the bin window y coordinate}
  @begin{return}
    @arg{wx} -- an integer with the widget x coordinate @br{}
    @arg{wy} -- an integer with the widget y coordinate
  @end{return}
  @begin{short}
    Converts bin window coordinates to widget relative coordinates.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}"
  (cffi:with-foreign-objects ((wx :int) (wy :int))
    (%tree-view-convert-bin-window-to-widget-coords view x y wx wy)
    (values (cffi:mem-ref wx :int)
            (cffi:mem-ref wy :int))))

(export 'tree-view-convert-bin-window-to-widget-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_tree_to_bin_window_coords
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_convert_tree_to_bin_window_coords"
               %tree-view-convert-tree-to-bin-window-coords) :void
  (view (g:object tree-view))
  (x :int)
  (y :int)
  (bx :pointer)
  (by :pointer))

(defun tree-view-convert-tree-to-bin-window-coords (view x y)
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[x]{an integer with the tree x coordinate}
  @argument[y]{an integer with the tree y coordinate}
  @begin{return}
    @arg{bx} -- an integer with the x coordinate relative to bin window @br{}
    @arg{by} -- an integer with the y coordinate relative to bin window
  @end{return}
  @begin{short}
    Converts tree view coordinates, coordinates in full scrollable area of the
    tree view, to bin window coordinates.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}"
  (cffi:with-foreign-objects ((bx :int) (by :int))
    (%tree-view-convert-tree-to-bin-window-coords view x y bx by)
    (values (cffi:mem-ref bx :int)
            (cffi:mem-ref by :int))))

(export 'tree-view-convert-tree-to-bin-window-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_tree_to_widget_coords
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_convert_tree_to_widget_coords"
               %tree-view-convert-tree-to-widget-coords) :void
  (view g:object)
  (x :int)
  (y :int)
  (wx :pointer)
  (wy :pointer))

(defun tree-view-convert-tree-to-widget-coords (view x y)
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[x]{an integer with the x coordinate relative to the tree view}
  @argument[y]{an integer with the y coordinate relative to the tree view}
  @begin{return}
    @arg{wx} -- an integer with the widget x coordinate @br{}
    @arg{wy} -- an integer with the widget y coordinate
  @end{return}
  @begin{short}
    Converts tree view coordinates, coordinates in full scrollable area of the
    tree, to widget coordinates.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}"
  (cffi:with-foreign-objects ((wx :int) (wy :int))
    (%tree-view-convert-tree-to-widget-coords view x y wx wy)
    (values (cffi:mem-ref wx :int)
            (cffi:mem-ref wy :int))))

(export 'tree-view-convert-tree-to-widget-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_widget_to_bin_window_coords
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_convert_widget_to_bin_window_coords"
               %tree-view-convert-widget-to-bin-window-coords) :void
  (tree-view g:object)
  (x :int)
  (y :int)
  (bx :pointer)
  (by :pointer))

(defun tree-view-convert-widget-to-bin-window-coords (view x y)
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[x]{an integer with the x coordinate relative to the widget}
  @argument[y]{an integer with the y coordinate relative to the widget}
  @begin{return}
    @arg{bx} -- an integer with the bin window x coordinate @br{}
    @arg{by} -- an integer with the bin window y coordinate
  @end{return}
  @begin{short}
    Converts widget coordinates to coordinates for the bin window.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}"
  (cffi:with-foreign-objects ((bx :int) (by :int))
    (%tree-view-convert-widget-to-bin-window-coords view x y bx by)
    (values (cffi:mem-ref bx :int)
            (cffi:mem-ref by :int))))

(export 'tree-view-convert-widget-to-bin-window-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_widget_to_tree_coords
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_convert_widget_to_tree_coords"
               %tree-view-convert-widget-to-tree-coords) :void
  (tree-view g:object)
  (x :int)
  (y :int)
  (tx :pointer)
  (ty :pointer))

(defun tree-view-convert-widget-to-tree-coords (view x y)
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[x]{an integer with the x coordinate relative to the widget}
  @argument[y]{an integer with the y coordinate relative to the widget}
  @begin{return}
    @arg{tx} -- an integer with the tree view x coordinate @br{}
    @arg{ty} -- an integer with the tree view y coordinate
  @end{return}
  @begin{short}
    Converts widget coordinates to coordinates for the tree view, the full
    scrollable area of the tree view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}"
  (cffi:with-foreign-objects ((tx :int) (ty :int))
    (%tree-view-convert-widget-to-tree-coords view x y tx ty)
    (values (cffi:mem-ref tx :int)
            (cffi:mem-ref ty :int))))

(export 'tree-view-convert-widget-to-tree-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_enable_model_drag_dest
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_enable_model_drag_dest"
               tree-view-enable-model-drag-dest) :void
 #+liber-documentation
 "@version{#2024-5-28}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[formats]{a @class{gdk:content-formats} instance with the target
    formats that the drag will support}
  @argument[actions]{a @symbol{gdk:drag-action} bitmask of possible actions
    for a drag from this widget}
  @begin{short}
    Turns the tree view into a drop destination for automatic DND.
  @end{short}
  Calling this method sets the @slot[gtk:tree-view]{reorderable} property to
  the @em{false} value.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gdk:content-formats}
  @see-symbol{gdk:drag-action}
  @see-function{gtk:tree-view-reorderable}"
  (view (g:object tree-view))
  (formats (g:boxed gdk:content-formats))
  (actions gdk:drag-action))

(export 'tree-view-enable-model-drag-dest)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_enable_model_drag_source
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_enable_model_drag_source"
               tree-view-enable-model-drag-source) :void
 #+liber-documentation
 "@version{#2024-5-28}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[mask]{a @symbol{gdk:modifier-type} mask of allowed buttons to start
    drag}
  @argument[formts]{a @class{gdk:conent-formats} instance with the targt formats
    that the drag will support}
  @argument[actions]{a @symbol{gdk:drag-action} bitmask of possible actions
    for a drag from this widget}
  @begin{short}
    Turns the tree view into a drag source for automatic DND.
  @end{short}
  Calling this method sets the @slot[gtk:tree-view]{reorderable} property to
  the @em{false} value.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please do
    not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gdk:content-formats}
  @see-symbol{gdk:modifier-type}
  @see-symbol{gdk:drag-action}
  @see-function{gtk:tree-view-reorderable}"
  (view (g:object tree-view))
  (mask gdk:modifier-type)
  (formats (g:boxed gdk:content-formats))
  (actions gdk:drag-action))

(export 'tree-view-enable-model-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_unset_rows_drag_source
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_unset_rows_drag_source"
               tree-view-unset-rows-drag-source) :void
 #+liber-documentation
 "@version{#2024-5-28}
  @argument[view]{a @class{gtk:tree-view} widget}
  @begin{short}
    Undoes the effect of the @fun{gtk:tree-view-enable-model-drag-source}
    function.
  @end{short}
  Calling this method sets the @slot[gtk:tree-view]{reorderable} property to
  @em{false}.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please do
    not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-function{gtk:tree-view-enable-model-drag-source}
  @see-function{gtk:tree-view-reorderable}"
  (view (g:object tree-view)))

(export 'tree-view-unset-rows-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_unset_rows_drag_dest
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_unset_rows_drag_dest"
               tree-view-unset-rows-drag-dest) :void
 #+liber-documentation
 "@version{#2024-5-28}
  @argument[view]{a @class{gtk:tree-view} widget}
  @begin{short}
    Undoes the effect of the @fun{gtk:tree-view-enable-model-drag-dest}
    function.
  @end{short}
  Calling this method sets the @slot[gtk:tree-view]{reorderable} property to
  @em{false}.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-function{gtk:tree-view-enable-model-drag-dest}
  @see-function{gtk:tree-view-reorderable}"
  (view (g:object tree-view)))

(export 'tree-view-unset-rows-drag-dest)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_drag_dest_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_set_drag_dest_row" tree-view-set-drag-dest-row)
    :void
 #+liber-documentation
 "@version{#2024-5-28}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance of the row to highlight,
    or @code{nil}}
  @argument[pos]{a @symbol{gtk:tree-view-drop-position} value wich specifies
    whether to drop before, after or into the row}
  @begin{short}
    Sets the row that is highlighted for feedback.
  @end{short}
  If @arg{path} is @code{nil}, an existing highlight is removed.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}
  @see-symbol{gtk:tree-view-drop-position}
  @see-function{gtk:tree-view-get-drag-dest-row}"
  (view (g:object tree-view))
  (path (g:boxed tree-path))
  (pos tree-view-drop-position))

(export 'tree-view-set-drag-dest-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_drag_dest_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_get_drag_dest_row" %tree-view-get-drag-dest-row)
    :void
  (view (g:object tree-view))
  (path :pointer)
  (pos :pointer))

(defun tree-view-get-drag-dest-row (view)
 #+liber-documentation
 "@version{#2024-5-28}
  @argument[view]{a @class{gtk:tree-view} widget}
  @begin{return}
    @arg{path} -- a @class{gtk:tree-path} instance of the highlighted row,
      or @code{nil} @br{}
    @arg{pos}  -- a @symbol{gtk:tree-view-drop-position} position, or
      @code{nil}
  @end{return}
  @begin{short}
    Gets information about the row that is highlighted for feedback.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}
  @see-symbol{gtk:tree-view-drop-position}
  @see-function{gtk:tree-view-set-drag-dest-row}"
  (cffi:with-foreign-objects ((path :pointer) (pos :pointer))
    (%tree-view-get-drag-dest-row view path pos)
    (values (cffi:mem-ref path '(g:boxed tree-path :return))
            (cffi:mem-ref pos 'tree-view-drop-position))))

(export 'tree-view-get-drag-dest-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_dest_row_at_pos
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_get_dest_row_at_pos"
               %tree-view-get-dest-row-at-pos) :boolean
  (view (g:object tree-view))
  (xdrag :int)
  (ydrag :int)
  (path :pointer)
  (pos :pointer))

(defun tree-view-get-dest-row-at-pos (view x y)
 #+liber-documentation
 "@version{#2024-5-28}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[x]{an integer with the position to determine the destination row
    for}
  @argument[y]{an integer with the position to determine the destination row
    for}
  @begin{return}
    @arg{path} -- a @class{gtk:tree-path} instance of the highlighted row,
      or @code{nil} @br{}
    @arg{pos}  -- a @symbol{gtk:tree-view-drop-position} position,
      or @code{nil}
  @end{return}
  @begin{short}
    Determines the destination row for a given position.
  @end{short}
  The arguments @arg{x} and @arg{y} are expected to be in widget coordinates.
  This function is only meaningful if the tree view is realized. Therefore this
  function will always return @code{nil} if @arg{tree-view} is not realized or
  does not have a model.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}
  @see-symbol{gtk:tree-view-drop-position}"
  (cffi:with-foreign-objects ((path :pointer) (pos :int))
    (when (%tree-view-get-dest-row-at-pos view x y path pos)
      (values (cffi:mem-ref path '(g:boxed tree-path :return))
              (cffi:mem-ref pos 'tree-view-drop-position)))))

(export 'tree-view-get-dest-row-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_create_row_drag_icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_create_row_drag_icon"
               tree-view-create-row-drag-icon)
    (:pointer (:struct cairo:surface-t))
 #+liber-documentation
 "@version{#2025-3-30}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance}
  @begin{return}
    The newly allocated @symbol{cairo:surface-t} instance of the drag icon.
  @end{return}
  @begin{short}
    Creates a @symbol{cairo:surface-t} instance for the representation of the
    row at path.
  @end{short}
  This image is used for a drag icon.
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}
  @see-symbol{cairo:surface-t}"
  (view (g:object tree-view))
  (path (g:boxed tree-path)))

(export 'tree-view-create-row-drag-icon)

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewSearchEqualFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback tree-view-search-equal-func :boolean
  ((model (g:object tree-model))
   (column :int)
   (key :string)
   (iter (g:boxed tree-iter))
   (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func model column key iter)
      (return-true () :report "Return T" t)
      (return-false () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-view-search-equal-func)
      "Callback"
      (liber:symbol-documentation 'tree-view-search-equal-func)
 "@version{#2024-5-4}
  @syntax{lambda (model column key iter data) => result}
  @argument[model]{a @class{gtk:tree-model} object being searched}
  @argument[column]{an integer with the search column set by the
    @fun{gtk:tree-view-search-column} function}
  @argument[key]{a key string to compare with}
  @argument[iter]{a @class{gtk:tree-iter} iterator pointing the row of
    @arg{model} that should be compared with @arg{key}}
  @argument[result]{@em{false}, if the row matches, @em{true} otherwise}
  @begin{short}
    A callback function used for checking whether a row in model matches a
    search key string entered by the user.
  @end{short}
  Note the return value is reversed from what you would normally expect.
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}
  @see-function{gtk:tree-view-set-search-equal-func}")

(export 'tree-view-search-equal-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_search_equal_func ()
;;;
;;; GtkTreeViewSearchEqualFunc gtk_tree_view_get_search_equal_func
;;;                                                    (GtkTreeView *tree_view);
;;;
;;; Returns the compare function currently in use.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     the currently used compare function for the search code.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_search_equal_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_set_search_equal_func"
               %tree-view-set-search-equal-func) :void
  (view (g:object tree-view))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun tree-view-set-search-equal-func (view func)
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[func]{a @symbol{gtk:tree-view-search-equal-func} callback function
    to use during the search}
  @begin{short}
    Sets the compare callback function for the interactive search capabilities.
  @end{short}
  Note that for equality the callback function returns @em{false} on matches.
  @see-class{gtk:tree-view}
  @see-symbol{gtk:tree-view-search-equal-func}"
  (%tree-view-set-search-equal-func
          view
          (cffi:callback tree-view-search-equal-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'tree-view-set-search-equal-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_search_entry
;;; gtk_tree_view_set_search_entry
;;; ----------------------------------------------------------------------------

(defun (setf tree-view-search-entry) (value view)
  (cffi:foreign-funcall "gtk_tree_view_set_search_entry"
                        (g:object tree-view) view
                        (g:object entry) value
                        :void)
  value)

(cffi:defcfun ("gtk_tree_view_get_search_entry" tree-view-search-entry)
    (g:object entry)
 #+liber-documentation
 "@version{#2024-3-10}
  @syntax{(gtk:tree-view-search-entry view) => entry}
  @syntax{(setf (gtk:tree-view-search-entry view) entry)}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[entry]{a @class{gtk:entry} widget the interactive search code of
    the tree view should use or @code{nil}}
  @begin{short}
    Accessor of the @class{gtk:entry} widget which is currently in use as
    interactive search entry for the tree view.
  @end{short}
  The @fun{gtk:tree-view-search-entry} function returns the search entry which
  is currently in use as interactive search entry for the tree view. In case
  the built-in entry is being used, @code{nil} will be returned. The
  @setf{gtk:tree-view-search-entry} function sets the search entry.

  This is useful when you want to provide a search entry in your interface at
  all time at a fixed position. Passing @code{nil} for @arg{entry} will make
  the interactive search code use the built-in popup entry again.
  @see-class{gtk:tree-view}
  @see-class{gtk:entry}"
  (view (g:object tree-view)))

(export 'tree-view-search-entry)

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewRowSeparatorFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback tree-view-row-separator-func :boolean
    ((model (g:object tree-model))
     (iter (g:boxed tree-iter))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func model iter)
      (return-true () :report "Return T" t)
      (return-false () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-view-row-separator-func)
      "Callback"
      (liber:symbol-documentation 'tree-view-row-separator-func)
 "@version{2024-4-7}
  @syntax{lambda (model iter) => result}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[iter]{a @class{gtk:tree-iter} instance pointing at a row in
    @arg{model}}
  @argument[result]{@em{true} if the row is a separator}
  @begin{short}
    Callback function type for determining whether the row pointed to by
    @arg{iter} should be rendered as a separator.
  @end{short}
  A common way to implement this is to have a boolean column in the model,
  whose values the callback function returns.
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}")

(export 'tree-view-row-separator-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_row_separator_func                    not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_row_separator_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_set_row_separator_func"
               %tree-view-set-row-separator-func) :void
  (view (g:object tree-view))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun tree-view-set-row-separator-func (view func)
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[func]{a @symbol{gtk:tree-view-row-separator-func} callback function}
  @begin{short}
    Sets the row separator function, which is used to determine whether a row
    should be drawn as a separator.
  @end{short}
  If the row separator function is @code{nil}, no separators are drawn. This is
  the default value.
  @see-class{gtk:tree-view}
  @see-symbol{gtk:tree-view-row-separator-func}"
  (if func
      (%tree-view-set-row-separator-func
              view
              (cffi:callback tree-view-row-separator-func)
              (glib:allocate-stable-pointer func)
              (cffi:callback glib:stable-pointer-destroy-notify))
      (%tree-view-set-row-separator-func
              view
              (cffi:null-pointer)
              (cffi:null-pointer)
              (cffi:null-pointer))))

(export 'tree-view-set-row-separator-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_is_rubber_banding_active
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_is_rubber_banding_active"
               tree-view-is-rubber-banding-active) :boolean
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @begin{return}
    @em{True} if a rubber banding operation is currently being done in
    the tree view.
  @end{return}
  @begin{short}
    Returns whether a rubber banding operation is currently being done in
    the tree view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}"
  (view (g:object tree-view)))

(export 'tree-view-is-rubber-banding-active)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_grid_lines
;;; gtk_tree_view_set_grid_lines
;;; ----------------------------------------------------------------------------

(defun (setf tree-view-grid-lines) (value view)
  (cffi:foreign-funcall "gtk_tree_view_set_grid_lines"
                   (g:object tree-view) view
                   tree-view-grid-lines value
                   :void)
  value)

(cffi:defcfun ("gtk_tree_view_get_grid_lines" tree-view-grid-lines)
    tree-view-grid-lines
 #+liber-documentation
 "@version{#2024-3-10}
  @syntax{(gtk:tree-view-grid-lines view) => setting}
  @syntax{(setf (gtk:tree-view-grid-lines view) setting)}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[setting]{a @symbol{gtk:tree-view-grid-lines} value indicating
    which grid lines to enable}
  @begin{short}
    The @fun{gtk:tree-view-grid-lines} function returns which grid lines are
    enabled in the tree view.
  @end{short}
  The @setf{gtk:tree-view-grid-lines} function sets which grid lines to draw in
  the tree view.
  @see-class{gtk:tree-view}
  @see-symbol{gtk:tree-view-grid-lines}"
  (view (g:object tree-view)))

(export 'tree-view-grid-lines)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_tooltip_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_set_tooltip_row" tree-view-set-tooltip-row) :void
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[path]{a @class{gtk:tree-path} instance}
  @begin{short}
    Sets the tip area of @arg{tooltip} to be the area covered by the row at
    @arg{path}.
  @end{short}
  See also the @fun{gtk:tree-view-tooltip-column} function for a simpler
  alternative. See also the @fun{gtk:tooltip-set-tip-area} function.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view}
  @see-function{gtk:tree-view-tooltip-column}
  @see-function{gtk:tooltip-set-tip-area}"
  (view (g:object tree-view))
  (tooltip (g:object tooltip))
  (path (g:boxed tree-path)))

(export 'tree-view-set-tooltip-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_tooltip_cell
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_set_tooltip_cell" tree-view-set-tooltip-cell)
    :void
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[path]{a @class{gtk:tree-path} instance or @code{nil}}
  @argument[column]{a @class{gtk:tree-view-column} object or @code{nil}}
  @argument[renderer]{a @class{gtk:cell-renderer} or @code{nil}}
  @begin{short}
    Sets the tip area of @arg{tooltip} to the area @arg{path}, @arg{column} and
    @arg{renderer} have in common.
  @end{short}
  For example if the @arg{path} argument is @code{nil} and @arg{column} is set,
  the tip area will be set to the full area covered by @arg{column}. See also
  the @fun{gtk:tooltip-set-tip-area} function.

  Note that if the @arg{path} argument is not specified and @arg{renderer} is
  set and part of a @arg{column} containing the expander, the @arg{tooltip}
  might not show and hide at the correct position. In such cases @arg{path}
  must be set to the current node under the mouse cursor for this function to
  operate correctly.

  See also the @fun{gtk:tree-view-tooltip-column} function for a simpler
  alternative.
  @see-class{gtk:tree-view}
  @see-function{gtk:tooltip-set-tip-area}"
  (view (g:object tree-view))
  (tooltip (g:object tooltip))
  (path (g:boxed tree-path))
  (column (g:object tree-view-column))
  (renderer (g:object cell-renderer)))

(export 'tree-view-set-tooltip-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_tooltip_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_get_tooltip_context" %tree-view-tooltip-context)
    :boolean
  (view (g:object tree-view))
  (x (:pointer :int))
  (y (:pointer :int))
  (tip :boolean)
  (model :pointer)
  (path :pointer)
  (iter :pointer))

(defun tree-view-tooltip-context (view)
 #+liber-documentation
 "@version{#2024-3-10}
  @argument[view]{a @class{gtk:tree-view} widget}
  @begin{return}
    @arg{x} -- an integer with the x coordinate (relative to widget
      coordinates) @br{}
    @arg{y} -- an integer with the y coordinate (relative to widget
      coordinates) @br{}
    @arg{tip} -- a boolean whether this is a keyboard tooltip or not @br{}
    @arg{model} -- a @class{gtk:tree-model} object or @code{nil} @br{}
    @arg{path}  -- a @class{gtk:tree-path} instance or @code{nil} @br{}
    @arg{iter}  -- a @class{gtk:tree-iter} iterator or @code{nil}
  @end{return}
  @begin{short}
    This function is supposed to be used in a @code{\"query-tooltip\"} signal
    handler for @class{gtk:tree-view} widgets.
  @end{short}
  The @arg{x}, @arg{y} and @arg{tip} values which are received in the signal
  handler, should be passed to this function without modification.

  The return value indicates whether there is a tree view row at the given
  coordinates (@em{true}) or not (@em{false}) for mouse tooltips. For keyboard
  tooltips the row returned will be the cursor row. When @em{true}, then any of
  @arg{model}, @arg{path} and @arg{iter} which have been provided will be set to
  point to that row and the corresponding model. @arg{x} and @arg{y} will always
  be converted to be relative to @arg{view}'s \"bin window\" if @arg{tip}
  is @em{false}.
  @see-class{gtk:tree-view}"
  (cffi:with-foreign-objects ((x :int)
                              (y :int)
                              (tip :boolean)
                              (model :pointer)
                              (path :pointer)
                              (iter :pointer))
    (when (%tree-view-tooltip-context view
                                      x
                                      y
                                      tip
                                      model
                                      path
                                      iter)
      (values (cffi:mem-ref x :int)
              (cffi:mem-ref y :int)
              (cffi:mem-ref tip :boolean)
              (cffi:mem-ref model 'g:object)
              (cffi:mem-ref path '(g:boxed tree-path :return))
              (cffi:mem-ref iter '(g:boxed tree-iter :return))))))

(export 'tree-view-tooltip-context)

;;; --- End of file gtk4.tree-view.lisp ----------------------------------------
