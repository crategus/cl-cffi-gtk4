;;; ----------------------------------------------------------------------------
;;; gtk4.icon-view.lisp
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
;;; GtkIconView
;;;
;;;     A widget that displays a list of icons in a grid
;;;
;;; Types and Values
;;;
;;;     GtkIconViewDropPosition
;;;     GtkIconView
;;;
;;; Accessors
;;;
;;;     gtk_icon_view_set_activate_on_single_click
;;;     gtk_icon_view_get_activate_on_single_click
;;;     gtk_icon_view_set_column_spacing
;;;     gtk_icon_view_get_column_spacing
;;;     gtk_icon_view_set_columns
;;;     gtk_icon_view_get_columns
;;;     gtk_icon_view_set_item_orientation
;;;     gtk_icon_view_get_item_orientation
;;;     gtk_icon_view_set_item_padding
;;;     gtk_icon_view_get_item_padding
;;;     gtk_icon_view_set_item_width
;;;     gtk_icon_view_get_item_width
;;;     gtk_icon_view_set_margin
;;;     gtk_icon_view_get_margin
;;;     gtk_icon_view_set_markup_column
;;;     gtk_icon_view_get_markup_column
;;;     gtk_icon_view_set_model
;;;     gtk_icon_view_get_model
;;;     gtk_icon_view_set_pixbuf_column
;;;     gtk_icon_view_get_pixbuf_column
;;;     gtk_icon_view_set_reorderable
;;;     gtk_icon_view_get_reorderable
;;;     gtk_icon_view_set_row_spacing
;;;     gtk_icon_view_get_row_spacing
;;;     gtk_icon_view_set_selection_mode
;;;     gtk_icon_view_get_selection_mode
;;;     gtk_icon_view_set_spacing
;;;     gtk_icon_view_get_spacing
;;;     gtk_icon_view_set_text_column
;;;     gtk_icon_view_get_text_column
;;;     gtk_icon_view_set_tooltip_column
;;;     gtk_icon_view_get_tooltip_column
;;;
;;; Functions
;;;
;;;     GtkIconViewForeachFunc
;;;
;;;     gtk_icon_view_new
;;;     gtk_icon_view_new_with_area
;;;     gtk_icon_view_new_with_model
;;;     gtk_icon_view_get_path_at_pos
;;;     gtk_icon_view_get_item_at_pos
;;;     gtk_icon_view_set_cursor
;;;     gtk_icon_view_get_cursor
;;;     gtk_icon_view_selected_foreach
;;;     gtk_icon_view_get_cell_rect
;;;     gtk_icon_view_select_path
;;;     gtk_icon_view_unselect_path
;;;     gtk_icon_view_path_is_selected
;;;     gtk_icon_view_get_selected_items
;;;     gtk_icon_view_select_all
;;;     gtk_icon_view_unselect_all
;;;     gtk_icon_view_item_activated
;;;     gtk_icon_view_scroll_to_path
;;;     gtk_icon_view_get_visible_range
;;;     gtk_icon_view_set_tooltip_item
;;;     gtk_icon_view_set_tooltip_cell
;;;     gtk_icon_view_get_tooltip_context
;;;     gtk_icon_view_get_item_row
;;;     gtk_icon_view_get_item_column
;;;     gtk_icon_view_enable_model_drag_source
;;;     gtk_icon_view_enable_model_drag_dest
;;;     gtk_icon_view_unset_model_drag_source
;;;     gtk_icon_view_unset_model_drag_dest
;;;     gtk_icon_view_set_drag_dest_item
;;;     gtk_icon_view_get_drag_dest_item
;;;     gtk_icon_view_get_dest_item_at_pos
;;;     gtk_icon_view_create_drag_icon
;;;
;;; Properties
;;;
;;;     activate-on-single-click
;;;     cell-area
;;;     column-spacing
;;;     columns
;;;     item-orientation
;;;     item-padding
;;;     item-width
;;;     margin
;;;     markup-column
;;;     model
;;;     pixbuf-column
;;;     reorderable
;;;     row-spacing
;;;     selection-mode
;;;     spacing
;;;     text-column
;;;     tooltip-column
;;;
;;; Signals
;;;
;;;     activate-cursor-item
;;;     item-activated
;;;     move-cursor
;;;     select-all
;;;     select-cursor-item
;;;     selection-changed
;;;     toggle-cursor-item
;;;     unselect-all
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkIconView
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkCellLayout
;;;     GtkScrollable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;; TODO: The API for drag and drop operations must be newly implemented.

;;; ----------------------------------------------------------------------------
;;; GtkIconViewDropPosition
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkIconViewDropPosition" icon-view-drop-position
  (:export t
   :type-initializer "gtk_icon_view_drop_position_get_type")
  (:no-drop 0)
  (:drop-into 1)
  (:drop-left 2)
  (:drop-right 3)
  (:drop-above 4)
  (:drop-below 5))

#+liber-documentation
(setf (liber:alias-for-symbol 'icon-view-drop-position)
      "GEnum"
      (liber:symbol-documentation 'icon-view-drop-position)
 "@version{2025-07-22}
  @begin{declaration}
(gobject:define-genum \"GtkIconViewDropPosition\" icon-view-drop-position
  (:export t
   :type-initializer \"gtk_icon_view_drop_position_get_type\")
  (:no-drop 0)
  (:drop-into 1)
  (:drop-left 2)
  (:drop-right 3)
  (:drop-above 4)
  (:drop-below 5))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:no-drop]{No drop possible.}
      @entry[:drop-into]{Dropped item replaces the item.}
      @entry[:drop-left]{Droppped item is inserted to the left.}
      @entry[:drop-right]{Dropped item is inserted to the right.}
      @entry[:drop-above]{Dropped item is inserted above.}
      @entry[:drop-below]{Dropped item is inserted below.}
    @end{simple-table}
  @end{values}
  @short{An enumeration for determining where a dropped item goes.}
  @see-class{gtk:icon-view}")

;;; ----------------------------------------------------------------------------
;;; GtkIconView
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkIconView" icon-view
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkCellLayout"
                "GtkScrollable")
   :type-initializer "gtk_icon_view_get_type")
  ((activate-on-single-click
    icon-view-activate-on-single-click
    "activate-on-single-click" "gboolean" t t)
   (cell-area
    icon-view-cell-area
    "cell-area" "GtkCellArea" t t)
   (column-spacing
    icon-view-column-spacing
    "column-spacing" "gint" t t)
   (columns
    icon-view-columns
    "columns" "gint" t t)
   (item-orientation
    icon-view-item-orientation
    "item-orientation" "GtkOrientation" t t)
   (item-padding
    icon-view-item-padding
    "item-padding" "gint" t t)
   (item-width
    icon-view-item-width
    "item-width" "gint" t t)
   (margin
    icon-view-margin
    "margin" "gint" t t)
   (markup-column
    icon-view-markup-column
    "markup-column" "gint" t t)
   (model
    icon-view-model
    "model" "GtkTreeModel" t t)
   (pixbuf-column
    icon-view-pixbuf-column
    "pixbuf-column" "gint" t t)
   (reorderable
    icon-view-reorderable
    "reorderable" "gboolean" t t)
   (row-spacing
    icon-view-row-spacing
    "row-spacing" "gint" t t)
   (selection-mode
    icon-view-selection-mode
    "selection-mode" "GtkSelectionMode" t t)
   (spacing
    icon-view-spacing
    "spacing" "gint" t t)
   (text-column
    icon-view-text-column
    "text-column" "gint" t t)
   (tooltip-column
    icon-view-tooltip-column
    "tooltip-column" "gint" t t)))

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj icon-view) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:ICON-VIEW is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'icon-view 'type)
 "@version{2025-07-22}
  @begin{short}
    The @class{gtk:icon-view} widget provides an alternative view on a
    @class{gtk:tree-model} object.
  @end{short}
  It displays the model as a grid of icons with labels. Like the
  @class{gtk:tree-view} widget, it allows to select one or multiple items,
  depending on the selection mode, see the @fun{gtk:icon-view-selection-mode}
  function. In addition to selection with the arrow keys, the
  @class{gtk:icon-view} class supports rubberband selection, which is controlled
  by dragging the pointer.

  @image[icon-view]{Figure: GtkIconView}

  Note that if the tree model is backed by an actual tree store, as opposed to
  a flat list where the mapping to icons is obvious, the @class{gtk:icon-view}
  widget will only display the first level of the tree and ignore the tree's
  branches.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:icon-view} implementation has a single CSS node with name
    @code{iconview} and @code{.view} style class. For rubberband selection, a
    subnode with name @code{rubberband} is used.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[icon-view::activate-cursor-item]{signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:icon-view} widget on which the signal is
          emitted.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user activates the
      currently focused item. Applications should not connect to it, but may
      emit it with the @fun{g:signal-emit} function if they need to control
      activation programmatically. The default bindings for this signal are the
      @kbd{Space}, @kbd{Return} and @kbd{Enter} keys.
    @end{signal}
    @begin[icon-view::item-activated]{signal}
      @begin{pre}
lambda (view path)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:icon-view} widget on which the signal is
          emitted.}
        @entry[path]{The @class{gtk:tree-path} instance for the activated item.}
      @end{simple-table}
      The signal is emitted when the @fun{gtk:icon-view-item-activated} function
      is called or the user double clicks an item. It is also emitted when a
      non-editable item is selected and one of the @kbd{Space}, @kbd{Return} or
      @kbd{Enter} keys is pressed.
    @end{signal}
    @begin[icon-view::move-cursor]{signal}
      @begin{pre}
lambda (view step count extent modify)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:icon-view} widget that received the
          signal.}
        @entry[step]{The granularity of the move as a value of the
          @sym{gtk:movement-step} enumeration.}
        @entry[count]{The integer for the number of step units to move.}
        @entry[extend]{The boolean whether to extend the selection.}
        @entry[modify]{The boolean whether to modify the selection.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted when the user
      initiates a cursor movement. Applications should not connect to it, but
      may emit it with the @fun{g:signal-emit} function if they need to control
      the cursor programmatically. The default bindings for this signal include
      @begin{itemize}
        @item{Arrow keys which move by individual steps.}
        @item{@kbd{Home}/@kbd{End} keys which move to the first/last item.}
        @item{@kbd{PageUp}/@kbd{PageDown} which move by \"pages\".}
      @end{itemize}
      All of these will extend the selection when combined with the @kbd{Shift}
      modifier.
    @end{signal}
    @begin[icon-view::select-all]{signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:icon-view} widget on which the signal is
          emitted.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user selects all items.
      Applications should not connect to it, but may emit it with the
      @fun{g:signal-emit} function if they need to control selection
      programmatically. The default binding for this signal is the @kbd{Ctrl-a}
      key.
    @end{signal}
    @begin[icon-view::select-cursor-item]{signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:icon-view} widget on which the signal is
          emitted.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user selects the item that
      is currently focused. Applications should not connect to it, but may emit
      it with the @fun{g:signal-emit} function if they need to control selection
      programmatically. There is no default binding for this signal.
    @end{signal}
    @begin[icon-view::selection-changed]{signal}
      @begin{pre}
lambda (view)    :run-first
     @end{pre}
     @begin[code]{simple-table}
       @entry[view]{The @class{gtk:icon-view} widget on which the signal is
         emitted.}
     @end{simple-table}
     The signal is emitted when the selection changes, that is the set of
     selected items.
    @end{signal}
    @begin[icon-view::toggle-cursor-item]{signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:icon-view} widget on which the signal is
          emitted.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user toggles whether the
      currently focused item is selected or not. The exact effect of this depend
      on the selection mode. Applications should not connect to it, but may emit
      it with the @fun{g:signal-emit} function if they need to control selection
      programmatically. The default binding for this signal is the
      @kbd{Ctrl-Space} key.
    @end{signal}
    @begin[icon-view::unselect-all]{signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:icon-view} widget on which the signal is
          emitted.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user unselects all items.
      Applications should not connect to it, but may emit it with the
      @fun{g:signal-emit} function if they need to control selection
      programmatically. The default binding for this signal is the
      @kbd{Ctrl-Shift-a} key.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:icon-view-new}
  @see-constructor{gtk:icon-view-new-with-area}
  @see-constructor{gtk:icon-view-new-with-model}
  @see-slot{gtk:icon-view-activate-on-single-click}
  @see-slot{gtk:icon-view-cell-area}
  @see-slot{gtk:icon-view-column-spacing}
  @see-slot{gtk:icon-view-columns}
  @see-slot{gtk:icon-view-item-orientation}
  @see-slot{gtk:icon-view-item-padding}
  @see-slot{gtk:icon-view-item-width}
  @see-slot{gtk:icon-view-margin}
  @see-slot{gtk:icon-view-markup-column}
  @see-slot{gtk:icon-view-model}
  @see-slot{gtk:icon-view-pixbuf-column}
  @see-slot{gtk:icon-view-reorderable}
  @see-slot{gtk:icon-view-row-spacing}
  @see-slot{gtk:icon-view-selection-mode}
  @see-slot{gtk:icon-view-spacing}
  @see-slot{gtk:icon-view-text-column}
  @see-slot{gtk:icon-view-tooltip-column}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-view}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:icon-view-activate-on-single-click ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "activate-on-single-click"
                                               'icon-view) t)
 "The @code{activate-on-single-click} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Specifies whether the @sig[gtk:icon-view]{item-activated} signal will be
  emitted after a single click. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-activate-on-single-click)
      "Accessor"
      (documentation 'icon-view-activate-on-single-click 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-activate-on-single-click object) => setting}
  @syntax{(setf (gtk:icon-view-activate-on-single-click object) setting)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[setting]{a boolean that is @em{true} to emit the
    @sig[gtk:icon-view]{item-activated} signal on a single click}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{activate-on-single-click} slot of
    the @class{gtk:icon-view} class gets or sets whether the
    @sig[gtk:icon-view]{item-activated} signal will be emitted after a single
    click.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:grid-view}")

;;; --- gtk:icon-view-cell-area ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cell-area" 'icon-view) t)
 "The @code{cell-area} property of type @class{gtk:cell-area}
  (Read / Write / Construct) @br{}
  The cell area used to layout cell renderers for this icon view. If no area is
  specified when creating the icon view a @class{gtk:cell-area-box} object will
  be used.")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-cell-area)
      "Accessor"
      (documentation 'icon-view-cell-area 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-cell-area object) => area}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[area]{a @class{gtk:cell-area} object used to layout cell renderers}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{cell-area} slot of the
    @class{gtk:icon-view} class gets the cell area used to layout cell renderers
    for this icon view.
  @end{short}
  If no area is specified when creating the icon view with the
  @fun{gtk:icon-view-new-with-area} function a @class{gtk:cell-area-box} object
  will be used.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-area-box}
  @see-class{gtk:grid-view}
  @see-function{gtk:icon-view-new-with-area}")

;;; --- gtk:icon-view-column-spacing -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "column-spacing" 'icon-view) t)
 "The @code{column-spacing} property of type @code{:int} (Read / Write) @br{}
  Specifies the space which is inserted between the columns of the icon view.
  @br{}
  Allowed values: >= 0 @br{}
  Default value: 6")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-column-spacing)
      "Accessor"
      (documentation 'icon-view-column-spacing 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-column-spacing object) => spacing}
  @syntax{(setf (gtk:icon-view-column-spacing object) spacing)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[spacing]{an integer for the column spacing}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{column-spacing} slot of the
    @class{gtk:icon-view} class gets or sets the space which is inserted between
    the columns of the icon view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:grid-view}")

;;; --- gtk:icon-view-columns --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "columns" 'icon-view) t)
 "The @code{columns} property of type @code{:int} (Read / Write) @br{}
  Contains the number of the columns in which the items should be displayed.
  If it is -1, the number of columns will be chosen automatically to fill the
  available area. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-columns)
      "Accessor"
      (documentation 'icon-view-columns 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-columns object) => columns}
  @syntax{(setf (gtk:icon-view-columns object) columns)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[columns]{an integer for the number of columns}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{columns} slot of the
    @class{gtk:icon-view} class gets or sets the number of the columns in which
    the items should be displayed.
  @end{short}
  If @arg{columns} is -1, the number of columns will be chosen automatically to
  fill the available area.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:grid-view}")

;;; --- gtk:icon-view-item-orientation -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "item-orientation" 'icon-view) t)
 "The @code{item-orientation} property of type @sym{gtk:orientation}
  (Read / Write) @br{}
  Specifies how the cells, that is the icon and the text, of the item are
  positioned relative to each other. @br{}
  Default value: @val[gtk:orientation]{:vertical}")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-item-orientation)
      "Accessor"
      (documentation 'icon-view-item-orientation 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-item-orientation object) => orientation}
  @syntax{(setf (gtk:icon-view-item-orientation object) orientation)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[orientation]{a @sym{gtk:orientation} value for the relative
    position of texts and icons}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{item-orientation} slot of the
    @class{gtk:icon-view} class gets or sets whether the labels are drawn beside
    the icons instead of below.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:grid-view}
  @see-symbol{gtk:orientation}")

;;; --- gtk:icon-view-item-padding ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "item-padding" 'icon-view) t)
 "The @code{item-padding} property of type @code{:int} (Read / Write) @br{}
  Specifies the padding around each item of the icon view. @br{}
  Allowed values: >= 0 @br{}
  Default value: 6")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-item-padding)
      "Accessor"
      (documentation 'icon-view-item-padding 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-item-padding object) => padding}
  @syntax{(setf (gtk:icon-view-item-padding object) padding)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[padding]{an integer for the item padding}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{item-padding} slot of the
    @class{gtk:icon-view} class gets or sets the padding around each of the icon
    view's items.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:grid-view}")

;;; --- gtk:icon-view-item-width -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "item-width" 'icon-view) t)
 "The @code{item-width} property of type @code{:int} (Read / Write) @br{}
  Specifies the width to use for each item. If it is set to -1, the icon view
  will automatically determine a suitable item size. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-item-width)
      "Accessor"
      (documentation 'icon-view-item-width 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-item-width object) => width}
  @syntax{(setf (gtk:icon-view-item-width object) width)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[width]{an integer for the width for each item}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{item-width} slot of the
    @class{gtk:icon-view} class gets or sets the width to use for each item.
  @end{short}
  If it is set to -1, the icon view will automatically determine a suitable
  item size.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:grid-view}")

;;; --- gtk:icon-view-margin ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "margin" 'icon-view) t)
 "The @code{margin} property of type @code{:int} (Read / Write) @br{}
  Specifies the space which is inserted at the edges of the icon view. @br{}
  Allowed values: >= 0 @br{}
  Default value: 6")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-margin)
      "Accessor"
      (documentation 'icon-view-margin 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-margin object) => margin}
  @syntax{(setf (gtk:icon-view-margin object) margin)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[margin]{an integer for the margin}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{margin} slot of the
    @class{gtk:icon-view} class gets or sets the space which is inserted at the
    top, bottom, left and right of the icon view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:grid-view}")

;;; --- gtk:icon-view-markup-column --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "markup-column" 'icon-view) t)
 "The @code{markup-column} property of type @code{:int} (Read / Write) @br{}
  Contains the number of the model column containing markup information to be
  displayed. The markup column must be of @code{\"gchararray\"} type. If this
  property and the @slot[gtk:icon-view]{text-column} property are both set to
  column numbers, it overrides the text column. If both are set to -1, no texts
  are displayed. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-markup-column)
      "Accessor"
      (documentation 'icon-view-markup-column 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-markup-column object) => column}
  @syntax{(setf (gtk:icon-view-markup-column object) column)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[column]{an integer for a column in the currently used model, or -1
    to display no text}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{markup-column} slot of the
    @class{gtk:icon-view} class gets or sets the column with markup text for
    the icon view.
  @end{short}

  The markup column must be of @code{\"gchararray\"} type. If the markup column
  is set to something, it overrides the text column set by the
  @fun{gtk:icon-view-text-column} function.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:grid-view}
  @see-function{gtk:icon-view-text-column}")

;;; --- gtk:icon-view-model ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'icon-view) t)
 "The @code{model} property of type @class{gtk:tree-model} (Read / Write) @br{}
  The model for the icon view.")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-model)
      "Accessor"
      (documentation 'icon-view-model 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-model object) => model}
  @syntax{(setf (gtk:icon-view-model object) model)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[model]{a @class{gtk:tree-model} object}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{model} slot of the
    @class{gtk:icon-view} class gets or sets the model for the icon view.
  @end{short}
  Returns @code{nil} if the model is unset.

  If the icon view already has a model set, it will remove it before setting
  the new model. If @arg{model} is @code{nil}, then it will unset the old model.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-model}
  @see-class{gtk:grid-view}")

;;; --- gtk:icon-view-pixbuf-column --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixbuf-column" 'icon-view) t)
 "The @code{pixbuf-column} property of type @code{:int} (Read / Write) @br{}
  Contains the number of the model column containing the pixbufs which are
  displayed. The pixbuf column must be of @code{\"GdkPixbuf\"} type. Setting
  this property to -1 turns off the display of pixbufs. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-pixbuf-column)
      "Accessor"
      (documentation 'icon-view-pixbuf-column 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-pixbuf-column object) => column}
  @syntax{(setf (gtk:icon-view-pixbuf-column object) column)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[column]{an integer for a column in the currently used model, or
    -1 to disable}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{pixbuf-column} slot of the
    @class{gtk:icon-view} class gets or sets the number of the model column
    containing the pixbufs which are displayed.
  @end{short}
  The pixbuf column must be of @code{\"GdkPixbuf\"} type.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gdk-pixbuf:pixbuf}
  @see-class{gtk:grid-view}")

;;; --- gtk:icon-view-reorderable ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "reorderable" 'icon-view) t)
 "The @code{reorderable} property of type @code{:boolean} (Read / Write) @br{}
  The reorderable property specifies if the items can be reordered by drag
  and drop. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-reorderable)
      "Accessor"
      (documentation 'icon-view-reorderable 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-reorderable object) => reorderable}
  @syntax{(setf (gtk:icon-view-reorderable object) reorderable)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[reorderable]{@em{true}, if the list of items can be reordered}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{reorderable} slot of the
    @class{gtk:icon-view} class gets or sets whether the user can reorder the
    list via drag and drop.
  @end{short}
  This function is a convenience function to allow you to reorder models that
  support the @class{gtk:tree-drag-source} and the @class{gtk:tree-drag-dest}
  interfaces.

  Both @class{gtk:tree-store} and @class{gtk:list-store} objects support these.
  If @arg{reorderable} is @em{true}, then the user can reorder the model by
  dragging and dropping rows. The developer can listen to these changes by
  connecting to the model's @sig[gtk:tree-model]{row-inserted} and
  @sig[gtk:tree-model]{row-deleted} signals. The reordering is implemented by
  setting up the icon view as a drag source and destination. Therefore, drag
  and drop can not be used in a reorderable view for any other purpose.

  This function does not give you any degree of control over the order - any
  reordering is allowed. If more control is needed, you should probably handle
  drag and drop manually.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-drag-source}
  @see-class{gtk:tree-drag-dest}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-store}
  @see-class{gtk:grid-view}")

;;; --- gtk:icon-view-row-spacing ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "row-spacing" 'icon-view) t)
 "The @code{row-spacing} property of type @code{:int} (Read / Write) @br{}
  Specifies the space which is inserted between the rows of the icon view. @br{}
  Allowed values: >= 0 @br{}
  Default value: 6")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-row-spacing)
      "Accessor"
      (documentation 'icon-view-row-spacing 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-row-spacing object) => spacing}
  @syntax{(setf (gtk:icon-view-row-spacing object) spacing)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[spacing]{an integer for the row spacing}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{row-spacing} slot of the
    @class{gtk:icon-view} class gets or sets the space which is inserted between
    the rows of the icon view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:grid-view}")

;;; --- gtk:icon-view-selection-mode -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selection-mode" 'icon-view) t)
 "The @code{selection-mode} property of type @sym{gtk:selection-mode}
  (Read / Write) @br{}
  Specifies the selection mode of the icon view. If the mode is
  @val[gtk:selection-mode]{:multiple}, rubberband selection is enabled, for the
  other modes, only keyboard selection is possible. @br{}
  Default value: @val[gtk:selection-mode]{:single}")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-selection-mode)
      "Accessor"
      (documentation 'icon-view-selection-mode 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-selection-mode object) => mode}
  @syntax{(setf (gtk:icon-view-selection-mode object) mode)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[mode]{a @sym{gtk:selection-mode} value}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{selection-mode} slot of the
    @class{gtk:icon-view} class gets or sets the selection mode of the icon
    view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:grid-view}
  @see-symbol{gtk:selection-mode}")

;;; --- gtk:icon-view-spacing --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "spacing" 'icon-view) t)
 "The @code{spacing} property of type @code{:int} (Read / Write) @br{}
  Specifies the space which is inserted between the cells, that is the icon and
  the text, of an item. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-spacing)
      "Accessor"
      (documentation 'icon-view-spacing 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-spacing object) => spacing}
  @syntax{(setf (gtk:icon-view-spacing object) spacing)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[spacing]{an integer for the spacing}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{spacing} slot of the
    @class{gtk:icon-view} class gets or sets the space which is inserted
    between the cells, that is the icon and the text, of an item.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:grid-view}")

;;; --- gtk:icon-view-text-column ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text-column" 'icon-view) t)
 "The @code{text-column} property of type @code{:int} (Read / Write) @br{}
  Contains the number of the model column containing the texts which are
  displayed. The text column must be of @code{\"gchararray\"} type. If this
  property and the @slot[gtk:icon-view]{markup-column} property are both set to
  -1, no texts are displayed. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1 ")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-text-column)
      "Accessor"
      (documentation 'icon-view-text-column 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-text-column object) => column}
  @syntax{(setf (gtk:icon-view-text-column object) column)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[column]{an integer for a column in the currently used model, or
    -1 to display no text}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{text-column} slot of the
    @class{gtk:icon-view} class gets or sets the number of the model column
    containing the texts which are displayed.
  @end{short}
  The text column must be of @code{\"gchararray\"} type.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:grid-view}")

;;; --- gtk:icon-view-tooltip-column -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tooltip-column" 'icon-view) t)
 "The @code{tooltip-column} property of type @code{:int} (Read / Write) @br{}
  The column in the model containing the tooltip texts for the items. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'icon-view-tooltip-column)
      "Accessor"
      (documentation 'icon-view-tooltip-column 'function)
 "@version{2025-09-16}
  @syntax{(gtk:icon-view-tooltip-column object) => column}
  @syntax{(setf (gtk:icon-view-tooltip-column object) column)}
  @argument[object]{a @class{gtk:icon-view} widget}
  @argument[column]{an integer that is a valid column number}
  @begin{short}
    The accessor for the @slot[gtk:icon-view]{tooltip-column} slot of the
    @class{gtk:icon-view} class gets or sets the column in the model containing
    the tooltip texts for the items.
  @end{short}

  If you only plan to have simple (text-only) tooltips on full items, you can
  use this function to have the @class{gtk:icon-view} widget handle these
  automatically for you. The @arg{column} argument should be set to the column
  in the icon view's model containing the tooltip texts, or -1 to disable this
  feature.

  When enabled, the @slot[gtk:widget]{has-tooltip} property will be set to
  @em{true} and the icon view will connect a @sig[gtk:widget]{query-tooltip}
  signal handler. Note that the signal handler sets the text with the
  @fun{gtk:tooltip-set-markup} function, so &, <, and so on, have to be escaped
  in the text.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:grid-view}
  @see-function{gtk:tooltip-set-markup}
  @see-function{gtk:widget-has-tooltip}")

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_new
;;; ----------------------------------------------------------------------------

(declaim (inline icon-view-new))

(defun icon-view-new ()
 #+liber-documentation
 "@version{#2025-03-03}
  @return{The newly created @class{gtk:icon-view} widget.}
  @begin{short}
    Creates a new icon view.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-function{gtk:icon-view-new-with-area}
  @see-function{gtk:icon-view-new-with-model}"
  (make-instance 'icon-view))

(export 'icon-view-new)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_new_with_area
;;; ----------------------------------------------------------------------------

(declaim (inline icon-view-new-with-area))

(defun icon-view-new-with-area (area)
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[area]{a @class{gtk:cell-area} object to use to layout cells}
  @return{The newly created @class{gtk:icon-view} widget.}
  @begin{short}
    Creates a new icon view using the specified area to layout cells inside
    the icons.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:cell-area}
  @see-function{gtk:icon-view-new}
  @see-function{gtk:icon-view-new-with-model}"
  (make-instance 'icon-view
                 :cell-area area))

(export 'icon-view-new-with-area)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_new_with_model
;;; ----------------------------------------------------------------------------

(declaim (inline icon-view-new-with-model))

(defun icon-view-new-with-model (model)
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[model]{a @class{gtk:tree-model} object}
  @return{The newly created @class{gtk:icon-view} widget.}
  @begin{short}
    Creates a new icon view with the model @arg{model}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-model}
  @see-function{gtk:icon-view-new}
  @see-function{gtk:icon-view-new-with-area}"
  (make-instance 'icon-view
                 :model model))

(export 'icon-view-new-with-model)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_path_at_pos
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_get_path_at_pos" icon-view-path-at-pos)
    (g:boxed tree-path :return)
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[x]{an integer for the x position to be identified}
  @argument[y]{an integer for the y position to be identified}
  @begin{return}
    The @class{gtk:tree-path} instance corresponding to the icon or @code{nil}
    if no icon exists at that position.
  @end{return}
  @begin{short}
    Gets the path for the icon at the given position.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-path}"
  (view (g:object icon-view))
  (x :int)
  (y :int))

(export 'icon-view-path-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_item_at_pos
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_get_item_at_pos" %icon-view-item-at-pos) :boolean
  (view (g:object icon-view))
  (x :int)
  (y :int)
  (path :pointer)
  (cell :pointer))

(defun icon-view-item-at-pos (view x y)
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[x]{an integer for the x position to be identified}
  @argument[y]{an integer for the y position to be identified}
  @begin{return}
    @arg{path} -- a @class{gtk:tree-path} instance, or @code{nil} @br{}
    @arg{cell} -- a @class{gtk:cell-renderer} object responsible for the
    cell at (x,y), or @code{nil} if no item exists at the specified position
  @end{return}
  @begin{short}
    Gets the path and cell for the icon at the given position.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}"
  (cffi:with-foreign-objects ((path :pointer) (cell :pointer))
    (when (%icon-view-item-at-pos view x y path cell)
      (values (cffi:mem-ref path '(g:boxed tree-path :return))
              (cffi:mem-ref cell 'g:object)))))

(export 'icon-view-item-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_cursor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_set_cursor" icon-view-set-cursor) :void
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance}
  @argument[cell]{one of the @class{gtk:cell-renderer} objects of
    @arg{view}, or @code{nil}}
  @argument[start]{@em{true} if the specified cell should start being edited}
  @begin{short}
    Sets the current keyboard focus to be at @arg{path}, and selects it.
  @end{short}
  This is useful when you want to focus the user's attention on a particular
  item. If @arg{cell} is not @code{nil}, then focus is given to the cell
  specified by it. Additionally, if @arg{start} is @em{true}, then editing
  should be started in the specified cell.

  This function is often followed by a call of the @fun{gtk:widget-grab-focus}
  function in order to give keyboard focus to the widget. Please note that
  editing can only happen when the widget is realized.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-path}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:icon-view-get-cursor}
  @see-function{gtk:widget-grab-focus}"
  (view (g:object icon-view))
  (path (g:boxed tree-path))
  (cell (g:object cell-renderer))
  (start :boolean))

(export 'icon-view-set-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_cursor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_get_cursor" %icon-view-get-cursor) :boolean
  (view (g:object icon-view))
  (path :pointer)
  (cell :pointer))

(defun icon-view-get-cursor (view)
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[view]{a @class{gtk:icon-view} widget}
  @begin{return}
    @arg{path} -- a current @class{gtk:tree-path} cursor path, or @code{nil}
    @br{}
    @arg{cell} -- a current @class{gtk:cell-renderer} focus cell,
    or @code{nil} if the cursor is not set
  @end{return}
  @begin{short}
    Returns the current cursor path and focus cell.
  @end{short}
  If the cursor is not currently set, then @arg{path} will be @code{nil}. If no
  cell currently has focus, then @arg{cell} will be @code{nil}.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-path}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:icon-view-set-cursor}"
  (cffi:with-foreign-objects ((path :pointer) (cell :pointer))
    (when (%icon-view-get-cursor view path cell)
      (values (cffi:mem-ref path '(g:boxed tree-path :return))
              (cffi:mem-ref cell 'g:object)))))

(export 'icon-view-get-cursor)

;;; ----------------------------------------------------------------------------
;;; GtkIconViewForeachFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback icon-view-foreach-func :void
    ((view (g:object icon-view))
     (path (g:boxed tree-path))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func view path)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'icon-view-foreach-func)
      "Callback"
      (liber:symbol-documentation 'icon-view-foreach-func)
 "@version{#2025-07-22}
  @syntax{lambda (view path)}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance for a selected row}
  @begin{short}
    A callback function used by the @fun{gtk:icon-view-selected-foreach}
    function to map all selected rows.
  @end{short}
  It will be called on every selected row in the view.
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-path}
  @see-function{gtk:icon-view-selected-foreach}")

(export 'icon-view-foreach-func)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_selected_foreach
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_selected_foreach" %icon-view-selected-foreach)
    :void
  (view (g:object icon-view))
  (func :pointer)
  (data :pointer))

(defun icon-view-selected-foreach (view func)
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[func]{a @sym{gtk:icon-view-foreach-func} callback function to call
    for each selected icon}
  @begin{short}
    Calls a function for each selected icon.
  @end{short}
  Note that the model or selection cannot be modified from within this function.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-symbol{gtk:icon-view-foreach-func}"
  (glib:with-stable-pointer (ptr func)
    (%icon-view-selected-foreach view
                                 (cffi:callback icon-view-foreach-func)
                                 ptr)))

(export 'icon-view-selected-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_cell_rect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_get_cell_rect" %icon-view-cell-rect) :boolean
  (view (g:object icon-view))
  (path (g:boxed tree-path))
  (cell (g:object cell-renderer))
  (rect (g:boxed gdk:rectangle)))

(defun icon-view-cell-rect (view path cell)
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance}
  @argument[cell]{a @class{gtk:cell-renderer} object or @code{nil}}
  @begin{return}
    The @class{gdk:rectangle} instance for the cell rectangle or @code{nil}.
  @end{return}
  @begin{short}
    Returns the bounding rectangle in widget coordinates for the cell specified
    by @arg{path} and @arg{cell}.
  @end{short}
  If @arg{cell} is @code{nil} the main cell area is used. This function is only
  valid if @arg{view} is realized.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-path}
  @see-class{gtk:cell-renderer}
  @see-class{gdk:rectangle}"
  (let ((rect (gdk:rectangle-new)))
    (when (%icon-view-cell-rect view path cell rect)
      rect)))

(export 'icon-view-cell-rect)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_select_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_select_path" icon-view-select-path) :void
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance to be selected}
  @short{Selects the row at @arg{path}.}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-path}"
  (view (g:object icon-view))
  (path (g:boxed tree-path)))

(export 'icon-view-select-path)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_unselect_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_unselect_path" icon-view-unselect-path) :void
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance to be unselected}
  @short{Unselects the row at @arg{path}.}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-path}"
  (view (g:object icon-view))
  (path (g:boxed tree-path)))

(export 'icon-view-unselect-path)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_path_is_selected
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_path_is_selected" icon-view-path-is-selected)
    :boolean
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance to check selection on}
  @return{@em{True} if @arg{path} is selected.}
  @begin{short}
    Returns @em{true} if the icon pointed to by @arg{path} is currently
    selected.
  @end{short}
  If @arg{path} does not point to a valid location, @code{nil} is returned.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-path}"
  (view (g:object icon-view))
  (path (g:boxed tree-path)))

(export 'icon-view-path-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_selected_items
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_get_selected_items" icon-view-selected-items)
    (g:list-t (g:boxed tree-path :return))
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[view]{a @class{gtk:icon-view} widget}
  @begin{return}
    The list containing a @class{gtk:tree-path} instance for each selected row.
  @end{return}
  @begin{short}
    Creates a list of paths of all selected items.
  @end{short}
  Additionally, if you are planning on modifying the model after calling this
  function, you may want to convert the returned list into a list of
  @class{gtk:tree-row-reference} instances. To do this, you can use the
  @fun{gtk:tree-row-reference-new} function.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-row-reference}
  @see-function{gtk:tree-row-reference-new}"
  (view (g:object icon-view)))

(export 'icon-view-selected-items)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_select_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_select_all" icon-view-select-all) :void
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[view]{a @class{gtk:icon-view} widget}
  @begin{short}
    Selects all the icons.
  @end{short}
  The icon view must have its selection mode set to the
  @val[gtk:selection-mode]{:multiple} value of the @sym{gtk:selection-mode}
  enumeration.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-symbol{gtk:selection-mode}"
  (view (g:object icon-view)))

(export 'icon-view-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_unselect_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_unselect_all" icon-view-unselect-all) :void
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[view]{a @class{gtk:icon-view} widget}
  @short{Unselects all the icons.}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}"
  (view (g:object icon-view)))

(export 'icon-view-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_item_activated
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_item_activated" icon-view-item-activated) :void
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance to be activated}
  @begin{short}
    Activates the item determined by @arg{path}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-path}"
  (view (g:object icon-view))
  (path (g:boxed tree-path)))

(export 'icon-view-item-activated)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_scroll_to_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_scroll_to_path" %icon-view-scroll-to-path) :void
  (view (g:object icon-view))
  (path (g:boxed tree-path))
  (use-align :boolean)
  (row-align :float)
  (col-align :float))

(defun icon-view-scroll-to-path (view path
                                 &key (row-align 0.5 row-align-p)
                                      (col-align 0.5 col-align-p))
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance for the item to move to}
  @argument[row-align]{a number coerced to a single float for the vertical
    alignment of the item specified by path}
  @argument[col-align]{a number coerced to a single float for the horizontal
    alignment of the item specified by path}
  @begin{short}
    Moves the alignments of @arg{view} to the position specified by @arg{path}.
  @end{short}
  The @arg{row-align} argument determines where the row is placed, and the
  @arg{col-align} argument determines where the column is placed. Both are
  expected to be between 0.0 and 1.0. The 0.0 value means left/top alignment,
  the 1.0 value means right/bottom alignment, the 0.5 value means center. The
  keyword arguments have the default 0.5 value.

  If both @arg{row-align} and @arg{col-align} arguments are @code{nil}, then
  the alignment arguments are ignored, and the tree does the minimum amount of
  work to scroll the item onto the screen. This means that the item will be
  scrolled to the edge closest to its current position. If the item is currently
  visible on the screen, nothing is done.

  This function only works if the model is set, and @arg{path} is a valid row
  on the model. If the model changes before the @arg{view} is realized, the
  centered @arg{path} will be modified to reflect this change.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-path}"
  (%icon-view-scroll-to-path view
                             path
                             (or row-align-p col-align-p)
                             (coerce row-align 'single-float)
                             (coerce col-align 'single-float)))

(export 'icon-view-scroll-to-path)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_visible_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_get_visible_range" %icon-view-visible-range)
    :boolean
  (view (g:object icon-view))
  (start :pointer)
  (end :pointer))

(defun icon-view-visible-range (view)
 #+liber-documentation
 "@version{#2025-07-22}
  @syntax{(gtk:icon-view-visible-range view) => start, end}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[start]{a @class{gtk:tree-path} instance for the start of the region,
    or @code{nil}}
  @argument[end]{a @class{gtk:tree-path} instance for the end of the region,
    or  @code{nil}}
  @begin{short}
    Returns @arg{start} and @arg{end} to be the first and last visible path.
  @end{short}
  Returns @code{nil} if valid paths were not returned in @arg{start} and
  @arg{end}. Note that there may be invisible paths in between.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-path}"
  (cffi:with-foreign-objects ((start :pointer) (end :pointer))
    (when (%icon-view-visible-range view start end)
      (values (cffi:mem-ref start '(g:boxed tree-path :return))
              (cffi:mem-ref end '(g:boxed tree-path :return))))))

(export 'icon-view-visible-range)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_tooltip_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_set_tooltip_item" icon-view-set-tooltip-item)
    :void
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[path]{a @class{gtk:tree-path} instance}
  @begin{short}
    Sets the tip area of tooltip to be the area covered by the item at
    @arg{path}.
  @end{short}
  See also the @fun{gtk:icon-view-tooltip-column} function for a simpler
  alternative. See also the @fun{gtk:tooltip-set-tip-area} function.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-function{gtk:icon-view-tooltip-column}
  @see-function{gtk:tooltip-set-tip-area}"
  (view (g:object icon-view))
  (tooltip (g:object tooltip))
  (path (g:boxed tree-path)))

(export 'icon-view-set-tooltip-item)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_tooltip_cell
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_set_tooltip_cell" icon-view-set-tooltip-cell)
    :void
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[path]{a @class{gtk:tree-path} instance}
  @argument[cell]{a @class{gtk:cell-renderer} object or @code{nil}}
  @begin{short}
    Sets the tip area of the tooltip to the area which @arg{cell} occupies in
    the item pointed to by @arg{path}.
  @end{short}
  See also the @fun{gtk:tooltip-set-tip-area} function. See also the
  @fun{gtk:icon-view-tooltip-column} function for a simpler alternative.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-function{gtk:tooltip-set-tip-area}
  @see-function{gtk:icon-view-tooltip-column}"
  (view (g:object icon-view))
  (tooltip (g:object tooltip))
  (path (g:boxed tree-path))
  (cell (g:object cell-renderer)))

(export 'icon-view-set-tooltip-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_tooltip_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_get_tooltip_context" %icon-view-tooltip-context)
    :boolean
  (view (g:object icon-view))
  (x (:pointer :int))
  (y (:pointer :int))
  (tip :boolean)
  (model :pointer)
  (path :pointer)
  (iter (g:boxed tree-iter)))

(defun icon-view-tooltip-context (view x y tip)
 #+liber-documentation
 "@version{#2025-07-19}
  @syntax{(gtk:icon-view-tooltip-context view x y tip) => xx, yy, model, path,
    iter}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[x]{an integer for the x coordinate, relative to widget coordinates}
  @argument[y]{an integer for the y coordinate, relative to widget coordinates}
  @argument[tip]{a boolean whether this is a keyboard tooltip or not}
  @argument[xx]{an integer for @arg{x} converted to be relative to the bin
    window of @arg{view}}
  @argument[yy]{an integer for @arg{y} converted to be relative to the bin
    window of @arg{view}}
  @argument[model]{a @class{gtk:tree-model} object or @code{nil}}
  @argument[path]{a @class{gtk:tree-path} instance or @code{nil}}
  @argument[iter]{a @class{gtk:tree-iter} iterator or @code{nil}}
  @begin{short}
    This function is supposed to be used in a @sig[gtk:widget]{query-tooltip}
    signal handler for a @class{gtk:icon-view} widget.
  @end{short}
  The @arg{x}, @arg{y} and @arg{tip} values which are received in the signal
  handler, should be passed to this function without modification.

  The return value indicates whether there is an icon view item at the given
  coordinates, @em{true}) or not @em{false} for mouse tooltips. For keyboard
  tooltips the item returned will be the cursor item. When @em{true}, then any
  of @arg{model}, @arg{path} and @arg{iter} will be set to point to that row
  and the corresponding model. @arg{x} and @arg{y} will always be converted to
  be relative to the bin window of @arg{view} if @arg{tip} is @em{false}.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-path}
  @see-class{gtk:tree-iter}"
  (cffi:with-foreign-objects ((xx :int)
                              (yy :int)
                              (model :pointer)
                              (path :pointer))
    (setf (cffi:mem-ref xx :int) x
          (cffi:mem-ref yy :int) y)
    (let ((iter (make-tree-iter)))
      (when (%icon-view-tooltip-context view
                                        xx
                                        yy
                                        tip
                                        model
                                        path
                                        iter)
        (values (cffi:mem-ref xx :int)
                (cffi:mem-ref yy :int)
                (cffi:mem-ref model '(g:object tree-model))
                (cffi:mem-ref path '(g:boxed tree-path :return))
                iter)))))

(export 'icon-view-tooltip-context)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_item_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_get_item_row" icon-view-item-row) :int
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance of the item}
  @return{The integer for the row in which the item is displayed.}
  @begin{short}
    Gets the row in which the item path is currently displayed.
  @end{short}
  Row numbers start at 0.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-function{gtk:icon-view-item-column}"
  (view (g:object icon-view))
  (path (g:boxed tree-path)))

(export 'icon-view-item-row)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_item_column
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_get_item_column" icon-view-item-column) :int
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance of the item}
  @return{The integer for the column in which the item is displayed.}
  @begin{short}
    Gets the column in which the item path is currently displayed.
  @end{short}
  Column numbers start at 0.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-function{gtk:icon-view-item-row}"
  (view (g:object icon-view))
  (path (g:boxed tree-path)))

(export 'icon-view-item-column)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_enable_model_drag_source
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_enable_model_drag_source"
               icon-view-enable-model-drag-source) :void
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[mask]{a @sym{gdk:modifier-type} value for the allowed buttons to
    start drag}
  @argument[formats]{a @class{gdk:content-formats} instance for the formats
    that the drag will support}
  @argument[actions]{a @sym{gdk:drag-action} value for the possible actions
    for a drag from this widget}
  @begin{short}
    Turns the icon view into a drag source for automatic DND.
  @end{short}
  Calling this method sets the @slot[gtk:icon-view]{reorderable} property to
  the @em{false} value.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gdk:content-formats}
  @see-symbol{gdk:modifier-type}
  @see-symbol{gdk:drag-action}"
  (view (g:object icon-view))
  (mask gdk:modifier-type)
  (formats (g:boxed gdk:content-formats))
  (actions gdk:drag-action))

(export 'icon-view-enable-model-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_enable_model_drag_dest
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_enable_model_drag_dest"
               icon-view-enable-model-drag-dest) :void
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[formats]{a @class{gdk:content-formats} instance for the formats
    that the drag will support}
  @argument[actions]{a @sym{gdk:drag-action} value for the possible actions
    for a drag to this widget}
  @begin{short}
    Turns the icon view into a drop destination for automatic drag and drop.
  @end{short}
  Calling this method sets the @slot[gtk:icon-view]{reorderable} property to
  the @em{false} value.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-symbol{gdk:drag-action}"
  (view (g:object icon-view))
  (formats (g:boxed gdk:content-formats))
  (actions gdk:drag-action))

(export 'icon-view-enable-model-drag-dest)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_unset_model_drag_source
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_unset_model_drag_source"
               icon-view-unset-model-drag-source) :void
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[view]{a @class{gtk:icon-view} widget}
  @begin{short}
    Undoes the effect of the @fun{gtk:icon-view-enable-model-drag-source}
    function.
  @end{short}
  Calling this method sets the @slot[gtk:icon-view]{reorderable} property to
  @em{false}.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-function{gtk:icon-view-enable-model-drag-source}"
  (view (g:object icon-view)))

(export 'icon-view-unset-model-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_unset_model_drag_dest
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_unset_model_drag_dest"
               icon-view-unset-model-drag-dest) :void
 #+liber-documentation
 "@version{#2025-03-03}
  @argument[view]{a @class{gtk:icon-view} widget}
  @begin{short}
    Undoes the effect of the @fun{gtk:icon-view-enable-model-drag-dest}
    function.
  @end{short}
  Calling this method sets the @slot[gtk:icon-view]{reorderable} property to
  @em{false}.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-function{gtk:icon-view-enable-model-drag-dest}"
  (view (g:object icon-view)))

(export 'icon-view-unset-model-drag-dest)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_drag_dest_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_set_drag_dest_item" icon-view-set-drag-dest-item)
    :void
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance for the item to highlight,
    or @code{nil}}
  @argument[pos]{a @sym{gtk:icon-view-drop-position} value that specifies
    where to drop, relative to the item}
  @begin{short}
    Sets the item that is highlighted for feedback.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-symbol{gtk:icon-view-drop-position}
  @see-function{gtk:icon-view-get-drag-dest-item}"
  (view (g:object icon-view))
  (path (g:boxed tree-path))
  (pos icon-view-drop-position))

(export 'icon-view-set-drag-dest-item)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_drag_dest_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_get_drag_dest_item" %icon-view-get-drag-dest-item)
    :void
  (view (g:object icon-view))
  (path :pointer)
  (pos :pointer))

(defun icon-view-get-drag-dest-item (view)
 #+liber-documentation
 "@version{#2025-05-22}
  @syntax{(gtk:icon-view-get-drag-dest-item view) => path, pos}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance for the highlighted item,
    or @code{nil}}
  @argument[pos]{a @sym{gtk:icon-view-drop-position} value for the drop
    position, or @code{nil}}
  @begin{short}
    Gets information about the item that is highlighted for feedback.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-path}
  @see-symbol{gtk:icon-view-drop-position}
  @see-function{gtk:icon-view-set-drag-dest-item}"
  (cffi:with-foreign-objects ((path :pointer) (pos :pointer))
    (%icon-view-get-drag-dest-item view path pos)
    (values (cffi:mem-ref path '(g:boxed tree-path :return))
            (cffi:mem-ref pos 'icon-view-drop-position))))

(export 'icon-view-get-drag-dest-item)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_dest_item_at_pos
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_get_dest_item_at_pos"
               %icon-view-dest-item-at-pos) :boolean
  (view (g:object icon-view))
  (xdrag :int)
  (ydrag :int)
  (path :pointer)
  (pos :pointer))

(defun icon-view-dest-item-at-pos (view xdrag ydrag)
 #+liber-documentation
 "@version{#2025-07-22}
  @syntax{(gtk:icon-view-dest-item-at-pos view xdrag ydrag) => path, pos}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[xdrag]{an integer for the position to determine the destination
    item for}
  @argument[ydrag]{an integer for the position to determine the destination
    item for}
  @argument[path]{a @class{gtk:tree-path} instance for the item}
  @argument[pos]{a @sym{gtk:icon-view-drop-position} value}
  @begin{short}
    Determines the destination item for a given position.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-path}
  @see-symbol{gtk:icon-view-drop-position}"
  (cffi:with-foreign-objects ((path :pointer) (pos :pointer))
    (when (%icon-view-dest-item-at-pos view xdrag ydrag path pos)
      (values (cffi:mem-ref path '(g:boxed tree-path :return))
              (cffi:mem-ref pos 'icon-view-drop-position)))))

(export 'icon-view-dest-item-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_create_drag_icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_view_create_drag_icon" icon-view-create-drag-icon)
    (:pointer (:struct cairo:surface-t))
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[view]{a @class{gtk:icon-view} widget}
  @argument[path]{a @class{gtk:tree-path} instance}
  @begin{return}
    The newly allocated @sym{cairo:surface-t} surface for the drag icon.
  @end{return}
  @begin{short}
    Creates a @sym{cairo:surface-t} representation of the item at @arg{path}.
  @end{short}
  This image is used for a drag icon.
  @begin[Warning]{dictionary}
    The @class{gtk:icon-view} implementation is deprecated since 4.10.
    Use the @class{gtk:grid-view} implementation instead.
  @end{dictionary}
  @see-class{gtk:icon-view}
  @see-class{gtk:tree-path}
  @see-symbol{cairo:surface-t}"
  (view (g:object icon-view))
  (path (g:boxed tree-path)))

(export 'icon-view-create-drag-icon)

;;; --- End of file gtk4.icon-view.lisp ----------------------------------------
