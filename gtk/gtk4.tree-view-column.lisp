;;; ----------------------------------------------------------------------------
;;; gtk4.tree-view-column.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkTreeViewColumn
;;;
;;;     A visible column in a GtkTreeView widget
;;;
;;; Types and Values
;;;
;;;     GtkTreeViewColumnSizing
;;;     GtkTreeViewColumn
;;;
;;; Accessors
;;;
;;;     gtk_tree_view_column_set_alignment
;;;     gtk_tree_view_column_get_alignment
;;;     gtk_tree_view_column_set_clickable
;;;     gtk_tree_view_column_get_clickable
;;;     gtk_tree_view_column_set_expand
;;;     gtk_tree_view_column_get_expand
;;;     gtk_tree_view_column_get_fixed_width
;;;     gtk_tree_view_column_set_fixed_width
;;;     gtk_tree_view_column_set_max_width
;;;     gtk_tree_view_column_get_max_width
;;;     gtk_tree_view_column_set_min_width
;;;     gtk_tree_view_column_get_min_width
;;;     gtk_tree_view_column_set_reorderable
;;;     gtk_tree_view_column_get_reorderable
;;;     gtk_tree_view_column_set_resizable
;;;     gtk_tree_view_column_get_resizable
;;;     gtk_tree_view_column_set_sizing
;;;     gtk_tree_view_column_get_sizing
;;;     gtk_tree_view_column_set_sort_column_id
;;;     gtk_tree_view_column_get_sort_column_id
;;;     gtk_tree_view_column_set_sort_indicator
;;;     gtk_tree_view_column_get_sort_indicator
;;;     gtk_tree_view_column_set_sort_order
;;;     gtk_tree_view_column_get_sort_order
;;;     gtk_tree_view_column_set_spacing
;;;     gtk_tree_view_column_get_spacing
;;;     gtk_tree_view_column_set_title
;;;     gtk_tree_view_column_get_title
;;;     gtk_tree_view_column_set_visible
;;;     gtk_tree_view_column_get_visible
;;;     gtk_tree_view_column_set_widget
;;;     gtk_tree_view_column_get_widget
;;;     gtk_tree_view_column_get_width
;;;     gtk_tree_view_column_get_x_offset
;;;
;;; Functions
;;;
;;;     gtk_tree_view_column_new
;;;     gtk_tree_view_column_new_with_area
;;;     gtk_tree_view_column_new_with_attributes
;;;
;;;     gtk_tree_view_column_pack_start
;;;     gtk_tree_view_column_pack_end
;;;     gtk_tree_view_column_clear
;;;     gtk_tree_view_column_add_attribute
;;;     gtk_tree_view_column_set_attributes
;;;
;;;     GtkTreeCellDataFunc
;;;
;;;     gtk_tree_view_column_set_cell_data_func
;;;     gtk_tree_view_column_clear_attributes
;;;     gtk_tree_view_column_clicked
;;;     gtk_tree_view_column_get_button
;;;     gtk_tree_view_column_cell_set_cell_data
;;;     gtk_tree_view_column_cell_get_size
;;;     gtk_tree_view_column_cell_get_position
;;;     gtk_tree_view_column_cell_is_visible
;;;     gtk_tree_view_column_focus_cell
;;;     gtk_tree_view_column_queue_resize
;;;     gtk_tree_view_column_get_tree_view
;;;
;;; Properties
;;;
;;;     alignment
;;;     cell-area
;;;     clickable
;;;     expand
;;;     fixed-width
;;;     max-width
;;;     min-width
;;;     reorderable
;;;     resizable
;;;     sizing
;;;     sort-column-id
;;;     sort-indicator
;;;     sort-order
;;;     spacing
;;;     title
;;;     visible
;;;     widget
;;;     width
;;;     x-offset
;;;
;;; Signals
;;;
;;;     clicked
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkTreeViewColumn
;;;
;;; Implemented Interfaces
;;;
;;;     GtkBuildable
;;;     GtkCellLayout
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewColumnSizing
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkTreeViewColumnSizing" tree-view-column-sizing
  (:export t
   :type-initializer "gtk_tree_view_column_sizing_get_type")
  (:grow-only 0)
  (:autosize 1)
  (:fixed 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-view-column-sizing)
      "GEnum"
      (liber:symbol-documentation 'tree-view-column-sizing)
 "@version{#2021-2-24}
  @begin{short}
    The sizing method the tree view column uses to determine its width.
  @end{short}
  Please note that the value @code{:autosize} is inefficient for large tree
  views, and can make tree view columns appear choppy.
  @begin{pre}
(gobject:define-g-enum \"GtkTreeViewColumnSizing\" tree-view-column-sizing
  (:export t
   :type-initializer \"gtk_tree_view_column_sizing_get_type\")
  (:grow-only 0)
  (:autosize 1)
  (:fixed 2))
  @end{pre}
  @begin[code]{table}
    @entry[:grow-only]{Columns only get bigger in reaction to changes in the
      model.}
    @entry[:autosize]{Columns resize to be the optimal size everytime the model
      changes.}
    @entry[:fixed]{Columns are a fixed numbers of pixels wide.}
  @end{table}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-sizing}")

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewColumn
;;; ----------------------------------------------------------------------------

;; TODO: Implement the GtkCellLayout interface

(gobject:define-g-object-class "GtkTreeViewColumn" tree-view-column
  (:superclass g:initially-unowned
   :export t
   :interfaces ("GtkBuildable"
;                "GtkCellLayout"
                )
   :type-initializer "gtk_tree_view_column_get_type")
  ((alignment
    tree-view-column-alignment
    "alignment" "gfloat" t t)
   (cell-area
    tree-view-column-cell-area
    "cell-area" "GtkCellArea" t t)
   (clickable
    tree-view-column-clickable
    "clickable" "gboolean" t t)
   (expand
    tree-view-column-expand
    "expand" "gboolean" t t)
   (fixed-width
    tree-view-column-fixed-width
    "fixed-width" "gint" t t)
   (max-width
    tree-view-column-max-width
    "max-width" "gint" t t)
   (min-width
    tree-view-column-min-width
    "min-width" "gint" t t)
   (reorderable
    tree-view-column-reorderable
    "reorderable" "gboolean" t t)
   (resizable
    tree-view-column-resizable
    "resizable" "gboolean" t t)
   (sizing
    tree-view-column-sizing
    "sizing" "GtkTreeViewColumnSizing" t t)
   (sort-column-id
    tree-view-column-sort-column-id
    "sort-column-id" "gint" t t)
   (sort-indicator
    tree-view-column-sort-indicator
    "sort-indicator" "gboolean" t t)
   (sort-order
    tree-view-column-sort-order
    "sort-order" "GtkSortType" t t)
   (spacing
    tree-view-column-spacing
    "spacing" "gint" t t)
   (title
    tree-view-column-title
    "title" "gchararray" t t)
   (visible
    tree-view-column-visible
    "visible" "gboolean" t t)
   (widget
    tree-view-column-widget
    "widget" "GtkWidget" t t)
   (width
    tree-view-column-width
    "width" "gint" t nil)
   (x-offset
    tree-view-column-x-offset
    "x-offset" "gint" t nil)))

#+liber-documentation
(setf (documentation 'tree-view-column 'type)
 "@version{#2022-1-24}
  @begin{short}
    The @sym{gtk:tree-view-column} object represents a visible column in a
    @class{gtk:tree-view} widget.
  @end{short}
  It allows to set properties of the tree view column header, and functions as
  a holding pen for the cell renderers which determine how the data in the
  tree view column is displayed.

  Please refer to the tree view widget conceptual overview for an overview of
  all the objects and data types related to the tree view and how they work
  together.
  @begin[Signal Details]{dictionary}
    @subheading{The \"clicked\" signal}
      @begin{pre}
lambda (column)    :run-last
      @end{pre}
    Emitted when the header of the column has been clicked.
    @begin[code]{table}
      @entry[column]{The @class{gtk:tree-view-column} object which
        emitted the signal.}
    @end{table}
  @end{dictionary}
  @see-slot{gtk:tree-view-column-alignment}
  @see-slot{gtk:tree-view-column-cell-area}
  @see-slot{gtk:tree-view-column-clickable}
  @see-slot{gtk:tree-view-column-expand}
  @see-slot{gtk:tree-view-column-fixed-width}
  @see-slot{gtk:tree-view-column-max-width}
  @see-slot{gtk:tree-view-column-min-width}
  @see-slot{gtk:tree-view-column-reorderable}
  @see-slot{gtk:tree-view-column-resizable}
  @see-slot{gtk:tree-view-column-sizing}
  @see-slot{gtk:tree-view-column-sort-column-id}
  @see-slot{gtk:tree-view-column-sort-indicator}
  @see-slot{gtk:tree-view-column-sort-order}
  @see-slot{gtk:tree-view-column-spacing}
  @see-slot{gtk:tree-view-column-title}
  @see-slot{gtk:tree-view-column-visible}
  @see-slot{gtk:tree-view-column-widget}
  @see-slot{gtk:tree-view-column-width}
  @see-slot{gtk:tree-view-column-x-offset}
  @see-class{gtk:tree-view}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- tree-view-column-alignment ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "alignment"
                                               'tree-view-column) t)
 "The @code{alignment} property of type @code{:float} (Read / Write) @br{}
  Alignment of the tree view column header text or widget, 0.0 for left,
  0.5 for center, and 1.0 for right alignment. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-alignment)
      "Accessor"
      (documentation 'tree-view-column-alignment 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-alignment object) => align}
  @syntax[]{(setf (gtk:tree-view-column-alignment object) align)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[align]{a float with the alignment, which is between 0.0 and 1.0
    inclusive}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{alignment} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The @sym{gtk:tree-view-column-alignment} function returns the current
  alignment of the title or custom widget inside the tree view column header.
  The @sym{(setf gtk:tree-view-column-alignment)} function sets the alignment.
  The alignment determines the location inside the header button, 0.0 for left,
  0.5 for center, 1.0 for right alignment.
  @see-class{gtk:tree-view-column}")

;;; --- tree-view-column-cell-area ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cell-area"
                                               'tree-view-column) t)
 "The @code{cell-area} property of type @class{gtk:cell-area}
  (Read / Write / Construct) @br{}
  The cell area used to layout cell renderers for this tree view column. If no
  cell area is specified when creating the tree view column a horizontally
  oriented @class{gtk:cell-area-box} object will be used.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-cell-area)
      "Accessor"
      (documentation 'tree-view-column-cell-area 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-cell-area object) => cell-area}
  @syntax[]{(setf (gtk:tree-view-column-cell-area object) cell-area)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[cell-area]{a @class{gtk:cell-area} object}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{cell-area} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The cell area used to layout cell renderers for this tree view column. If no
  cell area is specified when creating the tree view column a horizontally
  oriented @class{gtk:cell-area-box} object will be used.
  @see-class{gtk:tree-view-column}")

;;; --- tree-view-column-clickable ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "clickable"
                                               'tree-view-column) t)
 "The @code{clickable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the tree view column header can be clicked. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-clickable)
      "Accessor"
      (documentation 'tree-view-column-clickable 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-clickable object) => clickable}
  @syntax[]{(setf (gtk:tree-view-column-clickable object) clickable)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[clickable]{@em{true} if the tree view column header is active}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{clickable} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The @sym{gtk:tree-view-column-clickable} function returns @em{true} if the
  user can click on the header for the tree view column. The
  @sym{(setf gtk:tree-view-column-clickable)} function sets the header to be
  active if clickable is @em{true}. When the header is active, then it can take
  keyboard focus, and can be clicked.
  @see-class{gtk:tree-view-column}")

;;; --- tree-view-column-expand ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expand"
                                               'tree-view-column) t)
 "The @code{expand} property of type @code{:boolean} (Read / Write) @br{}
  Tree view column gets share of extra width allocated to the widget. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-expand)
      "Accessor"
      (documentation 'tree-view-column-expand 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-expand object) => expand}
  @syntax[]{(setf (gtk:tree-view-column-expand object) expand)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[expand]{@em{true} if the tree view column should take available
    extra space, @em{false} if not}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{expand} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The @sym{gtk:tree-view-column-expand} function returns @em{true} if the tree
  view column expands to take any available space. The
  @sym{(setf gtk:tree-view-column-expand)} function sets the tree view column
  to take available extra space. This space is shared equally amongst all
  tree view columns that have the expand set to @em{true}. If no column has this
  option set, then the last column gets all extra space. By default, every
  column is created with this @em{false}.
  @see-class{gtk:tree-view-column}")

;;; --- tree-view-column-fixed-width -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "fixed-width"
                                               'tree-view-column) t)
 "The @code{fixed-width} property of type @code{:int} (Read / Write) @br{}
  Current fixed width of the tree view column. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-fixed-width)
      "Accessor"
      (documentation 'tree-view-column-fixed-width 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-fixed-width object) => fixed-width}
  @syntax[]{(setf (gtk:tree-view-column-fixed-width object) fixed-width)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[fixed-width]{an integer with the size to set the tree view column
  to, must be greater than 0}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{fixed-width} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The @sym{gtk:tree-view-column-fixed-width} function gets the fixed width of
  the tree view column in pixels. The
  @sym{(setf gtk:tree-view-column-fixed-width)} function sets the size. This is
  meaningful only if the @slot[gtk:tree-view-column]{sizing} property is
  @code{:fixed}. The size of the tree view column is clamped to the min/max
  width for the column. Please note that the min/max width of the column does
  not actually affect the @slot[gtk:tree-view-column]{fixed-width} property of
  the widget, just the actual size when displayed.
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-sizing}
  @see-function{gtk:tree-view-column-max-width}
  @see-function{gtk:tree-view-column-min-width}")

;;; --- tree-view-column-max-width ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-width"
                                               'tree-view-column) t)
 "The @code{max-width} property of type @code{:int} (Read / Write) @br{}
  Maximum allowed width of the tree view column. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1 @br{}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-max-width)
      "Accessor"
      (documentation 'tree-view-column-max-width 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-max-width object) => max-width}
  @syntax[]{(setf (gtk:tree-view-column-max-width object) max-width)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[max-width]{an integer with the maximum width of the tree view
    column in pixels, or -1}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{max-width} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The @sym{gtk:tree-view-column-max-width} function returns the maximum width in
  pixels of the tree view column, or -1 if no maximum width is set. The
  @sym{(setf gtk:tree-view-column-max-width)} function sets the maximum width.
  If @arg{max-width} is -1, then the maximum width is unset. Note, the tree view
  column can actually be wider than max width if it is the last column in a tree
  view. In this case, the column expands to fill any extra space.
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-min-width}")

;;; --- tree-view-column-min-width ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "min-width"
                                               'tree-view-column) t)
 "The @code{min-width} property of type @code{:int} (Read / Write )@br{}
  Minimum allowed width of the tree view column. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-min-width)
      "Accessor"
      (documentation 'tree-view-column-min-width 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-min-width object) => min-width}
  @syntax[]{(setf (gtk:tree-view-column-min-width object) min-width)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[min-width]{an integer with the minimum width of the tree view
    column in pixels, or -1}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{min-width} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The @sym{gtk:tree-view-column-min-width} function returns the minimum width in
  pixels of the tree view column, or -1 if no minimum width is set. The
  @sym{(setf gtk:tree-view-column-min-width)} function sets the minimum width.
  If @arg{min-width} is -1, then the minimum width is unset.
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-max-width}")

;;; --- tree-view-column-reorderable -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "reorderable"
                                               'tree-view-column) t)
 "The @code{reorderable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the tree view column can be reordered around the headers. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-reorderable)
      "Accessor"
      (documentation 'tree-view-column-reorderable 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-reorderable object) => reorderable}
  @syntax[]{(setf (gtk:tree-view-column-reorderable object) reorderable)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[reorderable]{@em{true}, if the tree view column can be reordered}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{reorderable} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  If @code{reorderable} is @em{true}, then the tree view column can be
  reordered by the user dragging the header.
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-resizable}")

;;; --- tree-view-column-resizable ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resizable"
                                               'tree-view-column) t)
 "The @code{resizable} property of type @code{:boolean} (Read / Write) @br{}
  Column is user-resizable. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-resizable)
      "Accessor"
      (documentation 'tree-view-column-resizable 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-resizable object) => resizable}
  @syntax[]{(setf (gtk:tree-view-column-resizable object) resizable)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[resizable]{@em{true}, if the tree view column can be resized}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{resizable} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The @sym{gtk:tree-view-column-resizable} function returns @em{true} if the
  tree view column can be resized by the user.

  If @code{resizable} is @em{true}, then the user can explicitly resize the
  tree view column by grabbing the outer edge of the column button. If
  @code{resizable} is @em{true} and the sizing mode of the tree view column is
  @code{:autosize}, then the sizing mode is changed to @code{:grow-only}.
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-sizing}
  @see-function{gtk:tree-view-column-reorderable}")

;;; --- tree-view-column-sizing ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sizing"
                                               'tree-view-column) t)
 "The @code{sizing} property of type @symbol{gtk:tree-view-column-sizing}
  (Read / Write) @br{}
  Resize mode of the tree view column. @br{}
  Default value: @code{:grow-only}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-sizing)
      "Accessor"
      (documentation 'tree-view-column-sizing 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-sizing object) => type}
  @syntax[]{(setf (gtk:tree-view-column-sizing object) type)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[type]{a value of the @symbol{gtk:tree-view-column-sizing}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{sizing} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The @sym{gtk:tree-view-column-sizing} function returns the current sizing type
  of the tree view column. The @sym{(setf gtk:tree-view-column-sizing)} sets the
  sizing type.
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-resizable}")

;;; --- tree-view-column-sort-column-id ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sort-column-id"
                                               'tree-view-column) t)
 "The @code{sort-column-id} property of type @code{:int} (Read / Write) @br{}
  Logical sort column ID this tree view column sorts on when selected for
  sorting. Setting the sort column ID makes the tree view column header
  clickable. Set to -1 to make the column unsortable. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-sort-column-id)
      "Accessor"
      (documentation 'tree-view-column-sort-column-id 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-sort-column-id object) => sort-column-id}
  @syntax[]{(setf (gtk:tree-view-column-sort-column-id object) sort-column-id)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[sort-column-id]{an integer with the @code{sort-column-id} of the
    model to sort on}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{sort-column-id} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The @sym{gtk:tree-view-column-sort-column-id} function gets the logical sort
  column ID that the model sorts on when this tree view column is selected for
  sorting. The @sym{(setf gtk:tree-view-column-sort-column-id)} function sets
  the logical sort column ID. Doing so makes the tree view column header
  clickable.
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-sort-indicator}
  @see-function{gtk:tree-view-column-sort-order}")

;;; --- tree-view-column-sort-indicator ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sort-indicator"
                                               'tree-view-column) t)
 "The @code{sort-indicator} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to show a sort indicator. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-sort-indicator)
      "Accessor"
      (documentation 'tree-view-column-sort-indicator 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-sort-indicator object) => setting}
  @syntax[]{(setf (gtk:tree-view-column-sort-indicator object) setting)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[setting]{@em{true} to display an indicator that the tree view
    column is sorted}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{sort-indicator} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  Call this function with a setting of @em{true} to display an arrow in the
  header button indicating the tree view column is sorted. Call the
  @fun{gtk:tree-view-column-sort-order} function to change the direction of the
  arrow.
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-sort-order}
  @see-function{gtk:tree-view-column-sort-column-id}")

;;; --- tree-view-column-sort-order --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sort-order"
                                               'tree-view-column) t)
 "The @code{sort-order} property of type @symbol{gtk:sort-type} (Read / Write)
  @br{}
  Sort direction the sort indicator should indicate. @br{}
  Default value: @code{:ascending}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-sort-order)
      "Accessor"
      (documentation 'tree-view-column-sort-order 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-sort-order object) => order}
  @syntax[]{(setf (gtk:tree-view-column-sort-order object) order)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[order]{a value of the @symbol{gtk:sort-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{sort-order} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  This does not actually sort the model. Use the
  @fun{gtk:tree-view-column-sort-column-id} function if you want automatic
  sorting support. This function is primarily for custom sorting behavior, and
  should be used in conjunction with the @fun{gtk:tree-sortable-sort-column-id}
  function to do that. For custom models, the mechanism will vary.

  The sort indicator changes direction to indicate normal sort or reverse
  sort. Note that you must have the sort indicator enabled to see anything
  when calling this function. See the @fun{gtk:tree-view-column-sort-indicator}
  function.
  @see-class{gtk:tree-view-column}
  @see-symbol{gtk:sort-type}
  @see-function{gtk:tree-view-column-sort-column-id}
  @see-function{gtk:tree-view-column-sort-indicator}
  @see-function{gtk:tree-sortable-sort-column-id}")

;;; --- tree-view-column-spacing -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "spacing"
                                               'tree-view-column) t)
 "The @code{spacing} property of type @code{:int} (Read / Write) @br{}
  Space which is inserted between cell renderers. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-spacing)
      "Accessor"
      (documentation 'tree-view-column-spacing 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-spacing object) => spacing}
  @syntax[]{(setf (gtk:tree-view-column-spacing object) spacing)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[spacing]{an integer with the distance between cell renderers in
    pixels}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{spacing} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The @sym{gtk:tree-view-column-spacing} function returns the spacing of the
  tree view column, which is the number of pixels to place between cell
  renderers packed into it. The @sym{(setf gtk:tree-view-column-spacing)}
  function sets the spacing field of the tree
  view column.
  @see-class{gtk:tree-view-column}")

;;; --- tree-view-column-title -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title"
                                               'tree-view-column) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  Title to appear in the tree view column header. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-title)
      "Accessor"
      (documentation 'tree-view-column-title 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-title object) => title}
  @syntax[]{(setf (gtk:tree-view-column-title object) title)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[title]{a string with the title of the tree view column}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{title} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The @sym{gtk:tree-view-column-title} function returns the title of the the
  tree view column. The @sym{(setf gtk:tree-view-column-spacing)} function sets
  the title. If a custom widget has been set, then this value is ignored.
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-widget}")

;;; --- tree-view-column-visible -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible"
                                               'tree-view-column) t)
 "The @code{visible} property of type @code{:boolean} (Read / Write) @br{}
  Whether to display the tree view column. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-visible)
      "Accessor"
      (documentation 'tree-view-column-visible 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-visible object) => visible}
  @syntax[]{(setf (gtk:tree-view-column-visible object) visible)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[visible]{@em{true} if the tree view column is visible}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{visible} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The @sym{gtk:tree-view-column-visible} function returns whether the tree view
  column is visible or not. If it is visible, then the tree will show the
  column. The @sym{(setf gtk:tree-view-column-visible)} function sets the
  visibility of the tree view column.
  @see-class{gtk:tree-view-column}")

;;; --- tree-view-column-widget ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "widget"
                                               'tree-view-column) t)
 "The @code{widget} property of type @class{gtk:widget} (Read / Write) @br{}
  Widget to put in the tree view column header button instead of column title.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-widget)
      "Accessor"
      (documentation 'tree-view-column-widget 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-widget object) => widget}
  @syntax[]{(setf (gtk:tree-view-column-widget object) widget)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[widget]{a @class{gtk:widget} child, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{widget} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The @sym{gtk:tree-view-column-widget}v function returns the child widget in
  the button on the tree view column header. If a custom widget has not been set
  then @code{nil} is returned. The  @sym{(setf gtk:tree-view-column-widget)}
  function sets the child widget. If the child widget is @code{nil}, then the
  header button is set with a @class{gtk:label} widget set to the title of the
  tree view column.
  @see-class{gtk:tree-view-column}
  @see-class{gtk:label}
  @see-function{gtk:tree-view-column-title}")

;;; --- tree-view-column-width -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width"
                                               'tree-view-column) t)
 "The @code{width} property of type @code{:int} (Read) @br{}
  Current width of the tree view column. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-width)
      "Accessor"
      (documentation 'tree-view-column-width 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-width object) => width}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[width]{an integer with the current width of the tree view column}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{width} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The @sym{gtk:tree-view-column-width} function returns the current size of the
  tree view column in pixels.
  @see-class{gtk:tree-view-column}")

;;; --- tree-view-column-x-offset ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "x-offset"
                                               'tree-view-column) t)
 "The @code{x-offset} property of type @code{:int} (Read) @br{}
  Current x position of the tree view column. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-x-offset)
      "Accessor"
      (documentation 'tree-view-column-x-offset 'function)
 "@version{#2021-2-24}
  @syntax[]{(gtk:tree-view-column-x-offset object) => offset}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[offset]{an integer with the current x offset in pixels}
  @begin{short}
    Accessor of the @slot[gtk:tree-view-column]{x-offset} slot of the
    @class{gtk:tree-view-column} class.
  @end{short}
  The @sym{gtk:tree-view-column-width} function returns the current x offset of
  the tree view column in pixels.
  @see-class{gtk:tree-view-column}")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_new
;;; ----------------------------------------------------------------------------

(defun tree-view-column-new ()
 #+liber-documentation
 "@version{#2021-2-24}
  @return{A newly created @class{gtk:tree-view-column} object.}
  @begin{short}
    Creates a new tree view column.
  @end{short}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-new-with-area}
  @see-function{gtk:tree-view-column-new-with-attributes}"
  (make-instance 'tree-view-column))

(export 'tree-view-column-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_new_with_area
;;; ----------------------------------------------------------------------------

(declaim (inline tree-view-column-new-with-area))

(defun tree-view-column-new-with-area (area)
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[area]{the @class{gtk:cell-area} object that the newly created
   tree view column should use to layout cell renderers}
  @return{A newly created @class{gtk:tree-view-column} object.}
  @begin{short}
    Creates a new tree view column using @arg{area} to render its cells.
  @end{short}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:cell-area}
  @see-function{gtk:tree-view-column-new}
  @see-function{gtk:tree-view-column-new-with-attributes}"
  (make-instance 'tree-view-column
                 :cell-area area))

(export 'tree-view-column-new-with-area)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_new_with_attributes
;;; ----------------------------------------------------------------------------

(defun tree-view-column-new-with-attributes (title renderer
                                                 &rest attributes)
 #+liber-documentation
 "@version{#2021-3-13}
  @argument[title]{a string with the title to set the header to}
  @argument[renderer]{the @class{gtk:cell-renderer} object}
  @argument[attributes]{a list of attributes}
  @return{A newly created @class{gtk:tree-view-column} object.}
  @begin{short}
    Creates a new tree view column with a number of default values.
  @end{short}
  This is equivalent to calling the @fun{gtk:tree-view-column-title},
  @fun{gtk:tree-view-column-pack-start}, and
  @fun{gtk:tree-view-column-set-attributes} functions on the newly created
  @class{gtk:tree-view-column} object.

  Here is an example:
  @begin{pre}
(let* ((renderer (gtk:cell-renderer-text-new))
       (column (gtk:tree-view-column-new-with-attributes \"Example\"
                                                         renderer
                                                         \"text\" 0
                                                         \"foreground\" 1)))
  ... )
  @end{pre}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:tree-view-column-title}
  @see-function{gtk:tree-view-column-pack-start}
  @see-function{gtk:tree-view-column-set-attributes}"
  (let ((column (make-instance 'tree-view-column
                               :title title)))
    (tree-view-column-pack-start column renderer :expand t)
    (apply #'tree-view-column-set-attributes column renderer attributes)
    column))

(export 'tree-view-column-new-with-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_pack_start
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_pack_start" %tree-view-column-pack-start)
    :void
  (column (g:object tree-view-column))
  (renderer (g:object cell-renderer))
  (expand :boolean))

(defun tree-view-column-pack-start (column renderer &key (expand t))
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{the @class{gtk:cell-renderer} object}
  @argument[expand]{@em{true} if the cell renderer is to be given extra space
    allocated to the tree view column}
  @begin{short}
    Packs the cell renderer into the beginning of the tree view column.
  @end{short}
  If @arg{expand} is @em{false}, then the cell renderer is allocated no more
  space than it needs. Any unused space is divided evenly between cell
  renderers for which @arg{expand} is @em{true}.
  @see-class{gtk:tree-view-column}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:tree-view-column-pack-end}"
  (%tree-view-column-pack-start column renderer expand))

(export 'tree-view-column-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_pack_end
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_pack_end" %tree-view-column-pack-end) :void
  (column (g:object tree-view-column))
  (renderer (g:object cell-renderer))
  (expand :boolean))

(defun tree-view-column-pack-end (column renderer &key (expand t))
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{the @class{gtk:cell-renderer} object}
  @argument[expand]{@em{true} if the cell renderer is to be given extra space
    allocated to the tree view column}
  @begin{short}
    Packs the cell renderer to the end of the tree view column.
  @end{short}
  If @arg{expand} is @em{false}, then the cell renderer is allocated no more
  space than it needs. Any unused space is divided evenly between cell
  renderers for which expand is @em{true}.
  @see-class{gtk:tree-view-column}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:tree-view-column-pack-start}"
  (%tree-view-column-pack-end column renderer expand))

(export 'tree-view-column-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_clear
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_clear" tree-view-column-clear) :void
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @begin{short}
    Unsets all the mappings on all cell renderers on the tree view column.
  @end{short}
  @see-class{gtk:tree-view-column}"
  (column (g:object tree-view-column)))

(export 'tree-view-column-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_add_attribute
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_add_attribute"
               tree-view-column-add-attribute) :void
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{the @class{gtk:cell-renderer} object to set attributes on}
  @argument[attribute]{a string with an attribute on the cell renderer}
  @argument[position]{an integer with the column position on the model to get
    the attribute from}
  @begin{short}
    Adds an attribute mapping to the list in the tree view column.
  @end{short}
  The argument @arg{column} is the column of the model to get a value from, and
  the attribute is the parameter on the cell renderer to be set from the value.
  So for example if column 2 of the model contains strings, you could have the
  \"text\" attribute of a @class{gtk:cell-renderer-text} get its values from
  column 2.
  @see-class{gtk:tree-view-column}
  @see-class{gtk:cell-renderer}
  @see-class{gtk:cell-renderer-text}
  @see-function{gtk:tree-view-column-new-with-attributes}"
  (column (g:object tree-view-column))
  (renderer (g:object cell-renderer))
  (attribute :string)
  (position :int))

(export 'tree-view-column-add-attribute)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_attributes
;;; ----------------------------------------------------------------------------

(defun tree-view-column-set-attributes (column renderer &rest attributes)
 #+liber-documentation
 "@version{#2021-3-13}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{the @class{gtk:cell-renderer} object we are setting the
    attributes of}
  @argument[attributes]{a list of attributes}
  @begin{short}
    Sets the attributes in the list as the attributes of the tree view column.
  @end{short}
  The attributes should be in attribute/column order, as in the
  @fun{gtk:tree-view-column-add-attribute} function. All existing attributes are
  removed, and replaced with the new attributes.
  @see-class{gtk:tree-view-column}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:tree-view-column-add-attribute}"
  (loop for (attribute col) on attributes by #'cddr
        do (tree-view-column-add-attribute column renderer attribute col)))

(export 'tree-view-column-set-attributes)

;;; ----------------------------------------------------------------------------
;;; GtkTreeCellDataFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback tree-cell-data-func :void
    ((column (g:object tree-view-column))
     (renderer (g:object cell-renderer))
     (model (g:object tree-model))
     (iter (g:boxed tree-iter))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func column renderer model iter)
      (return-from-tree-cell-data-func () nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-cell-data-func)
      "Callback"
      (liber:symbol-documentation 'tree-cell-data-func)
 "@version{#2021-2-24}
  @begin{short}
    A function to set the properties of a cell instead of just using the
    straight mapping between the cell and the model.
  @end{short}
  This is useful for customizing the cell renderer. For example, a function
  might get an integer from the tree model, and render it to the \"text\"
  attribute of \"cell\" by converting it to its written equivilent. This is set
  by calling the @fun{gtk:tree-view-column-set-cell-data-func} function.
  @begin{pre}
 lambda (column renderer model iter)
  @end{pre}
  @begin[code]{table}
    @entry[column]{A @class{gtk:tree-view-column} object.}
    @entry[renderer]{The @class{gtk:cell-renderer} object that is being
      rendered by the tree view column.}
    @entry[model]{The @class{gtk:tree-model} object being rendered.}
    @entry[iter]{A @class{gtk:tree-iter} object of the current row rendered.}
  @end{table}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-set-cell-data-func}")

(export 'tree-cell-data-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_cell_data_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_set_cell_data_func"
               %tree-view-column-set-cell-data-func) :void
  (column (g:object tree-view-column))
  (renderer (g:object cell-renderer))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun tree-view-column-set-cell-data-func (column renderer func)
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[func]{the @symbol{gtk:tree-cell-data-func} callback to use}
  @begin{short}
    Sets the callback function to use for the tree view column.
  @end{short}
  This function is used instead of the standard attributes mapping for setting
  the column value, and should set the value of the tree view column cell
  renderer as appropriate. @arg{func} may be @code{nil} to remove an older one.
  @see-class{gtk:tree-view-column}
  @see-class{gtk:cell-renderer}
  @see-symbol{gtk:tree-cell-data-func}"
  (%tree-view-column-set-cell-data-func
          column
          renderer
          (cffi:callback tree-cell-data-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'tree-view-column-set-cell-data-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_clear_attributes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_clear_attributes"
               tree-view-column-clear-attributes) :void
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{a @class{gtk:cell-renderer} to clear the attribute
    mapping on}
  @begin{short}
    Clears all existing attributes previously set e.g. with the
    @fun{gtk:tree-view-column-set-attributes} function.
  @end{short}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-set-attributes}"
  (column (g:object tree-view-column))
  (renderer (g:object cell-renderer)))

(export 'tree-view-column-clear-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_clicked
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_clicked" tree-view-column-clicked) :void
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @begin{short}
    Emits the \"clicked\" signal on the tree view column.
  @end{short}
  This function will only work if the tree view column is clickable.
  @see-class{gtk:tree-view-column}"
  (column (g:object tree-view-column)))

(export 'tree-view-column-clicked)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_button -> tree-view-column-button
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_get_button" tree-view-column-button)
    (g:object widget)
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @return{The @class{gtk:widget} button for the tree view column header.}
  @begin{short}
    Returns the button used in the tree view column header.
  @end{short}
  @see-class{gtk:tree-view-column}"
  (column (g:object tree-view-column)))

(export 'tree-view-column-button)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_set_cell_data
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_cell_set_cell_data"
               tree-view-column-cell-set-cell-data) :void
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[model]{the @class{gtk:tree-model} to to get the cell renderers
    attributes from}
  @argument[iter]{the @class{gtk:tree-iter} to to get the cell renderer's
    attributes from}
  @argument[is-expander]{@em{true}, if the row has children}
  @argument[is-expanded]{@em{true}, if the row has visible children}
  @begin{short}
    Sets the cell renderer based on the tree model and @arg{iter}.
  @end{short}
  That is, for every attribute mapping in the tree view column, it will get a
  value from the set column on the @arg{iter}, and use that value to set the
  attribute on the cell renderer. This is used primarily by the tree view.
  @see-class{gtk:tree-view-column}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}"
  (column (g:object tree-view-column))
  (model (g:object tree-model))
  (iter (g:boxed tree-iter))
  (is-expander :boolean)
  (is-expanded :boolean))

(export 'tree-view-column-cell-set-cell-data)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_get_size -> tree-view-column-cell-size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_cell_get_size" %tree-view-column-cell-size)
    :void
  (column (g:object tree-view-column))
  (area (g:boxed gdk:rectangle))
  (x-offset (:pointer :int))
  (y-offset (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun tree-view-column-cell-size (column area)
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[area]{a @class{gdk:rectangle} with the area a cell in the
    column will be allocated, or @code{nil}}
  @begin{return}
    @arg{x-offset} -- an integer with the x offset of a cell relative to
      @arg{area} @br{}
    @arg{y-offset} -- an integer with the y offset of a cell relative to
      @arg{area} @br{}
    @arg{width} -- an integer with the width needed to render a cell @br{}
    @arg{height} -- an integer with the height needed to render a cell
  @end{return}
  @begin{short}
    Obtains the width and height needed to render the column.
  @end{short}
  This is used primarily by the tree view.
  @see-class{gtk:tree-view-column}
  @see-class{gdk:rectangle}"
  (cffi:with-foreign-objects ((x-offset :int)
                              (y-offset :int)
                              (width :int)
                              (height :int))
    (%tree-view-column-cell-size column
                                 area
                                 x-offset
                                 y-offset
                                 width
                                 height)
    (values (cffi:mem-ref x-offset :int)
            (cffi:mem-ref y-offset :int)
            (cffi:mem-ref width :int)
            (cffi:mem-ref height :int))))

(export 'tree-view-column-cell-size)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_get_position -> tree-view-column-cell-position
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_cell_get_position"
               %tree-view-column-cell-position) :boolean
  (column (g:object tree-view-column))
  (renderer (g:object cell-renderer))
  (x-offset (:pointer :int))
  (width (:pointer :int)))

(defun tree-view-column-cell-position (column renderer)
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @begin{return}
    @arg{x-offset} -- an integer with the horizontal position of the cell
      within the tree view column @br{}
    @arg{width} -- an integer with the width of the cell
  @end{return}
  @begin{short}
    Obtains the horizontal position and size of a cell in a tree view column.
  @end{short}
  If the cell is not found in the column, @code{nil} is returned.
  @see-class{gtk:tree-view-column}
  @see-class{gtk:cell-renderer}"
  (cffi:with-foreign-objects ((x-offset :int) (width :int))
    (when (%tree-view-column-cell-position column
                                           renderer
                                           x-offset
                                           width)
      (values (cffi:mem-ref x-offset :int)
              (cffi:mem-ref width :int)))))

(export 'tree-view-column-cell-position)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_is_visible
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_cell_is_visible"
               tree-view-column-cell-is-visible) :boolean
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @begin{return}
    @em{True}, if any of the cells packed into the tree view column are
    currently visible.
  @end{return}
  @begin{short}
    Returns @em{true} if any of the cells packed into the tree view column are
    visible.
  @end{short}
  For this to be meaningful, you must first initialize the cells with the
  @fun{gtk:tree-view-column-cell-set-cell-data} function.
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-cell-set-cell-data}"
  (column (g:object tree-view-column)))

(export 'tree-view-column-cell-is-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_focus_cell
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_focus_cell" tree-view-column-focus-cell)
    :void
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @begin{short}
    Sets the current keyboard focus to be at cell, if the column contains 2 or
    more editable and activatable cells.
  @end{short}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:cell-renderer}"
  (column (g:object tree-view-column))
  (renderer (g:object cell-renderer)))

(export 'tree-view-column-focus-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_queue_resize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_queue_resize"
               tree-view-column-queue-resize) :void
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @begin{short}
    Flags the tree view column, and the cell renderers added to this column, to
    have their sizes renegotiated.
  @end{short}
  @see-class{gtk:tree-view-column}"
  (column (g:object tree-view-column)))

(export 'tree-view-column-queue-resize)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_tree_view -> tree-view-columun-tree-view
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_get_tree_view" tree-view-column-tree-view)
    (g:object widget)
 #+liber-documentation
 "@version{#2021-2-24}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @begin{return}
    The @class{gtk:tree-view} widet wherein the tree view column has been
    inserted if any, @code{nil} otherwise.
  @end{return}
  @begin{short}
    Returns the tree view wherein the tree view column has been inserted.
  @end{short}
  If the column is currently not inserted in any tree view, @code{nil} is
  returned.
  @see-class{gtk:tree-view-column}
  @see-class{gtk:tree-view}"
  (column (g:object tree-view-column)))

(export 'tree-view-column-tree-view)

;;; --- End of file gtk4.tree-view-column.lisp ---------------------------------
