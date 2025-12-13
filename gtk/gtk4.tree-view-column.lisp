;;; ----------------------------------------------------------------------------
;;; gtk4.tree-view-column.lisp
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

(gobject:define-genum "GtkTreeViewColumnSizing" tree-view-column-sizing
  (:export t
   :type-initializer "gtk_tree_view_column_sizing_get_type")
  (:grow-only 0)
  (:autosize 1)
  (:fixed 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-view-column-sizing)
      "GEnum"
      (liber:symbol-documentation 'tree-view-column-sizing)
 "@version{2025-12-08}
  @begin{declaration}
(gobject:define-genum \"GtkTreeViewColumnSizing\" tree-view-column-sizing
  (:export t
   :type-initializer \"gtk_tree_view_column_sizing_get_type\")
  (:grow-only 0)
  (:autosize 1)
  (:fixed 2))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:grow-only]{Columns only get bigger in reaction to changes in the
        model.}
      @entry[:autosize]{Columns resize to be the optimal size everytime the
        model changes.}
      @entry[:fixed]{Columns are a fixed numbers of pixels wide.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The sizing method the tree view column uses to determine its width.
  @end{short}
  Please note that the @val[gtk:tree-view-column-sizing]{:autosize} value is
  inefficient for large tree views, and can make tree view columns appear
  choppy.
  @begin[Warning]{dictionary}
    This enumeration is deprecated since 4.20. There is no replacement.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-sizing}")

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewColumn
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkTreeViewColumn" tree-view-column
  (:superclass g:initially-unowned
   :export t
   :interfaces ("GtkBuildable"
                "GtkCellLayout")
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

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj tree-view-column) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:TREE-VIEW-COLUMN is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'tree-view-column 'type)
 "@version{2025-07-22}
  @begin{short}
    The @class{gtk:tree-view-column} object represents a visible column in a
    @class{gtk:tree-view} widget.
  @end{short}
  It allows to set properties of the tree view column header, and functions as
  a holding pen for the cell renderers which determine how the data in the
  tree view column is displayed.

  Please refer to the tree view widget conceptual overview for an overview of
  all the objects and data types related to the tree view and how they work
  together.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[tree-view-column::clicked]{signal}
      @begin{pre}
lambda (column)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[column]{The @class{gtk:tree-view-column} object that emitted
          the signal.}
      @end{simple-table}
      Emitted when the header of the column has been clicked.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:tree-view-column-new}
  @see-constructor{gtk:tree-view-column-new-with-area}
  @see-constructor{gtk:tree-view-column-new-with-attributes}
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

;;; --- gtk:tree-view-column-alignment -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "alignment" 'tree-view-column) t)
 "The @code{alignment} property of type @code{:float} (Read / Write) @br{}
  The alignment of the tree view column header text or widget, 0.0 for left,
  0.5 for center, and 1.0 for right alignment. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-alignment)
      "Accessor"
      (documentation 'tree-view-column-alignment 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-alignment object) => align}
  @syntax{(setf (gtk:tree-view-column-alignment object) align)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[align]{a single float for the alignment, which is between 0.0 and
    1.0 inclusive}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{alignment} slot of the
    @class{gtk:tree-view-column} class gets or sets the alignment of the title
    or custom widget inside the tree view column header.
  @end{short}
  The alignment determines the location inside the header button, 0.0 for left,
  0.5 for center, 1.0 for right alignment.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}")

;;; --- gtk:tree-view-column-cell-area -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cell-area" 'tree-view-column) t)
 "The @code{cell-area} property of type @class{gtk:cell-area}
  (Read / Write / Construct) @br{}
  The cell area used to layout cell renderers for this tree view column. If no
  cell area is specified when creating the tree view column a horizontally
  oriented @class{gtk:cell-area-box} object will be used.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-cell-area)
      "Accessor"
      (documentation 'tree-view-column-cell-area 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-cell-area object) => area}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[area]{a @class{gtk:cell-area} object}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{cell-area} slot of the
    @class{gtk:tree-view-column} class returns the cell area used to layout cell
    renderers for this tree view column.
  @end{short}
  If no cell area is specified when creating the tree view column a horizontally
  oriented @class{gtk:cell-area-box} object will be used.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:cell-area}")

;;; --- gtk:tree-view-column-clickable -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "clickable" 'tree-view-column) t)
 "The @code{clickable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the tree view column header can be clicked. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-clickable)
      "Accessor"
      (documentation 'tree-view-column-clickable 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-clickable object) => clickable}
  @syntax{(setf (gtk:tree-view-column-clickable object) clickable)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[clickable]{@em{true} if the tree view column header is active}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{clickable} slot of the
    @class{gtk:tree-view-column} class gets or sets whether the tree view column
    header can be clicked.
  @end{short}
  When the header is active, then it can take keyboard focus, and can be
  clicked.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}")

;;; --- gtk:tree-view-column-expand --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expand" 'tree-view-column) t)
 "The @code{expand} property of type @code{:boolean} (Read / Write) @br{}
  The tree view column gets share of extra width allocated to the widget. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-expand)
      "Accessor"
      (documentation 'tree-view-column-expand 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-expand object) => expand}
  @syntax{(setf (gtk:tree-view-column-expand object) expand)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[expand]{@em{true} if the tree view column should take available
    extra space, @em{false} if not}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{expand} slot of the
    @class{gtk:tree-view-column} class gets or sets whether the tree view column
    gets share of extra width allocated to the widget.
  @end{short}
  This space is shared equally amongst all tree view columns that have the
  expand set to @em{true}. If no column has this option set, then the last
  column gets all extra space. By default, every column is created with this
  @em{false}.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}")

;;; --- gtk:tree-view-column-fixed-width ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "fixed-width"
                                               'tree-view-column) t)
 "The @code{fixed-width} property of type @code{:int} (Read / Write) @br{}
  The current fixed width of the tree view column. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-fixed-width)
      "Accessor"
      (documentation 'tree-view-column-fixed-width 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-fixed-width object) => width}
  @syntax{(setf (gtk:tree-view-column-fixed-width object) width)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[width]{an integer for the size to set the tree view column to,
    must be greater than 0}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{fixed-width} slot of the
    @class{gtk:tree-view-column} class gets or sets the fixed width of the tree
    view column in pixels.
  @end{short}
  This is meaningful only if the @slot[gtk:tree-view-column]{sizing} property
  has the @val[gtk:tree-view-column-sizing]{:fixed} value. The width of the
  tree view column is clamped to the min/max width for the column. Please note
  that the min/max width of the column does not actually affect the
  @slot[gtk:tree-view-column]{fixed-width} property of the widget, just the
  actual width when displayed.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-sizing}
  @see-function{gtk:tree-view-column-max-width}
  @see-function{gtk:tree-view-column-min-width}")

;;; --- gtk:tree-view-column-max-width -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-width" 'tree-view-column) t)
 "The @code{max-width} property of type @code{:int} (Read / Write) @br{}
  The maximum allowed width of the tree view column. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1 @br{}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-max-width)
      "Accessor"
      (documentation 'tree-view-column-max-width 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-max-width object) => width}
  @syntax{(setf (gtk:tree-view-column-max-width object) width)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[width]{an integer for the maximum width of the tree view column
    in pixels, or -1}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{max-width} slot of the
    @class{gtk:tree-view-column} class gets or sets the maximum width in pixels
    of the tree view column.
  @end{short}
  If @arg{width} is -1, then the maximum width is unset. Note, the tree view
  column can actually be wider than max width if it is the last column in a
  tree view. In this case, the column expands to fill any extra space.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-min-width}")

;;; --- gtk:tree-view-column-min-width -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "min-width" 'tree-view-column) t)
 "The @code{min-width} property of type @code{:int} (Read / Write )@br{}
  The minimum allowed width of the tree view column. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-min-width)
      "Accessor"
      (documentation 'tree-view-column-min-width 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-min-width object) => width}
  @syntax{(setf (gtk:tree-view-column-min-width object) width)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[width]{an integer for the minimum width of the tree view column
    in pixels, or -1}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{min-width} slot of the
    @class{gtk:tree-view-column} class gets or sets the minimum width in pixels
    of the tree view column.
  @end{short}
  If @arg{width} is -1, then the minimum width is unset.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-max-width}")

;;; --- gtk:tree-view-column-reorderable ---------------------------------------

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
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-reorderable object) => reorderable}
  @syntax{(setf (gtk:tree-view-column-reorderable object) reorderable)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[reorderable]{@em{true}, if the tree view column can be reordered}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{reorderable} slot of the
    @class{gtk:tree-view-column} class gets or sets whether the tree view column
    can be reordered around the headers.
  @end{short}
  If @code{reorderable} is @em{true}, then the tree view column can be
  reordered by the user dragging the header.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-resizable}")

;;; --- gtk:tree-view-column-resizable -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resizable" 'tree-view-column) t)
 "The @code{resizable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the column is user-resizable. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-resizable)
      "Accessor"
      (documentation 'tree-view-column-resizable 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-resizable object) => resizable}
  @syntax{(setf (gtk:tree-view-column-resizable object) resizable)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[resizable]{@em{true}, if the tree view column can be resized}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{resizable} slot of the
    @class{gtk:tree-view-column} class gets or sets whether the column is user
    resizable.
  @end{short}

  If @code{resizable} is @em{true}, then the user can explicitly resize the
  tree view column by grabbing the outer edge of the column button. If
  @code{resizable} is @em{true} and the sizing mode of the tree view column is
  @val[gtk:tree-view-column-sizing]{:autosize}, then the sizing mode is changed
  to @val[gtk:tree-view-column-sizing]{:grow-only}.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-sizing}
  @see-function{gtk:tree-view-column-reorderable}")

;;; --- gtk:tree-view-column-sizing --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sizing" 'tree-view-column) t)
 "The @code{sizing} property of type @sym{gtk:tree-view-column-sizing}
  (Read / Write) @br{}
  The resize mode of the tree view column. @br{}
  Default value: @val[gtk:tree-view-column-sizing]{:grow-only}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-sizing)
      "Accessor"
      (documentation 'tree-view-column-sizing 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-sizing object) => type}
  @syntax{(setf (gtk:tree-view-column-sizing object) type)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[type]{a @sym{gtk:tree-view-column-sizing} value}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{sizing} slot of the
    @class{gtk:tree-view-column} class gets or sets the resize mode of the tree
    view column.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-resizable}")

;;; --- gtk:tree-view-column-sort-column-id ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sort-column-id"
                                               'tree-view-column) t)
 "The @code{sort-column-id} property of type @code{:int} (Read / Write) @br{}
  The logical sort column ID this tree view column sorts on when selected for
  sorting. Setting the sort column ID makes the tree view column header
  clickable. Set to -1 to make the column unsortable. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-sort-column-id)
      "Accessor"
      (documentation 'tree-view-column-sort-column-id 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-sort-column-id object) => id}
  @syntax{(setf (gtk:tree-view-column-sort-column-id object) id)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[id]{an integer for the sort column ID of the model to sort
    on}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{sort-column-id} slot of the
    @class{gtk:tree-view-column} class gets or sets the logical sort column ID
    that the model sorts on when this tree view column is selected for sorting.
  @end{short}
  Doing so makes the tree view column header clickable.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-sort-indicator}
  @see-function{gtk:tree-view-column-sort-order}")

;;; --- gtk:tree-view-column-sort-indicator ------------------------------------

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
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-sort-indicator object) => setting}
  @syntax{(setf (gtk:tree-view-column-sort-indicator object) setting)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[setting]{@em{true} to display an indicator that the tree view
    column is sorted}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{sort-indicator} slot of the
    @class{gtk:tree-view-column} class gets or sets whether to show a sort
    indicator.
  @end{short}
  Call this function with a setting of @em{true} to display an arrow in the
  header button indicating the tree view column is sorted. Call the
  @fun{gtk:tree-view-column-sort-order} function to change the direction of the
  arrow.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-sort-order}
  @see-function{gtk:tree-view-column-sort-column-id}")

;;; --- gtk:tree-view-column-sort-order ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sort-order"
                                               'tree-view-column) t)
 "The @code{sort-order} property of type @sym{gtk:sort-type} (Read / Write)
  @br{}
  The sort direction the sort indicator should indicate. @br{}
  Default value: @val[gtk:sort-type]{:ascending}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-sort-order)
      "Accessor"
      (documentation 'tree-view-column-sort-order 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-sort-order object) => order}
  @syntax{(setf (gtk:tree-view-column-sort-order object) order)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[order]{a @sym{gtk:sort-type} value}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{sort-order} slot of the
    @class{gtk:tree-view-column} class gets or sets the sort direction the sort
    indicator should indicate.
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
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-symbol{gtk:sort-type}
  @see-function{gtk:tree-view-column-sort-column-id}
  @see-function{gtk:tree-view-column-sort-indicator}
  @see-function{gtk:tree-sortable-sort-column-id}")

;;; --- gtk:tree-view-column-spacing -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "spacing" 'tree-view-column) t)
 "The @code{spacing} property of type @code{:int} (Read / Write) @br{}
  The space which is inserted between cell renderers. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-spacing)
      "Accessor"
      (documentation 'tree-view-column-spacing 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-spacing object) => spacing}
  @syntax{(setf (gtk:tree-view-column-spacing object) spacing)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[spacing]{an integer for the distance between cell renderers in
    pixels}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{spacing} slot of the
    @class{gtk:tree-view-column} class gets or sets the spacing of the tree view
    column, which is the number of pixels to place between cell renderers packed
    into it.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}")

;;; --- gtk:tree-view-column-title ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'tree-view-column) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title to appear in the tree view column header. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-title)
      "Accessor"
      (documentation 'tree-view-column-title 'function)
 "@version{2025-05-21}
  @syntax{(gtk:tree-view-column-title object) => title}
  @syntax{(setf (gtk:tree-view-column-title object) title)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[title]{a string for the title of the tree view column}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{title} slot of the
    @class{gtk:tree-view-column} class gets or sets the title of the tree view
    column.
  @end{short}
  If a custom widget has been set, then this value is ignored.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:tree-view-column-widget}")

;;; --- gtk:tree-view-column-visible -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible" 'tree-view-column) t)
 "The @code{visible} property of type @code{:boolean} (Read / Write) @br{}
  Whether to display the tree view column. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-visible)
      "Accessor"
      (documentation 'tree-view-column-visible 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-visible object) => visible}
  @syntax{(setf (gtk:tree-view-column-visible object) visible)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[visible]{@em{true} if the tree view column is visible}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{visible} slot of the
    @class{gtk:tree-view-column} class gets or sets whether the tree view
    column is visible or not.
  @end{short}
  If it is visible, then the tree will show the column.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}")

;;; --- gtk:tree-view-column-widget --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "widget" 'tree-view-column) t)
 "The @code{widget} property of type @class{gtk:widget} (Read / Write) @br{}
  The widget to put in the tree view column header button instead of column
  title.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-widget)
      "Accessor"
      (documentation 'tree-view-column-widget 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-widget object) => widget}
  @syntax{(setf (gtk:tree-view-column-widget object) widget)}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[widget]{a @class{gtk:widget} child widget, or @code{nil}}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{widget} slot of the
    @class{gtk:tree-view-column} class gets or sets the child widget in the
    button on the tree view column header.
  @end{short}
  If a custom widget has not been set then @code{nil} is returned. If the child
  widget is @code{nil}, then the header button is set with a @class{gtk:label}
  widget set to the title of the tree view column.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:widget}
  @see-class{gtk:label}
  @see-function{gtk:tree-view-column-title}")

;;; --- gtk:tree-view-column-width ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width" 'tree-view-column) t)
 "The @code{width} property of type @code{:int} (Read) @br{}
  The current width of the tree view column. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-width)
      "Accessor"
      (documentation 'tree-view-column-width 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-width object) => width}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[width]{an integer for the current width of the tree view column}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{width} slot of the
    @class{gtk:tree-view-column} class returns the current size of the tree
    view column in pixels.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}")

;;; --- gtk:tree-view-column-x-offset ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "x-offset" 'tree-view-column) t)
 "The @code{x-offset} property of type @code{:int} (Read) @br{}
  The current x position of the tree view column. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'tree-view-column-x-offset)
      "Accessor"
      (documentation 'tree-view-column-x-offset 'function)
 "@version{2025-09-21}
  @syntax{(gtk:tree-view-column-x-offset object) => offset}
  @argument[object]{a @class{gtk:tree-view-column} object}
  @argument[offset]{an integer for the current x offset in pixels}
  @begin{short}
    The accessor for the @slot[gtk:tree-view-column]{x-offset} slot of the
    @class{gtk:tree-view-column} class returns the current x offset of the tree
    view column in pixels.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_new
;;; ----------------------------------------------------------------------------

(defun tree-view-column-new ()
 #+liber-documentation
 "@version{2024-05-03}
  @return{The newly created @class{gtk:tree-view-column} object.}
  @begin{short}
    Creates a new tree view column.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
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
 "@version{2024-03-08}
  @argument[area]{a @class{gtk:cell-area} object that the newly created
   tree view column should use to layout cell renderers}
  @return{The newly created @class{gtk:tree-view-column} object.}
  @begin{short}
    Creates a new tree view column using @arg{area} to render its cells.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
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

(defun tree-view-column-new-with-attributes (title renderer &rest attributes)
 #+liber-documentation
 "@version{2025-04-15}
  @argument[title]{a string for the title to set the header to}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[attributes]{pairs of attributes}
  @return{The newly created @class{gtk:tree-view-column} object.}
  @begin{short}
    Creates a new tree view column with a number of default values.
  @end{short}
  This is equivalent to calling the @fun{gtk:tree-view-column-title},
  @fun{gtk:tree-view-column-pack-start}, and
  @fun{gtk:tree-view-column-set-attributes} functions on the newly created
  @class{gtk:tree-view-column} object.
  @begin[Examples]{dictionary}
  @begin{pre}
(let* ((renderer (gtk:cell-renderer-text-new))
       (column (gtk:tree-view-column-new-with-attributes \"Example\"
                                                         renderer
                                                         \"text\" 0
                                                         \"foreground\" 1)))
  ... )
  @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
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
 "@version{#2024-03-08}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[expand]{@em{true} if the cell renderer is to be given extra space
    allocated to the tree view column}
  @begin{short}
    Packs the cell renderer into the beginning of the tree view column.
  @end{short}
  If the @arg{expand} argument is @em{false}, then the cell renderer is
  allocated no more space than it needs. Any unused space is divided evenly
  between cell renderers for which the @arg{expand} argument is @em{true}.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
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
 "@version{#2024-03-08}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[expand]{@em{true} if the cell renderer is to be given extra space
    allocated to the tree view column}
  @begin{short}
    Packs the cell renderer to the end of the tree view column.
  @end{short}
  If the @arg{expand} argument is @em{false}, then the cell renderer is
  allocated no more space than it needs. Any unused space is divided evenly
  between cell renderers for which the @code{expand} argument is @em{true}.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
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
 "@version{#2024-03-08}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @begin{short}
    Unsets all the mappings on all cell renderers on the tree view column.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}"
  (column (g:object tree-view-column)))

(export 'tree-view-column-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_add_attribute
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_add_attribute"
               tree-view-column-add-attribute) :void
 #+liber-documentation
 "@version{#2025-04-15}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object to set attributes on}
  @argument[attribute]{a string for an attribute on the cell renderer}
  @argument[position]{an integer for the column position on the model}
  @begin{short}
    Adds an attribute mapping to the list in the tree view column.
  @end{short}
  The @arg{column} argument is the column of the model to get a value from, and
  the attribute is the parameter on the cell renderer to be set from the value.
  So for example if column 2 of the model contains strings, you could have the
  @code{\"text\"} attribute of a @class{gtk:cell-renderer-text} object get its
  values from column 2.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
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
 "@version{#2024-05-04}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object we are setting the
    attributes of}
  @argument[attributes]{pairs of attributes}
  @begin{short}
    Sets the attributes in the list as the attributes of the tree view column.
  @end{short}
  The attributes should be in attribute/column order, as in the
  @fun{gtk:tree-view-column-add-attribute} function. All existing attributes are
  removed, and replaced with the new attributes.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:tree-view-column-add-attribute}"
  (iter (for (attribute col) on attributes by #'cddr)
        (tree-view-column-add-attribute column renderer attribute col)))

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
      (return-from-tree-cell-data-func () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'tree-cell-data-func)
      "Callback"
      (liber:symbol-documentation 'tree-cell-data-func)
 "@version{#2025-12-08}
  @syntax{lambda (column renderer model iter)}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object that is being rendered
    by the tree view column}
  @argument[model]{a @class{gtk:tree-model} object being rendered}
  @argument[iter]{a @class{gtk:tree-iter} object for the current row rendered}
  @begin{short}
    A function to set the properties of a cell instead of just using the
    straight mapping between the cell and the model.
  @end{short}
  This is useful for customizing the cell renderer. For example, a function
  might get an integer from the tree model, and render it to the @code{\"text\"}
  attribute of the cell by converting it to its written equivalent. This is set
  by calling the @fun{gtk:tree-view-column-set-cell-data-func} function.
  @begin[Warning]{dictionary}
    This callback function is deprecated since 4.20. There is no replacement.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:cell-renderer}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}
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
 "@version{#2025-07-22}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[func]{a @sym{gtk:tree-cell-data-func} callback function to use}
  @begin{short}
    Sets the callback function to use for the tree view column.
  @end{short}
  This function is used instead of the standard attributes mapping for setting
  the column value, and should set the value of the tree view column cell
  renderer as appropriate. The @arg{func} argument may be @code{nil} to remove
  an older one.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:cell-renderer}
  @see-symbol{gtk:tree-cell-data-func}"
  (if func
      (%tree-view-column-set-cell-data-func
              column
              renderer
              (cffi:callback tree-cell-data-func)
              (glib:allocate-stable-pointer func)
              (cffi:callback glib:stable-pointer-destroy-notify))
      (%tree-view-column-set-cell-data-func
              column
              renderer
              (cffi:null-pointer)
              (cffi:null-pointer)
              (cffi:null-pointer))))

(export 'tree-view-column-set-cell-data-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_clear_attributes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_clear_attributes"
               tree-view-column-clear-attributes) :void
 #+liber-documentation
 "@version{#2024-03-08}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object to clear the attribute
    mapping on}
  @begin{short}
    Clears all existing attributes previously set, for example, with the
    @fun{gtk:tree-view-column-set-attributes} function.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
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
 "@version{#2025-07-22}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @begin{short}
    Emits the @sig[gtk:tree-view-column]{clicked} signal on the tree view
    column.
  @end{short}
  This function will only work if the tree view column is clickable.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}"
  (column (g:object tree-view-column)))

(export 'tree-view-column-clicked)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_button
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_get_button" tree-view-column-button)
    (g:object widget)
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @begin{return}
    The @class{gtk:widget} object for the button for the tree view column
    header.
  @end{return}
  @begin{short}
    Returns the button used in the tree view column header.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:widget}"
  (column (g:object tree-view-column)))

(export 'tree-view-column-button)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_set_cell_data
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_cell_set_cell_data"
               tree-view-column-cell-set-cell-data) :void
 #+liber-documentation
 "@version{#2024-03-08}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[model]{a @class{gtk:tree-model} to to get the cell renderer's
    attributes from}
  @argument[iter]{a @class{gtk:tree-iter} to to get the cell renderer's
    attributes from}
  @argument[expander]{@em{true}, if the row has children}
  @argument[expanded]{@em{true}, if the row has visible children}
  @begin{short}
    Sets the cell renderer based on the tree model and @arg{iter}.
  @end{short}
  That is, for every attribute mapping in the tree view column, it will get a
  value from the column on the @arg{iter}, and use that value to set the
  attribute on the cell renderer. This is used primarily by the tree view.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}"
  (column (g:object tree-view-column))
  (model (g:object tree-model))
  (iter (g:boxed tree-iter))
  (expander :boolean)
  (expanded :boolean))

(export 'tree-view-column-cell-set-cell-data)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_get_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_cell_get_size" %tree-view-column-cell-size)
    :void
  (column (g:object tree-view-column))
  (xoffset (:pointer :int))
  (yoffset (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun tree-view-column-cell-size (column)
 #+liber-documentation
 "@version{2025-04-15}
  @syntax{(gtk:tree-view-column-cell-size column) => xoffset, yoffset, width,
    height}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[xoffset]{an integer for the x offset of a cell relative to
    @arg{area}}
  @argument[yoffset]{an integer for the y offset of a cell relative to
    @arg{area}}
  @argument[width]{an integer for the width needed to render a cell}
  @argument[height]{an integer for the height needed to render a cell}
  @begin{short}
    Obtains the width and height needed to render the column.
  @end{short}
  This is used primarily by the tree view.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-class{gdk:rectangle}"
  (cffi:with-foreign-objects ((xoffset :int)
                              (yoffset :int)
                              (width :int)
                              (height :int))
    (%tree-view-column-cell-size column xoffset yoffset width height)
    (values (cffi:mem-ref xoffset :int)
            (cffi:mem-ref yoffset :int)
            (cffi:mem-ref width :int)
            (cffi:mem-ref height :int))))

(export 'tree-view-column-cell-size)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_get_position
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_cell_get_position"
               %tree-view-column-cell-position) :boolean
  (column (g:object tree-view-column))
  (renderer (g:object cell-renderer))
  (offset (:pointer :int))
  (width (:pointer :int)))

(defun tree-view-column-cell-position (column renderer)
 #+liber-documentation
 "@version{#2025-04-15}
  @syntax{(gtk:tree-view-column-cell-position column renderer) => offset, width}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[offset]{an integer for the horizontal position of the cell within
    the tree view column}
  @argument[width]{an integer for the width of the cell}
  @begin{short}
    Obtains the horizontal position and width of a cell in a tree view column.
  @end{short}
  If the cell is not found in the column, @code{nil} is returned.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:cell-renderer}"
  (cffi:with-foreign-objects ((offset :int) (width :int))
    (when (%tree-view-column-cell-position column renderer offset width)
      (values (cffi:mem-ref offset :int)
              (cffi:mem-ref width :int)))))

(export 'tree-view-column-cell-position)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_is_visible
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_cell_is_visible"
               tree-view-column-cell-is-visible) :boolean
 #+liber-documentation
 "@version{#2024-03-08}
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
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
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
 "@version{#2024-03-08}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @begin{short}
    Sets the current keyboard focus to be at @arg{renderer}, if the column
    contains 2 or more editable and activatable cells.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
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
 "@version{#2024-03-08}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @begin{short}
    Flags the tree view column, and the cell renderers added to this column, to
    have their sizes renegotiated.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}"
  (column (g:object tree-view-column)))

(export 'tree-view-column-queue-resize)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_tree_view
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_view_column_get_tree_view" tree-view-column-tree-view)
    (g:object widget)
 #+liber-documentation
 "@version{#2024-03-08}
  @argument[column]{a @class{gtk:tree-view-column} object}
  @begin{return}
    The @class{gtk:tree-view} widget wherein the tree view column has been
    inserted if any, @code{nil} otherwise.
  @end{return}
  @begin{short}
    Returns the tree view wherein the tree view column has been inserted.
  @end{short}
  If the column is currently not inserted in any tree view, @code{nil} is
  returned.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-view-column} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:tree-view}"
  (column (g:object tree-view-column)))

(export 'tree-view-column-tree-view)

;;; --- End of file gtk4.tree-view-column.lisp ---------------------------------
