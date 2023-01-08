;;; ----------------------------------------------------------------------------
;;; gtk.grid-layout.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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
;;; GtkGridLayout
;;;
;;;     GtkGridLayout — Layout manager for grid-like widgets
;;;
;;; Types and Values
;;;
;;;     GtkGridLayout
;;;     GtkGridLayoutChild
;;;
;;; Accessors
;;;
;;;     gtk_grid_layout_set_baseline_row
;;;     gtk_grid_layout_get_baseline_row
;;;     gtk_grid_layout_set_column_homogeneous
;;;     gtk_grid_layout_get_column_homogeneous
;;;     gtk_grid_layout_set_column_spacing
;;;     gtk_grid_layout_get_column_spacing
;;;     gtk_grid_layout_set_row_homogeneous
;;;     gtk_grid_layout_get_row_homogeneous
;;;     gtk_grid_layout_set_row_spacing
;;;     gtk_grid_layout_get_row_spacing
;;;
;;;     gtk_grid_layout_child_set_column
;;;     gtk_grid_layout_child_get_column
;;;     gtk_grid_layout_child_set_column_span
;;;     gtk_grid_layout_child_get_column_span
;;;     gtk_grid_layout_child_set_row
;;;     gtk_grid_layout_child_get_row
;;;     gtk_grid_layout_child_set_row_span
;;;     gtk_grid_layout_child_get_row_span
;;;
;;; Functions
;;;
;;;     gtk_grid_layout_new
;;;     gtk_grid_layout_set_row_baseline_position
;;;     gtk_grid_layout_get_row_baseline_position
;;;
;;; Properties
;;;
;;;     baseline-row
;;;     column-homogeneous
;;;     column-spacing
;;;     row-homogeneous
;;;     row-spacing
;;;
;;;     column
;;;     column-span
;;;     row
;;;     row-span
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GtkLayoutChild
;;;     │   ╰── GtkGridLayoutChild
;;;     ╰── GtkLayoutManager
;;;         ╰── GtkGridLayout
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkGridLayoutChild
;;;
;;; Layout properties for children of GtkGridLayout.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGridLayoutChild" grid-layout-child
  (:superclass layout-child
   :export t
   :interfaces ()
   :type-initializer "gtk_grid_layout_child_get_type")
  ((column
    grid-layout-child-column
    "column" "gint" t t)
   (column-span
    grid-layout-child-column-span
    "column-span" "gint" t t)
   (row
    grid-layout-child-row
    "row" "gint" t t)
   (row-span
    grid-layout-child-row-span
    "row-span" "gint" t t)))

;;; ----------------------------------------------------------------------------
;;; GtkGridLayout
;;;
;;; Layout manager for grid-like widgets.
;;;
;;; GtkGridLayout is a layout manager which arranges child widgets in rows and
;;; columns, with arbitrary positions and horizontal/vertical spans.
;;;
;;; Children have an "attach point" defined by the horizontal and vertical index
;;; of the cell they occupy; children can span multiple rows or columns. The
;;; layout properties for setting the attach points and spans are set using the
;;; GtkGridLayoutChild associated to each child widget.
;;;
;;; The behaviour of GtkGrid when several children occupy the same grid cell is
;;; undefined.
;;;
;;; GtkGridLayout can be used like a GtkBoxLayout if all children are attached
;;; to the same row or column; however, if you only ever need a single row or
;;; column, you should consider using GtkBoxLayout.
;;;
;;; See Also
;;;
;;;     GtkBoxLayout
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGridLayout" grid-layout
  (:superclass layout-manager
   :export t
   :interfaces ()
   :type-initializer "gtk_grid_layout_get_type")
  ((baseline-row
    grid-layout-baseline-row
    "baseline-row" "gint" t t)
   (column-homogeneous
    grid-layout-column-homogeneous
    "column-homogeneous" "gboolean" t t)
   (column-spacing
    grid-layout-column-spacing
    "column-spacing" "gint" t t)
   (row-homogeneous
    grid-layout-row-homogeneous
    "row-homogeneous" "gboolean" t t)
   (row-spacing
    grid-layout-row-spacing
    "row-spacing" "gint" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “baseline-row” property
;;;  “baseline-row”             int
;;;The row to align to the baseline, when “valign” is set to GTK_ALIGN_BASELINE.

;;;Owner: GtkGridLayout

;;;Flags: Read / Write

;;;Allowed values: >= 0

;;;Default value: 0

;;;The “column-homogeneous” property
;;;  “column-homogeneous”       gboolean
;;;Whether all the columns in the grid have the same width.

;;;Owner: GtkGridLayout

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “column-spacing” property
;;;  “column-spacing”           int
;;;The amount of space between to consecutive columns.

;;;Owner: GtkGridLayout

;;;Flags: Read / Write

;;;Allowed values: [0,32767]

;;;Default value: 0

;;;The “row-homogeneous” property
;;;  “row-homogeneous”          gboolean
;;;Whether all the rows in the grid have the same height.

;;;Owner: GtkGridLayout

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “row-spacing” property
;;;  “row-spacing”              int
;;;The amount of space between to consecutive rows.

;;;Owner: GtkGridLayout

;;;Flags: Read / Write

;;;Allowed values: [0,32767]

;;;Default value: 0

;;;The “column” property
;;;  “column”                   int
;;;The column to place the child in.

;;;Owner: GtkGridLayoutChild

;;;Flags: Read / Write

;;;Default value: 0

;;;The “column-span” property
;;;  “column-span”              int
;;;The number of columns the child spans to.

;;;Owner: GtkGridLayoutChild

;;;Flags: Read / Write

;;;Allowed values: >= 1

;;;Default value: 1

;;;The “row” property
;;;  “row”                      int
;;;The row to place the child in.

;;;Owner: GtkGridLayoutChild

;;;Flags: Read / Write

;;;Default value: 0

;;;The “row-span” property
;;;  “row-span”                 int
;;;The number of rows the child spans to.

;;;Owner: GtkGridLayoutChild

;;;Flags: Read / Write

;;;Allowed values: >= 1

;;;Default value: 1




;;;Functions
;;;gtk_grid_layout_new ()
;;;GtkLayoutManager *
;;;gtk_grid_layout_new (void);
;;;Creates a new GtkGridLayout.

;;;Returns
;;;the newly created GtkGridLayout

;;;gtk_grid_layout_set_row_homogeneous ()
;;;void
;;;gtk_grid_layout_set_row_homogeneous (GtkGridLayout *grid,
;;;                                     gboolean homogeneous);
;;;Sets whether all rows of grid should have the same height.

;;;Parameters
;;;grid

;;;a GtkGridLayout

;;;homogeneous

;;;TRUE to make rows homogeneous

;;;gtk_grid_layout_get_row_homogeneous ()
;;;gboolean
;;;gtk_grid_layout_get_row_homogeneous (GtkGridLayout *grid);
;;;Checks whether all rows of grid should have the same height.

;;;Parameters
;;;grid

;;;a GtkGridLayout

;;;Returns
;;;TRUE if the rows are homogeneous, and FALSE otherwise

;;;gtk_grid_layout_set_row_spacing ()
;;;void
;;;gtk_grid_layout_set_row_spacing (GtkGridLayout *grid,
;;;                                 guint spacing);
;;;Sets the amount of space to insert between consecutive rows.

;;;Parameters
;;;grid

;;;a GtkGridLayout

;;;spacing

;;;the amount of space between rows, in pixels

;;;gtk_grid_layout_get_row_spacing ()
;;;guint
;;;gtk_grid_layout_get_row_spacing (GtkGridLayout *grid);
;;;Retrieves the spacing set with gtk_grid_layout_set_row_spacing().

;;;Parameters
;;;grid

;;;a GtkGridLayout

;;;Returns
;;;the spacing between consecutive rows

;;;gtk_grid_layout_set_column_homogeneous ()
;;;void
;;;gtk_grid_layout_set_column_homogeneous
;;;                               (GtkGridLayout *grid,
;;;                                gboolean homogeneous);
;;;Sets whether all columns of grid should have the same width.

;;;Parameters
;;;grid

;;;a GtkGridLayout

;;;homogeneous

;;;TRUE to make columns homogeneous

;;;gtk_grid_layout_get_column_homogeneous ()
;;;gboolean
;;;gtk_grid_layout_get_column_homogeneous
;;;                               (GtkGridLayout *grid);
;;;Checks whether all columns of grid should have the same width.

;;;Parameters
;;;grid

;;;a GtkGridLayout

;;;Returns
;;;TRUE if the columns are homogeneous, and FALSE otherwise

;;;gtk_grid_layout_set_column_spacing ()
;;;void
;;;gtk_grid_layout_set_column_spacing (GtkGridLayout *grid,
;;;                                    guint spacing);
;;;Sets the amount of space to insert between consecutive columns.

;;;Parameters
;;;grid

;;;a GtkGridLayout

;;;spacing

;;;the amount of space between columns, in pixels

;;;gtk_grid_layout_get_column_spacing ()
;;;guint
;;;gtk_grid_layout_get_column_spacing (GtkGridLayout *grid);
;;;Retrieves the spacing set with gtk_grid_layout_set_column_spacing().

;;;Parameters
;;;grid

;;;a GtkGridLayout

;;;Returns
;;;the spacing between consecutive columns

;;;gtk_grid_layout_set_row_baseline_position ()
;;;void
;;;gtk_grid_layout_set_row_baseline_position
;;;                               (GtkGridLayout *grid,
;;;                                int row,
;;;                                GtkBaselinePosition pos);
;;;Sets how the baseline should be positioned on row of the grid, in case that row is assigned more space than is requested.

;;;Parameters
;;;grid

;;;a GtkGridLayout

;;;row

;;;a row index

;;;pos

;;;a GtkBaselinePosition

;;;gtk_grid_layout_get_row_baseline_position ()
;;;GtkBaselinePosition
;;;gtk_grid_layout_get_row_baseline_position
;;;                               (GtkGridLayout *grid,
;;;                                int row);
;;;Returns the baseline position of row as set by gtk_grid_layout_set_row_baseline_position(), or the default value of GTK_BASELINE_POSITION_CENTER.

;;;Parameters
;;;grid

;;;a GtkGridLayout

;;;row

;;;a row index

;;;Returns
;;;the baseline position of row

;;;gtk_grid_layout_set_baseline_row ()
;;;void
;;;gtk_grid_layout_set_baseline_row (GtkGridLayout *grid,
;;;                                  int row);
;;;Sets which row defines the global baseline for the entire grid.

;;;Each row in the grid can have its own local baseline, but only one of those is global, meaning it will be the baseline in the parent of the grid .

;;;Parameters
;;;grid

;;;a GtkGridLayout

;;;row

;;;the row index

;;;gtk_grid_layout_get_baseline_row ()
;;;int
;;;gtk_grid_layout_get_baseline_row (GtkGridLayout *grid);
;;;Retrieves the row set with gtk_grid_layout_set_baseline_row().

;;;Parameters
;;;grid

;;;a GtkGridLayout

;;;Returns
;;;the global baseline row

;;;gtk_grid_layout_child_set_column ()
;;;void
;;;gtk_grid_layout_child_set_column (GtkGridLayoutChild *child,
;;;                                  int column);
;;;Sets the column number to attach the left side of child .

;;;Parameters
;;;child

;;;a GtkGridLayoutChild

;;;column

;;;the attach point for child

;;;gtk_grid_layout_child_get_column ()
;;;int
;;;gtk_grid_layout_child_get_column (GtkGridLayoutChild *child);
;;;Retrieves the column number to which child attaches its left side.

;;;Parameters
;;;child

;;;a GtkGridLayoutChild

;;;Returns
;;;the column number

;;;gtk_grid_layout_child_set_row ()
;;;void
;;;gtk_grid_layout_child_set_row (GtkGridLayoutChild *child,
;;;                               int row);
;;;Sets the row to place child in.

;;;Parameters
;;;child

;;;a GtkGridLayoutChild

;;;row

;;;the row for child

;;;gtk_grid_layout_child_get_row ()
;;;int
;;;gtk_grid_layout_child_get_row (GtkGridLayoutChild *child);
;;;Retrieves the row number to which child attaches its top side.

;;;Parameters
;;;child

;;;a GtkGridLayoutChild

;;;Returns
;;;the row number

;;;gtk_grid_layout_child_set_column_span ()
;;;void
;;;gtk_grid_layout_child_set_column_span (GtkGridLayoutChild *child,
;;;                                       int span);
;;;Sets the number of columns child spans to.

;;;Parameters
;;;child

;;;a GtkGridLayoutChild

;;;span

;;;the span of child

;;;gtk_grid_layout_child_get_column_span ()
;;;int
;;;gtk_grid_layout_child_get_column_span (GtkGridLayoutChild *child);
;;;Retrieves the number of columns that child spans to.

;;;Parameters
;;;child

;;;a GtkGridLayoutChild

;;;Returns
;;;the number of columns

;;;gtk_grid_layout_child_set_row_span ()
;;;void
;;;gtk_grid_layout_child_set_row_span (GtkGridLayoutChild *child,
;;;                                    int span);
;;;Sets the number of rows child spans to.

;;;Parameters
;;;child

;;;a GtkGridLayoutChild

;;;span

;;;the span of child

;;;gtk_grid_layout_child_get_row_span ()
;;;int
;;;gtk_grid_layout_child_get_row_span (GtkGridLayoutChild *child);
;;;Retrieves the number of rows that child spans to.

;;;Parameters
;;;child

;;;a GtkGridLayoutChild

;;;Returns
;;;the number of row

;;; --- End of file gtk.grid-layout.lisp ---------------------------------------
