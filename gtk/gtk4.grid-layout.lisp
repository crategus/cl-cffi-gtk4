;;; ----------------------------------------------------------------------------
;;; gtk4.grid-layout.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkGridLayoutChild" grid-layout-child
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

#+liber-documentation
(setf (documentation 'grid-layout-child 'type)
 "@version{2024-04-23}
  @begin{short}
    The @class{gtk:layout-child} subclass for children in a
    @class{gtk:grid-layout} object.
  @end{short}
  @see-slot{gtk:grid-layout-child-column}
  @see-slot{gtk:grid-layout-child-column-span}
  @see-slot{gtk:grid-layout-child-row}
  @see-slot{gtk:grid-layout-child-row-span}
  @see-class{gtk:grid-layout}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:grid-layout-child-column -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "column" 'grid-layout-child) t)
 "The @code{column} property of type @code{:int} (Read / Write) @br{}
  The column to place the child widget in. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'grid-layout-child-column)
      "Accessor"
      (documentation 'grid-layout-child-column 'function)
 "@version{#2025-06-30}
  @syntax{(gtk:grid-layout-child-column object) => column}
  @syntax{(setf (gtk:grid-layout-child-column object) column)}
  @argument[object]{a @class{gtk:grid-layout-child} object}
  @argument[column]{an integer for the column to place the child widget in}
  @begin{short}
    Accessor of the @slot[gtk:grid-layout-child]{column} slot of the
    @class{gtk:grid-layout-child} class.
  @end{short}
  The @fun{gtk:grid-layout-child-column} function retrieves the column number
  to which child attaches its left side. The
  @setf{gtk:grid-layout-child-column} function sets the column number.
  @see-class{gtk:grid-layout-child}")

;;; --- gtk:grid-layout-child-column-span --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "column-span"
                                               'grid-layout-child) t)
 "The @code{column-span} property of type @code{:int} (Read / Write) @br{}
  The number of columns the child spans to. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'grid-layout-child-column-span)
      "Accessor"
      (documentation 'grid-layout-child-column-span 'function)
 "@version{#2025-06-30}
  @syntax{(gtk:grid-layout-child-column-span object) => span}
  @syntax{(setf (gtk:grid-layout-child-column-span object) span)}
  @argument[object]{a @class{gtk:grid-layout-child} object}
  @argument[span]{an integer for the number of columns}
  @begin{short}
    Accessor of the @slot[gtk:grid-layout-child]{column-span} slot of the
    @class{gtk:grid-layout-child} class.
  @end{short}
  The @fun{gtk:grid-layout-child-column-span} function retrieves the number of
  columns that the child widget spans to. The
  @setf{gtk:grid-layout-child-column-span} function sets the number of columns.
  @see-class{gtk:grid-layout-child}")

;;; --- gtk:grid-layout-child-row ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "row" 'grid-layout-child) t)
 "The @code{row} property of type @code{:int} (Read / Write) @br{}
  The row to place the child widget in. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'grid-layout-child-row)
      "Accessor"
      (documentation 'grid-layout-child-row 'function)
 "@version{#2025-06-30}
  @syntax{(gtk:grid-layout-child-row object) => row}
  @syntax{(setf (gtk:grid-layout-child-column object) row)}
  @argument[object]{a @class{gtk:grid-layout-child} object}
  @argument[row]{an integer for the row to place the child widget in}
  @begin{short}
    Accessor of the @slot[gtk:grid-layout-child]{row} slot of the
    @class{gtk:grid-layout-child} class.
  @end{short}
  The @fun{gtk:grid-layout-child-row} function retrieves the row number to
  which child attaches its top side. The
  @setf{gtk:grid-layout-child-row} function sets the row number.
  @see-class{gtk:grid-layout-child}")

;;; --- gtk:grid-layout-child-row-span -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "row-span" 'grid-layout-child) t)
 "The @code{row-span} property of type @code{:int} (Read / Write) @br{}
  The number of rows the child widget spans to. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'grid-layout-child-row-span)
      "Accessor"
      (documentation 'grid-layout-child-row-span 'function)
 "@version{#2025-06-30}
  @syntax{(gtk:grid-layout-child-row-span object) => span}
  @syntax{(setf (gtk:grid-layout-child-row-span object) span)}
  @argument[object]{a @class{gtk:grid-layout-child} object}
  @argument[span]{an integer for the number of rows}
  @begin{short}
    Accessor of the @slot[gtk:grid-layout-child]{row-span} slot of the
    @class{gtk:grid-layout-child} class.
  @end{short}
  The @fun{gtk:grid-layout-child-row-span} function retrieves the number of
  rows that the child widget spans to. The @setf{gtk:grid-layout-child-row-span}
  function sets the number of rows.
  @see-class{gtk:grid-layout-child}")

;;; ----------------------------------------------------------------------------
;;; GtkGridLayout
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkGridLayout" grid-layout
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

#+liber-documentation
(setf (documentation 'grid-layout 'type)
 "@version{2024-04-23}
  @begin{short}
    Layout manager for grid like widgets.
  @end{short}
  The @class{gtk:grid-layout} object is a layout manager which arranges child
  widgets in rows and columns, with arbitrary positions and horizontal/vertical
  spans.

  Children have an \"attach point\" defined by the horizontal and vertical index
  of the cell they occupy. Children can span multiple rows or columns. The
  layout properties for setting the attach points and spans are set using the
  @class{gtk:grid-layout-child} object associated to each child widget.

  The behaviour of the @class{gtk:grid} widget when several children occupy the
  same grid cell is undefined.

  The @class{gtk:grid-layout} object can be used like a @class{gtk:box-layout}
  object if all children are attached to the same row or column. However, if
  you only ever need a single row or column, you should consider using the
  @class{gtk:box-layout} object.
  @see-constructor{gtk:grid-layout-new}
  @see-slot{gtk:grid-layout-baseline-row}
  @see-slot{gtk:grid-layout-column-homogeneous}
  @see-slot{gtk:grid-layout-column-spacing}
  @see-slot{gtk:grid-layout-row-homogeneous}
  @see-slot{gtk:grid-layout-row-spacing}
  @see-class{gtk:box-layout}
  @see-class{gtk:grid-layout-child}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:grid-layout-baseline-row -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "baseline-row" 'grid-layout) t)
 "The @code{baseline-row} property of type @code{:int} (Read / Write) @br{}
  The row to align to the baseline, when the @slot[gtk:widget]{valign} property
  is set to @val[gtk:align]{:baseline}. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'grid-layout-baseline-row)
      "Accessor"
      (documentation 'grid-layout-baseline-row 'function)
 "@version{2025-06-30}
  @syntax{(gtk:grid-layout-baseline-row object) => row}
  @syntax{(setf (gtk:grid-layout-baseline-row object) row)}
  @argument[object]{a @class{gtk:grid-layout} object}
  @argument[row]{an integer for the row index}
  @begin{short}
    Accessor of the @slot[gtk:grid-layout]{baseline-row} slot of the
    @class{gtk:grid-layout} class.
  @end{short}
  The @fun{gtk:grid-layout-baseline-row} function retrieves the baseline row.
  The @setf{gtk:grid-layout-baseline-row} function sets which row defines the
  global baseline for the entire grid.

  Each row in the grid can have its own local baseline, but only one of those
  is global, meaning it will be the baseline in the parent of the grid widget.
  @see-class{gtk:grid-layout}")

;;; --- gtk:grid-layout-column-homogeneous -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "column-homogeneous"
                                               'grid-layout) t)
 "The @code{column-homogeneous} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether all the columns in the grid have the same width. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'grid-layout-column-homogeneous)
      "Accessor"
      (documentation 'grid-layout-column-homogeneous 'function)
 "@version{2024-04-23}
  @syntax{(gtk:grid-layout-column-homogeneous object) => homogeneous}
  @syntax{(setf (gtk:grid-layout-column-homogeneous object) homogeneous)}
  @argument[object]{a @class{gtk:grid-layout} object}
  @argument[homogeneous]{a boolean whether all the columns in the grid have the
    same width}
  @begin{short}
    Accessor of the @slot[gtk:grid-layout]{column-homogeneous} slot of the
    @class{gtk:grid-layout} class.
  @end{short}
  The @fun{gtk:grid-layout-column-homogeneous} function checks whether all
  columns of the grid should have the same width. The
  @setf{gtk:grid-layout-column-homogeneous} function sets whether all columns
  of the grid should have the same width.
  @see-class{gtk:grid-layout}")

;;; --- gtk:grid-layout-column-spacing -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "column-spacing" 'grid-layout) t)
 "The @code{column-spacing} property of type @code{:int} (Read / Write) @br{}
  The amount of space between to consecutive columns. @br{}
  Allowed values: [0,32767] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'grid-layout-column-spacing)
      "Accessor"
      (documentation 'grid-layout-column-spacing 'function)
 "@version{2025-06-30}
  @syntax{(gtk:grid-layout-column-spacing object) => spacing}
  @syntax{(setf (gtk:grid-layout-column-spacing object) spacing)}
  @argument[object]{a @class{gtk:grid-layout} object}
  @argument[spacing]{an integer for the amount of space between to consecutive
    columns}
  @begin{short}
    Accessor of the @slot[gtk:grid-layout]{column-spacing} slot of the
    @class{gtk:grid-layout} class.
  @end{short}
  The @fun{gtk:grid-layout-column-spacing} function retrieves the amount of
  space to insert between consecutive columns. The
  @setf{gtk:grid-layout-column-spacing} function sets the spacing.
  @see-class{gtk:grid-layout}")

;;; --- gtk:grid-layout-row-homogeneous ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "row-homogeneous"
                                               'grid-layout) t)
 "The @code{row-homogeneous} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether all the rows in the grid have the same height. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'grid-layout-row-homogeneous)
      "Accessor"
      (documentation 'grid-layout-row-homogeneous 'function)
 "@version{2024-04-23}
  @syntax{(gtk:grid-layout-row-homogeneous object) => homogeneous}
  @syntax{(setf (gtk:grid-layout-row-homogeneous object) homogeneous)}
  @argument[object]{a @class{gtk:grid-layout} object}
  @argument[homogeneous]{a boolean whether all the rows in the grid have the
    same height}
  @begin{short}
    Accessor of the @slot[gtk:grid-layout]{row-homogeneous} slot of the
    @class{gtk:grid-layout} class.
  @end{short}
  The @fun{gtk:grid-layout-row-homogeneous} function checks whether all
  rows of the grid should have the same height. The
  @setf{gtk:grid-layout-row-homogeneous} function sets whether all rows of the
  grid should have the same height.
  @see-class{gtk:grid-layout}")

;;; --- gtk:grid-layout-row-spacing --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "row-spacing" 'grid-layout) t)
 "The @code{row-spacing} property of type @code{:int} (Read / Write) @br{}
  The amount of space between to consecutive rows. @br{}
  Allowed values: [0,32767] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'grid-layout-row-spacing)
      "Accessor"
      (documentation 'grid-layout-row-spacing 'function)
 "@version{2025-06-30}
  @syntax{(gtk:grid-layout-row-spacing object) => spacing}
  @syntax{(setf (gtk:grid-layout-row-spacing object) spacing)}
  @argument[object]{a @class{gtk:grid-layout} object}
  @argument[spacing]{an integer for the amount of space between to consecutive
    rows}
  @begin{short}
    Accessor of the @slot[gtk:grid-layout]{row-spacing} slot of the
    @class{gtk:grid-layout} class.
  @end{short}
  The @fun{gtk:grid-layout-row-spacing} function retrieves the amount of space
  to insert between consecutive rows. The @setf{gtk:grid-layout-row-spacing}
  function sets the spacing.
  @see-class{gtk:grid-layout}")

;;; ----------------------------------------------------------------------------
;;; gtk_grid_layout_new
;;; ----------------------------------------------------------------------------

(declaim (inline grid-layout-new))

(defun grid-layout-new ()
 #+liber-documentation
 "@version{2024-04-23}
  @return{The newly created @class{gtk:grid-layout} object.}
  @short{Creates a new grid layout.}
  @see-class{gtk:grid-layout}"
  (make-instance 'grid-layout))

(export 'grid-layout-new)

;; -----------------------------------------------------------------------------
;;; gtk_grid_layout_set_row_baseline_position
;;; gtk_grid_layout_get_row_baseline_position
;;; ----------------------------------------------------------------------------

(defun (setf grid-layout-row-baseline-position) (position layout row)
  (cffi:foreign-funcall "gtk_grid_layout_set_row_baseline_position"
                        (g:object grid-layout) layout
                        :int row
                        baseline-position position
                        :void)
  position)

(cffi:defcfun ("gtk_grid_layout_get_row_baseline_position"
               grid-layout-row-baseline-position) baseline-position
 #+liber-documentation
 "@version{2025-06-30}
  @syntax{(gtk:grid-layout-row-baseline-position layout) => position}
  @syntax{(setf (gtk:grid-layout-row-baseline-position layout) position)}
  @argument[layout]{a @class{gtk:grid-layout} object}
  @argument[row]{an integer for the row index}
  @argument[position]{a @sym{gtk:baseline-position} value}
  @begin{short}
    The @fun{gtk:grid-layout-row-baseline-position} function returns the
    baseline position of @arg{row}.
  @end{short}
  The @setf{gtk:grid-layout-row-baseline-position} function sets how the
  baseline should be positioned on @arg{row} of the grid, in case that @arg{row}
  is assigned more space than is requested. The default value is the
  @val[gtk:baseline-position]{:center} value of the @sym{gtk:baseline-position}
  enumeration.
  @see-class{gtk:grid-layout}
  @see-symbol{gtk:baseline-position}"
  (layout (g:object grid-layout))
  (row :int))

(export 'grid-layout-row-baseline-position)

;;; --- End of file gtk4.grid-layout.lisp --------------------------------------
