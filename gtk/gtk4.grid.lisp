;;; ----------------------------------------------------------------------------
;;; gtk4.grid.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2026 Dieter Kaiser
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
;;; GtkGrid
;;;
;;;     Pack widgets in a rows and columns
;;;
;;; Types and Values
;;;
;;;     GtkGrid
;;;
;;; Accessors
;;;
;;;     gtk_grid_get_baseline_row
;;;     gtk_grid_set_baseline_row
;;;     gtk_grid_get_column_homogeneous
;;;     gtk_grid_set_column_homogeneous
;;;     gtk_grid_get_column_spacing
;;;     gtk_grid_set_column_spacing
;;;     gtk_grid_get_row_homogeneous
;;;     gtk_grid_set_row_homogeneous
;;;     gtk_grid_get_row_spacing
;;;     gtk_grid_set_row_spacing
;;;
;;; Functions
;;;
;;;     gtk_grid_new
;;;
;;;     gtk_grid_attach
;;;     gtk_grid_attach_next_to
;;;     gtk_grid_remove
;;;     gtk_grid_get_child_at
;;;     gtk_grid_query_child
;;;     gtk_grid_insert_row
;;;     gtk_grid_insert_column
;;;     gtk_grid_remove_row
;;;     gtk_grid_remove_column
;;;     gtk_grid_insert_next_to
;;;     gtk_grid_get_row_baseline_position
;;;     gtk_grid_set_row_baseline_position
;;;
;;; Properties
;;;
;;;     baseline-row
;;;     column-homogeneous
;;;     column-spacing
;;;     row-homogeneous
;;;     row-spacing
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkGrid
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkGrid
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkGrid" grid
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_grid_get_type")
  ((baseline-row
    grid-baseline-row
    "baseline-row" "gint" t t)
   (column-homogeneous
    grid-column-homogeneous
    "column-homogeneous" "gboolean" t t)
   (column-spacing
    grid-column-spacing
    "column-spacing" "gint" t t)
   (row-homogeneous
    grid-row-homogeneous
    "row-homogeneous" "gboolean" t t)
   (row-spacing
    grid-row-spacing
    "row-spacing" "gint" t t)))

#+liber-documentation
(setf (documentation 'grid 'type)
 "@version{2025-06-26}
  @begin{short}
    The @class{gtk:grid} widget is a container which arranges its child widgets
    in rows and columns, with arbitrary positions and horizontal/vertical spans.
  @end{short}

  @image[grid]{Figure: GtkGrid}

  Children are added using the @fun{gtk:grid-attach} function. They can span
  multiple rows or columns. It is also possible to add a child widget next to
  an existing child widget, using the @fun{gtk:grid-attach-next-to} function.
  To remove a child widget from the grid, use the @fun{gtk:grid-remove}
  function. The behaviour of the @class{gtk:grid} widget when several children
  occupy the same grid cell is undefined.
  @begin[GtkGrid as GtkBuildable]{dictionary}
    Every child in a @class{gtk:grid} widget has access to a custom
    @class{gtk:buildable} element, called @code{<layout>}. It can by used to
    specify a position in the grid and optionally spans. All properties that
    can be used in the @code{<layout>} element are implemented by the
    @class{gtk:grid-layout-child} class.

    It is implemented by the @class{gtk:widget} class using the
    @class{gtk:layout-manager} class.

    To showcase it, here is a simple example:
    @begin{pre}
<object class=\"GtkGrid\" id=\"my_grid\">
  <child>
    <object class=\"GtkButton\" id=\"button1\">
      <property name=\"label\">Button 1</property>
      <layout>
        <property name=\"column\">0</property>
        <property name=\"row\">0</property>
      </layout>
    </object>
  </child>
  <child>
    <object class=\"GtkButton\" id=\"button2\">
      <property name=\"label\">Button 2</property>
      <layout>
        <property name=\"column\">1</property>
        <property name=\"row\">0</property>
      </layout>
    </object>
  </child>
  <child>
    <object class=\"GtkButton\" id=\"button3\">
      <property name=\"label\">Button 3</property>
      <layout>
        <property name=\"column\">2</property>
        <property name=\"row\">0</property>
        <property name=\"row-span\">2</property>
      </layout>
    </object>
  </child>
  <child>
    <object class=\"GtkButton\" id=\"button4\">
      <property name=\"label\">Button 4</property>
      <layout>
        <property name=\"column\">0</property>
        <property name=\"row\">1</property>
        <property name=\"column-span\">2</property>
      </layout>
    </object>
  </child>
</object>
    @end{pre}
    It organizes the first two buttons side-by-side in one cell each. The third
    button is in the last column but spans across two rows. This is defined by
    the @slot[gtk:grid-layout-child]{row-span} property. The last button is
    located in the second row and spans across two columns, which is defined by
    the @slot[gtk:grid-layout-child]{column-span} property.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @class{gtk:grid} implementation uses a single CSS node with name
    @code{grid}.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    Until GTK 4.10, the @class{gtk:grid} implementation used the
    @val[gtk:accessible-role]{:group} role of the @sym{gtk:accessible-role}
    enumeration. Starting from GTK 4.12, it uses the
    @val[gtk:accessible-role]{:generic} role.
  @end{dictionary}
  @see-constructor{gtk:grid-new}
  @see-slot{gtk:grid-baseline-row}
  @see-slot{gtk:grid-column-homogeneous}
  @see-slot{gtk:grid-column-spacing}
  @see-slot{gtk:grid-row-homogeneous}
  @see-slot{gtk:grid-row-spacing}
  @see-class{gtk:box}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:grid-baseline-row --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "baseline-row" 'grid) t)
 "The @code{baseline-row} property of type @code{:int} (Read / Write) @br{}
  The row to align to the baseline when @slot[gtk:widget]{valign} has the
  @val[gtk:align]{:center} value of the @sym{gtk:align} enumeration. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0 @br{}")

#+liber-documentation
(setf (liber:alias-for-function 'grid-baseline-row)
      "Accessor"
      (documentation 'grid-baseline-row 'function)
 "@version{2025-07-29}
  @syntax{(gtk:grid-baseline-row object) => row}
  @syntax{(setf (gtk:grid-baseline-row object) row)}
  @argument[object]{a @class{gtk:grid} widget}
  @argument[row]{an integer for the row index}
  @begin{short}
    The accessor for the @slot[gtk:grid]{baseline-row} slot of the
    @class{gtk:grid} class gets or sets the row to align to the baseline when
    @slot[gtk:widget]{valign} has the @val[gtk:align]{:center} value of the
    @sym{gtk:align} enumeration.
  @end{short}

  Each row in the grid can have its own local baseline, but only one of those
  is global, meaning it will be the baseline in the parent of the grid.
  @see-class{gtk:grid}
  @see-symbol{gtk:align}")

;;; --- gtk:grid-column-homogeneous --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "column-homogeneous" 'grid) t)
 "The @code{column-homogeneous} property of type @code{:boolean} (Read / Write)
  @br{}
  If @em{true}, the columns are all the same width. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'grid-column-homogeneous)
      "Accessor"
      (documentation 'grid-column-homogeneous 'function)
 "@version{2025-07-29}
  @syntax{(gtk:grid-column-homogeneous object) => homogenous}
  @syntax{(setf (gtk:grid-column-homogeneous object) homogenous)}
  @argument[object]{a @class{gtk:grid} widget}
  @argument[homogeneous]{@em{true} to make columns homogeneous}
  @begin{short}
    The accessor for the @slot[gtk:grid]{column-homogeneous} slot of the
    @class{gtk:grid} class gets or sets whether all columns of the grid have
    the same width.
  @end{short}
  @see-class{gtk:grid}
  @see-function{gtk:grid-row-homogeneous}")

;;; --- gtk:grid-column-spacing ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "column-spacing" 'grid) t)
 "The @code{column-spacing} property of type @code{:int} (Read / Write) @br{}
  The amount of space between two consecutive columns. @br{}
  Allowed values: [0, 32767] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'grid-column-spacing)
      "Accessor"
      (documentation 'grid-column-spacing 'function)
 "@version{2025-07-29}
  @syntax{(gtk:grid-column-spacing object) => spacing}
  @syntax{(setf (gtk:grid-column-spacing object) spacing)}
  @argument[object]{a @class{gtk:grid} widget}
  @argument[spacing]{an integer for the amount of space to insert between
    columns}
  @begin{short}
    The accessor for the @slot[gtk:grid]{column-spacing} slot of the
    @class{gtk:grid} class gets or sets the amount of space between two columns
    of the grid.
  @end{short}
  @see-class{gtk:grid}
  @see-function{gtk:grid-row-spacing}")

;;; --- gtk:grid-row-homogeneous -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "row-homogeneous" 'grid) t)
 "The @code{row-homogeneous} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, the rows are all the same height. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'grid-row-homogeneous)
      "Accessor"
      (documentation 'grid-row-homogeneous 'function)
 "@version{2025-07-29}
  @syntax{(gtk:grid-row-homogeneous object) => homogeneous}
  @syntax{(setf (gtk:grid-row-homogeneous object) homogeneous)}
  @argument[object]{a @class{gtk:grid} widget}
  @argument[homogeneous]{@em{true} to make rows homogeneous}
  @begin{short}
    The accessor for the @slot[gtk:grid]{row-homogeneous} slot of the
    @class{gtk:grid} class gets or sets whether all rows of the grid have the
    same height.
  @end{short}
  @see-class{gtk:grid}
  @see-function{gtk:grid-column-homogeneous}")

;;; --- gtk:grid-row-spacing ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "row-spacing" 'grid) t)
 "The @code{row-spacing} property of type @code{:int} (Read / Write) @br{}
  The amount of space between two consecutive rows. @br{}
  Allowed values: [0, 32767] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'grid-row-spacing)
      "Accessor"
      (documentation 'grid-row-spacing 'function)
 "@version{2025-07-29}
  @syntax{(gtk:grid-row-spacing object) => spacing}
  @syntax{(setf (gtk:grid-row-spacing object) spacing)}
  @argument[object]{a @class{gtk:grid} widget}
  @argument[spacing]{an integer for the amount of space to insert between rows}
  @begin{short}
    The accessor for the @slot[gtk:grid]{row-spacing} slot of the
    @class{gtk:grid} class gets or sets the amount of space between two rows of
    the grid.
  @end{short}
  @see-class{gtk:grid}
  @see-function{gtk:grid-column-spacing}")

;;; ----------------------------------------------------------------------------
;;; gtk_grid_new
;;; ----------------------------------------------------------------------------

(declaim (inline grid-new))

(defun grid-new ()
 #+liber-documentation
 "@version{2025-05-10}
  @return{The new @class{gtk:grid} widget.}
  @short{Creates a new grid widget.}
  @see-class{gtk:grid}"
  (make-instance 'grid))

(export 'grid-new)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_attach
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_grid_attach" %grid-attach) :void
  (grid (g:object grid))
  (child (g:object widget))
  (left :int)
  (top :int)
  (width :int)
  (height :int))

(defun grid-attach (grid child left top &optional (width 1) (height 1))
 #+liber-documentation
 "@version{2025-05-10}
  @argument[grid]{a @class{gtk:grid} widget}
  @argument[child]{a @class{gtk:widget} child widget to add}
  @argument[left]{an integer for the column number to attach the left side of
    @arg{child} to}
  @argument[top]{an integer for the row number to attach the top side of
    @arg{child} to}
  @argument[width]{an optional integer for the number of columns that
    @arg{child} will span, the default value is 1}
  @argument[height]{an optional integer for the number of rows that
    @arg{child} will span, the default value is 1}
  @begin{short}
    Adds a child widget to the grid.
  @end{short}
  The position of the child widget is determined by the @arg{left} and @arg{top}
  arguments. The number of cells that the child widget will occupy is determined
  by the optional @arg{width} and @arg{height} arguments.
  @see-class{gtk:grid}
  @see-class{gtk:widget}
  @see-function{grid-attach-next-to}"
  (%grid-attach grid child left top width height))

(export 'grid-attach)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_attach_next_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_grid_attach_next_to" %grid-attach-next-to) :void
  (grid (g:object grid))
  (child (g:object widget))
  (sibling (g:object widget))
  (side position-type)
  (width :int)
  (height :int))

(defun grid-attach-next-to (grid child sibling side
                            &optional (width 1) (height 1))
 #+liber-documentation
 "@version{2025-06-26}
  @argument[grid]{a @class{gtk:grid} widget}
  @argument[child]{a @class{gtk:widget} child widget to add}
  @argument[sibling]{a @class{gtk:widget} sibling widget of the grid that
    @arg{child} will be placed next to, or @code{nil} to place @arg{child} at
    the beginning or end}
  @argument[side]{a @sym{gtk:position-type} value for the side of @arg{sibling}
    that @arg{child} is positioned next to}
  @argument[width]{an optional integer for the number of columns that
    @arg{child} will span, the default value is 1}
  @argument[height]{an optional integer for the number of rows that
    @arg{child} will span, the default value is 1}
  @begin{short}
    Adds a child widget to the grid.
  @end{short}
  The child widget is placed next to @arg{sibling}, on the side determined
  by @arg{side}. When the @arg{sibling} argument is @code{nil}, the child
  widget is placed in row 0, for left or right placement, or column 0, for top
  or bottom placement, at the end indicated by @arg{side}.
  @see-class{gtk:grid}
  @see-class{gtk:widget}
  @see-symbol{gtk:position-type}
  @see-function{gtk:grid-attach}"
  (%grid-attach-next-to grid child sibling side width height))

(export 'grid-attach-next-to)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_grid_remove" grid-remove) :void
 #+liber-documentation
 "@version{2025-05-10}
  @argument[grid]{a @class{gtk:grid} widget}
  @argument[child]{a @class{gtk:widget} child widget to remove}
  @begin{short}
    Removes a child widget from the grid.
  @end{short}
  The child widget must have been added with the @fun{gtk:grid-attach} or
  @fun{gtk:grid-attach-next-to} functions.
  @see-class{gtk:grid}
  @see-class{gtk:widget}
  @see-function{gtk:grid-attach}
  @see-function{gtk:grid-attach-next-to}"
  (grid (g:object widget))
  (child (g:object widget)))

(export 'grid-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_get_child_at
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_grid_get_child_at" grid-child-at) (g:object widget)
 #+liber-documentation
 "@version{2025-05-10}
  @argument[grid]{a @class{gtk:grid} widget}
  @argument[left]{an integer for the left edge of the cell}
  @argument[top]{an integer for the top edge of the cell}
  @begin{return}
    The @class{gtk:widget} child widget at the given position, or @code{nil}.
  @end{return}
  @begin{short}
    Gets the child widget of the grid whose area covers the grid cell whose
    upper left corner is at @arg{left}, @arg{top}.
  @end{short}
  @see-class{gtk:grid}
  @see-class{gtk:widget}"
  (grid (g:object grid))
  (left :int)
  (top :int))

(export 'grid-child-at)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_query_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_grid_query_child" %grid-query-child) :void
  (grid (g:object grid))
  (child (g:object widget))
  (column (:pointer :int))
  (row (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun grid-query-child (grid child)
 #+liber-documentation
 "@version{2025-05-10}
  @syntax{(gtk:grid-query-child grid child) => column, row, width, height}
  @argument[grid]{a @class{gtk:grid} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[column]{an integer for the column used to attach the left side
    of @arg{child}}
  @argument[row]{an integer for the row used to attach the top side of
    @arg{child}}
  @argument[width]{an integer for the number of columns @arg{child} spans}
  @argument[height]{an integer for the number of rows @arg{child} spans}
  @begin{short}
    Queries the attach points and spans of the child widget inside the given
    grid.
  @end{short}
  @see-class{gtk:grid}
  @see-class{gtk:widget}"
  (cffi:with-foreign-objects ((column :int)
                              (row :int)
                              (width :int)
                              (height :int))
    (%grid-query-child grid child column row width height)
    (values (cffi:mem-ref column :int)
            (cffi:mem-ref row :int)
            (cffi:mem-ref width :int)
            (cffi:mem-ref height :int))))

(export 'grid-query-child)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_insert_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_grid_insert_row" grid-insert-row) :void
 #+liber-documentation
 "@version{2025-05-10}
  @argument[grid]{a @class{gtk:grid} widget}
  @argument[pos]{an integer for the position to insert the row at}
  @begin{short}
    Inserts a row at the specified position.
  @end{short}
  Children which are attached at or below this position are moved one row down.
  Children which span across this position are grown to span the new row.
  @see-class{gtk:grid}
  @see-function{gtk:grid-insert-column}
  @see-function{gtk:grid-insert-next-to}"
  (grid (g:object grid))
  (pos :int))

(export 'grid-insert-row)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_insert_column
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_grid_insert_column" grid-insert-column) :void
 #+liber-documentation
 "@version{2025-05-10}
  @argument[grid]{a @class{gtk:grid} widget}
  @argument[pos]{an integer for the position to insert the column at}
  @begin{short}
    Inserts a column at the specified position.
  @end{short}
  Children which are attached at or to the right of this position are
  moved one column to the right. Children which span across this position
  are grown to span the new column.
  @see-class{gtk:grid}
  @see-function{gtk:grid-insert-row}
  @see-function{gtk:grid-insert-next-to}"
  (grid (g:object grid))
  (pos :int))

(export 'grid-insert-column)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_remove_row
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_grid_remove_row" grid-remove-row) :void
 #+liber-documentation
 "@version{2025-05-10}
  @argument[grid]{a @class{gtk:grid} widget}
  @argument[pos]{an integer for the position of the row to remove}
  @begin{short}
    Removes a row from the grid.
  @end{short}
  Children that are placed in this row are removed, spanning children that
  overlap this row have their height reduced by one, and children below the
  row are moved up.
  @see-class{gtk:grid}
  @see-function{gtk:grid-remove-column}
  @see-function{gtk:grid-insert-row}"
  (grid (g:object grid))
  (pos :int))

(export 'grid-remove-row)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_remove_column
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_grid_remove_column" grid-remove-column) :void
 #+liber-documentation
 "@version{2025-05-10}
  @argument[grid]{a @class{gtk:grid} widget}
  @argument[pos]{an integer for the position of the column to remove}
  @begin{short}
    Removes a column from the grid.
  @end{short}
  Children that are placed in this column are removed, spanning children that
  overlap this column have their width reduced by one, and children after the
  column are moved to the left.
  @see-class{gtk:grid}
  @see-function{gtk:grid-remove-row}
  @see-function{gtk:grid-insert-column}"
  (grid (g:object grid))
  (pos :int))

(export 'grid-remove-column)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_insert_next_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_grid_insert_next_to" grid-insert-next-to) :void
 #+liber-documentation
 "@version{2025-06-26}
  @argument[grid]{a @class{gtk:grid} widget}
  @argument[sibling]{a @class{gtk:widget} sibling widget of the grid that the
    new row or column will be placed next to}
  @argument[side]{a @sym{gtk:position-type} value for the side of @arg{sibling}
    that @arg{child} is positioned next to}
  @begin{short}
    Inserts a row or column at the specified position.
  @end{short}
  The new row or column is placed next to @arg{sibling}, on the side determined
  by @arg{side}. If the @arg{side} argument is @val[gtk:position-type]{:top} or
  @val[gtk:position-type]{:bottom}, a row is inserted. If the @arg{side}
  argument is @val[gtk:position-type]{:left} or @val[gtk:position-type]{:right},
  a column is inserted.
  @see-class{gtk:grid}
  @see-class{gtk:widget}
  @see-symbol{gtk:position-type}
  @see-function{gtk:grid-insert-column}
  @see-function{gtk:grid-insert-row}"
  (grid (g:object grid))
  (sibling (g:object widget))
  (side position-type))

(export 'grid-insert-next-to)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_get_row_baseline_position
;;; gtk_grid_set_row_baseline_position
;;; ----------------------------------------------------------------------------

(defun (setf grid-row-baseline-position) (pos grid row)
  (cffi:foreign-funcall "gtk_grid_set_row_baseline_position"
                        (g:object grid) grid
                        :int row
                        position-type pos
                        :void)
  pos)

(cffi:defcfun ("gtk_grid_get_row_baseline_position"
               grid-row-baseline-position) position-type
 #+liber-documentation
 "@version{2025-06-26}
  @syntax{(gtk:grid-row-baseline-position grid row) => pos}
  @syntax{(setf (gtk:grid-row-baseline-position grid row) pos)}
  @argument[grid]{a @class{gtk:grid} widget}
  @argument[row]{an integer for the row index}
  @argument[pos]{a value of the @sym{gtk:baseline-position} enumeration}
  @begin{short}
    The @fun{gtk:grid-row-baseline-position} function returns the baseline
    position of @arg{row}.
  @end{short}
  The @setf{gtk:grid-row-baseline-position} function sets how the baseline
  should be positioned on @arg{row} of the grid, in case that @arg{row} is
  assigned more space than is requested. The default baseline position is
  @val[gtk:baseline-position]{:center}.
  @see-class{gtk:grid}
  @see-symbol{gtk:baseline-position}"
  (grid (g:object grid))
  (row :int))

(export 'grid-row-baseline-position)

;;; --- End of file gtk4.grid.lisp ---------------------------------------------
