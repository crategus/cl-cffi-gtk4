;;; ----------------------------------------------------------------------------
;;; gtk4.column-view-sorter.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2026 Dieter Kaiser
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
;;;     GtkColumnViewSorter
;;;
;;; Accessors
;;;
;;;     gtk_column_view_sorter_get_primary_sort_column
;;;     gtk_column_view_sorter_get_primary_sort_order
;;;
;;; Functions
;;;
;;;     gtk_column_view_sorter_get_n_sort_columns
;;;     gtk_column_view_sorter_get_nth_sort_column
;;;
;;; Properties
;;;
;;;     primary-sort-column
;;;     primary-sort-order
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSorter
;;;         ╰── GtkColumnViewSorter
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkColumnViewSorter
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkColumnViewSorter" column-view-sorter
  (:superclass sorter
   :export t
   :interfaces ()
   :type-initializer "gtk_column_view_sorter_get_type")
  ((primary-sort-column
    column-view-sorter-primary-sort-column
    "primary-sort-column" "GtkColumnViewColumn" t nil)
   (primary-sort-order
    column-view-sorter-primary-sort-order
    "primary-sort-order" "GtkSortType" t nil)))

#+liber-documentation
(setf (documentation 'column-view-sorter 'type)
 "@version{2025-07-24}
  @begin{short}
    The @class{gtk:column-view-sorter} class is a sorter implementation that is
    geared towards the needs of the @class{gtk:column-view} widget.
  @end{short}
  The sorter returned by the @fun{gtk:column-view-sorter} function is a
  @class{gtk:column-view-sorter} object.

  In column views, sorting can be configured by associating sorters with
  columns, and users can invert sort order by clicking on column headers. The
  @class{gtk:column-view-sorter} API is designed to allow saving and restoring
  this configuration.

  If you are only interested in the primary sort column, that is, the column
  where a sort indicator is shown in the header, then you can just look at the
  @slot[gtk:column-view-sorter]{primary-sort-column} and
  @slot[gtk:column-view-sorter]{primary-sort-order} properties.

  If you want to store the full sort configuration, including secondary sort
  columns that are used for tie breaking, then you can use the
  @fun{gtk:column-view-sorter-nth-sort-column} function. To get notified about
  changes, use the @sig[gtk:sorter]{changed} signal.

  Since 4.10
  @begin[Examples]{dictionary}
    To restore a saved sort configuration on a @class{gtk:column-view} widget,
    use code like:
    @begin{pre}
(let ((sorter (gtk:column-view-sorter view)))
  (dotimes (pos (gtk:column-view-sorter-n-sort-columns sorter))
    (multiple-value-bind (column order)
        (gtk:column-view-sorter-nth-sort-column sorter pos)
      (gtk:column-view-sort-by-column view column order)
      ... )))
    @end{pre}
  @end{dictionary}
  @see-slot{gtk:column-view-sorter-primary-sort-column}
  @see-slot{gtk:column-view-sorter-primary-sort-order}
  @see-class{gtk:column-view}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:column-view-sorter-primary-sort-column -----------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-sort-column"
                                               'column-view-sorter) t)
 "The @code{primary-sort-column} property of type @class{gtk:column-view-column}
  (Read) @br{}
  The primary sort column is the one that displays the triangle in a column
  view header. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-sorter-primary-sort-column)
      "Accessor"
      (documentation 'column-view-sorter-primary-sort-column 'function)
 "@version{2025-09-28}
  @syntax{(gtk:column-view-sorter-primary-sort-column object) => column}
  @argument[object]{a @class{gtk:column-view-sorter} object}
  @argument[column]{a @class{gtk:column-view-column} object for the primary
    sort column}
  @begin{short}
    The accessor for the @slot[gtk:column-view-sorter]{primary-sort-column} slot
    of the @class{gtk:column-view-sorter} class returns the primary sort column.
  @end{short}
  The primary sort column is the one that displays the triangle in a column view
  header.

  Since 4.10
  @see-class{gtk:column-view-sorter}
  @see-class{gtk:column-view-column}")

;;; --- gtk:column-view-sorter-primary-sort-order ------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-sort-order"
                                               'column-view-sorter) t)
 "The @code{primary-sort-order} property of type @sym{gtk:sort-type} (Read)
  @br{}
  The primary sort order determines whether the triangle displayed in the column
  view header of the primary sort column points upwards or downwards. @br{}
  Default value: @val[gtk:sort-type]{:ascending}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-sorter-primary-sort-order)
      "Accessor"
      (documentation 'column-view-sorter-primary-sort-order 'function)
 "@version{2025-09-28}
  @syntax{(gtk:column-view-sorter-primary-sort-order object) => order}
  @argument[object]{a @class{gtk:column-view-sorter} object}
  @argument[column]{a @sym{gtk:sort-type} value for the primary sort order}
  @begin{short}
    The accessor for the @slot[gtk:column-view-sorter]{primary-sort-order} slot
    of the @class{gtk:column-view-sorter} class returns the primary sort order.
  @end{short}
  The primary sort order determines whether the triangle displayed in the column
  view header of the primary sort column points upwards or downwards. If there
  is no primary sort column, then this function returns the
  @val[gtk:sort-type]{:ascending} value.

  Since 4.10
  @see-class{gtk:column-view-sorter}
  @see-symbol{gtk:sort-type}")

;;; ----------------------------------------------------------------------------
;;; gtk_column_view_sorter_get_n_sort_columns
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_column_view_sorter_get_n_sort_columns"
               column-view-sorter-n-sort-columns) :uint
 #+liber-documentation
 "@version{2025-07-24}
  @argument[sorter]{a @class{gtk:column-view-sorter} object}
  @return{The unsigned integer for the number of sort columns.}
  @begin{short}
    Returns the number of columns by which the sorter sorts.
  @end{short}
  If the sorter of the primary sort column does not determine a total order,
  then the secondary sorters are consulted to break the ties. Use the
  @sig[gtk:sorter]{changed} signal to get notified when the number of sort
  columns changes.

  Since 4.10
  @see-class{gtk:column-view-sorter}"
  (sorter (g:object column-view-sorter)))

(export 'column-view-sorter-n-sort-columns)

;;; ----------------------------------------------------------------------------
;;; gtk_column_view_sorter_get_nth_sort_column
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_column_view_sorter_get_nth_sort_column"
               %column-view-sorter-nth-sort-column)
    (g:object column-view-column)
  (sorter (g:object column-view-sorter))
  (pos :uint)
  (order :pointer))

(defun column-view-sorter-nth-sort-column (sorter pos)
 #+liber-documentation
 "@version{2025-07-24}
  @syntax{(gtk:column-view-sorter-nth-sort-column sorter pos) => column, order}
  @argument[sorter]{a @class{gtk:column-view-sorter} object}
  @argument[pos]{an unsigned integer for the position of the sort column
    to retrieve, 0 for the primary sort column}
  @argument[column]{a @class{gtk:column-view-column} object}
  @argument[order]{a @sym{gtk:sort-type} value for the sort order}
  @begin{short}
    Gets the position‘th sort column and its associated sort order.
  @end{short}
  Use the @sig[gtk:sorter]{changed} signal to get notified when sort columns
  change.

  Since 4.10
  @see-class{gtk:column-view-sorter}
  @see-class{gtk:column-view-column}
  @see-symbol{gtk:sort-type}"
  (cffi:with-foreign-object (order 'sort-type)
    (let ((column (%column-view-sorter-nth-sort-column sorter pos order)))
      (values column
              (cffi:mem-ref order 'sort-type)))))

(export 'column-view-sorter-nth-sort-column)

;;; --- End of file gtk4.column-view-sorter.lisp -------------------------------
