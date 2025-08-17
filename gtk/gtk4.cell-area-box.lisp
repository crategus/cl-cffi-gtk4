;;; ----------------------------------------------------------------------------
;;; gtk4.cell-area-box.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.12 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2025 Dieter Kaiser
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
;;; GtkCellAreaBox
;;;
;;;     A cell area that renders GtkCellRenderers into a row or a column
;;;
;;; Types and Values
;;;
;;;     GtkCellAreaBox
;;;
;;; Accessors
;;;
;;;     gtk_cell_area_box_get_spacing
;;;     gtk_cell_area_box_set_spacing
;;;
;;; Functions
;;;
;;;     gtk_cell_area_box_new
;;;     gtk_cell_area_box_pack_start
;;;     gtk_cell_area_box_pack_end
;;;
;;; Properties
;;;
;;;     spacing
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellArea
;;;             ╰── GtkCellAreaBox
;;;
;;; Implemented Interfaces
;;;
;;;     GtkCellLayout
;;;     GtkBuildable
;;;     GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCellAreaBox
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkCellAreaBox" cell-area-box
  (:superclass cell-area
   :export t
   :interfaces ("GtkCellLayout"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_cell_area_box_get_type")
  ((spacing
    cell-area-box-spacing
    "spacing" "gint" t t)))

#+liber-documentation
(setf (documentation 'cell-area-box 'type)
 "@version{2025-07-22}
  @begin{short}
    The @class{gtk:cell-area-box} object renders cell renderers into a row or a
    column depending on its orientation.
  @end{short}
  The @class{gtk:cell-area-box} implementation uses a notion of packing. Packing
  refers to adding cell renderers with reference to a particular position in a
  @class{gtk:cell-area-box} object. There are two reference positions: the start
  and the end of the box. When the @class{gtk:cell-area-box} object is oriented
  in the @val[gtk:orientation]{:vertical} orientation, the start is defined as
  the top of the box and the end is defined as the bottom. In the
  @val[gtk:orientation]{:horizontal} orientation start is defined as the left
  side and the end is defined as the right side.

  Alignments of @class{gtk:cell-renderer} objects rendered in adjacent rows can
  be configured by configuring the align child cell property with the
  @fun{gtk:cell-area-cell-property} function or by specifying the
  @arg{align} argument to the @fun{gtk:cell-area-box-pack-start} and
  @fun{gtk:cell-area-box-pack-end} functions.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area-box} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-constructor{gtk:cell-area-box-new}
  @see-slot{gtk:cell-area-box-spacing}
  @see-class{gtk:cell-renderer}
  @see-symbol{gtk:orientation}
  @see-function{gtk:cell-area-cell-property}
  @see-function{gtk:cell-area-box-pack-start}
  @see-function{gtk:cell-area-box-pack-end}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "spacing" 'cell-area-box) t)
 "The @code{spacing} property of type @code{:int} (Read / Write) @br{}
  The amount of space to reserve between cells. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cell-area-box-spacing)
      "Accessor"
      (documentation 'cell-area-box-spacing 'function)
 "@version{2025-08-13}
  @syntax{(gtk:cell-area-box-spacing object) => spacing}
  @syntax{(setf (gtk:cell-area-box-spacing object) spacing)}
  @argument[object]{a @class{gtk:cell-area-box} object}
  @argument[spacing]{an integer for the space to add between
    @class{gtk:cell-renderer} objects}
  @begin{short}
    The accessor for the @slot[gtk:cell-area-box]{spacing} slot of the
    @class{gtk:cell-area-box} class gets or sets the spacing added between cell
    renderers.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area-box} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area-box}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_box_new
;;; ----------------------------------------------------------------------------

(declaim (inline cell-area-box-new))

(defun cell-area-box-new ()
 #+liber-documentation
 "@version{2024-02-21}
  @return{The newly created @class{gtk:cell-area-box} object.}
  @short{Creates a new cell area box.}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area-box} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area-box}"
  (make-instance 'cell-area-box))

(export 'cell-area-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_box_pack_start
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_box_pack_start" %cell-area-box-pack-start) :void
  (box (g:object cell-area-box))
  (renderer (g:object cell-renderer))
  (expand :boolean)
  (align :boolean)
  (fixed :boolean))

(defun cell-area-box-pack-start (box renderer
                                 &key (expand t) (align t) (fixed t))
 #+liber-documentation
 "@version{#2024-02-21}
  @argument[box]{a @class{gtk:cell-area-box} widget}
  @argument[renderer]{a @class{gtk:cell-renderer} object to add}
  @argument[expand]{a boolean whether @arg{renderer} should receive extra space
    when the area receives more than its natural size}
  @argument[align]{a boolean whether @arg{renderer} should be aligned in
    adjacent rows}
  @argument[fixed]{a boolean whether @arg{renderer} should have the same size
    in all rows}
  @begin{short}
    Adds a renderer to the cell area box, packed with reference to the start of
    the box.
  @end{short}
  The renderer is packed after any other @class{gtk:cell-renderer} object packed
  with reference to the start of the box.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area-box} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area-box}
  @see-class{gtk:cell-renderer}"
  (%cell-area-box-pack-start box renderer expand align fixed))

(export 'cell-area-box-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_box_pack_end
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_box_pack_end" %cell-area-box-pack-end) :void
  (box (g:object cell-area-box))
  (renderer (g:object cell-renderer))
  (expand :boolean)
  (align :boolean)
  (fixed :boolean))

(defun cell-area-box-pack-end (box child
                               &key (expand t) (align t) (fixed t))
 #+liber-documentation
 "@version{#2024-02-21}
  @argument[box]{a @class{gtk:cell-area-box} widget}
  @argument[renderer]{a @class{gtk:cell-renderer} object to add}
  @argument[expand]{a boolean whether @arg{renderer} should receive extra space
    when the area receives more than its natural size}
  @argument[align]{a boolean whether @arg{renderer} should be aligned in
    adjacent rows}
  @argument[fixed]{a boolean whether @arg{renderer} should have the same size
    in all rows}
  @begin{short}
    Adds a renderer to cell area box, packed with reference to the end of
    the box.
  @end{short}
  The renderer is packed after, away from end of, any other
  @class{gtk:cell-renderer} object packed with reference to the end of the box.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area-box} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area-box}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-area-box-pack-start}"
  (%cell-area-box-pack-end box child expand align fixed))

(export 'cell-area-box-pack-end)

;;; --- End of file gtk4.cell-area-box.lisp ------------------------------------
