;;; ----------------------------------------------------------------------------
;;; gtk4.box-layout.lisp
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
;;; GtkBoxLayout
;;;
;;;     Layout manager for placing all children in a single row or column
;;;
;;; Types and Values
;;;
;;;     GtkBoxLayout
;;;
;;; Accessors
;;;
;;;     gtk_box_layout_get_homogeneous
;;;     gtk_box_layout_set_homogeneous
;;;     gtk_box_layout_get_spacing
;;;     gtk_box_layout_set_spacing
;;;     gtk_box_layout_get_baseline_position
;;;     gtk_box_layout_set_baseline_position
;;;     gtk_box_layout_get_baseline_child                  Since 4.12
;;;     gtk_box_layout_set_baseline_child                  Since 4.12
;;;
;;; Functions
;;;
;;;     gtk_box_layout_new
;;;
;;; Properties
;;;
;;;     baseline-child                                     Since 4.12
;;;     baseline-position
;;;     homogeneous
;;;     spacing
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkLayoutManager
;;;         ╰── GtkBoxLayout
;;;
;;; Implemented Interfaces
;;;
;;;     GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBoxLayout
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkBoxLayout" box-layout
  (:superclass layout-manager
   :export t
   :interfaces ("GtkOrientable")
   :type-initializer "gtk_box_layout_get_type")
  (#+gtk-4-12
   (baseline-child
    box-layout-baseline-child
    "baseline-child" "gint" t t)
   (baseline-position
    box-layout-baseline-position
    "baseline-position" "GtkBaselinePosition" t t)
   (homogeneous
    box-layout-homogeneous
    "homogeneous" "gboolean" t t)
   (spacing
    box-layout-spacing
    "spacing" "gint" t t)))

#+liber-documentation
(setf (documentation 'box-layout 'type)
 "@version{2025-08-12}
  @begin{short}
    The @class{gtk:box-layout} class is a layout manager that arranges the
    children of any widget using it into a single row or column.
  @end{short}
  Whether it is a row or column depends on the value of its
  @slot[gtk:orientable]{orientation} property. Within the other dimension all
  children all allocated the same size. The @class{gtk:box-layout} object will
  respect the @slot[gtk:widget]{halign} and @slot[gtk:widget]{valign} properties
  of each child widget.

  If you want all children to be assigned the same size, you can use the
  @slot[gtk:box-layout]{homogeneous} property. If you want to specify the
  amount of space placed between each child, you can use the
  @slot[gtk:box-layout]{spacing} property.
  @see-constructor{gtk:box-layout-new}
  @see-slot{gtk:box-layout-baseline-child}
  @see-slot{gtk:box-layout-baseline-position}
  @see-slot{gtk:box-layout-homogeneous}
  @see-slot{gtk:box-layout-spacing}
  @see-class{gtk:layout-manager}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:box-layout-baseline-child ------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "baseline-child" 'box-layout) t)
 "The @code{baseline-child} property of type @code{:int} (Read / Write) @br{}
  The child that determines the baseline of the box in vertical layout. If the
  child does baseline positioning, then its baseline is lined up with the
  baseline of the box. If it does not, then the bottom edge of the child is
  used. Since 4.12 @br{}
  Default value: -1")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'box-layout-baseline-child)
      "Accessor"
      (documentation 'box-layout-baseline-child 'function)
 "@version{2025-08-01}
  @syntax{(gtk:box-layout-baseline-child object) => index}
  @syntax{(setf (gtk:box-layout-baseline-child object) index)}
  @argument[object]{a @class{gtk:box-layout} object}
  @argument[index]{an integer for the child position, or -1}
  @begin{short}
    The accessor for the @slot[gtk:box-layout]{baseline-child} slot of the
    @class{gtk:box-layout} class gets or sets the index of the child that
    determines the baseline in vertical layout.
  @end{short}
  @see-class{gtk:box-layout}")

;;; --- gtk:box-layout-baseline-position ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "baseline-position"
                                               'box-layout) t)
 "The @code{baseline-position} property of type @sym{gtk:baseline-position}
  (Read / Write) @br{}
  The position of the allocated baseline within the extra space allocated to
  each child of the widget using a box layout manager. This property is only
  relevant for horizontal layouts containing at least one child with a baseline
  alignment. @br{}
  Default value: @val[gtk:baseline-position]{:center}")

#+liber-documentation
(setf (liber:alias-for-function 'box-layout-baseline-position)
      "Accessor"
      (documentation 'box-layout-baseline-position 'function)
 "@version{2025-08-01}
  @syntax{(gtk:box-layout-baseline-position object) => position}
  @syntax{(setf (gtk:box-layout-baseline-position object) position)}
  @argument[object]{a @class{gtk:box-layout} object}
  @argument[position]{a @sym{gtk:baseline-position} value}
  @begin{short}
    The accessor for the @slot[gtk:box-layout]{baseline-position} slot of the
    @class{gtk:box-layout} class gets or sets the baseline position of a box
    layout.
  @end{short}

  The baseline position affects only horizontal boxes with at least one
  baseline aligned child. If there is more vertical space available than
  requested, and the baseline is not allocated by the parent then the given
  position is used to allocate the baseline within the extra space available.
  @see-class{gtk:box-layout}
  @see-symbol{gtk:baseline-position}")

;;; --- gtk:box-layout-homogeneous ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "homogeneous" 'box-layout) t)
 "The @code{homogeneous} property of type @code{:boolean} (Read / Write) @br{}
  Whether the box layout should distribute the available space homogeneously
  among the children of the widget using it as a layout manager. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'box-layout-homogeneous)
      "Accessor"
      (documentation 'box-layout-homogeneous 'function)
 "@version{2025-08-01}
  @syntax{(gtk:box-layout-homogeneous object) => homogeneous}
  @syntax{(setf (gtk:box-layout-homogeneous object) homogeneous)}
  @argument[object]{a @class{gtk:box-layout} object}
  @argument[homogeneous]{@em{true} if the box layout is homogeneous}
  @begin{short}
    The accessor for the @slot[gtk:box-layout]{homogeneous} slot of the
    @class{gtk:box-layout} class gets or sets whether the box layout will
    allocate the same size to all children.
  @end{short}
  @see-class{gtk:box-layout}")

;;; --- gtk:box-layout-spacing -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "spacing" 'box-layout) t)
 "The @code{spacing} property of type @code{:int} (Read / Write) @br{}
  The space between each child of the widget using the box layout as its
  layout manager. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'box-layout-spacing)
      "Accessor"
      (documentation 'box-layout-spacing 'function)
 "@version{2025-08-12}
  @syntax{(gtk:box-layout-spacing object) => spacing}
  @syntax{(setf (gtk:box-layout-spacing object) spacing)}
  @argument[object]{a @class{gtk:box-layout} object}
  @argument[spacing]{an integer for the spacing of the box layout}
  @begin{short}
    The accessor for the @slot[gtk:box-layout]{spacing} slot of the
    @class{gtk:box-layout} class gets or sets the space that the box layout
    puts between children.
  @end{short}
  @see-class{gtk:box-layout}")

;;; ----------------------------------------------------------------------------
;;; gtk_box_layout_new
;;; ----------------------------------------------------------------------------

(declaim (inline box-layout-new))

(defun box-layout-new (orientation)
 #+liber-documentation
 "@version{2025-06-30}
  @argument[orientation]{a @sym{gtk:orientation} value}
  @return{The new @class{gtk:box-layout} object.}
  @short{Creates a new box layout.}
  @see-class{gtk:box-layout}
  @see-symbol{gtk:orientation}"
  (make-instance 'box-layout
                 :orientation orientation))

(export 'box-layout-new)

;;; --- End of file gtk4.box-layout.lisp ---------------------------------------
