;;; ----------------------------------------------------------------------------
;;; gtk4.box.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
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
;;; GtkBox
;;;
;;;     The GtkBox widget arranges child widgets into a single row or column.
;;;
;;; Types and Values
;;;
;;;     GtkBox
;;;
;;; Accessors
;;;
;;;     gtk_box_get_baseline_child                          Since 4.12
;;;     gtk_box_set_baseline_child                          Since 4.12
;;;     gtk_box_get_baseline_position
;;;     gtk_box_set_baseline_position
;;;     gtk_box_get_homogeneous
;;;     gtk_box_set_homogeneous
;;;     gtk_box_get_spacing
;;;     gtk_box_set_spacing
;;;
;;; Functions
;;;
;;;     gtk_box_new
;;;
;;;     gtk_box_append
;;;     gtk_box_prepend
;;;     gtk_box_remove
;;;     gtk_box_insert_child_after
;;;     gtk_box_reorder_child_after
;;;
;;; Properties
;;;
;;;     baseline-child                                      Since 4.12
;;;     baseline-position
;;;     homogeneous
;;;     spacing
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkBox
;;;                 ├── GtkShortcutsGroup
;;;                 ╰── GtkShortcutsSection
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
;;; GtkBox
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkBox" box
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_box_get_type")
  (#+gtk-4-12
   (baseline-child
    box-baseline-child
    "baseline-child" "gint" t t)
   (baseline-position
    box-baseline-position
    "baseline-position" "GtkBaselinePosition" t t)
   (homogeneous
    box-homogeneous
    "homogeneous" "gboolean" t t)
   (spacing
    box-spacing
    "spacing" "gint" t t)))

#+liber-documentation
(setf (documentation 'box 'type)
 "@version{2025-07-26}
  @begin{short}
    The @class{gtk:box} widget arranges child widgets into a single row or
    column.
  @end{short}

  @image[box]{Figure: GtkBox}

  Whether it is a row or column depends on the value of its
  @slot[gtk:orientable]{orientation} property. Within the other dimension, all
  children are allocated the same size. Of course, the @slot[gtk:widget]{halign}
  and @slot[gtk:widget]{valign} properties can be used on the children to
  influence their allocation.

  Use repeated calls to the @fun{gtk:box-append} function to pack widgets into
  a box from start to end. Use the @fun{gtk:box-remove} function to remove
  widgets from the box. The @fun{gtk:box-insert-child-after} function can be
  used to add a child widget at a particular position.

  Use the @fun{gtk:box-homogeneous} function to specify whether or not all
  children of the box are forced to get the same amount of space and the
  @fun{gtk:box-spacing} function to determine how much space will be minimally
  placed between all children in the box. Note that spacing is added between
  the children.

  Use the @fun{gtk:box-reorder-child-after} function to move a child widget to
  a different place in the box.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:box} implementation uses a single CSS node with name
    @code{box}.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    Until GTK 4.10, the @class{gtk:box} implementation used the
    @val[gtk:accessible-role]{:group} role of the @sym{gtk:accessible-role}
    enumeration. Starting from GTK 4.12, it uses the
    @val[gtk:accessible-role]{:generic} role.
  @end{dictionary}
  @see-constructor{gtk:box-new}
  @see-slot{gtk:box-baseline-child}
  @see-slot{gtk:box-baseline-position}
  @see-slot{gtk:box-homogeneous}
  @see-slot{gtk:box-spacing}
  @see-class{gtk:widget}
  @see-class{gtk:orientable}
  @see-symbol{gtk:orientation}
  @see-symbol{gtk:accessible-role}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:box-baseline-child -------------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "baseline-child" 'box) t)
 "The @code{baseline-child} property of type @code{:int} (Read / Write) @br{}
  The position of the child widget that determines the baseline of the box in
  vertical layout. This is only relevant if the box is in vertical orientation.
  Since 4.12 @br{}
  Default value: -1")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'box-baseline-child)
      "Accessor"
      (documentation 'box-baseline-child 'function)
 "@version{2025-07-29}
  @syntax{(gtk:box-baseline-child object) => child}
  @syntax{(setf (gtk:box-baseline-child object) child)}
  @argument[object]{a @class{gtk:box} widget}
  @argument[child]{an integer for the baseline child position, or -1}
  @begin{short}
    The accessor for the @slot[gtk:box]{baseline-child} slot of the
    @class{gtk:box} class gets or sets the position of the child widget that
    determines the baseline of the box in vertical layout.
  @end{short}
  This is only relevant if the box is in vertical orientation.

  Since 4.12
  @see-class{gtk:box}")

;;; --- gtk:box-baseline-position ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "baseline-position" 'box) t)
 "The @code{baseline-position} property of type @sym{gtk:baseline-position}
  (Read / Write) @br{}
  The position of the baseline aligned widgets if extra space is available.@br{}
  Default value: @val[gtk:baseline-position]{:center}")

#+liber-documentation
(setf (liber:alias-for-function 'box-baseline-position)
      "Accessor"
      (documentation 'box-baseline-position 'function)
 "@version{2025-07-29}
  @syntax{(gtk:box-baseline-position object) => position}
  @syntax{(setf (gtk:box-baseline-position object) position)}
  @argument[object]{a @class{gtk:box} widget}
  @argument[position]{a value of the @sym{gtk:baseline-position} enumeration}
  @begin{short}
    The accessor for the @slot[gtk:box]{baseline-position} slot of the
    @class{gtk:box} class gets or sets the position of the baseline aligned
    widgets if extra space is available.
  @end{short}

  This affects only horizontal boxes with at least one baseline aligned child
  widget. If there is more vertical space available than requested, and the
  baseline is not allocated by the parent widget then the @arg{position} value
  is used to allocate the baseline with respect to the extra space available.
  @see-class{gtk:box}
  @see-symbol{gtk:baseline-position}")

;;; --- gtk:box-homogeneous ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "homogeneous" 'box) t)
 "The @code{homogeneous} property of type @code{:boolean} (Read / Write) @br{}
  Whether the children should all be the same size. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'box-homogeneous)
      "Accessor"
      (documentation 'box-homogeneous 'function)
 "@version{2025-07-29}
  @syntax{(gtk:box-homogeneous object) => homogeneous}
  @syntax{(setf (gtk:box-homogeneous object) homogeneous)}
  @argument[object]{a @class{gtk:box} widget}
  @argument[homogeneous]{@em{true} to create equal allotments, @em{false}
    for variable allotments}
  @begin{short}
    The accessor for the @slot[gtk:box]{homogeneous} slot of the @class{gtk:box}
    class gets or sets whether or not all children of the box are given equal
    space in the box.
  @end{short}
  @see-class{gtk:box}")

;;; --- gtk:box-spacing --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "spacing" 'box) t)
 "The @code{spacing} property of type @code{:int} (Read / Write) @br{}
  The amount of space between children. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'box-spacing)
      "Accessor"
      (documentation 'box-spacing 'function)
 "@version{2025-07-29}
  @syntax{(gtk:box-spacing object) => spacing}
  @syntax{(setf (gtk:box-spacing object) spacing)}
  @argument[object]{a @class{gtk:box} widget}
  @argument[spacing]{an integer for the number of pixels to put between
    children}
  @begin{short}
    The accessor for the @slot[gtk:box]{spacing} slot of the @class{gtk:box}
    class gets or sets the number of pixels to place between the children of
    the box.
  @end{short}
  @see-class{gtk:box}")

;;; ----------------------------------------------------------------------------
;;; gtk_box_new
;;; ----------------------------------------------------------------------------

(declaim (inline box-new))

(defun box-new (&optional (orientation :horizontal) (spacing 0))
 #+liber-documentation
 "@version{2025-06-26}
  @argument[orientation]{an optional @sym{gtk:orientation} value, the default
    is @val[gtk:orientation]{:horizontal}}
  @argument[spacing]{an optional integer for the number of pixels to place by
    default between children}
  @return{The new @class{gtk:box} widget.}
  @begin{short}
    Creates a new box with the given @arg{orientation} and an optional value
    for the @slot[gtk:box]{spacing} property.
  @end{short}
  The default value for the @arg{spacing} argument is 0.
  @see-class{gtk:box}
  @see-symbol{gtk:orientation}
  @see-function{gtk:box-spacing}"
  (make-instance 'box
                 :orientation orientation
                 :spacing spacing))

(export 'box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_box_append
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_box_append" box-append) :void
 #+liber-documentation
 "@version{2025-04-23}
  @argument[box]{a @class{gtk:box} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @short{Adds a child widget as the last child widget to the box.}
  @see-class{gtk:box}
  @see-class{gtk:widget}"
  (box (g:object box))
  (child (g:object widget)))

(export 'box-append)

;;; ----------------------------------------------------------------------------
;;; gtk_box_prepend
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_box_prepend" box-prepend) :void
 #+liber-documentation
 "@version{2025-04-23}
  @argument[box]{a @class{gtk:box} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @short{Adds a child widget as the first child widget to the box.}
  @see-class{gtk:box}
  @see-class{gtk:widget}"
  (box (g:object box))
  (child (g:object widget)))

(export 'box-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_box_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_box_remove" box-remove) :void
 #+liber-documentation
 "@version{2025-04-23}
  @argument[box]{a @class{gtk:box} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Removes a child widget from the box.
  @end{short}
  The child widget must have been added before with the @fun{gtk:box-append},
  @fun{gtk:box-prepend}, or @fun{gtk:box-insert-child-after} functions.
  @see-class{gtk:box}
  @see-class{gtk:widget}
  @see-function{gtk:box-append}
  @see-function{gtk:box-prepend}
  @see-function{gtk:box-insert-child-after}"
  (box (g:object box))
  (child (g:object widget)))

(export 'box-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_box_insert_child_after
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_box_insert_child_after" box-insert-child-after) :void
 #+liber-documentation
 "@version{2025-04-23}
  @argument[box]{a @class{gtk:box} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[sibling]{a @class{gtk:widget} sibling widget after which to insert
    @arg{child}}
  @begin{short}
    Inserts the child widget in the position after the sibling widget in the
    list of children in the box.
  @end{short}
  If the @arg{sibling} argument is @code{nil}, insert the child widget at the
  first position.
  @see-class{gtk:box}
  @see-class{gtk:widget}"
  (box (g:object box))
  (child (g:object widget))
  (sibling (g:object widget)))

(export 'box-insert-child-after)

;;; ----------------------------------------------------------------------------
;;; gtk_box_reorder_child_after
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_box_reorder_child_after" box-reorder-child-after) :void
 #+liber-documentation
 "@version{2025-04-23}
  @argument[box]{a @class{gtk:box} widget}
  @argument[child]{a @class{gtk:widget} child widget to move}
  @argument[sibling]{a @class{gtk:widget} sibling widget to move @arg{child}
    after}
  @begin{short}
    Moves the child widget to the position after @arg{sibling} in the list of
    box children.
  @end{short}
  If the @arg{sibling} argument is @code{nil}, move the child widget to the
  first position.
  @see-class{gtk:box}
  @see-class{gtk:widget}"
  (box (g:object box))
  (child (g:object widget))
  (sibling (g:object widget)))

(export 'box-reorder-child-after)

;;; --- End of file gtk4.box.lisp ----------------------------------------------
