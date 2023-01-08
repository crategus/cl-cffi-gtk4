;;; ----------------------------------------------------------------------------
;;; gtk.box.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.9 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
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

(define-g-object-class "GtkBox" box
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_box_get_type")
  ((baseline-position
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
 "@version{2022-11-25}
  @begin{short}
    The @sym{gtk:box} widget arranges child widgets into a single row or
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
    The @sym{gtk:box} implementation uses a single CSS node with name
    @code{box}.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @sym{gtk:box} implementation uses the @code{:group} role of the
    @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @see-slot{gtk:box-baseline-position}
  @see-slot{gtk:box-homogeneous}
  @see-slot{gtk:box-spacing}
  @see-constructor{gtk:box-new}
  @see-class{gtk:widget}
  @see-class{gtk:orientable}
  @see-symbol{gtk:orientation}
  @see-symbol{gtk:accessible-role}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- box-baseline-position --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "baseline-position" 'box) t)
 "The @code{baseline-position} property of type @symbol{gtk:baseline-position}
  (Read / Write) @br{}
  The position of the baseline aligned widgets if extra space is available.@br{}
  Default value: @code{:center}")

#+liber-documentation
(setf (liber:alias-for-function 'box-baseline-position)
      "Accessor"
      (documentation 'box-baseline-position 'function)
 "@version{#2022-1-14}
  @syntax[]{(gtk:box-baseline-position object) => position}
  @syntax[]{(setf (gtk:box-baseline-position object) position)}
  @argument[object]{a @class{gtk:box} widget}
  @argument[position]{a value of the @symbol{gtk:baseline-position} enumeration}
  @begin{short}
    Accessor of the @slot[box]{baseline-position} slot of the
    @class{gtk:box} class.
  @end{short}
  The @sym{gtk:box-baseline-position} function gets the baseline position of a
  box. The @sym{(setf gtk:box-baseline-position)} function sets the baseline
  position.

  This affects only horizontal boxes with at least one baseline aligned child
  widget. If there is more vertical space available than requested, and the
  baseline is not allocated by the parent widget then the @arg{position} value
  is used to allocate the baseline with respect to the extra space available.
  @see-class{gtk:box}
  @see-symbol{gtk:baseline-position}")

;;; --- box-homogeneous --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "homogeneous" 'box) t)
 "The @code{homogeneous} property of type @code{:boolean} (Read / Write) @br{}
  Whether the children should all be the same size. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'box-homogeneous)
      "Accessor"
      (documentation 'box-homogeneous 'function)
 "@version{#2022-1-14}
  @syntax[]{(gtk:box-homogeneous object) => homogeneous}
  @syntax[]{(setf (gtk:box-homogeneous object) homogeneous)}
  @argument[object]{a @class{gtk:box} widget}
  @argument[homogeneous]{@em{true} to create equal allotments, @em{false}
    for variable allotments}
  @begin{short}
    Accessor of the @slot[box]{homogeneous} slot of the @class{gtk:box}
    class.
  @end{short}
  The @sym{gtk:box-homogeneous} function returns whether or not all children of
  the box are given equal space in the box. The @sym{(setf gtk:box-homogeneous)}
  function sets the property.
  @see-class{gtk:box}")

;;; --- box-spacing ------------------------------------------------------------

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
 "@version{#2022-1-14}
  @syntax[]{(gtk:box-spacing object) => spacing}
  @syntax[]{(setf (gtk:box-spacing object) spacing)}
  @argument[object]{a @class{gtk:box} widget}
  @argument[spacing]{an integer with the number of pixels to put between
    children}
  @begin{short}
    Accessor of the @slot[gtk:box]{spacing} slot of the @class{gtk:box} class.
  @end{short}
  The @sym{gtk:box-spacing} function returns the spacing between children. The
  @sym{(setf gtk:box-spacing)} function sets the number of pixels to place
  between the children of the box.
  @see-class{gtk:box}")

;;; ----------------------------------------------------------------------------
;;; gtk_box_new
;;; ----------------------------------------------------------------------------

(declaim (inline box-new))

(defun box-new (orientation &optional (spacing 0))
 #+liber-documentation
 "@version{#2022-1-14}
  @argument[orientation]{an orientation of type @symbol{gtk:orientation}}
  @argument[spacing]{an optional integer with the number of pixels to place by
    default between children}
  @return{A new @class{gtk:box} widget.}
  @begin{short}
    Creates a new box with the given @arg{orientation} and an optional value
    for the @slot[box]{spacing} property.
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

(defcfun ("gtk_box_append" box-append) :void
 #+liber-documentation
 "@version{2022-11-25}
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

(defcfun ("gtk_box_prepend" box-prepend) :void
 #+liber-documentation
 "@version{#2022-1-14}
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

(defcfun ("gtk_box_remove" box-remove) :void
 #+liber-documentation
 "@version{#2022-1-14}
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

(defcfun ("gtk_box_insert_child_after" box-insert-child-after) :void
 #+liber-documentation
 "@version{#2022-1-14}
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

(defcfun ("gtk_box_reorder_child_after" box-reorder-child-after) :void
 #+liber-documentation
 "@version{#2022-1-14}
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

;;; --- End of file gtk.box.lisp -----------------------------------------------
