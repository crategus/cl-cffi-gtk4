;;; ----------------------------------------------------------------------------
;;; gtk.orientable.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
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
;;; GtkOrientable
;;;
;;;     An interface for flippable widgets
;;;
;;; Types and Values
;;;
;;;     GtkOrientable
;;;
;;; Accessors
;;;
;;;     gtk_orientable_get_orientation
;;;     gtk_orientable_set_orientation
;;;
;;; Properties
;;;
;;;     orientation
;;;
;;; Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkOrientable
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkOrientable" orientable
  (:export t
   :type-initializer "gtk_orientable_get_type")
  ((orientation
    orientable-orientation
    "orientation" "GtkOrientation" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'orientable)
      "Interface"
      (documentation 'orientable 'type)
 "@version{#2022-1-16}
  @begin{short}
    An interface for flippable widgets.
  @end{short}

  The @sym{gtk:orientable} interface is implemented by all widgets that can be
  oriented horizontally or vertically. A @sym{gtk:orientable} widget is more
  flexible in that it allows the orientation to be changed at runtime, allowing
  the widget to \"flip\".
  @see-slot{gtk:orientable-orientation}
  @see-symbol{gtk:orientation}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- orientable-orientation -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "orientation" 'orientable) t)
 "The @code{orientation} property of type @symbol{gtk:orientation}
  (Read / Write) @br{}
  The orientation of the orientable widget. @br{}
  Default value: @code{:horizontal}")

#+liber-documentation
(setf (liber:alias-for-function 'orientable-orientation)
      "Accessor"
      (documentation 'orientable-orientation 'function)
 "@version{#2022-1-16}
  @syntax[]{(gtk:orientable-orientation object) => orientation}
  @syntax[]{(setf (gtk:orientable-orientation object) orientation)}
  @argument[object]{a @class{gtk:orientable} widget}
  @argument[orientation]{a value of the @symbol{gtk:orientation} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:orientable]{orientation} slot of the
    @class{gtk:orientable} interface.
  @end{short}
  The @sym{gtk:orientable-orientation} function returns the orientation of the
  orientable widget. The @sym{(setf gtk:orientable-orientation)} function sets
  the orientation.
  @see-class{gtk:orientable}
  @see-symbol{gtk:orientation}")

;;; --- End of file gtk.orientable.lisp ----------------------------------------
