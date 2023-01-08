;;; ----------------------------------------------------------------------------
;;; gtk.scrollbar.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK+ library.
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
;;; GtkScrollbar
;;;
;;;     A Scrollbar
;;;
;;; Types and Values
;;;
;;;     GtkScrollbar
;;;
;;; Accessors
;;;
;;;     gtk_scrollbar_get_adjustment
;;;     gtk_scrollbar_set_adjustment
;;;
;;; Functions
;;;
;;;     gtk_scrollbar_new
;;;
;;; Properties
;;;
;;;     adjustment
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkScrollbar
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
;;; struct GtkScrollbar
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkScrollbar" scrollbar
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_scrollbar_get_type")
  ((adjustment
    scrollbar-adjustment
    "adjustment" "GtkAdjustment" t t)))

#+liber-documentation
(setf (documentation 'scrollbar 'type)
 "@version{#2021-12-27}
  @begin{short}
    The @sym{gtk:scrollbar} widget is a horizontal or vertical scrollbar,
    depending on the value of the @slot[orientable]{orientation} property.
  @end{short}

  @image[scrollbar]{Figure: GtkScrollbar}

  Its position and movement are controlled by the adjustment that is passed to
  or created by the @fun{gtk:scrollbar-new} function. See the
  @class{gtk:adjustment} documentation for more details. The
  @slot[gtk:adjustment]{value} field sets the position of the thumb and must be
  between @slot[gtk:adjustment]{lower} and @slot[gtk:adjustment]{upper} -
  @slot[gtk:adjustment]{page-size}. The @slot[gtk:adjustment]{page-size}
  represents the size of the visible scrollable area. The fields
  @slot[gtk:adjustment]{step-increment} and
  @slot[gtk:adjustment]{page-increment} fields are added to or subtracted from
  the @slot[gtk:adjustment]{value} when the user asks to move by a step, using
  e.g. the cursor arrow keys, or by a page, using e.g. the @kbd{Page Down}/
  @kbd{Page Up} keys.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
scrollbar
╰── range[.fine-tune]
    ╰── trough
        ╰── slider
    @end{pre}
    The @sym{gtk:scrollbar} implementation has a main CSS node with name
    @code{scrollbar} and a subnode for its contents. The main node gets
    the @code{.horizontal} or @code{.vertical} style classes applied, depending
    on the scrollbar's orientation.

    The range node gets the @code{.fine-tune} style class added when the
    scrollbar is in 'fine-tuning' mode.

    Other style classes that may be added to scrollbars inside the
    @class{gtk:scrolled-window} widget include the positional classes
    @code{.left}, @code{.right}, @code{.top}, @code{.bottom}, and style classes
    related to overlay scrolling, @code{.overlay-indicator},
    @code{.dragging}, @code{.hovering}.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @sym{gtk:scrollbar} implementation uses the @code{:scrollbar} role
    of the @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @see-slot{gtk:scrollbar-adjustment}
  @see-class{gtk:adjustment}
  @see-class{gtk:scrolled-window}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- scrollbar-adjustment -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "adjustment" 'scrollbar) t)
 "The @code{adjustment} property of type @class{gtk:adjustment}
  (Read / Write / Contruct) @br{}
  The adjustment that contains the current value of this scrollbar.")

#+liber-documentation
(setf (liber:alias-for-function 'scrollbar-adjustment)
      "Accessor"
      (documentation 'scrollbar-adjustment 'function)
 "@version{#2022-6-16}
  @syntax[]{(gtk:scrollbar-adjustment object) => adjustment}
  @syntax[]{(setf (gtk:scrollbar-adjustment object) adjustment)}
  @argument[object]{a @class{gtk:scrollbar} widget}
  @argument[adjustment]{a @class{gtk:adjustment} object}
  @begin{short}
    Accessor of the @slot[gtk:scrollbar]{adjustment} slot of the
    @class{gtk:scrollbar} class.
  @end{short}

  The @sym{gtk:scrolled-window-child} function returns the adjustment of the
  adjustment. The @sym{(setf gtk:scrolled-window-child)} function makes the
  scrollbar use the given adjustment.
  @see-class{gtk:scrollbar}
  @see-class{gtk:adjustment}")

;;; ----------------------------------------------------------------------------
;;; gtk_scrollbar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline scrollbar-new))

(defun scrollbar-new (orientation &optional (adjustment nil))
 #+liber-documentation
 "@version{#2021-12-27}
  @argument[orientation]{a @symbol{gtk:orientation} value for the orientation
    of the scrollbar}
  @argument[adjustment]{an optional @class{gtk:adjustment} object to use,
    the default is to create a new adjustment}
  @return{The new @class{gtk:scrollbar} widget.}
  @short{Creates a new scrollbar with the given @arg{orientation}.}
  @see-class{gtk:scrollbar}
  @see-class{gtk:adjustment}
  @see-symbol{gtk:orientation}"
  (make-instance 'scrollbar
                 :orientation orientation
                 :adjustment adjustment))

(export 'scrollbar-new)

;;; --- End of file gtk.scrollbar.lisp -----------------------------------------
