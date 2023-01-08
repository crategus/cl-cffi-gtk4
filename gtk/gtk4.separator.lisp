;;; ----------------------------------------------------------------------------
;;; gtk.separator.lisp
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
;;; GtkSeparator
;;;
;;;     A separator widget
;;;
;;; Types and Values
;;;
;;;     GtkSeparator
;;;
;;; Functions
;;;
;;;     gtk_separator_new
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkSeparator
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
;;; GtkSeparator
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSeparator" separator
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_separator_get_type")
  nil)

#+liber-documentation
(setf (documentation 'separator 'type)
 "@version{#2022-9-9}
  @begin{short}
    The @sym{gtk:separator} widget is a horizontal or vertical separator widget.
  @end{short}

  @image[separator]{Figure: GtkSeparator}

  A @sym{gtk:separator} widget can be used to group the widgets within a window.
  It displays a line with a shadow to make it appear sunken into the interface.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:separator} implementation has a single CSS node with name
    @code{separator}. The node gets one of the @code{.horizontal} or
    @code{.vertical} style classes.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @sym{gtk:separator} implementation uses the @code{:separator} role of
    the @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @see-class{gtk:orientable}")

;;; ----------------------------------------------------------------------------
;;; gtk_separator_new
;;; ----------------------------------------------------------------------------

(declaim (inline separator-new))

(defun separator-new (orientation)
 #+liber-documentation
 "@version{#2022-9-9}
  @argument[orientation]{a value of the  @symbol{gtk:orientation} enumeration}
  @return{A new @class{gtk:separator} widget.}
  @begin{short}
    Creates a new separator widget with the given @arg{orientation}.
  @end{short}
  @see-class{gtk:separator}
  @see-symbol{gtk:orientation}"
  (make-instance 'separator
                 :orientation orientation))

(export 'separator-new)

;;; --- End of file gtk.separator.lisp -----------------------------------------
