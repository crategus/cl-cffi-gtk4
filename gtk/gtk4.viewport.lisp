;;; ----------------------------------------------------------------------------
;;; gtk.viewport.lisp
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
;;; GtkViewport
;;;
;;;     An adapter which makes widgets scrollable
;;;
;;; Types and Values
;;;
;;;     GtkViewport
;;;
;;; Accessors
;;;
;;;     gtk_viewport_set_child
;;;     gtk_viewport_get_child
;;;     gtk_viewport_set_scroll_to_focus
;;;     gtk_viewport_get_scroll_to_focus
;;;
;;; Functions
;;;
;;;     gtk_viewport_new
;;;
;;; Properties
;;;
;;;     child
;;;     scroll-to-focus
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkViewport
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkScrollable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkViewport
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkViewport" viewport
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkScrollable")
   :type-initializer "gtk_viewport_get_type")
  ((child
    viewport-child
    "child" "GtkWidget" t t)
   (scroll-to-focus
    viewport-scroll-to-focus
    "scroll-to-focus" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'viewport 'type)
 "@version{#2021-3-19}
  @begin{short}
    The @sym{gtk:viewport} widget acts as an adaptor class, implementing
    scrollability for child widgets that lack their own scrolling capabilities.
  @end{short}
  Use the @sym{gtk:viewport} widget to scroll child widgets such as the widgets
  @class{gtk:grid}, @class{gtk:box}, and so on.

  The @sym{gtk:viewport} widget will start scrolling content only if allocated
  less than the minimum size of the child widget in a given orientation.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:viewport} widget has a single CSS node with name
    @code{viewport}.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @sym{gtk:viewport} implementation uses the @code{:group} role of the
    @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @see-slot{gtk:viewport-child}
  @see-slot{gtk:viewport-scroll-to-focus}
  @see-class{gtk:scrolled-window}
  @see-class{gtk:scrollable}
  @see-class{gtk:adjustment}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- viewport-child -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'viewport) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget of the viewport.")

#+liber-documentation
(setf (liber:alias-for-function 'viewport-child)
      "Accessor"
      (documentation 'viewport-child 'function)
 "@version{#2022-2-5}
  @syntax[]{(gtk:viewport-child object) => child}
  @syntax[]{(setf (gtk:viewport-child object) child)}
  @argument[object]{a @class{gtk:viewport} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:viewport]{child} slot of the
    @class{gtk:viewport} class.
  @end{short}
  @see-class{gtk:viewport}
  @see-class{gtk:widget}")

;;; --- viewport-scroll-to-focus -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scroll-to-focus"
                                               'viewport) t)
 "The @code{scroll-to-focus} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to scroll when the focus changes. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'viewport-scroll-to-focus)
      "Accessor"
      (documentation 'viewport-scroll-to-focus 'function)
 "@version{#2022-2-5}
  @syntax[]{(gtk:viewport-scroll-to-focus object) => setting}
  @syntax[]{(setf (gtk:viewport-scroll-to-focus object) setting)}
  @argument[object]{a @class{gtk:viewport} widget}
  @argument[setting]{a boolean whether to keep the focus widget scrolled to
    view}
  @begin{short}
    Accessor of the @slot[gtk:viewport]{scroll-to-focus} slot of the
    @class{gtk:viewport} class.
  @end{short}

  The @sym{gtk:viewport-scroll-to-focus} function gets whether the viewport is
  scrolling to keep the focused child in view. The
  @sym{gtk:viewport-scroll-to-focus} function sets whether the viewport should
  automatically scroll to keep the focused child in view.
  @see-class{gtk:viewport}")

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline viewport-new))

(defun viewport-new (&optional (hadjustment nil) (vadjustment nil))
 #+liber-documentation
 "@version{#2021-3-18}
  @argument[hadjustment]{horizontal @class{gtk:adjustment} object}
  @argument[vadjustment]{vertical @class{gtk:adjustment} object}
  @return{A new @class{gtk:viewport} widget.}
  @begin{short}
    Creates a new viewport with the given adjustments, of with default
    adjustments if none are given.
  @end{short}
  @see-class{gtk:viewport}
  @see-class{gtk:adjustment}"
  (make-instance 'viewport
                 :hadjustment hadjustment
                 :vadjustment vadjustment))

(export 'viewport-new)

;;; --- End of file gtk.viewport.lisp ------------------------------------------
