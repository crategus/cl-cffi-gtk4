;;; ----------------------------------------------------------------------------
;;; gtk.widget-paintable.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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
;;; GtkWidgetPaintable
;;;
;;;     Drawing a widget elsewhere
;;;
;;; Types and Values
;;;
;;;     GtkWidgetPaintable
;;;
;;; Accessors
;;;
;;;     gtk_widget_paintable_get_widget
;;;     gtk_widget_paintable_set_widget
;;;
;;; Functions
;;;
;;;     gtk_widget_paintable_new
;;;
;;; Properties
;;;
;;;     widget
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTooltip
;;;
;;; Implemented Interfaces
;;;
;;;     GdkPaintable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkWidgetPaintable
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkWidgetPaintable" widget-paintable
  (:superclass g:object
   :export t
   :interfaces ("GdkPaintable")
   :type-initializer "gtk_widget_paintable_get_type")
  ((widget
    widget-paintable-widget
    "widget" "GtkWidget" t t)))

(setf (documentation 'widget-paintable 'type)
 "@version{#2022-7-10}
  @begin{short}
    The @sym{gtk:widget-paintable} object is an implementation of the
    the @class{gdk:paintable} interface that allows displaying the contents of
    a @class{gtk:widget} widget.
  @end{short}

  The @sym{gtk:widget-paintable} object will also take care of the widget not
  being in a state where it can be drawn, like when it is not shown, and just
  draw nothing or where it does not have a size, like when it is hidden, and
  report no size in that case.

  Of course, the @sym{gtk:widget-paintable} object allows you to monitor widgets
  for size changes by emitting the \"invalidate-size\" signal whenever the size
  of the widget changes as well as for visual changes by emitting the
  \"invalidate-contents\" signal whenever the widget changes.

  You can of course use a @sym{gtk:widget-paintable} object everywhere a
  @class{gdk:paintable} object is allowed, including using it on a
  @class{gtk:picture} widget, or one of its parents, that it was set on itself
  via the @fun{gtk:picture-paintable} function. The paintable will take care of
  recursion when this happens. If you do this however, ensure the
  @slot[gtk:picture]{can-shrink} property is set to @em{true} or you might end
  up with an infinitely growing widget.
  @see-class{gdk:paintable}
  @see-class{gtk:widget}
  @see-class{gtk:picture}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- widget-paintable-widget ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "widget" 'widget-paintable) t)
 "The @code{widget} property of type @class{gtk:widget} (Read / Write) @br{}
  The observed widget or @code{nil} if none.")

#+liber-documentation
(setf (liber:alias-for-function 'widget-paintable-widget)
      "Accessor"
      (documentation 'widget-paintable-widget 'function)
 "@version{#2022-7-10}
  @syntax[]{(gtk:widget-paintable-widget object) => widget}
  @syntax[]{(setf (gtk:widget-paintable-widget object) widget)}
  @argument[object]{a @class{gtk:widget-paintable} object}
  @argument[widget]{a @class{gtk:widget} widget to observe or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:widget-paintable]{widget} slot of the
    @class{gtk:widget-paintable} object.
  @end{short}

  The @sym{gtk:widget-paintable-widget} function returns the widget that is
  observed or @code{nil} if none. The @sym{(setf gtk:widget-paintable-widget)}
  function sets the widget that should be observed.
  @see-class{gtk:widget-paintable}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_paintable_new ()
;;; ----------------------------------------------------------------------------

(defun widget-paintable-new (widget)
 #+liber-documentation
 "@version{#2022-7-10}
  @argument[widget]{a @class{gtk:widget} widget or @code{nil}}
  @return{A new @class{gtk:widget-paintable} object.}
  @short{Creates a new paintable widget observing the given @arg{widget}.}
  @see-class{gtk:widget-paintable}
  @see-class{gtk:widget}"
  (make-instance 'widget-paintable
                 :widget widget))

(export 'widget-paintable-new)

;;; --- End of file gtk.widget-paintable.lisp ----------------------------------
