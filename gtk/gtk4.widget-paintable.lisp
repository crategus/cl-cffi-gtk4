;;; ----------------------------------------------------------------------------
;;; gtk4.widget-paintable.lisp
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
;;;     ╰── GtkWidgetPaintable
;;;
;;; Implemented Interfaces
;;;
;;;     GdkPaintable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkWidgetPaintable
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkWidgetPaintable" widget-paintable
  (:superclass g:object
   :export t
   :interfaces ("GdkPaintable")
   :type-initializer "gtk_widget_paintable_get_type")
  ((widget
    widget-paintable-widget
    "widget" "GtkWidget" t t)))

(setf (documentation 'widget-paintable 'type)
 "@version{2023-08-31}
  @begin{short}
    The @class{gtk:widget-paintable} object is an implementation of the
    the @class{gdk:paintable} interface that allows displaying the contents of
    a @class{gtk:widget} object.
  @end{short}

  The @class{gtk:widget-paintable} object will also take care of the widget not
  being in a state where it can be drawn, like when it is not shown, and just
  draw nothing or where it does not have a size, like when it is hidden, and
  report no size in that case.

  Of course, the @class{gtk:widget-paintable} object allows you to monitor
  widgets for size changes by emitting the @sig[gdk:paintable]{invalidate-size}
  signal whenever the size of the widget changes as well as for visual changes
  by emitting the @sig[gdk:paintable]{invalidate-contents} signal whenever the
  widget changes.

  You can of course use a @class{gtk:widget-paintable} object everywhere a
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

;;; --- gtk:widget-paintable-widget --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "widget" 'widget-paintable) t)
 "The @code{widget} property of type @class{gtk:widget} (Read / Write) @br{}
  The observed widget or @code{nil} if none.")

#+liber-documentation
(setf (liber:alias-for-function 'widget-paintable-widget)
      "Accessor"
      (documentation 'widget-paintable-widget 'function)
 "@version{2025-08-13}
  @syntax{(gtk:widget-paintable-widget object) => widget}
  @syntax{(setf (gtk:widget-paintable-widget object) widget)}
  @argument[object]{a @class{gtk:widget-paintable} object}
  @argument[widget]{a @class{gtk:widget} object to observe or @code{nil}}
  @begin{short}
    The accessor for the @slot[gtk:widget-paintable]{widget} slot of the
    @class{gtk:widget-paintable} object gets or sets the widget that is
    observed.
  @end{short}
  Returns @code{nil} if none.
  @see-class{gtk:widget-paintable}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_paintable_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_paintable_new" widget-paintable-new)
    (g:object widget-paintable :return)
 #+liber-documentation
 "@version{2024-11-02}
  @argument[widget]{a @class{gtk:widget} object or @code{nil}}
  @return{The new @class{gtk:widget-paintable} object.}
  @short{Creates a new paintable widget observing the given @arg{widget}.}
  @see-class{gtk:widget-paintable}
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-paintable-new)

;;; --- End of file gtk4.widget-paintable.lisp ---------------------------------
