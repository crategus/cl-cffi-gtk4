;;; ----------------------------------------------------------------------------
;;; gtk4.viewport.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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

(gobject:define-g-object-class "GtkViewport" viewport
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
 "@version{2023-8-6}
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
 "@version{2023-8-6}
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
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'viewport-scroll-to-focus)
      "Accessor"
      (documentation 'viewport-scroll-to-focus 'function)
 "@version{2023-8-6}
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
  @sym{(setf gtk:viewport-scroll-to-focus)} function sets whether the viewport 
  should automatically scroll to keep the focused child in view.
  @see-class{gtk:viewport}")

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline viewport-new))

(defun viewport-new (&optional (hadjustment nil) (vadjustment nil))
 #+liber-documentation
 "@version{2023-8-6}
  @argument[hadjustment]{a horizontal @class{gtk:adjustment} object}
  @argument[vadjustment]{a vertical @class{gtk:adjustment} object}
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

;;; --- End of file gtk4.viewport.lisp -----------------------------------------
