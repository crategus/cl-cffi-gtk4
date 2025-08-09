;;; ----------------------------------------------------------------------------
;;; gtk4.viewport.lisp
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
;;;     gtk_viewport_scroll_to                              Since 4.12
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
;;; GtkViewport
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkViewport" viewport
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
 "@version{2025-07-25}
  @begin{short}
    The @class{gtk:viewport} widget acts as an adaptor class, implementing
    scrollability for child widgets that lack their own scrolling capabilities.
  @end{short}
  Use the @class{gtk:viewport} widget to scroll child widgets such as the
  widgets @class{gtk:grid}, @class{gtk:box}, and so on.

  The @class{gtk:viewport} widget will start scrolling content only if allocated
  less than the minimum size of the child widget in a given orientation.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:viewport} widget has a single CSS node with name
    @code{viewport}.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:viewport} implementation uses the
    @val[gtk:accessible-role]{:group} role of the @sym{gtk:accessible-role}
    enumeration.
  @end{dictionary}
  @see-slot{gtk:viewport-child}
  @see-slot{gtk:viewport-scroll-to-focus}
  @see-class{gtk:scrolled-window}
  @see-class{gtk:scrollable}
  @see-class{gtk:adjustment}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:viewport-child -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'viewport) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget of the viewport.")

#+liber-documentation
(setf (liber:alias-for-function 'viewport-child)
      "Accessor"
      (documentation 'viewport-child 'function)
 "@version{2024-07-05}
  @syntax{(gtk:viewport-child object) => child}
  @syntax{(setf (gtk:viewport-child object) child)}
  @argument[object]{a @class{gtk:viewport} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:viewport]{child} slot of the
    @class{gtk:viewport} class.
  @end{short}
  @see-class{gtk:viewport}
  @see-class{gtk:widget}")

;;; --- gtk:viewport-scroll-to-focus -------------------------------------------

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
 "@version{2024-07-05}
  @syntax{(gtk:viewport-scroll-to-focus object) => setting}
  @syntax{(setf (gtk:viewport-scroll-to-focus object) setting)}
  @argument[object]{a @class{gtk:viewport} widget}
  @argument[setting]{a boolean whether to keep the focus widget scrolled to
    view}
  @begin{short}
    Accessor of the @slot[gtk:viewport]{scroll-to-focus} slot of the
    @class{gtk:viewport} class.
  @end{short}
  The @fun{gtk:viewport-scroll-to-focus} function gets whether the viewport is
  scrolling to keep the focused child in view. The
  @setf{gtk:viewport-scroll-to-focus} function sets whether the viewport should
  automatically scroll to keep the focused child in view.
  @see-class{gtk:viewport}")

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_new
;;; ----------------------------------------------------------------------------

(declaim (inline viewport-new))

(defun viewport-new (&optional (hadjustment nil) (vadjustment nil))
 #+liber-documentation
 "@version{2024-07-05}
  @argument[hadjustment]{an optional horizontal @class{gtk:adjustment} object}
  @argument[vadjustment]{an optional vertical @class{gtk:adjustment} object}
  @return{The new @class{gtk:viewport} widget.}
  @begin{short}
    Creates a new viewport with the given adjustments, or with default
    adjustments if none are given.
  @end{short}
  @see-class{gtk:viewport}
  @see-class{gtk:adjustment}"
  (make-instance 'viewport
                 :hadjustment hadjustment
                 :vadjustment vadjustment))

(export 'viewport-new)

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_scroll_to                                  Since 4.12
;;; ----------------------------------------------------------------------------

#+gtk-4-12
(cffi:defcfun ("gtk_viewport_scroll_to" %viewport-scroll-to) :void
  (viewport (g:object viewport))
  (descendant (g:object widget))
  (scroll (g:boxed scroll-info)))

#+gtk-4-12
(defun viewport-scroll-to (viewport descendant &optional scroll)
 #+liber-documentation
 "@version{2025-04-07}
  @argument[viewport]{a @class{gtk:viewport} widget}
  @argument[descendant]{a descendant @class{gtk:widget} object of the viewport}
  @argument[scroll]{an optional @class{gtk:scroll-info} instance, or the default
  @code{nil} value to scroll into view}
  @begin{short}
    Scrolls a descendant of the viewport into view.
  @end{short}
  The viewport and the descendant widget must be visible and mapped for this
  function to work, otherwise no scrolling will be performed.

  Since 4.12
  @see-class{gtk:viewport}
  @see-class{gtk:widget}
  @see-class{gtk:scroll-info}"
  (%viewport-scroll-to viewport descendant scroll))

#+gtk-4-12
(export 'viewport-scroll-to)

;;; --- End of file gtk4.viewport.lisp -----------------------------------------
