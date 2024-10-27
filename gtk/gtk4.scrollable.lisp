;;; ----------------------------------------------------------------------------
;;; gtk4.scrollable.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2024 Dieter Kaiser
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
;;; GtkScrollable
;;;
;;;     An interface for scrollable widgets
;;;
;;; Types and Values
;;;
;;;     GtkScrollablePolicy
;;;     GtkScrollable
;;;
;;; Accessors
;;;
;;;     gtk_scrollable_get_hadjustment
;;;     gtk_scrollable_set_hadjustment
;;;     gtk_scrollable_get_hscroll_policy
;;;     gtk_scrollable_set_hscroll_policy
;;;     gtk_scrollable_get_vadjustment
;;;     gtk_scrollable_set_vadjustment
;;;     gtk_scrollable_get_vscroll_policy
;;;     gtk_scrollable_set_vscroll_policy
;;;
;;; Functions
;;;
;;;     gtk_scrollable_get_border
;;;
;;; Properties
;;;
;;;     hadjustment
;;;     hscroll-policy
;;;     vadjustment
;;;     vscroll-policy
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkScrollable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkScrollablePolicy
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkScrollablePolicy" scrollable-policy
  (:export t
   :type-initializer "gtk_scrollable_policy_get_type")
  (:minimum 0)
  (:natural 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'scrollable-policy)
      "GEnum"
      (liber:symbol-documentation 'scrollable-policy)
 "@version{2023-8-6}
  @begin{declaration}
(gobject:define-genum \"GtkScrollablePolicy\" scrollable-policy
  (:export t
   :type-initializer \"gtk_scrollable_policy_get_type\")
  (:minimum 0)
  (:natural 1))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:minimum]{Scrollable adjustments are based on the minimum size.}
      @entry[:natural]{Scrollable adjustments are based on the natural size.}
    @end{table}
  @end{values}
  @begin{short}
    Defines the policy to be used in a scrollable widget when updating the
    scrolled window adjustments in a given orientation.
  @end{short}
  @see-class{gtk:scrollable}")

;;; ----------------------------------------------------------------------------
;;; GtkScrollable
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkScrollable" scrollable
  (:export t
   :type-initializer "gtk_scrollable_get_type")
  ((hadjustment
    scrollable-hadjustment
    "hadjustment" "GtkAdjustment" t t)
   (hscroll-policy
    scrollable-hscroll-policy
    "hscroll-policy" "GtkScrollablePolicy" t t)
   (vadjustment
    scrollable-vadjustment
    "vadjustment" "GtkAdjustment" t t)
   (vscroll-policy
    scrollable-vscroll-policy
    "vscroll-policy" "GtkScrollablePolicy" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'scrollable)
      "Interface"
      (documentation 'scrollable 'type)
 "@version{2023-8-6}
  @begin{short}
    The @class{gtk:scrollable} interface is an interface that is implemented by
    widgets with native scrolling ability.
  @end{short}
  To implement this interface you should override the
  @slot[gtk:scrollable]{hadjustment} and
  @slot[gtk:scrollable]{vadjustment} properties.

  @subheading{Creating a scrollable widget}
  All scrollable widgets should do the following:
  @begin{itemize}
    @begin{item}
      When a parent widget sets the adjustments of the scrollable child widget
      the widget should populate the @slot[gtk:adjustment]{lower},
      @slot[gtk:adjustment]{upper}, @slot[gtk:adjustment]{step-increment},
      @slot[gtk:adjustment]{page-increment} and @slot[gtk:adjustment]{page-size}
      properties of the adjustment and connect to the @code{\"value-changed\"}
      signal.
    @end{item}
    @begin{item}
      Because its preferred size is the size for a fully expanded widget, the
      scrollable widget must be able to cope with underallocations. This means
      that it must accept any value passed to its
      @code{GtkWidgetClass.size_allocate()} function.
    @end{item}
    @begin{item}
      When the parent allocates space to the scrollable child widget, the
      widget should update the properties of the adjustments with new values.
    @end{item}
    @begin{item}
      When any of the adjustments emits the @code{\"value-changed\"} signal,
      the scrollable widget should scroll its contents.
    @end{item}
  @end{itemize}
  @see-slot{gtk:scrollable-hadjustment}
  @see-slot{gtk:scrollable-hscroll-policy}
  @see-slot{gtk:scrollable-vadjustment}
  @see-slot{gtk:scrollable-vscroll-policy}
  @see-class{gtk:scrollbar}
  @see-class{gtk:scrolled-window}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:scrollable-hadjustment ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hadjustment" 'scrollable) t)
 "The @code{hadjustment} property of type @class{gtk:adjustment}
  (Read / Write / Construct) @br{}
  The horizontal adjustment of the scrollable widget. This adjustment is shared
  between the scrollable widget and its parent.")

#+liber-documentation
(setf (liber:alias-for-function 'scrollable-hadjustment)
      "Accessor"
      (documentation 'scrollable-hadjustment 'function)
 "@version{2023-8-6}
  @syntax{(gtk:scrollable-hadjustment object) => hadjustment}
  @syntax{(setf (gtk:scrollable-hadjustment object) hadjustment)}
  @argument[object]{a @class{gtk:scrollable} widget}
  @argument[hadjustment]{a @class{gtk:adjustment} object}
  @begin{short}
    Accessor of the @slot[gtk:scrollable]{hadjustment} slot of the
    @class{gtk:scrollable} interface.
  @end{short}
  The @fun{gtk:scrollable-hadjustment} function retrieves the adjustment used
  for horizontal scrolling. The @setf{gtk:scrollable-hadjustment} function sets
  the horizontal adjustment.
  @see-class{gtk:scrollable}
  @see-class{gtk:adjustment}
  @see-function{gtk:scrollable-vadjustment}")

;;; --- gtk:scrollable-hscroll-policy ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hscroll-policy" 'scrollable) t)
 "The @code{hscroll-policy} property of type @symbol{gtk:scrollable-policy}
  (Read / Write) @br{}
  Determines whether horizontal scrolling should start once the scrollable
  widget is allocated less than its minimum width or less than its natural
  width. @br{}
  Default value: @code{:minimum}")

#+liber-documentation
(setf (liber:alias-for-function 'scrollable-hscroll-policy)
      "Accessor"
      (documentation 'scrollable-hscroll-policy 'function)
 "@version{2023-8-6}
  @syntax{(gtk:scrollable-hscroll-policy object) => policy}
  @syntax{(setf (gtk:scrollable-hscroll-policy object) policy)}
  @argument[object]{a @class{gtk:scrollable} widget}
  @argument[policy]{a @symbol{gtk:scrollable-policy} value for the horizontal
    scrolling policy}
  @begin{short}
    Accessor of the @slot[gtk:scrollable]{hscroll-policy} slot of the
    @class{gtk:scrollable} interface.
  @end{short}
  The @fun{gtk:scrollable-hscroll-policy} function gets the horizontal scrolling
  policy which determines whether horizontal scrolling should start below the
  minimum width or below the natural width. The
  @setf{gtk:scrollable-hscroll-policy} function sets the horizontal scrolling
  policy.
  @see-class{gtk:scrollable}
  @see-symbol{gtk:scrollable-policy}
  @see-function{gtk:scrollable-vscroll-policy}")

;;; --- gtk:scrollabe-vadjustment ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "vadjustment" 'scrollable) t)
 "The @code{vadjustment} property of type @class{gtk:adjustment}
  (Read / Write / Construct) @br{}
  The vertical adjustment of the scrollable widget. This adjustment is shared
  between the scrollable widget and its parent.")

#+liber-documentation
(setf (liber:alias-for-function 'scrollable-vadjustment)
      "Accessor"
      (documentation 'scrollable-vadjustment 'function)
 "@version{2023-8-6}
  @syntax{(gtk:scrollable-vadjustment object) => vadjustment}
  @syntax{(setf (gtk:scrollable-vadjustment object) vadjustment)}
  @argument[object]{a @class{gtk:scrollable} widget}
  @argument[vadjustment]{a @class{gtk:adjustment} object}
  @begin{short}
    Accessor of the @slot[gtk:scrollable]{vadjustment} slot of the
    @class{gtk:scrollable} interface.
  @end{short}
  The @fun{gtk:scrollable-vadjustment} function retrieves the adjustment used
  for vertical scrolling. The @setf{gtk:scrollable-vadjustment} function sets
  the vertical adjustment.
  @see-class{gtk:scrollable}
  @see-class{gtk:adjustment}
  @see-function{gtk:scrollable-hadjustment}")

;;; --- gtk:scrollable-vscroll-policy ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "vscroll-policy" 'scrollable) t)
 "The @code{vscroll-policy} property of type @symbol{gtk:scrollable-policy}
  (Read / Write) @br{}
  Determines whether vertical scrolling should start once the scrollable
  widget is allocated less than its minimum height or less than its natural
  height. @br{}
  Default value: @code{:minimum}")

#+liber-documentation
(setf (liber:alias-for-function 'scrollable-vscroll-policy)
      "Accessor"
      (documentation 'scrollable-vscroll-policy 'function)
 "@version{2023-8-6}
  @syntax{(gtk:scrollable-vscroll-policy object) => policy}
  @syntax{(setf (gtk:scrollable-vscroll-policy object) policy)}
  @argument[object]{a @class{gtk:scrollable} widget}
  @argument[policy]{a @symbol{gtk:scrollable-policy} value for the vertical
    scrolling policy}
  @begin{short}
    Accessor of the @slot[gtk:scrollable]{vscroll-policy} slot of the
    @class{gtk:scrollable} interface.
  @end{short}
  The @fun{gtk:scrollable-vscroll-policy} function gets the vertical scrolling
  policy which determines whether vertical scrolling should start below the
  minimum height or below the natural height. The
  @setf{gtk:scrollable-hscroll-policy} function sets the vertical scrolling
  policy.
  @see-class{gtk:scrollable}
  @see-symbol{gtk:scrollable-policy}
  @see-function{gtk:scrollable-hscroll-policy}")

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_get_border
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_scrollable_get_border" %scrollable-border) :boolean
  (scrollable (g:object scrollable))
  (border (g:boxed border)))

(defun scrollable-border (scrollable)
 "@version{2023-8-6}
  @argument[scrollable]{a @class{gtk:scrollable} widget}
  @return{The @class{gtk:border} instance or @code{nil}.}
  @begin{short}
    Returns the size of a non-scrolling border around the outside of the
    scrollable widget.
  @end{short}
  An example for this would be tree view headers. GTK can use this information
  to display overlayed graphics, like the overshoot indication, at the right
  position.
  @see-class{gtk:scrollable}
  @see-class{gtk:border}"
  (let ((border (border-new)))
    (when (%scrollable-border scrollable border)
      border)))

(export 'scrollable-border)

;;; --- End of file gtk4.scrollable.lisp ---------------------------------------
