;;; ----------------------------------------------------------------------------
;;; gtk4.scrollbar.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; GtkScrollbar
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkScrollbar" scrollbar
  (:superclass widget
   :export t
   :interfaces ("GtkAccessibleRange"
                "GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_scrollbar_get_type")
  ((adjustment
    scrollbar-adjustment
    "adjustment" "GtkAdjustment" t t)))

#+liber-documentation
(setf (documentation 'scrollbar 'type)
 "@version{2024-6-1}
  @begin{short}
    The @class{gtk:scrollbar} widget is a horizontal or vertical scrollbar,
    depending on the value of the @slot[gtk:orientable]{orientation} property.
  @end{short}

  @image[scrollbar]{Figure: GtkScrollbar}

  Its position and movement are controlled by the adjustment that is passed to
  or created by the @fun{gtk:scrollbar-new} function. See the
  @class{gtk:adjustment} documentation for more details. The
  @slot[gtk:adjustment]{value} property sets the position of the thumb and must
  be between the @slot[gtk:adjustment]{lower} and @slot[gtk:adjustment]{upper} -
  @slot[gtk:adjustment]{page-size} values. The @slot[gtk:adjustment]{page-size}
  property represents the size of the visible scrollable area. The
  @slot[gtk:adjustment]{step-increment} and
  @slot[gtk:adjustment]{page-increment} properties are added to or subtracted
  from the @slot[gtk:adjustment]{value} property when the user asks to move by
  a step, using, for example, the cursor arrow keys, or by a page, using, for
  example, the @kbd{Page Down}/@kbd{Page Up} keys.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
scrollbar
╰── range[.fine-tune]
    ╰── trough
        ╰── slider
    @end{pre}
    The @class{gtk:scrollbar} implementation has a main CSS node with name
    @code{scrollbar} and a subnode for its contents. The main node gets
    the @code{.horizontal} or @code{.vertical} style classes applied, depending
    on the orientation of the scrollbar. The range node gets the
    @code{.fine-tune} style class added when the scrollbar is in 'fine-tuning'
    mode.

    Other style classes that may be added to scrollbars inside the
    @class{gtk:scrolled-window} widget include the @code{.left}, @code{.right},
    @code{.top}, @code{.bottom} positional classes and
    @code{.overlay-indicator}, @code{.dragging}, @code{.hovering} style classes
    related to overlay scrolling.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:scrollbar} implementation uses the @code{:scrollbar} role
    of the @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @see-constructor{gtk:scrollbar-new}
  @see-slot{gtk:scrollbar-adjustment}
  @see-class{gtk:adjustment}
  @see-class{gtk:scrolled-window}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:scrollbar-adjustment -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "adjustment" 'scrollbar) t)
 "The @code{adjustment} property of type @class{gtk:adjustment}
  (Read / Write / Construct) @br{}
  The adjustment that contains the current value of this scrollbar.")

#+liber-documentation
(setf (liber:alias-for-function 'scrollbar-adjustment)
      "Accessor"
      (documentation 'scrollbar-adjustment 'function)
 "@version{2023-8-6}
  @syntax{(gtk:scrollbar-adjustment object) => adjustment}
  @syntax{(setf (gtk:scrollbar-adjustment object) adjustment)}
  @argument[object]{a @class{gtk:scrollbar} widget}
  @argument[adjustment]{a @class{gtk:adjustment} object}
  @begin{short}
    Accessor of the @slot[gtk:scrollbar]{adjustment} slot of the
    @class{gtk:scrollbar} class.
  @end{short}
  The @fun{gtk:scrollbar-adjustment} function returns the adjustment of the
  scrollbar. The @setf{gtk:scrollbar-adjustment} function makes the scrollbar
  use the given @arg{adjustment}.
  @see-class{gtk:scrollbar}
  @see-class{gtk:adjustment}")

;;; ----------------------------------------------------------------------------
;;; gtk_scrollbar_new
;;; ----------------------------------------------------------------------------

(declaim (inline scrollbar-new))

(defun scrollbar-new (orientation &optional (adjustment nil))
 #+liber-documentation
 "@version{2023-8-6}
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

;;; --- End of file gtk4.scrollbar.lisp ----------------------------------------
