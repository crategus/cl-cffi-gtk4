;;; ----------------------------------------------------------------------------
;;; gtk4.accessible-range.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GtkAccessibleRange
;;;
;;; Prerequiste
;;;
;;;     GtkAccessible
;;; ----------------------------------------------------------------------------

(in-package :gtk)

(gobject:define-g-interface "GtkAccessibleRange" accessible-range
  (:superclass accessible
   :export t
   :type-initializer "gtk_accessible_range_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'accessible-range)
      "Interface"
      (documentation 'accessible-range 'type)
 "@version{2023-8-24}
  @begin{short}
    The @class{gtk:accessible-range} interface describes ranged controls, e.g.
    controls which have a single value within an allowed range and that can
    optionally be changed by the user.
  @end{short}
  The interface is expected to be implemented by controls using the following
  roles:
  @begin{itemize}
    @item{GTK_ACCESSIBLE_ROLE_METER}
    @item{GTK_ACCESSIBLE_ROLE_PROGRESS_BAR}
    @item{GTK_ACCESSIBLE_ROLE_SCROLLBAR}
    @item{GTK_ACCESSIBLE_ROLE_SLIDER}
    @item{GTK_ACCESSIBLE_ROLE_SPIN_BUTTON}
  @end{itemize}
  If that is not the case, a warning will be issued at run time.

  In addition to this interface, its implementors are expected to provide the
  correct values for the following properties:
  @begin{itemize}
    @item{GTK_ACCESSIBLE_PROPERTY_VALUE_MAX}
    @item{GTK_ACCESSIBLE_PROPERTY_VALUE_MIN}
    @item{GTK_ACCESSIBLE_PROPERTY_VALUE_NOW}
    @item{GTK_ACCESSIBLE_PROPERTY_VALUE_TEXT}
  @end{itemize}
  Since 4.10
  @see-class{gtk:accessible}")

;;; --- End of file gtk4.accessible-range.lisp ---------------------------------
