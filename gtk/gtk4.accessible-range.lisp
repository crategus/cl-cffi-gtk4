;;; ----------------------------------------------------------------------------
;;; gtk4.accessible-range.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2024 Dieter Kaiser
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
;;; Functions
;;;
;;;     gtk_accessible_range_set_current_value
;;;
;;; Prerequiste
;;;
;;;     GtkAccessible
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAccessibleRange
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkAccessibleRange" accessible-range
  (:superclass accessible
   :export t
   :type-initializer "gtk_accessible_range_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'accessible-range)
      "Interface"
      (documentation 'accessible-range 'type)
 "@version{2024-11-5}
  @begin{short}
    The @class{gtk:accessible-range} interface describes ranged controls, for
    example, controls which have a single value within an allowed range and
    that can optionally be changed by the user.
  @end{short}
  The interface is expected to be implemented by controls using the following
  roles of the @symbol{gtk:accessible-role} enumeration:
  @begin{itemize}
    @item{:meter}
    @item{:progress-bar}
    @item{:scrollbar}
    @item{:slider}
    @item{:spin-button}
  @end{itemize}
  If that is not the case, a warning will be issued at run time.

  In addition to this interface, its implementors are expected to provide the
  correct values for the following properties of the
  @symbol{gtk:accessible-property} enumeration:
  @begin{itemize}
    @item{:value-max}
    @item{:value-min}
    @item{:value-now}
    @item{:value-text}
  @end{itemize}
  Since 4.10
  @see-class{gtk:accessible}
  @see-symbol{gtk:accessible-role}
  @see-symbol{gtk:accessible-property}")

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_range_set_current_value                  Since 4.10
;;; ----------------------------------------------------------------------------

;; FIXME: This function is not exported from the C library. The C function
;; does nothing and returns TRUE. Consider to implement the virtual function
;; for the interface from the Lisp side.

#+nil
(defun accessible-range-set-current-value (accessible value)
 #+liber-documentation
 "@version{#2024-11-5}
  @argument[accessible]{a @class{gtk:accessible-range} object}
  @argument[value]{a number coerced to a double float for the value}
  @begin{short}
    Sets the current value of the accessible range.
  @end{short}
  This operation should behave similarly as if the user performed the action.

  Since 4.10
  @see-class{gtk:accessible-range}"
  (cffi:foreign-funcall "gtk_accessible_range_default_set_current_value"
                        (g:object accessible-range) accessible
                        :double (coerce value 'double-float)
                        :boolean))

#+nil
(export 'accessible-range-set-current-value)

;;; --- End of file gtk4.accessible-range.lisp ---------------------------------
