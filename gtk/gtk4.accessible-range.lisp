;;; ----------------------------------------------------------------------------
;;; gtk4.accessible-range.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.22 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2026 Dieter Kaiser
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
 "@version{2026-08-26}
  @begin{short}
    The @class{gtk:accessible-range} interface describes ranged controls, for
    example, controls which have a single value within an allowed range and
    that can optionally be changed by the user.
  @end{short}
  The interface is expected to be implemented by controls using the following
  roles of the @sym{gtk:accessible-role} enumeration:
  @begin{itemize}
    @item{@val[gtk:accessible-role]{:meter}}
    @item{@val[gtk:accessible-role]{:progress-bar}}
    @item{@val[gtk:accessible-role]{:scrollbar}}
    @item{@val[gtk:accessible-role]{:slider}}
    @item{@val[gtk:accessible-role]{:spin-button}}
  @end{itemize}
  If that is not the case, a warning will be issued at run time. In addition
  to this interface, its implementors are expected to provide the correct
  values for the following properties of the @sym{gtk:accessible-property}
  enumeration:
  @begin{itemize}
    @item{@val[gtk:accessible-property]{:value-max}}
    @item{@val[gtk:accessible-property]{:value-min}}
    @item{@val[gtk:accessible-property]{:value-now}}
    @item{@val[gtk:accessible-property]{:value-text}}
  @end{itemize}
  Since 4.10
  @see-class{gtk:accessible}
  @see-symbol{gtk:accessible-role}
  @see-symbol{gtk:accessible-property}")

;;; ----------------------------------------------------------------------------
;;; GtkAccessibleRange Inferface vtable
;;; ----------------------------------------------------------------------------

;; TODO: Improve the implementation of the DEFINE-VTABLE macro. For this
;; interface we do not have access from the Lisp side to the default virtual
;; method implementation in the C code. Therefore, we have implemented the
;; default callback from the Lisp side. Ideally, it would be better to use the
;; default implementation in the C code. The idea is to use the keyword :DEFAULT
;; to invoke the C function, which is installed in the C code. We already have
;; code that can chain up to the installed virtual methods in the C code.
;; Further generalisation seems natural.

(gobject:define-vtable ("GtkAccessibleRange" accessible-range)
  (:skip parent-instance (:struct g:type-interface))
  ;; Methods of the GtkAccessibleRange interface
  (set-current-value (:boolean
                      (accessible (g:object accessible-range))
                      (value :double))))

;;; --- gtk:accessible-range-set-current-value-impl ----------------------------

;; This is the default implementation.

(defmethod accessible-range-set-current-value-impl
           ((accessible accessible-range) value)
  (declare (ignorable accessible value))
  t)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_range_set_current_value                  Since 4.10
;;; ----------------------------------------------------------------------------

(defun accessible-range-set-current-value (accessible value)
 #+liber-documentation
 "@version{2026-08-26}
  @argument[accessible]{a @class{gtk:accessible-range} object}
  @argument[value]{a number coerced to a double float for the value}
  @return{@em{True} if the operation was performed, false otherwise.}
  @begin{short}
    Sets the current value of the accessible range.
  @end{short}
  This operation should behave similarly as if the user performed the action.

  Since 4.10
  @begin[Notes]{dictionary}
    The default implementation of the virtual method does nothing and returns
    @em{true}.
  @end{dictionary}
  @see-class{gtk:accessible-range}"
  (accessible-range-set-current-value-impl accessible
                                           (coerce value 'double-float)))

(export 'accessible-range-set-current-value)

;;; --- End of file gtk4.accessible-range.lisp ---------------------------------
