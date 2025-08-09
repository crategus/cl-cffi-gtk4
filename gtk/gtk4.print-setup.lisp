;;; ----------------------------------------------------------------------------
;;; gtk4.print-setup.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2024 - 2025 Dieter Kaiser
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
;;;     GtkPrintSetup
;;;
;;; Functions
;;;
;;;     gtk_print_setup_get_page_setup
;;;     gtk_print_setup_get_print_settings
;;;     gtk_print_setup_ref                                 not needed
;;;     gtk_print_setup_unref                               not needed
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPrintSetup
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque print-setup "GtkPrintSetup"
  :export t
  :type-initializer "gtk_print_setup_get_type"
  :alloc (error "GtkPrintSetup cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'print-setup)
      "GBoxed"
      (documentation 'print-setup 'type)
 "@version{2024-11-10}
  @begin{declaration}
(glib:define-gboxed-opaque print-setup \"GtkPrintSetup\"
  :export t
  :type-initializer \"gtk_print_setup_get_type\"
  :alloc (error \"GtkPrintSetup cannot be created from the Lisp side.\"))
  @end{declaration}
  @begin{short}
    The @class{gtk:print-setup} structure is an auxiliary structure for printing
    that allows decoupling the setup from the printing.
  @end{short}
  A print setup is obtained by calling the @fun{gtk:print-dialog-setup}
  function, and can later be passed to print functions such as the
  @fun{gtk:print-dialog-print-file} function.

  Print setups can be reused for multiple print calls.

  Applications may wish to store the page setup and print settings from the
  print setup and copy them to the print dialog if they want to keep using them.

  Since 4.14
  @see-class{gtk:print-dialog}
  @see-function{gtk:print-dialog-setup}
  @see-function{gtk:print-dialog-print-file}")

;;; ----------------------------------------------------------------------------
;;; gtk_print_setup_get_page_setup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_setup_get_page_setup" print-setup-page-setup)
    (g:object page-setup)
 #+liber-documentation
 "@version{#2024-11-10}
  @argument[setup]{a @class{gtk:print-setup} instance}
  @return{The @class{gtk:page-setup} instance, or @code{nil}.}
  @begin{short}
    Returns the page setup of @arg{setup}.
  @end{short}
  It may be different from the page setup of the @class{gtk:print-dialog}
  instance if the user changed it during the setup process.

  Since 4.14
  @see-class{gtk:print-setup}
  @see-class{gtk:page-setup}"
  (setup (g:boxed print-setup)))

(export 'print-setup-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_print_setup_get_print_settings
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_setup_get_print_settings" print-setup-print-settings)
    (g:object print-settings)
 #+liber-documentation
 "@version{#2024-11-10}
  @argument[setup]{a @class{gtk:print-setup} instance}
  @return{The @class{gtk:print-settings} instance, or @code{nil}.}
  @begin{short}
    Returns the print settings of @arg{setup}.
  @end{short}
  They may be different from the print settings of the @class{gtk:print-dialog}
  object if the user changed them during the setup process.

  Since 4.14
  @see-class{gtk:print-setup}
  @see-class{gtk:print-settings}"
  (setup (g:boxed print-setup)))

(export 'print-setup-print-settings)

;;; ----------------------------------------------------------------------------
;;; gtk_print_setup_ref                                     not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_setup_unref                                   not needed
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.print-setup.lisp --------------------------------------
