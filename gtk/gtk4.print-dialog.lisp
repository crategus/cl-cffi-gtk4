;;; ----------------------------------------------------------------------------
;;; gtk4.print-dialog.lisp
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
;;;     GtkPrintDialog
;;;
;;; Accesors
;;;
;;;     gtk_print_dialog_get_accept_label
;;;     gtk_print_dialog_set_accept_label
;;;     gtk_print_dialog_get_modal
;;;     gtk_print_dialog_set_modal
;;;     gtk_print_dialog_get_page_setup
;;;     gtk_print_dialog_set_page_setup
;;;     gtk_print_dialog_get_print_settings
;;;     gtk_print_dialog_set_print_settings
;;;     gtk_print_dialog_get_title
;;;     gtk_print_dialog_set_title
;;;
;;; Functions
;;;
;;;     gtk_print_dialog_new
;;;
;;;     gtk_print_dialog_print                              not implemented
;;;     gtk_print_dialog_print_finish                       not implemented
;;;     gtk_print_dialog_print_file
;;;     gtk_print_dialog_print_file_finish
;;;     gtk_print_dialog_setup
;;;     gtk_print_dialog_setup_finish
;;;
;;; Properties
;;;
;;;     accept-label
;;;     modal
;;;     page-setup
;;;     print-settings
;;;     title
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPrintDialog
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkPrintDialog" print-dialog
  (:superclass g:object
    :export t
    :interfaces nil
    :type-initializer "gtk_print_dialog_get_type")
  ((accept-label
    print-dialog-accept-label
    "accept-label" "gchararray" t t)
   (modal
    print-dialog-modal
    "modal" "gboolean" t t)
   (page-setup
    print-dialog-page-setup
    "page-setup" "GtkPageSetup" t t)
   (print-settings
    print-dialog-print-settings
    "print-settings" "GtkPrintSettings" t t)
   (title
    print-dialog-title
    "title" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'print-dialog 'type)
 "@version{2024-11-10}
  @begin{short}
    The @class{gtk:print-dialog} object collects the arguments that are needed
    to present a print dialog to the user, such as a title for the dialog and
    whether it should be modal.
  @end{short}

  The dialog is shown with the @fun{gtk:print-dialog-setup} function. The actual
  printing can be done with the @fun{gtk:print-dialog-print-file} function.
  These APIs follows the GIO async pattern, and the results can be obtained by
  calling the corresponding finish methods.

  Since 4.14
  @see-constructor{gtk:print-dialog-new}
  @see-slot{gtk:print-dialog-accept-label}
  @see-slot{gtk:print-dialog-modal}
  @see-slot{gtk:print-dialog-page-setup}
  @see-slot{gtk:print-dialog-print-settings}
  @see-slot{gtk:print-dialog-title}
  @see-class{gtk:page-setup}
  @see-class{gtk:print-settings}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:print-dialog-accept-label ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accept-label" 'print-dialog) t)
 "The @code{accept-label} property of type @code{:string} (Read / Write) @br{}
  The label that may be shown on the accept button of a print dialog. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'print-dialog-accept-label)
      "Accessor"
      (documentation 'print-dialog-accept-label 'function)
 "@version{2025-08-12}
  @syntax{(gtk:print-dialog-accecpt-label object) => label}
  @syntax{(setf (gtk:print-dialog-accept-label object) label)}
  @argument[object]{a @class{gtk:print-dialog} object}
  @argument[label]{a string for the accept label}
  @begin{short}
    The accessor for the @slot[gtk:print-dialog]{accept-label} slot of the
    @class{gtk:print-dialog} class gets or sets the label that will be shown on
    the accept button of the print dialog.
  @end{short}

  Since 4.14
  @see-class{gtk:print-dialog}")

;;; --- gtk:print-dialog-modal -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modal" 'print-dialog) t)
 "The @code{modal} property of type @code{:boolean} (Read / Write) @br{}
  Whether the print dialog is modal. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'print-dialog-modal)
      "Accessor"
      (documentation 'print-dialog-modal 'function)
 "@version{2025-08-12}
  @syntax{(gtk:print-dialog-modal object) => setting}
  @syntax{(setf (gtk:print-dialog-modal object) setting)}
  @argument[object]{a @class{gtk:print-dialog} object}
  @argument[setting]{a boolean whether the print dialog is modal}
  @begin{short}
    The accessor for the @slot[gtk:print-dialog]{modal} slot of the
    @class{gtk:print-dialog} class gets or sets whether the print dialog blocks
    interaction with the parent window while it is presented.
  @end{short}

  Since 4.14
  @see-class{gtk:print-dialog}")

;;; --- gtk:print-dialog-page-setup --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "page-setup" 'print-dialog) t)
 "The @code{page-setup} property of type @class{gtk:page-setup} (Read / Write)
  @br{}
  The page setup to use.")

#+liber-documentation
(setf (liber:alias-for-function 'print-dialog-page-setup)
      "Accessor"
      (documentation 'print-dialog-page-setup 'function)
 "@version{2025-08-12}
  @syntax{(gtk:print-dialog-page-setup object) => setup}
  @syntax{(setf (gtk:print-dialog-page-setup object) setup)}
  @argument[object]{a @class{gtk:print-dialog} object}
  @argument[setup]{a @class{gtk:page-setup} object}
  @begin{short}
    The accessor for the @slot[gtk:print-dialog]{page-setup} slot of the
    @class{gtk:print-dialog} class gets or sets the page setup for the print
    dialog.
  @end{short}

  Since 4.14
  @see-class{gtk:print-dialog}
  @see-class{gtk:page-setup}")

;;; --- gtk:print-dialog-print-settings ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "print-settings"
                                               'print-dialog) t)
 "The @code{print-settings} property of type @class{gtk:print-settings}
 (Read / Write) @br{}
  The print settings to use.")

#+liber-documentation
(setf (liber:alias-for-function 'print-dialog-print-settings)
      "Accessor"
      (documentation 'print-dialog-print-settings 'function)
 "@version{2025-08-12}
  @syntax{(gtk:print-dialog-print-settings object) => settings}
  @syntax{(setf (gtk:print-dialog-print-settings object) settings)}
  @argument[object]{a @class{gtk:print-dialog} object}
  @argument[settings]{a @class{gtk:print-settings} object}
  @begin{short}
    The accessor for the @slot[gtk:print-dialog]{print-settings} slot of the
    @class{gtk:print-dialog} class gets or sets the print settings for the
    print dialog.
  @end{short}

  Since 4.14
  @see-class{gtk:print-dialog}
  @see-class{gtk:print-settings}")

;;; --- gtk:print-dialog-title -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'print-dialog) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title that may be shown on the print dialog. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'print-dialog-title)
      "Accessor"
      (documentation 'print-dialog-title 'function)
 "@version{2025-08-12}
  @syntax{(gtk:print-dialog-title object) => title}
  @syntax{(setf (gtk:print-dialog-title object) title)}
  @argument[object]{a @class{gtk:print-dialog} object}
  @argument[title]{a @class{gtk:print-settings} object}
  @begin{short}
    The accessor for the @slot[gtk:print-dialog]{title} slot of the
    @class{gtk:print-dialog} class gets or sets the title that will be shown on
    the print dialog.
  @end{short}

  Since 4.14
  @see-class{gtk:print-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_print_dialog_new
;;; ----------------------------------------------------------------------------

(defun print-dialog-new ()
 #+liber-documentation
 "@version{2024-11-10}
  @return{The new @class{gtk:print-dialog} object.}
  @short{Creates a new print dialog.}

  Since 4.14
  @see-class{gtk:print-dialog}"
  (make-instance 'print-dialog))

(export 'print-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_print_dialog_print                                  not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_dialog_print_finish                           not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_dialog_print_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_dialog_print_file" %print-dialog-print-file) :void
  (dialog (g:object print-dialog))
  (parent (g:object window))
  (setup (g:object page-setup))
  (file g:object)
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun print-dialog-print-file (dialog parent setup file cancellable func)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[dialog]{a @class{gtk:print-dialog} object}
  @argument[parent]{a parent @class{gtk:window} widget}
  @argument[setup]{a @class{gtk:print-setup} instance to use}
  @argument[file]{a @class{g:file} object to print}
  @argument[cancellable]{a @class{g:cancellable} instance to cancel the
    operation}
  @argument[func]{a @sym{g:async-ready-callback} callback funtion to call when
    the operation is complete}
  @begin{short}
    The @fun{gtk:print-dialog-print-file} function prints a file.
  @end{short}
  If you pass @code{nil} as print setup, then this method will present a print
  dialog. Otherwise, it will attempt to print directly, without user
  interaction.

  This method completes asynchronously. Use the
  @fun{gtk:print-dialog-print-file-finish} function inside the
  @sym{g:async-ready-callback} callback function to obtain the result of the
  operation.

  Since 4.14
  @see-class{gtk:print-dialog}
  @see-class{gtk:window}
  @see-class{gtk:page-setup}
  @see-class{g:file}
  @see-class{g:cancellable}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%print-dialog-print-file dialog
                              parent
                              setup
                              file
                              cancellable
                              (cffi:callback g:async-ready-callback)
                              ptr)))

(export 'print-dialog-print-file)

;;; ----------------------------------------------------------------------------
;;; gtk_print_dialog_print_file_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_dialog_print_file_finish"
               %print-dialog-print-file-finish) :boolean
  (dialog (g:object print-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun print-dialog-print-file-finish (dialog result)
 #+liber-documentation
 "@version{#2025-06-27}
  @argument[dialog]{a @class{gtk:print-dialog} object}
  @argument[result]{a @sym{g:async-result} instance}
  @return{The boolean whether the call was successful.}
  @begin{short}
    Finishes the @fun{gtk:print-dialog-print-file} function call and returns
    the results.
  @end{short}

  Since 4.14
  @see-class{gtk:print-dialog}
  @see-function{gtk:print-dialog-print-file}"
  (glib:with-ignore-error (err)
    (%print-dialog-print-file-finish dialog result err)))

(export 'print-dialog-print-file-finish)

;;; ----------------------------------------------------------------------------
;;; gtk_print_dialog_setup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_dialog_setup" %print-dialog-setup) :void
  (dialog (g:object print-dialog))
  (parent (g:object window))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun print-dialog-setup (dialog parent cancellable func)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[dialog]{a @class{gtk:print-dialog} object}
  @argument[parent]{a parent @class{gtk:window} widget}
  @argument[cancellable]{a @class{g:cancellable} instance to cancel the
    operation}
  @argument[func]{a @sym{g:async-ready-callback} callback funtion to call when
    the operation is complete}
  @begin{short}
    The @fun{gtk:print-dialog-setup} function presents a print dialog to let
    the user select a printer, and set up print settings and page setup.
  @end{short}
  The callback will be called when the dialog is dismissed. The obtained
  @class{gtk:print-setup} instance can then be passed to the
  @fun{gtk:print-dialog-print-file} function.

  One possible use for this method is to have the user select a printer, then
  show a page setup UI in the application, for example, to arrange images on a
  page, then call the @fun{gtk:print-dialog-print-file} function on @arg{dialog}
  to do the printing without further user interaction.

  This method completes asynchronously. Use the
  @fun{gtk:print-dialog-setup-finish} function inside the
  @sym{g:async-ready-callback} callback function to obtain the result of the
  operation.

  Since 4.14
  @see-class{gtk:print-dialog}
  @see-class{gtk:window}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gtk:print-dialog-print-file}
  @see-function{gtk:print-dialog-setup-finish}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%print-dialog-setup dialog
                         parent
                         cancellable
                         (cffi:callback g:async-ready-callback)
                         ptr)))

(export 'print-dialog-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_print_dialog_setup_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_dialog_setup_finish" %print-dialog-setup-finish)
    (g:boxed print-setup :return)
  (dialog (g:object print-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun print-dialog-setup-finish (dialog result)
 #+liber-documentation
 "@version{#2025-07-11}
  @argument[dialog]{a @class{gtk:print-dialog} object}
  @argument[result]{a @sym{g:async-result} instance}
  @begin{return}
    The @class{gtk:print-setup} instance that resulted from the call, or
    @code{nil} if the call was not successful.
  @end{return}
  @begin{short}
    Finishes the @fun{gtk:print-dialog-setup} function call.
  @end{short}
  If the call was successful, it returns a @class{gtk:print-setup} instance
  which contains the print settings and page setup information that will be
  used to print.

  Since 4.14
  @see-class{gtk:print-dialog}
  @see-class{g:async-result}
  @see-class{gtk:print-setup}
  @see-function{gtk:print-dialog-setup}"
  (glib:with-ignore-error (err)
    (%print-dialog-setup-finish dialog result err)))

(export 'print-dialog-setup-finish)

;;; --- End of file gtk4.print-dialog.lisp -------------------------------------
