;;; ----------------------------------------------------------------------------
;;; gtk4.alert-dialog.lisp
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
;;; GtkAlertDialog
;;;
;;;     Present a message to the user
;;;
;;; Types and Values
;;;
;;;     GtkAlertDialog
;;;
;;; Accessors
;;;
;;;     gtk_alert_dialog_get_buttons
;;;     gtk_alert_dialog_set_buttons
;;;     gtk_alert_dialog_get_cancel_button
;;;     gtk_alert_dialog_set_cancel_button
;;;     gtk_alert_dialog_get_default_button
;;;     gtk_alert_dialog_set_default_button
;;;     gtk_alert_dialog_get_detail
;;;     gtk_alert_dialog_set_detail
;;;     gtk_alert_dialog_get_message
;;;     gtk_alert_dialog_set_message
;;;     gtk_alert_dialog_get_modal
;;;     gtk_alert_dialog_set_modal
;;;
;;; Functions
;;;
;;;     gtk_alert_dialog_new
;;;     gtk_alert_dialog_choose
;;;     gtk_alert_dialog_choose_finish
;;;     gtk_alert_dialog_show
;;;
;;; Properties
;;;
;;;     buttons
;;;     cancel-button
;;;     default-button
;;;     detail
;;;     message
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkAlertDialog
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAlertDialog
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkAlertDialog" alert-dialog
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_alert_dialog_get_type")
  ((buttons
    alert-dialog-buttons
    "buttons" "GStrv" t t)
   (cancel-button
    alert-dialog-cancel-button
    "cancel-button" "gint" t t)
   (default-button
    alert-dialog-default-button
    "default-button" "gint" t t)
   (detail
    alert-dialog-detail
    "detail" "gchararray" t t)
   (message
    alert-dialog-message
    "message" "gchararray" t t)
   (modal
    alert-dialog-modal
    "modal" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'alert-dialog 'type)
 "@version{2024-4-11}
  @begin{short}
    The @class{gtk:alert-dialog} object collects the arguments that are needed
    to present a message to the user.
  @end{short}
  The message is shown with the @fun{gtk:alert-dialog-choose} function. This API
  follows the GIO async pattern, and the result can be obtained by calling the
  @fun{gtk:alert-dialog-choose-finish} function.

  If you do not need to wait for a button to be clicked, you can use the
  @fun{gtk:alert-dialog-show} function.
  @begin[Examples]{dictionary}
    Create an alert dialog with a @class{g:cancellable} object.
    @begin{pre}
(defun create-alert-dialog (parent)
  (let ((dialog (make-instance 'gtk:alert-dialog
                               :message \"Alert Alert Alert\"
                               :detail \"The detail of the alert dialog.\"
                               :buttons '(\"Cancel\" \"OK\")
                               :cancel-button 0
                               :default-button 1
                               :modal t))
        (cancellable (g:cancellable-new)))
    ;; Cancel the alert dialog after waiting 10 seconds for user response
    (g:timeout-add-seconds 10
                           (lambda ()
                             (g:cancellable-cancel cancellable)
                             glib:+source-remove+))
    ;; Show the alert dialog
    (gtk:alert-dialog-choose dialog
        parent
        cancellable
        ;; The GAsyncReadyCallback function
        (lambda (source result)
          ;; Get the result
          (let ((result (gtk:alert-dialog-choose-finish source result)))
            (format t \"Alert dialog result is ~a~%\" result))))))
    @end{pre}
  @end{dictionary}
  Since 4.10
  @see-class{g:async-result}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:alert-dialog-buttons -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "buttons" 'alert-dialog) t)
 "The @code{buttons} property of type @type{g:strv-t} (Read / Write) @br{}
  Labels for buttons to show in the alert dialog. The labels should be
  translated and may contain a \"_\" to indicate the mnemonic character. If
  this property is not set, then a Close button is automatically created.")

#+liber-documentation
(setf (liber:alias-for-function 'alert-dialog-buttons)
      "Accessor"
      (documentation 'alert-dialog-buttons 'function)
 "@version{2023-9-20}
  @syntax{(gtk:alert-dialog-buttons object) => buttons}
  @syntax{(setf (gtk:alert-dialog-buttons object) buttons)}
  @argument[object]{a @class{gtk:alert-dialog} object}
  @argument[buttons]{a list of strings with the button labels}
  @begin{short}
    Accessor of the @slot[gtk:alert-dialog]{buttons} slot of the
    @class{gtk:alert-dialog} class.
  @end{short}
  The @fun{gtk:alert-dialog-buttons} function returns the button labels for the
  alert dialog. The @setf{gtk:alert-dialog-buttons} function sets the buttons.

  Since 4.10
  @see-class{gtk:alert-dialog}")

;;; --- gtk:alert-dialog-cancel-button -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cancel-button" 'alert-dialog) t)
 "The @code{cancel-button} property of type @code{:int} (Read / Write) @br{}
  This property determines what happens when the @kbd{Escape} key is pressed
  while the alert dialog is shown. If this property holds the index of a button
  in the @slot[gtk:alert-dialog]{buttons} property, then pressing the
  @kbd{Escape} key is treated as if that button was pressed. If it is -1 or not
  a valid index for the buttons array, then an error is returned. If the
  @slot[gtk:alert-dialog]{buttons} property is @code{nil}, then the
  automatically created Close button is treated as both Cancel and Default
  button, so 0 is returned. @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'alert-dialog-cancel-button)
      "Accessor"
      (documentation 'alert-dialog-cancel-button 'function)
 "@version{2023-9-20}
  @syntax{(gtk:alert-dialog-cancel-button object) => button}
  @syntax{(setf (gtk:alert-dialog-cancel-button object) button)}
  @argument[object]{a @class{gtk:alert-dialog} object}
  @argument[button]{an integer with the new Cancel button}
  @begin{short}
    Accessor of the @slot[gtk:alert-dialog]{cancel-button} slot of the
    @class{gtk:alert-dialog} class.
  @end{short}
  The @fun{gtk:alert-dialog-cancel-button} function returns the index of the
  Cancel button. The @setf{gtk:alert-dialog-cancel-button} function sets the
  index of the Cancel button. See the @slot[gtk:alert-dialog]{cancel-button}
  property for details of how this value is used.

  Since 4.10
  @see-class{gtk:alert-dialog}")

;;; --- gtk:alert-dialog-default-button ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "default-button"
                                               'alert-dialog) t)
 "The @code{default-button} property of type @code{:int} (Read / Write) @br{}
  This property determines what happens when the @kbd{Return} key is pressed
  while the alert dialog is shown. If this property holds the index of a button
  in the @slot[gtk:alert-dialog]{buttons} property, then pressing the
  @kbd{Return} key is treated as if that button was pressed. If it is -1 or not
  a valid index for the buttons list, then nothing happens. If the
  @slot[gtk:alert-dialog]{buttons} property is @code{nil}, then the
  automatically created Close button is treated as both
  Cancel and Default button, so 0 is returned. @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'alert-dialog-default-button)
      "Accessor"
      (documentation 'alert-dialog-default-button 'function)
 "@version{2023-9-20}
  @syntax{(gtk:alert-dialog-default-button object) => button}
  @syntax{(setf (gtk:alert-dialog-default-button object) button)}
  @argument[object]{a @class{gtk:alert-dialog} object}
  @argument[button]{an integer with the new Default button}
  @begin{short}
    Accessor of the @slot[gtk:alert-dialog]{default-button} slot of the
    @class{gtk:alert-dialog} class.
  @end{short}
  The @fun{gtk:alert-dialog-default-button} function returns the index of the
  Default button. The @setf{gtk:alert-dialog-default-button} function sets the
  index of the Default button. See the @slot[gtk:alert-dialog]{default-button}
  property for details of how this value is used.

  Since 4.10
  @see-class{gtk:alert-dialog}")

;;; --- gtk:alert-dialog-detail ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "detail" 'alert-dialog) t)
 "The @code{detail} property of type @code{:string} (Read / Write) @br{}
  The detail text for the alert dialog. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'alert-dialog-detail)
      "Accessor"
      (documentation 'alert-dialog-detail 'function)
 "@version{2023-9-20}
  @syntax{(gtk:alert-dialog-detail object) => detail}
  @syntax{(setf (gtk:alert-dialog-detail object) detail)}
  @argument[object]{a @class{gtk:alert-dialog} object}
  @argument[detail]{a string with the detail text}
  @begin{short}
    Accessor of the @slot[gtk:alert-dialog]{detail} slot of the
    @class{gtk:alert-dialog} class.
  @end{short}
  The @fun{gtk:alert-dialog-detail} function returns the detail text that will
  be shown in the alert dialog. The @setf{gtk:alert-dialog-detail} function
  sets the detail text.

  Since 4.10
  @see-class{gtk:alert-dialog}")

;;; --- gtk:alert-dialog-message -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "message" 'alert-dialog) t)
 "The @code{message} property of type @code{:string} (Read / Write) @br{}
  The message for the alert dialog. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'alert-dialog-message)
      "Accessor"
      (documentation 'alert-dialog-message 'function)
 "@version{2023-9-20}
  @syntax{(gtk:alert-dialog-message object) => message}
  @syntax{(setf (gtk:alert-dialog-message object) message)}
  @argument[object]{a @class{gtk:alert-dialog} object}
  @argument[message]{a string with the message}
  @begin{short}
    Accessor of the @slot[gtk:alert-dialog]{message} slot of the
    @class{gtk:alert-dialog} class.
  @end{short}
  The @fun{gtk:alert-dialog-message} function returns the message that will be
  shown in the alert dialog. The @setf{gtk:alert-dialog-message} function sets
  the message.

  Since 4.10
  @see-class{gtk:alert-dialog}")

;;; --- gtk:alert-dialog-modal -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modal" 'alert-dialog) t)
 "The @code{modal} property of type @code{:boolean} (Read / Write) @br{}
  Whether the alert dialog is modal. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'alert-dialog-modal)
      "Accessor"
      (documentation 'alert-dialog-modal 'function)
 "@version{2023-9-20}
  @syntax{(gtk:alert-dialog-modal object) => modal}
  @syntax{(setf (gtk:alert-dialog-modal object) modal)}
  @argument[object]{a @class{gtk:alert-dialog} object}
  @argument[modal]{a boolean whether the alert dialog is modal}
  @begin{short}
    Accessor of the @slot[gtk:alert-dialog]{modal} slot of the
    @class{gtk:alert-dialog} class.
  @end{short}
  The @fun{gtk:alert-dialog-modal} function returns whether the alert dialog
  blocks interaction with the parent window while it is presented. The
  @setf{gtk:alert-dialog-modal} function sets whether the alert dialog blocks
  interaction with the parent window.

  Since 4.10
  @see-class{gtk:alert-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_alert_dialog_new
;;; ----------------------------------------------------------------------------

(defun alert-dialog-new (msg &rest args)
 #+liber-documentation
 "@version{2024-4-11}
  @argument[msg]{a format string for the message}
  @argument[args]{arguments for @arg{msg}}
  @return{The new @class{gtk:alert-dialog} object.}
  @begin{short}
    Creates a new alert dialog.
  @end{short}
  The message will be set to the formatted string resulting from the arguments.

  Since 4.10
  @see-class{gtk:alert-dialog}"
  (if msg
      (make-instance 'alert-dialog
                     :message (apply #'format nil msg args))
      (make-instance 'alert-dialog)))

(export 'alert-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_alert_dialog_choose
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_alert_dialog_choose" %alert-dialog-choose) :void
  (dialog (g:object alert-dialog))
  (parent (g:object window))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun alert-dialog-choose (dialog parent cancellable func)
 #+liber-documentation
 "@version{2023-9-20}
  @argument[dialog]{a @class{gtk:alert-dialog} object}
  @argument[parent]{a parent @class{gtk:window} widget}
  @argument[cancellable]{a @class{g:cancellable} object to cancel the operation}
  @argument[func]{a @symbol{g:async-ready-callback} callback function to call
    when the operation is complete}
  @begin{short}
    This function shows the alert dialog to the user.
  @end{short}
  The callback will be called when the alert dialog is dismissed. It should call
  the @fun{gtk:alert-dialog-choose-finish} function to obtain the result.

  It is ok to pass @code{nil} for the callback if the alert dialog does not have
  more than one button. A simpler API for this case is the
  @fun{gtk:alert-dialog-show} function.

  Since 4.10
  @see-class{gtk:alert-dialog}
  @see-class{gtk:window}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gtk:alert-dialog-choose-finish}
  @see-function{gtk:alert-dialog-show}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%alert-dialog-choose dialog
                          parent
                          cancellable
                          (cffi:callback g:async-ready-callback)
                          ptr)))

(export 'alert-dialog-choose)

;;; ----------------------------------------------------------------------------
;;; gtk_alert_dialog_choose_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_alert_dialog_choose_finish" %alert-dialog-choose-finish)
    :int
  (dialog (g:object alert-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun alert-dialog-choose-finish (dialog result)
 #+liber-documentation
 "@version{2023-9-20}
  @argument[dialog]{a @class{gtk:alert-dialog} object}
  @argument[result]{a @class{g:async-result} object with the result}
  @return{The integer with the index of the button that was clicked, -1 if the
    alert dialog was cancelled and the @slot[gtk:alert-dialog]{cancel-button}
    property is not set.}
  @begin{short}
    Finishes the the @fun{gtk:alert-dialog-choose} function call and returns the
    index of the button that was clicked.
  @end{short}

  Since 4.10
  @see-class{gtk:alert-dialog}
  @see-class{g:async-result}
  @see-function{gtk:alert-dialog-choose}
  @see-function{gtk:alert-dialog-cancel-button}"
  (glib:with-ignore-g-error (err)
    (%alert-dialog-choose-finish dialog result err)))

(export 'alert-dialog-choose-finish)

;;; ----------------------------------------------------------------------------
;;; gtk_alert_dialog_show
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_alert_dialog_show" alert-dialog-show) :void
 #+liber-documentation
 "@version{2023-9-20}
  @argument[dialog]{a @class{gtk:alert-dialog} object}
  @argument[parent]{a parent @class{gtk:window} widget}
  @begin{short}
    Show the alert dialog to the user.
  @end{short}
  This function is a simple version of the @fun{gtk:alert-dialog-choose}
  function intended for alert dialogs with a single button. If you want to
  cancel the alert dialog or if the alert dialog has more than one button, you
  should use that function instead and provide it with a @class{g:cancellable}
  object or callback respectively.

  Since 4.10
  @see-class{gtk:alert-dialog}
  @see-class{gtk:window}
  @see-class{g:cancellable}
  @see-function{gtk:alert-dialog-choose}"
  (dialog (g:object alert-dialog))
  (parent (g:object window)))

(export 'alert-dialog-show)

;;; --- End of file gtk4.alert-dialog.lisp -------------------------------------
