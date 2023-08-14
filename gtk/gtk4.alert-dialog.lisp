;;; ----------------------------------------------------------------------------
;;; gtk4.alert-dialog.lisp
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

(gobject:define-g-object-class "GtkAlertDialog" alert-dialog
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

;;;Description


;;;A GtkAlertDialog object collects the arguments that are needed to present a message to the user.

;;;The message is shown with the gtk_alert_dialog_choose() function. This API follows the GIO async pattern, and the result can be obtained by calling gtk_alert_dialog_choose_finish().

;;;If you don’t need to wait for a button to be clicked, you can use gtk_alert_dialog_show().

;;;Available since: 4.10


;;; ----------------------------------------------------------------------------
;;; gtk_alert_dialog_new
;;;
;;; Creates a new GtkAlertDialog object.
;;;
;;; since: 4.10
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;;gtk_alert_dialog_choose
;;;This function shows the alert to the user.

;;;since: 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gtk_alert_dialog_choose_finish
;;;Finishes the gtk_alert_dialog_choose() call and returns the index of the button that was clicked.

;;;since: 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gtk_alert_dialog_get_buttons
;;;Returns the button labels for the alert.

;;;since: 4.10

;;;gtk_alert_dialog_get_cancel_button
;;;Returns the index of the cancel button.

;;;since: 4.10

;;;gtk_alert_dialog_get_default_button
;;;Returns the index of the default button.

;;;since: 4.10

;;;gtk_alert_dialog_get_detail
;;;Returns the detail text that will be shown in the alert.

;;;since: 4.10

;;;gtk_alert_dialog_get_message
;;;Returns the message that will be shown in the alert.

;;;since: 4.10

;;;gtk_alert_dialog_get_modal
;;;Returns whether the alert blocks interaction with the parent window while it is presented.

;;;since: 4.10

;;;gtk_alert_dialog_set_buttons
;;;Sets the button labels for the alert.

;;;since: 4.10

;;;gtk_alert_dialog_set_cancel_button
;;;Sets the index of the cancel button.

;;;since: 4.10

;;;gtk_alert_dialog_set_default_button
;;;Sets the index of the default button.

;;;since: 4.10

;;;gtk_alert_dialog_set_detail
;;;Sets the detail text that will be shown in the alert.

;;;since: 4.10

;;;gtk_alert_dialog_set_message
;;;Sets the message that will be shown in the alert.

;;;since: 4.10

;;;gtk_alert_dialog_set_modal
;;;Sets whether the alert blocks interaction with the parent window while it is presented.

;;;since: 4.10

;;; ----------------------------------------------------------------------------
;;; gtk_alert_dialog_show
;;;
;;; void
;;; gtk_alert_dialog_show (
;;;   GtkAlertDialog* self,
;;;   GtkWindow* parent
;;; )
;;;
;;;
;;; Show the alert to the user.
;;;
;;; This function is a simple version of gtk_alert_dialog_choose() intended for 
;;; dialogs with a single button. If you want to cancel the dialog or if the 
;;; alert has more than one button, you should use that function instead and 
;;; provide it with a GCancellable or callback respectively.
;;;
;;; since: 4.10
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_alert_dialog_show" alert-dialog-show) :void
  (dialog (g:object alert-dialog))
  (parent (g:object window)))

(export 'alert-dialog-show)

;;;Properties 
;;;Gtk.AlertDialog:buttons
;;;Labels for buttons to show in the alert.

;;;since: 4.10

;;;Gtk.AlertDialog:cancel-button
;;;This property determines what happens when the Escape key is pressed while the alert is shown.

;;;since: 4.10

;;;Gtk.AlertDialog:default-button
;;;This property determines what happens when the Return key is pressed while the alert is shown.

;;;since: 4.10

;;;Gtk.AlertDialog:detail
;;;The detail text for the alert.

;;;since: 4.10

;;;Gtk.AlertDialog:message
;;;The message for the alert.

;;;since: 4.10

;;;Gtk.AlertDialog:modal
;;;Whether the alert is modal.

;;;since: 4.10


;;; --- End of file gtk4.alert-dialog.lisp -------------------------------------
