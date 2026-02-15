;;;; Message Dialog
;;;;
;;;; This file contains two variants for creating a message dialog. The first
;;;; variant uses the <tt>make-instance</tt> function. The second variant
;;;; constructs the message dialog with the <tt>gtk:message-dialog-new</tt>
;;;; function.
;;;;
;;;; The installed signal handler for the <tt>"response"</tt> signal writes the
;;;; obtained reponse value as an integer on the console.
;;;;
;;;; <b>Warning</b>
;;;;
;;;; The <tt>GtkMessageDialog</tt> widget is deprectated since version 4.10. See
;;;; the <tt>GtkAlertDialog</tt> widget for a replacement of the message dialog.
;;;;
;;;; In this example, we bind the <tt>gtk-init:*warn-deprecated*</tt> global
;;;; variable to <tt>NIL</tt> to avoid warnings.
;;;;
;;;; 2026-02-15

(in-package :gtk4-example)

;; Variant 1

(defun create-message-dialog (parent)
  (let ((gtk-init:*warn-deprecated* nil))
    (let ((dialog (make-instance 'gtk:message-dialog
                                 :transient-for parent
                                 :modal t
                                 :message-type :info
                                 :buttons :ok
                                 :text "Message Dialog"
                                 :secondary-text "The secondary text.")))
      ;; Handler for the "response" signal of the dialog
      (g:signal-connect dialog "response"
                        (lambda (dialog response)
                          (format t "Response is ~a~%" response)
                          (gtk:window-destroy dialog)))
      (gtk:window-present dialog))))

;; Variant 2

(defun create-message-dialog-2 (parent)
  (let ((gtk-init:*warn-deprecated* nil))
    (let ((dialog (gtk:message-dialog-new parent
                                          '(:modal)
                                          :info
                                          :ok-cancel
                                          "Message Dialog"
                                          parent)))
      ;; Set secondary text with the accessor
      (setf (gtk:message-dialog-secondary-text dialog)
            "Created with constructor and with two buttons.")
      ;; Handler for the "response" signal of the dialog
      (g:signal-connect dialog "response"
                        (lambda (dialog response)
                          (format t "Response is ~a~%" response)
                          (gtk:window-destroy dialog)))
      (gtk:window-present dialog))))
