;;;; Simple Message Dialog - 2022-11-11

(in-package :gtk4-example)

(defun do-message-dialog-simple (&optional application)
  (let ((dialog (make-instance 'gtk:message-dialog
                               :application application
                               :modal t
                               :message-type :info
                               :transient-for (make-instance 'gtk:window)
                               :buttons :ok
                               :text "Message Dialog"
                               :secondary-text "With secondary text")))
    ;; Handler for the "response" signal of the dialog
    (g:signal-connect dialog "response"
                      (lambda (dialog response)
                        (format t "Response is ~a" response)
                        (gtk:window-destroy dialog)))
    (gtk:widget-show dialog)))

;; A variant which uses the GTK:MESSAGE-DIALOG-NEW function
(defun do-message-dialog-simple2 (&optional application)
  (let ((dialog (gtk:message-dialog-new (make-instance 'gtk:window)
                                        '(:modal)
                                        :info
                                        :ok
                                        "Message Dialog"
                                        nil)))
    ;; Set more properties
    (setf (gtk:window-application dialog) application)
    (setf (gtk:message-dialog-secondary-text dialog) "With secondary text")
    ;; Handler for the "response" signal of the dialog
    (g:signal-connect dialog "response"
                      (lambda (dialog response)
                        (format t "Response is ~a" response)
                        (gtk:window-destroy dialog)))
    (gtk:widget-show dialog)))
