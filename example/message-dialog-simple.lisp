;;;; Simple Message Dialog - 2023-5-7

(in-package :gtk4-example)

(defun create-message-dialog-simple (parent)
  (let ((dialog (make-instance 'gtk:message-dialog
                               :transient-for parent
                               :modal t
                               :message-type :info
                               :buttons :ok
                               :text "Message Dialog"
                               :secondary-text "With secondary text")))
    ;; Handler for the "response" signal of the dialog
    (g:signal-connect dialog "response"
                      (lambda (dialog response)
                        (format t "Response is ~a~%" response)
                        (gtk:window-destroy dialog)))
    (gtk:widget-show dialog)))

;; A variant which uses the GTK:MESSAGE-DIALOG-NEW function
(defun create-message-dialog-simple2 (parent)
  (let ((dialog (gtk:message-dialog-new parent
                                        '(:modal)
                                        :info
                                        :ok-cancel
                                        "Message Dialog for ~a"
                                        parent)))
    ;; Set secondary text
    (setf (gtk:message-dialog-secondary-text dialog) "With secondary text")
    ;; Handler for the "response" signal of the dialog
    (g:signal-connect dialog "response"
                      (lambda (dialog response)
                        (format t "Response is ~a~%" response)
                        (gtk:window-destroy dialog)))
    (gtk:widget-show dialog)))
