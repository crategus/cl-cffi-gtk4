;;;; Example Alert Dialog - 2023-9-20
;;;;
;;;; The GTK:ALERT-DIALOG object collects the arguments that are needed to
;;;; present a message to the user. The message is shown with the
;;;; GTK:ALERT-DIALOG-CHOOSE function. This API follows the GIO async pattern,
;;;; and the result can be obtained by calling the
;;;; GTK:ALERT-DIALOG-CHOOSE-FINISH function.
;;;;
;;;; If you do not need to wait for a button to be clicked, you can use the
;;;; GTK:ALERT-DIALOG-SHOW function.

(in-package :gtk4-example)

(defun create-alert-dialog (parent)
  (let ((dialog (make-instance 'gtk:alert-dialog
                               :message "Alert Alert Alert"
                               :detail "The detail of the alert dialog."
                               :buttons '("Cancel" "OK")
                               :cancel-button 0
                               :default-button 1
                               :modal t))
        (cancellable (g:cancellable-new)))
    ;; Cancel the alert dialog after waiting 10 seconds for user response
    (g:timeout-add-seconds 10
                           (lambda ()
                             (g:cancellable-cancel cancellable)
                             glib:+g-source-remove+))
    ;; Show the alert dialog
    (gtk:alert-dialog-choose dialog
        parent
        cancellable
        ;; The GAsyncReadyCallback function
        (lambda (source result)
          ;; Get the result
          (let ((result (gtk:alert-dialog-choose-finish source result)))
            (format t "Alert dialog result is ~a~%" result))))))
