;;;; Alert Dialog
;;;;
;;;; The <tt>gtk:alert-dialog</tt> object collects the arguments that are needed
;;;; to present a message to the user. The message is shown with the
;;;; <tt>gtk:alert-dialog-choose</tt> function. This API follows the GIO async
;;;; pattern, and the result can be obtained by calling the
;;;; <tt>gtk:alert-dialog-choose-finish</tt> function.
;;;;
;;;; If you do not need to wait for a button to be clicked, you can use the
;;;; <tt>gtk:alert-dialog-show</tt> function.
;;;;
;;;; 2026-01-25

(in-package :gtk4-example)

(defun create-alert-dialog (parent)
  (let ((dialog (make-instance 'gtk:alert-dialog
                               :message "Alert"
                               :detail "The detail of the alert dialog."
                               :buttons '("Cancel" "OK")
                               :cancel-button 0
                               :default-button 1
                               :modal t))
        (cancellable (g:cancellable-new)))
    ;; Cancel alert dialog after waiting 10 seconds for user response
    (g:timeout-add-seconds 5
                           (lambda ()
                             (g:cancellable-cancel cancellable)
                             g:+source-remove+))
    ;; Show alert dialog
    (gtk:alert-dialog-choose dialog
        parent
        cancellable
        ;; Set GAsyncReadyCallback function
        (lambda (source result)
          ;; Get the result
          (let ((result (gtk:alert-dialog-choose-finish source result)))
            (format t "Alert dialog result is ~a~%" result))))))
