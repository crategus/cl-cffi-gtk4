;;;; Example Dialog for Quick Message - 2022-11-11

(in-package :gtk4-example)

(defun create-quick-message (parent msg)
  (let ((dialog (gtk:dialog-new-with-buttons "Message"
                                             parent
                                             '(:destroy-with-parent :modal)
                                             "OK"
                                             :none)))
    (g:signal-connect dialog "response"
                      (lambda (widget response)
                        (declare (ignore response))
                        (gtk:window-destroy widget)))
    (gtk:box-append (gtk:dialog-content-area dialog)
                    (make-instance 'gtk:label
                                   :label msg
                                   :margin-top 12
                                   :margin-bottom 12
                                   :margin-start 12
                                   :margin-end 12))
    (gtk:widget-show dialog)))

(defun do-dialog-quick-message (&optional application)
  (let ((parent (make-instance 'gtk:window
                               :title "Show Quick Message"
                               :application application)))
    (create-quick-message parent
                          "Quick Message with GtkDialog")
    (gtk:widget-show parent)))
