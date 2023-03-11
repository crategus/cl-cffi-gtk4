;;;; Various Message Dialogs - 2022-11-11

(in-package :gtk4-example)

;; Example for GtkMessageDialog documentation
(defun create-message-dialog-new (parent filename)
  (let ((dialog (gtk:message-dialog-new parent
                                        '(:modal :destroy-with-parent)
                                        :error
                                        :close
                                        "Error loading file ~s"
                                        filename)))
    (g:signal-connect dialog "response"
                      (lambda (dialog response)
                        (declare (ignore response))
                        (gtk:window-destroy dialog)))
    (gtk:widget-show dialog)))

(defun do-message-dialog-new (&optional application)
  (let ((parent (make-instance 'gtk:window
                               :title "Show Message Dialog"
                               :application application)))
    (create-message-dialog-new parent "message.lisp")
    (gtk:widget-show parent)))

;; Example for GtkMessageDialog documentation
(defun create-message-dialog-new-with-markup (parent filename)
  (let ((dialog (gtk:message-dialog-new-with-markup
                                        parent
                                        '(:modal :destroy-with-parent)
                                        :error
                                        :close
                                        "<b>Error loading file ~s</b>"
                                        filename)))
    (g:signal-connect dialog "response"
                      (lambda (dialog response)
                        (declare (ignore response))
                        (gtk:window-destroy dialog)))
    (gtk:widget-show dialog)))

(defun do-message-dialog-new-with-markup (&optional application)
  (let ((parent (make-instance 'gtk:window
                               :title "Show Message Dialog"
                               :application application)))
    (create-message-dialog-new-with-markup parent "message.lisp")
    (gtk:widget-show parent)))

;; Example for GtkMessageDialog documentation
;; TODO: Check the usage of FILENAME in this example
(defun create-message-dialog-with-markup (parent filename)
  (declare (ignore filename))
  (let ((dialog (gtk:message-dialog-new-with-markup
                                        parent
                                        '(:modal :destroy-with-parent)
                                        :error
                                        :close
                                        nil
                                        nil)))
    (g:signal-connect dialog "response"
                      (lambda (dialog response)
                        (declare (ignore response))
                        (gtk:window-destroy dialog)))
    (gtk:message-dialog-set-markup dialog "<b>Error</b>")
    (gtk:message-dialog-format-secondary-text dialog
                                              "Error loading file ~s"
                                              "message.lisp")
    (gtk:widget-show dialog)))

(defun do-message-dialog-with-markup (&optional application)
  (let ((parent (make-instance 'gtk:window
                               :title "Show Message Dialog"
                               :application application)))
    (create-message-dialog-with-markup parent "message.lisp")
    (gtk:widget-show parent)))

;; Example for GtkMessageDialog documentation
;; TODO: Check the usage of FILENAME in this example
(defun create-message-dialog-with-secondary-markup (parent filename)
  (declare (ignore filename))
  (let ((dialog (gtk:message-dialog-new-with-markup
                                        parent
                                        '(:modal :destroy-with-parent)
                                        :error
                                        :close
                                        nil
                                        nil)))
    (g:signal-connect dialog "response"
                      (lambda (dialog response)
                        (declare (ignore response))
                        (gtk:window-destroy dialog)))
    (gtk:message-dialog-set-markup dialog "<b>Error</b>")
    (gtk:message-dialog-format-secondary-markup dialog
                                                "Error loading file <b>~s</b>"
                                                "message.lisp")
    (gtk:widget-show dialog)))

(defun do-message-dialog-with-secondary-markup (&optional application)
  (let ((parent (make-instance 'gtk:window
                               :title "Show Message Dialog"
                               :application application)))
    (create-message-dialog-with-secondary-markup parent "message.lisp")
    (gtk:widget-show parent)))
