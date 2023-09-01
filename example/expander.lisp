;;;; Example Expander - 2023-8-23
;;;;
;;;; GtkExpander allows to provide additional content that is initially hidden.
;;;; This is also known as "disclosure triangle". This example also shows how
;;;; to make the window resizable only if the expander is expanded.
 
(in-package :gtk4-example)

(defun create-expander (&optional parent)
  (let* ((dialog (make-instance 'gtk:message-dialog
                                :transient-for parent
                                :message-type :info
                                :buttons :close
                                :default-width 320
                                :text
                                "Message with Expander"
                                :secondary-text
                                "See Details for more Information."
                                ;; FIXME: The dialog is not destroyed with
                                ;; the parent window. We make it modal.
                                :destroy-with-parent t
                                :modal t))
        (area (gtk:message-dialog-message-area dialog))
        (label (gtk:widget-last-child area))
        (view (make-instance 'gtk:text-view
                             :left-margin 12
                             :right-margin 12
                             :top-margin 12
                             :bottom-margin 12
                             :editable nil
                             :cursor-visible nil
                             :pixel-above-lines 2
                             :pixel-below-lines 2
                             :wrap-mode :word))
        (scrolled (make-instance 'gtk:scrolled-window
                                 :child view
                                 :margin-top 12
                                 :min-content-height 100
                                 :has-frame t
                                 :hscroll-policy :never
                                 :vscroll-policy :automatic
                                 :propagate-natural-height t
                                 :vexpand t))
        (expander (make-instance 'gtk:expander
                                 :child scrolled
                                 :label "Details"
                                 :vexpand t))
        (buffer (gtk:text-view-buffer view)))

    ;; Set properties on the label
    (setf (gtk:label-wrap label) nil)
    (setf (gtk:widget-vexpand label) nil)

    ;; Put some text in the text buffer
    (setf (gtk:text-buffer-text buffer) *some-text*)

    ;; Append the expander to the message area
    (gtk:box-append area expander)

    ;; Set signal handler for the expander
    (g:signal-connect expander "notify::expanded"
                      (lambda (expander param)
                        (declare (ignore param))
                        (setf (gtk:window-resizable dialog)
                              (gtk:expander-expanded expander))))

    ;; Set response signal handler for the message dialog
    (g:signal-connect dialog "response"
                      (lambda (dialog response)
                        (declare (ignore response))
                        (gtk:window-destroy dialog)))

    (setf (gtk:widget-visible dialog) t)))

