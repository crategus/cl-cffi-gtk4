;;;; Simple Text View - 2022-9-2

(in-package :gtk4-example)

(defun do-text-view-simple (&optional application)
  (let* ((textview (make-instance 'gtk:text-view
                                  :wrap-mode :word
                                  :top-margin 6
                                  :top-bottom 6
                                  :left-margin 6
                                  :right-margin 6))
         (window (make-instance 'gtk:window
                                :application application
                                :child textview
                                :title "Simple Text View"
                                :default-width 350
                                :default-height 200))
         (buffer (gtk:text-view-buffer textview)))
    (g:signal-connect window "close-request"
        (lambda (widget)
          (declare (ignore widget))
          (format t "in DESTROY signal~%")
          (let ((start (gtk:text-buffer-start-iter buffer))
                (end (gtk:text-buffer-end-iter buffer))
                (include-hidden-chars t))
            (print (gtk:text-buffer-get-text buffer
                                             start
                                             end
                                             include-hidden-chars))
            (terpri))))
    (setf (gtk:text-buffer-text buffer) *lorem-ipsum-short*)
    (gtk:widget-show window)))
