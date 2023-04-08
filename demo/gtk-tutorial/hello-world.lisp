;;;; Example Hello World - 2023-4-5

(in-package :gtk4-tutorial)

(defun hello-world ()
  (let ((app (make-instance 'gtk:application
                            :application-id "com.crategus.hello-world"
                            :flags :none)))
    (g:signal-connect app "activate"
            (lambda (application)
              (let* ((button (make-instance 'gtk:button
                                            :label "Hello World"))
                     (box (make-instance 'gtk:box
                                         :orientation :vertical
                                         :halign :center
                                         :valign :center))
                     (window (make-instance 'gtk:application-window
                                            :application application
                                            :child box
                                            :title "Window"
                                            :resizable nil
                                            :default-width 200
                                            :default-height 200)))
                (g:signal-connect window "close-request"
                        (lambda (window)
                          (declare (ignore window))
                          (format t "Ignore close request. Click the button.~%")
                          gdk:+gdk-event-stop+))
                (g:signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (format t "Hello World~%")
                          (gtk:window-destroy window)))
                (gtk:box-append box button)
                (gtk:widget-show window))))
    (g:application-run app nil)))
