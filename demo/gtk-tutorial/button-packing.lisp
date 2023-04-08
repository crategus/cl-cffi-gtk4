;;;; Example Button packing - 2023-4-6

(in-package :gtk4-tutorial)

(defun button-packing ()
  (let ((app (make-instance 'gtk:application
                            :application-id "com.crategus.button-packing"
                            :flags :none)))

    (g:signal-connect app "activate"
            (lambda (application)
              (let* ((grid (make-instance 'gtk:grid
                                          :margin-top 12
                                          :margin-bottom 12
                                          :margin-start 12
                                          :margin-end 12
                                          :column-homogeneous t
                                          :column-spacing 6
                                          :row-homogeneous t
                                          :row-spacing 6))
                     (window (make-instance 'gtk:window
                                            :title "Button Packing"
                                            :application application
                                            :child grid
                                            :default-width 320))
                     (button1 (make-instance 'gtk:button
                                             :label "Button 1"))
                     (button2 (make-instance 'gtk:button
                                             :label "Button 2"))
                     (button3 (make-instance 'gtk:button
                                             :label "Quit")))

                (g:signal-connect button1 "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Button 1 clicked.~%")))
                (g:signal-connect button2 "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Button 2 clicked.~%")))
                (g:signal-connect button3 "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:window-destroy window)))
                ;; Pack and show the widgets
                (gtk:grid-attach grid button1 0 0 1 1)
                (gtk:grid-attach grid button2 1 0 1 1)
                (gtk:grid-attach grid button3 0 1 2 1)
                (gtk:widget-show window))))

    (g:application-run app nil)))

