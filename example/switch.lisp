;;;; Example Switch - 2023-3-26

(in-package :gtk4-example)

(defun do-switch (&optional application)
  (let* ((box (make-instance 'gtk:box
                             :orientation :horizontal
                             :margin-top 48
                             :margin-bottom 48
                             :margin-start 36
                             :margin-end 12
                             :spacing 18))
        (window (make-instance 'gtk:window
                               :title "Switch"
                               :child box
                               :application application
                               :resizable nil))
        (switch (make-instance 'gtk:switch
                               :active t))
        (label (make-instance 'gtk:label
                              :xalign 0.0
                              :label "Switch is On")))
    (g:signal-connect switch "notify::active"
        (lambda (widget param)
          (declare (ignore param))
          (if (gtk:switch-active widget)
              (setf (gtk:label-label label) "Switch is On")
              (setf (gtk:label-label label) "Switch is Off"))))
    (gtk:box-append box switch)
    (gtk:box-append box label)
    (gtk:widget-show window)))
