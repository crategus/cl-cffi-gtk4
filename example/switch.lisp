;;;; Example Switch - 2022-11-18

(in-package :gtk4-example)

(defun do-switch (&optional application)
  (let* ((hbox (make-instance 'gtk:box
                              :orientation :horizontal
                              :halign :center
                              :valign :center
                              :margin-top 12
                              :margin-bottom 12
                              :margin-left 12
                              :margin-right 12
                              :spacing 24))
        (window (make-instance 'gtk:window
                               :title "Example Switch"
                               :child hbox
                               :application application))
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
    (gtk:box-append hbox switch)
    (gtk:box-append hbox label)
    (gtk:widget-show window)))
