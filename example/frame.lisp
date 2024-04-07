;;;; Frame Widget
;;;;
;;;; 2024-4-6

(in-package :gtk4-example)

(defun do-frame (&optional application)
  (let* ((frame (make-instance 'gtk:frame
                               :label "Frame Label"
                               :label-xalign 0.9
                               :margin-top 12
                               :margin-bottom 12
                               :margin-start 12
                               :margin-end 12))
         (window (make-instance 'gtk:window
                                :application application
                                :child frame
                                :title "Frame Widget"
                                :default-width 250
                                :default-height 200
                                :resizable nil)))
    (gtk:window-present window)))
