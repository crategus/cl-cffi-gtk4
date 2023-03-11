;;;; Example Scale Button - 2022-11-18

(in-package :gtk4-example)

;; TODO: The size property is no longer available. Replace this functionality.

(defun do-scale-button (&optional application)
  (let* ((button (make-instance 'gtk:scale-button
                                :margin-left 60
                                :value 9.0
                                :icons
                                '("face-crying"     ; lowest value
                                  "face-smile-big"  ; highest value
                                  "face-sad"        ; other value between
                                  "face-worried"
                                  "face-uncertain"
                                  "face-plain"
                                  "face-smile")
                                :adjustment
                                (make-instance 'gtk:adjustment
                                               :lower 0.0
                                               :upper 10.0
                                               :step-increment 1.0
                                               :page-increment 2.0)))
        (window (make-instance 'gtk:window
                               :title "Example Scale Button"
                               :child button
                               :application application
                               :width-request 360
                               :height-request 240)))

;    (format t "Child of button is ~a~%" (gtk:button-child button))

    ;; Pack and show the widgets
    (gtk:widget-show window)))
