;;;; Scale Button
;;;;
;;;; 2024-4-19

(in-package :gtk4-example)

(defun do-scale-button (&optional application)
  (let* ((provider (gtk:css-provider-new))
         (button (make-instance 'gtk:scale-button
                                :margin-start 60
                                :margin-end 60
                                :margin-top 60
                                :margin-bottom 60
                                :value 6.0
                                :icons
                                '("face-crying"     ; lowest value
                                  "face-smile-big"  ; highest value
                                  "face-sad"        ; other values between
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
                               :title "Scale Button"
                               :child button
                               :application application
                               :resizable nil
                               :width-request 300
                               :height-request 240))
        (css-button (format nil "scalebutton > button
                                   {background : orange;
                                    -gtk-icon-size : 48px;}")))
    ;; Load and apply provider to style the button
    (gtk:css-provider-load-from-string provider css-button)
    (gtk:widget-add-provider button provider)
    ;; Show the window
    (gtk:window-present window)))
