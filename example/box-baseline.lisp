;;;; Baseline Box
;;;;
;;;; 2025-07-26

(in-package :gtk4-example)

(defun do-box-baseline (&optional application)
  (let* ((css-button
          ".baselinebox button         { padding: 0px; }
           .baselinebox button > label { color: black;
                                         background-color: yellow; }
          .baselinebox button:first-child > label {
                                         background-color: red;}
          .baselinebox button:last-child > label {
                                         background-color : green; }")
         ;; Create a horizontal box
         (box (make-instance 'gtk:box
                             :orientation :horizontal
                             :homogeneous nil
                             :spacing 12
                             :margin-bottom 12
                             :margin-end 12
                             :margin-start 12
                             :margin-top 12))
         ;; Create a toplevel window
         (window (make-instance 'gtk:window
                                :child box
                                :application application
                                :title "Baseline Box"))
         (provider (make-instance 'gtk:css-provider)))
    ;; Add Start button
    (gtk:box-append box
                    (make-instance 'gtk:button
                                   :label "START"
                                   :halign :start
                                   :valign :start
                                   :width-request 120
                                   :height-request 120))
    ;; Add Baseline Center button
    (gtk:box-append box
                    (make-instance 'gtk:button
                                   :label "CENTER"
                                   :halign :start
                                   :valign :baseline-center
                                   :width-request 80
                                   :height-request 80))
    ;; Add Baseline Fill button
    (gtk:box-append box
                    (make-instance 'gtk:button
                                   :label "FILL"
                                   :halign :start
                                   :valign :baseline-fill
                                   :width-request 80
                                   :height-request 80))
    ;; Add End button
    (gtk:box-append box
                    (make-instance 'gtk:button
                                   :label "END"
                                   :halign :start
                                   :valign :end
                                   :width-request 60
                                   :height-request 60))
    ;; Load CSS from data into the provider and apply CSS
    (gtk:css-provider-load-from-string provider css-button)
    (gtk:widget-add-css-class box "baselinebox")
    (gtk:widget-add-provider box provider)
    ;; Present the window
    (gtk:window-present window)))
