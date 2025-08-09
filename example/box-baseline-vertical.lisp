;;;; Baseline Vertical Box
;;;;
;;;; 2025-07-26

;; TODO: Example is not finished. The vertical expansion is equal for all
;; 4 boxes. It should be different?!

(in-package :gtk4-example)

(defun make-vertical-box (baseline)
  (let ((vbox (make-instance 'gtk:box
                             :baseline-child baseline
                             :orientation :vertical
                             :vexpand t
                             :homogeneous nil)))
    (gtk:box-append vbox
                    (make-instance 'gtk:button
                                   :label "START"
                                   :halign :start
                                   :valign :start
                                   :vexpand t
                                   :width-request 120
                                   :height-request 120))
    (gtk:box-append vbox
                    (make-instance 'gtk:button
                                   :label "CENTER"
                                   :halign :start
                                   :valign :baseline-center
                                   :vexpand t
                                   :width-request 80
                                   :height-request 80))
    (gtk:box-append vbox
                    (make-instance 'gtk:button
                                   :label "FILL"
                                   :halign :start
                                   :valign :baseline-fill
                                   :vexpand t
                                   :width-request 80
                                   :height-request 80))
    (gtk:box-append vbox
                    (make-instance 'gtk:button
                                   :label "END"
                                   :halign :start
                                   :valign :end
                                   :vexpand t
                                   :width-request 60
                                   :height-request 60))
    vbox))

(defun do-box-baseline-vertical (&optional application)
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
                             :valign :baseline-fill
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
    ;; Add Start box
    (gtk:box-append box (make-vertical-box 0))
    ;; Add Baseline Center box
    (gtk:box-append box (make-vertical-box 1))
    ;; Add Baseline Fill box
    (gtk:box-append box (make-vertical-box 2))
    ;; Add End box
    (gtk:box-append box (make-vertical-box 3))
    ;; Load CSS from data into the provider and apply CSS
    (gtk:css-provider-load-from-string provider css-button)
    (gtk:widget-add-css-class box "baselinebox")
    (gtk:widget-add-provider box provider)
    ;; Present the window
    (gtk:window-present window)))
