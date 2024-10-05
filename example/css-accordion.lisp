;;;; CSS Accordion
;;;;
;;;; An accordion demo written using CSS transitions and multiple backgrounds.
;;;;
;;;; Last version: 2024-5-6

(in-package :gtk4-example)

(defun do-css-accordion (&optional application)
  (let* ((box (make-instance 'gtk:box
                             :orientation :horizontal
                             :halign :center
                             :valign :center))
         (frame (make-instance 'gtk:frame
                               :child box))
         (window (make-instance 'gtk:window
                                :application application
                                :title "CSS Accordion"
                                :child frame
                                :default-height 300
                                :default-width 600))
         (provider (make-instance 'gtk:css-provider))
         (path (glib-sys:sys-path "resource/css-accordion.css")))
    ;; Load and install the CSS for the accordion
    (gtk:css-provider-load-from-path provider path)
    (gtk:widget-add-css-class frame "accordion")
    (gtk:widget-add-provider frame provider)
    ;; Add the buttons to the box
    (gtk:box-append box
                    (gtk:button-new-with-label "This"))
    (gtk:box-append box
                    (gtk:button-new-with-label "Is"))
    (gtk:box-append box
                    (gtk:button-new-with-label "A"))
    (gtk:box-append box
                    (gtk:button-new-with-label "CSS"))
    (gtk:box-append box
                    (gtk:button-new-with-label "Accordion"))
    (gtk:box-append box
                    (gtk:button-new-with-label "."))
    ;; Present the window
    (gtk:window-present window)))
