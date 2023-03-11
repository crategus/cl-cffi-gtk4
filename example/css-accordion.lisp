;;;; CSS Accordion - 2022-11-18
;;;;
;;;; An accordion demo written using CSS transitions and multiple backgrounds.

(in-package :gtk4-example)

(defun do-css-accordion (&optional application)
  (let* ((box (make-instance 'gtk:box
                             :orientation :horizontal
                             :halign :center
                             :valign :center))
         (window (make-instance 'gtk:window
                                :application application
                                :title "CSS Accordion"
                                :child box
                                :default-height 300
                                :default-width 600))
         (provider (make-instance 'gtk:css-provider))
         (csspath (sys-path "resource/css-accordion.css")))
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
    ;; Load CSS from file into the provider
    (gtk:css-provider-load-from-path provider csspath)
    ;; Apply CSS to the widgets
    (apply-css-to-widget provider window)
    ;; Show the window
    (gtk:widget-show window)))
