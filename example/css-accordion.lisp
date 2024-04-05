;;;; CSS Accordion
;;;;
;;;; An accordion demo written using CSS transitions and multiple backgrounds.
;;;;
;;;; 2024-4-3

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
         (display (gtk:widget-display window))
         (provider (make-instance 'gtk:css-provider))
         (path (sys-path "resource/css-accordion.css")))
    ;; Load and install the CSS for the accordion
    (gtk:widget-add-css-class frame "accordion")
    (gtk:css-provider-load-from-path provider path)
    (gtk:style-context-add-provider-for-display display provider)
    ;; Remove the provider when destroying the window
    (g:signal-connect window "destroy"
        (lambda (widget)
          (let ((display (gtk:widget-display widget)))
            (gtk:style-context-remove-provider-for-display display provider))))
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
