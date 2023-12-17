;;;; CSS Accordion
;;;;
;;;; An accordion demo written using CSS transitions and multiple backgrounds.
;;;;
;;;; 2023-12-16

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
         (path (sys-path "resource/css-accordion.css")))
    ;; Load and install the CSS for the accordion
    (gtk:css-provider-load-from-path provider path)
    (gtk:style-context-add-provider-for-display (gdk:display-default)
                                                provider
                                                gtk:+gtk-priority-user+)
    (gtk:widget-add-css-class frame "accordion")
    ;; Remove the provider when destroying the window
    (g:signal-connect window "destroy"
        (lambda (widget)
          (declare (ignore widget))
          (gtk:style-context-remove-provider-for-display (gdk:display-default)
                                                         provider)))
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
