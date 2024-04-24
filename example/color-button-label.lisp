;;;; Color Button Label
;;;;
;;;; The example shows a color button. The button is initialized with the color
;;;; "Black". The handler for the "color-set" signal changes the color of the
;;;; "Color button" label. To change the color of the label the CSS color is
;;;; loaded in a CSS provider and the style context of the label is updated.
;;;;
;;;; 2024-4-19

(in-package :gtk4-example)

(defun do-color-button-label (&optional application)
  (let* ((box (make-instance 'gtk:box
                             :orientation :horizontal
                             :spacing 24
                             :margin-top 12
                             :margin-bottom 12
                             :margin-start 12
                             :margin-end 12))
         (window (make-instance 'gtk:window
                                :application application
                                :child box
                                :title "Color Button Label"
                                :default-width 310
                                :default-height 180))
         (provider (gtk:css-provider-new))
         (label (make-instance 'gtk:label
                               :label "<big><b>Color button</b></big>"
                               :use-markup t))
         (button (make-instance 'gtk:color-button
                                :rgba (gdk:rgba-parse "Black")
                                :width-request 90
                                :height-request 24)))
    ;; Handler for the "color-set" signal
    (g:signal-connect button "color-set"
       (lambda (widget)
         (let* ((rgba (gtk:color-chooser-rgba widget))
                (css-label (format nil "label { color : ~a; }"
                                       (gdk:rgba-to-string rgba))))
           ;; Update the color of the label
           (gtk:css-provider-load-from-string provider css-label)
           (gtk:widget-add-provider widget provider))))
    ;; Pack and show the widgets
    (gtk:box-append box button)
    (gtk:box-append box label)
    (gtk:window-present window)))
