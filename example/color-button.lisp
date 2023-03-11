;;;; Example Color Button - 2022-8-27
;;;;
;;;; The example shows a color button. The button is initialized with the color
;;;; "Blue". The handler for the "color-set" signal prints the selected color
;;;; on the console.

(in-package :gtk4-example)

(defun do-color-button (&optional application)
  (let* ((button (make-instance 'gtk:color-button
                                 :rgba (gdk:rgba-parse "Blue")
                                 :title "Choose a color from the palette"
                                 :margin-top 48
                                 :margin-bottom 48
                                 :margin-start 48
                                 :margin-end 48))
         (window (make-instance 'gtk:window
                                 :title "Color Button"
                                 :application application
                                 :child button
                                 :default-width 270
                                 :default-height 210)))
    (g:signal-connect button "color-set"
        (lambda (widget)
          (let ((rgba (gtk:color-chooser-rgba widget)))
            (format t "Selected color is ~a~%" (gdk:rgba-to-string rgba)))))
    (gtk:widget-show window)))
