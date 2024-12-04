;;;; Color Button
;;;;
;;;; This example shows a color button. The button is initialized with the color
;;;; "Blue". The handler for the <tt>"color-set"</tt> signal prints the selected
;;;; color on the console.
;;;;
;;;; 2024-4-6

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
    (gtk:window-present window)))
