;;;; Color Chooser Widget
;;;;
;;;; 2024-4-6

(in-package #:gtk4-example)

(defun do-color-chooser-widget (&optional application)
  (let* ((color-chooser (make-instance 'gtk:color-chooser-widget
                                       :margin-top 12
                                       :margin-bottom 12
                                       :margin-start 12
                                       :margin-end 12))
         (window (make-instance 'gtk:window
                                 :application application
                                 :child color-chooser
                                 :title "Color Chooser Widget"
                                 :border-width 12
                                 :default-width 400)))
    (g:signal-connect color-chooser "color-activated"
        (lambda (chooser color)
          (declare (ignore chooser))
          (format t "Selected color is ~a~%" (gdk:rgba-to-string color))))
    (gtk:window-present window)))
