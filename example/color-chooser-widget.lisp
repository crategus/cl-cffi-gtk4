;;;; Color Chooser Widget
;;;;
;;;; The <tt>gtk:color-chooser-widget</tt> widget lets the user select a color.
;;;; By default, the chooser presents a prefined palette of colors, plus a small
;;;; number of settable custom colors. It is also possible to select a different
;;;; color with the single-color editor. To enter the single-color editing mode,
;;;; use the context menu of any color of the palette, or use the '+' button to
;;;; add a new custom color.
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
