;;;; Example Color Chooser Dialog - 2022-8-28
;;;;
;;;; Clicking on the drawing area opens a color chooser dialog to select a
;;;; background color for the drawing area. The default palette is replaced
;;;; for this color chooser dialog.

(in-package :gtk4-example)

(let ((message "Click to change the background color.")
      (bg-color (gdk:rgba-parse "White"))
      ;; Color palette with 9 Red RGBA colors
      (palette1 (list (gdk:rgba-parse "IndianRed")
                      (gdk:rgba-parse "LightCoral")
                      (gdk:rgba-parse "Salmon")
                      (gdk:rgba-parse "DarkSalmon")
                      (gdk:rgba-parse "LightSalmon")
                      (gdk:rgba-parse "Crimson")
                      (gdk:rgba-parse "Red")
                      (gdk:rgba-parse "FireBrick")
                      (gdk:rgba-parse "DarkRed")))
      ;; Gray palette with 9 gray RGBA colors
      (palette2 (list (gdk:rgba-parse "Gainsboro")
                      (gdk:rgba-parse "LightGray")
                      (gdk:rgba-parse "Silver")
                      (gdk:rgba-parse "DarkGray")
                      (gdk:rgba-parse "Gray")
                      (gdk:rgba-parse "DimGray")
                      (gdk:rgba-parse "LightSlateGray")
                      (gdk:rgba-parse "SlateGray")
                      (gdk:rgba-parse "DarkSlateGray"))))

  (defun do-color-chooser-dialog (&optional application)
    (let* ((area (make-instance 'gtk:drawing-area))
           (window (make-instance 'gtk:window
                                  :title "Color Chooser Dialog"
                                  :application application
                                  :child area
                                  :default-width 400))
           (gesture (make-instance 'gtk:gesture-click)))
      ;; Draw the background color and a hint on the drawing area
      (gtk:drawing-area-set-draw-func area
          (lambda (widget cr width height)
            (declare (ignore widget width height))
            (let (;(cr (pointer cr))
                  (red (gdk:rgba-red bg-color))
                  (green (gdk:rgba-green bg-color))
                  (blue (gdk:rgba-blue bg-color)))
                  ;; Paint the current color on the drawing area
                  (cairo:set-source-rgb cr red green blue)
                  (cairo:paint cr)
                  ;; Print a hint on the drawing area
                  (cairo:set-source-rgb cr (- 1 red)
                                           (- 1 green)
                                           (- 1 blue))
                  (cairo:select-font-face cr "Sans")
                  (cairo:set-font-size cr 12)
                  (cairo:move-to cr 12 24)
                  (cairo:show-text cr message))))
      ;; Add the controller to the drawing area
      (gtk:widget-add-controller area gesture)
      ;; Create and run a color chooser dialog to select a background color
      (g:signal-connect gesture "pressed"
          (lambda (gesture n x y)
            (declare (ignore gesture n x y))
            (let ((dialog (make-instance 'gtk:color-chooser-dialog
                                         :transient-for window
                                         :use-alpha nil)))
              ;; Add a custom palette to the dialog
              (gtk:color-chooser-add-palette dialog :vertical 1 palette1)
              ;; Add a second coustom palette to the dialog
              (gtk:color-chooser-add-palette dialog :vertical 1 palette2)
              ;; Set the actual background color for the color chooser
              (setf (gtk:color-chooser-rgba dialog) bg-color)
              ;; Connect handler to the response signal of the dialog
              (g:signal-connect dialog "response"
                  (lambda (dialog response)
                    (when (= -5 response) ; the :ok value
                      ;; Change the background color for the drawing area
                      (format t "new color is ~a~%"
                                (gtk:color-chooser-rgba dialog))
                      (setf bg-color (gtk:color-chooser-rgba dialog))
                      (gtk:widget-queue-draw area))
                    (gtk:window-destroy dialog)))
              (gtk:widget-show dialog))))
      ;; Show the window
      (gtk:widget-show window))))
