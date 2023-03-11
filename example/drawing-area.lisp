;;;; Example Drawing area - 2022-11-18

(in-package :gtk4-example)

(defun do-drawing-area (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :application application
                                :child area
                                :title "Drawing Area"
                                :default-width 400
                                :default-height 300)))
    ;; Set a drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (let* ((context (gtk:widget-style-context widget))
                 (color (gtk:style-context-color context)))
            ;; Set the color from the style context of the widget
            (gdk:cairo-set-source-rgba cr color)
            ;; Draw and fill a circle on the drawing area
            (cairo:arc cr
                       (/ width 2.0)
                       (/ height 2.0)
                       (- (/ (min width height) 2.0) 12)
                       0.0
                       (* 2.0 pi))
            (cairo:fill cr))))
    ;; Show the window
    (gtk:widget-show window)))
