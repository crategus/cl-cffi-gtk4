;;;; Fixed Container
;;;;
;;;; In this example, three buttons are placed in the fixed container with the
;;;; <tt>gtk:fixed-put</tt> function. When pressed, the buttons are moved
;;;; randomly with the <tt>gtk:fixed-move</tt> function.
;;;;
;;;; To get the width and height of the fixed container and the buttons the
;;;; <tt>gtk:widget-width</tt> and <tt>gtk:widget-height</tt> are used.
;;;;
;;;; 2024-4-4

(in-package :gtk4-example)

(defun move-button (fixed button)
  (let ((width (- (gtk:widget-width fixed)
                  (gtk:widget-width button)))
        (height (- (gtk:widget-height fixed)
                   (gtk:widget-height button))))
    (gtk:fixed-move fixed button (random width) (random height))
    (format t " position : ~a~%" (multiple-value-list
                                  (gtk:fixed-child-position fixed button)))
    (format t "transform : ~a~%" (gtk:fixed-child-transform fixed button))
    (format t "          : ~a~%" (gsk:transform-to-string
                                   (gtk:fixed-child-transform fixed button)))
    (format t " category : ~a~%" (gsk:transform-category
                                   (gtk:fixed-child-transform fixed button)))))

(defun do-fixed (&optional (application nil))
  (let* ((fixed (make-instance 'gtk:fixed))
         (window (make-instance 'gtk:window
                                :child fixed
                                :title "Fixed Container"
                                :application application
                                :default-width 350
                                :default-height 200)))
    (dotimes (i 3)
      (let ((button (gtk:button-new-with-label "Press me")))
        (g:signal-connect button "clicked"
                          (lambda (widget)
                            (move-button fixed widget)))
        (gtk:fixed-put fixed button (random 250) (random 180))))
    (gtk:window-present window)))
