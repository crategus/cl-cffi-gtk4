;;;; Example Fixed Container - 2022-11-12
;;;;
;;;; In this example, three buttons are placed in the fixed container with the
;;;; function gtk:fixed-put. When pressed, the buttons are moved randomly with
;;;; the function gtk:fixed-move.
;;;;
;;;; To get the width and height of the fixed container and the buttons the
;;;; functions gtk:widget-allocated-width and gtk:widget-allocated-height are
;;;; used.

(in-package :gtk4-example)

(defun move-button (fixed button)
  (let ((width (- (gtk:widget-allocated-width fixed)
                  (gtk:widget-allocated-width button)))
        (height (- (gtk:widget-allocated-height fixed)
                   (gtk:widget-allocated-height button))))
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
    (gtk:widget-show window)))
