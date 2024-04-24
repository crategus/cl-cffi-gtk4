;;;; Fixed Container
;;;;
;;;; In this example, three buttons are placed in the fixed container with the
;;;; <tt>gtk:fixed-put</tt> function. When pressed, the buttons are moved
;;;; randomly with the <tt>gtk:fixed-move</tt> function.
;;;;
;;;; To get the width and height of the fixed container and the buttons the
;;;; <tt>gtk:widget-width</tt> and <tt>gtk:widget-height</tt> functions are
;;;; used.
;;;;
;;;; The position of the button in the fixed container is retrieved with the
;;;; <tt>gtk:fixed-child-position</tt> function and the child transform with
;;;; with the <tt>gtk:fixed-child-transform</tt> function.
;;;;
;;;; 2024-4-19

(in-package :gtk4-example)

(defun move-button (fixed button)
  (let ((width (- (gtk:widget-width fixed)
                  (* 2 (gtk:widget-width button))))
        (height (- (gtk:widget-height fixed)
                   (* 2 (gtk:widget-height button)))))
    ;; Move button
    (gtk:fixed-move fixed button (random width) (random height))
    ;; Print info about transform on the console
    (let ((transform (gtk:fixed-child-transform fixed button)))
      (format t "~%Position and child transform~%")
      (format t "   position : ~a~%" (multiple-value-list
                                       (gtk:fixed-child-position fixed button)))
      (format t "  transform : ~a~%" (gsk:transform-to-string transform))
      (format t "   category : ~a~%" (gsk:transform-category transform)))))

(defun do-fixed (&optional application)
  (let* ((fixed (make-instance 'gtk:fixed))
         (window (make-instance 'gtk:window
                                :child fixed
                                :title "Fixed Container"
                                :application application
                                :default-width 380
                                :default-height 240)))
    (dotimes (i 3)
      (let ((button (gtk:button-new-with-label "Press me")))
        (g:signal-connect button "clicked"
                          (lambda (widget)
                            (move-button fixed widget)))
        (gtk:fixed-put fixed button (random 280) (random 160))))
    (gtk:window-present window)))
