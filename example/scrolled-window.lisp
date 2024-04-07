;;;; Scrolled Window
;;;;
;;;; The scrolled window widget is a container that accepts a single child
;;;; widget, makes that child scrollable using either internally added
;;;; scrollbars or externally associated adjustments, and optionally draws a
;;;; frame around the child.
;;;;
;;;; 2024-4-6

(in-package #:gtk4-example)

(defun do-scrolled-window (&optional (application nil))
  (let* ((picture (gtk:picture-new-for-filename (sys-path "resource/ducky.png")))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child picture
                                  :hscrollbar-policy :automatic
                                  :vscrollbar-policy :always))
         (window (make-instance 'gtk:window
                                :title "Example Scrolled Window"
                                :child scrolled
                                :application application
                                :width-request 350
                                :height-request 300)))
      ;; Make the picture unshrinkable.
      (setf (gtk:picture-can-shrink picture) nil)
      ;; Show the window with the scrolled and the picture
      (gtk:window-present window)))
