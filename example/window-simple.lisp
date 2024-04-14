;;;; Simple Window
;;;;
;;;; This example shows a very simple window. The window has the title
;;;; "Simple Window". The window can be sized and moved.
;;;;
;;;; 2024-4-10

(in-package :gtk4-example)

(defun do-window-simple (&optional application)
  (let (;; Create a toplevel window
        (window (make-instance 'gtk:window
                               :title "Simple Window")))
    ;; Add the window to the given application
    (setf (gtk:window-application window) application)
    ;; Show the window.
    (gtk:window-present window)))
