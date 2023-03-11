;;;; Example Simple Window - 2022-11-11
;;;;
;;;; This example shows a very simple window. The window has the title "Window".
;;;; The window can be sized and moved.

(in-package :gtk4-example)

(defun do-window-simple (&optional (application nil))
  (let (;; Create a toplevel window.
        (window (make-instance 'gtk:window
                               :title "Window")))
    ;; Add the window to the given application
    (setf (gtk:window-application window) application)
    ;; Show the window.
    (gtk:widget-show window)))
