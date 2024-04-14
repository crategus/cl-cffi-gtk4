;;;; Stack
;;;;
;;;; The <tt>GtkStack</tt> widget is a container that shows a single child at a
;;;; time, with nice transitions when the visible child changes.
;;;; The <tt>GtkStackSwitcher</tt> widget adds buttons to control which child
;;;; is visible.
;;;;
;;;; 2024-4-14

(in-package :gtk4-example)

(defun do-stack (&optional (application nil))
  (let* ((builder (gtk:builder-new-from-file (sys-path "resource/stack.ui")))
         (window (gtk:builder-object builder "window1")))
    (setf (gtk:window-application window) application)
    (setf (gtk:widget-visible window) t)))
