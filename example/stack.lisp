;;;; Stack
;;;;
;;;; The <tt>GtkStack</tt> widget is a container that shows a single child at a
;;;; time, with nice transitions when the visible child changes.
;;;; The <tt>GtkStackSwitcher</tt> widget adds buttons to control which child
;;;; is visible.
;;;;
;;;; 2024-12-4

(in-package :gtk4-example)

(defun do-stack (&optional (application nil))
  (let* ((path (glib-sys:sys-path "resource/stack.ui"))
         (builder (gtk:builder-new-from-file path))
         (window (gtk:builder-object builder "window1")))
    (setf (gtk:window-application window) application)
    (gtk:window-present window)))
