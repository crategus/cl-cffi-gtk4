;;;; Example Stack - 2023-8-9
;;;;
;;;; GtkStack is a container that shows a single child at a time, with
;;;; nice transitions when the visible child changes.
;;;; GtkStackSwitcher adds buttons to control which child is visible.

(in-package :gtk4-example)

#+nil
(defun example-stack (&optional (application nil))
  (within-main-loop
    (let* ((builder (gtk:builder-new-from-file (sys-path "resource/stack.ui")))
           (window (gtk:builder-object builder "window1")))
      (setf (gtk:window-application window) application)
      (g:signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))
      (gtk:widget-show-all window))))

(defun do-stack (&optional (application nil))
  (let* ((builder (gtk:builder-new-from-file (sys-path "resource/stack.ui")))
         (window (gtk:builder-object builder "window1")))
    (setf (gtk:window-application window) application)
    (setf (gtk:widget-visible window) t)))
