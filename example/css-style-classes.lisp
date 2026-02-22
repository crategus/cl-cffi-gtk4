;;;; Style Classes
;;;;
;;;; GTK uses CSS for theming. Style classes can be associated with widgets to
;;;; inform the theme about intended rendering.
;;;;
;;;; This demo shows some common examples where theming features of GTK are
;;;; used for certain effects: primary toolbars and linked buttons.
;;;;
;;;; 2026-02-21

(in-package :gtk4-example)

(defun do-css-style-classes (&optional application)
  (let* ((path (glib-sys:sys-path "resource/css-style-classes.ui"))
         (builder (gtk:builder-new-from-file path))
         (grid (gtk:builder-object builder "grid"))
         (window (make-instance 'gtk:window
                                :application application
                                :child grid
                                :title "Style Classes")))
    (gtk:window-present window)))
