;;;; Cursors
;;;;
;;;; Demonstrates a useful set of available cursors. The cursors shown here are
;;;; the ones defined by CSS, which we assume to be available. The example shows
;;;; creating cursors by name or from an image, with or without a fallback.
;;;;
;;;; Last version: 2024-5-15

(in-package :gtk4-example)

(defun do-cursors (&optional application)
  (let* ((path (glib-sys:sys-path "resource/cursors.ui"))
         (builder (gtk:builder-new-from-file path))
         (window (gtk:builder-object builder "window")))
    (setf (gtk:window-application window) application)
    (gtk:window-present window)))
