;;;; Constraints Builder
;;;;
;;;; The <tt>gtk:constraint-layout</tt> objects can be created in <tt>.ui</tt>
;;;; files, and constraints can be set up at that time as well, as this example
;;;; demonstrates. It shows various ways to do spacing and sizing with
;;;; constraints.
;;;;
;;;; Make the window wider to see the rows react differently
;;;;
;;;; Latest version: 2024-5-15

(in-package :gtk4-example)

(gobject:define-g-object-subclass "ConstraintsGrid" constraints-grid
  (:superclass gtk:widget
   :export t
   :interfaces ())
  nil)

(defun do-constraint-builder (&optional application)
  (let* ((path (glib-sys:sys-path "resource/constraint-builder.ui"))
         (builder (gtk:builder-new-from-file (glib-sys:sys-path path)))
         (window (gtk:builder-object builder "window")))
    (setf (gtk:window-application window) application)
    (gtk:window-present window)))
