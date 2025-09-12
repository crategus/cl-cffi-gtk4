;;;; List Box Controls
;;;;
;;;; The <tt>GtkListBox</tt> widget is well-suited for creating "button strips"
;;;; - lists of controls for use in preference dialogs or settings panels. To
;;;; create this style of list, use the <tt>.rich-list</tt> style class.
;;;;
;;;; 2025-09-03

(in-package :gtk4-example)

(defun do-list-box-controls (&optional application)
  (let* ((path (glib-sys:sys-path "resource/list-box-controls.ui"))
         (builder (gtk:builder-new-from-file path))
         (window (gtk:builder-object builder "window"))
         (listbox (gtk:builder-object builder "listbox1"))
         (switch (gtk:builder-object builder "switch"))
         (check (gtk:builder-object builder "check"))
         (image (gtk:builder-object builder "image")))
    (setf (gtk:window-application window) application)
    (g:signal-connect listbox "row-activated"
                      (lambda (listbox row)
                        (declare (ignore listbox))
                        (cond ((gtk:widget-is-ancestor switch row)
                               (setf (gtk:switch-active switch)
                                     (not (gtk:switch-active switch))))
                              ((gtk:widget-is-ancestor check row)
                               (setf (gtk:check-button-active check)
                                     (not (gtk:check-button-active check))))
                              ((gtk:widget-is-ancestor image row)
                               (setf (gtk:widget-opacity image)
                                     (- 1.0
                                        (gtk:widget-opacity image))))
                              (t
                               (warn "Unknown widget in ROW found")))))
    (gtk:window-present window)))
