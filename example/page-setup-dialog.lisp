;;;; Create Page Setup Dialog
;;;;
;;;; 2024-1-5

(in-package :gtk4-example)

(defun create-page-setup-dialog (&optional parent)
  (let* (;(gtk-init:*gtk-warn-deprecated* nil)
         (path (glib-sys:sys-path "resource/page-setup.ini"))
         (pagesetup (gtk:page-setup-new))
         (dialog (gtk:page-setup-unix-dialog-new "Page Setup Dialog" parent)))
    ;; Connect a handler to the "response" signal
    (g:signal-connect dialog "response"
            (lambda (widget response)
              (format t "Response is: ~s~%" response)
              (when (= -5 response)
                (setf pagesetup
                      (gtk:page-setup-unix-dialog-page-setup dialog))
                (gtk:page-setup-to-file pagesetup path))
              (gtk:window-destroy widget)))
    ;; Load and set Page setup from file
    (if (gtk:page-setup-load-file pagesetup path)
        (format t "PAGE SETUP successfully loaded~%")
        (format t "PAGE SETUP cannot be loaded, use standard settings~%"))
    (setf (gtk:page-setup-unix-dialog-page-setup dialog) pagesetup)
    ;; Present dialog
    (gtk:window-present dialog)))
