;;;; Create Page Setup Dialog
;;;;
;;;; Presents a page setup dialog for platforms which do not provide a native
;;;; page setup dialog, like Unix.
;;;;
;;;; 2026-02-13

(in-package :gtk4-example)

(defun create-page-setup-dialog (&optional parent)
  (let* ((path (glib-sys:sys-path "resource/page-setup.ini"))
         (dialog (gtk:page-setup-unix-dialog-new "Page Setup Dialog" parent))
         (pagesetup (gtk:page-setup-new)))
    ;; Connect a handler to the "response" signal
    (g:signal-connect dialog "response"
            (lambda (widget response)
              (when (= -5 response) ; response type is :ok
                (format t "Update page setup and print settings.~%")
                ;; Get page setup and store it
                (setf pagesetup
                      (gtk:page-setup-unix-dialog-page-setup dialog))
                (gtk:page-setup-to-file pagesetup path))
              (gtk:window-destroy widget)))
    ;; Load and set page setup from file
    (if (gtk:page-setup-load-file pagesetup path)
        (format t "Page setup successfully loaded.~%")
        (format t "Page setup cannot be loaded.~%"))
    (setf (gtk:page-setup-unix-dialog-page-setup dialog) pagesetup)
    ;; Present dialog
    (gtk:window-present dialog)))
