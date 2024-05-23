;;;; File Chooser Dialog
;;;;
;;;; The <tt>gtk:file-chooser-dialog</tt> widget is a dialog suitable for use
;;;; with "File/Open" or "File/Save as" commands. This widget works by putting
;;;; a <tt>gtk:file-chooser-widget</tt> widget inside a <tt>gtk:dialog</tt>
;;;; widget. It exposes the <tt>gtk:file-chooser</tt> interface, so you can use
;;;; all of the <tt>gtk:file-chooser</tt> functions on the file chooser dialog
;;;; as well as those for the <tt>gtk:dialog</tt> widget.
;;;;
;;;; If you want to integrate well with the platform you should use the
;;;; <tt>gtk:file-chooser-native</tt> API, which will use a platform-specific
;;;; dialog if available and fall back to the <tt>gtk:file-chooser-dialog</tt>
;;;; widget otherwise.
;;;;
;;;; Last version: 2024-5-20

(in-package :gtk4-example)

(defun create-file-chooser-dialog (parent)
  (let ((filter-all (gtk:file-filter-new))
        (filter-picture (gtk:file-filter-new))
        (dialog (gtk:file-chooser-dialog-new "File Chooser Dialog"
                                             parent
                                             :open
                                             "Open" 100
                                             "Cancel" :cancel)))
    (setf (gtk:window-modal dialog) t)
    (g:signal-connect dialog "response"
                      (lambda (dialog response)
                        (format t "  Response is ~a~%" response)
                        (unless (eq :cancel
                                    (gtk:response-type-keyword response))
                          (format t "Selected file is ~a~%"
                                  (gtk:file-chooser-namestring dialog)))
                        (gtk:window-destroy dialog)))
    ;; Add a file filter
    (setf (gtk:file-filter-name filter-all) "All Files")
    (gtk:file-filter-add-pattern filter-all "*")
    (gtk:file-chooser-add-filter dialog filter-all)
    ;; Add a second file filter for pictures
    (setf (gtk:file-filter-name filter-picture) "All Pictures")
    (gtk:file-filter-add-pixbuf-formats filter-picture)
    (gtk:file-chooser-add-filter dialog filter-picture)
    ;; Present the dialog
    (gtk:window-present dialog)))
