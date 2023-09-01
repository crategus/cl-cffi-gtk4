;;;; Example File Chooser Dialog - 2023-8-22

(in-package :gtk4-example)

(defun create-file-chooser-dialog (parent)
  (let ((filter-all (gtk:file-filter-new))
        (filter-picture (gtk:file-filter-new))
        (dialog (gtk:file-chooser-dialog-new "File Chooser Dialog"
                                             parent
                                             :open
                                             "open" :accept
                                             "cancel" :cancel)))
    (setf (gtk:window-modal dialog) t)
    (g:signal-connect dialog "response"
                      (lambda (dialog response)
                        (declare (ignore response))
                        (format t "Selected file is ~a~%"
                                (gtk:file-chooser-namestring dialog))
                        (gtk:window-destroy dialog)))
    ;; Add a file filter
    (setf (gtk:file-filter-name filter-all) "All Files")
    (gtk:file-filter-add-pattern filter-all "*")
    (gtk:file-chooser-add-filter dialog filter-all)
    ;; Add a file filter for pictures
    (setf (gtk:file-filter-name filter-picture) "All Pictures")
    (gtk:file-filter-add-pixbuf-formats filter-picture)
    (gtk:file-chooser-add-filter dialog filter-picture)
    ;; Show the dialog
    (setf (gtk:widget-visible dialog) t)))
