;;;; Example File Chooser Dialog - 2022-11-19

;; TODO: Improve this example for usage in the GTK demo.
;; Show the usage of shortcut folders.

(in-package :gtk4-example)

(defun create-file-chooser-dialog (parent)
  (let ((filter-all (gtk:file-filter-new))
        (filter-picture (gtk:file-filter-new))
        (dialog (gtk:file-chooser-dialog-new "File Chooser Dialog"
                                             parent
                                             :open
                                             "open" :accept
                                             "cancel" :cancel)))
    (g:signal-connect dialog "response"
                      (lambda (widget response)
                        (declare (ignore response))
                        (gtk:window-destroy widget)))
    ;; Add a file filter
    (setf (gtk:file-filter-name filter-all) "All Files")
    (gtk:file-filter-add-pattern filter-all "*")
    (gtk:file-chooser-add-filter dialog filter-all)
    ;; Add a file filter for pictures
    (setf (gtk:file-filter-name filter-picture) "All Pictures")
    (gtk:file-filter-add-pixbuf-formats filter-picture)
    (gtk:file-chooser-add-filter dialog filter-picture)
    ;; Show the dialog
    (gtk:widget-show dialog)))


(defun do-file-chooser-dialog (&optional application)
  (let ((parent (make-instance 'gtk:window
                               :application application
                               :title "File Chooser Dialog")))
    (create-file-chooser-dialog parent)
    (gtk:widget-show parent)))
