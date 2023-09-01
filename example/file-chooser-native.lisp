;;;; Example GtkFileChooserNative - 2023-8-30

(in-package :gtk4-example)

(defun create-file-chooser-native (&optional parent)
  (let ((native (gtk:file-chooser-native-new "Open File"
                                             parent
                                             :open
                                             "_Open"
                                             "_Cancel")))
    ;; Connect a signal handler
    (g:signal-connect native "response"
        (lambda (dialog response)
          (when (= -3 response) ; -3 for the :accept value
            (let* ((file (gtk:file-chooser-file dialog))
                   (launcher (gtk:file-launcher-new file)))
              ;; Open the file
              (gtk:file-launcher-launch launcher parent nil
                  (lambda (source result)
                    (declare (ignore source result))
                    (format t "Opened the file ~a~%"
                              (g:file-basename file))))))))
    ;; Show the native file chooser
    (gtk:native-dialog-show native)))
