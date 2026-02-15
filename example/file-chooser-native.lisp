;;;; File Chooser Native
;;;;
;;;; The <tt>gtk:file-chooser-native</tt> class is an abstraction of a dialog
;;;; suitable for use with "File Open" or "File Save as" commands. By default,
;;;; this just uses a <tt>gtk:file-chooser-dialog</tt> widget to implement the
;;;; actual dialog. However, on certain platforms, such as Windows and MacOS,
;;;; the native platform file chooser is used instead. When the application is
;;;; running in a sandboxed environment without direct filesystem access such as
;;;; Flatpak, the <tt>gtk:file-chooser-native</tt> object may call the proper
;;;; APIs (portals) to let the user choose a file and make it available to the
;;;; application.
;;;;
;;;; Last version: 2025-3-24

(in-package :gtk4-example)

(defun create-file-chooser-native (&optional parent)
  (let* ((gtk-init:*warn-deprecated* nil)
         (native (gtk:file-chooser-native-new "Open File"
                                              parent
                                              :open
                                              "_Open"
                                              "_Cancel")))
    ;; Connect a signal handler
    (g:signal-connect native "response"
            (lambda (dialog response)
              (when (eq :accept
                        (gtk:response-type-keyword response))
                (let* ((file (gtk:file-chooser-file dialog))
                       (launcher (gtk:file-launcher-new file)))
                  ;; Open the file
                  (gtk:file-launcher-launch launcher parent nil
                      (lambda (source result)
                        (declare (ignore source result))
                        (format t "Open file ~a~%" (g:file-basename file))))
                  (gtk:native-dialog-destroy native)))))
    ;; Show the native file chooser
    (gtk:native-dialog-show native)))
