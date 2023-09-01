;;;; Pickers and Launchers - 2023-8-27
;;;;
;;;; The dialogs are mainly intended for use in preference dialogs.
;;;; They allow to select colors, fonts and applications.
;;;;
;;;; The launchers let you open files or URIs in applications that
;;;; can handle them.

;; TODO: The implementation of opening an URI and drag and drop is missing.

(in-package :gtk4-example)

(defun do-pickers (&optional application)
  (let* ((grid (make-instance 'gtk:grid
                              :margin-start 20
                              :margin-end 20
                              :margin-top 20
                              :margin-bottom 20
                              :row-spacing 6
                              :column-spacing 6))
         (window (make-instance 'gtk:window
                                :title "Pickers and Launchers"
                                :application application
                                :child grid)))
    (let ((label (make-instance 'gtk:label
                                :label "Color:"
                                :halign :start
                                :valign :center
                                :hexpand t))
          (picker (make-instance 'gtk:color-dialog-button
                                 :dialog (make-instance 'gtk:color-dialog))))
      (setf (gtk:label-mnemonic-widget label) picker)
      (gtk:grid-attach grid label 0 0 1 1)
      (gtk:grid-attach grid picker 1 0 1 1))

    (let ((label (make-instance 'gtk:label
                                :label "Font:"
                                :halign :start
                                :valign :center
                                :hexpand t))
          (picker (make-instance 'gtk:font-dialog-button
                                 :dialog (make-instance 'gtk:font-dialog))))
      (setf (gtk:label-mnemonic-widget label) picker)
      (gtk:grid-attach grid label 0 1 1 1)
      (gtk:grid-attach grid picker 1 1 1 1))
    (let ((label (make-instance 'gtk:label
                                :label "File:"
                                :halign :start
                                :valign :center
                                :hexpand t))
          (picker (make-instance 'gtk:box
                                 :orientation :horizontal
                                 :spacing 6))
          (button (make-instance 'gtk:button
                                 :icon-name "document-open-symbolic"))
          (label1 (make-instance 'gtk:label
                                 :label "None"
                                 :xalign 0.0
                                 :ellipsize :middle
                                 :hexpand t))
          (app-picker (make-instance 'gtk:button
                                     :icon-name "emblem-system-symbolic"
                                     :halign :end
                                     :sensitive nil)))
      (g:signal-connect button "clicked"
          (lambda (button)
            (let ((parent (gtk:widget-root button))
                  (dialog (gtk:file-dialog-new)))
              (gtk:file-dialog-open dialog parent nil
                  (lambda (source result)
                    (let ((file (gtk:file-dialog-open-finish source result)))
                      (when file
                        (let ((name (g:file-basename file)))
                          (setf (gtk:label-label label1) name)
                          (setf (gtk:widget-sensitive app-picker) t)
                          (setf (g:object-data app-picker "file") file)))))))))
      (g:signal-connect app-picker "clicked"
          (lambda (button)
            (let* ((parent (gtk:widget-root button))
                   (file (g:object-data button "file"))
                   (launcher (gtk:file-launcher-new file)))
              (gtk:file-launcher-launch launcher parent nil
                  (lambda (source result)
                    (declare (ignore source result))
                    (format t "OPEN-APP-DONE~%"))))))
      (gtk:grid-attach grid label 0 2 1 1)
      (gtk:box-append picker label1)
      (gtk:box-append picker button)
      (gtk:box-append picker app-picker)
      (gtk:grid-attach grid picker 1 2 1 1))
    (gtk:window-present window)))
