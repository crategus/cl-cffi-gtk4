;;;; GtkFontDialog
;;;;
;;;; Last version: 2026-01-04

(in-package :gtk4-example)

(defun do-font-dialog (&optional application)
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (font (pango:font-map-load-font fontmap context desc))
         (grid (make-instance 'gtk:grid
                              :margin-start 20
                              :margin-end 20
                              :margin-top 20
                              :margin-bottom 20
                              :row-spacing 6
                              :column-spacing 6))
         (window (make-instance 'gtk:window
                                :title "Font Dialog"
                                :application application
                                :child grid)))
    ;; Font Button
    (let ((label (make-instance 'gtk:label
                                :label "Font Button"
                                :halign :start
                                :valign :center
                                :hexpand t))
          (picker (make-instance 'gtk:font-dialog-button
                                 :dialog (make-instance 'gtk:font-dialog))))
      (setf (gtk:label-mnemonic-widget label) picker)
      (gtk:grid-attach grid label 0 0 1 1)
      (gtk:grid-attach grid picker 1 0 1 1))
    ;; Font Dialog for choose font
    (let ((label (make-instance 'gtk:label
                                 :label "Choose Font"
                                 :halign :start
                                 :valign :center
                                 :hexpand t))
          (button (make-instance 'gtk:button
                                 :label "Choose Font")))
      (g:signal-connect button "clicked"
          (lambda (button)
            (let* ((parent (gtk:widget-root button))
                   (dialog (make-instance 'gtk:font-dialog
                                          :title "Choose Font")))
              (gtk:font-dialog-choose-font dialog parent desc nil
                  (lambda (source result)
                    (let ((desc (gtk:font-dialog-choose-font-finish source result)))
                      (when desc
                        (setf (gtk:button-label button)
                              (pango:font-description-to-string desc)))))))))
      (gtk:grid-attach grid label 0 1 1 1)
      (gtk:grid-attach grid button 1 1 1 1))
    ;; Font Dialog for choose font & features
    (let ((label (make-instance 'gtk:label
                                 :label "Choose Font & Features"
                                 :halign :start
                                 :valign :center
                                 :hexpand t))
          (button (make-instance 'gtk:button
                                 :label "Choose Font & Features")))
      (g:signal-connect button "clicked"
          (lambda (button)
            (let* ((parent (gtk:widget-root button))
                   (dialog (make-instance 'gtk:font-dialog
                                          :title "Choose Font & Features")))
              (gtk:font-dialog-choose-font-and-features dialog parent desc nil
                  (lambda (source result)
                    (multiple-value-bind (desc features language)
                        (gtk:font-dialog-choose-font-and-features-finish source result)
                      (declare (ignore features language))
                      (when desc
                        (setf (gtk:button-label button)
                              (pango:font-description-to-string desc)))))))))
      (gtk:grid-attach grid label 0 2 1 1)
      (gtk:grid-attach grid button 1 2 1 1))
    ;; Font dialog for choose family
    (let ((label (make-instance 'gtk:label
                                 :label "Choose Family"
                                 :halign :start
                                 :valign :center
                                 :hexpand t))
          (button (make-instance 'gtk:button
                                 :label "Choose Family")))
      (g:signal-connect button "clicked"
          (lambda (button)
            (let* ((parent (gtk:widget-root button))
                   (dialog (make-instance 'gtk:font-dialog
                                          :title "Choose Family"))
                   (family (pango:font-map-family fontmap "Serif")))
              (gtk:font-dialog-choose-family dialog parent family nil
                  (lambda (source result)
                    (let ((family (gtk:font-dialog-choose-family-finish source result)))
                      (when family
                        (setf (gtk:button-label button)
                              (pango:font-family-name family)))))))))
      (gtk:grid-attach grid label 0 3 1 1)
      (gtk:grid-attach grid button 1 3 1 1))
    ;; Font dialog for choose face
    (let ((label (make-instance 'gtk:label
                                 :label "Choose Face"
                                 :halign :start
                                 :valign :center
                                 :hexpand t))
          (button (make-instance 'gtk:button
                                 :label "Choose Face")))
      (g:signal-connect button "clicked"
          (lambda (button)
            (let* ((parent (gtk:widget-root button))
                   (dialog (make-instance 'gtk:font-dialog
                                          :title "Choose Face"))
                   (face (pango:font-face font)))
              (gtk:font-dialog-choose-face dialog parent face nil
                  (lambda (source result)
                    (let ((face (gtk:font-dialog-choose-face-finish source result)))
                      (when face
                        (setf (gtk:button-label button)
                              (pango:font-face-face-name face)))))))))
      (gtk:grid-attach grid label 0 4 1 1)
      (gtk:grid-attach grid button 1 4 1 1))
    (gtk:window-present window)))
