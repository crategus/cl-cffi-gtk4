;;;; Font Chooser Button - 2022-11-20

(in-package :gtk4-example)

(defun font-filter (family face)
  (declare (ignore face))
  (member (pango:font-family-name family)
          '("Sans" "Serif")
          :test #'equal))

(defun do-font-button (&optional application)
  (let* ((button (make-instance 'gtk:font-button
                                :use-font t
                                :use-size t))
         (window (make-instance 'gtk:window
                                :child button
                                :application application
                                :title "Example Font Chooser Button"
                                :default-width 300
                                :default-height 100)))
    ;; Set a filter function to select fonts for the font chooser
    (gtk:font-chooser-set-filter-func button #'font-filter)
    (g:signal-connect button "font-set"
       (lambda (widget)
         (declare (ignore widget))
         (format t "Font is set:~%")
         (format t "   Font name   : ~A~%"
                   (gtk:font-chooser-font button))
         (format t "   Font family : ~A~%"
                   (pango:font-family-name
                     (gtk:font-chooser-font-family button)))
         (format t "   Font face   : ~A~%"
                   (pango:font-face-face-name
                     (gtk:font-chooser-font-face button)))
         (format t "   Font size   : ~A~%"
                   (gtk:font-chooser-font-size button))))
    (gtk:widget-show window)))
