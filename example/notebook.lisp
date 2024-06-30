;;;; Notebook
;;;;
;;;; 2024-4-14

(in-package :gtk4-example)

(defun do-notebook (&optional application)
  (let* ((notebook (make-instance 'gtk:notebook
                                  :enable-popup t))
         (window (make-instance 'gtk:window
                                :title "Notebook"
                                :application application
                                :child notebook
                                :default-width 300
                                :default-height 210)))
    (dotimes (i 3)
      (let ((page (make-instance 'gtk:label
                                 :label
                                 (format nil
                                         "Text for page ~a" (1+ i))))
            (label (make-instance 'gtk:label
                                  :label (format nil "Page ~a" (1+ i)))))
        (gtk:notebook-add-page notebook page label)
        ;; Make the tabs detachable
        (setf (gtk:notebook-tab-detachable notebook page) t)
        ;; Make the tabs reorderable
        (setf (gtk:notebook-tab-reorderable notebook page) t)))
    (gtk:window-present window)))
