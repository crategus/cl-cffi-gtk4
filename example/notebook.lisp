;;;; Notebook
;;;;
;;;; The <tt>gtk:notebook</tt> widget is a layout container whose children are
;;;; pages that can be switched between using tab labels along one edge.
;;;;
;;;; There are many configuration options for <tt>gtk:notebook</tt> widgets.
;;;; Among other things, you can choose on which edge the tabs appear, see the
;;;; <tt>gtk:notebook-tab-pos</tt> function, whether, if there are too many tabs
;;;; to fit the notebook should be made bigger or scrolling arrows added, see
;;;; the <tt>gtk:notebook-scrollable</tt> function, and whether there will be a
;;;; popup menu allowing the users to switch pages, see the
;;;; <tt>gtk:notebook-popup-enable</tt> function.
;;;;
;;;; Last version: 2024-10-3

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
            (menu (make-instance 'gtk:label
                                 :label
                                 (format nil "Switch to page ~a" (1+ i))))
            (label (make-instance 'gtk:label
                                  :label (format nil "Page ~a" (1+ i)))))
        ;; Add notebook page with label und menu label
        (gtk:notebook-add-page notebook page label :menu menu)
        ;; Make tabs detachable
        (setf (gtk:notebook-tab-detachable notebook page) t)
        ;; Make tabs reorderable
        (setf (gtk:notebook-tab-reorderable notebook page) t)))
    (gtk:window-present window)))
