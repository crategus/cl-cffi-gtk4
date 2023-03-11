;;;; Example Statusbar - 2022-7-23

(in-package :gtk4-example)

(defun do-statusbar (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :homogeneous nil
                              :spacing 3))
         (window (make-instance 'gtk:window
                                :title "Example Statusbar"
                                :child vbox
                                :application application
                                :default-width 300))
         (statusbar (make-instance 'gtk:statusbar))
         (id (gtk:statusbar-context-id statusbar "Example Statusbar"))
         (count 0))

    (gtk:box-append vbox statusbar)

    (let ((button (gtk:button-new-with-label "Push Item")))
      (g:signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (setq count (+ 1 count))
           (gtk:statusbar-push statusbar id (format nil "Item ~A" count))))
      (gtk:box-append vbox button))

    (let ((button (gtk:button-new-with-label "Pop Item")))
      (g:signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (gtk:statusbar-pop statusbar id)))
      (gtk:box-append vbox button))

    (gtk:widget-show window)))
