;;;; Example Spinner - 2022-11-18

;;;; GtkSpinner allows to show that background activity is on-going.

(in-package :gtk4-example)

(defun do-spinner (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :margin-top 6
                              :margin-bottom 6
                              :margin-start 6
                              :margin-end 6
                              :spacing 3))
         (window (make-instance 'gtk:window
                                :title "Example Spinner"
                                :child vbox
                                :application application))
         (sensitive (gtk:spinner-new))
         (insensitive (gtk:spinner-new)))

    (let ((hbox (make-instance 'gtk:box
                               :orientation :horizontal
                               :spacing 6
                               :sensitive t)))
      (gtk:box-append hbox sensitive)
      (gtk:box-append hbox (make-instance 'gtk:entry
                                          :text "Sensitive Spinner"))
      (gtk:box-append vbox hbox))

    (let ((hbox (make-instance 'gtk:box
                               :orientation :horizontal
                               :spacing 6
                               :sensitive nil)))
      (gtk:box-append hbox insensitive)
      (gtk:box-append hbox (make-instance 'gtk:entry
                                          :text "Insensitive Spinner"))
      (gtk:box-append vbox hbox))

    (let ((toggle (make-instance 'gtk:toggle-button
                                 :label "Stop animation"
                                 :margin-top 6)))
      (g:signal-connect toggle "toggled"
          (lambda (button)
            (if (gtk:toggle-button-active button)
                (progn
                  (setf (gtk:button-label button) "Start animation")
                  (gtk:spinner-stop sensitive)
                  (gtk:spinner-stop insensitive))
                (progn
                  (setf (gtk:button-label button) "Stop animation")
                  (gtk:spinner-start sensitive)
                  (gtk:spinner-start insensitive)))))
      (g:signal-emit toggle "toggled")
      (gtk:box-append vbox toggle))
    (gtk:widget-show window)))
