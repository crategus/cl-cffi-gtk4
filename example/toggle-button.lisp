;;;; Example Toggle Buttons - 2022-11-18

(in-package :gtk4-example)

(defun do-toggle-button (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :halign :center
                              :spacing 12
                              :margin-top 12
                              :margin-bottom 12))
         (window (make-instance 'gtk:window
                                :title "Toggle Buttons"
                                :child vbox
                                :application application
                                :resizable nil)))
    ;; Create three toggle buttons and put the buttons into the box
    (let ((group nil)
          (button (gtk:toggle-button-new-with-mnemonic "Toggle Button _1")))
      (gtk:box-append vbox button)
      (setf group button)
      ;; Create and add the second radio button to the group
      (setf button
            (gtk:toggle-button-new-with-mnemonic "Toggle Button _2"))
      (setf (gtk:toggle-button-group button) group)
      (gtk:box-append vbox button)
      ;; Make the second button active
      (setf (gtk:toggle-button-active button) t)
      ;; Create and add the third toggle button to the group
      (setf button
            (gtk:toggle-button-new-with-mnemonic "Toggle Button _3"))
      (setf (gtk:toggle-button-group button) group)
      (gtk:box-append vbox button))
    (gtk:widget-show window)))
