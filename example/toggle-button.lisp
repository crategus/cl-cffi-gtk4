;;;; Toggle Buttons
;;;;
;;;; 2024-4-6

(in-package :gtk4-example)

(defun on-toggle-button-toggled (button label)
  (when (gtk:toggle-button-active button)
    (setf (gtk:label-label label)
          (format nil "<b>~a is active</b>" (gtk:button-label button)))))

(defun do-toggle-button (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :spacing 12
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 48
                              :margin-end 48))
         (window (make-instance 'gtk:window
                                :title "Toggle Buttons"
                                :child vbox
                                :application application
                                :resizable nil))
         (label (make-instance 'gtk:label
                               :margin-top 9
                               :margin-bottom 6
                               :label "<b>Toogle Button ? is active</b>"
                               :use-markup t
                               :use-underline t))
         (group nil) (button nil))
    ;; Create three grouped toggle buttons
    (setf button (gtk:toggle-button-new-with-mnemonic "Toggle Button _1"))
    (g:signal-connect button "toggled"
                             (lambda (button)
                               (on-toggle-button-toggled button label)))
    (gtk:box-append vbox button)
    (setf group button)
    ;; Create and add the second radio button to the group
    (setf button (gtk:toggle-button-new-with-mnemonic "Toggle Button _2"))
    (g:signal-connect button "toggled"
                             (lambda (button)
                               (on-toggle-button-toggled button label)))
    (setf (gtk:toggle-button-group button) group)
    (gtk:box-append vbox button)
    ;; Make the second button active
    (setf (gtk:toggle-button-active button) t)
    ;; Create and add the third toggle button to the group
    (setf button (gtk:toggle-button-new-with-mnemonic "Toggle Button _3"))
    (g:signal-connect button "toggled"
                             (lambda (button)
                               (on-toggle-button-toggled button label)))
    (setf (gtk:toggle-button-group button) group)
    (gtk:box-append vbox button)
    ;; Add a label which shows the status of the toggle buttons
    (gtk:box-append vbox label)
    ;; Show the window
    (gtk:window-present window)))
