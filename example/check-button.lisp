;;;; Example Check Buttons - 2022-9-8

(in-package :gtk4-example)

(defun do-check-button (&optional application)
  (let* ((grid (make-instance 'gtk:grid
                              :halign :center
                              :column-spacing 12
                              :row-spacing 12
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 12
                              :margin-end 12))
         (window (make-instance 'gtk:window
                                :title "Check Buttons"
                                :child grid
                                :application application
                                :resizable nil)))
    ;; Create three radio buttons and put the buttons into the grid
    (let ((group nil)
          (button (gtk:check-button-new-with-label "Radio Button 1")))
      (gtk:grid-attach grid button 0 0 1 1)
      (setf group button)
      ;; Create and add the second radio button to the group
      (setf button (gtk:check-button-new-with-label "Radio Button 2"))
      (setf (gtk:check-button-group button) group)
      (gtk:grid-attach grid button 0 1 1 1)
      ;; Make the second button active
      (setf (gtk:check-button-active button) t)
      ;; Create and add the third radio button to the group
      (setf button (gtk:check-button-new-with-label "Radio Button 3"))
      (setf (gtk:check-button-group button) group)
      (gtk:grid-attach grid button 0 2 1 1))
    ;; Create three check buttons and put the buttons into the grid
    (gtk:grid-attach grid
                     (gtk:check-button-new-with-mnemonic "Check Button _1")
                     1 0 1 1)
    (gtk:grid-attach grid
                     (gtk:check-button-new-with-mnemonic "Check Button _2")
                       1 1 1 1)
    (gtk:grid-attach grid
                     (gtk:check-button-new-with-mnemonic "Check Button _3")
                     1 2 1 1)
    ;; Make the first check button active
    (setf (gtk:check-button-active (gtk:grid-child-at grid 1 0)) t)
    ;; Show the window and its content
    (gtk:widget-show window)))
