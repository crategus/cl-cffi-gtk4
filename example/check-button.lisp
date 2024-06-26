;;;; Check Buttons
;;;;
;;;; 2024-5-4

(in-package :gtk4-example)

(defun do-check-button (&optional application)
  (let* ((grid (make-instance 'gtk:grid
                              :column-spacing 24
                              :row-spacing 12
                              :halign :center
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 12
                              :margin-end 12))
         (window (make-instance 'gtk:window
                                :title "Check Buttons"
                                :child grid
                                :application application
                                :resizable nil))
         (group nil) (button nil))
    ;; Create three radio buttons and put the buttons into the grid
    (setf button (gtk:check-button-new-with-label "Radio 1"))
    (setf group button)
    (gtk:grid-attach grid button 0 0 1 1)
    ;; Create and add the second radio button to the group
    (setf button (gtk:check-button-new-with-label "Radio 2"))
    (setf (gtk:check-button-group button) group)
    (gtk:grid-attach grid button 0 1 1 1)
    ;; Make second radio button active
    (setf (gtk:check-button-active button) t)
    ;; Create and add the third radio button to the group
    (setf button (gtk:check-button-new-with-label "Radio 3"))
    (setf (gtk:check-button-group button) group)
    (gtk:grid-attach grid button 0 2 1 1)
    ;; Create three check buttons and put the buttons into the grid
    (gtk:grid-attach grid
                     (gtk:check-button-new-with-mnemonic "Check _1")
                     1 0 1 1)
    (gtk:grid-attach grid
                     (gtk:check-button-new-with-mnemonic "Check _2")
                       1 1 1 1)
    (gtk:grid-attach grid
                     (gtk:check-button-new-with-mnemonic "Check _3")
                     1 2 1 1)
    ;; Make first check button active
    (setf (gtk:check-button-active (gtk:grid-child-at grid 1 0)) t)
    ;; Present window
    (gtk:window-present window)))
