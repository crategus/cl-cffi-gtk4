;;;; Check Buttons
;;;;
;;;; The <tt>gtk:check-button</tt> widget places a label next to an indicator.
;;;; The <tt>gtk:check-button</tt> widget is created by calling either the
;;;; <tt>gtk:check-button-new</tt> or <tt>gtk:check-button-new-with-label</tt>
;;;; functions. The state of a check button can be set specifically using the
;;;; <tt>gtk:check-button-active</tt> function.
;;;;
;;;; Check buttons can be grouped together, to form mutually exclusive groups.
;;;; Only one of the buttons can be toggled at a time, and toggling another one
;;;; will switch the currently toggled one off. Grouped check buttons use a
;;;; different indicator, and are commonly referred to as radio buttons. To add
;;;; a <tt>gtk:check-button</tt> widget to a group, use the
;;;; <tt>gtk:check-button-group</tt> function.
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
    (gtk:grid-attach grid button 0 0)
    ;; Create and add the second radio button to the group
    (setf button (gtk:check-button-new-with-label "Radio 2"))
    (setf (gtk:check-button-group button) group)
    (gtk:grid-attach grid button 0 1)
    ;; Make second radio button active
    (setf (gtk:check-button-active button) t)
    ;; Create and add the third radio button to the group
    (setf button (gtk:check-button-new-with-label "Radio 3"))
    (setf (gtk:check-button-group button) group)
    (gtk:grid-attach grid button 0 2)
    ;; Create three check buttons and put the buttons into the grid
    (gtk:grid-attach grid
                     (gtk:check-button-new-with-mnemonic "Check _1")
                     1 0)
    (gtk:grid-attach grid
                     (gtk:check-button-new-with-mnemonic "Check _2")
                       1 1)
    (gtk:grid-attach grid
                     (gtk:check-button-new-with-mnemonic "Check _3")
                     1 2)
    ;; Make first check button active
    (setf (gtk:check-button-active (gtk:grid-child-at grid 1 0)) t)
    ;; Present window
    (gtk:window-present window)))
