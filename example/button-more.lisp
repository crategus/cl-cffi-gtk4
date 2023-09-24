;;;; Example More Buttons - 2023-9-19

(in-package :gtk4-example)

(defun on-button-clicked (parent button)
  (let* ((child (gtk:button-child button))
         (gtype (g:type-from-instance child))
         (message (cond ((eq gtype (g:gtype "GtkLabel"))
                         (format nil "Button ~a clicked"
                                     (gtk:button-label button)))
                        ((eq gtype (g:gtype "GtkBox"))
                         (format nil "Button ~a clicked"
                                     (gtk:label-label
                                         (gtk:widget-last-child child))))
                        (t
                         (format nil "Button has no label"))))
         (dialog (make-instance 'gtk:alert-dialog
                                :message message
                                :detail "Signal handler for button called."
                                :modal t)))
    (gtk:alert-dialog-show dialog parent)))

(defun do-button-more (&optional application)
  (let* ((grid (make-instance 'gtk:grid
                              :column-spacing 9
                              :row-spacing 9
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 12
                              :margin-end 12
                              :halign :center))
         (window (make-instance 'gtk:window
                                :title "More Buttons"
                                :child grid
                                :application application
                                :resizable nil))
         (group (make-instance 'gtk:size-group
                               :mode :horizontal))
         (button nil)
         (box nil))
    ;; These are the standard functions to create a button
    (gtk:grid-attach grid
                     (setf button (gtk:button-new-with-label "Label"))
                     0 0 1 1)
    (g:signal-connect button "clicked"
                      (lambda (button)
                        (on-button-clicked window button)))
    (gtk:size-group-add-widget group button)
    (gtk:grid-attach grid
                     (setf button (gtk:button-new-with-mnemonic "_Mnemonic"))
                     0 1 1 1)
    (g:signal-connect button "clicked"
                      (lambda (button)
                        (on-button-clicked window button)))
    (gtk:size-group-add-widget group button)
    (gtk:grid-attach grid
                     (setf button (gtk:button-new-from-icon-name "edit-paste"))
                     0 2 1 1)
    (g:signal-connect button "clicked"
                      (lambda (button)
                        (on-button-clicked window button)))
    (gtk:size-group-add-widget group button)
    ;; Create some buttons with an image and a label
    (setf box (make-instance 'gtk:box
                             :orientation :horizontal
                             :spacing 9))
    (gtk:box-append box (make-instance 'gtk:image :icon-name "edit-clear"))
    (gtk:box-append box (make-instance 'gtk:label :label "Edit Clear"))
    (setf button (make-instance 'gtk:button :child box))
    (gtk:grid-attach grid button 1 0 1 1)
    (g:signal-connect button "clicked"
                      (lambda (button)
                        (on-button-clicked window button)))
    (gtk:size-group-add-widget group button)
    ;; A second button
    (setf box (make-instance 'gtk:box
                             :orientation :horizontal
                             :spacing 9))
    (gtk:box-append box (make-instance 'gtk:image :icon-name "edit-copy"))
    (gtk:box-append box (make-instance 'gtk:label :label "Edit Copy"))
    (setf button (make-instance 'gtk:button :child box))
    (gtk:grid-attach grid button 1 1 1 1)
    (g:signal-connect button "clicked"
                      (lambda (button)
                        (on-button-clicked window button)))
    (gtk:size-group-add-widget group button)
    ;; A third button
    (setf box (make-instance 'gtk:box
                             :orientation :horizontal
                             :spacing 9))
    (gtk:box-append box (make-instance 'gtk:image :icon-name "edit-cut"))
    (gtk:box-append box (make-instance 'gtk:label :label "Edit Cut"))
    (setf button (make-instance 'gtk:button :child box))
    (g:signal-connect button "clicked"
                      (lambda (button)
                        (on-button-clicked window button)))
    (gtk:grid-attach grid button 1 2 1 1)
    ;; Show the window
    (gtk:size-group-add-widget group button)
    (setf (gtk:widget-visible window) t)))
