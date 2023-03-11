;;;; Example Grid Spacing - 2022-11-11

(in-package :gtk4-example)

(defun do-grid-spacing (&optional (application nil))
  (let* ((grid (make-instance 'gtk:grid
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 12
                              :margin-end 12
                              :column-homogeneous t
                              :column-spacing 6
                              :row-homogeneous t
                              :row-spacing 6))
         (window (make-instance 'gtk:window
                                :title "Grid Spacing"
                                :application application
                                :child grid
                                :default-width 320))
         (button1 (make-instance 'gtk:toggle-button
                                 :label "More Row Spacing"))
         (button2 (make-instance 'gtk:toggle-button
                                 :label "More Col Spacing"))
         (button3 (make-instance 'gtk:button
                                 :label "Button")))
    ;; Signal handler for the first toggle button
    (g:signal-connect button1 "toggled"
       (lambda (widget)
         (if (gtk:toggle-button-active widget)
             (progn
               (setf (gtk:grid-row-spacing grid) 48)
               (setf (gtk:button-label widget) "Less Row Spacing"))
             (progn
               (setf (gtk:grid-row-spacing grid) 6)
               (setf (gtk:button-label widget) "More Row Spacing")))))
    ;; Signal handler for the second toggle button
    (g:signal-connect button2 "toggled"
       (lambda (widget)
         (if (gtk:toggle-button-active widget)
             (progn
               (setf (gtk:grid-column-spacing grid) 48)
               (setf (gtk:button-label widget) "Less Col Spacing"))
             (progn
               (setf (gtk:grid-column-spacing grid) 6)
               (setf (gtk:button-label widget) "More Col Spacing")))))
    ;; Pack and show the widgets
    (gtk:grid-attach grid button1 0 0 1 1)
    (gtk:grid-attach grid button2 1 0 1 1)
    (gtk:grid-attach grid button3 0 1 2 1)
    (gtk:widget-show window)))
