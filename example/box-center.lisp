;;;; Center Box
;;;;
;;;; The example shows three buttons with colored labels. The red button
;;;; shows the start position in the box, the green button the end position,
;;;; and the yellow button is a center widget.
;;;;
;;;; In addition, this example demonstrate how to use CSS style information to
;;;; change the appearance of a widget.
;;;;
;;;; 2025-07-26

(in-package :gtk4-example)

(defun do-box-center (&optional application)
  (let* ((css-button
          ".centerbox button         { padding: 0px; }
           .centerbox button > label { color: black;
                                       background-color: yellow; }
          .centerbox button:first-child > label {
                                       background-color: red;}
          .centerbox button:last-child > label {
                                       background-color : green; }")
         ;; Create a center box
         (box (make-instance 'gtk:center-box
                             :orientation :horizontal
                             :homogeneous nil
                             :spacing 0
                             :margin-bottom 12
                             :margin-end 12
                             :margin-start 12
                             :margin-top 12
                             :halign :fill
                             :valign :center
                             :width-request 480))
         ;; Create a toplevel window
         (window (make-instance 'gtk:window
                                :child box
                                :application application
                                :title "Center Box"))
         (provider (make-instance 'gtk:css-provider)))
    ;; Add Start button
    (setf (gtk:center-box-start-widget box)
          (make-instance 'gtk:button
                         :label "START"
                         :width-request 120
                         :height-request 120))
    ;; Add Center button
    (setf (gtk:center-box-center-widget box)
          (make-instance 'gtk:button
                         :label "CENTER"
                         :width-request 80))
    ;; Add End button
    (setf (gtk:center-box-end-widget box)
          (make-instance 'gtk:button
                         :label "END"
                         :width-request 60))
    ;; Load CSS from data into the provider and apply CSS
    (gtk:css-provider-load-from-string provider css-button)
    (gtk:widget-add-css-class box "centerbox")
    (gtk:widget-add-provider box provider)
    ;; Show the window
    (gtk:window-present window)))
