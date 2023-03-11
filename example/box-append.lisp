;;;; Example Box - 2022-11-11

(in-package :gtk4-example)

(defun make-box (homogeneous spacing)
  (let ((box (make-instance 'gtk:box
                            :orientation :horizontal
                            :baseline-position :top
                            :homogeneous homogeneous
                            :spacing spacing)))
    (gtk:box-append box (gtk:button-new-with-label "This"))
    (gtk:box-append box (gtk:button-new-with-label "is"))
    (gtk:box-append box (gtk:button-new-with-label "a"))
    (gtk:box-append box (gtk:button-new-with-label "button"))
    (gtk:box-append box (gtk:button-new-with-label "party"))
    (gtk:box-append box (gtk:button-new-with-label "."))
    box))

(defun do-box-append (&optional (application nil))
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :spacing 12
                              :margin-bottom 12
                              :margin-end 12
                              :margin-start 12
                              :margin-top 12))
         (window (make-instance 'gtk:window
                                :title "Example Box"
                                :application application
                                :child vbox)))
    ;; Non-homogenous box
    (gtk:box-append vbox
                    (make-instance 'gtk:label
                                   :use-markup t
                                   :label "<b>Non-homogeneous box</b>"
                                   :xalign 0))
    (gtk:box-append vbox (make-box nil 3))
    ;; Homogeneous box
    (gtk:box-append vbox
                    (make-instance 'gtk:label
                                   :use-markup t
                                   :label "<b>Homogeneous box</b>"
                                   :xalign 0))
    (gtk:box-append vbox (make-box t 3))
    ;; Homogeneous box more spacing
    (gtk:box-append vbox
                    (make-instance 'gtk:label
                                   :use-markup t
                                   :label
                                   "<b>Homogeneous box with more spacing</b>"
                                   :xalign 0))
    (gtk:box-append vbox (make-box t 12))
    (gtk:widget-show window)))
