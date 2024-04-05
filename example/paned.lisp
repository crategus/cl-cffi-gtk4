;;;; Paned Window
;;;;
;;;; 2024-4-4

(in-package :gtk4-example)

(defun do-paned (&optional (application nil))
  (let* ((paned1 (make-instance 'gtk:paned
                                :position 100
                                :orientation :vertical))
         (paned2 (make-instance 'gtk:paned
                                :position 100
                                :orientation :horizontal))
         (window (make-instance 'gtk:window
                                :title "Paned Window"
                                :child paned1
                                :application application
                                :default-width 320
                                :default-height 280)))
    (setf (gtk:paned-start-child paned2)
          (make-instance 'gtk:frame
                         :label "Pane 1"
                         :label-xalign 0
                         :label-yalign 0))
    (setf (gtk:paned-end-child paned2)
          (make-instance 'gtk:frame
                         :label "Pane 2"
                         :label-xalign 0
                         :label-yalign 0))
    (setf (gtk:paned-end-child paned1)
          (make-instance 'gtk:frame
                         :label "Pane 3"
                         :label-xalign 0
                         :label-yalign 0))
    (setf (gtk:paned-start-child paned1) paned2)
    (gtk:window-present window)))
