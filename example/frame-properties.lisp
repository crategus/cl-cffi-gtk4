;;;; Frame Widget Properties
;;;;
;;;; This example allows to change interactively the appearance of the frame.
;;;;
;;;; 2024-4-6

(in-package :gtk4-example)

(defun do-frame-properties (&optional application)
  (let* (;; A horizontal Box for the content of the window
         (content (make-instance 'gtk:box
                                 :orientation :horizontal
                                 :margin-top 12
                                 :margin-bottom 12
                                 :margin-start 12
                                 :margin-end 12
                                 :spacing 24))
         (window (make-instance 'gtk:window
                                :application application
                                :child content
                                :title "Frame Widget"
                                :resizable nil))
         ;; A vertical box for the actions
         (action (make-instance 'gtk:box
                                :orientation :vertical
                                :spacing 6))
         ;; A Frame with a label
         (frame (make-instance 'gtk:frame
                               :child (gtk:label-new "Test Frame")
                               :label "Label"
                               :label-xalign 0.1
                               :width-request 200
                               :height-request 200))
         ;; Store the Label Widget of the Frame
         (label-widget (gtk:frame-label-widget frame))
         ;; The entry for input the text for the Frame Label
         (entry (make-instance 'gtk:entry
                                :secondary-icon-name "gtk-ok"
                                :secondary-icon-tooltip-text
                                "Change the Label of the Frame")))
    ;; Set the Frame Label
    (g:signal-connect entry "icon-press"
                      (lambda (entry pos)
                        (declare (ignore pos))
                        (setf (gtk:frame-label frame)
                              (gtk:editable-text entry))))
    (gtk:box-append action
                    (make-instance 'gtk:label
                                   :use-markup t
                                   :xalign 0.0
                                   :label "<b>Set Frame Label</b>"))
    ;; Put the actual Label text into the entry.
    (setf (gtk:editable-text entry) (gtk:frame-label frame))
    ;; Pack the entry in the action widget.
    (gtk:box-append action entry)
    ;; Set the Label alignment
    (let ((hbox (make-instance 'gtk:box
                                :orientation :horizontal
                                :homogenous t
                                :spacing 6))
          (xspin (make-instance 'gtk:spin-button
                                :adjustment
                                (make-instance 'gtk:adjustment
                                               :value
                                               (gtk:frame-label-xalign frame)
                                               :lower 0.0
                                               :upper 1.0
                                               :step-increment 0.1
                                               :page-increment 0.1
                                               :page-size 0.0)
                                :climb-rate 0
                                :digits 2
                                :wrap t)))
      (g:signal-connect xspin "value-changed"
         (lambda (spin)
           (setf (gtk:frame-label-xalign frame)
                 (gtk:spin-button-value spin))))
      (gtk:box-append action
                      (make-instance 'gtk:label
                                     :use-markup t
                                     :xalign 0.0
                                     :margin-top 12
                                     :label "<b>Align Frame Label</b>"))
      (gtk:box-append hbox xspin)
      (gtk:box-append action hbox))
    ;; Set a Label Widget
    (let ((toggle (gtk:check-button-new-with-label "Show Themed Icon")))
      (g:signal-connect toggle "toggled"
          (lambda (widget)
            (if (gtk:check-button-active widget)
                (let ((image (gtk:image-new-from-icon-name "gtk-home")))
                  ;; Store the actual Label Widget.
                  (setf label-widget (gtk:frame-label-widget frame))
                  (setf (gtk:widget-sensitive entry) nil)
                  (setf (gtk:frame-label-widget frame) image)
                  (gtk:widget-show image))
                  ;; Restore the saved Label Widget.
                  (progn
                    (setf (gtk:widget-sensitive entry) t)
                    (setf (gtk:frame-label-widget frame) label-widget)))))
      (gtk:box-append action
                      (make-instance 'gtk:label
                                     :use-markup t
                                     :xalign 0.0
                                     :margin-top 12
                                     :label "<b>Change Label Widget</b>"))
      (gtk:box-append action toggle))
    ;; Add frame and action to the content of the window.
    (gtk:box-append content frame)
    (gtk:box-append content action)
    ;; Show the window.
    (gtk:window-present window)))
