;;;; Switch
;;;;
;;;; The <tt>gtk:switch widget</tt> is a widget that has two states: on or off.
;;;; The user can control which state should be active by clicking the switch,
;;;; or by dragging the handle. The <tt>gtk:switch</tt> widget can also handle
;;;; situations where the underlying state changes with a delay. See the
;;;; <tt>"state-set"</tt> signal for details.
;;;;
;;;; 2025-2-22

(in-package :gtk4-example)

(defun do-switch (&optional application)
  (let* ((css "frame { border-style: solid;
                       border-radius: 0;
                       border-color: rgb(1,1,1); }" )
         (box (make-instance 'gtk:box
                             :orientation :horizontal
                             :margin-top 48
                             :margin-bottom 48
                             :margin-start 36
                             :margin-end 12
                             :spacing 18))
        (frame (make-instance 'gtk:frame
                              :child box))
        (window (make-instance 'gtk:window
                               :title "Switch"
                               :child frame
                               :application application
                               :resizable nil))
        (switch (make-instance 'gtk:switch
                               :active t))
        (label (make-instance 'gtk:label
                              :xalign 0.0
                              :label "Switch is On"))
        (provider (gtk:css-provider-new)))
    ;; Load CSS from data into the provider and apply CSS
    (gtk:css-provider-load-from-string provider css)
    (gtk:widget-add-provider frame provider)
    ;; Connect signal handler
    (g:signal-connect switch "notify::active"
        (lambda (widget param)
          (declare (ignore param))
          (if (gtk:switch-active widget)
              (setf (gtk:label-label label) "Switch is On")
              (setf (gtk:label-label label) "Switch is Off"))))
    ;; Pack widgets and present window
    (gtk:box-append box switch)
    (gtk:box-append box label)
    (gtk:window-present window)))
