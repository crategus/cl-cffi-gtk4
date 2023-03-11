;;;; Event Controller - 2022-8-22

(in-package :gtk4-example)

(defun print-controller-details (controller)
  (format t "  Details for the Event Controller :~%")
  (format t "        name : ~a~%"
            (gtk-event-controller-name controller))
  (format t "       limit : ~a~%"
            (gtk-event-controller-propagation-limit controller))
  (format t "       phase : ~a~%"
            (gtk-event-controller-propagation-phase controller))
  (format t "      widget : ~a~%"
            (gtk-event-controller-widget controller))
  (format t "       event : ~a~%"
            (gtk-event-controller-current-event controller))
  (format t "      device : ~a~%"
            (gtk-event-controller-current-event-device controller))
  (format t "       state : ~a~%"
            (gtk-event-controller-current-event-state controller))
  (format t "        time : ~a~%"
            (gtk-event-controller-current-event-time controller)))

(defun do-event-controller (&optional application)
  (let* ((vbox (make-instance 'gtk-box
                              :orientation :horizontal))
         (window (make-instance 'gtk-window
                                :application application
                                :child vbox
                                :title "Event Controller")))

    (let* ((entry (make-instance 'gtk-text))
           (frame (make-instance 'gtk-frame
                                 :child entry))
           (controller (make-instance 'gtk-event-controller-key
                                      :name "GtkEventControllerKey")))

      (gtk-box-append vbox frame)

      ;; Add the key event controller to the text widget
      (gtk-widget-add-controller entry controller)
      ;; Handle the signals for the key event controller
      (g-signal-connect controller "key-pressed"
          (lambda (controller keyval keycode state)
            (format t "~%in Signal KEY-PRESSED~%")
            (format t "  controller : ~a~%" controller)
            (format t "      keyval : ~a~%" keyval)
            (format t "     keycode : ~a~%" keycode)
            (format t "       state : ~a~%" state)

            (print-controller-details controller)

;        (format t "Details for the Event : ~%")
;        (let ((event (gtk-event-controller-current-event controller)))
;          (format t "      keyval : ~a~%" (gdk-key-event-keyval event))
;          (format t "     keycode : ~a~%" (gdk-key-event-keycode event))
;          (format t "      layout : ~a~%" (gdk-key-event-layout event))
;          (format t "       level : ~a~%" (gdk-key-event-level event))
;          (format t "       match : ~a~%"
;                    (multiple-value-list (gdk-key-event-match event))))

      ))
      (g-signal-connect controller "key-released"
          (lambda (controller keyval keycode state)
            (format t "~%in Signal KEY-RELEASED~%")
            (format t "  controller : ~a~%" controller)
            (format t "      keyval : ~a~%" keyval)
            (format t "     keycode : ~a~%" keycode)
            (format t "       state : ~a~%" state)
            (print-controller-details controller)
      )))

    ;; Show the window.
    (gtk-widget-show window)))
