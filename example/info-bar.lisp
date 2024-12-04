;;;; Info Bar
;;;;
;;;; The <tt>gtk:info-bar</tt> widget can be used to show messages to the user
;;;; without showing a dialog. It is often temporarily shown at the top or
;;;; bottom of a document. In contrast to the <tt>gtk:dialog</tt> widget, which
;;;; has a horizontal action area at the bottom, the info bar has a vertical
;;;; action area at the side.
;;;;
;;;; 2024-4-4

(in-package :gtk4-example)

(defun create-info-bar (msg type)
  (let ((infobar (make-instance 'gtk:info-bar
                                :message-type type
                                :show-close-button t))
        (message (make-instance 'gtk:label :label msg)))
    ;; Add a label with the message to the content of the info bar
    (gtk:info-bar-add-child infobar message)
    ;; Connect a signal handler to the info bar
    (g:signal-connect infobar "response"
                      (lambda (widget response)
                        (declare (ignore response))
                        (gtk:widget-hide widget)))
    infobar))

(defun do-info-bar (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical))
         (window (make-instance 'gtk:window
                                :title "Info bar"
                                :child vbox
                                :application application
                                :default-width 250
                                :default-height 200))
         (label (make-instance 'gtk:label
                               :label
                               "Click me to show the info bar."
                               :margin-top 48
                               :margin-bottom 24
                               :margin-start 24
                               :margin-end 24))
         (infobar (create-info-bar "A short message for the info bar." :info))
         (gesture (make-instance 'gtk:gesture-click)))
    ;; Add the controller to the drawing area
    (gtk:widget-add-controller window gesture)
    ;; Signal handler for the gesture
    (g:signal-connect gesture "pressed"
        (lambda (gesture n x y)
          (declare (ignore gesture n x y))
          (when (not (gtk:widget-is-visible infobar))
            (gtk:widget-show infobar))))
    ;; Pack and show the widgets
    (gtk:box-append vbox infobar)
    (gtk:box-append vbox label)
    (gtk:widget-hide infobar)
    (gtk:window-present window)))


(defun do-info-bar-1 (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical))
         (window (make-instance 'gtk:window
                                :title "Info bar"
                                :child vbox
                                :application application
                                :default-width 250))
         (infobar (make-instance 'gtk:info-bar
                                  :no-show-all t))
         (message (make-instance 'gtk:label
                                 :label "")))
    ;; Add a label to the content area of the info bar
    (gtk:info-bar-add-child infobar message)
    ;; Add a button OK to the action area
    (gtk:info-bar-add-button infobar "Ok" 1)
    ;; Connect a handler for the "response" signal of the info bar
    (g:signal-connect infobar "response"
       (lambda (widget response-id)
         (declare (ignore widget))
         (format t "response-id is ~A~%" response-id)
         (gtk:widget-hide infobar)))
    ;; Show the info bar
    (gtk:box-append vbox infobar)
    (setf (gtk:label-text message) "An Info Message in the content area.")
    (setf (gtk:info-bar-message-type infobar) :info)
    (gtk:window-present window)))
