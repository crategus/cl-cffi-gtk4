;;;; Application Notification - 2024-12-28

;; FIXME: This examples does not seem to work as expected. Try to work out an
;; example that demonstrates the usage of the g:notification class.

(in-package :gtk4-application)

(defun application-notification (&rest argv)
  (let (;; Create an application
        (app (make-instance 'gtk:application
                            :application-id
                            "com.crategus.application-notification"
                            :flags :default-flags)))
    ;; Connect signal "startup" to the application
    (g:signal-connect app "startup"
        (lambda (application)
          ;; Add action "clear-all" to the application
          (let ((action (g:simple-action-new "app.clear-all" nil)))
            (format t "~&in STARTUP for application~%")
            (g:signal-connect action "activate"
                (lambda (action parameter)
                  (declare (ignore action parameter))
                  (format t "in action CLEAR-ALL~%")))
            (g:action-map-add-action application action))))
    ;; Connect signal "activate" to the application
    (g:signal-connect app "activate"
        (lambda (application)
          ;; Create an application window
          (let* ((button (gtk:button-new-with-label "Send message"))
                 (window (make-instance 'gtk:application-window
                                       :application application
                                       :title "Application Notification"
                                       :child button
                                       :default-width 480
                                       :default-height 300)))
            (format t "~&in ACTIVATE for application~%")
            (g:signal-connect button "clicked"
                    (lambda (button)
                      (declare (ignore button))
                      (let ((msg (g:notification-new "Three lines of text")))
                        (format t "~&Send notification~%")
                        ;; Add more information to the notification
                        (g:notification-set-body msg "Keep up the good work.")
                        (g:notification-add-button msg "Start over "
                                                       "app.clear-all")
                        (g:notification-set-default-action msg "app.clear-all")
                        (g:application-send-notification application
                                                       "three-lines"
                                                       msg))))
            ;; Show the application window
            (gtk:window-present window))))
    (when (not (g:application-register app))
      (format t "~&Registration of the application failed.~%"))
    ;; Run the application
    (g:application-run app argv)))
