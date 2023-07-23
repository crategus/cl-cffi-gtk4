;;;; Application Inhibit - 2023-7-16

;; FIXME: This examples does not work as expected. Try to work out an example
;; that demonstrates the gtk:application-inhibit function.

(in-package :gtk4-application)

(defun application-inhibit (&rest argv)
  (let (;; Create an application
        (app (make-instance 'gtk:application
                            :application-id "com.crategus.application-inhibit"
                            :flags :default-flags)))
    ;; Connect signal "activate" to the applicaton
    (g:signal-connect app "activate"
        (lambda (application)
          ;; Create an application window
          (let* ((cookie 0)
                 (flags '(:logout :switch :suspend :idle))
                 (message "Application is inhibited.")
                 (toggle (make-instance 'gtk:toggle-button
                                        :label "Inhibit application"))
                 (window (make-instance 'gtk:application-window
                                        :application application
                                        :title "Application Inhibit"
                                        :child toggle
                                        :default-width 480
                                        :default-height 300)))
            (g:signal-connect toggle "toggled"
                (lambda (widget)
                  (if (gtk:toggle-button-active widget)
                      (progn
                        (setf (gtk:button-label widget) "Unhibit Application")
                        (setf cookie
                              (gtk:application-inhibit app nil flags message))
                        (format t "Application is inhibited: ~a~%" cookie))
                      (progn
                        (setf (gtk:button-label widget) "Inhibit Application")
                        (gtk:application-uninhibit app cookie)
                        (setf cookie 0)
                        (format t "Application is unhibited: ~a~%" cookie)))))
            ;; Show the application window
            (gtk:widget-show window))))
    ;; Run the application
    (g:application-run app argv)))
