;;;; Register Application - 2023-8-6

;; TODO: The QUERY-END signal is not called. Does REGISTER-SESSION work?

(in-package :gtk4-application)

(defun application-register (&rest argv)
  (let (;; Create an application
        (app (make-instance 'gtk:application
                            :flags :default-flags
                            :register-session t
                            :application-id 
                            "com.crategus.application-register")))

    (g:signal-connect app "query-end"
        (lambda (application)
          (declare (ignore application))
          (format t "in QUERY-END signal~&")))

    ;; Connect signal "activate" to the application
    (g:signal-connect app "activate"
        (lambda (application)
          (g:application-hold application)
          ;; Create an application window
          (let ((window (make-instance 'gtk:application-window
                                       :application application
                                       :title "Register Application"
                                       :default-width 480
                                       :default-height 300)))
            ;; Make the window visible
            (setf (gtk:widget-visible window) t)
            (g:application-release application))))
  ;; Run the application
  (g:application-run app argv)))
