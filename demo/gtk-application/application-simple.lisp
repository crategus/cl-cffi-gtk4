;;;; Simple Application - 2023-7-16
;;;;
;;;; This example is included in the API documentation for GtkApplication.

(in-package :gtk4-application)

(defun application-simple (&rest argv)
  (let (;; Create an application
        (app (make-instance 'gtk:application
                            :flags :default-flags
                            :application-id "com.crategus.application-simple")))
    ;; Connect signal "activate" to the application
    (g:signal-connect app "activate"
        (lambda (application)
          (g:application-hold application)
          ;; Create an application window
          (let ((window (make-instance 'gtk:application-window
                                       :application application
                                       :title "Simple Application"
                                       :default-width 480
                                       :default-height 300)))
            ;; Make the window visible
            (setf (gtk:widget-visible window) t)
            (g:application-release application))))
  ;; Run the application
  (g:application-run app argv)))
