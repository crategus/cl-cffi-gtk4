;;;; Simple Application
;;;;
;;;; This example is included in the API documentation for GtkApplication.
;;;;
;;;; Last version: 2025-4-21

(in-package :gtk4-application)

(defun application-simple (&rest argv)
  (let (;; Create an application
        (app (make-instance 'gtk:application
                            :flags :default-flags
                            :application-id "com.crategus.application-simple")))
    ;; Connect signal "activate" to the application
    (g:signal-connect app "activate"
        (lambda (application)
          ;; Create an application window
          (let ((window (make-instance 'gtk:application-window
                                       :application application
                                       :title "Simple Application"
                                       :default-width 480
                                       :default-height 300)))
            ;; Present the application window
            (gtk:window-present window))))
  ;; Run the application
  (g:application-run app argv)))
