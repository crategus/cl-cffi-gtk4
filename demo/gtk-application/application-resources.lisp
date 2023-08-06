;;;; Application with Automatic Resources - 2023-8-6
;;;;
;;;; This application automatically loads a menubar, an icon, and a shortcuts
;;;; window from resources.

;; TODO: The keyboard accelerators do not work as expected. Only the
;; CTRL-? key works.

(in-package :gtk4-application)

(defun application-resources (&rest argv)
  ;; Register the resources
  (gio:with-g-resources (resource (sys-path "resource/application.gresource"))
    (let (;; Create an application
          (app (make-instance 'gtk:application
                               :flags :default-flags
                               :application-id
                               "com.crategus.application")))
      ;; Connect "startup" signal
      (g:signal-connect app "startup"
          (lambda (application)
            (let* ((theme (gtk:icon-theme-for-display (gdk:display-default)))
                   (icons (gtk:icon-theme-icon-names theme)))
            ;; Print informations about the automatically loaded resources
            (format t "in STARTUP~%")
            (format t "Resource path : ~a~%"
                      (g:application-resource-base-path app))
            (format t "   Theme path : ~a~%"
                      (gtk:icon-theme-resource-path
                          (gtk:icon-theme-for-display (gdk:display-default))))
            (format t "      menubar : ~a~%" 
                      (gtk:application-menubar application))
            (format t "         icon : ~a~%" 
                      (first (member "my-gtk-logo" icons :test #'string=))))))
      ;; Connect "activate" signal
      (g:signal-connect app "activate"
          (lambda (application)
            (g:application-hold application)
            ;; Create an application window
            (let ((window (make-instance 'gtk:application-window
                                         :application application
                                         :title "Application Resources"
                                         :show-menubar t
                                         :icon-name "my-gtk-logo"
                                         :default-width 480
                                         :default-height 300)))
              ;; Make the window visible
              (setf (gtk:widget-visible window) t)
              (g:application-release application))))
    ;; Run the application
    (g:application-run app argv))))
