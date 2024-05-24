;;;; Application with automatic Resources
;;;;
;;;; This application automatically loads a menubar, an icon, and a shortcuts
;;;; window from resources.
;;;;
;;;; Last version: 2024-5-24

(in-package :gtk4-application)

(defun application-resources (&rest argv)
  ;; Register the resources
  (g:with-resource (resource (glib-sys:check-and-create-resources
                                     "gtk4-application.xml"
                                     :package "gtk4-application"
                                     :sourcedir "resource/"))
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
            (format t "   Resource path : ~a~%"
                      (g:application-resource-base-path app))
            (format t "      Theme path : ~a~%"
                      (gtk:icon-theme-resource-path
                          (gtk:icon-theme-for-display (gdk:display-default))))
            (format t "         menubar : ~a~%"
                      (gtk:application-menubar application))
            (format t "   menubar by id : ~a~%"
                      (gtk:application-menu-by-id application "menubar"))
            (format t "            icon : ~a~%"
                      (first (member "gtk-logo" icons :test #'string=))))))
      ;; Connect "activate" signal
      (g:signal-connect app "activate"
          (lambda (application)
            (g:application-hold application)
            ;; Create an application window
            (let (;; Define action entries for the menu items
                  (entries (list (list "about")))
                  (accels (list (list "win-show-help-overlay" "F1")
                                (list "win.about" "<Control>A")))
                  (window (make-instance 'gtk:application-window
                                         :application application
                                         :title "Application Resources"
                                         :show-menubar t
                                         :icon-name "gtk-logo"
                                         :default-width 480
                                         :default-height 300)))
              ;; Get the automatically loaded shortcuts window
              (format t "Shortcuts window : ~a~%"
                        (gtk:application-window-help-overlay window))
              ;; Add accelerators for the application
              (iter (for (name accel) on accels by #'cddr)
                    (setf (gtk:application-accels-for-action application name)
                          accel))
              ;; Add the action entries to the application window
              (g:action-map-add-action-entries window entries)
              ;; Present the window
              (gtk:window-present window)
              (g:application-release application))))
      ;; Run the application
      (g:application-run app argv))))
