;;;; Application with Automatic Resources
;;;;
;;;; This application automatically loads a menu bar, an icon, and a shortcuts
;;;; window from resources.
;;;;
;;;; The resources are loaded and registered with the G:WITH-RESOURCE macro.
;;;; The Lisp utility function GLIB-SYS:CHECK-AND-CREATE-RESOURCES checks if the
;;;; resource file to be loaded exists. If the file is missing, the function
;;;; compiles the resource definition in GTK4-APPLICATION.XML and the resource
;;;; files from the "RESOURCE/" folder into a loadable resource file.
;;;;
;;;; The "STARTUP" signal handler prints information about the automatically
;;;; loaded resources to the console. To view the output, start this example
;;;; after loading the GTK4-APPLICATION package using the
;;;; (GTK4-APPLICATION:APPLICATION-RESOURCES) command  from the Lisp prompt.
;;;;
;;;; Notes:
;;;;
;;;; The GTK:SHORTCUTS-WINDOW implementation is deprecated since GTK 4.18 and
;;;; will be removed in GTK 5. In this example the warnings for deprecated
;;;; widgets are switched off.
;;;;
;;;; Last updated: 2025-08-27

(in-package :gtk4-application)

(defun activate-about (window)
  (gtk:show-about-dialog window
                         :program-name "Application with Automatic Resources"
                         :logo-icon-name "gtk-logo"
                         :authors '("Dieter Kaiser")
                         :comments
                         (format nil "This GTK example automatically loads a ~
                                      menu bar, an icon, and a shortcuts ~
                                      window from resources.")))

(defun application-resources (&rest argv)
  (let (;; Switch off warnings for deprecated shortcuts window implementation
        (gtk-init:*warn-deprecated* nil))
    ;; Register the resources for automatic loading
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
              ;; Create an application window
              (let* ((window (make-instance 'gtk:application-window
                                            :application application
                                            :title "Application Resources"
                                            :show-menubar t
                                            :icon-name "gtk-logo"
                                            :default-width 480
                                            :default-height 300))
                     (entries `(("about"
                                 ,(lambda (action param)
                                    (declare (ignore action param))
                                    (activate-about window)))))
                     (accels '("win.show-help-overlay" "F1"
                               "win.about" "<Control>A")))
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
                (gtk:window-present window))))
        ;; Run the application
        (g:application-run app argv)))))
