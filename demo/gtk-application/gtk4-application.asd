;;;; gtk4-application.asd

(asdf:defsystem :gtk4-application
  :author "Dieter Kaiser"
  :version "0.1.0"
  :license "MIT"
  :serial t
  :depends-on (:cl-cffi-gtk4)
  :components ((:file "gtk4-application")
               (:file "application-command-line")
               (:file "application-inhibit")
               (:file "application-menubar")
               (:file "application-notification")
               (:file "application-simple")
               (:file "application-resources")
               (:file "application-register")
               (:file "sunny")
               (:file "bloatpad")
               (:file "myapplication")
              ))

;;; 2025-3-7
