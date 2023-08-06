;;;; gtk4-application.asd

(asdf:defsystem :gtk4-application
  :author "Dieter Kaiser"
  :license "MIT"
  :serial t
  :depends-on (:cl-cffi-gtk4)
  :components ((:file "gtk4-application")
               (:file "application-command-line")
               (:file "application-inhibit")
               (:file "application-menu")
               (:file "application-notification")
               (:file "application-simple")
               (:file "application-resources")
               (:file "application-register")
               (:file "sunny")
              ))

;;; --- 2023-8-3 ---------------------------------------------------------------
