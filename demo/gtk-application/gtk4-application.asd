;;;; gtk4-application.asd

(asdf:defsystem :gtk4-application
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk4)
  :components ((:file "gtk4-application")
               (:file "application-command-line")
               (:file "application-inhibit")
               (:file "application-menu")
               (:file "application-notification")
               (:file "application-simple")
              ))

;; 2022-1-21
