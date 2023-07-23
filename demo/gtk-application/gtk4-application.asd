;;;; gtk4-application.asd

(asdf:defsystem :gtk4-application
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk4 :float-features)
  :components ((:file "gtk4-application")
               (:file "application-command-line")
               (:file "application-inhibit")
               (:file "application-menu")
               (:file "application-notification")
               (:file "application-simple")
              ))

;;; --- 2023-7-16 --------------------------------------------------------------
