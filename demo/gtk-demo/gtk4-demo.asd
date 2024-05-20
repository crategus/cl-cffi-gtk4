;;;; gtk4-demo.asd

(asdf:defsystem :gtk4-demo
  :author "Dieter Kaiser"
  :version "0.1.0"
  :license "MIT"
  :serial nil
  :components ((:file "gtk4-demo-package")
               (:file "gtk4-demo-catalog")
               (:file "gtk4-demo"))
  :depends-on (:gtk4-example
               :gtk4-application
               :pango-example
               :cairo-example))

;;; 2024-4-4
