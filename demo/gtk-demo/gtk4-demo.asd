;;;; gtk4-demo.asd

(asdf:defsystem :gtk4-demo
  :author "Dieter Kaiser"
  :version "0.1.0"
  :license "MIT"
  :serial t
  :depends-on (:gtk4-example
               :pango-example
               :cairo-example)
  :components ((:file "gtk4-demo")))

;;; 2024-3-29
