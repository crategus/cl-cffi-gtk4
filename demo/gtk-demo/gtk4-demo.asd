;;;; gtk4-demo.lisp

(asdf:defsystem :gtk4-demo
  :author "Dieter Kaiser"
  :license "MIT"
  :serial t
  :depends-on (:gtk4-example
               :pango-example
               :cairo-example)
  :components ((:file "gtk4-demo")))

;;; --- 2023-8-23 --------------------------------------------------------------
