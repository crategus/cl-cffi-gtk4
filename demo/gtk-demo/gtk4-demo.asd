;;;; gtk4-demo.lisp

(asdf:defsystem :gtk4-demo
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:gtk4-example
               :pango-example
               :cairo-example)
  :components ((:file "gtk4-demo")))

;;; --- 2023-3-13 --------------------------------------------------------------
