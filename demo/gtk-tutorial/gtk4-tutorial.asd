;;;; gtk4-tutorial.asd

(asdf:defsystem :gtk4-tutorial
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk4)
  :components ((:file "gtk4-tutorial")
               (:file "window-simple")
               (:file "hello-world")
               (:file "button-packing")
               (:file "button-packing-with-builder")
               (:file "drawing-area")))

;;; --- 2023-4-6 ---------------------------------------------------------------
