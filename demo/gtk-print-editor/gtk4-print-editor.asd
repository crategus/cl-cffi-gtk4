;;;; gtk4-print-editor.asd

(asdf:defsystem :gtk4-print-editor
  :author "Dieter Kaiser"
  :license "MIT"
  :serial t
  :depends-on (:cl-cffi-gtk4 :iterate)
  :components ((:file "gtk4-print-editor")))

;;; 2024-3-8
