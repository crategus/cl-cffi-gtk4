;;; gtk4.getting-started.asd

(asdf:defsystem :gtk4-getting-started
  :author "Dieter Kaiser"
  :version "0.1.0"
  :license "MIT"
  :depends-on (:cl-cffi-gtk4)
  :components ((:file "pinus")
               (:file "pinus1")
               (:file "pinus2")
               (:file "pinus3")
               (:file "pinus4")
               (:file "pinus5")
               (:file "pinus6")
               (:file "pinus7")
               (:file "pinus8")))

(asdf:defsystem :gtk4-getting-started/pinus
  :author "Dieter Kaiser"
  :version "0.1.0"
  :license "MIT"
  :entry-point "pinus:pinus"
  :build-operation "asdf:program-op"
  :build-pathname "binary/pinus"
  :serial t
  :depends-on (:gtk4-getting-started))

;;; 2025-12-26
