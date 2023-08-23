(in-package :gtk-test)

(in-suite gtk-test)

(test gtk-test-finished
  (cond (*first-run-gtk-test*
         (setf *first-run-gtk-test* nil)
         (format t "~%First run of the gtk-test suite finished.~%"))
        (t
         (format t "~%Second or more run of the gtk-test suite finished.~%"))))

;;; --- 2023-8-23 --------------------------------------------------------------
