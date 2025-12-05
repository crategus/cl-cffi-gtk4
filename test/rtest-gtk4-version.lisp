(in-package :gtk-test)

(def-suite gtk-version :in gtk-core)
(in-suite gtk-version)

;;; --- Functions --------------------------------------------------------------

;;;     gtk_get_major_version

(test gtk-major-version
  (is (= 4 (gtk:major-version))))

;;;     gtk_get_minor_version

#+crategus
(test gtk-minor-version
  (is (= 20 (gtk:minor-version))))

;;;     gtk_get_micro_version

#+crategus
(test gtk-micro-version
  (is (= 1 (gtk:micro-version))))

;;;     gtk_get_binary_age                                  not implemented
;;;     gtk_get_interface_age                               not implemented

;;;     gtk_check_version

(test gtk-check-version
  (is (string= "GTK version too old (major mismatch)"
               (gtk:check-version 5 0 0)))
  (is-false (gtk:check-version 4 0 0)))

;;; 2025-11-08
