(in-package :gtk-test)

(def-suite gtk-version :in gtk-suite)
(in-suite gtk-version)

;;; --- Types and Values -------------------------------------------------------

;;;     GTK_MAJOR_VERSION                                  not implemented
;;;     GTK_MINOR_VERSION                                  not implemented
;;;     GTK_MICRO_VERSION                                  not implemented
;;;     GTK_BINARY_AGE                                     not implemented
;;;     GTK_INTERFACE_AGE                                  not implemented
;;;     GTK_CHECK_VERSION                                  not implemented

;;; --- Functions --------------------------------------------------------------

;;;     gtk_get_major_version

(test major-version
  (is (= 4 (gtk:major-version))))

;;;     gtk_get_minor_version

(test minor-version
  (is (<= 6 (gtk:minor-version))))

;;;     gtk_get_micro_version

(test micro-version
  (is (<= 0 (gtk:micro-version))))

;;;     gtk_get_binary_age

#+nil
(test binary-age
  (is (<= 606 (gtk:binary-age))))

;;;     gtk_get_interface_age

#+nil
(test interface-age
  (is (<= 6 (gtk:interface-age))))

;;;     gtk_check_version

(test check-version
  (is (string= "GTK version too old (major mismatch)"
               (gtk:check-version 5 0 0)))
  (is-false (gtk:check-version 4 0 0)))

;;; 2022-11-11
