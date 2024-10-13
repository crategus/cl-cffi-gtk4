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

(test gtk-major-version
  (is (= 4 (gtk:major-version))))

;;;     gtk_get_minor_version

(test gtk-minor-version
  (is (= 16 (gtk:minor-version))))

;;;     gtk_get_micro_version

(test gtk-micro-version
  (is (= 2 (gtk:micro-version))))

;;;     gtk_get_binary_age                                 not implemented
;;;     gtk_get_interface_age                              not implemented

;;;     gtk_check_version

(test gtk-check-version
  (is (string= "GTK version too old (major mismatch)"
               (gtk:check-version 5 0 0)))
  (is-false (gtk:check-version 4 0 0)))

;;; 2024-10-13
