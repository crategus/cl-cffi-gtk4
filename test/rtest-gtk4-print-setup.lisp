(in-package :gtk-test)

(def-suite gtk-print-setup :in gtk-suite)
(in-suite gtk-print-setup)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintSetup

(test gtk-print-setup-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkPrintSetup"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintSetup")
          (g:gtype (cffi:foreign-funcall "gtk_print_setup_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:print-setup
          (glib:symbol-for-gtype "GtkPrintSetup"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_print_setup_get_page_setup
;;;     gtk_print_setup_get_print_settings

;;; 2024-11-10
