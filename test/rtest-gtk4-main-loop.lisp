(in-package :gtk-test)

(def-suite gtk-main-loop :in gtk-suite)
(in-suite gtk-main-loop)

;;; --- Types and Values -------------------------------------------------------

;;;     GTK_PRIORITY_RESIZE                                not implemented

;;; --- Functions --------------------------------------------------------------

;;;     gtk_init
;;;     gtk_init_check

;;;     gtk_is_initialized

(test gtk-is-initialized
  (is (gtk:is-initialized)))

;;;     gtk_disable_setlocale

;;;     gtk_get_default_language

(test gtk-default-language
  (is (typep (gtk:default-language) 'pango:language)))

;;;     gtk_get_locale_direction

(test gtk-locale-direction
  (is (eq :ltr (gtk:locale-direction))))

;;; 2024-7-4
