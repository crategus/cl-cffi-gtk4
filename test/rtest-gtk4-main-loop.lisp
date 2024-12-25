(in-package :gtk-test)

(def-suite gtk-main-loop :in gtk-core)
(in-suite gtk-main-loop)

;;; --- Functions --------------------------------------------------------------

;;;     gtk_init

;;;     gtk_init_check

(test gtk-init-check
  (is-true (gtk:init-check)))

;;;     gtk_is_initialized

(test gtk-is-initialized
  (is-true (gtk:is-initialized)))

;;;     gtk_disable_setlocale

;;;     gtk_get_default_language

(test gtk-default-language
  (is (typep (gtk:default-language) 'pango:language)))

;;;     gtk_get_locale_direction

(test gtk-locale-direction
  (is (eq :ltr (gtk:locale-direction))))

;;; 2024-11-5
