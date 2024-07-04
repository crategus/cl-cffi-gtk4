(in-package :gtk-test)

(def-suite gtk-style-provider :in gtk-suite)
(in-suite gtk-style-provider)

;;; --- Types and Values -------------------------------------------------------

;;;     GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
;;;     GTK_STYLE_PROVIDER_PRIORITY_THEME
;;;     GTK_STYLE_PROVIDER_PRIORITY_SETTINGS
;;;     GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
;;;     GTK_STYLE_PROVIDER_PRIORITY_USER

(test gtk-priority-constants
  (is (=   1 gtk:+priority-fallback+))
  (is (= 200 gtk:+priority-theme+))
  (is (= 400 gtk:+priority-settings+))
  (is (= 600 gtk:+priority-application+))
  (is (= 800 gtk:+priority-user+)))

;;;     GtkStyleProvider

(test gtk-style-provider-interface
  ;; Check type
  (is (g:type-is-interface "GtkStyleProvider"))
  ;; Check registered name
  (is (eq 'gtk:style-provider
          (glib:symbol-for-gtype "GtkStyleProvider")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStyleProvider")
          (g:gtype (cffi:foreign-funcall "gtk_style_provider_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (gtk-test:list-interface-prerequisites "GtkStyleProvider")))
  ;; Check interface properties
  (is (equal '()
             (gtk-test:list-interface-properties "GtkStyleProvider")))
  ;; Check interface signals
  (is (equal '("gtk-private-changed")
             (gtk-test:list-signals "GtkStyleProvider")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkStyleProvider" GTK-STYLE-PROVIDER
                    (:EXPORT T :TYPE-INITIALIZER "gtk_style_provider_get_type"))
             (gobject:get-g-type-definition "GtkStyleProvider"))))

;;; --- Signals ----------------------------------------------------------------

;;;     gtk-private-changed

;;; 2024-4-1
