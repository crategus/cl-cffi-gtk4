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
  (is (=   1 gtk:+gtk-priority-fallback+))
  (is (= 200 gtk:+gtk-priority-theme+))
  (is (= 400 gtk:+gtk-priority-settings+))
  (is (= 600 gtk:+gtk-priority-application+))
  (is (= 800 gtk:+gtk-priority-user+)))

;;;     GtkStyleProvider

(test gtk-style-provider-interface
  ;; Type check
  (is (g:type-is-interface "GtkStyleProvider"))
  ;; Check the registered name
  (is (eq 'gtk:style-provider
          (glib:symbol-for-gtype "GtkStyleProvider")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkStyleProvider")
          (g:gtype (cffi:foreign-funcall "gtk_style_provider_get_type" :size))))
  ;; Check the interface prerequisites
  (is (equal '()
             (list-interface-prerequisites "GtkStyleProvider")))
  ;; Get the the interface properties
  (is (equal '()
             (list-interface-properties "GtkStyleProvider")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkStyleProvider" GTK-STYLE-PROVIDER
                    (:EXPORT T :TYPE-INITIALIZER "gtk_style_provider_get_type"))
             (gobject:get-g-type-definition "GtkStyleProvider"))))

;;; --- Signals ----------------------------------------------------------------

;;;     gtk-private-changed

;;; --- 2023-8-30 --------------------------------------------------------------
