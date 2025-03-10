(in-package :gtk-test)

(def-suite gtk-style-provider :in gtk-theming)
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
             (glib-test:list-interface-prerequisites "GtkStyleProvider")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkStyleProvider")))
  ;; Check interface signals
  (is (equal '("gtk-private-changed")
             (glib-test:list-signals "GtkStyleProvider")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkStyleProvider" GTK:STYLE-PROVIDER
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_style_provider_get_type"))
             (gobject:get-gtype-definition "GtkStyleProvider"))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-style-provide-gtk-private-changed-signal
  (let* ((name "gtk-private-changed")
         (gtype (g:gtype "GtkStyleProvider"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; 2024-11-2
